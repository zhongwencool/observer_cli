#!/usr/bin/env escript
%%! -noshell
-mode(compile).
-include_lib("kernel/include/file.hrl").
main([Dir]) ->
    nonode@nohost = node(),
    false = is_alive(),
    code:add_pathsa(filelib:wildcard("_build/test/lib/*/ebin")),
    code:add_pathsa(filelib:wildcard("_build/test/lib/*/test")),
    {ok, _} = application:ensure_all_started(sasl),
    {ok, Tcp} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Socket} = socket:open(inet, stream, tcp),
    {ok, Server} = gen_server:start_link(
        observer_cli_snapshot_test, #{payload => [a, b, c], binary => <<1, 2>>}, []
    ),
    {ok, Statem} = gen_statem:start_link(
        observer_cli_snapshot_test, {gen_statem_fixture, ready, #{count => 1}}, []
    ),
    {ok, Event} = gen_event:start_link(),
    ok = gen_event:add_handler(Event, observer_cli_snapshot_test, #{payload => 1}),
    io:format("Generating fixture captures on isolated ~p~n", [node()]),
    Tracee = spawn(fun Loop() ->
        timer:sleep(1),
        Loop()
    end),
    Specs = [
        {snapshot, #{}},
        {snapshot, #{deep => true}},
        {snapshot, #{test_probe_outcomes => #{resources => {error, probe_failed}}}},
        {memory, #{}},
        {memory, #{test_probe_outcomes => #{memory => {error, probe_failed}}}},
        {schedulers, #{duration_ms => 250}},
        {distribution, #{}},
        {processes, #{}},
        {processes, #{duration_ms => 250}},
        {processes, #{test_process_source => #{count_fun => fun() -> 1000001 end}}},
        {applications, #{}},
        {ets, #{}},
        {ets, #{test_ets_source => #{count_fun => fun() -> 1000001 end}}},
        {mnesia, #{}},
        {network, #{}},
        {network, #{duration_ms => 250}},
        {ports, #{}},
        {sockets, #{}},
        {sockets, #{duration_ms => 250}},
        {process, #{target => <<"init">>}},
        {process, #{target => <<"absent_schema_fixture">>}},
        {port, #{target => list_to_binary(port_to_list(Tcp))}},
        {port, #{target => <<"missing">>}},
        {otp_state, #{target => list_to_binary(pid_to_list(Server)), behavior => gen_server}},
        {otp_state, #{target => list_to_binary(pid_to_list(Statem)), behavior => gen_statem}},
        {otp_state, #{target => list_to_binary(pid_to_list(Event)), behavior => gen_event}},
        {otp_state, #{target => list_to_binary(pid_to_list(Server)), behavior => gen_statem}},
        {otp_state, #{target => <<"absent_schema_fixture">>, behavior => gen_event}},
        {supervision_tree, #{app => <<"kernel">>}},
        {supervision_tree, #{app => <<"kernel">>, test_application_source => app_source(invalid)}},
        {supervision_tree, #{
            app => <<"kernel">>,
            test_application_source => app_source([
                {specs, 100001}, {active, 100001}, {supervisors, 0}, {workers, 100001}
            ])
        }},
        {supervision_tree, #{app => <<"absent_schema_fixture">>}},
        {diagnose, #{}},
        {diagnose, #{observe => <<"5000">>}},
        {diagnose, #{observe => <<"5000">>, deep => true}},
        {logs, #{handler => null, tail => 10}},
        {logs, #{
            handler => <<"schema_fixture">>,
            tail => 10,
            test_log_env => log_env(<<"normal\n", 255, "\n">>)
        }},
        {logs, #{
            handler => <<"schema_fixture">>,
            tail => 200,
            test_log_env => log_env(
                binary:copy(<<"sample log line ", (binary:copy(<<"x">>, 700))/binary, "\n">>, 200)
            )
        }},
        {network, #{
            test_network_source => #{
                count_fun => fun() -> 1 end,
                all_fun => fun() -> {error, fixture_enumeration_failed} end
            }
        }},
        {sockets, #{test_socket_source => #{available_fun => fun() -> false end}}},
        {mnesia, #{sort => size, test_mnesia_source => mnesia_source()}},
        {mnesia, #{test_mnesia_source => #{available_fun => fun() -> false end}}},
        {trace, #{
            action => call,
            mfa => <<"timer:sleep/1">>,
            pid => list_to_binary(pid_to_list(Tracee)),
            duration_ms => 100,
            max => 1,
            replace_existing_trace => true
        }},
        {trace, #{
            action => call,
            mfa => <<"erlang:node/0">>,
            pid => list_to_binary(pid_to_list(self())),
            duration_ms => 100,
            max => 1,
            replace_existing_trace => true
        }},
        {trace, #{action => stop_all, all => true}},
        {trace, #{action => call}}
    ],
    try
        lists:foreach(
            fun({{Cmd, Request}, Index}) ->
                lists:foreach(
                    fun(Policy) ->
                        Result = observer_cli_snapshot:dispatch(self(), Cmd, Request, #{
                            timeout_ms => 15000, identifier_policy => Policy
                        }),
                        case Result of
                            #{<<"result">> := Response} ->
                                write(
                                    Dir,
                                    atom_to_list(Cmd) ++ "-" ++ integer_to_list(Index) ++ "-" ++
                                        atom_to_list(Policy),
                                    Response
                                );
                            _ ->
                                erlang:error({fixture_dispatch_failed, Cmd, Request, Result})
                        end
                    end,
                    [include, redact]
                )
            end,
            lists:zip(Specs, lists:seq(1, length(Specs)))
        ),
        pure_fixtures(Dir)
    after
        exit(Tracee, kill),
        gen_tcp:close(Tcp),
        socket:close(Socket),
        gen_server:stop(Server),
        gen_statem:stop(Statem),
        gen_event:stop(Event)
    end.
write(Dir, Name, Response) ->
    ok = filelib:ensure_dir(filename:join(Dir, Name ++ ".json")),
    ok = file:write_file(filename:join(Dir, Name ++ ".json"), json:encode(Response)),
    case maps:get(<<"command">>, Response) of
        <<"diagnose">> ->
            WithActions = observer_cli_escriptize:add_next_actions(Response),
            ok = file:write_file(
                filename:join(Dir, Name ++ "-actions.json"), json:encode(WithActions)
            );
        _ ->
            ok
    end.
pure_fixtures(Dir) ->
    Resources = #{
        process => #{observed_count_including_observer => 96, limit => 100},
        port => #{observed_count_including_observer => 1, limit => 100},
        atom => #{observed_count_including_observer => 1, limit => 100},
        ets => #{observed_count => 1, limit => 100}
    },
    Samples = [
        #{
            status => ok,
            resources => Resources,
            monotonic_start_ms => I,
            monotonic_finish_ms => I + 1,
            monotonic_midpoint_ms => I,
            process_inventory => #{status => error, reason_code => process_inventory_failed}
        }
     || I <- [0, 1500]
    ],
    Timing = #{
        started_at => <<"2026-09-28T00:00:00Z">>,
        finished_at => <<"2026-09-28T00:00:02Z">>,
        duration_ms => 1501,
        controller => self(),
        module_loaded_before_sample => true
    },
    Raw = observer_cli_diagnostic:build_report(Samples, [0, 1500], Timing, #{
        status => error, reason_code => distribution_probe_failed
    }),
    {ok, Report} = observer_cli_snapshot:normalize(Raw, include),
    write(Dir, "diagnose-findings-partial", Report),
    RawGap = observer_cli_diagnostic:build_report(
        [#{status => error}, #{status => error}], [0, 1500], Timing, #{
            status => error, reason_code => distribution_probe_failed
        }
    ),
    {ok, Gap} = observer_cli_snapshot:normalize(RawGap, include),
    write(Dir, "diagnose-required-gap", Gap).

log_env(Binary) ->
    Path = filename:absname("schema-fixture-never-opened.log"),
    Info = #file_info{type = regular, size = byte_size(Binary), major_device = 1, inode = 2},
    #{
        os_type => fun() -> {unix, linux} end,
        handler_ids => fun() -> [schema_fixture] end,
        handler_config => fun(Id) ->
            {ok, #{
                id => Id,
                module => logger_std_h,
                config => #{type => file, file => Path, modes => [raw]}
            }}
        end,
        read_link_info => fun(_) -> {ok, Info} end,
        open => fun(_) -> {ok, mock_fd} end,
        read_file_info => fun(_) -> {ok, Info} end,
        position => fun(_) -> {ok, byte_size(Binary)} end,
        pread => fun(_, Start, Length) -> {ok, binary:part(Binary, Start, Length)} end,
        close => fun(_) -> ok end
    }.
mnesia_source() ->
    #{
        available_fun => fun() -> true end,
        running_fun => fun() -> yes end,
        local_tables_fun => fun() -> [ram_table, disk_table, external_table] end,
        info_fun => fun
            (T, storage_type) ->
                case T of
                    ram_table -> ram_copies;
                    disk_table -> disc_only_copies;
                    external_table -> external
                end;
            (_, size) ->
                1;
            (_, memory) ->
                20
        end,
        whereis_fun => fun(_) -> undefined end,
        ets_info_fun => fun(_, _) -> undefined end,
        word_size_fun => fun() -> 8 end
    }.

app_source(Counts) ->
    #{
        loaded_fun => fun() -> [{kernel, "fixture", "1.0"}] end,
        supervisor_fun => fun(_) -> {ok, self()} end,
        alive_fun => fun(_) -> true end,
        count_children_fun => fun(_) -> Counts end
    }.
