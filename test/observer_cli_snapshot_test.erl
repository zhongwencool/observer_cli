-module(observer_cli_snapshot_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

capabilities_test() ->
    ?assertEqual(#{protocol_version => 1}, observer_cli_snapshot:capabilities()).

default_snapshot_is_scan_free_fact_package_test() ->
    Response = snapshot(#{}),
    ?assertEqual(<<"snapshot">>, maps:get(<<"command">>, Response)),
    ?assertMatch(
        #{<<"node">> := <<"node-1">>, <<"otp_release">> := _},
        maps:get(<<"target">>, Response)
    ),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, Capture)),
    ?assert(is_binary(maps:get(<<"started_at">>, Capture))),
    ?assert(is_binary(maps:get(<<"finished_at">>, Capture))),
    ?assert(maps:get(<<"duration_ms">>, Capture) >= 0),
    assert_probe(<<"runtime">>, true, <<"ok">>, Capture),
    assert_probe(<<"resources">>, true, <<"ok">>, Capture),
    assert_probe(<<"memory">>, true, <<"ok">>, Capture),
    assert_probe(<<"schedulers">>, false, <<"ok">>, Capture),
    assert_probe(<<"distribution">>, false, <<"ok">>, Capture),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(1, maps:get(<<"snapshot_version">>, Data)),
    Resources = maps:get(<<"resources">>, Data),
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                true,
                maps:get(<<"observer_contaminated">>, maps:get(Key, Resources))
            )
        end,
        [<<"process">>, <<"port">>, <<"atom">>]
    ),
    Memory = maps:get(<<"memory">>, Data),
    ?assertEqual(
        true,
        maps:get(<<"observer_contaminated">>, maps:get(<<"beam">>, Memory))
    ),
    GC = maps:get(<<"garbage_collection">>, Memory),
    ?assertEqual(
        maps:get(<<"reclaimed_words_total">>, GC) * erlang:system_info(wordsize),
        maps:get(<<"reclaimed_bytes_total">>, GC)
    ),
    ?assertEqual(
        false,
        maps:get(
            <<"scheduler_wall_time_enabled_by_observer_cli">>,
            maps:get(<<"schedulers">>, Data)
        )
    ),
    Distribution = maps:get(<<"distribution">>, Data),
    ?assertEqual(<<"empty">>, maps:get(<<"state">>, Distribution)),
    ?assertEqual([], maps:get(<<"connected_peers">>, Distribution)),
    [ModuleEffect] = [
        Effect
     || #{<<"id">> := <<"module_load">>} = Effect <-
            maps:get(<<"observer_effects">>, Capture)
    ],
    ?assertEqual(true, maps:get(<<"module_loaded_before_sample">>, ModuleEffect)),
    ?assertEqual([], maps:get(<<"errors">>, Response)),
    ?assertEqual([], maps:get(<<"warnings">>, Response)),
    ?assertEqual(
        nomatch,
        binary:match(term_to_binary(Response), atom_to_binary(node()))
    ),
    assert_json_safe(Response).

default_snapshot_does_not_call_full_enumerators_test() ->
    Parent = self(),
    Tracer = spawn(fun() -> trace_forwarder(Parent) end),
    Enumerators = [
        {erlang, processes, 0},
        {erlang, ports, 0},
        {ets, all, 0},
        {socket, which_sockets, 0},
        {application, loaded_applications, 0},
        {application, which_applications, 0},
        {application, which_applications, 1},
        {mnesia, system_info, 1}
    ],
    lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, true, [local]) end, Enumerators),
    erlang:trace(new, true, [call, {tracer, Tracer}]),
    try
        _ = snapshot(#{}),
        receive
            {enumerator_called, Call} -> ?assertEqual(no_full_enumerator_call, Call)
        after 100 ->
            ok
        end
    after
        erlang:trace(new, false, [call]),
        lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, false, [local]) end, Enumerators),
        exit(Tracer, kill)
    end.

snapshot_probe_failure_semantics_test() ->
    Unavailable = snapshot(#{
        test_probe_outcomes => #{schedulers => {unavailable, capability_unavailable}}
    }),
    UnavailableCapture = maps:get(<<"capture">>, Unavailable),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, UnavailableCapture)),
    assert_probe(<<"schedulers">>, false, <<"unavailable">>, UnavailableCapture),
    ?assertNot(is_map_key(<<"schedulers">>, maps:get(<<"data">>, Unavailable))),
    ?assertMatch(
        [#{<<"probe">> := <<"schedulers">>, <<"reason_code">> := <<"capability_unavailable">>}],
        maps:get(<<"warnings">>, Unavailable)
    ),
    OptionalTimeout = snapshot(#{
        test_probe_outcomes => #{schedulers => {timeout, target_timeout}}
    }),
    ?assertEqual(
        <<"partial">>,
        maps:get(<<"status">>, maps:get(<<"capture">>, OptionalTimeout))
    ),
    ?assertMatch(
        [#{<<"class">> := <<"partial">>, <<"probe">> := <<"schedulers">>}],
        maps:get(<<"errors">>, OptionalTimeout)
    ),
    RequiredError = snapshot(#{
        test_probe_outcomes => #{resources => {error, probe_failed}}
    }),
    ?assertEqual(
        <<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, RequiredError))
    ),
    ?assertNot(is_map_key(<<"resources">>, maps:get(<<"data">>, RequiredError))),
    ?assert(is_map_key(<<"memory">>, maps:get(<<"data">>, RequiredError))),
    ?assertMatch(
        [#{<<"class">> := <<"required_probe">>, <<"probe">> := <<"resources">>}],
        maps:get(<<"errors">>, RequiredError)
    ).

deep_snapshot_composes_narrow_probe_defaults_test() ->
    Response = snapshot(#{deep => true}),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, Capture)),
    lists:foreach(
        fun(Id) -> assert_probe(Id, false, <<"ok">>, Capture) end,
        [
            <<"processes">>,
            <<"applications">>,
            <<"ets">>,
            <<"mnesia">>,
            <<"network">>,
            <<"ports">>,
            <<"sockets">>
        ]
    ),
    Data = maps:get(<<"data">>, Response),
    lists:foreach(
        fun({Id, Sort, Semantics}) ->
            ProbeData = maps:get(Id, Data),
            ?assertEqual(Sort, maps:get(<<"sort">>, ProbeData)),
            ?assertEqual(Semantics, maps:get(<<"sort_semantics">>, ProbeData)),
            ?assert(maps:get(<<"returned_count">>, ProbeData) =< 20)
        end,
        [
            {<<"processes">>, <<"memory">>, <<"total">>},
            {<<"applications">>, <<"memory">>, <<"current">>},
            {<<"ets">>, <<"memory">>, <<"current">>},
            {<<"network">>, <<"oct">>, <<"total">>},
            {<<"ports">>, <<"queue_size">>, <<"current_or_lifetime">>},
            {<<"sockets">>, <<"io">>, <<"total">>}
        ]
    ),
    ?assertEqual(<<"memory">>, maps:get(<<"sort">>, maps:get(<<"mnesia">>, Data))),
    assert_json_safe(Response).

deep_snapshot_shares_report_identifier_dictionary_test() ->
    Pid = spawn(fun process_fixture/0),
    Table = make_ref(),
    ProcessSource = process_source([Pid], fun(Current, Keys) ->
        process_info_fixture(Current, Keys, 100)
    end),
    EtsValues = #{
        id => Table,
        name => goal_11_table,
        size => 1,
        memory => 2,
        owner => Pid,
        type => set,
        protection => public,
        keypos => 1
    },
    EtsSource = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() -> [Table] end,
        info_fun => fun(_Table, Key) -> maps:get(Key, EtsValues) end,
        word_size_fun => fun() -> 8 end
    },
    try
        Response = snapshot(#{
            deep => true,
            test_process_source => ProcessSource,
            test_ets_source => EtsSource,
            test_deep_probe_outcomes => #{
                applications =>
                    {unavailable, scan_budget_exceeded, #{admission_stage => fixture}, 0, [
                        admission_only
                    ]}
            }
        }),
        Data = maps:get(<<"data">>, Response),
        [Process] = maps:get(<<"items">>, maps:get(<<"processes">>, Data)),
        [TableItem] = maps:get(<<"items">>, maps:get(<<"ets">>, Data)),
        ?assertEqual(maps:get(<<"pid">>, Process), maps:get(<<"owner">>, TableItem))
    after
        exit(Pid, kill)
    end.

deep_snapshot_refusal_and_started_failure_contract_test() ->
    Refused = snapshot(#{
        deep => true,
        test_deep_probe_outcomes => #{
            ets =>
                {unavailable, scan_budget_exceeded,
                    #{admission_stage => pre_enumeration, observed_table_count => 100001}, 0, [
                        admission_only
                    ]}
        }
    }),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, maps:get(<<"capture">>, Refused))),
    [Skipped] = maps:get(<<"skipped">>, maps:get(<<"data">>, Refused)),
    ?assertEqual(<<"ets">>, maps:get(<<"probe">>, Skipped)),
    ?assertEqual(
        100001,
        maps:get(
            <<"observed_table_count">>, maps:get(<<"admission_evidence">>, Skipped)
        )
    ),
    lists:foreach(
        fun({Probe, Outcome}) ->
            Response = snapshot(#{
                deep => true, test_deep_probe_outcomes => #{Probe => Outcome}
            }),
            Capture = maps:get(<<"capture">>, Response),
            ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
            ?assertNot(is_map_key(atom_to_binary(Probe), maps:get(<<"data">>, Response)))
        end,
        [
            {processes, {timeout, target_timeout}},
            {applications, {error, probe_failed}}
        ]
    ).

deep_snapshot_heap_boundary_is_partial_test() ->
    Parent = self(),
    Source = process_source([self()], fun(_Pid, _Keys) -> undefined end),
    HeapSource = Source#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Parent ! {deep_heap_worker, self()},
                length(lists:seq(1, 1000000))
            end}
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            #{
                deep => true,
                test_process_source => HeapSource,
                test_deep_probe_outcomes => #{
                    applications =>
                        {unavailable, scan_budget_exceeded, #{admission_stage => fixture}, 0, [
                            admission_only
                        ]}
                }
            },
            #{timeout_ms => 5000, identifier_policy => redact, max_heap_words => 262144}
        ),
    Worker =
        receive
            {deep_heap_worker, Pid} -> Pid
        end,
    ?assertNot(is_process_alive(Worker)),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
    [ProcessProbe] = [
        Probe
     || #{<<"id">> := <<"processes">>} = Probe <- maps:get(<<"probes">>, Capture)
    ],
    ?assertEqual(<<"worker_heap_limit_exceeded">>, maps:get(<<"reason_code">>, ProcessProbe)).

deep_snapshot_started_timeout_cleans_probe_worker_test() ->
    Parent = self(),
    Source = (process_source([self()], fun(_Pid, _Keys) -> undefined end))#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Parent ! {deep_timeout_worker, self()},
                receive
                    stop -> ok
                end
            end}
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            #{deep => true, test_process_source => Source},
            options(2000, redact)
        ),
    Worker =
        receive
            {deep_timeout_worker, Pid} -> Pid
        end,
    ?assertNot(is_process_alive(Worker)),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
    [ProcessProbe] = [
        Probe
     || #{<<"id">> := <<"processes">>} = Probe <- maps:get(<<"probes">>, Capture)
    ],
    ?assertEqual(<<"timeout">>, maps:get(<<"status">>, ProcessProbe)),
    ?assertEqual(<<"target_timeout">>, maps:get(<<"reason_code">>, ProcessProbe)).

deep_snapshot_controller_disconnect_cleans_probe_worker_test() ->
    Parent = self(),
    Controller = spawn(fun process_fixture/0),
    Killer = spawn(fun() ->
        receive
            {deep_worker, Worker} ->
                Parent ! {deep_worker, Worker},
                exit(Controller, kill)
        end
    end),
    Source = (process_source([self()], fun(_Pid, _Keys) -> undefined end))#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Killer ! {deep_worker, self()},
                receive
                    stop -> ok
                end
            end}
    },
    Result = observer_cli_snapshot:dispatch(
        Controller,
        snapshot,
        #{deep => true, test_process_source => Source},
        options(5000, redact)
    ),
    Worker =
        receive
            {deep_worker, Pid} -> Pid
        end,
    ?assertEqual(<<"error">>, maps:get(<<"status">>, Result)),
    ?assertEqual(<<"controller_disconnected">>, maps:get(<<"reason_code">>, Result)),
    ?assertNot(is_process_alive(Worker)).

local_snapshot_text_and_term_envelopes_test() ->
    Response = snapshot(#{}),
    {ok, Text} = observer_cli_cli:encode(text, Response),
    ?assertNotEqual(nomatch, binary:match(Text, <<"observer_cli.cli/v1">>)),
    {ok, Term} = observer_cli_cli:encode(term, Response),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)).

runtime_inspection_commands_test_() ->
    {timeout, 10, fun runtime_inspection_commands/0}.

runtime_inspection_commands() ->
    Memory = inspection(memory, #{}),
    ?assertMatch(
        #{
            <<"command">> := <<"memory">>,
            <<"data">> := #{
                <<"runtime">> := #{<<"word_size_bytes">> := _},
                <<"memory">> := #{<<"beam">> := #{<<"total_bytes">> := _}}
            }
        },
        Memory
    ),
    MemoryFacts = maps:get(<<"memory">>, maps:get(<<"data">>, Memory)),
    ?assertEqual(
        [<<"beam">>, <<"persistent_term">>],
        lists:sort(maps:keys(MemoryFacts))
    ),
    Schedulers = inspection(schedulers, #{duration_ms => 250}),
    Measurement = maps:get(<<"data">>, Schedulers),
    ?assertEqual(<<"valid">>, maps:get(<<"status">>, Measurement)),
    ?assertEqual(<<"opaque_same_window">>, maps:get(<<"wall_time_unit">>, Measurement)),
    ?assertEqual(
        <<"runnable_or_running_observation_not_backlog">>,
        maps:get(<<"semantics">>, maps:get(<<"run_queues">>, Measurement))
    ),
    ?assertEqual(false, maps:get(<<"snapshot_atomic">>, maps:get(<<"run_queues">>, Measurement))),
    ?assertNot(is_map_key(<<"utilization_ns">>, Measurement)),
    Distribution = inspection(distribution, #{limit => 20}),
    DistributionData = maps:get(<<"data">>, Distribution),
    ?assertEqual(
        <<"context_only_not_backlog_health">>,
        maps:get(<<"queue_semantics">>, DistributionData)
    ),
    ?assert(is_list(maps:get(<<"controller_queues">>, DistributionData))).

process_inventory_boundary_and_stable_top_n_test() ->
    Parent = self(),
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 4)],
    try
        Values = maps:from_list(lists:zip(Pids, [10, 20, 20, 5])),
        Source = process_source(Pids, fun(Pid, Keys) ->
            Parent ! {process_info_keys, Keys},
            process_info_fixture(Pid, Keys, maps:get(Pid, Values))
        end),
        Response = inspection_include(processes, #{
            sort => memory, limit => 2, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(4, maps:get(<<"scanned_count">>, Data)),
        ?assertEqual(4, maps:get(<<"eligible_count">>, Data)),
        ?assertEqual(2, maps:get(<<"returned_count">>, Data)),
        ?assertEqual(2, maps:get(<<"dropped_count">>, Data)),
        ?assertEqual(<<"fixture_list">>, maps:get(<<"inventory_path">>, Data)),
        [First, Second] = maps:get(<<"items">>, Data),
        ExpectedTie = lists:sort([lists:nth(2, Pids), lists:nth(3, Pids)]),
        ?assertEqual(
            [list_to_binary(pid_to_list(Pid)) || Pid <- ExpectedTie],
            [maps:get(<<"pid">>, First), maps:get(<<"pid">>, Second)]
        ),
        KeysList = [
            receive
                {process_info_keys, K} -> K
            end
         || _ <- Pids
        ],
        ?assert(
            lists:all(
                fun(Keys) ->
                    Keys =:= [current_function, initial_call, memory, registered_name]
                end,
                KeysList
            )
        ),
        Forbidden = [
            messages,
            dictionary,
            state,
            current_stacktrace,
            links,
            monitors,
            monitored_by,
            suspending,
            binary
        ],
        ?assertEqual([], [Key || Keys <- KeysList, Key <- Keys, lists:member(Key, Forbidden)]),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"#Ref<">>))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

binary_memory_is_explicit_and_refs_do_not_escape_test() ->
    Parent = self(),
    Pid = spawn(fun process_fixture/0),
    Reference = make_ref(),
    try
        Source = process_source([Pid], fun(_Pid, Keys) ->
            Parent ! {binary_keys, Keys},
            [
                {registered_name, []},
                {current_function, {?MODULE, process_fixture, 0}},
                {initial_call, {?MODULE, process_fixture, 0}},
                {memory, 100},
                {binary, [{Reference, 42, 1}]}
            ]
        end),
        Response = inspection(processes, #{
            sort => binary_memory, limit => 1, test_process_source => Source
        }),
        receive
            {binary_keys, Keys} -> ?assertEqual(true, lists:member(binary, Keys))
        end,
        [Item] = maps:get(<<"items">>, maps:get(<<"data">>, Response)),
        ?assertEqual(42, maps:get(<<"binary_memory_bytes">>, Item)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), ref_to_binary(Reference)))
    after
        exit(Pid, kill)
    end.

stable_process_window_lifecycle_and_late_hot_test() ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    try
        Window = observer_cli_snapshot:stable_process_window(
            #{Stable => 10, LateHot => 10, Reset => 50, Dead => 1},
            #{Stable => 20, LateHot => 1010, Reset => 2, Born => 9999},
            250
        ),
        ?assertEqual(#{Stable => 10, LateHot => 1000}, maps:get(stable, Window)),
        ?assertEqual([Born], maps:get(born, Window)),
        ?assertEqual([Dead], maps:get(dead, Window)),
        ?assertEqual([Reset], maps:get(reset, Window))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

reduction_window_keeps_full_baseline_and_stable_pids_test() ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    try
        Source = #{
            count_fun => fun() -> 5 end,
            fold =>
                {fixture_window, fun(Fun, Acc) ->
                    Sample =
                        case get(goal08_sample) of
                            undefined -> 1;
                            N -> N + 1
                        end,
                    put(goal08_sample, Sample),
                    Current =
                        case Sample of
                            1 -> [Stable, LateHot, Reset, Dead];
                            2 -> [Stable, LateHot, Reset, Born]
                        end,
                    lists:foldl(Fun, Acc, Current)
                end},
            info_fun => fun(Pid, [reductions]) ->
                Sample = get(goal08_sample),
                Values =
                    case Sample of
                        1 -> #{Stable => 10, LateHot => 10, Reset => 50, Dead => 1};
                        2 -> #{Stable => 20, LateHot => 1010, Reset => 2, Born => 9999}
                    end,
                [{reductions, maps:get(Pid, Values)}]
            end,
            sleep_fun => fun(_Duration) -> ok end,
            monotonic_fun => fun() ->
                case get(goal08_clock) of
                    undefined ->
                        put(goal08_clock, 250),
                        0;
                    N ->
                        N
                end
            end,
            whereis_fun => fun erlang:whereis/1,
            alive_fun => fun erlang:is_process_alive/1
        },
        Response = inspection_include(processes, #{
            sort => reductions, limit => 1, duration_ms => 250, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        [Item] = maps:get(<<"items">>, Data),
        ?assertEqual(list_to_binary(pid_to_list(LateHot)), maps:get(<<"pid">>, Item)),
        ?assertEqual(1000, maps:get(<<"reductions_delta">>, Item)),
        ?assertEqual(4, maps:get(<<"baseline_count">>, Data)),
        ?assertEqual(1, maps:get(<<"born_count">>, Data)),
        ?assertEqual(1, maps:get(<<"dead_count">>, Data)),
        ?assertEqual(1, maps:get(<<"reset_count">>, Data)),
        ?assertEqual(2, maps:get(<<"retained_sample_count">>, Data)),
        ?assert(maps:get(<<"working_set_estimated_bytes">>, Data) > 0)
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

process_scan_admission_refuses_before_enumeration_test() ->
    Parent = self(),
    Source = (process_source([], fun(_Pid, _Keys) -> undefined end))#{
        count_fun => fun() -> 100001 end,
        fold =>
            {must_not_scan, fun(_Fun, Acc) ->
                Parent ! scanned,
                Acc
            end}
    },
    Response = inspection(processes, #{sort => memory, test_process_source => Source}),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)),
    ?assertEqual(<<"pre_enumeration">>, maps:get(<<"admission_stage">>, Data)),
    receive
        scanned -> ?assert(false)
    after 50 -> ok
    end,
    [Probe] = maps:get(<<"probes">>, maps:get(<<"capture">>, Response)),
    ?assertEqual(<<"unavailable">>, maps:get(<<"status">>, Probe)),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, maps:get(<<"capture">>, Response))).

scheduler_window_invalidates_unsafe_samples_test() ->
    First = scheduler_sample_fixture(#{1 => {10, 20}, 3 => {5, 10}}, 0),
    Valid = observer_cli_snapshot:scheduler_window(
        First,
        scheduler_sample_fixture(#{1 => {20, 40}, 3 => {10, 20}}, 250)
    ),
    ?assertEqual(valid, maps:get(status, Valid)),
    ?assertEqual(0.5, maps:get(utilization_ratio, maps:get(normal, Valid))),
    assert_invalid_scheduler_window(
        topology_changed,
        observer_cli_snapshot:scheduler_window(
            First,
            (scheduler_sample_fixture(#{1 => {20, 40}, 3 => {10, 20}}, 250))#{
                topology => (maps:get(topology, First))#{schedulers_online => 2}
            }
        )
    ),
    assert_invalid_scheduler_window(
        missing_scheduler_id,
        observer_cli_snapshot:scheduler_window(
            First,
            scheduler_sample_fixture(#{1 => {20, 40}}, 250)
        )
    ),
    assert_invalid_scheduler_window(
        zero_denominator,
        observer_cli_snapshot:scheduler_window(First, First#{monotonic_ms => 250})
    ).

scheduler_wall_time_cleanup_is_paired_test() ->
    put(scheduler_flags, []),
    FlagFun = fun(Enabled) -> put(scheduler_flags, [Enabled | get(scheduler_flags)]) end,
    ?assertException(
        error,
        sample_failed,
        observer_cli_snapshot:measure_scheduler(
            250,
            FlagFun,
            fun() -> erlang:error(sample_failed) end,
            fun(_Duration) -> ok end
        )
    ),
    ?assertEqual([false, true], get(scheduler_flags)),
    CleanupFlagFun = fun
        (true) -> ok;
        (false) -> erlang:error(cleanup_failed)
    end,
    ?assertException(
        error,
        cleanup_failed,
        observer_cli_snapshot:measure_scheduler(
            250,
            CleanupFlagFun,
            fun() -> scheduler_sample_fixture(#{1 => {10, 20}, 3 => {5, 10}}, 0) end,
            fun(_Duration) -> ok end
        )
    ),
    erase(scheduler_flags).

distribution_controller_exclusion_and_capability_test() ->
    Controller = 'controller@host',
    Visible = 'visible@host',
    Hidden = 'hidden@host',
    Port = open_port({spawn, "cat"}, []),
    try
        Data = observer_cli_snapshot:distribution_context(
            Controller,
            [Hidden, Controller, Visible],
            [Controller, Visible],
            [Hidden],
            {ok, [{Visible, Port}, {Controller, Port}, {Hidden, alternative_carrier}]},
            {ok, 1048576},
            fun(P) -> erlang:port_info(P, queue_size) end
        ),
        ?assertEqual(2, maps:get(connected_peer_count, Data)),
        ?assertEqual(
            [#{peer => {identifier, peer, Controller}, reason => diagnostics_controller}],
            maps:get(excluded_peers, Data)
        ),
        [HiddenQueue, VisibleQueue] = maps:get(controller_queues, Data),
        ?assertEqual(unavailable, maps:get(status, HiddenQueue)),
        ?assertEqual(capability_unavailable, maps:get(reason_code, HiddenQueue)),
        ?assertEqual(available, maps:get(status, VisibleQueue)),
        ?assertEqual(unavailable, maps:get(health_inference, VisibleQueue)),
        Unavailable = observer_cli_snapshot:distribution_context(
            undefined,
            [Visible],
            [Visible],
            [],
            {unavailable, capability_unavailable},
            {unavailable, capability_unavailable},
            fun(_P) -> erlang:error(unexpected_port_info) end
        ),
        ?assertEqual(
            #{status => unavailable, reason_code => capability_unavailable},
            maps:get(controller_queue_capability, Unavailable)
        ),
        [UnavailableQueue] = maps:get(controller_queues, Unavailable),
        ?assertEqual(capability_unavailable, maps:get(reason_code, UnavailableQueue))
    after
        port_close(Port)
    end.

normalization_and_identifier_policy_test() ->
    Reference = make_ref(),
    Raw = #{
        node => {identifier, node, 'target@host'},
        pid => self(),
        same_pid => self(),
        reference => Reference,
        table => {identifier, table, Reference},
        socket => {identifier, socket, {'$socket', Reference}},
        mfa => {mfa, observer_cli_snapshot, capabilities, 0},
        values => [1, 1.5, true, false, null, value]
    },
    {ok, Redacted} = observer_cli_snapshot:normalize(Raw, redact),
    ?assertEqual(<<"node-1">>, maps:get(<<"node">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"pid">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"same_pid">>, Redacted)),
    ?assertEqual(<<"ref-1">>, maps:get(<<"reference">>, Redacted)),
    ?assertEqual(<<"table-1">>, maps:get(<<"table">>, Redacted)),
    ?assertEqual(<<"socket-1">>, maps:get(<<"socket">>, Redacted)),
    ?assertEqual(
        #{<<"module">> => <<"module-1">>, <<"function">> => <<"function-1">>, <<"arity">> => 0},
        maps:get(<<"mfa">>, Redacted)
    ),
    assert_json_safe(Redacted),
    {ok, Included} = observer_cli_snapshot:normalize(Raw, include),
    ?assertEqual(<<"target@host">>, maps:get(<<"node">>, Included)),
    ?assertEqual(list_to_binary(pid_to_list(self())), maps:get(<<"pid">>, Included)),
    ?assertEqual(
        #{
            <<"module">> => <<"observer_cli_snapshot">>,
            <<"function">> => <<"capabilities">>,
            <<"arity">> => 0
        },
        maps:get(<<"mfa">>, Included)
    ).

invalid_utf8_and_field_cap_test() ->
    {ok, Tagged} = observer_cli_snapshot:normalize(<<16#FF, 0, 16#FE>>, include),
    ?assertEqual(<<"base64">>, maps:get(<<"encoding">>, Tagged)),
    ?assertEqual(<<16#FF, 0, 16#FE>>, base64:decode(maps:get(<<"data">>, Tagged))),
    ?assertEqual(
        {error, field_too_large},
        observer_cli_snapshot:normalize(binary:copy(<<"x">>, 64 * 1024 + 1), include)
    ).

dispatch_success_and_schema_failures_test() ->
    ?assertMatch(
        #{
            <<"status">> := <<"ok">>,
            <<"result">> := #{<<"pid">> := <<"pid-1">>},
            <<"cleanup_confirmed">> := true
        },
        dispatch_observed(#{pid => self()}, 2000, redact)
    ),
    assert_error(
        <<"field_too_large">>,
        dispatch_observed(binary:copy(<<"x">>, 64 * 1024 + 1), 2000, include)
    ),
    Oversized = #{
        required => lists:duplicate(20, binary:copy(<<"x">>, 60 * 1024))
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(Oversized, 3000, include)
    ),
    assert_error(
        <<"invalid_evidence_pointer">>,
        dispatch_observed(
            #{data => #{value => 1}, findings => [#{evidence => [#{path => <<"/data/missing">>}]}]},
            2000,
            include
        )
    ).

evidence_preserving_truncation_test() ->
    Item = #{value => binary:copy(<<"x">>, 60 * 1024)},
    Items = lists:duplicate(20, Item),
    Report = #{
        items => Items,
        returned_count => 20,
        dropped_count => 0,
        truncated => false,
        findings => [#{evidence => [#{path => <<"/items/0/value">>}]}]
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = dispatch_observed(
        Report, 3000, include
    ),
    ResultItems = maps:get(<<"items">>, Result),
    ?assert(length(ResultItems) < 20),
    ?assertEqual(true, maps:get(<<"truncated">>, Result)),
    ?assertEqual(length(ResultItems), maps:get(<<"returned_count">>, Result)),
    ?assertEqual(20 - length(ResultItems), maps:get(<<"dropped_count">>, Result)),
    ?assertMatch(#{<<"value">> := _}, hd(ResultItems)),
    ?assert(erlang:external_size(Result) =< 1024 * 1024),
    ProtectedTail = Report#{
        findings := [#{evidence => [#{path => <<"/items/19/value">>}]}]
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(ProtectedTail, 3000, include)
    ).

timeout_crash_and_heap_cleanup_test_() ->
    {timeout, 10, fun timeout_crash_and_heap_cleanup/0}.

timeout_crash_and_heap_cleanup() ->
    TimeoutResult = observer_cli_snapshot:dispatch(
        self(), test_timeout, self(), options(1050, include)
    ),
    TimeoutWorker = receive_worker(),
    assert_error(<<"target_timeout">>, TimeoutResult),
    ?assertNot(is_process_alive(TimeoutWorker)),
    CrashResult = observer_cli_snapshot:dispatch(
        self(), test_crash, self(), options(2000, include)
    ),
    CrashWorker = receive_worker(),
    assert_error(<<"probe_failed">>, CrashResult),
    ?assertNot(is_process_alive(CrashWorker)),
    ?assertEqual(nomatch, binary:match(term_to_binary(CrashResult), <<"fixture_secret">>)),
    HeapResult = observer_cli_snapshot:dispatch(
        self(), test_heap, self(), (options(3000, include))#{max_heap_words => 4096}
    ),
    HeapWorker = receive_worker(),
    assert_error(<<"worker_heap_limit_exceeded">>, HeapResult),
    ?assertNot(is_process_alive(HeapWorker)).

controller_disconnect_cleanup_test_() ->
    {timeout, 10, fun controller_disconnect_cleanup/0}.

controller_disconnect_cleanup() ->
    Parent = self(),
    Controller = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Coordinator = spawn(fun() ->
        Result = observer_cli_snapshot:dispatch(
            Controller, test_timeout, Parent, options(5000, include)
        ),
        Parent ! {dispatch_result, self(), Result}
    end),
    CoordinatorRef = erlang:monitor(process, Coordinator),
    Worker = receive_worker(),
    exit(Controller, kill),
    Result =
        receive
            {dispatch_result, Coordinator, DispatchResult} -> DispatchResult
        after 2000 ->
            erlang:error(dispatch_timeout)
        end,
    assert_error(<<"controller_disconnected">>, Result),
    receive
        {'DOWN', CoordinatorRef, process, Coordinator, normal} -> ok
    after 2000 ->
        erlang:error(coordinator_cleanup_timeout)
    end,
    ?assertNot(is_process_alive(Worker)),
    ?assertNot(is_process_alive(Coordinator)).

options(Timeout, Policy) ->
    #{timeout_ms => Timeout, identifier_policy => Policy}.

dispatch_observed(Request, Timeout, Policy) ->
    Result = observer_cli_snapshot:dispatch(
        self(), test_observed_echo, {self(), Request}, options(Timeout, Policy)
    ),
    Worker = receive_worker(),
    ?assertNot(is_process_alive(Worker)),
    Result.

snapshot(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            Request,
            options(3000, redact)
        ),
    Response.

inspection(Command, Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            Command,
            Request,
            options(3000, redact)
        ),
    Response.

inspection_include(Command, Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), Command, Request, options(3000, include)
        ),
    Response.

process_source(Pids, InfoFun) ->
    #{
        count_fun => fun() -> length(Pids) end,
        fold => {fixture_list, fun(Fun, Acc) -> lists:foldl(Fun, Acc, Pids) end},
        info_fun => InfoFun,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end,
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    }.

process_info_fixture(_Pid, Keys, Metric) ->
    Values = #{
        registered_name => [],
        current_function => {?MODULE, process_fixture, 0},
        initial_call => {?MODULE, process_fixture, 0},
        memory => Metric,
        message_queue_len => Metric,
        reductions => Metric,
        total_heap_size => Metric
    },
    [{Key, maps:get(Key, Values)} || Key <- Keys].

process_fixture() ->
    receive
        stop -> ok
    end.

ref_to_binary(Reference) ->
    list_to_binary(ref_to_list(Reference)).

scheduler_sample_fixture(Wall, Monotonic) ->
    #{
        topology => #{
            schedulers_configured => 2,
            schedulers_online => 1,
            dirty_cpu_schedulers_configured => 1,
            dirty_cpu_schedulers_online => 1
        },
        wall_time => [{Id, Active, Total} || {Id, {Active, Total}} <- maps:to_list(Wall)],
        run_queue_lengths => [0, 0, 0],
        monotonic_ms => Monotonic
    }.

assert_invalid_scheduler_window(Reason, Window) ->
    ?assertEqual(invalid, maps:get(status, Window)),
    ?assertEqual(Reason, maps:get(reason_code, Window)),
    ?assertEqual(opaque_same_window, maps:get(wall_time_unit, Window)),
    ?assertEqual(false, maps:get(run_queue_snapshot_atomic, Window)).

assert_probe(Id, Required, Status, Capture) ->
    Probes = maps:get(<<"probes">>, Capture),
    [Probe] = [Item || #{<<"id">> := ProbeId} = Item <- Probes, ProbeId =:= Id],
    ?assertMatch(
        #{
            <<"required">> := Required,
            <<"status">> := Status,
            <<"reason_code">> := _,
            <<"duration_ms">> := _,
            <<"samples">> := _,
            <<"coverage">> := _
        },
        Probe
    ).

trace_forwarder(Parent) ->
    receive
        {trace, _Pid, call, Call} ->
            Parent ! {enumerator_called, Call},
            trace_forwarder(Parent);
        _Other ->
            trace_forwarder(Parent)
    end.

receive_worker() ->
    receive
        {test_worker, Worker} -> Worker
    after 2000 ->
        erlang:error(worker_start_timeout)
    end.

assert_error(ReasonCode, Result) ->
    ?assertEqual(
        #{
            <<"status">> => <<"error">>,
            <<"reason_code">> => ReasonCode,
            <<"cleanup_confirmed">> => true
        },
        Result
    ).

assert_json_safe(Map) when is_map(Map) ->
    lists:foreach(
        fun({Key, Value}) ->
            ?assert(is_binary(Key)),
            assert_json_safe(Value)
        end,
        maps:to_list(Map)
    );
assert_json_safe(List) when is_list(List) ->
    lists:foreach(fun assert_json_safe/1, List);
assert_json_safe(Value) when is_binary(Value); is_integer(Value); is_float(Value) ->
    ok;
assert_json_safe(Value) when Value =:= true; Value =:= false; Value =:= null ->
    ok.

-endif.
