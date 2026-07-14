-module(observer_cli_escriptize_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

command_request_converts_validated_cli_values_test() ->
    ?assertEqual(
        #{duration_ms => 1500},
        observer_cli_escriptize:command_request(schedulers, [], #{})
    ),
    ?assertEqual(
        #{duration_ms => 250},
        observer_cli_escriptize:command_request(schedulers, [], #{duration => "250ms"})
    ),
    ?assertEqual(
        #{sort => reductions, limit => 5, duration_ms => 250},
        observer_cli_escriptize:command_request(
            processes, [], #{sort => "reductions", limit => "5", duration => "250ms"}
        )
    ),
    ?assertEqual(
        #{target => "#Port<0.1>"},
        observer_cli_escriptize:command_request(port, ["#Port<0.1>"], #{})
    ),
    ?assertEqual(
        #{
            action => call,
            mfa => "erlang:node/0",
            pid => "<0.1.0>",
            duration_ms => 1000,
            max => 2,
            replace_existing_trace => true
        },
        observer_cli_escriptize:command_request(
            trace,
            ["call", "erlang:node/0"],
            #{
                pid => "<0.1.0>",
                duration => "1s",
                limit => "2",
                replace_existing_trace => true
            }
        )
    ),
    ?assertEqual(
        #{action => stop_all, all => false},
        observer_cli_escriptize:command_request(trace, ["stop"], #{})
    ),
    ?assertEqual(
        #{action => stop_all, all => true},
        observer_cli_escriptize:command_request(trace, ["stop"], #{all => true})
    ),
    ?assertEqual(
        #{target => <<"server">>, behavior => gen_server},
        observer_cli_escriptize:command_request(
            otp_state, ["server"], #{behavior => "gen_server"}
        )
    ),
    ?assertEqual(
        #{target => <<"machine">>, behavior => gen_statem},
        observer_cli_escriptize:command_request(
            otp_state, ["machine"], #{behavior => "gen_statem"}
        )
    ),
    ?assertEqual(
        #{target => <<"events">>, behavior => gen_event, limit => 20},
        observer_cli_escriptize:command_request(
            otp_state, ["events"], #{behavior => "gen_event"}
        )
    ),
    ?assertEqual(
        #{target => <<"events">>, behavior => gen_event, limit => 5},
        observer_cli_escriptize:command_request(
            otp_state, ["events"], #{behavior => "gen_event", limit => "5"}
        )
    ),
    ?assertEqual(
        #{app => "kernel"},
        observer_cli_escriptize:command_request(supervision_tree, [], #{app => "kernel"})
    ),
    ?assertEqual(
        #{deep => true},
        observer_cli_escriptize:command_request(snapshot, [], #{deep => true, limit => "20"})
    ),
    ?assertEqual(
        #{handler => null, tail => 200},
        observer_cli_escriptize:command_request(logs, [], #{})
    ),
    ?assertEqual(
        #{handler => <<"app file">>, tail => 500},
        observer_cli_escriptize:command_request(
            logs, [], #{handler => "app file", tail => "500"}
        )
    ),
    ?assertEqual(
        #{observe => "5s", app => "kernel"},
        observer_cli_escriptize:command_request(
            diagnose, [], #{observe => "5s", app => "kernel", limit => "20"}
        )
    ),
    ?assertEqual(
        #{sort => memory, limit => 20, duration_ms => 250},
        observer_cli_escriptize:command_request(
            sockets, [], #{sort => "memory", limit => "20", duration => "250ms"}
        )
    ).

controller_boundary_helpers_test() ->
    ?assertEqual(trace_call, observer_cli_escriptize:response_command(trace, #{action => call})),
    ?assertEqual(
        trace_stop_all,
        observer_cli_escriptize:response_command(trace, #{action => stop_all})
    ),
    ?assertEqual(memory, observer_cli_escriptize:response_command(memory, #{})),
    ?assert(observer_cli_escriptize:valid_probe_reason(<<"ok">>, null)),
    ?assert(observer_cli_escriptize:valid_probe_reason(<<"error">>, <<"probe_failed">>)),
    ?assertNot(observer_cli_escriptize:valid_probe_reason(<<"ok">>, <<"reason">>)),
    lists:foreach(
        fun({Command, Probe}) ->
            ?assertEqual(Probe, observer_cli_escriptize:required_probe_id(Command))
        end,
        [
            {memory, <<"memory">>},
            {schedulers, <<"scheduler_wall_time">>},
            {distribution, <<"distribution">>},
            {processes, <<"process_inventory">>},
            {process, <<"process_info">>},
            {port, <<"port_info">>},
            {applications, <<"application_inventory">>},
            {ets, <<"ets_inventory">>},
            {mnesia, <<"mnesia_inventory">>},
            {network, <<"network_inventory">>},
            {ports, <<"port_inventory">>},
            {sockets, <<"socket_inventory">>},
            {otp_state, <<"otp_state">>},
            {supervision_tree, <<"supervision_tree">>},
            {logs, <<"log_file_tail">>},
            {trace_call, <<"trace">>},
            {trace_stop_all, <<"trace">>},
            {unknown, undefined}
        ]
    ),
    PointerValue = #{<<"a/b">> => #{<<"~key">> => [value]}},
    ?assert(observer_cli_escriptize:pointer_exists(PointerValue, <<"/a~1b/~0key/0">>)),
    lists:foreach(
        fun(Pointer) ->
            ?assertNot(observer_cli_escriptize:pointer_exists(PointerValue, Pointer))
        end,
        [
            <<>>,
            <<"missing-slash">>,
            <<"/~2">>,
            <<"/a~1b/missing">>,
            <<"/a~1b/~0key/00">>,
            <<"/a~1b/~0key/nope">>,
            <<"/a~1b/~0key/2">>,
            <<"/a~1b/~0key/0/extra">>
        ]
    ),
    lists:foreach(
        fun(Value) -> ?assert(observer_cli_escriptize:public_value(Value, 0)) end,
        [null, true, false, 1, 1.5, <<"text">>, [1, 2], #{<<"key">> => <<"value">>}]
    ),
    ?assertNot(observer_cli_escriptize:public_value(<<16#ff>>, 0)),
    ?assertNot(observer_cli_escriptize:public_value(#{atom_key => value}, 0)),
    ?assertNot(observer_cli_escriptize:public_value(self(), 0)),
    ?assertNot(observer_cli_escriptize:public_value(value, 33)),
    ?assertEqual(<<"plain">>, observer_cli_escriptize:public_text("plain")),
    ?assertEqual(<<"base64:/w==">>, observer_cli_escriptize:public_text([255])),
    ?assertEqual(json, observer_cli_escriptize:command_format(#{json => true})),
    ?assertEqual(json, observer_cli_escriptize:command_format(#{format => "json"})),
    ?assertEqual(term, observer_cli_escriptize:command_format(#{format => "term"})),
    ?assertEqual(text, observer_cli_escriptize:command_format(#{})),
    ?assertEqual(
        trace_call, observer_cli_escriptize:command_identity(trace, ["call", "erlang:node/0"])
    ),
    ?assertEqual(
        trace_stop_all, observer_cli_escriptize:command_identity(trace, ["stop"])
    ),
    ?assertEqual(memory, observer_cli_escriptize:command_identity(memory, [])),
    ?assertEqual(<<"otp-state">>, observer_cli_escriptize:command_display(otp_state)),
    ?assertEqual(
        <<"observer_cli otp-state --help">>,
        observer_cli_escriptize:command_help_command(otp_state)
    ),
    ?assertEqual(memory, observer_cli_escriptize:command_from_args(["memory"])),
    ?assertEqual(memory, observer_cli_escriptize:command_from_args(["--json", "memory"])),
    ?assertEqual(trace_call, observer_cli_escriptize:command_from_args(["trace", "call"])),
    ?assertEqual(
        trace_call,
        observer_cli_escriptize:command_from_args(["--json", "trace", "call"])
    ),
    ?assertEqual(
        trace_stop_all, observer_cli_escriptize:command_from_args(["trace", "stop", "--all"])
    ),
    ?assertEqual(
        trace_stop_all,
        observer_cli_escriptize:command_from_args(["--json", "trace", "stop", "--all"])
    ),
    ?assertEqual(undefined, observer_cli_escriptize:command_from_args(["unknown"])),
    ?assertEqual(undefined, observer_cli_escriptize:command_from_args([])),
    ?assertEqual(json, observer_cli_escriptize:requested_format(["--json"])),
    ?assertEqual(json, observer_cli_escriptize:requested_format(["--format", "json"])),
    ?assertEqual(term, observer_cli_escriptize:requested_format(["x", "--format", "term"])),
    ?assertEqual(text, observer_cli_escriptize:requested_format([])),
    ?assertEqual(ok, observer_cli_escriptize:ensure_output_format(connect, #{})),
    ?assertEqual(ok, observer_cli_escriptize:ensure_output_format(disconnect, #{format => "term"})),
    case code:ensure_loaded(json) of
        {module, json} ->
            ?assertEqual(
                ok, observer_cli_escriptize:ensure_output_format(connect, #{json => true})
            );
        {error, _Reason} ->
            ?assertMatch(
                {error, capability, json_unavailable},
                observer_cli_escriptize:ensure_output_format(connect, #{json => true})
            )
    end.

command_output_and_error_paths_test() ->
    Response = observer_cli_cli:response(
        memory, complete, null, null, #{<<"value">> => 1}, []
    ),
    assert_halt(7, fun() ->
        observer_cli_escriptize:command_output(#{format => "term"}, Response, 7)
    end),
    assert_halt(2, fun() ->
        observer_cli_escriptize:command_error(memory, #{}, argument, invalid_arguments)
    end),
    EarlyError = assert_halt(2, fun() ->
        observer_cli_escriptize:command_error(unknown, term, argument, unknown_command)
    end),
    ?assertNotEqual(
        nomatch, binary:match(iolist_to_binary(EarlyError), <<"<<\"command\">> => null">>)
    ),
    assert_halt(3, fun() ->
        observer_cli_escriptize:command_error(memory, term, connection, connection_failed)
    end),
    Oversized = observer_cli_cli:response(
        memory,
        complete,
        null,
        null,
        #{<<"value">> => binary:copy(<<"x">>, 1024 * 1024)},
        []
    ),
    assert_halt(4, fun() ->
        observer_cli_escriptize:command_output(#{format => "term"}, Oversized, 0)
    end),
    EncodeError = #{category => internal, reason => encoding_failed},
    assert_halt(4, fun() -> observer_cli_escriptize:output_encode_error(EncodeError) end),
    assert_halt(2, fun() ->
        observer_cli_escriptize:output_command_encode_error(
            memory, json, #{category => capability, reason => json_unavailable}
        )
    end),
    assert_halt(4, fun() ->
        observer_cli_escriptize:output_command_encode_error(memory, text, EncodeError)
    end),
    assert_halt(2, fun() ->
        observer_cli_escriptize:main(["memory", "--invalid"])
    end),
    assert_halt(2, fun() -> observer_cli_escriptize:main(["version"]) end),
    assert_halt(2, fun() -> observer_cli_escriptize:main(["--bogus"]) end),
    assert_halt(2, fun() ->
        observer_cli_escriptize:main(["tui", "target@host", "cookie", "1000", "extra"])
    end),
    assert_halt(3, fun() ->
        observer_cli_escriptize:main([
            "tui", "target@host", lists:duplicate(256, $x), "1000"
        ])
    end),
    lists:foreach(
        fun(Arguments) ->
            assert_halt(2, fun() -> observer_cli_escriptize:main(Arguments) end)
        end,
        [
            ["process", "--bogus"],
            ["port"],
            ["otp-state"],
            ["supervision-tree"],
            ["trace", "call", "erlang:node/0", "--pid", "<0.1.0>"],
            ["trace", "stop"]
        ]
    ),
    assert_halt(2, fun() ->
        observer_cli_escriptize:main(["memory"])
    end),
    assert_halt(2, fun() -> observer_cli_escriptize:main(["unknown", "--help"]) end).

assert_halt(Expected, Fun) ->
    {ok, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        80,
        [],
        fun() ->
            put(observer_cli_test_halt_fun, fun(Code) -> throw({halt, Code}) end),
            put(observer_cli_test_output, capture),
            try Fun() of
                _ -> ?assert(false)
            catch
                throw:{halt, Expected} -> ok
            after
                erase(observer_cli_test_halt_fun),
                erase(observer_cli_test_output)
            end
        end
    ),
    Output.

required_modules_test_() ->
    [
        {"required modules contain roots only", fun required_modules_contain_roots_only/0},
        {"resolve target name", fun resolve_target_name_test/0},
        {"random local node name", fun random_local_node_name_test/0},
        {"ensure set env", fun ensure_set_env_test/0},
        {"ensure set env stop remote fun", fun ensure_set_env_stop_remote_fun_test/0},
        {"parse args", fun parse_args_test/0},
        {"run args", fun run_args_test/0},
        {"run command args", fun run_command_args_test/0},
        {"run args usage", fun run_args_usage/0},
        {"main usage", fun main_usage_test/0},
        {"command help", fun command_help_test/0},
        {"escript command exits", {timeout, 30, fun escript_command_exits/0}},
        {"remote load local", fun remote_load_local_test/0},
        {"remote load peer node", fun remote_load_peer_node_test/0},
        {"remote load replaces incompatible bundle",
            fun remote_load_replaces_incompatible_bundle/0},
        {"run starts distribution", fun run_starts_distribution_test/0},
        {"run waits for missing node", fun run_waits_for_missing_node_test/0},
        {"run waits for stopped peer", fun run_waits_for_stopped_peer_test/0},
        {"run preinstalled TUI once", fun run_preinstalled_tui_once_test/0},
        {"run automatically-loaded TUI once", fun run_automatically_loaded_tui_once_test/0},
        {"run rejects failed remote load", fun run_rejects_failed_remote_load_test/0},
        {"run name mode mismatch", fun run_name_mode_mismatch_test/0},
        {"run unreachable node", {timeout, 20000, fun run_unreachable_node_test/0}},
        {"refuse pre-distributed controller", fun refuse_pre_distributed_controller/0},
        {"stop before connect on random failure", fun random_failure_stops_before_connect/0},
        {"forward remaining command deadline", fun forward_remaining_command_deadline/0},
        {"dynamic controller handshake", {timeout, 20000, fun dynamic_controller_handshake/0}},
        {"invalid remote dispatch contract",
            {timeout, 20000, fun invalid_remote_dispatch_contract/0}},
        {"active context lifecycle", {timeout, 40000, fun active_context_lifecycle/0}},
        {"connect cleanup preserves context", fun connect_cleanup_preserves_context/0},
        {"connect missing diagnostics", {timeout, 30000, fun connect_missing_diagnostics/0}},
        {"connect incompatible diagnostics",
            {timeout, 30000, fun connect_incompatible_diagnostics/0}},
        {"connect sanitizes hostile diagnostics",
            {timeout, 30000, fun connect_sanitizes_hostile_diagnostics/0}},
        {"missing capability", {timeout, 20000, fun missing_capability/0}},
        {"incompatible capability", {timeout, 20000, fun incompatible_capability/0}},
        {"undefined capability callback", {timeout, 20000, fun undefined_capability/0}},
        {"failing capability callback", {timeout, 20000, fun failing_capability/0}},
        {"non-map capability callback", {timeout, 20000, fun nonmap_capability/0}},
        {"hostile capability values", {timeout, 20000, fun hostile_capability/0}}
    ].

required_modules_contain_roots_only() ->
    ok = application:load(
        application_spec(#{
            application => formatter_dependency,
            applications => [kernel, stdlib],
            included_applications => [],
            modules => [formatter_dependency_module]
        })
    ),
    ok = application:load(
        application_spec(#{
            application => formatter_included,
            applications => [kernel, stdlib],
            included_applications => [],
            modules => [formatter_included_module]
        })
    ),
    ok = application:load(
        application_spec(#{
            application => formatter_root,
            applications => [kernel, stdlib, formatter_dependency],
            included_applications => [formatter_included],
            modules => [formatter_root_module, observer_cli]
        })
    ),
    try
        Roots = [observer_cli, recon, formatter_root],
        Expected = lists:usort(
            lists:append([observer_cli_escriptize:application_modules(App) || App <- Roots])
        ),
        Modules = observer_cli_escriptize:required_modules(Roots),
        ?assertEqual(Expected, Modules),
        ?assertNot(lists:member(formatter_dependency_module, Modules)),
        ?assertNot(lists:member(formatter_included_module, Modules))
    after
        application:unload(formatter_root),
        application:unload(formatter_included),
        application:unload(formatter_dependency)
    end.

application_spec(#{
    application := Application,
    applications := Applications,
    included_applications := IncludedApplications,
    modules := Modules
}) ->
    {application, Application, [
        {modules, Modules},
        {included_applications, IncludedApplications},
        {applications, Applications}
    ]}.

resolve_target_name_test() ->
    {Node1, shortnames} = observer_cli_escriptize:resolve_target_name("target@host"),
    ?assertEqual(list_to_atom("target@host"), Node1),
    {Node2, longnames} = observer_cli_escriptize:resolve_target_name("target@host.example"),
    ?assertEqual(list_to_atom("target@host.example"), Node2),
    {ok, Host} = inet:gethostname(),
    {Node3, shortnames} = observer_cli_escriptize:resolve_target_name("target"),
    ?assertEqual(list_to_atom("target@" ++ Host), Node3).

random_local_node_name_test() ->
    Name = observer_cli_escriptize:random_local_node_name(),
    ?assert(lists:prefix("observer_cli_", Name)).

ensure_set_env_test() ->
    application:unset_env(test_env_app, sample),
    observer_cli_escriptize:ensure_set_env(test_env_app, [{sample, 1}]),
    ?assertEqual(1, application:get_env(test_env_app, sample, undefined)),
    observer_cli_escriptize:ensure_set_env(test_env_app, [{sample, 2}]),
    ?assertEqual(1, application:get_env(test_env_app, sample, undefined)),
    application:unset_env(test_env_app, sample).

ensure_set_env_stop_remote_fun_test() ->
    Parent = self(),
    StopFun = fun() -> Parent ! stop_remote_called end,
    application:set_env(test_stop_app, test_stop_remote, true),
    application:set_env(test_stop_app, test_stop_remote_fun, StopFun),
    try
        ?assertEqual(ok, observer_cli_escriptize:ensure_set_env(test_stop_app, [{sample, 1}])),
        receive
            stop_remote_called -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        application:unset_env(test_stop_app, test_stop_remote),
        application:unset_env(test_stop_app, test_stop_remote_fun)
    end.

parse_args_test() ->
    ?assertEqual(
        {ok, #{
            route => tui,
            target => "target@host",
            cookie => undefined,
            interval => 1500
        }},
        observer_cli_escriptize:parse_args(["tui", "target@host"])
    ),
    ?assertEqual(
        {ok, #{
            route => tui,
            target => "target@host",
            cookie => "test_cookie",
            interval => 2000
        }},
        observer_cli_escriptize:parse_args(["tui", "target@host", "test_cookie", "2000"])
    ),
    ?assertMatch(
        {error, #{reason := {unknown_command, "target@host"}}},
        observer_cli_escriptize:parse_args(["target@host"])
    ),
    ?assertMatch(
        {error, #{reason := {unknown_command, "target@host"}}},
        observer_cli_escriptize:parse_args(["target@host", "test_cookie", "2000"])
    ),
    ?assertMatch({error, #{exit_code := 2}}, observer_cli_escriptize:parse_args([])),
    ?assertMatch(
        {error, #{exit_code := 2}},
        observer_cli_escriptize:parse_args(["tui", "target@host", "cookie"])
    ),
    ?assertMatch(
        {error, #{exit_code := 2}},
        observer_cli_escriptize:parse_args(["tui", "target@host", "cookie", "2000", "extra"])
    ),
    ?assertMatch(
        {error, #{exit_code := 2}},
        observer_cli_escriptize:parse_args(["tui", "target@host", "cookie", "not-an-integer"])
    ),
    ?assertMatch(
        {error, #{reason := {unknown_option, "--load-diagnostics"}}},
        observer_cli_escriptize:parse_args([
            "connect",
            "--node",
            "target@host",
            "--cookie-env",
            "ERL_COOKIE",
            "--load-diagnostics"
        ])
    ).

logs_real_response_and_validator_test() ->
    Dir = temporary_directory("observer_cli_logs_validator"),
    Path = filename:join(Dir, "app.log"),
    Handler = observer_cli_logs_validator,
    Request = #{handler => <<"observer_cli_logs_validator">>, tail => 200},
    try
        ok = file:write_file(Path, <<"before\nvalidator marker\n">>),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [append, raw]}
        }),
        Response = dispatch_logs(Request),
        ?assertEqual(
            ok, observer_cli_escriptize:validate_response(logs, include, node(), Response)
        ),
        ?assert(
            observer_cli_escriptize:validate_logs_response(
                Request, Response, atom_to_binary(node())
            )
        ),
        ?assertEqual({ok, Response, 0}, observer_cli_escriptize:dispatch_response(Response)),
        assert_log_response_mutations_rejected(Request, Response),
        assert_log_contract_boundaries(Request, Response)
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

logs_partial_and_error_rows_validate_test() ->
    Dir = temporary_directory("observer_cli_logs_rows"),
    Path = filename:join(Dir, "large.log"),
    Handler = observer_cli_logs_rows,
    Request = #{handler => <<"observer_cli_logs_rows">>, tail => 200},
    try
        ok = file:write_file(Path, binary:copy(<<"x">>, 40000)),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [append, raw]}
        }),
        Partial = dispatch_logs(Request),
        ?assertMatch(#{<<"outcome">> := <<"partial">>}, Partial),
        ?assert(
            observer_cli_escriptize:validate_logs_response(
                Request, Partial, atom_to_binary(node())
            )
        ),
        ?assertEqual({ok, Partial, 3}, observer_cli_escriptize:dispatch_response(Partial)),
        ok = logger:remove_handler(Handler),
        UnsupportedRequest = #{handler => <<"default">>, tail => 200},
        Unsupported = dispatch_logs(UnsupportedRequest),
        ?assertMatch(
            #{
                <<"outcome">> := <<"error">>,
                <<"meta">> := #{
                    <<"capture">> := #{
                        <<"probes">> := [#{<<"reason_code">> := <<"unsupported_log_handler">>}]
                    }
                }
            },
            Unsupported
        ),
        ?assert(
            observer_cli_escriptize:validate_logs_response(
                UnsupportedRequest, Unsupported, atom_to_binary(node())
            )
        ),
        ?assertEqual({ok, Unsupported, 2}, observer_cli_escriptize:dispatch_response(Unsupported))
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

logs_reason_matrix_accepts_valid_rows_test() ->
    Target = atom_to_binary(node()),
    Auto = #{handler => null, tail => 200},
    Handler = <<"matrix_handler">>,
    Explicit = #{handler => Handler, tail => 200},
    Supported = log_source_summary(Handler, <<"logger_std_h_file">>, true, null),
    Selected = Supported#{
        <<"configured_path">> => <<"/tmp/matrix.log">>,
        <<"active_handler_fd_match">> => <<"unknown">>
    },
    Unsupported = log_source_summary(
        <<"console">>, <<"other">>, false, <<"unsupported_log_handler">>
    ),
    Rows =
        [
            {Auto,
                log_error_response(
                    Target, <<"unsupported_target_platform">>, [], null, [], false, 0, 0
                )},
            {Auto,
                log_error_response(
                    Target, <<"scan_budget_exceeded">>, [], null, [], true, 0, 0
                )},
            {Auto,
                log_error_response(
                    Target,
                    <<"log_handler_required">>,
                    [
                        log_source_summary(<<"first">>, <<"logger_std_h_file">>, true, null),
                        log_source_summary(<<"second">>, <<"logger_std_h_file">>, true, null)
                    ],
                    null,
                    [<<"source_classification_complete">>],
                    true,
                    2,
                    0
                )},
            {Auto,
                log_error_response(
                    Target,
                    <<"log_source_unavailable">>,
                    [Unsupported],
                    null,
                    [<<"source_classification_complete">>],
                    true,
                    1,
                    0
                )},
            {Explicit,
                log_error_response(
                    Target,
                    <<"log_handler_not_found">>,
                    [],
                    null,
                    [<<"source_classification_complete">>],
                    false,
                    0,
                    0
                )},
            {Explicit,
                log_error_response(
                    Target,
                    <<"log_handler_not_found">>,
                    [],
                    null,
                    [<<"source_classification_complete">>],
                    false,
                    1,
                    0
                )},
            {Explicit,
                log_error_response(
                    Target,
                    <<"log_file_unavailable">>,
                    [Supported],
                    Selected,
                    [<<"source_classification_complete">>, <<"source_selected">>],
                    false,
                    1,
                    1
                )},
            {Explicit, log_byte_partial_response(Target, Supported, Selected)}
        ] ++
            [
                {Explicit,
                    log_error_response(
                        Target,
                        Reason,
                        [log_source_summary(Handler, Kind, false, Reason)],
                        null,
                        [<<"source_classification_complete">>],
                        false,
                        1,
                        0
                    )}
             || {Reason, Kind} <- [
                    {<<"unsupported_log_handler">>, <<"other">>},
                    {<<"unsupported_file_modes">>, <<"logger_std_h_file">>},
                    {<<"log_path_unrepresentable">>, <<"logger_std_h_file">>},
                    {<<"invalid_log_handler_config">>, <<"other">>}
                ]
            ],
    lists:foreach(
        fun({Request, Response}) ->
            ?assert(observer_cli_escriptize:validate_logs_response(Request, Response, Target))
        end,
        Rows
    ),
    ?assertNot(
        observer_cli_escriptize:validate_logs_response(
            Explicit,
            log_error_response(
                Target,
                <<"unsupported_file_modes">>,
                [
                    log_source_summary(
                        Handler, <<"other">>, false, <<"unsupported_file_modes">>
                    )
                ],
                null,
                [<<"source_classification_complete">>],
                false,
                1,
                0
            ),
            Target
        )
    ).

logs_peer_historical_marker_test_() ->
    {timeout, 30, fun logs_peer_historical_marker/0}.

logs_peer_historical_marker() ->
    with_distribution(fun(_Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_logs")}),
        Dir = temporary_directory("observer_cli_logs_peer"),
        Path = filename:join(Dir, "app.log"),
        Archive = filename:join(Dir, "app.log.1"),
        Handler = observer_cli_logs_peer,
        Request = #{handler => <<"observer_cli_logs_peer">>, tail => 20},
        try
            ok = observer_cli_escriptize:remote_load(Node),
            ok = erpc:call(Node, logger, add_handler, [
                Handler,
                logger_std_h,
                #{config => #{type => file, file => Path, modes => [write, raw]}}
            ]),
            ok = erpc:call(Node, logger, notice, ["observer-cli pre-connect marker"]),
            ok = erpc:call(Node, logger_std_h, filesync, [Handler]),
            BeforeIds = erpc:call(Node, logger, get_handler_ids, []),
            BeforeConfig = erpc:call(Node, logger, get_handler_config, [Handler]),
            {ok, Response, 0} = observer_cli_escriptize:run_dispatch(
                Node, logs, Request, #{}, 10000
            ),
            Lines = maps:get(
                <<"lines">>, maps:get(<<"tail">>, maps:get(<<"data">>, Response))
            ),
            ?assert(
                lists:any(
                    fun(Line) -> binary:match(Line, <<"pre-connect marker">>) =/= nomatch end,
                    Lines
                )
            ),
            ?assertEqual(BeforeIds, erpc:call(Node, logger, get_handler_ids, [])),
            ?assertEqual(BeforeConfig, erpc:call(Node, logger, get_handler_config, [Handler])),
            ok = file:rename(Path, Archive),
            {ok, Missing, 3} = observer_cli_escriptize:run_dispatch(
                Node, logs, Request, #{}, 10000
            ),
            ?assertMatch(
                #{
                    <<"outcome">> := <<"error">>,
                    <<"meta">> := #{
                        <<"capture">> := #{
                            <<"probes">> := [
                                #{<<"reason_code">> := <<"log_file_unavailable">>}
                            ]
                        }
                    }
                },
                Missing
            ),
            ?assertEqual(false, filelib:is_file(Path)),
            ?assertEqual(BeforeIds, erpc:call(Node, logger, get_handler_ids, [])),
            ?assertEqual(BeforeConfig, erpc:call(Node, logger, get_handler_config, [Handler]))
        after
            _ = erpc:call(Node, logger, remove_handler, [Handler]),
            peer:stop(Peer),
            file:del_dir_r(Dir)
        end
    end).

dispatch_logs(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), logs, Request, #{timeout_ms => 5000, identifier_policy => include}
        ),
    Response.

log_error_response(Target, Reason, Sources, Selected, Coverage, Enumerated, Lookups, Attempts) ->
    observer_cli_cli:response(
        logs,
        error,
        #{<<"node">> => Target, <<"otp_release">> => <<"29">>},
        log_capture(
            <<"unavailable">>, Reason, Coverage, Enumerated, Lookups, Attempts
        ),
        #{<<"sources">> => Sources, <<"selected_source">> => Selected, <<"tail">> => null},
        []
    ).

log_byte_partial_response(Target, Source, Selected) ->
    observer_cli_cli:response(
        logs,
        partial,
        #{<<"node">> => Target, <<"otp_release">> => <<"29">>},
        log_capture(
            <<"error">>,
            <<"log_byte_cap_reached">>,
            [
                <<"source_classification_complete">>,
                <<"source_selected">>,
                <<"path_prechecked">>,
                <<"fd_identity_verified">>,
                <<"bytes_captured">>,
                <<"post_read_verified">>
            ],
            false,
            2,
            1
        ),
        #{
            <<"sources">> => [Source],
            <<"selected_source">> => Selected,
            <<"tail">> => #{
                <<"scope">> => <<"configured_path">>,
                <<"active_handler_fd_match">> => <<"unknown">>,
                <<"visibility">> => <<"reader_visible">>,
                <<"command_filesync_requested">> => false,
                <<"consistency">> => <<"non_atomic">>,
                <<"content_trust">> => <<"untrusted">>,
                <<"requested_lines">> => 200,
                <<"returned_lines">> => 1,
                <<"captured_eof_bytes">> => 65537,
                <<"bytes_read">> => 65536,
                <<"has_more">> => true,
                <<"content_truncated">> => true,
                <<"truncation_reasons">> => [<<"byte_cap">>],
                <<"truncated_line_indexes">> => [0],
                <<"lines">> => [<<"retained fragment">>]
            }
        },
        []
    ).

log_source_summary(Id, Kind, Supported, Reason) ->
    #{
        <<"id">> => Id,
        <<"addressable">> => true,
        <<"handler_kind">> => Kind,
        <<"supported">> => Supported,
        <<"reason_code">> => Reason
    }.

log_capture(Status, Reason, Coverage, Enumerated, Lookups, Attempts) ->
    #{
        <<"started_at">> => <<"2026-07-13T00:00:00.000Z">>,
        <<"finished_at">> => <<"2026-07-13T00:00:00.000Z">>,
        <<"duration_ms">> => 0,
        <<"probes">> => [
            #{
                <<"id">> => <<"log_file_tail">>,
                <<"required">> => true,
                <<"status">> => Status,
                <<"reason_code">> => Reason,
                <<"duration_ms">> => 0,
                <<"samples">> => 1,
                <<"coverage">> => Coverage
            }
        ],
        <<"observer_effects">> => [
            #{
                <<"id">> => <<"diagnostics_worker">>,
                <<"affected_facts">> => [
                    <<"process_count">>,
                    <<"port_count">>,
                    <<"memory">>,
                    <<"io">>,
                    <<"garbage_collection">>
                ]
            },
            #{
                <<"id">> => <<"module_load">>,
                <<"module_loaded_before_sample">> => true
            },
            #{
                <<"id">> => <<"configured_log_read">>,
                <<"handler_ids_enumerated">> => Enumerated,
                <<"handler_config_lookups">> => Lookups,
                <<"read_attempts">> => Attempts,
                <<"raw_read_cap_bytes">> => 65536,
                <<"atime_may_change">> => true,
                <<"consistency">> => <<"non_atomic">>,
                <<"command_filesync_attempted">> => false
            }
        ]
    }.

assert_log_response_mutations_rejected(Request, Response) ->
    Data = maps:get(<<"data">>, Response),
    Tail = maps:get(<<"tail">>, Data),
    Selected = maps:get(<<"selected_source">>, Data),
    Capture = response_capture(Response),
    [Probe] = maps:get(<<"probes">>, Capture),
    Effects = maps:get(<<"observer_effects">>, Capture),
    LogEffect = lists:last(Effects),
    Mutations = [
        Response#{
            <<"issues">> := [
                #{
                    <<"severity">> => <<"warning">>,
                    <<"class">> => <<"partial">>,
                    <<"reason_code">> => <<"unexpected">>,
                    <<"message">> => null
                }
            ]
        },
        Response#{
            <<"data">> := Data#{
                <<"selected_source">> := Selected#{
                    <<"configured_path">> := <<"relative.log">>
                }
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"selected_source">> := Selected#{
                    <<"supported">> := false
                }
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"sources">> := maps:get(<<"sources">>, Data) ++
                    maps:get(<<"sources">>, Data)
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"tail">> := Tail#{
                    <<"requested_lines">> := 199
                }
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"tail">> := Tail#{
                    <<"returned_lines">> := maps:get(<<"returned_lines">>, Tail) + 1
                }
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"tail">> := Tail#{
                    <<"bytes_read">> := maps:get(<<"bytes_read">>, Tail) + 1
                }
            }
        },
        Response#{
            <<"data">> := Data#{
                <<"tail">> := Tail#{
                    <<"content_truncated">> := true,
                    <<"truncation_reasons">> := [<<"line_cap">>],
                    <<"truncated_line_indexes">> := [0]
                }
            }
        },
        replace_capture(Response, Capture#{<<"duration_ms">> := 2}),
        replace_capture(Response, Capture#{
            <<"probes">> := [
                Probe#{
                    <<"coverage">> := [<<"source_selected">>, <<"source_classification_complete">>]
                }
            ]
        }),
        replace_capture(Response, Capture#{
            <<"probes">> := [
                Probe#{
                    <<"status">> := <<"error">>, <<"reason_code">> := <<"log_line_cap_reached">>
                }
            ]
        }),
        replace_capture(Response, Capture#{<<"observer_effects">> := Effects ++ [LogEffect]}),
        replace_capture(Response, Capture#{
            <<"observer_effects">> :=
                lists:sublist(Effects, length(Effects) - 1) ++
                [
                    LogEffect#{
                        <<"handler_config_lookups">> := 67
                    }
                ]
        }),
        replace_capture(Response, Capture#{
            <<"observer_effects">> :=
                lists:sublist(Effects, length(Effects) - 1) ++
                [
                    LogEffect#{
                        <<"read_attempts">> := 0
                    }
                ]
        })
    ],
    lists:foreach(
        fun(Malformed) ->
            ?assertNot(
                observer_cli_escriptize:validate_logs_response(
                    Request, Malformed, atom_to_binary(node())
                )
            )
        end,
        Mutations
    ),
    ?assertNot(
        observer_cli_escriptize:validate_logs_response(
            Request#{tail := 199}, Response, atom_to_binary(node())
        )
    ).

assert_log_contract_boundaries(Request, Response) ->
    Target = atom_to_binary(node()),
    Valid = fun(CandidateRequest, CandidateResponse, ExpectedTarget) ->
        observer_cli_escriptize:validate_logs_response(
            CandidateRequest, CandidateResponse, ExpectedTarget
        )
    end,
    Data = maps:get(<<"data">>, Response),
    [Source] = maps:get(<<"sources">>, Data),
    Tail = maps:get(<<"tail">>, Data),
    Lines = maps:get(<<"lines">>, Tail),
    Capture = response_capture(Response),
    [Probe] = maps:get(<<"probes">>, Capture),
    [Diagnostics, Module | _] = Effects = maps:get(<<"observer_effects">>, Capture),
    LogRead = lists:last(Effects),
    WithData = fun(NewData) -> Response#{<<"data">> := NewData} end,
    WithTail = fun(NewTail) -> WithData(Data#{<<"tail">> := NewTail}) end,
    WithLines = fun(NewLines) -> WithTail(Tail#{<<"lines">> := NewLines}) end,
    ReplaceFirstLine = fun(Line) -> WithLines([Line | tl(Lines)]) end,
    Rejected = [
        {invalid, Response, Target},
        {Request, Response, <<"other@host">>},
        {Request, WithData(#{}), Target},
        {Request, WithData(Data#{<<"sources">> := invalid}), Target},
        {Request, WithData(Data#{<<"sources">> := [#{}]}), Target},
        {Request,
            WithData(Data#{
                <<"sources">> := [Source#{<<"id">> := <<255>>}],
                <<"selected_source">> := null,
                <<"tail">> := null
            }), Target},
        {Request,
            WithData(Data#{
                <<"sources">> := [Source#{<<"id">> := invalid}],
                <<"selected_source">> := null,
                <<"tail">> := null
            }), Target},
        {Request,
            WithData(Data#{
                <<"sources">> := [
                    Source#{
                        <<"id">> := <<"--unaddressable">>,
                        <<"addressable">> := false,
                        <<"reason_code">> := <<"unaddressable_handler_id">>
                    }
                ],
                <<"selected_source">> := null,
                <<"tail">> := null
            }), Target},
        {Request, WithData(Data#{<<"selected_source">> := invalid}), Target},
        {Request, ReplaceFirstLine(<<255>>), Target},
        {Request,
            ReplaceFirstLine(#{
                <<"encoding">> => <<"base64">>, <<"data">> => base64:encode(<<"valid">>)
            }), Target},
        {Request, ReplaceFirstLine(#{<<"encoding">> => <<"base64">>, <<"data">> => <<"!">>}),
            Target},
        {Request, ReplaceFirstLine(invalid), Target},
        {Request,
            WithTail(Tail#{
                <<"content_truncated">> := true,
                <<"truncation_reasons">> := [<<"line_cap">>, <<"byte_cap">>],
                <<"truncated_line_indexes">> := [0]
            }), Target},
        {Request, WithTail(Tail#{<<"truncated_line_indexes">> := invalid}), Target},
        {Request, WithTail(Tail#{<<"truncated_line_indexes">> := [0, 0]}), Target},
        {Request, replace_capture(Response, invalid), Target},
        {Request,
            replace_capture(Response, Capture#{
                <<"probes">> := [Probe#{<<"coverage">> := invalid}]
            }), Target},
        {Request,
            replace_capture(Response, Capture#{
                <<"observer_effects">> := [invalid, Module, LogRead]
            }), Target},
        {Request,
            replace_capture(Response, Capture#{
                <<"observer_effects">> := [Diagnostics, invalid, LogRead]
            }), Target},
        {Request,
            replace_capture(Response, Capture#{
                <<"observer_effects">> := [Diagnostics, Module, invalid, LogRead]
            }), Target},
        {Request,
            replace_capture(Response, Capture#{
                <<"observer_effects">> := [Diagnostics, Module, invalid]
            }), Target}
    ],
    lists:foreach(
        fun({CandidateRequest, CandidateResponse, ExpectedTarget}) ->
            ?assertNot(Valid(CandidateRequest, CandidateResponse, ExpectedTarget))
        end,
        Rejected
    ),
    Base64Line = #{
        <<"encoding">> => <<"base64">>, <<"data">> => base64:encode(<<255>>)
    },
    ?assert(Valid(Request, ReplaceFirstLine(Base64Line), Target)),
    EmptyTail = Tail#{
        <<"returned_lines">> := 0,
        <<"captured_eof_bytes">> := 0,
        <<"bytes_read">> := 0,
        <<"has_more">> := false,
        <<"content_truncated">> := false,
        <<"truncation_reasons">> := [],
        <<"truncated_line_indexes">> := [],
        <<"lines">> := []
    },
    ?assert(Valid(Request, WithTail(EmptyTail), Target)),
    CombinedTail = Tail#{
        <<"returned_lines">> := 2,
        <<"captured_eof_bytes">> := 65537,
        <<"bytes_read">> := 65536,
        <<"has_more">> := true,
        <<"content_truncated">> := true,
        <<"truncation_reasons">> := [<<"byte_cap">>, <<"line_cap">>],
        <<"truncated_line_indexes">> := [0, 1],
        <<"lines">> := [<<"x">>, binary:copy(<<"y">>, 32765)]
    },
    CombinedCapture = Capture#{
        <<"probes">> := [
            Probe#{<<"status">> := <<"error">>, <<"reason_code">> := <<"log_byte_cap_reached">>}
        ]
    },
    Combined = replace_capture(
        (WithTail(CombinedTail))#{<<"outcome">> := <<"partial">>}, CombinedCapture
    ),
    ?assert(Valid(Request, Combined, Target)),
    ExplicitPlatform = log_error_response(
        Target, <<"unsupported_target_platform">>, [], null, [], false, 0, 0
    ),
    ?assert(Valid(Request, ExplicitPlatform, Target)),
    UnsupportedMatrix = log_error_response(
        Target,
        <<"log_handler_not_found">>,
        [
            log_source_summary(
                maps:get(handler, Request),
                <<"other">>,
                false,
                <<"unsupported_log_handler">>
            )
        ],
        null,
        [<<"source_classification_complete">>],
        false,
        1,
        0
    ),
    ?assertNot(Valid(Request, UnsupportedMatrix, Target)),
    ok.

run_args_test() ->
    ?assertEqual(
        {ok, "target@host", test_cookie, 2000},
        observer_cli_escriptize:run_args(
            ["tui", "target@host", "test_cookie", "2000"],
            fun(TargetNode, Cookie, Interval) -> {ok, TargetNode, Cookie, Interval} end
        )
    ).

run_command_args_test() ->
    Parent = self(),
    ?assertMatch(
        {ok, #{route := command, command := memory}},
        observer_cli_escriptize:run_args(
            ["memory", "--json"],
            fun(_TargetNode, _Cookie, _Interval) -> Parent ! run_called end
        )
    ),
    receive
        run_called -> ?assert(false)
    after 0 ->
        ok
    end.

run_args_usage() ->
    Parent = self(),
    ?assertEqual(
        ok,
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_args(
                    ["tui", "target@host", "cookie"],
                    fun(_TargetNode, _Cookie, _Interval) -> Parent ! run_called end
                )
            end
        )
    ),
    receive
        run_called -> ?assert(false)
    after 0 ->
        ok
    end.

main_usage_test() ->
    observer_cli_test_io:with_input(
        [],
        fun() ->
            ?assertEqual(ok, observer_cli_escriptize:main([]))
        end
    ).

command_help_test() ->
    {ok, TopHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(TopHelp, [
        "observer_cli connect --node NODE",
        "Diagnostics:",
        "otp-state PID_OR_NAME",
        "--help, -h",
        "help COMMAND",
        "--version",
        "Run 'observer_cli COMMAND --help'"
    ]),
    assert_help_width(TopHelp),
    {ok, HelpAlias} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["help"]) end
    ),
    ?assertEqual(TopHelp, HelpAlias),
    {ok, DashedHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["--format", "term", "--help"]) end
    ),
    ?assertEqual(TopHelp, DashedHelp),
    {ok, ShortHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["-h"]) end
    ),
    ?assertEqual(TopHelp, ShortHelp),
    Commands = [
        "connect",
        "status",
        "disconnect",
        "snapshot",
        "diagnose",
        "memory",
        "schedulers",
        "distribution",
        "processes",
        "process",
        "applications",
        "ets",
        "mnesia",
        "network",
        "ports",
        "port",
        "sockets",
        "otp-state",
        "supervision-tree",
        "trace"
    ],
    lists:foreach(
        fun(Command) ->
            {ok, Help} = observer_cli_test_io:capture_with_geometry(
                24, 80, [], fun() -> observer_cli_escriptize:main([Command, "--help"]) end
            ),
            observer_cli_test_io:assert_stable_fragments(Help, [
                "Usage:", "observer_cli " ++ Command
            ]),
            assert_help_width(Help)
        end,
        Commands
    ),
    {ok, MemoryHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["memory", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(MemoryHelp, ["BEAM memory", "allocator"]),
    {ok, MemoryHelpAlias} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["help", "memory"]) end
    ),
    ?assertEqual(MemoryHelp, MemoryHelpAlias),
    {ok, ProcessesHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["processes", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(ProcessesHelp, [
        "message_queue_len", "250ms..10s", "--sort reductions"
    ]),
    {ok, ProcessHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["process", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(ProcessHelp, [
        "bounded, normalized current stacktrace", "Messages", "dictionary"
    ]),
    {ok, OtpStateHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["otp-state", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(OtpStateHelp, [
        "full OTP behavior state copy",
        "full-copy",
        "up to 5s",
        "gen_statem",
        "output soft cap",
        "minimum 10s"
    ]),
    {ok, NetworkHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["network", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(NetworkHelp, ["recv_cnt", "send_cnt", "cnt"]),
    {ok, TraceHelp} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["trace", "--help"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(TraceHelp, [
        "node-global",
        "external/global",
        "loaded, exported MFA",
        "local intra-module calls are excluded",
        "never arguments, returns, exceptions, or stacks",
        "legacy process/port trace flags",
        "and tracers plus static call patterns",
        "without restoring prior state",
        "processes or ports occupying its fixed",
        "tracer/formatter names",
        "not directly cleared",
        "fixed-name occupant can disable one",
        "trace call",
        "trace stop --all"
    ]),
    lists:foreach(
        fun({Arguments, Fragments}) ->
            {ok, Help} = observer_cli_test_io:capture_with_geometry(
                24, 80, [], fun() -> observer_cli_escriptize:main(Arguments) end
            ),
            observer_cli_test_io:assert_stable_fragments(Help, Fragments),
            assert_help_width(Help)
        end,
        [
            {["tui", "--help"], ["REFRESH_MS", "positional COOKIE"]},
            {["trace", "call", "--help"], [
                "module:function/arity",
                "Only external/global",
                "local intra-module calls are excluded",
                "Setup and teardown clear node-wide",
                "process/port trace flags",
                "on-load and call-memory",
                "Prior state is not restored",
                "process or port",
                "occupying a",
                "fixed tracer or formatter name",
                "not directly",
                "occupant can disable one",
                "Live target-local tracee PID; required",
                "duration plus seven seconds",
                "returns, exceptions, and stacks are never collected",
                "included by default; use --redact",
                "data.trace.trace_complete=true",
                "loss, module change, or interference",
                "At most 1000 events are returned",
                "response-cap",
                "outcome=complete and exit 0",
                "cleanup_unconfirmed is outcome=error/exit 4",
                "may omit trace data",
                "100ms..60s; 10s by default",
                "1..1000 events; 100 by default",
                "Recon burst breaker, not a pacer",
                "trip event included; the first event after an",
                "expired window is forwarded and resets the",
                "total capture may exceed N",
                "across windows; conflicts with --limit",
                "--replace-existing-trace"
            ]},
            {["trace", "stop", "--help"], [
                "node-wide legacy process/port trace flags",
                "call-memory",
                "process or port occupying a fixed tracer",
                "not directly",
                "occupant can disable one",
                "--all acknowledges",
                "Explicit timeout minimum: 5s",
                "When trace data is present",
                "public stop response has events=[]",
                "original waiting",
                "With no owned observer_cli",
                "trace, cleanup still runs",
                "cleanup_unconfirmed",
                "outcome=error, exit 4",
                "verify target trace state",
                "--all"
            ]},
            {["logs", "--help"], [
                "once from one trusted",
                "configured path",
                "does not wait for new lines",
                "does not flush Logger buffers",
                "sensitive and untrusted",
                "--redact and --include-identifiers",
                "--handler HANDLER_ID",
                "--tail LINES",
                "Remote-operation deadline; 10s default, max 120s",
                "64 KiB",
                "32 KiB"
            ]},
            {["snapshot", "--deep", "--help"], ["snapshot", "--deep"]}
        ]
    ),
    {ok, Version} = observer_cli_test_io:capture_with_geometry(
        24, 80, [], fun() -> observer_cli_escriptize:main(["--version"]) end
    ),
    observer_cli_test_io:assert_stable_fragments(Version, [
        "observer_cli 2.0.0", "observer_cli.cli/v1", "protocol 1", "controller OTP"
    ]),
    ?assertEqual(nonode@nohost, node()).

assert_help_width(Help) ->
    Binary = iolist_to_binary(Help),
    lists:foreach(
        fun(Line) -> ?assert(byte_size(Line) =< 79) end,
        binary:split(Binary, <<"\n">>, [global])
    ).

escript_command_exits() ->
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    CliBeamDir = filename:join(AppDir, "test"),
    EscriptizeBeamDir = filename:join(AppDir, "ebin"),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_exit_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".escript"
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts -pa ~ts~n"
        "main([\"command\"]) -> observer_cli_escriptize:main([\"memory\", \"--format\", \"term\", \"--invalid\"]);~n"
        "main([\"trace_call\"]) -> no_context(), observer_cli_escriptize:main([\"trace\", \"call\", \"erlang:node/0\", \"--pid\", \"<0.1.0>\", \"--replace-existing-trace\", \"--format\", \"term\"]);~n"
        "main([\"trace_stop\"]) -> no_context(), observer_cli_escriptize:main([\"trace\", \"stop\", \"--all\", \"--format\", \"term\"]);~n"
        "main([\"trace_text\"]) -> no_context(), observer_cli_escriptize:main([\"trace\", \"call\", \"erlang:node/0\", \"--pid\", \"<0.1.0>\", \"--replace-existing-trace\"]);~n"
        "main([\"success\"]) -> erlang:halt(observer_cli_escriptize:response_exit_code(#{<<\"outcome\">> => <<\"complete\">>}));~n"
        "main([\"diagnose_findings\"]) -> erlang:halt(observer_cli_escriptize:response_exit_code(#{<<\"outcome\">> => <<\"complete\">>, <<\"command\">> => <<\"diagnose\">>, <<\"data\">> => #{<<\"findings\">> => [finding]}}));~n"
        "main([Category]) -> erlang:halt(observer_cli_cli:exit_code(list_to_atom(Category))).~n"
        "no_context() -> os:putenv(\"HOME\", filename:join(os:getenv(\"TMPDIR\", \"/tmp\"), \"observer_cli_no_context_\" ++ integer_to_list(erlang:unique_integer([positive])))).~n",
        [CliBeamDir, EscriptizeBeamDir]
    ),
    ok = file:write_file(Script, Contents),
    try
        {2, CommandOutput} = run_escript(Escript, [Script, "command"]),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"observer_cli.cli/v1">>)),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"capture">>)),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"null">>)),
        {2, TraceCall} = run_escript(Escript, [Script, "trace_call"]),
        ?assertNotEqual(nomatch, binary:match(TraceCall, <<"<<\"trace_call\">>">>)),
        {2, TraceStop} = run_escript(Escript, [Script, "trace_stop"]),
        ?assertNotEqual(nomatch, binary:match(TraceStop, <<"<<\"trace_stop_all\">>">>)),
        {2, TraceText} = run_escript(Escript, [Script, "trace_text"]),
        ?assertNotEqual(nomatch, binary:match(TraceText, <<"observer_cli trace call:">>)),
        ?assertEqual(nomatch, binary:match(TraceText, <<"trace_call:">>)),
        ?assertEqual({0, <<>>}, run_escript(Escript, [Script, "success"])),
        ?assertEqual({1, <<>>}, run_escript(Escript, [Script, "diagnose_findings"])),
        ?assertEqual({3, <<>>}, run_escript(Escript, [Script, "connection"])),
        ?assertEqual({4, <<>>}, run_escript(Escript, [Script, "internal"]))
    after
        file:delete(Script)
    end.

command_deadline_uses_remaining_budget_test() ->
    ?assertEqual({ok, 1234}, observer_cli_escriptize:command_timeout(#{}, 1234)),
    ?assertEqual(
        {ok, 1000}, observer_cli_escriptize:command_timeout(#{timeout => "1s"}, 2000)
    ),
    ?assertEqual({error, target_timeout}, observer_cli_escriptize:command_timeout(#{}, 0)).

target_error_exit_classification_test() ->
    ?assertEqual(
        {error, schema, invalid_schema},
        observer_cli_escriptize:target_dispatch_error(<<"invalid_schema">>)
    ),
    lists:foreach(
        fun(Reason) ->
            ?assertEqual(
                {error, schema, binary_to_existing_atom(Reason)},
                observer_cli_escriptize:target_dispatch_error(Reason)
            )
        end,
        [
            <<"field_too_large">>,
            <<"response_too_deep">>,
            <<"response_too_large">>,
            <<"invalid_evidence_pointer">>,
            <<"invalid_identifier">>,
            <<"invalid_identifier_policy">>
        ]
    ),
    ?assertEqual(
        {error, cleanup, cleanup_unconfirmed},
        observer_cli_escriptize:target_dispatch_error(<<"cleanup_unconfirmed">>)
    ),
    ?assertEqual(
        {error, internal, internal_error},
        observer_cli_escriptize:target_dispatch_error(<<"internal_error">>)
    ),
    ?assertEqual(
        {error, capability, capability_unavailable},
        observer_cli_escriptize:target_dispatch_error(<<"capability_unavailable">>)
    ),
    ?assertEqual(
        {error, safety_refusal, worker_heap_limit_exceeded},
        observer_cli_escriptize:target_dispatch_error(<<"worker_heap_limit_exceeded">>)
    ),
    ?assertEqual(
        {error, required_probe, <<"target_timeout">>},
        observer_cli_escriptize:target_dispatch_error(<<"target_timeout">>)
    ).

direct_unavailable_exit_classification_test() ->
    Response = fun(Reason) ->
        #{
            <<"outcome">> => <<"error">>,
            <<"issues">> => [],
            <<"meta">> => #{
                <<"capture">> => #{
                    <<"probes">> => [
                        #{<<"status">> => <<"unavailable">>, <<"reason_code">> => Reason}
                    ]
                }
            }
        }
    end,
    Capability = Response(<<"capability_unavailable">>),
    ?assertEqual(
        {ok, Capability, 2}, observer_cli_escriptize:dispatch_response(Capability)
    ),
    Budget = Response(<<"scan_budget_exceeded">>),
    ?assertEqual({ok, Budget, 3}, observer_cli_escriptize:dispatch_response(Budget)),
    Internal0 = Response(<<"capture_internal_error">>),
    InternalCapture = maps:get(<<"capture">>, maps:get(<<"meta">>, Internal0)),
    [InternalProbe] = maps:get(<<"probes">>, InternalCapture),
    Internal = Internal0#{
        <<"meta">> := #{
            <<"capture">> => InternalCapture#{
                <<"probes">> := [InternalProbe#{<<"status">> := <<"error">>}]
            }
        }
    },
    ?assertEqual({ok, Internal, 4}, observer_cli_escriptize:dispatch_response(Internal)).

outcome_exit_matrix_test() ->
    Complete = #{<<"outcome">> => <<"complete">>, <<"command">> => <<"snapshot">>},
    Diagnose = Complete#{<<"command">> := <<"diagnose">>, <<"data">> => #{<<"findings">> => []}},
    Partial = Complete#{<<"outcome">> := <<"partial">>},
    Error = fun(Class) ->
        #{
            <<"outcome">> => <<"error">>,
            <<"issues">> => [observer_cli_cli:error(Class, fixture_error)]
        }
    end,
    SafetyWarning = (observer_cli_cli:error(safety_refusal, global_trace_replacement))#{
        <<"severity">> := <<"warning">>
    },
    lists:foreach(
        fun({Response, ExitCode}) ->
            ?assertEqual(ExitCode, observer_cli_escriptize:response_exit_code(Response)),
            ?assertEqual(
                {ok, Response, ExitCode}, observer_cli_escriptize:dispatch_response(Response)
            )
        end,
        [
            {Complete, 0},
            {Diagnose, 0},
            {Diagnose#{<<"data">> := #{<<"findings">> => [finding]}}, 1},
            {Partial, 3},
            {Error(argument), 2},
            {Error(capability), 2},
            {
                (Error(capability))#{
                    <<"issues">> := [
                        observer_cli_cli:error(capability, fixture_error), SafetyWarning
                    ]
                },
                2
            },
            {Error(safety_refusal), 3},
            {Error(connection), 3},
            {Error(cleanup), 4},
            {Error(schema), 4},
            {Error(internal), 4}
        ]
    ).

optional_unavailable_probe_keeps_complete_test() ->
    Base = valid_controller_response(snapshot, atom_to_binary(node())),
    Optional = (fixture_probe(<<"optional_capability">>))#{
        <<"required">> := false,
        <<"status">> := <<"unavailable">>,
        <<"reason_code">> := <<"capability_unavailable">>,
        <<"samples">> := 0
    },
    Response = replace_capture(
        Base,
        (response_capture(Base))#{
            <<"probes">> := maps:get(<<"probes">>, response_capture(Base)) ++ [Optional]
        }
    ),
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(snapshot, include, node(), Response)
    ),
    ?assertEqual({ok, Response, 0}, observer_cli_escriptize:dispatch_response(Response)),
    ?assertEqual([], maps:get(<<"issues">>, Response)),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            snapshot, include, node(), Response#{<<"outcome">> := <<"error">>}
        )
    ),
    [Required | Rest] = maps:get(<<"probes">>, response_capture(Response)),
    FailedRequired = Required#{
        <<"status">> := <<"timeout">>,
        <<"reason_code">> := <<"target_timeout">>,
        <<"samples">> := 0
    },
    FatalPartial = replace_capture(
        Response#{
            <<"outcome">> := <<"partial">>,
            <<"issues">> := [observer_cli_cli:error(cleanup, cleanup_unconfirmed)]
        },
        (response_capture(Response))#{<<"probes">> := [FailedRequired | Rest]}
    ),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            snapshot, include, node(), FatalPartial
        )
    ).

probe_failure_is_not_duplicated_test() ->
    Response = diagnostic_response(partial, []),
    ?assertEqual([], maps:get(<<"issues">>, Response)),
    ?assertEqual([], maps:get(<<"skipped">>, maps:get(<<"data">>, Response))),
    ?assertMatch(
        [#{<<"reason_code">> := <<"required_coverage_incomplete">>}],
        maps:get(<<"probes">>, response_capture(Response))
    ),
    Duplicate = (observer_cli_cli:error(partial, required_coverage_incomplete))#{
        <<"severity">> := <<"warning">>
    },
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            diagnose,
            redact,
            ignored,
            Response#{<<"issues">> := [Duplicate]}
        )
    ),
    Data = maps:get(<<"data">>, Response),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            diagnose,
            redact,
            ignored,
            Response#{
                <<"data">> := Data#{
                    <<"skipped">> := [
                        #{
                            <<"id">> => <<"duplicate">>,
                            <<"reason_code">> => <<"required_coverage_incomplete">>
                        }
                    ]
                }
            }
        )
    ).

trace_incomplete_data_is_complete_test() ->
    Base = valid_controller_response(trace_call, atom_to_binary(node())),
    PartialTrace = maps:remove(
        <<"module_reloaded">>,
        (fixture_trace_capture())#{
            <<"status">> := <<"partial">>,
            <<"trace_complete">> := false,
            <<"truncated">> := true,
            <<"dropped_count">> := null
        }
    ),
    lists:foreach(
        fun(Command) ->
            Reason =
                case Command of
                    trace_call -> <<"duration_elapsed">>;
                    trace_stop_all -> <<"stopped">>
                end,
            Response = Base#{
                <<"command">> := atom_to_binary(Command),
                <<"data">> := #{
                    <<"reason">> => Reason,
                    <<"trace">> => PartialTrace#{<<"reason">> := Reason}
                }
            },
            ?assertEqual(
                ok,
                observer_cli_escriptize:validate_response(
                    Command, include, node(), Response
                )
            ),
            ?assertEqual(
                {ok, Response, 0}, observer_cli_escriptize:dispatch_response(Response)
            )
        end,
        [trace_call, trace_stop_all]
    ).

trace_payload_mutations_are_rejected_test() ->
    Base = valid_controller_response(trace_call, atom_to_binary(node())),
    Trace = fixture_trace_capture(),
    Event = #{
        <<"tracee">> => <<"<0.1.0>">>,
        <<"mfa">> => maps:get(<<"mfa">>, Trace),
        <<"offset_ms">> => 0
    },
    lists:foreach(
        fun(ValidTrace) ->
            Data0 = maps:get(<<"data">>, Base),
            ?assertEqual(
                ok,
                observer_cli_escriptize:validate_response(
                    trace_call,
                    include,
                    node(),
                    Base#{<<"data">> := Data0#{<<"trace">> := ValidTrace}}
                )
            )
        end,
        [
            Trace#{
                <<"status">> := <<"partial">>,
                <<"trace_complete">> := false,
                <<"truncated">> := true,
                <<"dropped_count">> := 1
            },
            Trace#{
                <<"status">> := <<"partial">>,
                <<"trace_complete">> := false,
                <<"module_reloaded">> := true
            },
            Trace#{
                <<"status">> := <<"partial">>,
                <<"trace_complete">> := false,
                <<"truncated">> := true,
                <<"dropped_count">> := null,
                <<"interference_detected">> := true
            }
        ]
    ),
    Mutations = [
        Trace#{<<"trace_complete">> := false},
        Trace#{<<"truncated">> := false, <<"dropped_count">> := 1},
        Trace#{<<"truncated">> := true, <<"dropped_count">> := 1},
        Trace#{
            <<"status">> := <<"partial">>,
            <<"trace_complete">> := false,
            <<"module_reloaded">> := false
        },
        Trace#{
            <<"status">> := <<"partial">>,
            <<"trace_complete">> := false,
            <<"truncated">> := true,
            <<"dropped_count">> := null,
            <<"module_reloaded">> := true
        },
        Trace#{<<"interference_detected">> := true},
        Trace#{<<"events">> := [Event#{<<"tracee">> := 42}]},
        Trace#{<<"events">> := [Event#{<<"tracee">> := <<"<0.2.0>">>}]},
        Trace#{<<"events">> := lists:duplicate(1001, Event)},
        Trace#{<<"unexpected">> => true},
        maps:remove(<<"reason">>, Trace),
        maps:remove(<<"mfa">>, Trace),
        Trace#{<<"mfa">> := (maps:get(<<"mfa">>, Trace))#{<<"arity">> := 256}},
        Trace#{<<"cleanup_confirmed">> := yes},
        Trace#{<<"cleanup_confirmed">> := false},
        Trace#{<<"status">> := <<"unknown">>},
        Trace#{<<"module_reloaded">> => true},
        Trace#{<<"module_reloaded">> => yes}
    ],
    lists:foreach(
        fun(Mutation) ->
            Data = maps:get(<<"data">>, Base),
            Response = Base#{<<"data">> := Data#{<<"trace">> := Mutation}},
            ?assertEqual(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(trace_call, include, node(), Response)
            )
        end,
        Mutations
    ),
    Data = maps:get(<<"data">>, Base),
    ReasonMismatch = Base#{
        <<"data">> := Data#{<<"reason">> := <<"duration_elapsed">>, <<"trace">> := Trace}
    },
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(trace_call, include, node(), ReasonMismatch)
    ).

trace_envelope_mutations_are_rejected_test() ->
    Base = valid_controller_response(trace_call, atom_to_binary(node())),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_call, include, node(), Base)
    ),
    [Issue] = maps:get(<<"issues">>, Base),
    Capture = response_capture(Base),
    [Probe] = maps:get(<<"probes">>, Capture),
    [Effect] = maps:get(<<"observer_effects">>, Capture),
    ReplaceProbes = fun(Probes) ->
        replace_capture(Base, Capture#{<<"probes">> := Probes})
    end,
    ReplaceEffects = fun(Effects) ->
        replace_capture(Base, Capture#{<<"observer_effects">> := Effects})
    end,
    Mutations = [
        Base#{<<"issues">> := []},
        Base#{<<"issues">> := [Issue#{<<"reason_code">> := <<"totally_unrelated">>}]},
        Base#{<<"issues">> := [Issue#{<<"class">> := <<"capability">>}]},
        Base#{<<"issues">> := [Issue#{<<"message">> := null}]},
        Base#{<<"issues">> := [Issue, Issue]},
        ReplaceEffects([]),
        ReplaceEffects([#{<<"id">> => <<"bogus">>}]),
        ReplaceEffects([Effect, Effect]),
        ReplaceEffects([Effect#{<<"extra">> => true}]),
        ReplaceEffects([Effect#{<<"controller">> := <<>>}]),
        ReplaceProbes([Probe#{<<"coverage">> := []}]),
        ReplaceProbes([Probe#{<<"coverage">> := [<<"bogus">>]}]),
        ReplaceProbes([Probe#{<<"samples">> := 2}]),
        ReplaceProbes([Probe#{<<"duration_ms">> := 999}])
    ],
    lists:foreach(
        fun(Response) ->
            ?assertEqual(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(
                    trace_call, include, node(), Response
                )
            )
        end,
        Mutations
    ).

trace_cleanup_outcome_mismatches_are_rejected_test() ->
    Base = valid_controller_response(trace_call, atom_to_binary(node())),
    Trace = maps:remove(
        <<"module_reloaded">>,
        (fixture_trace_capture())#{
            <<"status">> := <<"partial">>,
            <<"reason">> := <<"cleanup_unconfirmed">>,
            <<"trace_complete">> := false,
            <<"truncated">> := true,
            <<"dropped_count">> := null,
            <<"cleanup_confirmed">> := false
        }
    ),
    Data = #{<<"reason">> => <<"cleanup_unconfirmed">>, <<"trace">> => Trace},
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call, include, node(), Base#{<<"data">> := Data}
        )
    ),
    [Probe] = maps:get(<<"probes">>, response_capture(Base)),
    ErrorProbe = Probe#{
        <<"status">> := <<"error">>, <<"reason_code">> := <<"cleanup_unconfirmed">>
    },
    ErrorCapture = (response_capture(Base))#{<<"probes">> := [ErrorProbe]},
    ValidError = replace_capture(
        Base#{<<"outcome">> := <<"error">>, <<"data">> := Data}, ErrorCapture
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_call, include, node(), ValidError)
    ),
    CleanupClaimed = Trace#{<<"cleanup_confirmed">> := true},
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            ValidError#{
                <<"data">> := Data#{<<"trace">> := CleanupClaimed}
            }
        )
    ),
    OtherReason = <<"capture_internal_error">>,
    OtherProbe = ErrorProbe#{<<"reason_code">> := OtherReason},
    OtherTrace = Trace#{<<"reason">> := OtherReason},
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            replace_capture(
                ValidError#{
                    <<"data">> := #{<<"reason">> => OtherReason, <<"trace">> => OtherTrace}
                },
                ErrorCapture#{<<"probes">> := [OtherProbe]}
            )
        )
    ).

trace_reason_outcome_mismatches_are_rejected_test() ->
    Call = valid_controller_response(trace_call, atom_to_binary(node())),
    CallData = maps:get(<<"data">>, Call),
    CallTrace = maps:get(<<"trace">>, CallData),
    FailureReason = <<"capture_internal_error">>,
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            Call#{
                <<"data">> := #{
                    <<"reason">> => FailureReason,
                    <<"trace">> => CallTrace#{<<"reason">> := FailureReason}
                }
            }
        )
    ),
    DurationReason = <<"duration_elapsed">>,
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            Call#{
                <<"data">> := #{
                    <<"reason">> => DurationReason,
                    <<"trace">> => CallTrace#{<<"reason">> := DurationReason}
                }
            }
        )
    ),
    Stop = valid_controller_response(trace_stop_all, atom_to_binary(node())),
    StopData = maps:get(<<"data">>, Stop),
    StopTrace = maps:get(<<"trace">>, StopData),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_stop_all,
            include,
            node(),
            Stop#{
                <<"data">> := #{
                    <<"reason">> => <<"limit_reached">>,
                    <<"trace">> => StopTrace#{<<"reason">> := <<"limit_reached">>}
                }
            }
        )
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_stop_all,
            include,
            node(),
            Stop#{
                <<"data">> := #{
                    <<"reason">> => <<"stopped">>,
                    <<"trace">> => CallTrace#{<<"reason">> := <<"stopped">>}
                }
            }
        )
    ),
    [Probe] = maps:get(<<"probes">>, response_capture(Call)),
    ErrorTrace = maps:remove(
        <<"module_reloaded">>,
        CallTrace#{
            <<"status">> := <<"partial">>,
            <<"reason">> := FailureReason,
            <<"trace_complete">> := false,
            <<"truncated">> := true,
            <<"dropped_count">> := null
        }
    ),
    ErrorResponse = replace_capture(
        Call#{
            <<"outcome">> := <<"error">>,
            <<"data">> := #{<<"reason">> => FailureReason, <<"trace">> => ErrorTrace}
        },
        (response_capture(Call))#{
            <<"probes">> := [
                Probe#{<<"status">> := <<"error">>, <<"reason_code">> := FailureReason}
            ]
        }
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_call, include, node(), ErrorResponse)
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            ErrorResponse#{
                <<"data">> := #{
                    <<"reason">> => FailureReason,
                    <<"trace">> => ErrorTrace#{<<"module_reloaded">> => false}
                }
            }
        )
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            ErrorResponse#{
                <<"data">> := #{
                    <<"reason">> => FailureReason,
                    <<"trace">> => CallTrace#{<<"reason">> := FailureReason}
                }
            }
        )
    ),
    SuccessReason = <<"limit_reached">>,
    [ErrorProbe] = maps:get(<<"probes">>, response_capture(ErrorResponse)),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            replace_capture(
                ErrorResponse#{
                    <<"data">> := #{
                        <<"reason">> => SuccessReason,
                        <<"trace">> => ErrorTrace#{<<"reason">> := SuccessReason}
                    }
                },
                (response_capture(ErrorResponse))#{
                    <<"probes">> := [ErrorProbe#{<<"reason_code">> := SuccessReason}]
                }
            )
        )
    ).

trace_stop_events_are_rejected_test() ->
    Base = valid_controller_response(trace_stop_all, atom_to_binary(node())),
    Data = maps:get(<<"data">>, Base),
    Trace = maps:get(<<"trace">>, Data),
    Event = #{
        <<"tracee">> => maps:get(<<"tracee">>, Trace),
        <<"mfa">> => maps:get(<<"mfa">>, Trace),
        <<"offset_ms">> => 0
    },
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_stop_all,
            include,
            node(),
            Base#{<<"data">> := Data#{<<"trace">> := Trace#{<<"events">> := [Event]}}}
        )
    ).

controller_validates_real_trace_responses_test_() ->
    {timeout, 5, fun controller_validates_real_trace_responses/0}.

controller_validates_real_trace_responses() ->
    Base = #{
        action => call,
        pid => list_to_binary(pid_to_list(self())),
        duration_ms => 100,
        max => 1,
        replace_existing_trace => true
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Missing} =
        observer_cli_snapshot:dispatch(
            self(),
            trace,
            Base#{mfa => <<"erlang:node/255">>},
            #{timeout_ms => 2000, identifier_policy => include}
        ),
    ?assertMatch(
        #{
            <<"outcome">> := <<"error">>,
            <<"issues">> := [
                #{<<"class">> := <<"capability">>, <<"reason_code">> := <<"mfa_unavailable">>}
            ],
            <<"meta">> := #{<<"capture">> := null}
        },
        Missing
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_call, include, node(), Missing)
    ),
    lists:foreach(
        fun(Response) ->
            ?assertEqual(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(
                    trace_call, include, node(), Response
                )
            )
        end,
        [#{}, maps:remove(<<"schema">>, Missing), maps:remove(<<"command">>, Missing)]
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            Missing#{<<"issues">> := not_a_list}
        )
    ),
    [MissingIssue] = maps:get(<<"issues">>, Missing),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call,
            include,
            node(),
            Missing#{
                <<"issues">> := [MissingIssue#{<<"reason_code">> := <<"totally_unrelated">>}]
            }
        )
    ),
    #{<<"status">> := <<"ok">>, <<"result">> := Partial} =
        observer_cli_snapshot:dispatch(
            self(),
            trace,
            Base#{mfa => <<"erlang:node/0">>},
            #{timeout_ms => 2000, identifier_policy => include}
        ),
    ?assertMatch(
        #{
            <<"outcome">> := <<"complete">>,
            <<"meta">> := #{
                <<"capture">> := #{<<"probes">> := [#{<<"status">> := <<"ok">>}]}
            },
            <<"issues">> := [#{<<"severity">> := <<"warning">>}],
            <<"data">> := #{<<"trace">> := #{<<"trace_complete">> := false}}
        },
        Partial
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_call, include, node(), Partial)
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_call, include, node(), maps:remove(<<"data">>, Partial)
        )
    ),
    ?assertEqual({ok, Partial, 0}, observer_cli_escriptize:dispatch_response(Partial)).

controller_response_validation_test() ->
    Response = valid_controller_response(memory, <<"node@host">>),
    Target = 'node@host',
    ?assertEqual(
        [<<"command">>, <<"data">>, <<"issues">>, <<"meta">>, <<"outcome">>, <<"schema">>],
        lists:sort(maps:keys(Response))
    ),
    ?assertEqual(ok, observer_cli_escriptize:validate_response(memory, include, Target, Response)),
    OtpState = valid_controller_response(otp_state, <<"node@host">>),
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(otp_state, include, Target, OtpState)
    ),
    [OtpProbe] = maps:get(<<"probes">>, response_capture(OtpState)),
    MismatchReason = <<"behavior_shape_mismatch">>,
    Mismatch0 = OtpState#{
        <<"outcome">> := <<"partial">>,
        <<"data">> := (maps:get(<<"data">>, OtpState))#{
            <<"status">> := <<"error">>,
            <<"reason_code">> => MismatchReason,
            <<"structural_validation">> := <<"failed">>,
            <<"current_state_shape">> := null,
            <<"data_shape">> := null,
            <<"visited_node_count">> := 0
        }
    },
    Mismatch = replace_capture(
        Mismatch0,
        (response_capture(OtpState))#{
            <<"probes">> := [
                OtpProbe#{<<"status">> := <<"error">>, <<"reason_code">> := MismatchReason}
            ]
        }
    ),
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(otp_state, include, Target, Mismatch)
    ),
    ?assertEqual({ok, Mismatch, 3}, observer_cli_escriptize:dispatch_response(Mismatch)),
    BadOutcome = OtpState#{<<"data">> := maps:get(<<"data">>, Mismatch)},
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(otp_state, include, Target, BadOutcome)
    ),
    ?assertEqual(
        {error, schema, invalid_command_response},
        observer_cli_escriptize:validated_response(
            otp_state,
            include,
            Target,
            BadOutcome,
            fun observer_cli_escriptize:dispatch_response/1
        )
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            otp_state, include, Target, Mismatch#{<<"outcome">> := <<"complete">>}
        )
    ),
    RedactedOtp = valid_controller_response(otp_state, <<"node-1">>),
    RedactedStateData = (maps:get(<<"data">>, RedactedOtp))#{
        <<"current_state">> := <<"label-1">>, <<"current_state_identity">> := <<"available">>
    },
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(
            otp_state, redact, Target, RedactedOtp#{<<"data">> := RedactedStateData}
        )
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            otp_state,
            redact,
            Target,
            RedactedOtp#{
                <<"data">> := RedactedStateData#{<<"current_state">> := <<"atom:idle">>}
            }
        )
    ),
    [Handler] = maps:get(<<"handlers">>, otp_state_data(gen_event)),
    RedactedHandler = Handler#{
        <<"module">> := <<"module-1">>,
        <<"id">> := <<"label-1">>,
        <<"id_identity">> := <<"available">>
    },
    RedactedEventData = (otp_state_data(gen_event))#{<<"handlers">> := [RedactedHandler]},
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(
            otp_state, redact, Target, RedactedOtp#{<<"data">> := RedactedEventData}
        )
    ),
    lists:foreach(
        fun(RawHandler) ->
            ?assertEqual(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(
                    otp_state,
                    redact,
                    Target,
                    RedactedOtp#{
                        <<"data">> := RedactedEventData#{<<"handlers">> := [RawHandler]}
                    }
                )
            )
        end,
        [
            RedactedHandler#{<<"id">> := <<"atom:handler">>},
            RedactedHandler#{<<"module">> := <<"handler">>}
        ]
    ),
    lists:foreach(
        fun(Malformed) ->
            ?assertMatch(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(memory, include, Target, Malformed)
            )
        end,
        [
            Response#{<<"schema">> := <<"observer_cli.cli/v2">>},
            Response#{<<"command">> := <<"snapshot">>},
            Response#{<<"extra">> => true},
            Response#{<<"data">> := #{<<"unsafe">> => self()}},
            old_seven_field_response(memory, <<"node@host">>),
            replace_target(
                Response, #{<<"node">> => <<"other@host">>, <<"otp_release">> => <<"29">>}
            ),
            replace_target(
                Response, #{<<"node">> => <<"node@host">>, <<"otp_release">> => <<"latest">>}
            ),
            replace_capture(
                Response,
                (response_capture(Response))#{
                    <<"started_at">> := <<"yesterday">>
                }
            ),
            replace_capture(Response, (response_capture(Response))#{<<"probes">> := []})
        ]
    ),
    Redacted = valid_controller_response(memory, <<"node-1">>),
    ?assertEqual(ok, observer_cli_escriptize:validate_response(memory, redact, Target, Redacted)),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(memory, redact, Target, Response)
    ),
    lists:foreach(
        fun(Node) ->
            Invalid = replace_target(
                Redacted, #{<<"node">> => Node, <<"otp_release">> => <<"29">>}
            ),
            ?assertMatch(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(memory, redact, Target, Invalid)
            )
        end,
        [<<"node-secret">>, <<"node-0">>, <<"node-01">>]
    ).

controller_response_contract_rejections_test() ->
    Target = 'node@host',
    Response = valid_controller_response(memory, <<"node@host">>),
    ErrorResponse0 = Response#{
        <<"outcome">> := <<"error">>,
        <<"data">> := null,
        <<"issues">> := [
            observer_cli_cli:error(capability, cleanup_unconfirmed)
        ]
    },
    ErrorResponse = replace_capture(ErrorResponse0, null),
    Wrapper = #{
        <<"items">> => [],
        <<"scanned_count">> => -1,
        <<"eligible_count">> => 0,
        <<"returned_count">> => 1,
        <<"dropped_count">> => 0,
        <<"complete">> => true,
        <<"truncated">> => false
    },
    Diagnose0 = diagnostic_response(complete, []),
    Finding = #{<<"evidence">> => [#{<<"path">> => <<"/data/context/missing">>}]},
    Diagnose = Diagnose0#{
        <<"data">> := (maps:get(<<"data">>, Diagnose0))#{<<"findings">> := [Finding]}
    },
    InvalidPointer0 = diagnostic_response(complete, [#{<<"id">> => <<"invalid.pointer">>}]),
    InvalidPointerData0 = maps:get(<<"data">>, InvalidPointer0),
    InvalidPointerContext0 = maps:get(<<"context">>, InvalidPointerData0),
    [InvalidPointerFinding0] = maps:get(<<"findings">>, InvalidPointerData0),
    [InvalidPointerEvidence0] = maps:get(<<"evidence">>, InvalidPointerFinding0),
    InvalidPointer = InvalidPointer0#{
        <<"data">> := InvalidPointerData0#{
            <<"context">> := InvalidPointerContext0#{<<"~2">> => true},
            <<"findings">> := [
                InvalidPointerFinding0#{
                    <<"evidence">> := [
                        InvalidPointerEvidence0#{<<"path">> := <<"/data/context/~2">>}
                    ]
                }
            ]
        }
    },
    OffsetTime = replace_capture(
        Response,
        (response_capture(Response))#{
            <<"started_at">> := <<"2026-07-11T00:00:00+08:00">>,
            <<"finished_at">> := <<"2026-07-11T00:00:01+08:00">>
        }
    ),
    Leaked0 = diagnostic_response(complete, []),
    LeakedData0 = maps:get(<<"data">>, Leaked0),
    LeakedContext0 = maps:get(<<"context">>, LeakedData0),
    Leaked = Leaked0#{
        <<"data">> := LeakedData0#{
            <<"context">> := LeakedContext0#{<<"leaked_pid">> => <<"<0.123.0>">>}
        }
    },
    lists:foreach(
        fun({Command, Policy, Malformed}) ->
            ?assertMatch(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(Command, Policy, Target, Malformed)
            )
        end,
        [
            {memory, include, ErrorResponse},
            {memory, include, ErrorResponse#{
                <<"issues">> := [
                    (observer_cli_cli:error(cleanup, cleanup_unconfirmed))#{<<"extra">> => true}
                ]
            }},
            {memory, include, Response#{<<"data">> := Wrapper}},
            {memory, include, OffsetTime},
            {diagnose, redact, Diagnose0#{<<"data">> := #{}}},
            {diagnose, redact, Diagnose},
            {diagnose, redact, InvalidPointer},
            {diagnose, redact, Leaked}
        ]
    ).

controller_validates_real_resource_responses_test() ->
    #{<<"status">> := <<"ok">>, <<"result">> := MissingProcess} =
        observer_cli_snapshot:dispatch(
            self(), process, #{target => <<"definitely_missing">>}, #{
                timeout_ms => 5000, identifier_policy => include
            }
        ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(
            process, include, node(), MissingProcess
        )
    ),
    #{<<"status">> := <<"ok">>, <<"result">> := MissingPort} =
        observer_cli_snapshot:dispatch(
            self(), port, #{target => <<"#Port<0.999999999>">>}, #{
                timeout_ms => 5000, identifier_policy => include
            }
        ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(port, include, node(), MissingPort)
    ),
    lists:foreach(
        fun({Command, Request}) ->
            #{<<"status">> := <<"ok">>, <<"result">> := Response} =
                observer_cli_snapshot:dispatch(self(), Command, Request, #{
                    timeout_ms => 5000, identifier_policy => include
                }),
            ?assertEqual(
                ok,
                observer_cli_escriptize:validate_response(
                    Command, include, node(), Response
                )
            )
        end,
        [
            {processes, #{sort => memory, limit => 1}},
            {ets, #{sort => memory, limit => 1}},
            {sockets, #{sort => io, limit => 1}}
        ]
    ),
    #{<<"status">> := <<"ok">>, <<"result">> := ProcessResponse} =
        observer_cli_snapshot:dispatch(
            self(), processes, #{sort => memory, limit => 1}, #{
                timeout_ms => 5000, identifier_policy => include
            }
        ),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            processes, include, node(), ProcessResponse#{<<"data">> := #{}}
        )
    ),
    ProcessData = maps:get(<<"data">>, ProcessResponse),
    ?assertMatch(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            processes,
            include,
            node(),
            ProcessResponse#{
                <<"data">> := ProcessData#{<<"dropped_count">> := 999999}
            }
        )
    ),
    Response = valid_controller_response(memory, atom_to_binary(node())),
    ResponseData = maps:get(<<"data">>, Response),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(
            memory,
            include,
            node(),
            Response#{
                <<"data">> := ResponseData#{
                    <<"audit">> => #{<<"returned_count">> => 1}
                }
            }
        )
    ),
    #{<<"status">> := <<"ok">>, <<"result">> := Unavailable} =
        observer_cli_snapshot:dispatch(
            self(),
            sockets,
            #{
                sort => io,
                limit => 1,
                test_socket_source => #{available_fun => fun() -> false end}
            },
            #{timeout_ms => 5000, identifier_policy => include}
        ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(sockets, include, node(), Unavailable)
    ),
    ?assertMatch({ok, Unavailable, 2}, observer_cli_escriptize:dispatch_response(Unavailable)),
    #{<<"status">> := <<"ok">>, <<"result">> := Refused} =
        observer_cli_snapshot:dispatch(
            self(),
            processes,
            #{
                sort => memory,
                limit => 1,
                test_process_source => #{count_fun => fun() -> 100001 end}
            },
            #{timeout_ms => 5000, identifier_policy => include}
        ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(processes, include, node(), Refused)
    ),
    ?assertMatch({ok, Refused, 3}, observer_cli_escriptize:dispatch_response(Refused)).

controller_validates_required_unavailable_responses_test() ->
    lists:foreach(
        fun({Command, Probe}) ->
            #{<<"status">> := <<"ok">>, <<"result">> := Response} =
                observer_cli_snapshot:dispatch(
                    self(),
                    Command,
                    #{test_probe_outcomes => #{Probe => {unavailable, capability_unavailable}}},
                    #{timeout_ms => 5000, identifier_policy => include}
                ),
            ?assertMatch(#{<<"node">> := _}, response_target(Response)),
            ?assert(is_map(maps:get(<<"data">>, Response))),
            ?assertEqual(<<"partial">>, maps:get(<<"outcome">>, Response)),
            ?assertMatch(
                [
                    #{
                        <<"status">> := <<"unavailable">>,
                        <<"reason_code">> := <<"capability_unavailable">>
                    }
                    | _
                ],
                maps:get(<<"probes">>, response_capture(Response))
            ),
            ?assertEqual([], maps:get(<<"issues">>, Response)),
            ?assertEqual(
                ok,
                observer_cli_escriptize:validate_response(
                    Command, include, node(), Response
                )
            )
        end,
        [{snapshot, runtime}, {memory, memory}]
    ).

oversized_structured_output_keeps_error_envelope_test() ->
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_oversized_output_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts/ebin~n"
        "main([Format]) ->~n"
        "  Response = observer_cli_cli:response(memory, complete, null, null, "
        "#{<<\"huge\">> => binary:copy(<<\"x\">>, 1048576)}, []),~n"
        "  observer_cli_escriptize:command_output(#{format => Format}, Response, 0).~n",
        [AppDir]
    ),
    ok = file:write_file(Script, Contents),
    try
        {4, Term} = run_escript(Escript, [Script, "term"]),
        ?assertNotEqual(nomatch, binary:match(Term, <<"observer_cli.cli/v1">>)),
        ?assertNotEqual(nomatch, binary:match(Term, <<"response_too_large">>)),
        case code:ensure_loaded(json) of
            {module, json} ->
                {4, Json} = run_escript(Escript, [Script, "json"]),
                ?assertNotEqual(
                    nomatch, binary:match(Json, <<"\"schema\":\"observer_cli.cli/v1\"">>)
                ),
                ?assertNotEqual(nomatch, binary:match(Json, <<"response_too_large">>));
            {error, _Reason} ->
                {2, JsonError} = run_escript(Escript, [Script, "json"]),
                ?assertNotEqual(nomatch, binary:match(JsonError, <<"JSON output requires OTP 27">>))
        end
    after
        file:delete(Script)
    end.

cleanup_and_error_priority_test() ->
    Cleanup = (valid_controller_response(trace_call, <<"node@host">>))#{
        <<"outcome">> := <<"error">>,
        <<"issues">> := [
            observer_cli_cli:error(cleanup, cleanup_unconfirmed)
        ]
    },
    ?assertEqual(
        {ok, Cleanup, 4}, observer_cli_escriptize:dispatch_response(Cleanup)
    ),
    Capability = Cleanup#{
        <<"issues">> := [
            observer_cli_cli:error(capability, capability_unavailable)
        ]
    },
    ?assertEqual(
        {ok, Capability, 2},
        observer_cli_escriptize:dispatch_response(Capability)
    ),
    ?assertEqual(
        {error, cleanup, cleanup_unconfirmed},
        observer_cli_escriptize:cleanup_outcome({ok, Cleanup, 0}, {
            error, cleanup_unconfirmed
        })
    ).

exit_reason_matrix_test() ->
    lists:foreach(
        fun({Class, Expected}) ->
            Response = #{
                <<"outcome">> => <<"error">>,
                <<"issues">> => [#{<<"severity">> => <<"error">>, <<"class">> => Class}]
            },
            ?assertEqual(Expected, observer_cli_escriptize:response_exit_code(Response))
        end,
        [
            {<<"format">>, 2},
            {<<"controller">>, 3},
            {<<"distribution">>, 3},
            {<<"required_probe">>, 3},
            {<<"partial">>, 3}
        ]
    ),
    ?assertEqual(
        4,
        observer_cli_escriptize:response_exit_code(#{
            <<"outcome">> => <<"error">>, <<"issues">> => [#{<<"severity">> => <<"error">>}]
        })
    ),
    lists:foreach(
        fun({Reason, Expected}) ->
            Response = #{
                <<"outcome">> => <<"error">>,
                <<"issues">> => [],
                <<"meta">> => #{
                    <<"capture">> => #{
                        <<"probes">> => [
                            #{
                                <<"status">> => <<"error">>, <<"reason_code">> => Reason
                            }
                        ]
                    }
                }
            },
            ?assertEqual(Expected, observer_cli_escriptize:response_exit_code(Response))
        end,
        [
            {<<"mfa_unavailable">>, 2},
            {<<"mfa_not_traceable">>, 2},
            {<<"log_source_unavailable">>, 2},
            {<<"log_handler_required">>, 2},
            {<<"log_handler_not_found">>, 2},
            {<<"unsupported_file_modes">>, 2},
            {<<"unsupported_log_file_type">>, 2},
            {<<"log_path_unrepresentable">>, 2},
            {<<"invalid_log_handler_config">>, 2},
            {<<"unsupported_target_platform">>, 2},
            {<<"invalid_schema">>, 4},
            {<<"internal_error">>, 4},
            {<<"dispatcher_disconnected">>, 4},
            {<<"helper_setup_failed">>, 4}
        ]
    ),
    ?assertEqual(
        4,
        observer_cli_escriptize:response_exit_code(#{
            <<"outcome">> => <<"error">>, <<"issues">> => []
        })
    ).

trace_contract_boundary_matrix_test() ->
    Target = 'node@host',
    TargetText = <<"node@host">>,
    Call = valid_controller_response(trace_call, TargetText),
    Stop = valid_controller_response(trace_stop_all, TargetText),
    [CallProbe] = maps:get(<<"probes">>, response_capture(Call)),
    GlobalWarning = #{
        <<"severity">> => <<"warning">>,
        <<"class">> => <<"safety_refusal">>,
        <<"reason_code">> => <<"global_trace_replacement">>,
        <<"message">> => <<"node-global trace state is replaced">>
    },
    NullStop = observer_cli_cli:response(
        trace_stop_all,
        error,
        null,
        null,
        null,
        [observer_cli_cli:error(capability, capability_unavailable), GlobalWarning]
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(trace_stop_all, include, Target, NullStop)
    ),
    ?assertEqual(
        {error, invalid_command_response},
        observer_cli_escriptize:validate_response(
            trace_stop_all,
            include,
            Target,
            NullStop#{
                <<"issues">> := maps:get(<<"issues">>, NullStop) ++
                    [observer_cli_cli:error(cleanup, cleanup_unconfirmed)]
            }
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            distribution, #{<<"data">> => invalid}, [fixture_probe(<<"distribution">>)]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            supervision_tree,
            #{<<"data">> => invalid},
            [fixture_probe(<<"supervision_tree">>)]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call, maps:remove(<<"outcome">>, Call), [CallProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call, Call#{<<"data">> := invalid}, [CallProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call, Call#{<<"outcome">> := <<"partial">>}, [CallProbe]
        )
    ),
    CallData = maps:get(<<"data">>, Call),
    CallTrace = maps:get(<<"trace">>, CallData),
    NoReload = maps:remove(<<"module_reloaded">>, CallTrace),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call,
            Call#{<<"data">> := CallData#{<<"trace">> := NoReload}},
            [CallProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call,
            Call#{<<"data">> := CallData#{<<"trace">> := #{}}},
            [CallProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call,
            Call#{
                <<"data">> := CallData#{
                    <<"trace">> := CallTrace#{<<"events">> := [invalid]}
                }
            },
            [CallProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            trace_call,
            Call#{
                <<"data">> := CallData#{
                    <<"trace">> := CallTrace#{<<"mfa">> := invalid}
                }
            },
            [CallProbe]
        )
    ),
    [StopProbe] = maps:get(<<"probes">>, response_capture(Stop)),
    ?assert(observer_cli_escriptize:valid_command_payload(trace_stop_all, Stop, [StopProbe])).

otp_state_schema_boundary_matrix_test() ->
    Target = 'node@host',
    Base = valid_controller_response(otp_state, <<"node@host">>),
    [Probe] = maps:get(<<"probes">>, response_capture(Base)),
    Success = fun(Data) -> Base#{<<"data">> := Data} end,
    Error = fun(Data, Reason) ->
        replace_capture(
            (Success(Data))#{<<"outcome">> := <<"partial">>},
            (response_capture(Base))#{
                <<"probes">> := [
                    Probe#{<<"status">> := <<"error">>, <<"reason_code">> := Reason}
                ]
            }
        )
    end,
    Validate = fun(Response) ->
        observer_cli_escriptize:validate_response(otp_state, include, Target, Response)
    end,
    Server = otp_state_data(gen_server),
    Statem = otp_state_data(gen_statem),
    Event = otp_state_data(gen_event),
    Shape = #{<<"type">> => <<"atom">>},
    ?assertEqual({error, invalid_command_response}, Validate(Success(#{}))),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(Server#{<<"behavior">> := <<"unknown">>}))
    ),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(Server#{<<"status">> := <<"unknown">>}))
    ),
    ServerTimeout = Server#{
        <<"status">> := <<"error">>,
        <<"reason_code">> => <<"state_timeout">>,
        <<"state_shape">> := null,
        <<"visited_node_count">> := 0
    },
    ?assertEqual(ok, Validate(Error(ServerTimeout, <<"state_timeout">>))),
    StatemNotFound = Statem#{
        <<"status">> := <<"not_found">>,
        <<"structural_validation">> := <<"not_performed">>,
        <<"current_state_shape">> := null,
        <<"data_shape">> := null,
        <<"visited_node_count">> := 0
    },
    ?assertEqual(ok, Validate(Success(StatemNotFound))),
    lists:foreach(
        fun({Reason, Validation}) ->
            Data = StatemNotFound#{
                <<"status">> := <<"error">>,
                <<"reason_code">> => Reason,
                <<"structural_validation">> := Validation
            },
            ?assertEqual(ok, Validate(Error(Data, Reason)))
        end,
        [
            {<<"state_timeout">>, <<"not_performed">>},
            {<<"state_shape_failed">>, <<"passed">>}
        ]
    ),
    InvalidReason = StatemNotFound#{
        <<"status">> := <<"error">>, <<"reason_code">> => <<"unknown_state_failure">>
    },
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Error(InvalidReason, <<"unknown_state_failure">>))
    ),
    BaseLimits = maps:get(<<"limits">>, Server),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(Event#{<<"limits">> := BaseLimits}))
    ),
    MissingShape = (maps:remove(<<"state_shape">>, Server))#{<<"unused">> => null},
    ?assertEqual({error, invalid_command_response}, Validate(Success(MissingShape))),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(StatemNotFound#{<<"current_state">> := <<"idle">>}))
    ),
    lists:foreach(
        fun(Data) -> ?assertEqual(ok, Validate(Success(Data))) end,
        [
            Statem#{
                <<"truncated">> := true, <<"truncation_reason">> := <<"depth_cap">>
            },
            Statem#{
                <<"current_state_shape">> := null,
                <<"data_shape">> := null,
                <<"truncated">> := true,
                <<"truncation_reason">> := <<"node_cap">>
            },
            Statem#{
                <<"data_shape">> := null,
                <<"truncated">> := true,
                <<"truncation_reason">> := <<"node_cap">>
            }
        ]
    ),
    EventNotFound = Event#{
        <<"status">> := <<"not_found">>,
        <<"structural_validation">> := <<"not_performed">>,
        <<"observed_handler_count">> := 0,
        <<"returned_count">> := 0,
        <<"dropped_count">> := 0,
        <<"shape_budget_exhausted_count">> := 0,
        <<"handlers">> := [],
        <<"visited_node_count">> := 0
    },
    ?assertEqual(ok, Validate(Success(EventNotFound))),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(EventNotFound#{<<"observed_handler_count">> := 1}))
    ),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(Success(Statem#{<<"current_state">> := <<"idle">>}))
    ),
    lists:foreach(
        fun(StateShape) ->
            ?assertEqual(ok, Validate(Success(Server#{<<"state_shape">> := StateShape})))
        end,
        [
            #{<<"type">> => <<"binary">>, <<"size_bytes">> => 3},
            #{<<"type">> => <<"bitstring">>, <<"size_bits">> => 3},
            #{<<"type">> => <<"truncated">>, <<"truncation_reason">> => <<"node_cap">>}
        ]
    ),
    IntegerNodeCap = #{
        <<"type">> => <<"list">>,
        <<"size">> => 3,
        <<"children">> => [Shape, Shape],
        <<"returned_count">> => 2,
        <<"truncated">> => true,
        <<"truncation_reason">> => <<"node_cap">>
    },
    ?assertEqual(ok, Validate(Success(Server#{<<"state_shape">> := IntegerNodeCap}))),
    ?assertEqual(
        {error, invalid_command_response},
        Validate(
            Success(Server#{
                <<"state_shape">> := IntegerNodeCap#{
                    <<"truncation_reason">> := <<"invalid_cap">>
                }
            })
        )
    ),
    ?assertEqual(
        ok,
        Validate(
            Success(Server#{
                <<"state_shape">> := IntegerNodeCap#{<<"size">> := null}
            })
        )
    ),
    Redacted = valid_controller_response(otp_state, <<"node-1">>),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(
            otp_state, redact, Target, Redacted#{<<"data">> := Server}
        )
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:validate_response(otp_state, redact, Target, Redacted)
    ),
    lists:foreach(
        fun(Data) ->
            ?assertEqual(
                {error, invalid_command_response},
                observer_cli_escriptize:validate_response(
                    otp_state, redact, Target, Redacted#{<<"data">> := Data}
                )
            )
        end,
        [
            #{<<"behavior">> => <<"unknown">>},
            #{<<"behavior">> => <<"gen_event">>, <<"handlers">> => [#{}]},
            #{<<"behavior">> => <<"gen_statem">>, <<"current_state">> => invalid}
        ]
    ).

capability_error_classification_test() ->
    ?assertEqual(
        {error, capability, {diagnostics_incompatible, null}},
        observer_cli_escriptize:capability_error(error, undef)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:capability_error(error, {erpc, timeout})
    ),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:capability_error(error, {erpc, noconnection})
    ),
    ?assertEqual(
        {error, required_probe, capability_probe_failed},
        observer_cli_escriptize:capability_error(error, remote_crash)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:probe_response(
            status,
            #{node => "node@host"},
            node(),
            {error, required_probe, target_timeout},
            0
        )
    ),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:probe_response(
            status,
            #{node => "node@host"},
            node(),
            {error, connection, connection_failed},
            0
        )
    ).

valid_controller_response(Command, Node) ->
    #{
        <<"schema">> => <<"observer_cli.cli/v1">>,
        <<"command">> => atom_to_binary(Command),
        <<"outcome">> => <<"complete">>,
        <<"data">> => fixture_data(Command),
        <<"meta">> => #{
            <<"target">> => #{<<"node">> => Node, <<"otp_release">> => <<"29">>},
            <<"capture">> => #{
                <<"started_at">> => <<"2026-07-11T00:00:00Z">>,
                <<"finished_at">> => <<"2026-07-11T00:00:01Z">>,
                <<"duration_ms">> => 1000,
                <<"probes">> => fixture_probes(Command),
                <<"observer_effects">> => fixture_effects(Command)
            }
        },
        <<"issues">> => fixture_issues(Command)
    }.

response_capture(Response) ->
    maps:get(<<"capture">>, maps:get(<<"meta">>, Response)).

response_target(Response) ->
    maps:get(<<"target">>, maps:get(<<"meta">>, Response)).

replace_capture(Response, Capture) ->
    Meta = maps:get(<<"meta">>, Response),
    Response#{<<"meta">> := Meta#{<<"capture">> := Capture}}.

replace_target(Response, Target) ->
    Meta = maps:get(<<"meta">>, Response),
    Response#{<<"meta">> := Meta#{<<"target">> := Target}}.

old_seven_field_response(Command, Node) ->
    #{
        <<"schema">> => <<"observer_cli.cli/v1">>,
        <<"command">> => atom_to_binary(Command),
        <<"target">> => #{<<"node">> => Node, <<"otp_release">> => <<"29">>},
        <<"capture">> => #{<<"status">> => <<"complete">>},
        <<"data">> => fixture_data(Command),
        <<"warnings">> => [],
        <<"errors">> => []
    }.

fixture_probes(snapshot) ->
    [fixture_probe(Id) || Id <- [<<"runtime">>, <<"resources">>, <<"memory">>]];
fixture_probes(diagnose) ->
    [fixture_probe(<<"core_limits">>)];
fixture_probes(memory) ->
    [fixture_probe(Id) || Id <- [<<"memory">>, <<"allocator">>]];
fixture_probes(Command) when Command =:= trace_call; Command =:= trace_stop_all ->
    [
        (fixture_probe(<<"trace">>))#{
            <<"duration_ms">> := 1000,
            <<"coverage">> := [<<"recon_2_5_6">>, <<"external_global_calls_only">>]
        }
    ];
fixture_probes(Command) ->
    [fixture_probe(atom_to_binary(Command))].

fixture_effects(Command) when Command =:= trace_call; Command =:= trace_stop_all ->
    [
        #{
            <<"id">> => <<"global_trace_replacement">>,
            <<"controller">> => <<"<0.1.0>">>
        }
    ];
fixture_effects(_Command) ->
    [].

fixture_issues(Command) when Command =:= trace_call; Command =:= trace_stop_all ->
    [
        #{
            <<"severity">> => <<"warning">>,
            <<"class">> => <<"safety_refusal">>,
            <<"reason_code">> => <<"global_trace_replacement">>,
            <<"message">> => <<"fixture warning">>
        }
    ];
fixture_issues(_Command) ->
    [].

fixture_probe(Id) ->
    #{
        <<"id">> => Id,
        <<"required">> => true,
        <<"status">> => <<"ok">>,
        <<"reason_code">> => null,
        <<"duration_ms">> => 1,
        <<"samples">> => 1,
        <<"coverage">> => []
    }.

fixture_data(memory) ->
    #{<<"runtime">> => #{}, <<"memory">> => #{<<"allocator">> => #{}}};
fixture_data(otp_state) ->
    otp_state_data(gen_statem);
fixture_data(trace_call) ->
    #{<<"reason">> => <<"limit_reached">>, <<"trace">> => fixture_trace_capture()};
fixture_data(trace_stop_all) ->
    StopTrace = maps:remove(
        <<"module_reloaded">>,
        (fixture_trace_capture())#{
            <<"status">> := <<"partial">>,
            <<"reason">> := <<"stopped">>,
            <<"trace_complete">> := false,
            <<"truncated">> := true,
            <<"dropped_count">> := null
        }
    ),
    #{<<"reason">> => <<"stopped">>, <<"trace">> => StopTrace};
fixture_data(_Command) ->
    #{}.

fixture_trace_capture() ->
    #{
        <<"status">> => <<"complete">>,
        <<"reason">> => <<"limit_reached">>,
        <<"trace_complete">> => true,
        <<"truncated">> => false,
        <<"dropped_count">> => 0,
        <<"events">> => [],
        <<"module_reloaded">> => false,
        <<"interference_detected">> => false,
        <<"coverage">> => <<"external_global_calls_only">>,
        <<"cleanup_confirmed">> => true,
        <<"tracee">> => <<"<0.1.0>">>,
        <<"mfa">> => #{
            <<"module">> => <<"erlang">>,
            <<"function">> => <<"node">>,
            <<"arity">> => 0
        }
    }.

run_escript(Escript, Args) ->
    Port = open_port(
        {spawn_executable, Escript},
        [binary, exit_status, stderr_to_stdout, {args, Args}]
    ),
    collect_escript(Port, <<>>).

collect_escript(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_escript(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 10000 ->
        erlang:error(escript_timeout)
    end.

run_unreachable_node_test() ->
    Cookie = "observer_cli_test_cookie",
    CookieAtom = list_to_atom(Cookie),
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true -> ok;
        false -> {ok, _} = net_kernel:start([observer_cli_test, shortnames])
    end,
    erlang:set_cookie(node(), CookieAtom),
    try
        try
            observer_cli_escriptize:run(
                "missing@invalid-host",
                CookieAtom,
                1000,
                fun(_Node) -> ok end
            )
        catch
            _:_ -> ok
        end,
        ok
    after
        erlang:set_cookie(node(), PrevCookie),
        case WasAlive of
            true -> ok;
            false -> _ = net_kernel:stop()
        end
    end.

run_name_mode_mismatch_test() ->
    Cookie = "observer_cli_test_cookie",
    CookieAtom = list_to_atom(Cookie),
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true -> ok;
        false -> {ok, _} = net_kernel:start([observer_cli_test, shortnames])
    end,
    ActualMode =
        case net_kernel:longnames() of
            true -> longnames;
            false -> shortnames
        end,
    {TargetNode, ExpectedMode} =
        case ActualMode of
            longnames -> {"target@host", shortnames};
            shortnames -> {"target@host.example", longnames}
        end,
    erlang:set_cookie(node(), CookieAtom),
    try
        ?assertError(
            {net_kernel_start_failed, {name_mode_mismatch, ExpectedMode, ActualMode, _}},
            observer_cli_escriptize:run(
                TargetNode,
                CookieAtom,
                1000
            )
        )
    after
        erlang:set_cookie(node(), PrevCookie),
        case WasAlive of
            true -> ok;
            false -> _ = net_kernel:stop()
        end
    end.

remote_load_local_test() ->
    ?assertEqual(ok, observer_cli_escriptize:remote_load(node())).

remote_load_peer_node_test() ->
    with_distribution(fun(_Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_remote")}),
        Key = test_remote_load_env,
        PrevEnv = application:get_env(observer_cli, Key),
        ok = application:set_env(observer_cli, Key, copied_to_peer),
        try
            Before = system_module_md5s(Node),
            ?assertEqual(false, erpc:call(Node, code, is_loaded, [observer_cli])),
            erpc:call(Node, application, unset_env, [observer_cli, Key]),
            ?assertEqual(
                ok,
                observer_cli_escriptize:run_remote(
                    Node,
                    fun observer_cli_escriptize:remote_module_available/1,
                    fun observer_cli_escriptize:remote_load/1,
                    fun() -> ok end
                )
            ),
            ?assertEqual(
                {ok, copied_to_peer},
                erpc:call(Node, application, get_env, [observer_cli, Key])
            ),
            ?assertNotEqual(false, erpc:call(Node, code, is_loaded, [observer_cli])),
            ?assertNotEqual(false, erpc:call(Node, code, is_loaded, [recon])),
            ?assertEqual(Before, system_module_md5s(Node))
        after
            restore_env(observer_cli, Key, PrevEnv),
            peer:stop(Peer)
        end
    end).

cross_otp_remote_load_test_() ->
    case os:getenv("OBSERVER_CLI_CROSS_OTP_ERL") of
        false ->
            [];
        Erl ->
            {timeout, 30, fun() -> cross_otp_remote_load(Erl) end}
    end.

cross_otp_remote_load(Erl) ->
    with_distribution(fun(_Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{
            exec => Erl, name => peer:random_name("observer_cli_cross_otp")
        }),
        try
            ControllerOtp = integer_to_list(?OTP_RELEASE),
            TargetOtp = erpc:call(Node, erlang, system_info, [otp_release]),
            ?assertNotEqual(ControllerOtp, TargetOtp),
            Before = system_module_md5s(Node),
            ?assertEqual(false, erpc:call(Node, code, is_loaded, [observer_cli])),
            ?assertError(
                {remote_otp_mismatch, ControllerOtp, TargetOtp},
                observer_cli_escriptize:remote_load(Node)
            ),
            Output = assert_halt(3, fun() ->
                observer_cli_escriptize:main(["tui", atom_to_list(Node)])
            end),
            OutputBinary = iolist_to_binary(Output),
            ?assertNotEqual(nomatch, binary:match(OutputBinary, list_to_binary(ControllerOtp))),
            ?assertNotEqual(nomatch, binary:match(OutputBinary, list_to_binary(TargetOtp))),
            ?assertNotEqual(nomatch, binary:match(OutputBinary, <<"same OTP major release">>)),
            ?assertEqual(false, erpc:call(Node, code, is_loaded, [observer_cli])),
            ?assertEqual(Before, system_module_md5s(Node))
        after
            peer:stop(Peer)
        end
    end).

remote_load_replaces_incompatible_bundle() ->
    with_distribution(fun(_Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_version")}),
        Dir = temporary_directory("observer_cli_old_bundle"),
        Source = filename:join(Dir, "observer_cli_snapshot.erl"),
        try
            ok = observer_cli_escriptize:remote_load(Node),
            ?assert(observer_cli_escriptize:remote_module_available(Node)),
            ok = file:write_file(
                Source,
                <<"-module(observer_cli_snapshot).\n-export([capabilities/0]).\ncapabilities() -> #{bundle_version => <<\"1.8.8\">>, protocol_version => 1}.\n">>
            ),
            {ok, observer_cli_snapshot, OldBeam} = compile:file(Source, [binary]),
            ?assertEqual(
                {module, observer_cli_snapshot},
                erpc:call(Node, code, load_binary, [observer_cli_snapshot, Source, OldBeam])
            ),
            ?assertNot(observer_cli_escriptize:remote_module_available(Node)),
            ok = observer_cli_escriptize:remote_load(Node),
            ?assert(observer_cli_escriptize:remote_module_available(Node))
        after
            peer:stop(Peer),
            file:del_dir_r(Dir)
        end
    end).

run_starts_distribution_test() ->
    WasAlive = erlang:is_alive(),
    case WasAlive of
        true ->
            ok;
        false ->
            Cookie = observer_cli_run_start_cookie,
            PrevCookie = erlang:get_cookie(),
            try
                ?assertError(
                    {remote_load_failed, 'missing@invalid-host'},
                    observer_cli_test_io:with_input(
                        [],
                        fun() ->
                            observer_cli_escriptize:run(
                                "missing@invalid-host",
                                Cookie,
                                1000,
                                fun(_Node) -> ok end
                            )
                        end
                    )
                )
            after
                erlang:set_cookie(node(), PrevCookie),
                net_kernel:stop()
            end
    end.

run_waits_for_missing_node_test() ->
    with_distribution(fun(Cookie) ->
        PrevStopEnv = application:get_env(observer_cli, test_stop_remote),
        ok = application:set_env(observer_cli, test_stop_remote, true),
        try
            ?assertError(
                {remote_load_failed, 'missing@invalid-host'},
                observer_cli_test_io:with_input(
                    [],
                    fun() ->
                        observer_cli_escriptize:run(
                            "missing@invalid-host",
                            Cookie,
                            1000,
                            fun(_Node) -> ok end
                        )
                    end
                )
            )
        after
            restore_env(observer_cli, test_stop_remote, PrevStopEnv)
        end
    end).

run_waits_for_stopped_peer_test() ->
    with_distribution(fun(Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_run")}),
        ok = observer_cli_escriptize:remote_load(Node),
        PrevStopEnv = application:get_env(observer_cli, test_stop_remote),
        ok = application:set_env(observer_cli, test_stop_remote, true),
        try
            spawn(fun() ->
                timer:sleep(200),
                peer:stop(Peer)
            end),
            ?assertEqual(
                ok,
                observer_cli_test_io:with_input(
                    [],
                    fun() ->
                        observer_cli_escriptize:run(
                            atom_to_list(Node),
                            Cookie,
                            1000,
                            fun(_Node) -> ok end
                        )
                    end
                )
            )
        after
            restore_env(observer_cli, test_stop_remote, PrevStopEnv),
            try peer:stop(Peer) of
                _ -> ok
            catch
                _:_ -> ok
            end
        end
    end).

run_preinstalled_tui_once_test() ->
    Parent = self(),
    ProbeFun = fun(_Node) -> true end,
    RemoteLoadFun = fun(_Node) -> Parent ! remote_load_called end,
    StartFun = fun() ->
        Parent ! tui_started,
        quit
    end,
    ?assertEqual(
        ok,
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_remote(
                    preinstalled@target, ProbeFun, RemoteLoadFun, StartFun
                )
            end
        )
    ),
    ?assertEqual([tui_started], drain_run_messages([])).

run_automatically_loaded_tui_once_test() ->
    Parent = self(),
    ProbeKey = make_ref(),
    ProbeFun = fun(_Node) ->
        case get(ProbeKey) of
            undefined ->
                put(ProbeKey, loaded),
                false;
            loaded ->
                true
        end
    end,
    RemoteLoadFun = fun(_Node) -> Parent ! remote_load_called end,
    StartFun = fun() ->
        Parent ! tui_started,
        quit
    end,
    try
        ?assertEqual(
            ok,
            observer_cli_test_io:with_input(
                [],
                fun() ->
                    observer_cli_escriptize:run_remote(
                        missing_module@target, ProbeFun, RemoteLoadFun, StartFun
                    )
                end
            )
        ),
        ?assertEqual([remote_load_called, tui_started], drain_run_messages([]))
    after
        erase(ProbeKey)
    end.

run_rejects_failed_remote_load_test() ->
    Parent = self(),
    ?assertError(
        {remote_load_failed, incompatible@target},
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_remote(
                    incompatible@target,
                    fun(_Node) -> false end,
                    fun(_Node) -> Parent ! remote_load_called end,
                    fun() -> Parent ! tui_started end
                )
            end
        )
    ),
    ?assertEqual([remote_load_called], drain_run_messages([])).

refuse_pre_distributed_controller() ->
    Root = temporary_directory("observer_cli_distributed_status_home"),
    CookieEnv = "OBSERVER_CLI_DISTRIBUTED_STATUS_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, "cookie"),
    try
        ok = observer_cli_cli:save_context(#{
            node => "target@host", cookie_env => CookieEnv
        }),
        with_distribution(fun(_Cookie) ->
            ?assertEqual(
                {error, controller, controller_already_distributed},
                observer_cli_escriptize:with_target(#{}, fun(_Target, _Capabilities) -> ok end)
            ),
            ?assertEqual(
                {error, controller, controller_already_distributed},
                observer_cli_escriptize:run_status(#{timeout => "10s"})
            )
        end)
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:del_dir_r(Root)
    end.

random_failure_stops_before_connect() ->
    ?assertEqual(nonode@nohost, node()),
    Parent = self(),
    ?assertEqual(
        {error, controller, random_cookie_unavailable},
        observer_cli_escriptize:connect_target(
            missing@host,
            shortnames,
            observer_cli_target_cookie,
            1000,
            fun() -> erlang:error(no_random_source) end,
            fun(_Target) -> Parent ! connect_called end,
            fun(_Target, _Capabilities) -> Parent ! callback_called end
        )
    ),
    receive
        connect_called -> ?assert(false);
        callback_called -> ?assert(false)
    after 0 ->
        ok
    end,
    ?assertEqual(nonode@nohost, node()).

forward_remaining_command_deadline() ->
    ?assertEqual(nonode@nohost, node()),
    Timeout = 3000,
    Remaining = observer_cli_escriptize:probe_target(
        missing@host,
        shortnames,
        observer_cli_target_cookie,
        Timeout,
        fun() -> binary:copy(<<16#aa>>, 24) end,
        fun(_Target) ->
            timer:sleep(100),
            true
        end,
        fun(_Target, _CapabilityResult, Budget) -> Budget end
    ),
    ?assert(Remaining > 0),
    ?assert(Remaining =< Timeout - 1100),
    ?assertEqual(nonode@nohost, node()).

dynamic_controller_handshake() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_dynamic_target_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [snapshot_beam_dir()]),
    RandomBytes = binary:copy(<<16#aa>>, 24),
    RandomCookie = binary_to_atom(binary:encode_hex(RandomBytes)),
    try
        ?assertEqual(
            {ok, #{bundle_version => <<"2.0.0">>, protocol_version => 1}},
            observer_cli_escriptize:connect_target(
                Target,
                shortnames,
                Cookie,
                10000,
                fun() -> RandomBytes end,
                fun net_kernel:connect_node/1,
                fun(ConnectedTarget, Capabilities) ->
                    Controller = node(self()),
                    assert_not_equal(nonode@nohost, Controller),
                    assert_equal(RandomCookie, erlang:get_cookie()),
                    assert_equal(Cookie, erlang:get_cookie(ConnectedTarget)),
                    ?assert(
                        lists:member(
                            Controller, erpc:call(ConnectedTarget, erlang, nodes, [hidden])
                        )
                    ),
                    ?assertNot(
                        lists:member(
                            Controller, erpc:call(ConnectedTarget, erlang, nodes, [visible])
                        )
                    ),
                    {ok, EpmdNames} = net_adm:names("localhost"),
                    ControllerName = hd(string:split(atom_to_list(Controller), "@")),
                    ?assertNot(lists:keymember(ControllerName, 1, EpmdNames)),
                    {ok, Capabilities}
                end
            )
        ),
        ?assertEqual(
            {error, internal, controller_failed},
            observer_cli_escriptize:connect_target(
                Target,
                shortnames,
                Cookie,
                10000,
                fun() -> RandomBytes end,
                fun net_kernel:connect_node/1,
                fun(_ConnectedTarget, _Capabilities) -> erlang:error(callback_failed) end
            )
        ),
        ?assertEqual(ok, observer_cli_escriptize:stop_controller(100))
    after
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

invalid_remote_dispatch_contract() ->
    ?assertEqual(nonode@nohost, node()),
    Dir = temporary_directory("observer_cli_invalid_remote_dispatch"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        <<
            "-module(observer_cli_snapshot).\n"
            "-export([capabilities/0, dispatch/4]).\n"
            "capabilities() -> #{bundle_version => <<\"2.0.0\">>, protocol_version => 1}.\n"
            "dispatch(_, _, _, _) -> #{invalid => true}.\n"
        >>
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = observer_cli_invalid_dispatch_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    try
        Results = observer_cli_escriptize:connect_target(
            Target,
            shortnames,
            Cookie,
            10000,
            fun() -> binary:copy(<<16#66>>, 24) end,
            fun net_kernel:connect_node/1,
            fun(ConnectedTarget, _Capabilities) ->
                [
                    observer_cli_escriptize:run_snapshot(
                        ConnectedTarget, #{}, #{}, 5000
                    ),
                    observer_cli_escriptize:run_dispatch(
                        ConnectedTarget, memory, #{}, #{}, 5000
                    ),
                    observer_cli_escriptize:run_diagnose(ConnectedTarget, #{}, 5000)
                ]
            end
        ),
        ?assertEqual(
            [
                {error, schema, invalid_snapshot_response},
                {error, schema, invalid_command_response},
                {error, schema, invalid_diagnose_response}
            ],
            Results
        )
    after
        stop_target(Port),
        file:del_dir_r(Dir)
    end,
    ?assertEqual(nonode@nohost, node()).

active_context_lifecycle() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_context_target_cookie,
    CookieText = atom_to_list(Cookie),
    {Port, Target} = start_target(shortnames, Cookie, [snapshot_beam_dir()]),
    {Escript, Script} = context_escript(),
    Root = temporary_directory("observer_cli_context_home"),
    CookieEnv = "OBSERVER_CLI_CONTEXT_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, CookieText),
    TargetText = atom_to_list(Target),
    try
        {0, Connect} = run_escript(Escript, [
            Script, "connect", "--node", TargetText, "--cookie-env", CookieEnv
        ]),
        ?assertNotEqual(
            nomatch,
            binary:match(Connect, iolist_to_binary(["Selected ", TargetText, "; probe succeeded."]))
        ),
        ?assertNotEqual(
            nomatch, binary:match(Connect, <<"No persistent connection is kept.">>)
        ),
        ?assertEqual(nomatch, binary:match(Connect, list_to_binary(CookieText))),
        ContextPath = observer_cli_cli:context_path(),
        {ok, #file_info{mode = ContextMode}} = file:read_file_info(ContextPath),
        ?assertEqual(8#600, ContextMode band 8#777),
        {ok, ContextBytes} = file:read_file(ContextPath),
        ?assertEqual(nomatch, binary:match(ContextBytes, list_to_binary(CookieText))),
        {0, Status} = run_escript(Escript, [Script, "status"]),
        ?assertNotEqual(nomatch, binary:match(Status, <<"probe succeeded">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"diagnostics_module=compatible">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"target_otp_release=">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"name_mode=short">>)),
        ?assertNotEqual(
            nomatch, binary:match(Status, iolist_to_binary(["cookie_source=env:", CookieEnv]))
        ),
        ?assertEqual(nomatch, binary:match(Status, list_to_binary(CookieText))),
        {2, StatelessError} = run_escript(Escript, [
            Script, "snapshot", "--node", TargetText
        ]),
        ?assertNotEqual(nomatch, binary:match(StatelessError, <<"--cookie-env">>)),
        {0, Disconnect} = run_escript(Escript, [Script, "disconnect"]),
        ?assertNotEqual(
            nomatch, binary:match(Disconnect, <<"Removed saved target context for ">>)
        ),
        ?assertEqual({error, enoent}, file:read_file_info(ContextPath)),
        {0, Again} = run_escript(Escript, [Script, "disconnect"]),
        ?assertEqual(<<"No active context.\n">>, Again)
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:delete(Script),
        file:del_dir_r(Root),
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

connect_cleanup_preserves_context() ->
    Root = temporary_directory("observer_cli_cleanup_context_home"),
    PreviousConfigHome = set_config_home(Root),
    Old = #{node => "old@host", name_mode => "short", cookie_env => "OLD_COOKIE"},
    New = #{node => "new@host", name_mode => "short", cookie_env => "NEW_COOKIE"},
    try
        ok = observer_cli_cli:save_context(Old),
        Path = observer_cli_cli:context_path(),
        {ok, Before} = file:read_file(Path),
        Failure = {error, cleanup, cleanup_unconfirmed},
        ?assertEqual(
            Failure, observer_cli_escriptize:save_connected_context(New, Failure)
        ),
        ?assertEqual({ok, Before}, file:read_file(Path)),
        Outcome = {ok, #{}, 0},
        ?assertEqual(
            Outcome, observer_cli_escriptize:save_connected_context(New, Outcome)
        ),
        ?assertEqual({ok, New}, observer_cli_cli:load_context())
    after
        restore_config_home(PreviousConfigHome),
        file:del_dir_r(Root)
    end.

connect_missing_diagnostics() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_missing_context_cookie,
    {Port, Target} = start_target(shortnames, Cookie, []),
    {Escript, Script} = context_escript(),
    Root = temporary_directory("observer_cli_missing_context_home"),
    CookieEnv = "OBSERVER_CLI_MISSING_CONTEXT_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        {0, Output} = run_escript(Escript, [
            Script,
            "connect",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv
        ]),
        ?assertNotEqual(nomatch, binary:match(Output, <<"diagnostics_module=missing">>)),
        ?assertEqual(nomatch, binary:match(Output, <<"--load-diagnostics">>)),
        ?assertNotEqual(nomatch, binary:match(Output, <<"Install the matching">>)),
        {0, Status} = run_escript(Escript, [Script, "status"]),
        ?assertNotEqual(nomatch, binary:match(Status, <<"diagnostics_module=missing">>)),
        {ok, DirectStatus, 0} = observer_cli_escriptize:run_status(#{timeout => "10s"}),
        ?assertMatch(
            #{<<"data">> := #{<<"diagnostics_module">> := <<"missing">>}}, DirectStatus
        ),
        ?assertEqual(
            {error, capability, capability_unavailable},
            observer_cli_escriptize:run_command(memory, #{
                arguments => [], timeout => "10s"
            })
        ),
        {2, Rejected} = run_escript(Escript, [
            Script,
            "connect",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv,
            "--load-diagnostics"
        ]),
        ?assertNotEqual(nomatch, binary:match(Rejected, <<"unknown option">>))
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:delete(Script),
        file:del_dir_r(Root),
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

connect_incompatible_diagnostics() ->
    ?assertEqual(nonode@nohost, node()),
    Dir = temporary_directory("observer_cli_incompatible_context"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        <<
            "-module(observer_cli_snapshot).\n"
            "-export([capabilities/0]).\n"
            "capabilities() -> #{bundle_version => <<\"1.8.8\">>, "
            "protocol_version => 1}.\n"
        >>
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = observer_cli_incompatible_context_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    {Escript, Script} = context_escript(),
    Root = temporary_directory("observer_cli_incompatible_context_home"),
    CookieEnv = "OBSERVER_CLI_INCOMPATIBLE_CONTEXT_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        {0, Connect} = run_escript(Escript, [
            Script,
            "connect",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv
        ]),
        ?assertNotEqual(nomatch, binary:match(Connect, <<"diagnostics_module=incompatible">>)),
        ?assertNotEqual(nomatch, binary:match(Connect, <<"bundle=1.8.8">>)),
        {0, Status} = run_escript(Escript, [Script, "status"]),
        ?assertNotEqual(nomatch, binary:match(Status, <<"diagnostics_module=incompatible">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"bundle=1.8.8">>)),
        {ok, DirectStatus, 0} = observer_cli_escriptize:run_status(#{timeout => "10s"}),
        ?assertMatch(
            #{<<"data">> := #{<<"diagnostics_module">> := <<"incompatible">>}},
            DirectStatus
        ),
        ?assertEqual(
            {error, capability, capability_unavailable},
            observer_cli_escriptize:run_command(memory, #{
                arguments => [], timeout => "10s"
            })
        )
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:delete(Script),
        file:del_dir_r(Root),
        stop_target(Port),
        file:del_dir_r(Dir)
    end,
    ?assertEqual(nonode@nohost, node()).

connect_sanitizes_hostile_diagnostics() ->
    ?assertEqual(nonode@nohost, node()),
    Dir = temporary_directory("observer_cli_hostile_context"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        [
            "-module(observer_cli_snapshot).\n",
            "-export([capabilities/0]).\n",
            "capabilities() -> #{bundle_version => list_to_binary([255]), ",
            "protocol_version => 2147483648, ",
            "secret => do_not_copy}.\n"
        ]
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = observer_cli_hostile_context_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    {Escript, Script} = context_escript(),
    Root = temporary_directory("observer_cli_hostile_context_home"),
    CookieEnv = "OBSERVER_CLI_HOSTILE_CONTEXT_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    ConnectArgs = [
        Script,
        "connect",
        "--node",
        atom_to_list(Target),
        "--cookie-env",
        CookieEnv
    ],
    try
        case code:ensure_loaded(json) of
            {module, json} ->
                {0, Json} = run_escript(Escript, ConnectArgs ++ ["--json"]),
                ?assertNotEqual(nomatch, binary:match(Json, <<"bundle_version">>)),
                ?assertNotEqual(nomatch, binary:match(Json, <<"protocol_version">>)),
                ?assertNotEqual(nomatch, binary:match(Json, <<"null">>)),
                ?assertEqual(nomatch, binary:match(Json, <<"do_not_copy">>));
            {error, _Reason} ->
                {2, JsonError} = run_escript(Escript, ConnectArgs ++ ["--json"]),
                ?assertNotEqual(nomatch, binary:match(JsonError, <<"JSON output requires">>)),
                ?assertEqual(
                    {error, enoent}, file:read_file_info(observer_cli_cli:context_path())
                ),
                {0, _TermConnect} = run_escript(Escript, ConnectArgs ++ ["--format", "term"])
        end,
        {0, Status} = run_escript(Escript, [Script, "status", "--format", "term"]),
        ?assertNotEqual(nomatch, binary:match(Status, <<"bundle_version">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"protocol_version">>)),
        ?assertNotEqual(nomatch, binary:match(Status, <<"null">>)),
        ?assertEqual(nomatch, binary:match(Status, <<"do_not_copy">>))
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:delete(Script),
        file:del_dir_r(Root),
        stop_target(Port),
        file:del_dir_r(Dir)
    end,
    ?assertEqual(nonode@nohost, node()).

context_escript() ->
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    ReconDir = code:lib_dir(recon),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_context_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".escript"
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts/ebin ~ts/ebin~n"
        "main(Args) -> observer_cli_escriptize:main(Args).~n",
        [AppDir, ReconDir]
    ),
    ok = file:write_file(Script, Contents),
    {Escript, Script}.

snapshot_escript_envelopes_test_() ->
    {timeout, 30, fun snapshot_escript_envelopes/0}.

diagnose_escript_exit_codes_test_() ->
    {timeout, 30, fun diagnose_escript_exit_codes/0}.

direct_remote_command_suite_test_() ->
    {timeout, 90, fun direct_remote_command_suite/0}.

direct_remote_command_suite() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_direct_command_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [
        snapshot_beam_dir(), filename:join(code:lib_dir(recon), "ebin")
    ]),
    Root = temporary_directory("observer_cli_direct_command_home"),
    CookieEnv = "OBSERVER_CLI_DIRECT_COMMAND_COOKIE",
    PreviousConfigHome = set_config_home(Root),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        assert_command_ok(
            connect,
            #{
                node => atom_to_list(Target),
                cookie_env => CookieEnv,
                arguments => [],
                timeout => "10s"
            }
        ),
        assert_command_ok(status, #{arguments => [], timeout => "10s"}),
        MainMemory = assert_halt(0, fun() ->
            observer_cli_escriptize:main(["memory", "--format", "term"])
        end),
        ?assertNotEqual(
            nomatch, binary:match(iolist_to_binary(MainMemory), <<"observer_cli.cli/v1">>)
        ),
        lists:foreach(
            fun({Command, Options}) ->
                assert_command_ok(Command, Options#{timeout => "15s"})
            end,
            [
                {snapshot, #{arguments => [], deep => true}},
                {diagnose, #{arguments => []}},
                {memory, #{arguments => []}},
                {schedulers, #{arguments => [], duration => "250ms"}},
                {distribution, #{arguments => [], limit => "20"}},
                {processes, #{
                    arguments => [], sort => "reductions", limit => "5", duration => "250ms"
                }},
                {process, #{arguments => ["init"]}},
                {applications, #{arguments => [], sort => "memory", limit => "5"}},
                {ets, #{arguments => [], sort => "size", limit => "5"}},
                {mnesia, #{arguments => [], sort => "memory", limit => "5"}},
                {network, #{arguments => [], sort => "oct", limit => "5", duration => "250ms"}},
                {ports, #{arguments => [], sort => "io", limit => "5"}},
                {port, #{arguments => ["#Port<0.0>"]}},
                {sockets, #{arguments => [], sort => "io", limit => "5", duration => "250ms"}},
                {otp_state, #{
                    arguments => ["alarm_handler"], behavior => "gen_event", limit => "5"
                }},
                {supervision_tree, #{arguments => [], app => "kernel"}},
                {trace, #{arguments => ["stop"], all => true}}
            ]
        ),
        assert_command_ok(disconnect, #{arguments => []}),
        CookieFile = filename:join(Root, "target.cookie"),
        ok = file:write_file(CookieFile, atom_to_binary(Cookie)),
        ok = file:change_mode(CookieFile, 8#600),
        assert_command_ok(
            connect,
            #{
                node => atom_to_list(Target),
                cookie_file => CookieFile,
                arguments => [],
                timeout => "10s"
            }
        ),
        {ok, FileStatus, 0} = observer_cli_escriptize:run_command(status, #{
            arguments => [], timeout => "10s"
        }),
        ?assertMatch(
            #{
                <<"data">> := #{
                    <<"cookie_source">> := #{<<"type">> := <<"file">>, <<"path">> := _}
                }
            },
            FileStatus
        ),
        assert_command_ok(disconnect, #{arguments => []})
    after
        true = os:unsetenv(CookieEnv),
        restore_config_home(PreviousConfigHome),
        file:del_dir_r(Root),
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

assert_command_ok(Command, Options) ->
    case observer_cli_escriptize:run_command(Command, Options) of
        {ok, #{<<"schema">> := <<"observer_cli.cli/v1">>}, _} -> ok;
        Result -> erlang:error({command_failed, Command, Result})
    end.

diagnose_escript_exit_codes() ->
    ?assertEqual(nonode@nohost, node()),
    {Escript, Script} = context_escript(),
    CookieEnv = "OBSERVER_CLI_DIAGNOSE_ESCRIPT_COOKIE",
    try
        lists:foreach(
            fun({Code, CaptureStatus, Findings}) ->
                diagnose_escript_exit_case(
                    Escript, Script, CookieEnv, Code, CaptureStatus, Findings
                )
            end,
            [
                {0, complete, []},
                {1, complete, [#{<<"id">> => <<"vm.process_limit_pressure">>}]},
                {3, partial, []}
            ]
        ),
        diagnose_escript_schema_exit_case(Escript, Script, CookieEnv)
    after
        true = os:unsetenv(CookieEnv),
        file:delete(Script)
    end,
    ?assertEqual(nonode@nohost, node()).

diagnose_escript_exit_case(Escript, Script, CookieEnv, ExitCode, CaptureStatus, Findings) ->
    Response = diagnostic_response(CaptureStatus, Findings),
    diagnose_escript_with_dispatch(
        Escript,
        Script,
        CookieEnv,
        ExitCode,
        io_lib:format("~tp", [
            #{
                <<"status">> => <<"ok">>,
                <<"result">> => Response,
                <<"cleanup_confirmed">> => true
            }
        ])
    ).

diagnose_escript_schema_exit_case(Escript, Script, CookieEnv) ->
    diagnose_escript_with_dispatch(
        Escript,
        Script,
        CookieEnv,
        4,
        "#{<<\"status\">> => <<\"ok\">>, <<\"result\">> => #{}}"
    ).

diagnose_escript_with_dispatch(Escript, Script, CookieEnv, ExitCode, DispatchResult) ->
    Dir = temporary_directory("observer_cli_diagnose_exit"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    Contents = io_lib:format(
        "-module(observer_cli_snapshot).~n"
        "-export([capabilities/0,dispatch/4]).~n"
        "capabilities() -> #{bundle_version => <<\"2.0.0\">>, protocol_version => 1}.~n"
        "dispatch(_,diagnose,_,_) -> ~s.~n",
        [DispatchResult]
    ),
    ok = file:write_file(Source, Contents),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = list_to_atom("observer_cli_diagnose_" ++ integer_to_list(ExitCode)),
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        {ExitCode, _Output} = run_escript(Escript, [
            Script,
            "diagnose",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv,
            "--format",
            "term"
        ])
    after
        stop_target(Port),
        file:del_dir_r(Dir)
    end.

diagnostic_response(Outcome, Findings) ->
    Response = valid_controller_response(diagnose, <<"node-1">>),
    NormalizedFindings = [
        Finding#{
            <<"severity">> => <<"warning">>,
            <<"entity">> => #{<<"type">> => <<"node">>, <<"id">> => <<"node-1">>},
            <<"summary">> => <<"fixture finding">>,
            <<"ruleset_version">> => 1,
            <<"evidence">> => [
                #{
                    <<"path">> => <<"/data/context/snapshot/fixture">>,
                    <<"sample_index">> => 0,
                    <<"monotonic_midpoint_ms">> => 0,
                    <<"observed">> => 1.0,
                    <<"operator">> => <<">">>,
                    <<"threshold">> => 0.85
                }
            ],
            <<"recommendations">> => [<<"inspect the fixture">>]
        }
     || Finding <- Findings
    ],
    replace_capture(
        Response#{
            <<"outcome">> := atom_to_binary(Outcome),
            <<"data">> := #{
                <<"ruleset">> => <<"observer_cli.quick">>,
                <<"ruleset_version">> => 1,
                <<"sampling_plan">> => #{},
                <<"findings">> => NormalizedFindings,
                <<"suspects">> => [],
                <<"context">> => #{<<"snapshot">> => #{<<"fixture">> => true}},
                <<"skipped">> => [],
                <<"summary">> => <<"fixture">>
            }
        },
        (response_capture(Response))#{
            <<"probes">> := [
                #{
                    <<"id">> => <<"core_limits">>,
                    <<"required">> => true,
                    <<"status">> =>
                        case Outcome of
                            complete -> <<"ok">>;
                            partial -> <<"error">>
                        end,
                    <<"reason_code">> =>
                        case Outcome of
                            complete -> null;
                            partial -> <<"required_coverage_incomplete">>
                        end,
                    <<"duration_ms">> => 1,
                    <<"samples">> => 1,
                    <<"coverage">> => []
                }
            ]
        }
    ).

snapshot_escript_envelopes() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_snapshot_escript_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [snapshot_beam_dir()]),
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_snapshot_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".escript"
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts/ebin~n"
        "main(Args) -> observer_cli_escriptize:main(Args).~n",
        [AppDir]
    ),
    CookieEnv = "OBSERVER_CLI_SNAPSHOT_ESCRIPT_COOKIE",
    ok = file:write_file(Script, Contents),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    Args = [
        Script,
        "snapshot",
        "--node",
        atom_to_list(Target),
        "--cookie-env",
        CookieEnv
    ],
    try
        {0, Text} = run_escript(Escript, Args),
        ?assertNotEqual(nomatch, binary:match(Text, <<"observer_cli snapshot">>)),
        ?assertEqual(nomatch, binary:match(Text, <<"observer_cli.cli/v1">>)),
        ?assertEqual(nomatch, binary:match(Text, <<"issues:">>)),
        ?assertEqual(nomatch, binary:match(Text, atom_to_binary(Target))),
        {0, Term} = run_escript(Escript, Args ++ ["--format", "term"]),
        {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
        {ok, Response} = erl_parse:parse_term(Tokens),
        ?assertMatch(
            #{
                <<"schema">> := <<"observer_cli.cli/v1">>,
                <<"command">> := <<"snapshot">>,
                <<"outcome">> := <<"complete">>,
                <<"meta">> := #{<<"capture">> := #{}}
            },
            Response
        ),
        assert_partial_snapshot_exit(Escript, Script, CookieEnv)
    after
        true = os:unsetenv(CookieEnv),
        file:delete(Script),
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

response_validation_boundaries_test() ->
    ?assert(
        observer_cli_escriptize:valid_target(
            #{<<"node">> => <<"target@host">>, <<"otp_release">> => <<"28">>},
            include,
            <<"target@host">>
        )
    ),
    ?assert(
        observer_cli_escriptize:valid_target(
            #{<<"node">> => <<"node-1">>, <<"otp_release">> => <<"28">>}, redact, ignored
        )
    ),
    ?assertNot(observer_cli_escriptize:valid_target(#{}, include, <<"target">>)),
    ?assert(observer_cli_escriptize:valid_otp_release(<<"28">>)),
    ?assertNot(observer_cli_escriptize:valid_otp_release(<<"028">>)),
    ?assertNot(observer_cli_escriptize:valid_otp_release(invalid)),
    ?assert(observer_cli_escriptize:valid_rfc3339(<<"2026-07-12T00:00:00Z">>)),
    ?assertNot(observer_cli_escriptize:valid_rfc3339(<<"not-a-timeZ">>)),
    ?assertNot(observer_cli_escriptize:valid_rfc3339(<<"2026-07-12">>)),
    ?assertNot(observer_cli_escriptize:valid_rfc3339(invalid)),
    Probe = #{
        <<"id">> => <<"runtime">>,
        <<"required">> => true,
        <<"status">> => <<"ok">>,
        <<"reason_code">> => null,
        <<"duration_ms">> => 1,
        <<"samples">> => 1,
        <<"coverage">> => []
    },
    ?assert(observer_cli_escriptize:valid_probe(Probe)),
    ?assertNot(observer_cli_escriptize:valid_probe(Probe#{<<"status">> := <<"bad">>})),
    ?assertNot(observer_cli_escriptize:valid_probe(Probe#{<<"id">> := <<>>})),
    ?assertNot(
        observer_cli_escriptize:valid_probe(
            Probe#{
                <<"status">> := <<"error">>, <<"reason_code">> := <<>>, <<"coverage">> := [atom]
            }
        )
    ),
    ?assertNot(observer_cli_escriptize:valid_probe(invalid)),
    Issues = [observer_cli_cli:error(argument, bad_value)],
    ?assert(observer_cli_escriptize:valid_issues(Issues)),
    ?assertNot(
        observer_cli_escriptize:valid_issues([
            (observer_cli_cli:error(argument, bad_value))#{<<"reason_code">> := <<>>}
        ])
    ),
    ?assertNot(observer_cli_escriptize:valid_issues([#{}])),
    ?assertNot(observer_cli_escriptize:valid_issues(invalid)),
    lists:foreach(
        fun({Class, Reason}) ->
            ?assert(observer_cli_escriptize:valid_issue_class(Class, Reason))
        end,
        [
            {<<"cleanup">>, <<"cleanup_unconfirmed">>},
            {<<"internal">>, <<"internal_error">>},
            {<<"internal">>, <<"helper_setup_failed">>},
            {<<"schema">>, <<"invalid_schema">>},
            {<<"capability">>, <<"capability_unavailable">>},
            {<<"capability">>, <<"mfa_unavailable">>},
            {<<"capability">>, <<"mfa_not_traceable">>},
            {<<"required_probe">>, <<"capability_unavailable">>},
            {<<"argument">>, <<"bad_value">>}
        ]
    ),
    ?assert(observer_cli_escriptize:valid_issue_class(<<"argument">>, <<"invalid_schema">>)),
    ?assertNot(observer_cli_escriptize:valid_issue_class(invalid, invalid)),
    validation_payload_contract(Probe),
    validation_redaction_contract(),
    ?assertEqual(
        snapshot,
        observer_cli_escriptize:command_from_arguments([
            "--json", "snapshot"
        ])
    ),
    ?assertEqual(undefined, observer_cli_escriptize:command_from_arguments(["--json"])),
    ?assertEqual(undefined, observer_cli_escriptize:cookie_atom(undefined)),
    ?assertEqual(test_cookie, observer_cli_escriptize:cookie_atom("test_cookie")),
    ?assertEqual(ok, observer_cli_escriptize:maybe_set_target_cookie(node(), undefined)),
    ?assertEqual({error, cleanup_unconfirmed}, observer_cli_escriptize:stop_controller(0)),
    ?assertEqual(
        standard_error,
        observer_cli_escriptize:command_output_device(text, #{<<"outcome">> => <<"error">>})
    ),
    ?assertEqual(
        standard_io,
        observer_cli_escriptize:command_output_device(term, #{<<"outcome">> => <<"error">>})
    ),
    ?assertEqual(
        #{sort => memory, limit => 2, duration_ms => 1000},
        observer_cli_escriptize:request_options(
            #{sort => "memory", limit => "2", duration => "1s"}
        )
    ),
    Complete = #{<<"outcome">> => <<"complete">>, <<"command">> => <<"snapshot">>},
    PartialResponse = #{<<"outcome">> => <<"partial">>, <<"command">> => <<"snapshot">>},
    ?assertEqual({ok, Complete, 0}, observer_cli_escriptize:snapshot_response(Complete)),
    ?assertEqual(
        {ok, PartialResponse, 3},
        observer_cli_escriptize:snapshot_response(PartialResponse)
    ),
    ?assertEqual(
        {error, schema, invalid_command_response},
        observer_cli_escriptize:snapshot_response(#{})
    ),
    DiagnoseNone = Complete#{
        <<"command">> := <<"diagnose">>, <<"data">> => #{<<"findings">> => []}
    },
    DiagnoseSome = DiagnoseNone#{<<"data">> := #{<<"findings">> => [finding]}},
    ?assertEqual({ok, DiagnoseNone, 0}, observer_cli_escriptize:diagnose_response(DiagnoseNone)),
    ?assertEqual({ok, DiagnoseSome, 1}, observer_cli_escriptize:diagnose_response(DiagnoseSome)),
    ?assertEqual(
        {ok, PartialResponse, 3},
        observer_cli_escriptize:diagnose_response(PartialResponse)
    ),
    ?assertEqual(
        {error, schema, invalid_command_response},
        observer_cli_escriptize:diagnose_response(#{})
    ),
    ?assertMatch(
        {ok, #{node := "target@host"}},
        observer_cli_escriptize:active_options(
            #{node => "target@host"}
        )
    ),
    ?assertMatch(
        {ok, _},
        observer_cli_escriptize:random_cookie(
            fun() -> binary:copy(<<1>>, 24) end
        )
    ),
    ?assertEqual(error, observer_cli_escriptize:random_cookie(fun() -> <<1>> end)),
    ?assertEqual(error, observer_cli_escriptize:random_cookie(fun() -> error(failed) end)),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:connect_before(target, fun(_) -> true end, 0)
    ),
    ?assertEqual(ok, observer_cli_escriptize:connect_before(target, fun(_) -> true end, 100)),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:connect_before(target, fun(_) -> false end, 100)
    ),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:connect_before(target, fun(_) -> exit(failed) end, 100)
    ),
    ?assertEqual(
        {error, connection, connection_failed},
        observer_cli_escriptize:connect_before(
            target,
            fun(_) -> receive
                after infinity -> true
                end end,
            1
        )
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:capabilities(node(), 0)
    ),
    ?assertEqual(
        {ok, #{bundle_version => <<"2.0.0">>, protocol_version => 1}},
        observer_cli_escriptize:capabilities(node(), 1000)
    ),
    ?assert(
        observer_cli_escriptize:compatible_capabilities(#{
            bundle_version => <<"2.0.0">>, protocol_version => 1, extra => supported
        })
    ),
    ?assertNot(observer_cli_escriptize:compatible_capabilities(#{protocol_version => 1})),
    ?assertNot(
        observer_cli_escriptize:compatible_capabilities(#{
            bundle_version => <<"1.8.8">>, protocol_version => 1
        })
    ),
    ?assertEqual(
        {error, schema, invalid_command_response},
        observer_cli_escriptize:validated_response(memory, include, node(), invalid, fun(X) -> X end)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:run_snapshot(node(), #{}, #{}, 0)
    ),
    ?assertEqual(
        {error, required_probe, invalid_timeout},
        observer_cli_escriptize:run_snapshot(node(), #{timeout => "bad"}, #{}, 100)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:run_dispatch(node(), memory, #{}, #{}, 0)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:run_diagnose(node(), #{}, 0)
    ),
    ?assertMatch(
        {ok, #{<<"value">> := 1}},
        observer_cli_escriptize:target_dispatch(
            node(), test_echo, #{value => 1}, #{}, include, 3000
        )
    ),
    ?assertEqual(
        {error, required_probe, <<"probe_failed">>},
        observer_cli_escriptize:target_dispatch(
            node(), test_crash, self(), #{}, include, 3000
        )
    ),
    receive
        {test_worker, CrashWorker} -> ?assertNot(is_process_alive(CrashWorker))
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual(
        {error, capability, no_active_context},
        observer_cli_escriptize:probe_options(#{}, fun(_, _, _) -> ok end)
    ),
    ?assertEqual(
        {error, argument, missing_cookie_source},
        observer_cli_escriptize:probe_options(
            #{node => "target@host"}, fun(_, _, _) -> ok end
        )
    ),
    ?assertEqual(
        {error, controller, random_cookie_unavailable},
        observer_cli_escriptize:connect_started(
            'target@host',
            cookie,
            erlang:monotonic_time(millisecond) + 1000,
            fun() -> invalid end,
            fun(_) -> true end,
            fun(_, _, _) -> ok end
        )
    ),
    ok = ignore_name_mode_result(shortnames),
    ok = ignore_name_mode_result(longnames),
    ?assertEqual({error, argument, no_active_context}, observer_cli_escriptize:run_connect(#{})),
    ?assertMatch(
        {error, internal, _},
        observer_cli_escriptize:save_connected_context(
            #{}, {ok, #{protocol_version => 1}, 0}
        )
    ),
    _ = observer_cli_escriptize:run_status(#{}),
    _ = observer_cli_escriptize:run_disconnect(),
    _ = observer_cli_escriptize:with_active_target(#{}, fun(_, _, _) -> ok end),
    _ = observer_cli_escriptize:run_snapshot(
        node(), #{include_identifiers => true}, 1, 3000
    ),
    _ = observer_cli_escriptize:run_dispatch(node(), memory, 1, #{}, 3000),
    _ = observer_cli_escriptize:run_diagnose(node(), #{test_value => 1}, 3000),
    ?assertEqual(
        {error, argument, invalid_node},
        observer_cli_escriptize:probe_options(
            #{node => "bad@", cookie_env => "MISSING"}, fun(_, _, _) -> ok end
        )
    ),
    Env = "OBSERVER_CLI_VALIDATION_COOKIE",
    true = os:putenv(Env, "cookie"),
    try
        ?assertEqual(
            {error, argument, invalid_timeout},
            observer_cli_escriptize:probe_options(
                #{node => "target@host", cookie_env => Env, timeout => "bad"},
                fun(_, _, _) -> ok end
            )
        )
    after
        true = os:unsetenv(Env)
    end,
    with_distribution(fun(_PreviousCookie) ->
        ?assertEqual(
            {error, connection, connection_failed},
            observer_cli_escriptize:connect_started(
                'target@host',
                cookie,
                erlang:monotonic_time(millisecond) + 1000,
                fun() -> binary:copy(<<2>>, 24) end,
                fun(_) -> false end,
                fun(_, _, _) -> ok end
            )
        ),
        ?assertEqual(
            ok,
            observer_cli_escriptize:connect_started(
                node(),
                erlang:get_cookie(),
                erlang:monotonic_time(millisecond) + 1000,
                fun() -> binary:copy(<<3>>, 24) end,
                fun(_) -> true end,
                fun(_Target, {ok, #{protocol_version := 1}}, _Remaining) -> ok end
            )
        ),
        ?assertEqual(
            {error, controller, controller_already_distributed},
            observer_cli_escriptize:probe_target(
                node(),
                shortnames,
                erlang:get_cookie(),
                1000,
                fun() -> binary:copy(<<4>>, 24) end,
                fun(_) -> true end,
                fun(_, _, _) -> ok end
            )
        ),
        ?assertEqual(
            {error, cleanup_unconfirmed},
            observer_cli_escriptize:controller_stopped(
                erlang:monotonic_time(millisecond)
            )
        )
    end),
    ?assertMatch(
        {ok,
            #{
                <<"outcome">> := <<"complete">>,
                <<"issues">> := [#{<<"severity">> := <<"warning">>}],
                <<"data">> := #{<<"diagnostics_module">> := <<"missing">>}
            },
            0},
        observer_cli_escriptize:probe_response(
            status,
            #{
                node => atom_to_list(node()),
                name_mode => "short",
                cookie_env => "OBSERVER_CLI_TEST_COOKIE"
            },
            node(),
            {error, capability, diagnostics_missing},
            1000
        )
    ),
    PartialDispatch = #{
        <<"outcome">> => <<"partial">>,
        <<"issues">> => []
    },
    ?assertEqual(
        {ok, PartialDispatch, 3},
        observer_cli_escriptize:dispatch_response(PartialDispatch)
    ),
    ?assertEqual(
        {error, schema, invalid_command_response},
        observer_cli_escriptize:dispatch_response(#{})
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:run_dispatch(node(), memory, #{}, #{redact => true}, 0)
    ),
    ?assertEqual(
        {error, required_probe, target_timeout},
        observer_cli_escriptize:run_diagnose(node(), #{include_identifiers => true}, 0)
    ),
    ?assertEqual(
        {error, argument, missing_cookie_source},
        observer_cli_escriptize:with_active_target(
            #{node => "target@host"}, fun(_, _, _) -> ok end
        )
    ),
    MissingEnv = "OBSERVER_CLI_MISSING_VALIDATION_COOKIE",
    true = os:unsetenv(MissingEnv),
    ?assertEqual(
        {error, connection, cookie_source_unavailable},
        observer_cli_escriptize:probe_options(
            #{node => "target@host", cookie_env => MissingEnv}, fun(_, _, _) -> ok end
        )
    ),
    ?assertEqual(
        ok,
        observer_cli_escriptize:controller_stopped(
            erlang:monotonic_time(millisecond) + 10
        )
    ),
    malformed_active_context_contract(),
    ok.

ignore_name_mode_result(Mode) ->
    try observer_cli_escriptize:ensure_net_kernel_name_mode(Mode) of
        _ -> ok
    catch
        _:_ -> ok
    end.

malformed_active_context_contract() ->
    Root = temporary_directory("observer_cli_malformed_active"),
    PreviousConfigHome = set_config_home(Root),
    Path = observer_cli_cli:context_path(),
    Dir = filename:dirname(Path),
    ok = filelib:ensure_dir(Path),
    ok = file:change_mode(Dir, 8#700),
    ok = file:write_file(Path, <<"invalid">>),
    ok = file:change_mode(Path, 8#600),
    try
        ?assertEqual(
            {error, internal, invalid_context},
            observer_cli_escriptize:run_status(#{})
        ),
        ?assertEqual(
            {error, internal, invalid_context},
            observer_cli_escriptize:with_active_target(#{}, fun(_, _, _) -> ok end)
        ),
        {ok, Disconnect, 0} = observer_cli_escriptize:run_disconnect(),
        ?assertEqual(
            true,
            maps:get(
                <<"recovered_invalid_context">>, maps:get(<<"data">>, Disconnect)
            )
        ),
        ?assertEqual({error, enoent}, file:read_file_info(Path))
    after
        restore_config_home(PreviousConfigHome),
        file:del_dir_r(Root)
    end.

validation_payload_contract(Probe) ->
    lists:foreach(
        fun({Command, Data}) ->
            ?assert(
                observer_cli_escriptize:valid_command_payload(
                    Command, #{<<"outcome">> => <<"complete">>, <<"data">> => Data}, [Probe]
                )
            )
        end,
        [
            {snapshot, #{}},
            {schedulers, #{<<"status">> => <<"ok">>}},
            {distribution, #{
                <<"connected_peers">> => [],
                <<"returned_peer_count">> => 0,
                <<"truncated">> => false
            }},
            {process, #{<<"status">> => <<"ok">>}},
            {port, #{<<"status">> => <<"ok">>}},
            {supervision_tree, #{
                <<"status">> => <<"ok">>, <<"risk_level">> => <<"low">>
            }}
        ]
    ),
    lists:foreach(
        fun({Command, Data}) ->
            ?assertNot(
                observer_cli_escriptize:valid_command_payload(
                    Command, #{<<"outcome">> => <<"complete">>, <<"data">> => Data}, [Probe]
                )
            )
        end,
        [
            {schedulers, #{<<"status">> => 42}},
            {distribution, #{
                <<"connected_peers">> => not_a_list,
                <<"returned_peer_count">> => -1,
                <<"truncated">> => no
            }},
            {process, #{<<"status">> => 42}},
            {port, #{<<"status">> => 42}},
            {supervision_tree, #{<<"status">> => <<"ok">>, <<"risk_level">> => 42}},
            {trace_call, #{<<"reason">> => null, <<"trace">> => []}}
        ]
    ),
    OtpProbe = Probe#{<<"id">> := <<"otp_state">>},
    lists:foreach(
        fun(Data) ->
            ?assert(
                observer_cli_escriptize:valid_command_payload(
                    otp_state, otp_state_success_payload(Data), [OtpProbe]
                )
            )
        end,
        [otp_state_data(gen_server), otp_state_data(gen_statem), otp_state_data(gen_event)]
    ),
    NotFound = (otp_state_data(gen_server))#{
        <<"status">> := <<"not_found">>,
        <<"state_shape">> := null,
        <<"visited_node_count">> := 0
    },
    ?assert(
        observer_cli_escriptize:valid_command_payload(
            otp_state, otp_state_success_payload(NotFound), [OtpProbe]
        )
    ),
    MismatchReason = <<"behavior_shape_mismatch">>,
    Mismatch = (otp_state_data(gen_statem))#{
        <<"status">> := <<"error">>,
        <<"reason_code">> => MismatchReason,
        <<"structural_validation">> := <<"failed">>,
        <<"current_state">> := null,
        <<"current_state_identity">> := <<"unavailable">>,
        <<"current_state_shape">> := null,
        <<"data_shape">> := null,
        <<"visited_node_count">> := 0
    },
    {MismatchPayload, ErrorProbe} = otp_state_error_payload(Mismatch, MismatchReason, OtpProbe),
    ?assert(
        observer_cli_escriptize:valid_command_payload(
            otp_state, MismatchPayload, [ErrorProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            otp_state,
            MismatchPayload#{<<"data">> := maps:remove(<<"reason_code">>, Mismatch)},
            [ErrorProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            otp_state,
            otp_state_success_payload(
                (otp_state_data(gen_statem))#{<<"structural_validation">> := <<"matched">>}
            ),
            [OtpProbe]
        )
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            otp_state,
            otp_state_success_payload(
                maps:remove(<<"state_shape">>, otp_state_data(gen_server))
            ),
            [OtpProbe]
        )
    ),
    EventData = otp_state_data(gen_event),
    ServerData = otp_state_data(gen_server),
    StatemData = otp_state_data(gen_statem),
    ServerLimits = maps:get(<<"limits">>, ServerData),
    ServerAcquisition = maps:get(<<"acquisition">>, ServerData),
    EventLimits = maps:get(<<"limits">>, EventData),
    [Handler] = maps:get(<<"handlers">>, EventData),
    Shape = maps:get(<<"state_shape">>, Handler),
    ValidOtpState = fun(Data) ->
        observer_cli_escriptize:valid_command_payload(
            otp_state, otp_state_success_payload(Data), [OtpProbe]
        )
    end,
    PartiallyReturnedEvent = EventData#{
        <<"observed_handler_count">> := 3,
        <<"dropped_count">> := 2,
        <<"shape_budget_exhausted_count">> := 3,
        <<"truncated">> := true,
        <<"truncation_reason">> := <<"node_cap">>
    },
    ?assert(ValidOtpState(PartiallyReturnedEvent)),
    ?assert(
        ValidOtpState(EventData#{
            <<"observed_handler_count">> := 3,
            <<"dropped_count">> := 2,
            <<"shape_budget_exhausted_count">> := 2,
            <<"truncated">> := true,
            <<"truncation_reason">> := <<"output_cap">>
        })
    ),
    ?assert(
        ValidOtpState(EventData#{
            <<"limits">> := EventLimits#{<<"handler_output_count">> := 1},
            <<"observed_handler_count">> := 2,
            <<"dropped_count">> := 1,
            <<"truncated">> := true,
            <<"truncation_reason">> := <<"output_cap">>
        })
    ),
    EmptyEvent = EventData#{
        <<"observed_handler_count">> := 0,
        <<"returned_count">> := 0,
        <<"handlers">> := [],
        <<"visited_node_count">> := 0
    },
    ?assert(ValidOtpState(EmptyEvent)),
    ?assert(
        ValidOtpState(ServerData#{
            <<"state_shape">> := null,
            <<"truncated">> := true,
            <<"truncation_reason">> := <<"output_cap">>
        })
    ),
    ?assert(
        ValidOtpState(StatemData#{
            <<"data_shape">> := null,
            <<"truncated">> := true,
            <<"truncation_reason">> := <<"output_cap">>
        })
    ),
    ContainerShape = #{
        <<"type">> => <<"list">>,
        <<"size">> => 3,
        <<"children">> => [Shape, Shape],
        <<"returned_count">> => 2,
        <<"truncated">> => true
    },
    ?assert(
        ValidOtpState(ServerData#{
            <<"state_shape">> := ContainerShape, <<"visited_node_count">> := 3
        })
    ),
    ImproperListShape = ContainerShape#{<<"size">> := null},
    ?assert(
        ValidOtpState(ServerData#{
            <<"state_shape">> := ImproperListShape, <<"visited_node_count">> := 3
        })
    ),
    TooDeepShape = lists:foldl(
        fun(_, Child) ->
            #{
                <<"type">> => <<"tuple">>,
                <<"size">> => 1,
                <<"children">> => [Child],
                <<"returned_count">> => 1,
                <<"truncated">> => false
            }
        end,
        Shape,
        lists:seq(1, 6)
    ),
    SecondHandler = Handler#{<<"index">> := 2},
    FullShape = full_state_shape(0),
    OversizedCount = (65536 div erlang:external_size(FullShape)) + 1,
    ?assert(OversizedCount =< 200),
    ?assert(OversizedCount * 127 =< 10000),
    OversizedHandlers = [
        Handler#{<<"index">> := Index, <<"state_shape">> := FullShape}
     || Index <- lists:seq(1, OversizedCount)
    ],
    OversizedShapes = EventData#{
        <<"limits">> := EventLimits#{<<"handler_output_count">> := OversizedCount},
        <<"observed_handler_count">> := OversizedCount,
        <<"returned_count">> := OversizedCount,
        <<"handlers">> := OversizedHandlers,
        <<"truncated">> := true,
        <<"truncation_reason">> := <<"depth_cap">>,
        <<"visited_node_count">> := OversizedCount * 127
    },
    lists:foreach(
        fun(InvalidData) ->
            ?assertNot(ValidOtpState(InvalidData))
        end,
        [
            ServerData#{<<"acquisition">> := []},
            ServerData#{<<"acquisition">> := ServerAcquisition#{<<"extra">> => true}},
            ServerData#{<<"limits">> := []},
            ServerData#{<<"limits">> := ServerLimits#{<<"output_bytes">> := 1}},
            ServerData#{<<"limits">> := ServerLimits#{<<"depth">> := 1}},
            ServerData#{<<"limits">> := ServerLimits#{<<"nodes">> := 1}},
            ServerData#{<<"limits">> := ServerLimits#{<<"container_prefix">> := 1}},
            ServerData#{<<"limits">> := ServerLimits#{<<"semantic_identifier_bytes">> := 1}},
            ServerData#{<<"limits">> := ServerLimits#{<<"extra">> => true}},
            ServerData#{<<"extra">> => true},
            ServerData#{<<"visited_node_count">> := 10001},
            ServerData#{<<"state_shape">> := #{<<"secret">> => <<"value">>}},
            ServerData#{<<"state_shape">> := Shape#{<<"extra">> => true}},
            ServerData#{<<"state_shape">> := null},
            ServerData#{<<"state_shape">> := TooDeepShape, <<"visited_node_count">> := 7},
            ServerData#{
                <<"state_shape">> := ImproperListShape#{
                    <<"type">> := <<"map">>
                },
                <<"visited_node_count">> := 3
            },
            ServerData#{
                <<"state_shape">> := ImproperListShape#{
                    <<"truncation_reason">> => <<"depth_cap">>
                },
                <<"visited_node_count">> := 3
            },
            ServerData#{
                <<"truncated">> := false, <<"truncation_reason">> := <<"output_cap">>
            },
            StatemData#{
                <<"current_state">> := binary:copy(<<"x">>, 129),
                <<"current_state_identity">> := <<"available">>
            },
            StatemData#{
                <<"truncated">> := true, <<"truncation_reason">> := <<"output_cap">>
            },
            EventData#{<<"returned_count">> := 0},
            EventData#{<<"dropped_count">> := 1},
            EventData#{<<"visited_node_count">> := 0},
            EmptyEvent#{<<"visited_node_count">> := 1},
            EventData#{<<"shape_budget_exhausted_count">> := -1},
            EventData#{<<"shape_budget_exhausted_count">> := 1},
            EventData#{
                <<"truncated">> := true, <<"truncation_reason">> := <<"node_cap">>
            },
            EventData#{
                <<"truncated">> := true, <<"truncation_reason">> := <<"output_cap">>
            },
            PartiallyReturnedEvent#{<<"shape_budget_exhausted_count">> := 1},
            PartiallyReturnedEvent#{<<"truncation_reason">> := <<"output_cap">>},
            EventData#{
                <<"observed_handler_count">> := 2,
                <<"dropped_count">> := 1,
                <<"truncated">> := true,
                <<"truncation_reason">> := <<"depth_cap">>
            },
            EventData#{
                <<"limits">> := EventLimits#{<<"handler_output_count">> := 1},
                <<"observed_handler_count">> := 2,
                <<"returned_count">> := 2,
                <<"handlers">> := [Handler, SecondHandler]
            },
            EventData#{<<"handlers">> := [Handler#{<<"index">> := 2}]},
            EventData#{<<"handlers">> := [Handler#{<<"state_shape">> := null}]},
            EventData#{<<"handlers">> := [Handler#{<<"extra">> => true}]},
            OversizedShapes
        ]
    ),
    InvalidHandler = maps:remove(
        <<"state_shape">>, hd(maps:get(<<"handlers">>, otp_state_data(gen_event)))
    ),
    ?assertNot(
        observer_cli_escriptize:valid_command_payload(
            otp_state,
            otp_state_success_payload(
                (otp_state_data(gen_event))#{<<"handlers">> := [InvalidHandler]}
            ),
            [OtpProbe]
        )
    ),
    Unavailable = Probe#{<<"status">> := <<"unavailable">>, <<"reason_code">> := <<"none">>},
    ?assert(
        observer_cli_escriptize:valid_list_command_payload(
            processes, #{<<"status">> => <<"unavailable">>}, [Unavailable]
        )
    ),
    ?assertNot(observer_cli_escriptize:valid_list_command_payload(processes, #{}, [Unavailable])),
    ?assertNot(observer_cli_escriptize:valid_list_command_payload(invalid, #{}, [])),
    ?assert(
        observer_cli_escriptize:valid_required_probes(
            schedulers, <<"complete">>, [Probe#{<<"id">> := <<"scheduler_wall_time">>}]
        )
    ),
    ?assertNot(observer_cli_escriptize:valid_required_probes(schedulers, <<"complete">>, [])),
    SnapshotProbes = [
        Probe#{<<"id">> := Id}
     || Id <- [<<"runtime">>, <<"resources">>, <<"memory">>]
    ],
    ?assert(
        observer_cli_escriptize:valid_required_probes(
            snapshot, <<"complete">>, SnapshotProbes
        )
    ),
    ?assert(
        observer_cli_escriptize:valid_required_probes(
            diagnose, <<"complete">>, [Probe#{<<"id">> := <<"core_limits">>}]
        )
    ),
    ?assert(
        observer_cli_escriptize:valid_required_probes(
            diagnose, <<"complete">>, [Probe#{<<"id">> := <<"core_limits_and_memory">>}]
        )
    ),
    ?assert(
        observer_cli_escriptize:valid_required_probes(
            memory, <<"complete">>, [
                Probe#{<<"id">> := <<"memory">>}, Probe#{<<"id">> := <<"allocator">>}
            ]
        )
    ).

otp_state_data(Behavior) ->
    Shape = #{<<"type">> => <<"atom">>},
    Limits = #{
        <<"output_bytes">> => 65536,
        <<"depth">> => 6,
        <<"nodes">> => 10000,
        <<"container_prefix">> => 2,
        <<"semantic_identifier_bytes">> => 128
    },
    Common = #{
        <<"status">> => <<"ok">>,
        <<"risk_level">> => <<"high">>,
        <<"behavior">> => atom_to_binary(Behavior),
        <<"behavior_source">> => <<"operator_asserted">>,
        <<"structural_validation">> => <<"passed">>,
        <<"acquisition">> => #{
            <<"full_state_copy_risk">> => true,
            <<"timeout_ms">> => 5000,
            <<"timeout_retracts_delivered_request">> => false
        },
        <<"limits">> => Limits,
        <<"truncated">> => false,
        <<"truncation_reason">> => null,
        <<"visited_node_count">> => 1
    },
    case Behavior of
        gen_server ->
            Common#{
                <<"structural_validation">> := <<"not_applicable">>, <<"state_shape">> => Shape
            };
        gen_statem ->
            Common#{
                <<"current_state">> => null,
                <<"current_state_identity">> => <<"unavailable">>,
                <<"current_state_shape">> => Shape,
                <<"data_shape">> => Shape
            };
        gen_event ->
            Common#{
                <<"limits">> := Limits#{
                    <<"handler_output_count">> => 20,
                    <<"max_handler_output_count">> => 200
                },
                <<"observed_handler_count">> => 1,
                <<"returned_count">> => 1,
                <<"dropped_count">> => 0,
                <<"shape_budget_exhausted_count">> => 0,
                <<"handlers">> => [
                    #{
                        <<"index">> => 1,
                        <<"module">> => <<"handler">>,
                        <<"id">> => null,
                        <<"id_identity">> => <<"unavailable">>,
                        <<"state_shape">> => Shape
                    }
                ]
            }
    end.

full_state_shape(6) ->
    #{
        <<"type">> => <<"atom">>,
        <<"truncated">> => true,
        <<"truncation_reason">> => <<"depth_cap">>
    };
full_state_shape(Depth) ->
    Child = full_state_shape(Depth + 1),
    #{
        <<"type">> => <<"tuple">>,
        <<"size">> => 2,
        <<"children">> => [Child, Child],
        <<"returned_count">> => 2,
        <<"truncated">> => false
    }.

otp_state_success_payload(Data) ->
    #{
        <<"data">> => Data,
        <<"outcome">> => <<"complete">>,
        <<"issues">> => []
    }.

otp_state_error_payload(Data, Reason, Probe) ->
    {
        #{
            <<"data">> => Data,
            <<"outcome">> => <<"partial">>,
            <<"issues">> => []
        },
        Probe#{<<"status">> := <<"error">>, <<"reason_code">> := Reason}
    }.

validation_redaction_contract() ->
    lists:foreach(
        fun({Key, Prefix}) ->
            ?assertEqual(Prefix, observer_cli_escriptize:identifier_field_prefix(Key))
        end,
        [
            {<<"pid">>, <<"pid-">>},
            {<<"owner">>, <<"pid-">>},
            {<<"group_leader">>, <<"pid-">>},
            {<<"controller">>, <<"pid-">>},
            {<<"controller_peer">>, <<"peer-">>},
            {<<"peer">>, <<"peer-">>},
            {<<"node">>, <<"node-">>},
            {<<"module">>, <<"module-">>},
            {<<"function">>, <<"function-">>},
            {<<"application">>, <<"application-">>},
            {<<"table">>, <<"table-">>},
            {<<"socket">>, <<"socket-">>},
            {<<"port">>, <<"port-">>},
            {<<"sockname">>, <<"endpoint-">>},
            {<<"peername">>, <<"endpoint-">>},
            {<<"registered_name">>, <<"name-">>},
            {<<"other">>, undefined}
        ]
    ),
    ?assert(observer_cli_escriptize:valid_stable_identifier(<<"pid-">>, <<"pid-1">>)),
    ?assertNot(observer_cli_escriptize:valid_stable_identifier(<<"pid-">>, <<"pid-01">>)),
    ?assertNot(observer_cli_escriptize:valid_stable_identifier(<<"pid-">>, <<"bad">>)),
    ?assert(observer_cli_escriptize:valid_redacted_identifier_field(<<"pid">>, <<"pid-1">>)),
    ?assert(observer_cli_escriptize:valid_redacted_identifier_field(<<"pid">>, null)),
    ?assertNot(observer_cli_escriptize:valid_redacted_identifier_field(<<"pid">>, <<"raw">>)),
    ?assertNot(observer_cli_escriptize:valid_redacted_identifier_field(<<"sockname">>, #{})),
    ?assert(observer_cli_escriptize:valid_redaction(#{<<"pid">> => <<"pid-1">>})),
    ?assertNot(observer_cli_escriptize:valid_redaction(#{<<"pid">> => <<"<0.1.0>">>})),
    Evidence = #{
        <<"path">> => <<"/data/context">>,
        <<"sample_index">> => 0,
        <<"monotonic_midpoint_ms">> => 1,
        <<"observed">> => 2,
        <<"operator">> => <<">">>,
        <<"threshold">> => 1
    },
    Finding = #{
        <<"id">> => <<"finding">>,
        <<"severity">> => <<"warning">>,
        <<"entity">> => #{<<"type">> => <<"vm">>, <<"id">> => <<"vm">>},
        <<"summary">> => <<"summary">>,
        <<"ruleset_version">> => 1,
        <<"evidence">> => [Evidence],
        <<"recommendations">> => [<<"inspect">>]
    },
    Response = #{<<"data">> => #{<<"context">> => #{}, <<"findings">> => [Finding]}},
    ?assert(observer_cli_escriptize:valid_evidence(Response, Evidence)),
    ?assertNot(observer_cli_escriptize:valid_evidence(Response, #{})),
    ?assert(observer_cli_escriptize:valid_findings(Response)),
    ?assertNot(
        observer_cli_escriptize:valid_findings(
            #{<<"data">> => #{<<"findings">> => [invalid]}}
        )
    ).

assert_partial_snapshot_exit(Escript, Script, CookieEnv) ->
    Dir = temporary_directory("observer_cli_partial_snapshot"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    Response0 = valid_controller_response(snapshot, <<"node-1">>),
    Probes = [
        case Probe of
            #{<<"id">> := <<"memory">>} ->
                Probe#{
                    <<"status">> := <<"timeout">>,
                    <<"reason_code">> := <<"target_timeout">>,
                    <<"samples">> := 0
                };
            _ ->
                Probe
        end
     || Probe <- maps:get(<<"probes">>, response_capture(Response0))
    ],
    Response = replace_capture(
        Response0#{<<"outcome">> := <<"partial">>},
        (response_capture(Response0))#{<<"probes">> := Probes}
    ),
    ok = file:write_file(
        Source,
        io_lib:format(
            "-module(observer_cli_snapshot).~n"
            "-export([capabilities/0,dispatch/4]).~n"
            "capabilities() -> #{bundle_version => <<\"2.0.0\">>, protocol_version => 1}.~n"
            "dispatch(_,snapshot,_,_) -> ~tp.~n",
            [
                #{
                    <<"status">> => <<"ok">>,
                    <<"result">> => Response,
                    <<"cleanup_confirmed">> => true
                }
            ]
        )
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = observer_cli_partial_snapshot_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        {3, Term} = run_escript(Escript, [
            Script,
            "snapshot",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv,
            "--format",
            "term"
        ]),
        {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
        {ok, Response} = erl_parse:parse_term(Tokens),
        ?assertMatch(
            #{
                <<"outcome">> := <<"partial">>,
                <<"issues">> := [],
                <<"meta">> := #{
                    <<"capture">> := #{
                        <<"probes">> := [_, _, #{<<"reason_code">> := <<"target_timeout">>}]
                    }
                }
            },
            Response
        )
    after
        stop_target(Port),
        file:del_dir_r(Dir)
    end.

missing_capability() ->
    capability_error_test([], {error, capability, diagnostics_missing}).

incompatible_capability() ->
    Dir = temporary_directory("observer_cli_incompatible"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        <<"-module(observer_cli_snapshot).\n-export([capabilities/0]).\ncapabilities() -> #{bundle_version => <<\"1.8.8\">>, protocol_version => 1}.\n">>
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    try
        capability_error_test([Dir], {
            error,
            capability,
            {diagnostics_incompatible, #{
                bundle_version => <<"1.8.8">>, protocol_version => 1
            }}
        })
    after
        file:del_dir_r(Dir)
    end.

undefined_capability() ->
    capability_source_test(
        "-export([noop/0]).\nnoop() -> ok.\n",
        {error, capability, {diagnostics_incompatible, null}}
    ).

failing_capability() ->
    capability_source_test(
        "-export([capabilities/0]).\ncapabilities() -> erlang:error(probe_failed).\n",
        {error, required_probe, capability_probe_failed}
    ).

nonmap_capability() ->
    capability_source_test(
        "-export([capabilities/0]).\ncapabilities() -> invalid.\n",
        {error, capability, {diagnostics_incompatible, #{}}}
    ).

capability_source_test(Body, Expected) ->
    Dir = temporary_directory("observer_cli_capability_source"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(Source, ["-module(observer_cli_snapshot).\n", Body]),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    try
        capability_error_test([Dir], Expected)
    after
        file:del_dir_r(Dir)
    end.

hostile_capability() ->
    Dir = temporary_directory("observer_cli_hostile_capability"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        [
            "-module(observer_cli_snapshot).\n",
            "-export([capabilities/0]).\n",
            "capabilities() -> #{bundle_version => invalid, ",
            "protocol_version => 2147483648, ",
            "secret => do_not_copy}.\n"
        ]
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    try
        capability_error_test([Dir], {
            error,
            capability,
            {diagnostics_incompatible, #{
                bundle_version => null, protocol_version => null
            }}
        })
    after
        file:del_dir_r(Dir)
    end.

capability_error_test(CodePaths, Expected) ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_capability_target_cookie,
    {Port, Target} = start_target(shortnames, Cookie, CodePaths),
    try
        ?assertEqual(
            Expected,
            observer_cli_escriptize:connect_target(
                Target,
                shortnames,
                Cookie,
                10000,
                fun() -> binary:copy(<<16#55>>, 24) end,
                fun net_kernel:connect_node/1,
                fun(_ConnectedTarget, _Capabilities) -> capability_accepted end
            )
        )
    after
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

snapshot_beam_dir() ->
    filename:join(code:lib_dir(observer_cli), "ebin").

start_target(NameMode, Cookie, CodePaths) ->
    Erl = filename:join([
        code:root_dir(), "erts-" ++ erlang:system_info(version), "bin", "erl"
    ]),
    Name = peer:random_name("observer_cli_diagnostic_target"),
    NameArguments =
        case NameMode of
            shortnames -> ["-sname", Name];
            longnames -> ["-name", Name ++ "@127.0.0.1"]
        end,
    PathArguments = lists:append([["-pa", Path] || Path <- CodePaths]),
    Arguments =
        ["-noshell", "-noinput"] ++
            NameArguments ++
            ["-setcookie", atom_to_list(Cookie)] ++
            PathArguments ++
            ["-eval", "io:put_chars(\"READY\\n\"), receive after infinity -> ok end."],
    Port = open_port(
        {spawn_executable, Erl},
        [binary, exit_status, stderr_to_stdout, {args, Arguments}]
    ),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    wait_target_ready(Port, <<>>),
    Host =
        case NameMode of
            shortnames ->
                {ok, Hostname} = inet:gethostname(),
                Hostname;
            longnames ->
                "127.0.0.1"
        end,
    {{Port, OsPid}, list_to_atom(Name ++ "@" ++ Host)}.

wait_target_ready(Port, Output) ->
    receive
        {Port, {data, Data}} ->
            Combined = <<Output/binary, Data/binary>>,
            case binary:match(Combined, <<"READY\n">>) of
                nomatch -> wait_target_ready(Port, Combined);
                _ -> ok
            end;
        {Port, {exit_status, Status}} ->
            erlang:error({target_start_failed, Status, Output})
    after 10000 ->
        erlang:error(target_start_timeout)
    end.

stop_target({Port, OsPid}) ->
    _ = os:cmd("kill -TERM " ++ integer_to_list(OsPid)),
    try port_close(Port) of
        true -> ok
    catch
        error:badarg -> ok
    end.

temporary_directory(Prefix) ->
    Dir = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Dir.

system_module_md5s(Node) ->
    maps:from_list([
        {{App, Module}, remote_module_md5(Node, Module)}
     || App <- [crypto, syntax_tools], Module <- remote_application_modules(Node, App)
    ]).

remote_application_modules(Node, App) ->
    case erpc:call(Node, application, load, [App]) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok
    end,
    {ok, Modules} = erpc:call(Node, application, get_key, [App, modules]),
    Modules.

remote_module_md5(Node, Module) ->
    case erpc:call(Node, code, is_loaded, [Module]) of
        false -> not_loaded;
        _ -> erpc:call(Node, erlang, get_module_info, [Module, md5])
    end.

assert_equal(Expected, Actual) ->
    ?assertEqual(Expected, Actual).

assert_not_equal(Unexpected, Actual) ->
    ?assertNotEqual(Unexpected, Actual).

drain_run_messages(Messages) ->
    receive
        Message when Message =:= remote_load_called; Message =:= tui_started ->
            drain_run_messages(Messages ++ [Message])
    after 0 ->
        Messages
    end.

with_distribution(Fun) ->
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true ->
            Fun(PrevCookie);
        false ->
            Name = list_to_atom(peer:random_name("observer_cli_origin")),
            {ok, _} = net_kernel:start([Name, shortnames]),
            try
                Fun(erlang:get_cookie())
            after
                erlang:set_cookie(node(), PrevCookie),
                net_kernel:stop()
            end
    end.

restore_env(App, Key, {ok, Value}) ->
    application:set_env(App, Key, Value);
restore_env(App, Key, undefined) ->
    application:unset_env(App, Key).

restore_os_env(Name, false) ->
    os:unsetenv(Name);
restore_os_env(Name, Value) ->
    os:putenv(Name, Value).

set_config_home(Root) ->
    Previous = {os:getenv("HOME"), os:getenv("XDG_CONFIG_HOME")},
    true = os:putenv("HOME", Root),
    true = os:putenv("XDG_CONFIG_HOME", Root),
    Previous.

restore_config_home({Home, ConfigHome}) ->
    restore_os_env("HOME", Home),
    restore_os_env("XDG_CONFIG_HOME", ConfigHome).

-endif.
