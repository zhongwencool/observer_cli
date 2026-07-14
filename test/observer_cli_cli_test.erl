-module(observer_cli_cli_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

reserved_command_words_test() ->
    Commands = [
        {"connect", connect},
        {"status", status},
        {"disconnect", disconnect},
        {"snapshot", snapshot},
        {"memory", memory},
        {"schedulers", schedulers},
        {"distribution", distribution},
        {"processes", processes},
        {"process", process},
        {"applications", applications},
        {"ets", ets},
        {"mnesia", mnesia},
        {"network", network},
        {"ports", ports},
        {"port", port},
        {"sockets", sockets},
        {"otp-state", otp_state},
        {"supervision-tree", supervision_tree},
        {"logs", logs},
        {"trace", trace},
        {"diagnose", diagnose}
    ],
    lists:foreach(
        fun({Word, Command}) ->
            Arguments =
                case Command of
                    process -> [Word, "<0.1.0>"];
                    port -> [Word, "#Port<0.1>"];
                    otp_state -> [Word, "<0.1.0>", "--behavior", "gen_server"];
                    supervision_tree -> [Word, "--app", "kernel"];
                    trace -> [Word, "stop", "--all"];
                    _ -> [Word]
                end,
            ?assertMatch(
                {ok, #{route := command, command := Command}},
                observer_cli_cli:parse(Arguments)
            )
        end,
        Commands
    ).

positional_tui_forms_are_unknown_commands_test() ->
    ?assertMatch(
        {error, #{reason := {unknown_command, "target@host"}}},
        observer_cli_cli:parse(["target@host"])
    ),
    ?assertMatch(
        {error, #{reason := {unknown_command, "target@host"}}},
        observer_cli_cli:parse(["target@host", "secret", "2000"])
    ).

explicit_tui_escape_test() ->
    ?assertEqual(
        {ok, #{
            route => tui,
            target => "memory",
            cookie => undefined,
            interval => 1500
        }},
        observer_cli_cli:parse(["tui", "memory"])
    ),
    ?assertEqual(
        {ok, #{
            route => tui,
            target => "diagnose",
            cookie => "secret",
            interval => 1500
        }},
        observer_cli_cli:parse(["tui", "diagnose", "secret", "1500"])
    ).

command_options_test() ->
    ?assertEqual(
        {ok, #{
            route => command,
            command => processes,
            arguments => [],
            options => #{
                node => "target@host",
                cookie_env => "ERL_COOKIE",
                sort => "memory",
                limit => "20",
                json => true
            }
        }},
        observer_cli_cli:parse([
            "processes",
            "--node",
            "target@host",
            "--cookie-env",
            "ERL_COOKIE",
            "--sort",
            "memory",
            "--limit",
            "20",
            "--json"
        ])
    ),
    ?assertEqual(
        {ok, #{
            route => command,
            command => trace,
            arguments => ["call", "my_mod:my_fun/2"],
            options => #{pid => "<0.123.0>", replace_existing_trace => true}
        }},
        observer_cli_cli:parse([
            "trace",
            "call",
            "my_mod:my_fun/2",
            "--pid",
            "<0.123.0>",
            "--replace-existing-trace"
        ])
    ).

logs_command_contract_test() ->
    ?assertMatch(
        {ok, #{command := logs, arguments := [], options := #{}}},
        observer_cli_cli:parse(["logs"])
    ),
    ?assertMatch(
        {ok, #{
            command := logs,
            arguments := [],
            options := #{handler := "app file", tail := "2000"}
        }},
        observer_cli_cli:parse(["logs", "--handler", "app file", "--tail", "2000"])
    ),
    lists:foreach(
        fun({Reason, Arguments}) ->
            assert_argument_error(Reason, observer_cli_cli:parse(Arguments))
        end,
        [
            {invalid_tail, ["logs", "--tail", "0"]},
            {invalid_tail, ["logs", "--tail", "2001"]},
            {invalid_tail, ["logs", "--tail", "many"]},
            {unaddressable_handler_id, ["logs", "--handler", "bad\nhandler"]},
            {{unknown_option, "--file"}, ["logs", "--file", "/tmp/app.log"]},
            {unsupported_command_option, ["logs", "--redact"]},
            {unsupported_command_option, ["logs", "--include-identifiers"]},
            {invalid_arguments, ["logs", "extra"]}
        ]
    ).

trace_command_contract_test() ->
    ?assertMatch(
        {ok, #{
            command := trace,
            arguments := ["call", "my_mod:my_fun/2"],
            options := #{
                pid := "<0.123.0>",
                replace_existing_trace := true,
                duration := "60s",
                rate := "200/s"
            }
        }},
        observer_cli_cli:parse([
            "trace",
            "call",
            "my_mod:my_fun/2",
            "--pid",
            "<0.123.0>",
            "--duration",
            "60s",
            "--rate",
            "200/s",
            "--replace-existing-trace"
        ])
    ),
    ?assertMatch(
        {ok, #{command := trace, arguments := ["stop"], options := #{all := true}}},
        observer_cli_cli:parse(["trace", "stop", "--all"])
    ),
    ?assertMatch(
        {ok, #{
            command := trace,
            arguments := ["stop"],
            options := #{all := true, timeout := "5s", redact := true}
        }},
        observer_cli_cli:parse([
            "trace", "stop", "--all", "--timeout", "5s", "--redact"
        ])
    ),
    ?assertMatch(
        {ok, #{options := #{redact := true}}},
        observer_cli_cli:parse([
            "trace",
            "call",
            "my_mod:my_fun/2",
            "--pid",
            "<0.123.0>",
            "--replace-existing-trace",
            "--redact"
        ])
    ),
    ?assertEqual({ok, 10000}, observer_cli_cli:trace_duration(#{})),
    ?assertEqual({ok, 100}, observer_cli_cli:trace_duration(#{duration => "100ms"})),
    ?assertEqual({ok, 60000}, observer_cli_cli:trace_duration(#{duration => "60s"})),
    ?assertEqual({ok, 100}, observer_cli_cli:trace_limit(#{})),
    ?assertEqual({ok, 1}, observer_cli_cli:trace_limit(#{limit => "1"})),
    ?assertEqual({ok, 1000}, observer_cli_cli:trace_limit(#{limit => "1000"})),
    ?assertEqual({ok, {1, 1000}}, observer_cli_cli:trace_limit(#{rate => "1/s"})),
    ?assertEqual({ok, {20, 1000}}, observer_cli_cli:trace_limit(#{rate => "20/s"})),
    ?assertEqual({ok, {200, 1000}}, observer_cli_cli:trace_limit(#{rate => "200/s"})),
    ?assertEqual({ok, 17000}, observer_cli_cli:timeout(#{replace_existing_trace => true})),
    ?assertEqual(
        {ok, 67000},
        observer_cli_cli:timeout(#{replace_existing_trace => true, duration => "60s"})
    ),
    lists:foreach(
        fun({Reason, Arguments}) ->
            assert_argument_error(Reason, observer_cli_cli:parse(Arguments))
        end,
        [
            {replace_existing_trace_required, [
                "trace", "call", "my_mod:my_fun/2", "--pid", "<0.1.0>"
            ]},
            {trace_pid_required, ["trace", "call", "my_mod:my_fun/2", "--replace-existing-trace"]},
            {trace_all_required, ["trace", "stop"]},
            {invalid_mfa, [
                "trace",
                "call",
                "my_mod:*/1",
                "--pid",
                "<0.1.0>",
                "--replace-existing-trace"
            ]},
            {invalid_limit, [
                "trace",
                "call",
                "my_mod:my_fun/2",
                "--pid",
                "<0.1.0>",
                "--limit",
                "1001",
                "--replace-existing-trace"
            ]},
            {invalid_rate, [
                "trace",
                "call",
                "my_mod:my_fun/2",
                "--pid",
                "<0.1.0>",
                "--rate",
                "201/s",
                "--replace-existing-trace"
            ]},
            {invalid_duration, [
                "trace",
                "call",
                "my_mod:my_fun/2",
                "--pid",
                "<0.1.0>",
                "--duration",
                "99ms",
                "--replace-existing-trace"
            ]},
            {{mutually_exclusive_options, limit, rate}, [
                "trace",
                "call",
                "my_mod:my_fun/2",
                "--pid",
                "<0.1.0>",
                "--limit",
                "10",
                "--rate",
                "2/s",
                "--replace-existing-trace"
            ]}
        ]
    ).

deep_snapshot_option_contract_test() ->
    ?assertMatch(
        {ok, #{command := snapshot, arguments := [], options := #{deep := true}}},
        observer_cli_cli:parse(["snapshot", "--deep"])
    ),
    assert_argument_error(
        unsupported_command_option, observer_cli_cli:parse(["memory", "--deep"])
    ),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["snapshot", "extra"])).

quick_diagnose_option_contract_test() ->
    ?assertMatch(
        {ok, #{command := diagnose, arguments := [], options := #{}}},
        observer_cli_cli:parse(["diagnose"])
    ),
    ?assertMatch(
        {ok, #{options := #{observe := "5s"}}},
        observer_cli_cli:parse(["diagnose", "--observe", "5s"])
    ),
    ?assertMatch(
        {ok, #{options := #{observe := "60s", deep := true}}},
        observer_cli_cli:parse(["diagnose", "--observe", "60s", "--deep"])
    ),
    ?assertMatch(
        {ok, #{options := #{observe := "30s", app := "kernel"}}},
        observer_cli_cli:parse(["diagnose", "--observe", "30s", "--app", "kernel"])
    ),
    lists:foreach(
        fun(Arguments) ->
            ?assertMatch(
                {error, #{exit_code := 2}}, observer_cli_cli:parse(["diagnose" | Arguments])
            )
        end,
        [
            ["--deep"],
            ["--app", "kernel"],
            ["--observe", "4s"],
            ["--observe", "61s"],
            ["--observe", "30s", "--deep", "--app", "kernel"]
        ]
    ),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["diagnose", "extra"])).

process_inspection_option_contract_test() ->
    ?assertMatch(
        {ok, #{command := processes, options := #{sort := "reductions", duration := "250ms"}}},
        observer_cli_cli:parse(["processes", "--sort", "reductions", "--duration", "250ms"])
    ),
    ?assertMatch(
        {ok, #{command := applications, options := #{sort := "process_count", limit := "200"}}},
        observer_cli_cli:parse(["applications", "--sort", "process_count", "--limit", "200"])
    ),
    ?assertMatch(
        {ok, #{command := process, arguments := ["registered_name"], options := #{info := true}}},
        observer_cli_cli:parse(["process", "registered_name", "--info"])
    ),
    assert_argument_error(invalid_sort, observer_cli_cli:parse(["processes", "--sort", "cpu"])),
    assert_argument_error(invalid_sort, observer_cli_cli:parse(["applications", "--sort", "cpu"])),
    assert_argument_error(invalid_limit, observer_cli_cli:parse(["processes", "--limit", "201"])),
    ?assertMatch(
        {ok, #{command := processes, options := #{sort := "memory", duration := "250ms"}}},
        observer_cli_cli:parse(["processes", "--sort", "memory", "--duration", "250ms"])
    ),
    assert_argument_error(process_target_required, observer_cli_cli:parse(["process"])),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["applications", "extra"])).

otp_state_is_explicit_high_risk_command_test() ->
    ?assertMatch(
        {ok, #{
            command := otp_state,
            arguments := ["server"],
            options := #{behavior := "gen_server", redact := true}
        }},
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_server", "--redact"
        ])
    ),
    assert_argument_error(
        otp_state_target_required,
        observer_cli_cli:parse(["otp-state", "--behavior", "gen_server"])
    ),
    assert_argument_error(
        otp_state_target_required,
        observer_cli_cli:parse(["otp-state", "one", "two", "--behavior", "gen_server"])
    ),
    assert_argument_error(behavior_required, observer_cli_cli:parse(["otp-state", "server"])),
    lists:foreach(
        fun(Behavior) ->
            assert_argument_error(
                invalid_behavior,
                observer_cli_cli:parse(["otp-state", "server", "--behavior", Behavior])
            )
        end,
        ["gen-server", "GEN_SERVER", "supervisor"]
    ),
    assert_argument_error(
        unsupported_command_option,
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_server", "--limit", "1"
        ])
    ),
    assert_argument_error(
        unsupported_command_option,
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_statem", "--limit", "1"
        ])
    ),
    lists:foreach(
        fun(Limit) ->
            assert_argument_error(
                invalid_limit,
                observer_cli_cli:parse([
                    "otp-state", "server", "--behavior", "gen_event", "--limit", Limit
                ])
            )
        end,
        ["0", "201", "many"]
    ),
    ?assertMatch(
        {ok, #{command := otp_state, options := #{behavior := "gen_event", limit := "200"}}},
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_event", "--limit", "200"
        ])
    ),
    assert_argument_error(
        otp_state_timeout_too_short,
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_server", "--timeout", "9999ms"
        ])
    ),
    ?assertMatch(
        {ok, #{command := otp_state, options := #{timeout := "10s"}}},
        observer_cli_cli:parse([
            "otp-state", "server", "--behavior", "gen_statem", "--timeout", "10s"
        ])
    ),
    assert_argument_error(
        {unknown_command, "gen-server-state"},
        observer_cli_cli:parse(["gen-server-state", "server"])
    ).

supervision_tree_is_one_level_application_command_test() ->
    ?assertMatch(
        {ok, #{command := supervision_tree, arguments := [], options := #{app := "kernel"}}},
        observer_cli_cli:parse(["supervision-tree", "--app", "kernel"])
    ),
    assert_argument_error(application_required, observer_cli_cli:parse(["supervision-tree"])),
    assert_argument_error(
        invalid_arguments,
        observer_cli_cli:parse(["supervision-tree", "kernel", "--app", "kernel"])
    ),
    assert_argument_error(
        {unknown_option, "--supervisor"},
        observer_cli_cli:parse(["supervision-tree", "--supervisor", "root", "--app", "kernel"])
    ),
    assert_argument_error(
        {unknown_option, "--depth"},
        observer_cli_cli:parse(["supervision-tree", "--app", "kernel", "--depth", "2"])
    ).

table_inspection_option_contract_test() ->
    ?assertMatch(
        {ok, #{command := ets, options := #{sort := "size", limit := "200"}}},
        observer_cli_cli:parse(["ets", "--sort", "size", "--limit", "200"])
    ),
    ?assertMatch(
        {ok, #{command := mnesia, options := #{sort := "memory"}}},
        observer_cli_cli:parse(["mnesia", "--sort", "memory"])
    ),
    assert_argument_error(invalid_sort, observer_cli_cli:parse(["ets", "--sort", "owner"])),
    assert_argument_error(invalid_limit, observer_cli_cli:parse(["mnesia", "--limit", "201"])),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["ets", "extra"])).

global_option_before_command_test() ->
    assert_argument_error(
        global_option_before_command,
        observer_cli_cli:parse(["--node", "target@host", "memory"])
    ).

invalid_option_test() ->
    assert_argument_error(
        {unknown_option, "--unknown"},
        observer_cli_cli:parse(["memory", "--unknown"])
    ),
    assert_argument_error(
        {missing_option_value, "--node"},
        observer_cli_cli:parse(["memory", "--node", "--json"])
    ),
    assert_argument_error(
        {duplicate_option, json},
        observer_cli_cli:parse(["memory", "--json", "--json"])
    ).

validation_edge_paths_test() ->
    Cases = [
        {{duplicate_option, node}, [
            "memory", "--node", "first@host", "--node", "second@host"
        ]},
        {{missing_option_value, "--node"}, ["memory", "--node"]},
        {unsupported_command_option, [
            "trace",
            "call",
            "erlang:node/0",
            "--pid",
            "<0.1.0>",
            "--replace-existing-trace",
            "--all"
        ]},
        {unsupported_command_option, ["trace", "stop", "--all", "--duration", "1s"]},
        {trace_timeout_too_short, [
            "trace",
            "call",
            "erlang:node/0",
            "--pid",
            "<0.1.0>",
            "--replace-existing-trace",
            "--duration",
            "1s",
            "--timeout",
            "7999ms"
        ]},
        {trace_stop_timeout_too_short, [
            "trace", "stop", "--all", "--timeout", "4999ms"
        ]},
        {invalid_timeout, [
            "trace",
            "call",
            "erlang:node/0",
            "--pid",
            "<0.1.0>",
            "--replace-existing-trace",
            "--timeout",
            "invalid"
        ]},
        {invalid_application, ["diagnose", "--observe", "5s", "--app", "bad\napp"]},
        {timeout_too_short, [
            "diagnose", "--observe", "5s", "--timeout", "9999ms"
        ]},
        {invalid_timeout, ["diagnose", "--observe", "5s", "--timeout", "invalid"]},
        {invalid_duration, ["network", "--duration", "invalid"]},
        {invalid_duration, ["processes", "--duration", "invalid"]},
        {unsupported_command_option, [
            "otp-state", "server", "--behavior", "gen_event", "--duration", "1s"
        ]},
        {invalid_timeout, [
            "otp-state", "server", "--behavior", "gen_server", "--timeout", "invalid"
        ]},
        {unsupported_command_option, [
            "trace",
            "call",
            "erlang:node/0",
            "--pid",
            "<0.1.0>",
            "--replace-existing-trace",
            "--handler",
            "default"
        ]},
        {unsupported_command_option, ["diagnose", "--sort", "memory"]},
        {unsupported_command_option, [
            "distribution", "--limit", "20", "--duration", "250ms"
        ]},
        {unsupported_command_option, ["supervision-tree", "--app", "bad\napp"]}
    ],
    lists:foreach(
        fun({Reason, Arguments}) ->
            assert_argument_error(Reason, observer_cli_cli:parse(Arguments))
        end,
        Cases
    ),
    ?assertMatch(
        {ok, #{command := trace}},
        observer_cli_cli:parse([
            "trace",
            "call",
            "erlang:node/0",
            "--pid",
            "<0.1.0>",
            "--replace-existing-trace",
            "--duration",
            "1s",
            "--timeout",
            "8s"
        ])
    ),
    ?assertMatch(
        {ok, #{command := diagnose}},
        observer_cli_cli:parse(["diagnose", "--observe", "5s", "--timeout", "10s"])
    ),
    ?assertEqual({error, invalid_rate}, observer_cli_cli:trace_limit(#{rate => "1/m"})),
    ?assertEqual(
        {error, invalid_duration},
        observer_cli_cli:timeout(#{replace_existing_trace => true, duration => "invalid"})
    ),
    ?assertEqual(
        {error, invalid_duration}, observer_cli_cli:timeout(#{duration => "invalid"})
    ),
    ?assertEqual({ok, 10000}, observer_cli_cli:timeout(#{observe => "5s"})),
    ?assertEqual(
        {error, invalid_observation_duration}, observer_cli_cli:timeout(#{observe => "invalid"})
    ).

command_specific_arguments_and_options_test() ->
    ExtraArguments = [
        ["connect", "extra"],
        ["status", "extra"],
        ["disconnect", "extra"],
        ["snapshot", "extra"],
        ["memory", "extra"],
        ["schedulers", "extra"],
        ["distribution", "extra"],
        ["processes", "extra"],
        ["process", "one", "two"],
        ["applications", "extra"],
        ["ets", "extra"],
        ["mnesia", "extra"],
        ["network", "extra"],
        ["ports", "extra"],
        ["port", "one", "two"],
        ["sockets", "extra"],
        ["otp-state", "one", "two", "--behavior", "gen_server"],
        ["supervision-tree", "extra", "--app", "kernel"],
        ["trace", "stop", "extra", "--all"],
        ["diagnose", "extra"]
    ],
    lists:foreach(
        fun(Arguments) ->
            ?assertMatch({error, #{exit_code := 2}}, observer_cli_cli:parse(Arguments))
        end,
        ExtraArguments
    ),
    IgnoredOptions = [
        ["connect", "--sort", "memory"],
        ["status", "--node", "target@host", "--cookie-env", "COOKIE"],
        ["disconnect", "--timeout", "10s"],
        ["memory", "--app", "kernel"],
        ["schedulers", "--limit", "20"],
        ["distribution", "--duration", "250ms"],
        ["processes", "--app", "kernel"],
        ["process", "<0.1.0>", "--sort", "memory"],
        ["applications", "--duration", "250ms"],
        ["ets", "--duration", "250ms"],
        ["mnesia", "--duration", "250ms"],
        ["network", "--app", "kernel"],
        ["ports", "--duration", "250ms"],
        ["port", "#Port<0.1>", "--sort", "memory"],
        ["sockets", "--app", "kernel"],
        ["otp-state", "server", "--behavior", "gen_server", "--info"],
        ["supervision-tree", "--app", "kernel", "--sort", "memory"],
        ["trace", "stop", "--all", "--sort", "memory"]
    ],
    lists:foreach(
        fun(Arguments) ->
            assert_argument_error(
                unsupported_command_option, observer_cli_cli:parse(Arguments)
            )
        end,
        IgnoredOptions
    ).

mutually_exclusive_options_test() ->
    lists:foreach(
        fun({Arguments, Left, Right}) ->
            assert_argument_error(
                {mutually_exclusive_options, Left, Right},
                observer_cli_cli:parse(["memory" | Arguments])
            )
        end,
        [
            {["--cookie-env", "COOKIE", "--cookie-file", "/tmp/cookie"], cookie_env, cookie_file},
            {["--redact", "--include-identifiers"], redact, include_identifiers},
            {["--limit", "10", "--rate", "5/s"], limit, rate},
            {["--json", "--format", "text"], json, format}
        ]
    ),
    ?assertMatch(
        {ok, #{options := #{json := true, format := "json"}}},
        observer_cli_cli:parse(["memory", "--json", "--format", "json"])
    ).

invalid_tui_forms_test() ->
    assert_argument_error(invalid_arguments, observer_cli_cli:parse([])),
    assert_argument_error(
        {unknown_command, "target"}, observer_cli_cli:parse(["target", "cookie"])
    ),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["tui"])),
    assert_argument_error(
        invalid_refresh_interval,
        observer_cli_cli:parse(["tui", "target", "cookie", "fast"])
    ),
    assert_argument_error(
        invalid_refresh_interval,
        observer_cli_cli:parse(["tui", "target", "cookie", "999"])
    ),
    assert_argument_error(global_option_before_command, observer_cli_cli:parse(["-x"])).

unsupported_format_test() ->
    assert_argument_error(
        {unsupported_format, "yaml"},
        observer_cli_cli:parse(["memory", "--format", "yaml"])
    ).

command_first_loader_is_rejected_test() ->
    assert_argument_error(
        {unknown_option, "--load-diagnostics"},
        observer_cli_cli:parse([
            "connect",
            "--node",
            "target@host",
            "--cookie-env",
            "COOKIE",
            "--load-diagnostics"
        ])
    ).

target_validation_test() ->
    ?assertEqual(
        {ok, {"target@host", shortnames}},
        observer_cli_cli:target(#{node => "target@host", cookie_env => "COOKIE"})
    ),
    ?assertEqual(
        {ok, {"target@host.example", longnames}},
        observer_cli_cli:target(#{node => "target@host.example", cookie_env => "COOKIE"})
    ),
    ?assertEqual(
        {ok, {"target@host", longnames}},
        observer_cli_cli:target(#{
            node => "target@host", cookie_env => "COOKIE", name_mode => "long"
        })
    ),
    {ok, Host} = inet:gethostname(),
    ?assertEqual(
        {ok, {"target@" ++ Host, shortnames}},
        observer_cli_cli:target(#{node => "target"})
    ),
    ?assertEqual({error, no_active_context}, observer_cli_cli:target(#{})),
    lists:foreach(
        fun(Node) ->
            assert_argument_error(
                invalid_node,
                observer_cli_cli:parse(["memory", "--node", Node, "--cookie-env", "COOKIE"])
            )
        end,
        ["@host", "target@", "target@@host", "target\n@host", lists:duplicate(256, $a)]
    ),
    assert_argument_error(
        {unsupported_name_mode, "wide"},
        observer_cli_cli:parse([
            "memory", "--node", "target@host", "--cookie-env", "COOKIE", "--name-mode", "wide"
        ])
    ),
    assert_argument_error(
        missing_cookie_source,
        observer_cli_cli:parse(["memory", "--node", "target@host"])
    ).

cookie_source_test() ->
    Env = "OBSERVER_CLI_COOKIE_SOURCE_TEST",
    Secret = "test cookie",
    true = os:putenv(Env, Secret),
    try
        ?assertEqual(
            {ok, list_to_binary(Secret)}, observer_cli_cli:cookie_source(#{cookie_env => Env})
        ),
        true = os:putenv(Env, ""),
        ?assertEqual({error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_env => Env}))
    after
        true = os:unsetenv(Env)
    end,
    ?assertEqual(
        {error, cookie_source_unavailable},
        observer_cli_cli:cookie_source(#{cookie_env => "OBSERVER_CLI_MISSING_COOKIE_TEST"})
    ),
    ?assertEqual(
        {error, invalid_cookie_source}, observer_cli_cli:cookie_source(#{cookie_env => "BAD=NAME"})
    ),
    ?assertEqual({error, missing_cookie_source}, observer_cli_cli:cookie_source(#{})),
    with_cookie_file(<<"file_secret\r\n">>, 8#600, fun(Path) ->
        ?assertEqual(
            {ok, <<"file_secret">>}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(<<"file_secret\n\n">>, 8#600, fun(Path) ->
        ?assertEqual(
            {error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(<<"file_secret">>, 8#644, fun(Path) ->
        ?assertEqual(
            {error, cookie_file_permissions},
            observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(binary:copy(<<"x">>, 256), 8#600, fun(Path) ->
        ?assertEqual(
            {error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(binary:copy(<<"x">>, 258), 8#600, fun(Path) ->
        ?assertEqual(
            {error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(<<>>, 8#600, fun(Path) ->
        ?assertEqual(
            {error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    with_cookie_file(<<1>>, 8#600, fun(Path) ->
        ?assertEqual(
            {error, invalid_cookie}, observer_cli_cli:cookie_source(#{cookie_file => Path})
        )
    end),
    ?assertEqual(
        {error, cookie_source_unavailable},
        observer_cli_cli:cookie_source(#{cookie_file => "/missing/observer-cli-cookie"})
    ),
    ?assertEqual(
        {error, invalid_cookie_source}, observer_cli_cli:cookie_source(#{cookie_file => invalid})
    ),
    Directory = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_cookie_dir_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Directory),
    try
        ?assertEqual(
            {error, cookie_source_unavailable},
            observer_cli_cli:cookie_source(#{cookie_file => Directory})
        )
    after
        file:del_dir(Directory)
    end.

context_option_validation_test() ->
    ?assertEqual({error, no_active_context}, observer_cli_cli:context_options(#{})),
    ?assertEqual({error, no_active_context}, observer_cli_cli:save_context(#{})),
    ?assertEqual(
        {error, invalid_cookie_source},
        observer_cli_cli:context_options(#{node => "target", cookie_env => "BAD=NAME"})
    ),
    ?assertEqual(
        {error, missing_cookie_source}, observer_cli_cli:context_options(#{node => "target"})
    ),
    ?assertEqual(
        {error, invalid_node},
        observer_cli_cli:context_options(#{node => "@", cookie_env => "COOKIE"})
    ),
    ?assertMatch(
        {ok, #{name_mode := "long", cookie_env := "COOKIE"}},
        observer_cli_cli:context_options(#{
            node => "target@host.example", cookie_env => "COOKIE"
        })
    ),
    ?assertEqual(
        {error, invalid_cookie_source},
        observer_cli_cli:context_options(#{
            node => "target", cookie_file => lists:duplicate(4097, $x)
        })
    ).

context_file_test() ->
    with_context_path(fun(Path) ->
        Secret = <<"must_not_be_stored">>,
        Context = context_term(<<"first@host">>, <<"env">>, <<"OBSERVER_COOKIE">>),
        ?assertEqual(ok, observer_cli_cli:write_context(Path, Context)),
        {ok, #file_info{mode = DirMode}} = file:read_file_info(filename:dirname(Path)),
        {ok, #file_info{mode = FileMode}} = file:read_file_info(Path),
        ?assertEqual(8#700, DirMode band 8#777),
        ?assertEqual(8#600, FileMode band 8#777),
        {ok, Bytes} = file:read_file(Path),
        ?assertEqual(nomatch, binary:match(Bytes, Secret)),
        ?assertEqual({ok, Context}, observer_cli_cli:read_context(Path)),
        ?assertEqual(
            {ok, #{
                node => "first@host", name_mode => "short", cookie_env => "OBSERVER_COOKIE"
            }},
            observer_cli_cli:decode_context(Context)
        ),
        ?assertEqual(ok, observer_cli_cli:delete_context(Path)),
        ?assertEqual(ok, observer_cli_cli:delete_context(Path))
    end),
    {ok, FileOptions} = observer_cli_cli:context_options(#{
        node => "target@host", cookie_file => "relative-cookie"
    }),
    ?assertEqual(absolute, filename:pathtype(maps:get(cookie_file, FileOptions))).

invalid_context_files_test() ->
    with_context_path(fun(Path) ->
        Valid = context_term(<<"target@host">>, <<"env">>, <<"OBSERVER_COOKIE">>),
        ok = observer_cli_cli:write_context(Path, Valid),
        ok = file:change_mode(filename:dirname(Path), 8#755),
        ?assertEqual(
            {error, context_directory_permissions}, observer_cli_cli:read_context(Path)
        ),
        ok = file:change_mode(filename:dirname(Path), 8#700),
        write_context_bytes(
            Path, term_to_binary(#{payload => binary:copy(<<"x">>, 1000)}, [compressed])
        ),
        ?assertEqual({error, invalid_context}, observer_cli_cli:read_context(Path)),
        write_context_bytes(Path, <<131, 255, 0>>),
        ?assertEqual({error, invalid_context}, observer_cli_cli:read_context(Path)),
        write_context_bytes(Path, binary:copy(<<0>>, 8193)),
        ?assertEqual({error, context_too_large}, observer_cli_cli:read_context(Path)),
        write_context_bytes(Path, term_to_binary(Valid)),
        ok = file:change_mode(Path, 8#644),
        ?assertEqual({error, context_file_permissions}, observer_cli_cli:read_context(Path)),
        ok = file:delete(Path),
        ok = file:make_dir(Path),
        ?assertEqual({error, invalid_context_file}, observer_cli_cli:read_context(Path)),
        ok = file:del_dir(Path),
        Target = Path ++ ".target",
        ok = file:write_file(Target, term_to_binary(Valid)),
        ok = file:make_symlink(Target, Path),
        ?assertEqual({error, invalid_context_file}, observer_cli_cli:read_context(Path)),
        ?assertEqual({error, invalid_context_file}, observer_cli_cli:delete_context(Path)),
        ok = file:delete(Path),
        ok = file:delete(Target)
    end),
    ?assertEqual(
        {error, invalid_context},
        observer_cli_cli:decode_context(
            (context_term(<<"target@host">>, <<"env">>, <<"OBSERVER_COOKIE">>))#{
                <<"extra">> => true
            }
        )
    ),
    ?assertEqual(
        {error, invalid_context},
        observer_cli_cli:decode_context(
            context_term(<<"target@host">>, <<"plain">>, <<"secret">>)
        )
    ).

concurrent_context_replace_test() ->
    with_context_path(fun(Path) ->
        Parent = self(),
        Contexts = [
            context_term(<<"first@host">>, <<"env">>, <<"FIRST_COOKIE">>),
            context_term(<<"second@host">>, <<"env">>, <<"SECOND_COOKIE">>)
        ],
        Refs = [
            begin
                Ref = make_ref(),
                spawn(fun() -> Parent ! {Ref, observer_cli_cli:write_context(Path, Context)} end),
                Ref
            end
         || Context <- Contexts
        ],
        ?assertEqual(
            [ok, ok],
            lists:sort([
                receive
                    {Ref, Result} -> Result
                end
             || Ref <- Refs
            ])
        ),
        {ok, Winner} = observer_cli_cli:read_context(Path),
        ?assert(lists:member(Winner, Contexts)),
        {ok, Files} = file:list_dir(filename:dirname(Path)),
        ?assertEqual(["context.etf"], Files)
    end).

timeout_validation_test() ->
    ?assertEqual({ok, 10000}, observer_cli_cli:timeout(#{})),
    ?assertEqual({ok, 1500}, observer_cli_cli:timeout(#{timeout => "1500ms"})),
    ?assertEqual({ok, 10000}, observer_cli_cli:timeout(#{timeout => "10s"})),
    ?assertEqual({ok, 120000}, observer_cli_cli:timeout(#{timeout => "120s"})),
    lists:foreach(
        fun(Text) ->
            assert_argument_error(
                invalid_timeout, observer_cli_cli:parse(["memory", "--timeout", Text])
            )
        end,
        ["0", "121s", "forever"]
    ).

scheduler_duration_and_deadline_validation_test() ->
    ?assertEqual({ok, 1500}, observer_cli_cli:duration(#{})),
    ?assertEqual({ok, 250}, observer_cli_cli:duration(#{duration => "250ms"})),
    ?assertEqual({ok, 10000}, observer_cli_cli:duration(#{duration => "10s"})),
    ?assertEqual({ok, 15000}, observer_cli_cli:timeout(#{duration => "10s"})),
    lists:foreach(
        fun(Text) ->
            assert_argument_error(
                invalid_duration,
                observer_cli_cli:parse(["schedulers", "--duration", Text])
            )
        end,
        ["249ms", "10001ms", "forever"]
    ),
    assert_argument_error(
        timeout_too_short,
        observer_cli_cli:parse([
            "schedulers", "--duration", "10s", "--timeout", "14999ms"
        ])
    ),
    ?assertMatch(
        {ok, #{command := schedulers}},
        observer_cli_cli:parse([
            "schedulers", "--duration", "10s", "--timeout", "15s"
        ])
    ),
    ?assertMatch(
        {ok, #{command := distribution}},
        observer_cli_cli:parse(["distribution", "--limit", "200"])
    ),
    assert_argument_error(
        invalid_limit,
        observer_cli_cli:parse(["distribution", "--limit", "201"])
    ).

io_resource_options_test() ->
    ?assertMatch(
        {ok, #{command := network, options := #{sort := "recv_oct", duration := "250ms"}}},
        observer_cli_cli:parse(["network", "--sort", "recv_oct", "--duration", "250ms"])
    ),
    lists:foreach(
        fun(Sort) ->
            ?assertMatch(
                {ok, #{command := network}},
                observer_cli_cli:parse(["network", "--sort", Sort])
            )
        end,
        ["recv_cnt", "send_cnt", "cnt"]
    ),
    ?assertMatch(
        {ok, #{command := ports, options := #{sort := "io", limit := "7"}}},
        observer_cli_cli:parse(["ports", "--sort", "io", "--limit", "7"])
    ),
    ?assertMatch(
        {ok, #{command := sockets, options := #{sort := "fails", duration := "10s"}}},
        observer_cli_cli:parse(["sockets", "--sort", "fails", "--duration", "10s"])
    ),
    ?assertMatch(
        {error, #{reason := invalid_sort}}, observer_cli_cli:parse(["network", "--sort", "io"])
    ),
    lists:foreach(
        fun(Sort) ->
            ?assertMatch(
                {error, #{reason := invalid_sort}},
                observer_cli_cli:parse(["network", "--sort", Sort])
            )
        end,
        ["queue_size", "memory", "input", "output"]
    ),
    ?assertMatch(
        {error, #{reason := unsupported_command_option}},
        observer_cli_cli:parse(["ports", "--duration", "250ms"])
    ),
    ?assertMatch(
        {error, #{reason := timeout_too_short}},
        observer_cli_cli:parse(["sockets", "--duration", "10s", "--timeout", "14999ms"])
    ).

response_envelope_test() ->
    Capture = #{<<"duration_ms">> => 12, <<"probes">> => [], <<"observer_effects">> => []},
    Data = #{<<"memory_bytes">> => 42},
    Response =
        observer_cli_cli:response(
            memory, complete, #{<<"node">> => <<"target@host">>}, Capture, Data, []
        ),
    ?assertEqual(
        #{
            <<"schema">> => <<"observer_cli.cli/v1">>,
            <<"command">> => <<"memory">>,
            <<"outcome">> => <<"complete">>,
            <<"data">> => Data,
            <<"meta">> => #{
                <<"target">> => #{<<"node">> => <<"target@host">>},
                <<"capture">> => Capture
            },
            <<"issues">> => []
        },
        Response
    ),
    ?assertEqual(
        [<<"command">>, <<"data">>, <<"issues">>, <<"meta">>, <<"outcome">>, <<"schema">>],
        lists:sort(maps:keys(Response))
    ),
    Issue = observer_cli_cli:error(argument, invalid_arguments),
    ?assertEqual(
        #{
            <<"severity">> => <<"error">>,
            <<"class">> => <<"argument">>,
            <<"reason_code">> => <<"invalid_arguments">>,
            <<"message">> => <<"invalid arguments">>
        },
        Issue
    ),
    ?assertEqual(
        #{
            <<"schema">> => <<"observer_cli.cli/v1">>,
            <<"command">> => null,
            <<"outcome">> => <<"error">>,
            <<"data">> => null,
            <<"meta">> => #{<<"target">> => null, <<"capture">> => null},
            <<"issues">> => [Issue]
        },
        observer_cli_cli:response(null, error, null, null, null, [Issue])
    ).

term_encoder_round_trip_test() ->
    Response = observer_cli_cli:response(
        null,
        error,
        null,
        null,
        null,
        [observer_cli_cli:error(argument, invalid_arguments)]
    ),
    {ok, Encoded} = observer_cli_cli:encode(term, Response),
    ?assertEqual(<<".\n">>, binary:part(Encoded, byte_size(Encoded) - 2, 2)),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Encoded)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)).

json_encoder_test() ->
    Success = observer_cli_cli:response(
        memory,
        complete,
        #{<<"node">> => <<"target@host">>},
        #{<<"duration_ms">> => 1, <<"probes">> => [], <<"observer_effects">> => []},
        #{},
        []
    ),
    Failure = observer_cli_cli:response(
        null, error, null, null, null, [observer_cli_cli:error(argument, invalid_arguments)]
    ),
    case code:ensure_loaded(json) of
        {module, json} ->
            lists:foreach(
                fun(Response) ->
                    {ok, Encoded} = observer_cli_cli:encode(json, Response),
                    ?assertEqual($\n, binary:last(Encoded)),
                    ?assertEqual(Response, erlang:apply(json, decode, [Encoded]))
                end,
                [Success, Failure]
            );
        {error, _Reason} ->
            lists:foreach(
                fun(Response) ->
                    ?assertEqual(
                        {error, #{
                            category => capability, exit_code => 2, reason => json_unavailable
                        }},
                        observer_cli_cli:encode(json, Response)
                    )
                end,
                [Success, Failure]
            )
    end.

encoder_cap_and_text_escaping_test() ->
    Oversized = observer_cli_cli:response(
        memory,
        complete,
        #{<<"node">> => <<"target@host">>},
        #{<<"duration_ms">> => 1, <<"probes">> => [], <<"observer_effects">> => []},
        #{<<"value">> => binary:copy(<<"x">>, 1024 * 1024)},
        []
    ),
    ?assertEqual(
        {error, #{category => schema, exit_code => 4, reason => response_too_large}},
        observer_cli_cli:encode(term, Oversized)
    ),
    ?assertEqual(
        {error, #{category => schema, exit_code => 4, reason => response_too_large}},
        observer_cli_cli:encode(text, Oversized)
    ),
    Dynamic = <<"safe", 27, "]0;title", 7, 10>>,
    ?assertEqual(<<"safe\\x1B]0;title\\x07\\x0A">>, observer_cli_cli:escape_text(Dynamic)),
    Unicode = <<"中文 😀"/utf8>>,
    ?assertEqual(Unicode, observer_cli_cli:escape_text(Unicode)),
    ?assertEqual(<<"\\x9B">>, observer_cli_cli:escape_text(<<16#C2, 16#9B>>)),
    ?assertEqual(
        <<"\\u{202E}">>, observer_cli_cli:escape_text(<<16#E2, 16#80, 16#AE>>)
    ),
    TextResponse = observer_cli_cli:response(
        memory,
        error,
        null,
        null,
        null,
        [observer_cli_cli:error(argument, {unknown_option, Dynamic})]
    ),
    {ok, Text} = observer_cli_cli:encode(text, TextResponse),
    ?assertEqual(nomatch, binary:match(Text, <<27>>)),
    ?assertEqual(nomatch, binary:match(Text, <<7>>)),
    ?assertEqual(nomatch, binary:match(Text, <<"schema:">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"meta:">>)).

logs_text_encoder_isolates_untrusted_lines_test() ->
    Response = log_response([
        <<"normal 中文"/utf8>>,
        <<"heading\r", 27, "]52;secret", 7>>,
        #{<<"encoding">> => <<"base64">>, <<"data">> => <<"/w==">>}
    ]),
    {ok, Text} = observer_cli_cli:encode(text, Response),
    ?assertNotEqual(nomatch, binary:match(Text, <<"--- UNTRUSTED LOG CONTENT ---">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"| normal 中文"/utf8>>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"| heading\\x0D\\x1B]52;secret\\x07">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"| base64:/w==">>)),
    ?assertEqual(nomatch, binary:match(Text, <<27>>)),
    ?assertEqual(nomatch, binary:match(Text, <<7>>)),
    Content = binary:split(Text, <<"--- UNTRUSTED LOG CONTENT ---\n">>),
    [_Header, Body] = Content,
    lists:foreach(
        fun(Line) ->
            case Line of
                <<>> -> ok;
                <<"| ", _/binary>> -> ok
            end
        end,
        binary:split(Body, <<"\n">>, [global])
    ).

structured_encoders_harden_terminal_codepoints_test() ->
    Dangerous = <<0, 27, 16#C2, 16#9B, 16#E2, 16#80, 16#AE, 16#E2, 16#80, 16#A8>>,
    Response = log_response([Dangerous]),
    {ok, Term} = observer_cli_cli:encode(term, Response),
    ?assertEqual(nomatch, binary:match(Term, <<0>>)),
    ?assertEqual(nomatch, binary:match(Term, <<27>>)),
    ?assertEqual(nomatch, binary:match(Term, <<16#C2, 16#9B>>)),
    ?assertEqual(nomatch, binary:match(Term, <<16#E2, 16#80, 16#AE>>)),
    ?assertEqual(nomatch, binary:match(Term, <<16#E2, 16#80, 16#A8>>)),
    ?assertEqual(1, byte_count(Term, $\n)),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)),
    case code:ensure_loaded(json) of
        {module, json} ->
            {ok, Json} = observer_cli_cli:encode(json, Response),
            ?assertEqual(nomatch, binary:match(Json, <<0>>)),
            ?assertEqual(nomatch, binary:match(Json, <<27>>)),
            ?assertEqual(nomatch, binary:match(Json, <<16#C2, 16#9B>>)),
            ?assertEqual(nomatch, binary:match(Json, <<16#E2, 16#80, 16#AE>>)),
            ?assertEqual(nomatch, binary:match(Json, <<16#E2, 16#80, 16#A8>>)),
            ?assertEqual(1, byte_count(Json, $\n)),
            ?assertEqual(Response, erlang:apply(json, decode, [Json]));
        {error, _} ->
            ok
    end.

logs_maximum_envelope_stays_within_encoder_cap_test() ->
    {Request, Target, Response} = maximum_log_response(),
    ?assert(observer_cli_escriptize:validate_logs_response(Request, Response, Target)),
    ?assert(erlang:external_size(Response) < 1024 * 1024),
    {ok, EncodedText} = observer_cli_cli:encode(text, Response),
    ?assert(byte_size(EncodedText) < 1024 * 1024),
    {ok, EncodedTerm} = observer_cli_cli:encode(term, Response),
    ?assert(byte_size(EncodedTerm) < 1024 * 1024),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(EncodedTerm)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)),
    case code:ensure_loaded(json) of
        {module, json} ->
            {ok, EncodedJson} = observer_cli_cli:encode(json, Response),
            ?assert(byte_size(EncodedJson) < 1024 * 1024),
            ?assertEqual(Response, erlang:apply(json, decode, [EncodedJson]));
        {error, _} ->
            ok
    end.

health_command_text_reports_test() ->
    Response = observer_cli_cli:response(
        diagnose,
        complete,
        #{<<"node">> => <<"node-1">>, <<"otp_release">> => <<"29">>},
        #{
            <<"duration_ms">> => 1500,
            <<"probes">> => [
                #{
                    <<"id">> => <<"core_limits">>,
                    <<"status">> => <<"ok">>,
                    <<"reason_code">> => null,
                    <<"required">> => true,
                    <<"duration_ms">> => 12,
                    <<"samples">> => 2,
                    <<"coverage">> => [<<"process_count_limit">>, <<"port_count_limit">>]
                },
                #{
                    <<"id">> => <<"scheduler_utilization">>,
                    <<"status">> => <<"timeout">>,
                    <<"reason_code">> => <<"probe_timeout">>,
                    <<"required">> => false,
                    <<"duration_ms">> => 1000,
                    <<"samples">> => 0,
                    <<"coverage">> => [<<"scheduler_utilization">>]
                }
            ],
            <<"observer_effects">> => [<<"scheduler_wall_time_enabled">>]
        },
        #{
            <<"summary">> => <<"Quick diagnostics completed with no limit findings.">>,
            <<"sampling_plan">> => #{<<"mode">> => <<"quick">>},
            <<"findings">> => [],
            <<"context">> => #{<<"snapshot">> => #{<<"runtime_samples">> => []}},
            <<"skipped">> => [
                #{
                    <<"id">> => <<"memory_growth_suspects">>,
                    <<"reason_code">> => <<"ruleset_not_calibrated">>
                }
            ]
        },
        []
    ),
    {ok, Text} = observer_cli_cli:encode(text, Response),
    lists:foreach(
        fun(Fragment) -> ?assertNotEqual(nomatch, binary:match(Text, Fragment)) end,
        [
            <<"observer_cli diagnose\n">>,
            <<"node-1">>,
            <<"Quick diagnostics completed with no limit findings.">>,
            <<"memory_growth_suspects">>,
            <<"duration_ms: 1500">>,
            <<"scheduler_utilization">>,
            <<"probe_timeout">>,
            <<"scheduler_wall_time_enabled">>
        ]
    ),
    lists:foreach(
        fun(Fragment) -> ?assertEqual(nomatch, binary:match(Text, Fragment)) end,
        [
            <<"schema:">>,
            <<"command: diagnose">>,
            <<"issues: []">>,
            <<"meta:">>,
            <<"core_limits">>
        ]
    ),
    ?assertEqual(nomatch, binary:match(Text, <<"#{">>)),
    lists:foreach(
        fun(Command) ->
            HealthResponse = Response#{<<"command">> := atom_to_binary(Command)},
            {ok, HealthText} = observer_cli_cli:encode(text, HealthResponse),
            ?assertMatch(<<"observer_cli ", _/binary>>, HealthText),
            ?assertEqual(nomatch, binary:match(HealthText, <<"#{">>))
        end,
        [
            diagnose,
            snapshot,
            memory,
            schedulers,
            distribution,
            network,
            processes,
            process,
            applications,
            ets,
            mnesia,
            ports,
            port,
            sockets,
            otp_state,
            supervision_tree,
            trace_call
        ]
    ).

command_text_and_error_encoding_test() ->
    Capabilities = #{<<"protocol_version">> => 1, <<"bundle_version">> => <<"2.0.0">>},
    ContextData = #{
        <<"node">> => <<"target@host">>,
        <<"name_mode">> => <<"short">>,
        <<"cookie_source">> => #{<<"type">> => <<"env">>, <<"name">> => <<"COOKIE">>},
        <<"probe">> => <<"succeeded">>,
        <<"diagnostics_module">> => <<"compatible">>,
        <<"expected_capabilities">> => Capabilities,
        <<"observed_capabilities">> => Capabilities,
        <<"persistent_connection">> => false
    },
    lists:foreach(
        fun({Command, Fragment}) ->
            Response = observer_cli_cli:response(
                Command,
                complete,
                #{<<"node">> => <<"target@host">>, <<"otp_release">> => <<"29">>},
                #{<<"duration_ms">> => 1, <<"probes">> => [], <<"observer_effects">> => []},
                ContextData,
                []
            ),
            {ok, Text} = observer_cli_cli:encode(text, Response),
            ?assertNotEqual(nomatch, binary:match(Text, Fragment))
        end,
        [
            {connect, <<"Selected target@host">>},
            {status, <<"Active target@host">>}
        ]
    ),
    lists:foreach(
        fun({Node, Fragment}) ->
            Response = observer_cli_cli:response(
                disconnect,
                complete,
                null,
                null,
                #{<<"node">> => Node, <<"disconnected">> => true},
                []
            ),
            {ok, Text} = observer_cli_cli:encode(text, Response),
            ?assertNotEqual(nomatch, binary:match(Text, Fragment))
        end,
        [
            {<<"target@host">>, <<"Removed saved target context for target@host">>},
            {null, <<"No active context">>}
        ]
    ),
    Recovery = observer_cli_cli:response(
        disconnect,
        complete,
        null,
        null,
        #{
            <<"node">> => null,
            <<"disconnected">> => true,
            <<"recovered_invalid_context">> => true
        },
        []
    ),
    ?assertEqual(
        {ok, <<"Removed invalid saved target context.\n">>},
        observer_cli_cli:encode(text, Recovery)
    ),
    ?assertMatch(
        {error, #{reason := unsupported_format}},
        observer_cli_cli:encode(yaml, #{})
    ),
    Missing = observer_cli_cli:response(
        connect,
        complete,
        #{<<"node">> => <<"target@host">>, <<"otp_release">> => <<"29">>},
        #{<<"duration_ms">> => 1, <<"probes">> => [], <<"observer_effects">> => []},
        ContextData#{
            <<"diagnostics_module">> := <<"missing">>,
            <<"observed_capabilities">> := null
        },
        []
    ),
    {ok, MissingText} = observer_cli_cli:encode(text, Missing),
    ?assertEqual(nomatch, binary:match(MissingText, <<"--load-diagnostics">>)),
    ?assertNotEqual(nomatch, binary:match(MissingText, <<"Install the matching">>)),
    JsonError =
        case code:ensure_loaded(json) of
            {module, json} -> json_encoding_failed;
            {error, _Reason} -> json_unavailable
        end,
    ?assertMatch(
        {error, #{reason := JsonError}},
        observer_cli_cli:encode(json, #{<<"pid">> => self()})
    ),
    lists:foreach(
        fun({Reason, Message}) ->
            Error = observer_cli_cli:error(argument, Reason),
            ?assertEqual(Message, maps:get(<<"message">>, Error))
        end,
        [
            {{unsupported_format, "yaml"}, <<"unsupported format: yaml">>},
            {json_unavailable, <<"JSON output requires OTP 27 or newer">>},
            {command_unavailable, <<"command capability is not available yet">>},
            {{remote_otp_mismatch, "29", "26"}, <<
                "TUI auto-load requires the same OTP major release; controller OTP 29, target OTP 26"
            >>},
            {invalid_command_response, <<
                "target response schema is incompatible; install the same observer_cli build on the controller and target"
            >>},
            {response_too_large, <<"encoded response exceeds one MiB">>},
            {<<"binary reason">>, <<"binary reason">>}
        ]
    ),
    ?assertEqual(
        <<"unknown_error">>,
        maps:get(<<"reason_code">>, observer_cli_cli:error(argument, 42))
    ),
    ?assertMatch(
        <<"base64:", _/binary>>,
        observer_cli_cli:escape_text(<<"valid", 16#ff>>)
    ).

text_encoder_public_edge_contract_test() ->
    Capabilities = #{<<"protocol_version">> => 1, <<"bundle_version">> => <<"2.0.0">>},
    Incompatible = observer_cli_cli:response(
        connect,
        complete,
        #{<<"node">> => <<"target@host">>, <<"otp_release">> => <<"29">>},
        null,
        #{
            <<"node">> => <<"target@host">>,
            <<"probe">> => <<"succeeded">>,
            <<"diagnostics_module">> => <<"incompatible">>,
            <<"name_mode">> => <<"short">>,
            <<"cookie_source">> => #{<<"type">> => <<"file">>, <<"path">> => <<"/tmp/cookie">>},
            <<"expected_capabilities">> => Capabilities,
            <<"observed_capabilities">> => null
        },
        []
    ),
    {ok, IncompatibleText} = observer_cli_cli:encode(text, Incompatible),
    ?assertNotEqual(nomatch, binary:match(IncompatibleText, <<"incompatible">>)),
    ?assertNotEqual(nomatch, binary:match(IncompatibleText, <<"file:/tmp/cookie">>)),

    {ok, TraceStopText} = observer_cli_cli:encode(
        text, observer_cli_cli:response(trace_stop_all, complete, null, null, #{}, [])
    ),
    ?assertMatch(<<"observer_cli trace stop", _/binary>>, TraceStopText),

    Values = observer_cli_cli:response(
        snapshot,
        <<"partial">>,
        null,
        null,
        #{<<"values">> => [#{}, [], <<>>, 1.5, {sample, json}, [<<"nested">>]]},
        []
    ),
    {ok, ValuesText} = observer_cli_cli:encode(text, Values),
    lists:foreach(
        fun(Fragment) -> ?assertNotEqual(nomatch, binary:match(ValuesText, Fragment)) end,
        [
            <<"[0]: {}">>,
            <<"[1]: []">>,
            <<"[2]: \"\"">>,
            <<"[3]: 1.5">>,
            <<"{sample,json}">>,
            <<"nested">>
        ]
    ),

    Truncated = log_response([<<"tail">>]),
    TruncatedData = (maps:get(<<"data">>, Truncated))#{
        <<"tail">> := (maps:get(<<"tail">>, maps:get(<<"data">>, Truncated)))#{
            <<"truncated_line_indexes">> := [0]
        }
    },
    {ok, TruncatedText} = observer_cli_cli:encode(
        text,
        Truncated#{
            <<"data">> := TruncatedData,
            <<"meta">> := (maps:get(<<"meta">>, Truncated))#{<<"target">> := null}
        }
    ),
    ?assertNotEqual(nomatch, binary:match(TruncatedText, <<"[earlier bytes omitted] tail">>)),

    FailedLogs = observer_cli_cli:response(
        logs,
        error,
        null,
        null,
        #{<<"sources">> => [], <<"selected_source">> => null, <<"tail">> => null},
        []
    ),
    {ok, FailedLogsText} = observer_cli_cli:encode(text, FailedLogs),
    ?assertNotEqual(nomatch, binary:match(FailedLogsText, <<"outcome=error reason=null">>)).

terminal_and_error_message_edge_contract_test() ->
    ?assertEqual(<<"base64:/w==">>, observer_cli_cli:escape_text([<<16#ff>>])),
    ?assertMatch(<<"base64:", _/binary>>, observer_cli_cli:escape_text([16#D800])),
    lists:foreach(
        fun(Codepoint) ->
            Expected = iolist_to_binary(io_lib:format("\\u{~.16B}", [Codepoint])),
            ?assertEqual(
                Expected,
                observer_cli_cli:escape_text(unicode:characters_to_binary([Codepoint]))
            )
        end,
        [16#061C, 16#200E, 16#200F, 16#2029, 16#2066]
    ),
    Cases = [
        {{duplicate_option, cookie_env}, <<"option may only be specified once: --cookie-env">>},
        {{missing_option_value, "--node"}, <<"missing value for option: --node">>},
        {
            {mutually_exclusive_options, cookie_env, cookie_file},
            <<"--cookie-env and --cookie-file cannot be used together">>
        },
        {{unsupported_name_mode, "wide"}, <<"unsupported name mode: wide">>},
        {diagnostics_incompatible,
            <<"the target observer_cli diagnostics bundle is incompatible">>},
        {global_option_before_command, <<"options must appear after the command name">>},
        {missing_cookie_source, <<"--node requires exactly one of --cookie-env or --cookie-file">>},
        {process_target_required, <<"process requires one PID_OR_NAME">>},
        {otp_state_target_required, <<"otp-state requires one PID_OR_NAME">>},
        {invalid_behavior, <<"--behavior must be gen_server, gen_statem, or gen_event">>},
        {observe_required, <<"--deep and --app require --observe DURATION">>},
        {trace_pid_required, <<"trace call requires --pid PID">>},
        {timeout_too_short, <<"--timeout must cover the sampling duration plus five seconds">>},
        {trace_timeout_too_short,
            <<"trace call --timeout must cover the trace duration plus seven seconds">>},
        {trace_stop_timeout_too_short, <<"trace stop --timeout must be at least five seconds">>},
        {otp_state_timeout_too_short, <<"--timeout must be at least 10s for otp-state">>},
        {unaddressable_handler_id,
            <<"--handler must be 1..255 safe Unicode characters and must not start with --">>},
        {invalid_tail, <<"--tail must be an integer from 1 to 2000">>},
        {invalid_refresh_interval, <<"REFRESH_MS must be an integer of at least 1000">>},
        {tui_start_failed,
            <<"interactive TUI startup failed; check target reachability, cookie, and bundle compatibility">>}
    ],
    lists:foreach(
        fun({Reason, Message}) ->
            ?assertEqual(Message, maps:get(<<"message">>, observer_cli_cli:error(argument, Reason)))
        end,
        Cases
    ).

exit_code_classes_test() ->
    ?assertEqual(2, observer_cli_cli:exit_code(capability)),
    ?assertEqual(3, observer_cli_cli:exit_code(partial)),
    ?assertEqual(4, observer_cli_cli:exit_code(schema)),
    ?assertEqual(4, observer_cli_cli:exit_code(unknown)),
    lists:foreach(
        fun({Category, Code}) ->
            ?assertEqual(Code, observer_cli_cli:exit_code(Category)),
            ?assertEqual(Code, observer_cli_cli:exit_code(#{category => Category}))
        end,
        [
            {argument, 2},
            {format, 2},
            {safety_refusal, 3},
            {scan_budget_exceeded, 3},
            {controller, 3},
            {distribution, 3},
            {connection, 3},
            {required_probe, 3},
            {internal, 4},
            {cleanup, 4}
        ]
    ).

private_validation_contract_test() ->
    ?assertEqual(ok, observer_cli_cli:validate_list_values(#{sort => "io"}, ["io"])),
    ?assertEqual(
        {error, invalid_sort},
        observer_cli_cli:validate_list_values(
            #{sort => "bad"}, ["io"]
        )
    ),
    ?assertEqual(
        {error, invalid_sort},
        observer_cli_cli:validate_list_values(
            #{sort => invalid}, ["io"]
        )
    ),
    ?assertEqual(
        {error, invalid_limit},
        observer_cli_cli:validate_list_values(
            #{sort => "io", limit => "201"}, ["io"]
        )
    ),
    ?assertEqual(ok, observer_cli_cli:validate_scheduler_timeout(#{timeout => "6s"}, 1000)),
    ?assertEqual(
        {error, timeout_too_short},
        observer_cli_cli:validate_scheduler_timeout(
            #{timeout => "5999ms"}, 1000
        )
    ),
    ?assertEqual(
        {error, invalid_timeout},
        observer_cli_cli:validate_scheduler_timeout(
            #{timeout => "bad"}, 1000
        )
    ),
    ?assertEqual(ok, observer_cli_cli:validate_scheduler_timeout(#{}, 1000)),
    ?assertEqual(
        {ok, {"name@host", shortnames}},
        observer_cli_cli:finish_target(
            "name", "host", #{}
        )
    ),
    ?assertEqual(
        {ok, {"name@127.0.0.1", longnames}},
        observer_cli_cli:finish_target(
            "name", "127.0.0.1", #{}
        )
    ),
    ?assertEqual(
        {error, invalid_node},
        observer_cli_cli:finish_target(
            lists:duplicate(255, $x), "host", #{}
        )
    ),
    ?assert(observer_cli_cli:valid_env_name("COOKIE_ENV")),
    ?assertNot(observer_cli_cli:valid_env_name("BAD=ENV")),
    ?assertNot(observer_cli_cli:valid_env_name([])),
    ?assert(observer_cli_cli:safe_cookie_file_mode(8#600)),
    ?assertNot(observer_cli_cli:safe_cookie_file_mode(8#644)),
    ?assertEqual(<<"cookie">>, observer_cli_cli:strip_cookie_lf(<<"cookie\n">>)),
    ?assertEqual(<<"cookie">>, observer_cli_cli:strip_cookie_lf(<<"cookie">>)),
    ?assertEqual(<<>>, observer_cli_cli:strip_cookie_lf(<<>>)),
    ?assert(observer_cli_cli:valid_application_name("kernel")),
    ?assertNot(observer_cli_cli:valid_application_name([])),
    lists:foreach(
        fun(Text) -> ?assert(observer_cli_cli:valid_mfa_text(Text)) end,
        ["erlang:node/0", "module:function/255"]
    ),
    lists:foreach(
        fun(Text) -> ?assertNot(observer_cli_cli:valid_mfa_text(Text)) end,
        [
            "",
            "module",
            ":function/1",
            "module:/1",
            "module:function",
            "_:function/1",
            "module:*/1",
            "module:function/256",
            "module:function/bad"
        ]
    ),
    ?assertEqual(250, observer_cli_cli:duration_ms("250ms")),
    ?assertEqual(2000, observer_cli_cli:duration_ms("2s")),
    ?assertEqual(15, observer_cli_cli:duration_ms("15")),
    ?assertEqual(error, observer_cli_cli:duration_ms(invalid)),
    ?assertEqual(6, observer_cli_cli:multiply_duration(3, 2)),
    ?assertEqual(error, observer_cli_cli:multiply_duration(error, 2)),
    ?assertEqual(<<"bad">>, observer_cli_cli:reason_code({bad, detail})),
    ?assertEqual(<<"bad">>, observer_cli_cli:reason_code({bad, left, right})),
    ?assertEqual(<<"bad">>, observer_cli_cli:reason_code(bad)),
    ?assertEqual(<<"bad">>, observer_cli_cli:reason_code(<<"bad">>)),
    ?assertEqual(<<"unknown_error">>, observer_cli_cli:reason_code(42)),
    EnvContext = observer_cli_cli:context_term(#{
        node => "name@host", name_mode => "short", cookie_env => "COOKIE"
    }),
    ?assertMatch(#{<<"cookie_source">> := #{<<"type">> := <<"env">>}}, EnvContext),
    FileContext = observer_cli_cli:context_term(#{
        node => "name@host", name_mode => "short", cookie_file => "/tmp/cookie"
    }),
    ?assertMatch(#{<<"cookie_source">> := #{<<"type">> := <<"file">>}}, FileContext),
    ?assertEqual(
        {ok, #{cookie_env => "COOKIE"}},
        observer_cli_cli:decode_context_source(
            #{<<"type">> => <<"env">>, <<"name">> => <<"COOKIE">>}
        )
    ),
    ?assertEqual(
        {ok, #{cookie_file => "/tmp/cookie"}},
        observer_cli_cli:decode_context_source(
            #{<<"type">> => <<"file">>, <<"path">> => <<"/tmp/cookie">>}
        )
    ),
    ?assertEqual(
        error,
        observer_cli_cli:decode_context_source(
            #{<<"type">> => <<"file">>, <<"path">> => <<"relative">>}
        )
    ),
    ?assertEqual(error, observer_cli_cli:decode_context_source(#{})),
    ?assertEqual(
        {error, invalid_context},
        observer_cli_cli:decode_context_fields(
            <<16#ff>>, <<"short">>, #{<<"type">> => <<"env">>, <<"name">> => <<"COOKIE">>}
        )
    ).

context_filesystem_boundary_test() ->
    Root = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_context_boundary_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Root),
    try
        BadPath = [0],
        ?assertEqual({error, no_active_context}, observer_cli_cli:target(#{})),
        ?assertEqual({error, no_active_context}, observer_cli_cli:context_options(#{})),
        _ = observer_cli_cli:load_context(),
        _ = observer_cli_cli:delete_context(),
        ?assertEqual(
            {error, invalid_context_directory},
            observer_cli_cli:write_context("/dev/null/context.etf", #{})
        ),
        ?assertEqual(
            {error, context_unavailable},
            observer_cli_cli:atomic_write_context(
                filename:join([Root, "missing", "context.etf"]), <<>>
            )
        ),
        ?assertEqual({error, context_unavailable}, observer_cli_cli:ensure_context_dir(BadPath)),
        ?assertEqual(
            {error, context_unavailable}, observer_cli_cli:safe_context_destination(BadPath)
        ),
        ?assertEqual(
            {error, context_unavailable}, observer_cli_cli:readable_context_dir(BadPath)
        ),
        ?assertEqual({error, context_unavailable}, observer_cli_cli:read_context_file(BadPath)),
        ?assertEqual({error, context_unavailable}, observer_cli_cli:delete_context_file(BadPath)),
        ?assertEqual(
            ok,
            observer_cli_cli:delete_context(filename:join([Root, "missing", "context.etf"]))
        ),
        Blocked = filename:join(Root, "blocked"),
        ok = file:make_dir(Blocked),
        ok = file:change_mode(Blocked, 8#500),
        try
            case file:write_file(filename:join(Blocked, "permission-probe"), <<>>) of
                {error, eacces} ->
                    ?assertEqual(
                        {error, context_unavailable},
                        observer_cli_cli:ensure_context_dir(filename:join(Blocked, "child"))
                    );
                {error, eperm} ->
                    ?assertEqual(
                        {error, context_unavailable},
                        observer_cli_cli:ensure_context_dir(filename:join(Blocked, "child"))
                    );
                ok ->
                    ok = file:delete(filename:join(Blocked, "permission-probe"))
            end
        after
            ok = file:change_mode(Blocked, 8#700)
        end,
        ?assertEqual(
            {error, context_unavailable},
            observer_cli_cli:read_context_bytes(filename:join(Root, "missing.etf"))
        ),
        AtomicTemp = filename:join(Root, "atomic-temp"),
        AtomicDest = filename:join(Root, "atomic-dest"),
        ok = file:write_file(AtomicTemp, <<"value">>),
        ?assertEqual(
            ok,
            observer_cli_cli:finish_atomic_write(
                AtomicTemp, AtomicDest, {ok, ok}
            )
        ),
        ?assertEqual(
            {error, context_unavailable},
            observer_cli_cli:finish_atomic_write(
                filename:join(Root, "missing-temp"), AtomicDest, {ok, ok}
            )
        ),
        ?assertEqual(
            {error, context_unavailable},
            observer_cli_cli:finish_atomic_write(
                AtomicTemp, AtomicDest, {{error, failed}, ok}
            )
        ),
        ?assertEqual(
            {error, invalid_context},
            observer_cli_cli:decode_context_fields(
                <<"target@host">>, <<"short">>, #{}
            )
        ),
        Existing = filename:join(Root, "existing"),
        ok = file:make_dir(Existing),
        ?assertEqual(ok, observer_cli_cli:ensure_context_dir(Existing)),
        New = filename:join([Root, "new", "context"]),
        ?assertEqual(ok, observer_cli_cli:ensure_context_dir(New)),
        NotDir = filename:join(Root, "not-dir"),
        ok = file:write_file(NotDir, <<>>),
        ?assertEqual(
            {error, invalid_context_directory},
            observer_cli_cli:ensure_context_dir(NotDir)
        ),
        Path = filename:join(Existing, "context.etf"),
        DirectoryDestination = filename:join(Existing, "directory-destination"),
        ok = file:make_dir(DirectoryDestination),
        ?assertEqual(
            {error, invalid_context_file},
            observer_cli_cli:write_context(DirectoryDestination, #{})
        ),
        ?assertEqual(ok, observer_cli_cli:safe_context_destination(Path)),
        ?assertEqual(
            ok,
            observer_cli_cli:atomic_write_context(Path, term_to_binary(#{ok => true}))
        ),
        ?assertEqual(ok, observer_cli_cli:safe_context_destination(Path)),
        ?assertEqual({ok, #{ok => true}}, observer_cli_cli:read_context_file(Path)),
        ?assertEqual({ok, #{ok => true}}, observer_cli_cli:read_context_bytes(Path)),
        ok = file:change_mode(Path, 8#644),
        ?assertEqual(
            {error, context_file_permissions},
            observer_cli_cli:safe_context_destination(Path)
        ),
        ?assertEqual({error, context_file_permissions}, observer_cli_cli:read_context_file(Path)),
        ?assertEqual({error, context_file_permissions}, observer_cli_cli:delete_context_file(Path)),
        ok = file:delete(Path),
        ok = file:make_dir(Path),
        ?assertEqual(
            {error, invalid_context_file}, observer_cli_cli:safe_context_destination(Path)
        ),
        ?assertEqual({error, invalid_context_file}, observer_cli_cli:read_context_file(Path)),
        ?assertEqual({error, invalid_context_file}, observer_cli_cli:delete_context_file(Path)),
        ?assertEqual(ok, observer_cli_cli:readable_context_dir(Existing)),
        ok = file:change_mode(Existing, 8#755),
        ?assertEqual(
            {error, context_directory_permissions}, observer_cli_cli:delete_context(Path)
        ),
        Locked = filename:join(Root, "locked"),
        ok = file:make_dir(Locked),
        LockedFile = filename:join(Locked, "context.etf"),
        ok = file:write_file(LockedFile, <<>>),
        ok = file:change_mode(LockedFile, 8#600),
        ok = file:change_mode(Locked, 8#500),
        try
            case file:write_file(filename:join(Locked, "permission-probe"), <<>>) of
                {error, eacces} ->
                    ?assertEqual(
                        {error, context_unavailable},
                        observer_cli_cli:delete_context_file(LockedFile)
                    );
                {error, eperm} ->
                    ?assertEqual(
                        {error, context_unavailable},
                        observer_cli_cli:delete_context_file(LockedFile)
                    );
                ok ->
                    ok = file:delete(filename:join(Locked, "permission-probe"))
            end
        after
            ok = file:change_mode(Locked, 8#700)
        end,
        ?assertEqual(
            {error, context_directory_permissions},
            observer_cli_cli:readable_context_dir(Existing)
        ),
        ?assertEqual(
            {error, invalid_context_directory},
            observer_cli_cli:readable_context_dir(NotDir)
        ),
        ?assertEqual(
            {error, no_active_context},
            observer_cli_cli:readable_context_dir(filename:join(Root, "missing"))
        ),
        ?assertEqual(
            {error, no_active_context},
            observer_cli_cli:read_context_file(filename:join(Root, "missing.etf"))
        ),
        ?assertEqual(
            ok,
            observer_cli_cli:delete_context_file(filename:join(Root, "missing.etf"))
        ),
        ?assertEqual(
            {error, invalid_context},
            observer_cli_cli:decode_context_binary({ok, <<131, 80, 0>>})
        ),
        ?assertEqual(
            {error, invalid_context},
            observer_cli_cli:decode_context_binary({ok, <<0>>})
        ),
        ?assertEqual(
            {error, context_too_large},
            observer_cli_cli:decode_context_binary({ok, binary:copy(<<0>>, 8193)})
        ),
        ?assertEqual({error, invalid_context}, observer_cli_cli:decode_context_binary(eof)),
        ?assertEqual(
            {error, context_unavailable},
            observer_cli_cli:decode_context_binary({error, failed})
        ),
        Cookie = filename:join(Root, "cookie"),
        ok = file:write_file(Cookie, <<"cookie">>),
        ?assertEqual({ok, <<"cookie">>}, observer_cli_cli:read_cookie_bytes(Cookie)),
        ?assertEqual({error, unavailable}, observer_cli_cli:read_cookie_bytes(Existing)),
        ?assertEqual({error, invalid_cookie_source}, observer_cli_cli:read_cookie_file(invalid)),
        ?assertEqual(
            {error, cookie_source_unavailable},
            observer_cli_cli:read_cookie_file(filename:join(Root, "missing-cookie"))
        ),
        ?assertEqual(
            {error, cookie_source_unavailable},
            observer_cli_cli:read_cookie_file(Existing)
        ),
        UnreadableCookie = filename:join(Root, "unreadable-cookie"),
        ok = file:write_file(UnreadableCookie, <<"cookie">>),
        ok = file:change_mode(UnreadableCookie, 8#200),
        try
            case file:read_file(UnreadableCookie) of
                {error, eacces} ->
                    ?assertEqual(
                        {error, cookie_source_unavailable},
                        observer_cli_cli:read_cookie_file(UnreadableCookie)
                    );
                {error, eperm} ->
                    ?assertEqual(
                        {error, cookie_source_unavailable},
                        observer_cli_cli:read_cookie_file(UnreadableCookie)
                    );
                {ok, _} ->
                    ok
            end
        after
            ok = file:change_mode(UnreadableCookie, 8#600)
        end,
        ?assertEqual(
            {error, invalid_context},
            observer_cli_cli:decode_context_fields(
                not_a_binary,
                <<"short">>,
                #{<<"type">> => <<"env">>, <<"name">> => <<"COOKIE">>}
            )
        ),
        LargeCookie = filename:join(Root, "large-cookie"),
        ok = file:write_file(LargeCookie, binary:copy(<<"x">>, 258)),
        ?assertEqual({error, invalid_cookie}, observer_cli_cli:read_cookie_file(LargeCookie))
    after
        file:del_dir_r(Root)
    end.

log_response(Lines) ->
    Bytes = lists:sum([
        case Line of
            Binary when is_binary(Binary) -> byte_size(Binary);
            #{<<"data">> := Encoded} -> byte_size(base64:decode(Encoded))
        end
     || Line <- Lines
    ]),
    Source = #{
        <<"id">> => <<"app_file">>,
        <<"addressable">> => true,
        <<"handler_kind">> => <<"logger_std_h_file">>,
        <<"supported">> => true,
        <<"reason_code">> => null
    },
    observer_cli_cli:response(
        logs,
        complete,
        #{<<"node">> => <<"app@host">>, <<"otp_release">> => <<"29">>},
        #{
            <<"started_at">> => <<"2026-07-13T00:00:00.000Z">>,
            <<"finished_at">> => <<"2026-07-13T00:00:00.001Z">>,
            <<"duration_ms">> => 1,
            <<"probes">> => [],
            <<"observer_effects">> => []
        },
        #{
            <<"sources">> => [Source],
            <<"selected_source">> => Source#{
                <<"configured_path">> => <<"/tmp/app.log">>,
                <<"active_handler_fd_match">> => <<"unknown">>
            },
            <<"tail">> => #{
                <<"scope">> => <<"configured_path">>,
                <<"active_handler_fd_match">> => <<"unknown">>,
                <<"visibility">> => <<"reader_visible">>,
                <<"command_filesync_requested">> => false,
                <<"consistency">> => <<"non_atomic">>,
                <<"content_trust">> => <<"untrusted">>,
                <<"requested_lines">> => 200,
                <<"returned_lines">> => length(Lines),
                <<"captured_eof_bytes">> => Bytes,
                <<"bytes_read">> => Bytes,
                <<"has_more">> => false,
                <<"content_truncated">> => false,
                <<"truncation_reasons">> => [],
                <<"truncated_line_indexes">> => [],
                <<"lines">> => Lines
            }
        },
        []
    ).

maximum_log_response() ->
    Ids = [
        unicode:characters_to_binary([
            lists:duplicate(253, 16#1F600), io_lib:format("~2.16.0B", [Index])
        ])
     || Index <- lists:seq(0, 63)
    ],
    [SelectedId | UnsupportedIds] = Ids,
    Source = log_source(SelectedId, true, <<"logger_std_h_file">>, true, null),
    Sources =
        [Source] ++
            [
                log_source(Id, true, <<"other">>, false, <<"unsupported_log_handler">>)
             || Id <- UnsupportedIds
            ],
    Selected = Source#{
        <<"configured_path">> => <<"/", (binary:copy(<<"p">>, 4095))/binary>>,
        <<"active_handler_fd_match">> => <<"unknown">>
    },
    Lines = [
        binary:copy(<<16#C2, 16#80>>, 16384),
        binary:copy(<<0, 1, 9, $", $\\>>, 6553),
        #{<<"encoding">> => <<"base64">>, <<"data">> => <<"/w==">>}
    ],
    Target = binary:copy(<<"n">>, 255),
    Request = #{handler => null, tail => 2000},
    Response = observer_cli_cli:response(
        logs,
        complete,
        #{<<"node">> => Target, <<"otp_release">> => <<"1234567890123456">>},
        #{
            <<"started_at">> => <<"2026-07-13T00:00:00.000Z">>,
            <<"finished_at">> => <<"2026-07-13T00:00:00.001Z">>,
            <<"duration_ms">> => 16#7FFFFFFFFFFFFFFF,
            <<"probes">> => [
                #{
                    <<"id">> => <<"log_file_tail">>,
                    <<"required">> => true,
                    <<"status">> => <<"ok">>,
                    <<"reason_code">> => null,
                    <<"duration_ms">> => 16#7FFFFFFFFFFFFFFF,
                    <<"samples">> => 1,
                    <<"coverage">> => [
                        <<"source_classification_complete">>,
                        <<"source_selected">>,
                        <<"path_prechecked">>,
                        <<"fd_identity_verified">>,
                        <<"bytes_captured">>,
                        <<"post_read_verified">>
                    ]
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
                    <<"module_loaded_before_sample">> => false
                },
                #{
                    <<"id">> => <<"distribution_controller">>,
                    <<"controller_peer">> => Target,
                    <<"dynamic_controller_name_atom">> => true
                },
                #{
                    <<"id">> => <<"configured_log_read">>,
                    <<"handler_ids_enumerated">> => true,
                    <<"handler_config_lookups">> => 65,
                    <<"read_attempts">> => 1,
                    <<"raw_read_cap_bytes">> => 65536,
                    <<"atime_may_change">> => true,
                    <<"consistency">> => <<"non_atomic">>,
                    <<"command_filesync_attempted">> => false
                }
            ]
        },
        #{
            <<"sources">> => Sources,
            <<"selected_source">> => Selected,
            <<"tail">> => #{
                <<"scope">> => <<"configured_path">>,
                <<"active_handler_fd_match">> => <<"unknown">>,
                <<"visibility">> => <<"reader_visible">>,
                <<"command_filesync_requested">> => false,
                <<"consistency">> => <<"non_atomic">>,
                <<"content_trust">> => <<"untrusted">>,
                <<"requested_lines">> => 2000,
                <<"returned_lines">> => 3,
                <<"captured_eof_bytes">> => 65536,
                <<"bytes_read">> => 65536,
                <<"has_more">> => false,
                <<"content_truncated">> => false,
                <<"truncation_reasons">> => [],
                <<"truncated_line_indexes">> => [],
                <<"lines">> => Lines
            }
        },
        []
    ),
    {Request, Target, Response}.

log_source(Id, Addressable, Kind, Supported, Reason) ->
    #{
        <<"id">> => Id,
        <<"addressable">> => Addressable,
        <<"handler_kind">> => Kind,
        <<"supported">> => Supported,
        <<"reason_code">> => Reason
    }.

byte_count(Binary, Byte) ->
    length(binary:matches(Binary, <<Byte>>)).

assert_argument_error(Reason, Result) ->
    ?assertEqual(
        {error, #{category => argument, exit_code => 2, reason => Reason}},
        Result
    ).

with_cookie_file(Contents, Mode, Fun) ->
    Path = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_cookie_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:write_file(Path, Contents),
    ok = file:change_mode(Path, Mode),
    try
        Fun(Path)
    after
        file:delete(Path)
    end.

with_context_path(Fun) ->
    Root = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_context_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Path = filename:join([Root, "observer_cli", "context.etf"]),
    ok = file:make_dir(Root),
    try
        Fun(Path)
    after
        file:del_dir_r(Root)
    end.

context_term(Node, SourceType, SourceValue) ->
    Source =
        case SourceType of
            <<"env">> -> #{<<"type">> => SourceType, <<"name">> => SourceValue};
            <<"file">> -> #{<<"type">> => SourceType, <<"path">> => SourceValue};
            _ -> #{<<"type">> => SourceType, <<"value">> => SourceValue}
        end,
    #{
        <<"version">> => 1,
        <<"node">> => Node,
        <<"name_mode">> => <<"short">>,
        <<"cookie_source">> => Source
    }.

write_context_bytes(Path, Bytes) ->
    ok = file:write_file(Path, Bytes),
    file:change_mode(Path, 8#600).

-endif.
