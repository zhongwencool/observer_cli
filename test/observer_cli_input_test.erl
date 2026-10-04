-module(observer_cli_input_test).
-include_lib("eunit/include/eunit.hrl").

registry_roundtrip_test() ->
    Commands = observer_cli_catalog:commands(),
    ?assertEqual(23, length(Commands)),
    lists:foreach(
        fun(D) ->
            Path = [binary_to_list(P) || P <- maps:get(<<"argv">>, D)],
            ?assertEqual({ok, D}, observer_cli_catalog:describe(Path, false)),
            lists:foreach(
                fun(Args) ->
                    ?assertMatch(
                        {ok, _}, observer_cli_input:parse([binary_to_list(A) || A <- Args])
                    )
                end,
                maps:get(<<"examples">>, D)
            )
        end,
        Commands
    ),
    {ok, Index} = observer_cli_catalog:describe([], false),
    ?assertEqual(5, length(maps:get(<<"entries">>, Index))),
    ?assertNot(maps:is_key(<<"commands">>, Index)),
    {ok, Full} = observer_cli_catalog:describe([], true),
    ?assertEqual(Commands, maps:get(<<"commands">>, Full)).

default_check_test() ->
    {ok, R} = observer_cli_input:parse(["check"]),
    ?assertEqual(diagnose, maps:get(capture, R)),
    ?assertMatch(
        #{observe := "15s", include_identifiers := true, focus := overview},
        maps:get(capture_options, R)
    ),
    {ok, Redacted} = observer_cli_input:parse(["check", "--redact"]),
    ?assertNot(maps:is_key(include_identifiers, maps:get(capture_options, Redacted))),
    ?assertMatch({ok, _}, observer_cli_input:parse(["check", "cpu", "--window", "5s"])),
    ?assertMatch({error, _}, observer_cli_input:parse(["check", "--window", "4999ms"])),
    ?assertMatch({error, _}, observer_cli_input:parse(["check", "--window", "61s"])).

global_option_positions_test() ->
    A = ["--node", "app@127.0.0.1", "--cookie-env", "COOKIE", "--json", "inspect", "process"],
    B = ["inspect", "--node", "app@127.0.0.1", "process", "--json", "--cookie-env", "COOKIE"],
    ?assertEqual(observer_cli_input:parse(A), observer_cli_input:parse(B)).

stable_sort_meaning_test() ->
    {ok, Now} = observer_cli_input:parse([
        "inspect", "process", "--sort", "memory", "--window", "5s"
    ]),
    ?assertMatch(
        #{sort := "memory", duration := "5s", rank_semantics := current},
        maps:get(capture_options, Now)
    ),
    {ok, Change} = observer_cli_input:parse([
        "inspect", "process", "--sort", "memory-change", "--window", "5s"
    ]),
    ?assertMatch(#{sort := "memory", rank_semantics := delta}, maps:get(capture_options, Change)),
    {ok, Rate} = observer_cli_input:parse([
        "inspect", "process", "--sort", "reductions-rate", "--window", "5s"
    ]),
    ?assertMatch(#{sort := "reductions", rank_semantics := rate}, maps:get(capture_options, Rate)),
    ?assertMatch(
        {error, _}, observer_cli_input:parse(["inspect", "process", "--sort", "reductions-rate"])
    ),
    ?assertMatch(
        {error, _}, observer_cli_input:parse(["inspect", "network", "--sort", "oct-change"])
    ).

explicit_selectors_test() ->
    {ok, Pid} = observer_cli_input:parse(["inspect", "process", "--pid", "<0.123.0>"]),
    ?assertEqual(process, maps:get(capture, Pid)),
    ?assertMatch(
        #{selector_kind := pid, arguments := ["<0.123.0>"]}, maps:get(capture_options, Pid)
    ),
    {ok, Name} = observer_cli_input:parse(["inspect", "process", "--name", "<0.123.0>"]),
    ?assertMatch(#{selector_kind := name}, maps:get(capture_options, Name)),
    ?assertMatch({error, _}, observer_cli_input:parse(["inspect", "process", "--pid", "pid-1"])),
    ?assertMatch(
        {error, _},
        observer_cli_input:parse(["inspect", "process", "--pid", "<0.1.0>", "--sort", "memory"])
    ),
    ?assertMatch(
        {error, _},
        observer_cli_input:parse(["inspect", "process", "--name", "init", "--pid", "<0.1.0>"])
    ),
    ?assertMatch({error, _}, observer_cli_input:parse(["inspect", "port", "--id", "port-1"])).

trace_pid_preflight_test() ->
    Prefix = ["trace", "call", "timer:sleep/1", "--replace-existing-trace", "--pid"],
    lists:foreach(
        fun(Pid) ->
            ?assertMatch({error, _}, observer_cli_input:parse(Prefix ++ [Pid]))
        end,
        ["pid-1", "all", "init", "<1.2.0>", "<0.1>", "<0.1.0>extra"]
    ),
    ?assertMatch(
        {ok, #{capture_options := #{pid := "<0.123.0>"}}},
        observer_cli_input:parse(Prefix ++ ["<0.123.0>"])
    ).

invalid_before_target_test() ->
    Cases = [
        ["check", "--node", "app@host"],
        ["check", "--cookie-env", "COOKIE"],
        ["check", "--node", "app@host", "--cookie-env", "COOKIE", "--cookie-file", "/tmp/cookie"],
        ["check", "--json", "--format", "text"],
        ["check", "--json", "--verbose"],
        ["check", "--window", "5s", "--timeout", "6s"],
        ["check", "cpu", "--deep"],
        ["check", "memory", "--deep", "--app", "kernel"],
        ["inspect", "logs", "--redact"],
        ["inspect", "state", "--pid", "<0.1.0>", "--behavior", "gen_server"],
        ["trace", "call", "timer:sleep/1", "--pid", "<0.1.0>"],
        ["trace", "stop"],
        ["tui", "--json"],
        ["describe", "--schema"],
        ["describe", "inspect", "process", "--schema", "--json"],
        ["describe", "check", "--full", "--json"]
    ],
    lists:foreach(fun(Args) -> ?assertMatch({error, _}, observer_cli_input:parse(Args)) end, Cases).

legacy_paths_fail_test() ->
    lists:foreach(
        fun(Name) ->
            ?assertMatch({error, _}, observer_cli_input:parse([Name])),
            ?assertMatch({error, _}, observer_cli_input:parse([Name, "--help"]))
        end,
        [
            "connect",
            "status",
            "disconnect",
            "diagnose",
            "snapshot",
            "processes",
            "memory",
            "otp-state"
        ]
    ),
    ?assertMatch({ok, #{route := help}}, observer_cli_input:parse(["inspect", "--help"])),
    ?assertMatch({ok, #{route := help}}, observer_cli_input:parse(["inspect"])).

offline_output_preflight_test() ->
    Invalid = [
        ["--json"],
        ["inspect", "--json"],
        ["trace", "--format", "term"],
        ["--help", "--json"],
        ["check", "--help", "--format", "json"],
        ["--version", "--format", "garbage"],
        ["--version", "--json"],
        ["--help", "--version"],
        ["--node", "app@host"],
        ["--cookie-env", "COOKIE"],
        ["inspect", "--node", "app@host", "--cookie-env", "COOKIE"]
    ],
    lists:foreach(
        fun(Args) -> ?assertMatch({error, _}, observer_cli_input:parse(Args)) end, Invalid
    ),
    lists:foreach(
        fun(Args) -> ?assertMatch({ok, #{route := help}}, observer_cli_input:parse(Args)) end,
        [
            [],
            ["--help"],
            ["inspect"],
            ["trace", "call", "--help"],
            ["help", "check"],
            ["--help", "--format", "text"]
        ]
    ),
    ?assertMatch({ok, #{route := version}}, observer_cli_input:parse(["--version"])),
    ?assertMatch(
        {ok, #{route := version}}, observer_cli_input:parse(["--version", "--format", "text"])
    ).

root_help_budget_test() ->
    Help = observer_cli_catalog:help([]),
    Lines = binary:split(Help, <<"\n">>, [global]),
    ?assert(length(Lines) - 1 =< 24),
    ?assert(lists:all(fun(L) -> length(unicode:characters_to_list(L)) =< 80 end, Lines)),
    ?assertNotEqual(nomatch, binary:match(Help, <<"15-second">>)).
