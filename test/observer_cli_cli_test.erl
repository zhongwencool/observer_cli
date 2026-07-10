-module(observer_cli_cli_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

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
        {"sockets", sockets},
        {"gen-server-state", gen_server_state},
        {"supervision-tree", supervision_tree},
        {"trace", trace},
        {"diagnose", diagnose}
    ],
    lists:foreach(
        fun({Word, Command}) ->
            ?assertMatch(
                {ok, #{route := command, command := Command}},
                observer_cli_cli:parse([Word])
            ),
            ?assertMatch(
                {ok, #{route := command, command := Command}},
                observer_cli_cli:parse([Word, "cookie", "1500"])
            )
        end,
        Commands
    ).

legacy_tui_forms_test() ->
    ?assertEqual(
        {ok, #{route => tui, target => "target@host", cookie => undefined, interval => 1500}},
        observer_cli_cli:parse(["target@host"])
    ),
    ?assertEqual(
        {ok, #{route => tui, target => "target@host", cookie => "secret", interval => 2000}},
        observer_cli_cli:parse(["target@host", "secret", "2000"])
    ).

explicit_tui_escape_test() ->
    ?assertEqual(
        {ok, #{route => tui, target => "memory", cookie => undefined, interval => 1500}},
        observer_cli_cli:parse(["tui", "memory"])
    ),
    ?assertEqual(
        {ok, #{route => tui, target => "diagnose", cookie => "secret", interval => 1500}},
        observer_cli_cli:parse(["tui", "diagnose", "secret", "1500"])
    ).

command_options_test() ->
    ?assertEqual(
        {ok, #{
            route => command,
            command => processes,
            arguments => [],
            options => #{node => "target@host", sort => "memory", limit => "20", json => true}
        }},
        observer_cli_cli:parse([
            "processes", "--node", "target@host", "--sort", "memory", "--limit", "20", "--json"
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
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["target", "cookie"])),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["tui"])),
    assert_argument_error(
        invalid_refresh_interval,
        observer_cli_cli:parse(["tui", "target", "cookie", "fast"])
    ).

assert_argument_error(Reason, Result) ->
    ?assertEqual(
        {error, #{category => argument, exit_code => 2, reason => Reason}},
        Result
    ).

-endif.
