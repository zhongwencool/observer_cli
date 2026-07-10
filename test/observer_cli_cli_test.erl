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

unsupported_format_test() ->
    assert_argument_error(
        {unsupported_format, "yaml"},
        observer_cli_cli:parse(["memory", "--format", "yaml"])
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

response_envelope_test() ->
    Capture = #{<<"status">> => <<"complete">>},
    Data = #{<<"memory_bytes">> => 42},
    ?assertEqual(
        #{
            <<"schema">> => <<"observer_cli.cli/v1">>,
            <<"command">> => <<"memory">>,
            <<"target">> => #{<<"node">> => <<"target@host">>},
            <<"capture">> => Capture,
            <<"data">> => Data,
            <<"warnings">> => [],
            <<"errors">> => []
        },
        observer_cli_cli:envelope(
            memory, #{<<"node">> => <<"target@host">>}, Capture, Data, [], []
        )
    ),
    Error = observer_cli_cli:error(argument, invalid_arguments),
    ?assertMatch(
        #{<<"capture">> := null, <<"data">> := null, <<"errors">> := [_]},
        observer_cli_cli:envelope(memory, null, null, null, [], [Error])
    ).

term_encoder_round_trip_test() ->
    Response = observer_cli_cli:envelope(
        memory,
        null,
        null,
        null,
        [],
        [observer_cli_cli:error(argument, invalid_arguments)]
    ),
    {ok, Encoded} = observer_cli_cli:encode(term, Response),
    ?assertEqual(<<".\n">>, binary:part(Encoded, byte_size(Encoded) - 2, 2)),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Encoded)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)).

json_encoder_test() ->
    Success = observer_cli_cli:envelope(
        memory,
        #{<<"node">> => <<"target@host">>},
        #{<<"status">> => <<"complete">>},
        #{},
        [],
        []
    ),
    Failure = observer_cli_cli:envelope(
        memory, null, null, null, [], [observer_cli_cli:error(argument, invalid_arguments)]
    ),
    case code:ensure_loaded(json) of
        {module, json} ->
            lists:foreach(
                fun(Response) ->
                    {ok, Encoded} = observer_cli_cli:encode(json, Response),
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
    Oversized = observer_cli_cli:envelope(
        memory,
        #{<<"node">> => <<"target@host">>},
        #{<<"status">> => <<"complete">>},
        #{<<"value">> => binary:copy(<<"x">>, 1024 * 1024)},
        [],
        []
    ),
    ?assertEqual(
        {error, #{category => schema, exit_code => 4, reason => response_too_large}},
        observer_cli_cli:encode(term, Oversized)
    ),
    Dynamic = <<"safe", 27, "]0;title", 7, 10>>,
    ?assertEqual(<<"safe\\x1B]0;title\\x07\\x0A">>, observer_cli_cli:escape_text(Dynamic)),
    TextResponse = observer_cli_cli:envelope(
        memory, null, null, null, [], [observer_cli_cli:error(argument, {unknown_option, Dynamic})]
    ),
    {ok, Text} = observer_cli_cli:encode(text, TextResponse),
    ?assertEqual(nomatch, binary:match(Text, <<27>>)),
    ?assertEqual(nomatch, binary:match(Text, <<7>>)).

exit_code_classes_test() ->
    ?assertEqual(0, observer_cli_cli:exit_code(success)),
    ?assertEqual(1, observer_cli_cli:exit_code(diagnose_findings)),
    ?assertEqual(2, observer_cli_cli:exit_code(capability)),
    ?assertEqual(3, observer_cli_cli:exit_code(partial)),
    ?assertEqual(4, observer_cli_cli:exit_code(schema)),
    ?assertEqual(4, observer_cli_cli:exit_code(unknown)).

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

-endif.
