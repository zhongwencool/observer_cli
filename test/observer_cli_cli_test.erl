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
        {"sockets", sockets},
        {"gen-server-state", gen_server_state},
        {"supervision-tree", supervision_tree},
        {"trace", trace},
        {"diagnose", diagnose}
    ],
    lists:foreach(
        fun({Word, Command}) ->
            Arguments =
                case Command of
                    process -> [Word, "<0.1.0>"];
                    _ -> [Word]
                end,
            ?assertMatch(
                {ok, #{route := command, command := Command}},
                observer_cli_cli:parse(Arguments)
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
    assert_argument_error(
        duration_requires_reductions_sort,
        observer_cli_cli:parse(["processes", "--sort", "memory", "--duration", "250ms"])
    ),
    assert_argument_error(process_target_required, observer_cli_cli:parse(["process"])),
    assert_argument_error(invalid_arguments, observer_cli_cli:parse(["applications", "extra"])).

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
        [
            spawn(fun() -> Parent ! observer_cli_cli:write_context(Path, Context) end)
         || Context <- Contexts
        ],
        ?assertEqual(
            [ok, ok],
            lists:sort([
                receive
                    Result -> Result
                end
             || _ <- Contexts
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
    ?assertMatch(
        {error, #{reason := unsupported_command_option}},
        observer_cli_cli:parse(["ports", "--duration", "250ms"])
    ),
    ?assertMatch(
        {error, #{reason := timeout_too_short}},
        observer_cli_cli:parse(["sockets", "--duration", "10s", "--timeout", "14999ms"])
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
