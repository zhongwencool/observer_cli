-module(observer_cli_catalog_test).

-include_lib("eunit/include/eunit.hrl").

catalog_roundtrip_test() ->
    Commands = observer_cli_catalog:commands(),
    Ids = [maps:get(<<"id">>, D) || D <- Commands],
    ?assertEqual(23, length(Commands)),
    ?assertEqual(length(Ids), length(lists:usort(Ids))),
    ?assertNot(lists:member(<<"tui">>, Ids)),
    lists:foreach(
        fun(D) ->
            Tokens = [binary_to_list(T) || T <- maps:get(<<"argv">>, D)],
            ?assertEqual({ok, D}, observer_cli_catalog:describe(Tokens)),
            Id = binary_to_existing_atom(maps:get(<<"id">>, D), utf8),
            ?assertEqual(maps:get(<<"name">>, D), observer_cli_catalog:public_name(Id)),
            Names = [maps:get(<<"name">>, O) || O <- maps:get(<<"options">>, D)],
            ?assertEqual(length(Names), length(lists:usort(Names))),
            lists:foreach(
                fun(Name) ->
                    ?assertMatch({_, _}, observer_cli_catalog:option("--" ++ binary_to_list(Name)))
                end,
                Names
            )
        end,
        Commands
    ),
    ?assertMatch(
        {ok, #{<<"commands">> := Commands, <<"target_protocol">> := 1}},
        observer_cli_catalog:describe([])
    ),
    {ok, #{<<"commands">> := Traces}} = observer_cli_catalog:describe(["trace"]),
    ?assertEqual([<<"trace_call">>, <<"trace_stop_all">>], [maps:get(<<"id">>, D) || D <- Traces]),
    ?assertEqual({error, unknown_command}, observer_cli_catalog:describe(["tui"])),
    ?assertEqual({error, unknown_command}, observer_cli_catalog:describe(["trace", "unknown"])),
    ?assertEqual({error, unknown_command}, observer_cli_catalog:describe(["memory", "extra"])).

existing_examples_parse_test() ->
    lists:foreach(
        fun(D) ->
            case maps:get(<<"id">>, D) of
                <<"describe">> ->
                    ok;
                _ ->
                    lists:foreach(
                        fun(Example) ->
                            Tokens = [binary_to_list(T) || T <- Example],
                            ?assertMatch({ok, #{route := command}}, observer_cli_cli:parse(Tokens))
                        end,
                        maps:get(<<"examples">>, D)
                    )
            end
        end,
        observer_cli_catalog:commands()
    ).

sort_and_limit_contract_test() ->
    lists:foreach(
        fun(Id) ->
            Name = binary_to_list(observer_cli_catalog:public_name(Id)),
            {ok, D} = observer_cli_catalog:describe([Name]),
            Sort = option(D, <<"sort">>),
            [Default | _] = Values = maps:get(<<"enum">>, Sort),
            ?assertEqual(Default, maps:get(<<"default">>, Sort)),
            lists:foreach(
                fun(Value) ->
                    ?assertMatch(
                        {ok, _}, observer_cli_cli:parse([Name, "--sort", binary_to_list(Value)])
                    )
                end,
                Values
            ),
            ?assertMatch({error, _}, observer_cli_cli:parse([Name, "--sort", "unknown"])),
            Limit = option(D, <<"limit">>),
            ?assertEqual(20, maps:get(<<"default">>, Limit)),
            ?assertMatch({ok, _}, observer_cli_cli:parse([Name, "--limit", "200"])),
            ?assertMatch({error, _}, observer_cli_cli:parse([Name, "--limit", "201"]))
        end,
        [processes, applications, ets, mnesia, network, ports, sockets]
    ).

trace_contract_test() ->
    {ok, Call} = observer_cli_catalog:describe(["trace", "call"]),
    ?assertEqual([<<"--replace-existing-trace">>], maps:get(<<"authorization">>, Call)),
    ?assertEqual(true, maps:get(<<"required">>, option(Call, <<"pid">>))),
    ?assertEqual(100, maps:get(<<"default">>, option(Call, <<"limit">>))),
    ?assertEqual(1000, maps:get(<<"maximum">>, option(Call, <<"limit">>))),
    ?assertEqual(10000, maps:get(<<"default">>, option(Call, <<"duration">>))),
    ?assertEqual(60000, maps:get(<<"maximum_ms">>, option(Call, <<"duration">>))),
    ?assertEqual({ok, 100}, observer_cli_cli:trace_limit(#{})),
    ?assertEqual({ok, 10000}, observer_cli_cli:trace_duration(#{})),
    {ok, Stop} = observer_cli_catalog:describe(["trace", "stop"]),
    ?assertEqual([<<"--all">>], maps:get(<<"authorization">>, Stop)),
    ?assertNot(lists:member(pid, observer_cli_catalog:allowed_options(trace_stop_all))),
    ?assert(lists:member(all, observer_cli_catalog:allowed_options(trace))).

rate_description_preserves_breaker_semantics_test() ->
    {ok, Call} = observer_cli_catalog:describe(["trace", "call"]),
    Summary = maps:get(<<"summary">>, option(Call, <<"rate">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"burst-breaker">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"not a pacer">>)),
    ?assertNotEqual(nomatch, binary:match(Summary, <<"trip event">>)),
    ?assertEqual(nomatch, binary:match(Summary, <<"rate cap">>)).

identifier_policy_test() ->
    lists:foreach(
        fun(Id) ->
            Name = binary_to_list(observer_cli_catalog:public_name(Id)),
            {ok, D} = observer_cli_catalog:describe([Name]),
            Policy = maps:get(<<"identifiers">>, D),
            ?assertEqual(<<"redacted">>, maps:get(<<"default">>, Policy)),
            ?assertEqual(false, maps:get(<<"aliases_executable">>, Policy))
        end,
        [snapshot, diagnose]
    ),
    {ok, Process} = observer_cli_catalog:describe(["processes"]),
    ?assertEqual(<<"included">>, maps:get(<<"default">>, maps:get(<<"identifiers">>, Process))),
    {ok, Logs} = observer_cli_catalog:describe(["logs"]),
    ?assertEqual(false, maps:get(<<"redaction_supported">>, maps:get(<<"identifiers">>, Logs))),
    ?assertNot(lists:member(redact, observer_cli_catalog:allowed_options(logs))).

offline_and_format_contract_test() ->
    {ok, D} = observer_cli_catalog:describe(["describe"]),
    ?assertEqual([], maps:get(<<"prerequisites">>, D)),
    ?assertEqual([], maps:get(<<"side_effects">>, D)),
    ?assertEqual([format, json, verbose, schema], observer_cli_catalog:allowed_options(describe)),
    ?assertEqual(27, maps:get(<<"json_minimum_controller_otp">>, maps:get(<<"output">>, D))),
    ?assertEqual(
        <<"--json or --format json; no command arguments">>,
        maps:get(<<"requires">>, option(D, <<"schema">>))
    ),
    ?assertEqual({flag, verbose}, observer_cli_catalog:option("--verbose")),
    ?assertEqual({flag, schema}, observer_cli_catalog:option("--schema")),
    ?assertEqual({value, cookie_file}, observer_cli_catalog:option("--cookie-file")),
    ?assertEqual(unknown, observer_cli_catalog:option("--unknown")),
    ?assertEqual(unknown, observer_cli_catalog:option("--中文")),
    ?assertEqual(positional, observer_cli_catalog:option("processes")),
    ?assertEqual(<<"unknown">>, observer_cli_catalog:public_name(unknown)).

structured_dependencies_test() ->
    {ok, Memory} = observer_cli_catalog:describe(["memory"]),
    MC = maps:get(<<"constraints">>, Memory),
    lists:foreach(
        fun(Source) ->
            ?assert(
                lists:member(
                    #{
                        <<"kind">> => <<"requires_options">>,
                        <<"when_present">> => [Source],
                        <<"options">> => [<<"node">>]
                    },
                    MC
                )
            )
        end,
        [<<"cookie-env">>, <<"cookie-file">>, <<"name-mode">>]
    ),
    ?assert(
        lists:member(
            #{
                <<"kind">> => <<"exactly_one_option">>,
                <<"when_present">> => [<<"node">>],
                <<"options">> => [<<"cookie-env">>, <<"cookie-file">>]
            },
            MC
        )
    ),
    {ok, Diagnose} = observer_cli_catalog:describe(["diagnose"]),
    DC = maps:get(<<"constraints">>, Diagnose),
    lists:foreach(
        fun(Dependent) ->
            ?assert(
                lists:member(
                    #{
                        <<"kind">> => <<"requires_options">>,
                        <<"when_present">> => [Dependent],
                        <<"options">> => [<<"observe">>]
                    },
                    DC
                )
            )
        end,
        [<<"deep">>, <<"app">>]
    ),
    ?assert(
        lists:member(
            #{<<"kind">> => <<"mutually_exclusive">>, <<"options">> => [<<"deep">>, <<"app">>]}, DC
        )
    ),
    {ok, Otp} = observer_cli_catalog:describe(["otp-state"]),
    ?assert(
        lists:member(
            #{
                <<"kind">> => <<"option_value">>,
                <<"when_present">> => [<<"limit">>],
                <<"option">> => <<"behavior">>,
                <<"value">> => <<"gen_event">>
            },
            maps:get(<<"constraints">>, Otp)
        )
    ),
    lists:foreach(
        fun({Tokens, Required}) ->
            {ok, D} = observer_cli_catalog:describe(Tokens),
            ?assert(
                lists:member(
                    #{<<"kind">> => <<"required_options">>, <<"options">> => Required},
                    maps:get(<<"constraints">>, D)
                )
            )
        end,
        [
            {["connect"], [<<"node">>]},
            {["otp-state"], [<<"behavior">>]},
            {["supervision-tree"], [<<"app">>]},
            {["trace", "call"], [<<"pid">>, <<"replace-existing-trace">>]},
            {["trace", "stop"], [<<"all">>]}
        ]
    ).

structured_timeout_and_format_test() ->
    lists:foreach(
        fun({Tokens, When, Sampling, Margin, Default}) ->
            {ok, D} = observer_cli_catalog:describe(Tokens),
            [Constraint] = [
                C
             || C <- maps:get(<<"constraints">>, D),
                maps:get(<<"kind">>, C) =:= <<"timeout_margin">>
            ],
            ?assertEqual(When, maps:get(<<"when_present">>, Constraint)),
            ?assertEqual(Sampling, maps:get(<<"sampling_option">>, Constraint)),
            ?assertEqual(Margin, maps:get(<<"margin_ms">>, Constraint)),
            ?assertEqual(Default, maps:get(<<"default_sampling_ms">>, Constraint, none)),
            ?assertNot(
                lists:any(
                    fun(C) ->
                        maps:get(<<"kind">>, C) =:= <<"required_options">> andalso
                            lists:member(<<"timeout">>, maps:get(<<"options">>, C))
                    end,
                    maps:get(<<"constraints">>, D)
                )
            )
        end,
        [
            {["schedulers"], [<<"timeout">>], <<"duration">>, 5000, 1500},
            {["trace", "call"], [<<"timeout">>], <<"duration">>, 7000, 10000},
            {["processes"], [<<"timeout">>, <<"duration">>], <<"duration">>, 5000, none},
            {["network"], [<<"timeout">>, <<"duration">>], <<"duration">>, 5000, none},
            {["sockets"], [<<"timeout">>, <<"duration">>], <<"duration">>, 5000, none},
            {["diagnose"], [<<"timeout">>, <<"observe">>], <<"observe">>, 5000, none}
        ]
    ),
    lists:foreach(
        fun(D) ->
            ?assert(
                lists:member(
                    #{
                        <<"kind">> => <<"effective_format">>,
                        <<"when_present">> => [<<"verbose">>],
                        <<"format">> => <<"text">>
                    },
                    maps:get(<<"constraints">>, D)
                )
            )
        end,
        observer_cli_catalog:commands()
    ),
    {ok, Describe} = observer_cli_catalog:describe(["describe"]),
    Constraints = maps:get(<<"constraints">>, Describe),
    ?assert(
        lists:member(
            #{
                <<"kind">> => <<"effective_format">>,
                <<"when_present">> => [<<"schema">>],
                <<"format">> => <<"json">>
            },
            Constraints
        )
    ),
    ?assert(
        lists:member(
            #{
                <<"kind">> => <<"positional_count">>,
                <<"when_present">> => [<<"schema">>],
                <<"count">> => 0
            },
            Constraints
        )
    ),
    ?assertNot(
        lists:any(fun(C) -> maps:get(<<"kind">>, C) =:= <<"exactly_one_option">> end, Constraints)
    ).

risk_level_contract_test() ->
    lists:foreach(
        fun({Tokens, Risk}) ->
            {ok, D} = observer_cli_catalog:describe(Tokens),
            ?assertEqual(Risk, maps:get(<<"risk_level">>, D))
        end,
        [
            {["describe"], <<"local">>},
            {["disconnect"], <<"local">>},
            {["trace", "call"], <<"high">>},
            {["trace", "stop"], <<"high">>},
            {["otp-state"], <<"high">>},
            {["supervision-tree"], <<"high">>},
            {["logs"], <<"high">>},
            {["connect"], <<"bounded_observation">>},
            {["status"], <<"bounded_observation">>},
            {["snapshot"], <<"bounded_observation">>},
            {["diagnose"], <<"bounded_observation">>},
            {["processes"], <<"bounded_observation">>}
        ]
    ).

packaged_schema_test() ->
    {ok, Binary} = observer_cli_catalog:schema(),
    ?assertNotEqual(
        nomatch, binary:match(Binary, <<"https://json-schema.org/draft/2020-12/schema">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Binary, <<"observer_cli.cli/v1">>)).

option(Descriptor, Name) ->
    [Option] = [O || O <- maps:get(<<"options">>, Descriptor), maps:get(<<"name">>, O) =:= Name],
    Option.
