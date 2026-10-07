-module(observer_cli_command_help_test).
-include_lib("eunit/include/eunit.hrl").

all_command_help_test_() ->
    [
        {binary_to_list(maps:get(<<"name">>, D)), fun() ->
            Help = observer_cli_catalog:help([binary_to_list(A) || A <- maps:get(<<"argv">>, D)]),
            assert_width(Help),
            ?assertEqual(nomatch, binary:match(Help, <<"Notes:">>)),
            assert_contains(Help, <<"Usage:\n">>),
            assert_contains(Help, <<"Examples:\n">>),
            [BeforeExamples, _] = binary:split(Help, <<"Examples:\n">>),
            lists:foreach(
                fun(O) ->
                    Name = maps:get(<<"name">>, O),
                    Pattern = <<"^  --", Name/binary, "(?: |$)">>,
                    {match, Matches} = re:run(BeforeExamples, Pattern, [multiline, global]),
                    ?assertEqual(1, length(Matches))
                end,
                maps:get(<<"options">>, D)
            ),
            case maps:get(<<"id">>, D) of
                <<"describe">> ->
                    ?assertEqual(nomatch, binary:match(Help, <<"Target options:">>)),
                    ?assertEqual(nomatch, binary:match(Help, <<"describe describe">>));
                _ ->
                    ?assert(
                        position(Help, <<"Command options:">>) <
                            position(Help, <<"Target options:">>)
                    ),
                    assert_contains(Help, <<"OBSERVER_CLI_NODE">>),
                    case maps:get(<<"id">>, D) of
                        <<"tui">> ->
                            ?assertEqual(nomatch, binary:match(Help, <<"Output options:">>));
                        _ ->
                            ?assert(
                                position(Help, <<"Target options:">>) <
                                    position(Help, <<"Output options:">>)
                            )
                    end
            end
        end}
     || D <- observer_cli_catalog:commands()
    ].

family_help_test() ->
    lists:foreach(
        fun(Family) ->
            Help = observer_cli_catalog:help([Family]),
            assert_width(Help),
            assert_contains(Help, <<"Usage:\n">>),
            assert_contains(Help, <<"Commands:\n">>),
            ?assertEqual(nomatch, binary:match(Help, <<"Choose a path">>))
        end,
        ["inspect", "trace"]
    ).

required_arguments_test() ->
    Call = observer_cli_catalog:help(["trace", "call"]),
    assert_contains(Call, <<"trace call MFA --pid PID --replace-existing-trace">>),
    assert_contains(Call, <<"Exact exported module:function/arity; no wildcards">>),
    assert_contains(Call, <<"previous state is not restored">>),
    assert_contains(Call, <<"--pid '<0.123.0>'">>),
    State = observer_cli_catalog:help(["inspect", "state"]),
    assert_contains(State, <<"(--pid PID | --name NAME)">>),
    assert_contains(State, <<"--behavior BEHAVIOR">>),
    assert_contains(State, <<"--allow-state-read">>),
    assert_contains(State, <<"full state">>),
    assert_contains(State, <<"only valid for gen_event">>),
    assert_contains(observer_cli_catalog:help(["trace", "stop"]), <<"trace stop --all [OPTIONS]">>),
    assert_contains(
        observer_cli_catalog:help(["inspect", "supervision"]), <<"supervision --app APP">>
    ),
    assert_contains(observer_cli_catalog:help(["describe"]), <<"describe [COMMAND_PATH]">>).

defaults_and_bounds_test() ->
    Check = observer_cli_catalog:help(["check"]),
    assert_contains(Check, <<"20s with the default window">>),
    assert_contains(Check, <<"Range: 5s..60s. Default: 15s.">>),
    {ok, CheckRequest} = observer_cli_input:parse(["check"]),
    ?assertEqual(
        {ok, 20000}, observer_cli_capture:timeout(maps:get(capture_options, CheckRequest))
    ),
    Call = observer_cli_catalog:help(["trace", "call"]),
    assert_contains(Call, <<"normally 17s">>),
    assert_contains(Call, <<"max(10s, --duration + 7s)">>),
    ?assertEqual({ok, 17000}, observer_cli_capture:timeout(#{replace_existing_trace => true})),
    ?assertEqual(
        {ok, 10000},
        observer_cli_capture:timeout(#{replace_existing_trace => true, duration => "1s"})
    ),
    Process = observer_cli_catalog:help(["inspect", "process"]),
    assert_contains(Process, <<"Default: 10s, or --window + 5s if longer">>),
    ?assertEqual({ok, 15000}, observer_cli_capture:timeout(#{duration => "10s"})),
    assert_contains(Process, <<"Range: 1..200. Default: 20.">>),
    assert_contains(Process, <<"Default: memory.">>),
    assert_contains(Call, <<"Range: 1..1000. Default: 100.">>),
    assert_contains(Call, <<"--rate N/s">>),
    assert_contains(observer_cli_catalog:help(["inspect", "state"]), <<"Range: 10s..120s.">>),
    assert_contains(observer_cli_catalog:help(["trace", "stop"]), <<"Range: 5s..120s.">>),
    Logs = observer_cli_catalog:help(["inspect", "logs"]),
    assert_contains(Logs, <<"Range: 1..2000. Default: 200.">>),
    assert_contains(Logs, <<"--redact is unsupported">>).

applicable_sort_help_test() ->
    lists:foreach(
        fun(Name) ->
            Help = observer_cli_catalog:help(["inspect", Name]),
            ?assertEqual(nomatch, binary:match(Help, <<"--window">>)),
            ?assertEqual(nomatch, binary:match(Help, <<"change/rate">>))
        end,
        ["application", "ets", "mnesia", "port"]
    ),
    lists:foreach(
        fun(Name) ->
            Help = observer_cli_catalog:help(["inspect", Name]),
            assert_contains(Help, <<"--sort METRIC">>),
            assert_contains(Help, <<"Choices:">>),
            assert_contains(Help, <<"change/rate metrics require --window">>)
        end,
        ["process", "network", "socket"]
    ).

example_quoting_test() ->
    D = observer_cli_catalog:descriptor(inspect_process),
    Help = observer_cli_command_help:render(D#{
        <<"examples">> := [
            [<<"inspect">>, <<"process">>, <<"--name">>, <<"worker's queue">>]
        ]
    }),
    assert_contains(Help, <<"--name 'worker'\\''s queue'">>),
    assert_contains(observer_cli_catalog:help(["inspect", "port"]), <<"--id '#Port<0.123>'">>).

assert_width(Help) ->
    ?assert(
        lists:all(fun(Line) -> byte_size(Line) =< 80 end, binary:split(Help, <<"\n">>, [global]))
    ).

assert_contains(Help, Text) -> ?assertNotEqual(nomatch, binary:match(Help, Text)).

position(Help, Text) ->
    {Start, _} = binary:match(Help, Text),
    Start.
