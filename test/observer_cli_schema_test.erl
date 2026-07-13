-module(observer_cli_schema_test).

-include_lib("eunit/include/eunit.hrl").

schema_packaged_test() ->
    ?assertMatch({ok, _}, file:read_file(schema_path())).

schema_contract_test_() ->
    case code:ensure_loaded(json) of
        {module, json} ->
            fun schema_contract/0;
        {error, _Reason} ->
            fun() -> ok end
    end.

schema_contract() ->
    {ok, Binary} = file:read_file(schema_path()),
    Schema = json:decode(Binary),
    Properties = maps:get(<<"properties">>, Schema),
    Definitions = maps:get(<<"$defs">>, Schema),
    EnvelopeKeys = lists:sort([
        <<"schema">>, <<"command">>, <<"outcome">>, <<"data">>, <<"meta">>, <<"issues">>
    ]),
    ?assertEqual(false, maps:get(<<"additionalProperties">>, Schema)),
    ?assertEqual(EnvelopeKeys, lists:sort(maps:get(<<"required">>, Schema))),
    ?assertEqual(EnvelopeKeys, lists:sort(maps:keys(Properties))),
    ?assertEqual(
        [<<"complete">>, <<"partial">>, <<"error">>],
        maps:get(<<"enum">>, maps:get(<<"outcome">>, Properties))
    ),
    ?assertEqual(
        [<<"ok">>, <<"unavailable">>, <<"timeout">>, <<"error">>],
        maps:get(
            <<"enum">>,
            maps:get(<<"status">>, maps:get(<<"properties">>, maps:get(<<"probe">>, Definitions)))
        )
    ),
    ?assertEqual(
        [<<"warning">>, <<"error">>],
        maps:get(
            <<"enum">>,
            maps:get(<<"severity">>, maps:get(<<"properties">>, maps:get(<<"issue">>, Definitions)))
        )
    ),
    IssueClasses = maps:get(
        <<"enum">>,
        maps:get(<<"class">>, maps:get(<<"properties">>, maps:get(<<"issue">>, Definitions)))
    ),
    ?assertEqual(
        [
            <<"argument">>,
            <<"format">>,
            <<"capability">>,
            <<"safety_refusal">>,
            <<"controller">>,
            <<"distribution">>,
            <<"connection">>,
            <<"cleanup">>,
            <<"schema">>,
            <<"internal">>,
            <<"required_probe">>,
            <<"partial">>
        ],
        IssueClasses
    ),
    ?assert(
        lists:all(
            fun(Class) -> observer_cli_escriptize:valid_issue_class(Class, <<"reason">>) end,
            IssueClasses
        )
    ),
    ?assertNot(observer_cli_escriptize:valid_issue_class(<<"unknown">>, <<"reason">>)),
    lists:foreach(
        fun(Name) ->
            ?assertEqual(false, maps:get(<<"additionalProperties">>, maps:get(Name, Definitions)))
        end,
        [
            <<"meta">>,
            <<"target">>,
            <<"capture">>,
            <<"probe">>,
            <<"issue">>,
            <<"logsData">>,
            <<"logSource">>,
            <<"selectedLogSource">>,
            <<"logTail">>,
            <<"logsCapture">>,
            <<"diagnosticsWorkerEffect">>,
            <<"moduleLoadEffect">>,
            <<"distributionControllerEffect">>,
            <<"configuredLogReadEffect">>
        ]
    ),
    lists:foreach(
        fun(Name) ->
            lists:foreach(
                fun(Branch) ->
                    Data = maps:get(<<"data">>, maps:get(<<"properties">>, Branch)),
                    ?assertMatch(
                        [#{<<"$ref">> := _}, #{<<"type">> := <<"null">>}],
                        maps:get(<<"oneOf">>, Data)
                    )
                end,
                maps:get(<<"oneOf">>, maps:get(Name, Definitions))
            )
        end,
        [
            <<"contextCommand">>,
            <<"snapshotDiagnosticCommand">>,
            <<"vmHealthCommand">>,
            <<"resourceListCommand">>,
            <<"resourceDetailCommand">>,
            <<"traceCommand">>,
            <<"logsCommand">>
        ]
    ),
    PreCommand = maps:get(<<"preCommandError">>, Definitions),
    PreProperties = maps:get(<<"properties">>, PreCommand),
    ?assertEqual(<<"null">>, maps:get(<<"type">>, maps:get(<<"command">>, PreProperties))),
    ?assert(maps:is_key(<<"contains">>, maps:get(<<"issues">>, PreProperties))),
    ?assertEqual(schema_commands(Definitions), cli_response_commands()).

schema_path() ->
    filename:join([code:priv_dir(observer_cli), "schema", "observer_cli.cli.v1.schema.json"]).

schema_commands(Definitions) ->
    lists:sort(
        lists:append([
            family_commands(Name, Definitions)
         || Name <- [
                <<"contextCommand">>,
                <<"snapshotDiagnosticCommand">>,
                <<"vmHealthCommand">>,
                <<"resourceListCommand">>,
                <<"resourceDetailCommand">>,
                <<"traceCommand">>,
                <<"logsCommand">>
            ]
        ])
    ).

family_commands(Name, Definitions) ->
    [
        maps:get(<<"const">>, maps:get(<<"command">>, maps:get(<<"properties">>, Branch)))
     || Branch <- maps:get(<<"oneOf">>, maps:get(Name, Definitions))
    ].

cli_response_commands() ->
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
        "applications",
        "ets",
        "mnesia",
        "network",
        "ports",
        "sockets",
        "process",
        "port",
        "otp-state",
        "supervision-tree",
        "logs"
    ],
    lists:sort(
        [
            atom_to_binary(observer_cli_cli:command(Command))
         || Command <- Commands
        ] ++ [<<"trace_call">>, <<"trace_stop_all">>]
    ).
