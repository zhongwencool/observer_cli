-module(observer_cli_agent_identity_test).
-include_lib("eunit/include/eunit.hrl").

identifiers_are_real_or_response_local_test() ->
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        Raw = #{
            first => {identifier, pid, self()},
            repeated => {identifier, pid, self()},
            second => {identifier, pid, Other}
        },
        {ok, Included} = observer_cli_snapshot:normalize(Raw, include),
        ?assertEqual(list_to_binary(pid_to_list(self())), maps:get(<<"first">>, Included)),
        {ok, Redacted} = observer_cli_snapshot:normalize(Raw, redact),
        Alias = maps:get(<<"first">>, Redacted),
        ?assertEqual(Alias, maps:get(<<"repeated">>, Redacted)),
        ?assertNotEqual(Alias, maps:get(<<"second">>, Redacted)),
        {ok, Later} = observer_cli_snapshot:normalize(#{only => {identifier, pid, Other}}, redact),
        ?assertEqual(Alias, maps:get(<<"only">>, Later)),
        ?assertNotEqual(list_to_binary(pid_to_list(self())), Alias)
    after
        Other ! stop
    end.

action_selectors_never_come_from_aliases_test() ->
    Finding = #{
        <<"id">> => <<"vm.process_limit_pressure">>,
        <<"entity">> => #{<<"type">> => <<"process">>, <<"id">> => <<"pid-1">>}
    },
    Actions = observer_cli_actions:from_findings([Finding]),
    lists:foreach(
        fun(Action) ->
            ?assertNot(lists:member(<<"pid-1">>, maps:get(<<"argv">>, Action))),
            ?assertEqual(<<"same_explicit_target">>, maps:get(<<"target_binding">>, Action))
        end,
        Actions
    ).
