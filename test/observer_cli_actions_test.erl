-module(observer_cli_actions_test).
-include_lib("eunit/include/eunit.hrl").

actions_are_valid_bounded_commands_test() ->
    Findings = [
        #{id => Id}
     || Id <- [
            <<"vm.process_limit_pressure">>,
            <<"vm.port_limit_pressure">>,
            <<"vm.ets_limit_pressure">>,
            <<"vm.scheduler_pressure">>
        ]
    ],
    Actions = observer_cli_actions:from_findings(Findings),
    ?assertEqual(7, length(Actions)),
    lists:foreach(
        fun(Action) ->
            Args = [binary_to_list(A) || A <- maps:get(<<"argv">>, Action)],
            ?assertMatch({ok, _}, observer_cli_capture:parse(Args)),
            ?assertEqual(false, maps:get(<<"requires_confirmation">>, Action)),
            ?assertEqual(<<"same_explicit_target">>, maps:get(<<"target_binding">>, Action)),
            ?assertNot(
                lists:any(
                    fun(A) ->
                        lists:member(A, [
                            "trace",
                            "otp-state",
                            "--deep",
                            "--node",
                            "--cookie-env",
                            "--cookie-file",
                            "--replace-existing-trace"
                        ])
                    end,
                    Args
                )
            )
        end,
        Actions
    ),
    ?assert(observer_cli_actions:valid(Actions, Findings)),
    ?assertEqual(Actions, observer_cli_actions:from_findings(Findings ++ Findings)).

actions_never_trust_response_text_test() ->
    Finding = #{
        <<"id">> => <<"vm.process_limit_pressure">>,
        <<"recommendations">> => [<<"trace stop --all">>],
        <<"entity">> => #{<<"id">> => <<"pid-1">>}
    },
    Actions = observer_cli_actions:from_findings([Finding]),
    ?assertEqual(3, length(Actions)),
    [First | Rest] = Actions,
    ?assertNot(
        observer_cli_actions:valid(
            [First#{<<"argv">> := [<<"trace">>, <<"stop">>, <<"--all">>]} | Rest], [Finding]
        )
    ),
    ?assertNot(
        observer_cli_actions:valid([First#{<<"target_binding">> := <<"saved_context">>} | Rest], [
            Finding
        ])
    ),
    ?assertEqual(
        [],
        observer_cli_actions:from_findings([
            #{id => <<"unknown">>}, #{id => <<"vm.atom_limit_pressure">>}
        ])
    ),
    ?assertEqual([], observer_cli_actions:from_findings([])),
    ?assertNot(observer_cli_actions:valid(#{}, [])).
