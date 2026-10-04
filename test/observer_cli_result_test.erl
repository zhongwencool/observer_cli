-module(observer_cli_result_test).
-include_lib("eunit/include/eunit.hrl").

finding_exit_policy_test() ->
    R = check_result(complete, [finding(<<"warning">>)], false),
    ?assertEqual(0, observer_cli_result:exit_code(R, #{})),
    ?assertEqual(1, observer_cli_result:exit_code(R, #{fail_on => "warning"})),
    ?assertEqual(0, observer_cli_result:exit_code(R, #{fail_on => "critical"})),
    Critical = check_result(complete, [finding(<<"critical">>)], false),
    ?assertEqual(1, observer_cli_result:exit_code(Critical, #{fail_on => "critical"})),
    Partial = check_result(partial, [finding(<<"critical">>)], false),
    ?assertEqual(3, observer_cli_result:exit_code(Partial, #{fail_on => "warning"})),
    ?assertMatch(#{<<"assessment">> := #{<<"status">> := <<"findings">>}}, Partial).

public_shape_and_no_findings_test() ->
    R = check_result(complete, [], false),
    ?assertEqual(9, map_size(R)),
    ?assertEqual(<<"observer_cli.cli/v2">>, maps:get(<<"schema">>, R)),
    ?assertEqual(<<"check">>, maps:get(<<"command">>, R)),
    ?assertMatch(#{<<"status">> := <<"no_findings">>}, maps:get(<<"assessment">>, R)),
    ?assertNot(maps:is_key(<<"findings">>, maps:get(<<"data">>, R))),
    ?assertNotEqual([], maps:get(<<"next_actions">>, R)),
    lists:foreach(
        fun(A) ->
            Args = [binary_to_list(V) || V <- maps:get(<<"argv">>, A)],
            ?assertMatch({ok, _}, observer_cli_input:parse(Args)),
            ?assertEqual(<<"same_explicit_target">>, maps:get(<<"target_binding">>, A))
        end,
        maps:get(<<"next_actions">>, R)
    ).

uncalibrated_focus_test() ->
    {ok, Route} = observer_cli_input:parse(["check", "memory", "--window", "5s"]),
    R = observer_cli_result:from_capture(Route, capture(complete, [])),
    ?assertMatch(
        #{<<"status">> := <<"not_evaluated">>, <<"findings">> := []}, maps:get(<<"assessment">>, R)
    ),
    ?assertEqual(0, observer_cli_result:exit_code(R, #{})).

typed_selector_redaction_test() ->
    {ok, Route} = observer_cli_input:parse(["inspect", "process"]),
    Captured = observer_cli_capture:response(
        processes,
        complete,
        null,
        null,
        #{
            <<"sort">> => <<"memory">>,
            <<"sort_semantics">> => <<"total">>,
            <<"items">> => [#{<<"pid">> => <<"<0.123.0>">>, <<"memory_bytes">> => 1}]
        },
        []
    ),
    R = observer_cli_result:from_capture(Route, Captured),
    [Item] = maps:get(<<"items">>, maps:get(<<"data">>, R)),
    ?assertEqual(
        #{<<"kind">> => <<"pid">>, <<"value">> => <<"<0.123.0>">>}, maps:get(<<"selector">>, Item)
    ),
    {ok, RedactedRoute} = observer_cli_input:parse(["inspect", "process", "--redact"]),
    Redacted = observer_cli_result:from_capture(RedactedRoute, Captured),
    [RedactedItem] = maps:get(<<"items">>, maps:get(<<"data">>, Redacted)),
    ?assertEqual(null, maps:get(<<"selector">>, RedactedItem)).

default_report_budget_and_evidence_test() ->
    R = check_result(complete, [], false),
    Text = observer_cli_present:render(R, 80),
    Lines = binary:split(Text, <<"\n">>, [global]),
    ?assert(length(Lines) - 1 =< 24),
    ?assert(lists:all(fun(L) -> length(unicode:characters_to_list(L)) =< 80 end, Lines)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"BEAM memory:">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"MiB">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"Next: observer_cli inspect process">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"planned_sample_count">>)).

check_result(Outcome, Findings, Redacted) ->
    Args =
        ["check"] ++
            case Redacted of
                true -> ["--redact"];
                false -> []
            end,
    {ok, Route} = observer_cli_input:parse(Args),
    observer_cli_result:from_capture(Route, capture(Outcome, Findings)).

capture(Outcome, Findings) ->
    Probes = [
        #{
            <<"id">> => Id,
            <<"required">> => Required,
            <<"status">> => <<"ok">>,
            <<"reason_code">> => null,
            <<"duration_ms">> => 15000,
            <<"samples">> => 5,
            <<"coverage">> => []
        }
     || {Id, Required} <- [
            {<<"core_limits_and_memory">>, true},
            {<<"scheduler_pressure">>, false},
            {<<"process_inventory">>, false}
        ]
    ],
    observer_cli_capture:response(
        diagnose,
        Outcome,
        #{<<"node">> => <<"app@host">>, <<"otp_release">> => <<"29">>},
        #{<<"duration_ms">> => 15002, <<"probes">> => Probes, <<"observer_effects">> => []},
        #{
            <<"findings">> => Findings,
            <<"summary">> => <<"synthetic fixture">>,
            <<"context">> => #{
                <<"current">> => #{
                    <<"memory">> => #{
                        <<"total_bytes">> => 1048576, <<"binary_bytes">> => 0, <<"ets_bytes">> => 0
                    },
                    <<"processes">> => #{
                        <<"items">> => [
                            #{
                                <<"pid">> => <<"<0.123.0>">>,
                                <<"memory_bytes">> => 10,
                                <<"message_queue_len">> => 0,
                                <<"reductions">> => 20
                            }
                        ]
                    }
                },
                <<"trends">> => #{
                    <<"global_memory">> => #{<<"deltas">> => #{<<"total_bytes">> => -10}}
                }
            }
        },
        []
    ).
finding(Severity) ->
    #{
        <<"id">> => <<"vm.process_limit_pressure">>,
        <<"severity">> => Severity,
        <<"summary">> => <<"Synthetic limit finding">>,
        <<"evidence">> => []
    }.
