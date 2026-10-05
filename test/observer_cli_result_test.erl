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

inventory_metric_units_test() ->
    R = (observer_cli_result:local(<<"inspect network">>, #{}))#{
        <<"data">> := #{
            <<"sort">> => <<"oct-rate">>,
            <<"sort_semantics">> => <<"rate">>,
            <<"items">> => [
                #{
                    <<"resource">> => <<"#Port<0.1>">>,
                    <<"oct">> => 9000,
                    <<"oct_per_second">> => 2048.0
                }
            ]
        }
    },
    Text = observer_cli_present:render(R, 80),
    ?assertNotEqual(nomatch, binary:match(Text, <<"bytes/s">>)),
    ?assertNotEqual(nomatch, binary:match(Text, <<"2.0 KiB">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"9000">>)).

redacted_actions_preserve_policy_test() ->
    R = check_result(complete, [], true),
    lists:foreach(
        fun(A) ->
            Argv = maps:get(<<"argv">>, A),
            ?assert(lists:member(<<"--redact">>, Argv)),
            ?assertMatch({ok, _}, observer_cli_input:parse([binary_to_list(V) || V <- Argv]))
        end,
        maps:get(<<"next_actions">>, R)
    ).

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

describe_text_retains_complete_contract_test() ->
    lists:foreach(
        fun({Path, Full}) ->
            {ok, Data} = observer_cli_catalog:describe(Path, Full),
            Response = observer_cli_result:local(<<"describe">>, Data),
            {ok, Expected} = observer_cli_capture:encode(verbose, Response),
            ?assertEqual(Expected, observer_cli_present:render(Response, 80)),
            ?assertNotEqual(nomatch, binary:match(Expected, <<"constraints">>)),
            ?assertNotEqual(nomatch, binary:match(Expected, <<"authorization">>)),
            ?assertNotEqual(nomatch, binary:match(Expected, <<"options">>))
        end,
        [
            {["inspect", "process"], false},
            {["trace", "call"], false},
            {["inspect"], false},
            {[], true}
        ]
    ),
    {ok, Index} = observer_cli_catalog:describe([], false),
    Text = observer_cli_present:render(observer_cli_result:local(<<"describe">>, Index), 80),
    ?assertEqual(nomatch, binary:match(Text, <<"constraints">>)),
    ?assert(length(binary:split(Text, <<"\n">>, [global])) =< 24).

nested_process_and_port_selectors_test() ->
    Pid = <<"<0.123.0>">>,
    Port = <<"#Port<0.42>">>,
    ProcessRows = #{<<"items">> => [#{<<"pid">> => Pid}]},
    PortRows = #{<<"items">> => [#{<<"resource">> => Port}]},
    CheckData = #{
        <<"context">> => #{
            <<"current">> => #{<<"processes">> => ProcessRows},
            <<"hot_processes_by_reductions">> => ProcessRows,
            <<"binary_holders">> => ProcessRows
        }
    },
    VmData = #{<<"processes">> => ProcessRows, <<"ports">> => PortRows},
    lists:foreach(
        fun(Redacted) ->
            RedactArgs =
                case Redacted of
                    true -> ["--redact"];
                    false -> []
                end,
            {ok, CheckRoute} = observer_cli_input:parse(["check" | RedactArgs]),
            Capture = (capture(complete, []))#{<<"data">> := CheckData},
            #{<<"data">> := #{<<"context">> := Context}} =
                observer_cli_result:from_capture(CheckRoute, Capture),
            #{
                <<"current">> := #{<<"processes">> := Current},
                <<"hot_processes_by_reductions">> := Hot
            } = Context,
            assert_row_selector(Current, <<"pid">>, Pid, Redacted),
            assert_row_selector(Hot, <<"pid">>, Pid, Redacted),
            assert_row_selector(maps:get(<<"binary_holders">>, Context), <<"pid">>, Pid, Redacted),
            {ok, VmRoute} = observer_cli_input:parse(["inspect", "vm" | RedactArgs]),
            VmCapture = Capture#{<<"command">> := <<"snapshot">>, <<"data">> := VmData},
            #{<<"data">> := #{<<"processes">> := Processes, <<"ports">> := Ports}} =
                observer_cli_result:from_capture(VmRoute, VmCapture),
            assert_row_selector(Processes, <<"pid">>, Pid, Redacted),
            assert_row_selector(Ports, <<"port">>, Port, Redacted)
        end,
        [false, true]
    ).

assert_row_selector(#{<<"items">> := [Item]}, Kind, Value, Redacted) ->
    Expected =
        case Redacted of
            true -> null;
            false -> #{<<"kind">> => Kind, <<"value">> => Value}
        end,
    ?assertEqual(Expected, maps:get(<<"selector">>, Item)).

context_actions_require_measured_evidence_test() ->
    Missing = #{
        <<"current">> => #{<<"memory">> => #{}},
        <<"distribution">> => #{<<"status">> => <<"unavailable">>},
        <<"trends">> => #{<<"sockets">> => #{<<"status">> => <<"unavailable">>, <<"items">> => []}}
    },
    lists:foreach(
        fun(Focus) ->
            Result = focus_context_result(Focus, Missing),
            ?assertEqual([], maps:get(<<"next_actions">>, Result)),
            ?assertEqual(3, observer_cli_result:exit_code(Result, #{}))
        end,
        ["memory", "connections"]
    ),
    Memory = focus_context_result("memory", #{
        <<"current">> => #{<<"memory">> => #{<<"total_bytes">> => 0}}
    }),
    ?assertMatch([#{<<"id">> := <<"observe_allocators">>}], maps:get(<<"next_actions">>, Memory)),
    Connections = focus_context_result("connections", #{
        <<"distribution">> => #{<<"connected_peer_count">> => 0},
        <<"trends">> => #{<<"sockets">> => #{<<"status">> => <<"ok">>, <<"items">> => []}}
    }),
    ?assertEqual(
        [<<"observe_peers">>, <<"observe_network">>],
        [maps:get(<<"id">>, A) || A <- maps:get(<<"next_actions">>, Connections)]
    ).

focus_context_result(Focus, Context) ->
    {ok, Route} = observer_cli_input:parse(["check", Focus]),
    Capture = (capture(partial, []))#{
        <<"data">> := #{<<"context">> => Context, <<"findings">> => []},
        <<"meta">> := #{<<"target">> => null, <<"capture">> => #{<<"probes">> => []}}
    },
    observer_cli_result:from_capture(Route, Capture).

port_detail_metric_units_test() ->
    Response = observer_cli_result:local(<<"inspect port">>, #{
        <<"resource">> => <<"#Port<0.42>">>,
        <<"memory">> => 2048,
        <<"queue_size">> => 0,
        <<"input">> => 1048576,
        <<"output">> => null,
        <<"os_pid">> => 1024,
        <<"links_total_count">> => 3
    }),
    Text = observer_cli_present:render(Response, 80),
    lists:foreach(
        fun(Expected) ->
            ?assertNotEqual(nomatch, binary:match(Text, Expected))
        end,
        [
            <<"memory: 2.0 KiB">>,
            <<"queue size: 0 B">>,
            <<"input: 1.0 MiB">>,
            <<"output: unavailable">>,
            <<"os pid: 1024">>,
            <<"links total count: 3">>
        ]
    ),
    ?assertEqual(nomatch, binary:match(Text, <<"os pid: 1.0 KiB">>)).
