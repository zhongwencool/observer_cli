-module(observer_cli_diagnostic_test).

-include_lib("eunit/include/eunit.hrl").

strict_limit_edges_and_highest_evidence_test() ->
    Samples = [
        runtime_sample(0, 85, 100, 94, 100, 10, 100, 10, 100),
        runtime_sample(1, 86, 100, 95, 100, 10, 100, 10, 100)
    ],
    Findings = observer_cli_diagnostic:limit_findings(Samples),
    ?assertEqual(
        [<<"vm.process_limit_pressure">>, <<"vm.port_limit_pressure">>],
        [maps:get(id, Finding) || Finding <- Findings]
    ),
    [Process, Port] = Findings,
    ?assertEqual(warning, maps:get(severity, Process)),
    ?assertEqual(critical, maps:get(severity, Port)),
    [ProcessEvidence] = maps:get(evidence, Process),
    ?assertEqual(1, maps:get(sample_index, ProcessEvidence)),
    ?assertEqual(
        <<"/data/context/snapshot/runtime_samples/1/process_usage_ratio">>,
        maps:get(path, ProcessEvidence)
    ).

stable_reductions_denominator_lifecycle_and_signed_gauges_test() ->
    Stable = spawn(fun wait/0),
    Reset = spawn(fun wait/0),
    NonPositive = spawn(fun wait/0),
    Dead = spawn(fun wait/0),
    Born = spawn(fun wait/0),
    try
        Context = observer_cli_diagnostic:reductions_context(
            #{
                values => #{
                    Stable => facts(10, 100, 10),
                    Reset => facts(10, 100, 50),
                    NonPositive => facts(10, 100, 20),
                    Dead => facts(1, 1, 1)
                }
            },
            #{
                values => #{
                    Stable => facts(4, 80, 30),
                    Reset => facts(12, 120, 2),
                    NonPositive => facts(9, 90, 20),
                    Born => facts(999, 999, 999)
                }
            }
        ),
        ?assertEqual(20, maps:get(stable_positive_reductions_denominator, Context)),
        ?assertEqual(1, maps:get(born_count, Context)),
        ?assertEqual(1, maps:get(dead_count, Context)),
        ?assertEqual(1, maps:get(reset_count, Context)),
        StableItem = item_for(Stable, maps:get(items, Context)),
        ?assertEqual(-6, maps:get(message_queue_len_delta, StableItem)),
        ?assertEqual(-20, maps:get(memory_bytes_delta, StableItem)),
        ?assertEqual(1.0, maps:get(share_of_stable_scanned_reductions, StableItem)),
        ResetItem = item_for(Reset, maps:get(items, Context)),
        ?assertEqual(counter_reset, maps:get(reductions_state, ResetItem)),
        ?assertEqual(null, maps:get(share_of_stable_scanned_reductions, ResetItem))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, [Stable, Reset, NonPositive, Dead, Born])
    end.

zero_reductions_denominator_is_null_test() ->
    Pid = spawn(fun wait/0),
    try
        Context = observer_cli_diagnostic:reductions_context(
            #{values => #{Pid => facts(1, 2, 10)}},
            #{values => #{Pid => facts(0, 1, 10)}}
        ),
        ?assertEqual(0, maps:get(stable_positive_reductions_denominator, Context)),
        [Item] = maps:get(items, Context),
        ?assertEqual(null, maps:get(share_of_stable_scanned_reductions, Item))
    after
        exit(Pid, kill)
    end.

required_gap_suppresses_findings_and_optional_refusal_stays_complete_test() ->
    Timing = timing(),
    High = sample(0, resources(96, 100), unavailable_inventory()),
    Partial = observer_cli_diagnostic:build_report(
        [High, High#{status := error}], [0, 1500], Timing, #{}
    ),
    ?assertEqual(partial, maps:get(status, maps:get(capture, Partial))),
    ?assertEqual([], maps:get(findings, maps:get(data, Partial))),
    Complete = observer_cli_diagnostic:build_report(
        [High, sample(1, resources(10, 100), unavailable_inventory())],
        [0, 1500],
        Timing,
        #{}
    ),
    ?assertEqual(complete, maps:get(status, maps:get(capture, Complete))),
    ?assertEqual(1, length(maps:get(findings, maps:get(data, Complete)))),
    ?assert(
        lists:any(
            fun
                (#{id := Id, reason_code := Reason}) ->
                    Id =:= hot_processes_by_reductions andalso Reason =:= scan_budget_exceeded;
                (_) ->
                    false
            end,
            maps:get(skipped, maps:get(data, Complete))
        )
    ).

dispatch_validates_evidence_and_redacts_context_test() ->
    Pid = spawn(fun wait/0),
    try
        Inventory = #{status => ok, values => #{Pid => facts(1, 10, 1)}, audit => #{}},
        Base = erlang:monotonic_time(millisecond),
        First = (sample(0, resources(96, 100), Inventory))#{
            monotonic_start_ms := Base,
            monotonic_finish_ms := Base,
            monotonic_midpoint_ms := Base
        },
        Second = First#{resources := resources(10, 100)},
        Request = #{
            interval_ms => 0,
            test_samples => [First, Second]
        },
        #{<<"status">> := <<"ok">>, <<"result">> := Response} =
            observer_cli_snapshot:dispatch(
                self(), diagnose, Request, #{timeout_ms => 3000, identifier_policy => redact}
            ),
        ?assertMatch(
            #{
                <<"capture">> := #{<<"status">> := <<"complete">>},
                <<"data">> := #{
                    <<"ruleset">> := <<"observer_cli.quick">>,
                    <<"ruleset_version">> := 1,
                    <<"findings">> := [_]
                }
            },
            Response
        ),
        ?assertEqual(
            nomatch, binary:match(term_to_binary(Response), list_to_binary(pid_to_list(Pid)))
        )
    after
        exit(Pid, kill)
    end.

two_complete_low_cost_scheduler_windows_are_required_test() ->
    High = scheduler_window(0.85, 1),
    ?assertEqual([], observer_cli_diagnostic:scheduler_findings([High])),
    [Finding] = observer_cli_diagnostic:scheduler_findings([High, High]),
    ?assertEqual(<<"vm.scheduler_pressure">>, maps:get(id, Finding)),
    ?assertEqual(
        [],
        observer_cli_diagnostic:scheduler_findings([
            High, High#{heavy_probe_overlap := true}
        ])
    ),
    ?assertEqual(
        [],
        observer_cli_diagnostic:scheduler_findings([
            High, High#{status := invalid, reason_code => topology_changed}
        ])
    ).

scheduler_counter_reset_and_offline_topology_are_invalid_test() ->
    Topology = #{
        schedulers_configured => 4,
        schedulers_online => 2,
        dirty_cpu_schedulers_configured => 3,
        dirty_cpu_schedulers_online => 1
    },
    First = scheduler_sample(
        Topology,
        [{1, 0, 10}, {2, 0, 10}, {3, 999, 1000}, {5, 0, 10}],
        [0, 0, 0],
        0
    ),
    Second = scheduler_sample(
        Topology,
        [{1, 9, 20}, {2, 9, 20}, {3, 999, 2000}, {5, 9, 20}],
        [1, 0, 1],
        100
    ),
    Window = observer_cli_snapshot:scheduler_window(First, Second),
    ?assertEqual(valid, maps:get(status, Window)),
    ?assertEqual(0.9, maps:get(utilization_ratio, maps:get(normal, Window))),
    Reset = observer_cli_snapshot:scheduler_window(Second, First),
    ?assertEqual(invalid, maps:get(status, Reset)),
    Changed = observer_cli_snapshot:scheduler_window(
        First, Second#{topology := Topology#{schedulers_online := 1}}
    ),
    ?assertEqual(topology_changed, maps:get(reason_code, Changed)).

application_trend_correlates_only_available_direct_child_ids_test() ->
    FirstPid = spawn(fun wait/0),
    SecondPid = spawn(fun wait/0),
    try
        Samples = [
            #{
                application => #{
                    status => ok,
                    identity_unavailable_count => 2,
                    children => [app_child(<<"atom:worker">>, FirstPid), unavailable_app_child()]
                }
            },
            #{
                application => #{
                    status => ok,
                    identity_unavailable_count => 2,
                    children => [app_child(<<"atom:worker">>, SecondPid), unavailable_app_child()]
                }
            }
        ],
        Trend = observer_cli_diagnostic:application_trend(Samples),
        [Item] = maps:get(items, Trend),
        ?assertEqual(true, maps:get(pid_changed, Item)),
        ?assertEqual(4, maps:get(identity_unavailable_count, Trend))
    after
        exit(FirstPid, kill),
        exit(SecondPid, kill)
    end.

observation_required_sets_optional_outcomes_and_exit_precedence_test() ->
    Plan5 = lists:seq(0, 4000, 1000),
    CompleteSamples = [observation_sample(Index, unavailable) || Index <- lists:seq(0, 4)],
    Complete = observer_cli_diagnostic:observation_report(
        observation, CompleteSamples, Plan5, unavailable_holder(), timing(), #{}
    ),
    ?assertEqual(complete, maps:get(status, maps:get(capture, Complete))),
    ?assertEqual(1, length(maps:get(findings, maps:get(data, Complete)))),
    StartedFailureSamples = [observation_sample(0, error) | tl(CompleteSamples)],
    StartedFailure = observer_cli_diagnostic:observation_report(
        observation, StartedFailureSamples, Plan5, unavailable_holder(), timing(), #{}
    ),
    ?assertEqual(partial, maps:get(status, maps:get(capture, StartedFailure))),
    GapSamples = [
        observation_sample(0, unavailable),
        #{
            status => error,
            reason_code => sampling_gap
        }
        | lists:nthtail(2, CompleteSamples)
    ],
    Gap = observer_cli_diagnostic:observation_report(
        observation, GapSamples, Plan5, unavailable_holder(), timing(), #{}
    ),
    ?assertEqual(partial, maps:get(status, maps:get(capture, Gap))),
    ?assertEqual([], maps:get(findings, maps:get(data, Gap))),
    Plan7 = lists:seq(0, 6000, 1000),
    Deep = observer_cli_diagnostic:observation_report(
        deep,
        [observation_sample(I, unavailable) || I <- lists:seq(0, 6)],
        Plan7,
        unavailable_holder(),
        timing(),
        #{}
    ),
    ?assertEqual(complete, maps:get(status, maps:get(capture, Deep))),
    App = observer_cli_diagnostic:observation_report(
        application,
        [
            (observation_sample(I, unavailable))#{application := #{status => not_running}}
         || I <- lists:seq(0, 4)
        ],
        Plan5,
        unavailable_holder(),
        timing(),
        #{}
    ),
    ?assertEqual(complete, maps:get(status, maps:get(capture, App))).

observation_sample(Index, InventoryStatus) ->
    Inventory =
        case InventoryStatus of
            unavailable -> unavailable_inventory();
            error -> #{status => error, reason_code => process_inventory_failed}
        end,
    (sample(Index, resources(96, 100), Inventory))#{
        memory => #{
            status => ok,
            values => #{
                total_bytes => 100 + Index,
                binary_bytes => 10 + Index
            }
        },
        ets_inventory => #{status => unavailable, reason_code => scan_budget_exceeded},
        port_inventory => #{status => unavailable, reason_code => scan_budget_exceeded},
        application => #{status => unavailable, reason_code => application_not_requested}
    }.

unavailable_holder() ->
    #{status => unavailable, reason_code => scan_budget_exceeded}.

scheduler_window(Ratio, Runnable) ->
    Pool = #{
        status => available,
        utilization_ratio => Ratio,
        active_delta => #{value => round(Ratio * 100), unit => opaque_same_window},
        total_delta => #{value => 100, unit => opaque_same_window}
    },
    DirtyPool = Pool#{
        utilization_ratio := 0.0,
        active_delta := #{value => 0, unit => opaque_same_window}
    },
    Queue = #{end_observed_runnable_count_including_observer => Runnable},
    #{
        status => valid,
        heavy_probe_overlap => false,
        normal => Pool,
        dirty_cpu => DirtyPool,
        run_queues => #{normal => Queue, dirty_cpu => Queue}
    }.

scheduler_sample(Topology, Wall, Queues, Monotonic) ->
    #{
        topology => Topology,
        wall_time => Wall,
        run_queue_lengths => Queues,
        monotonic_ms => Monotonic
    }.

app_child(Id, Pid) ->
    #{
        identity => available,
        id => {identifier, child, Id},
        child => #{pid => {identifier, pid, Pid}}
    }.

unavailable_app_child() ->
    #{identity => unavailable, id => null, child => #{pid => null}}.

runtime_sample(Index, Process, ProcessLimit, Port, PortLimit, Atom, AtomLimit, Ets, EtsLimit) ->
    #{
        sample_index => Index,
        monotonic_midpoint_ms => Index * 1500,
        process_observed_count_including_observer => Process,
        process_limit => ProcessLimit,
        process_usage_ratio => Process / ProcessLimit,
        port_observed_count_including_observer => Port,
        port_limit => PortLimit,
        port_usage_ratio => Port / PortLimit,
        atom_observed_count_including_observer => Atom,
        atom_limit => AtomLimit,
        atom_usage_ratio => Atom / AtomLimit,
        ets_observed_count_including_observer => Ets,
        ets_limit => EtsLimit,
        ets_usage_ratio => Ets / EtsLimit
    }.

resources(Process, Limit) ->
    #{
        process => contaminated(Process, Limit),
        port => contaminated(0, 100),
        atom => contaminated(0, 100),
        ets => #{observed_count => 0, limit => 100}
    }.

contaminated(Count, Limit) ->
    #{observed_count_including_observer => Count, limit => Limit, observer_contaminated => true}.

sample(Index, Resources, Inventory) ->
    #{
        status => ok,
        monotonic_start_ms => Index * 1500,
        monotonic_finish_ms => Index * 1500,
        monotonic_midpoint_ms => Index * 1500,
        resources => Resources,
        process_inventory => Inventory
    }.

unavailable_inventory() ->
    #{status => unavailable, reason_code => scan_budget_exceeded}.

facts(Queue, Memory, Reductions) ->
    #{message_queue_len => Queue, memory_bytes => Memory, reductions => Reductions}.

timing() ->
    #{
        started_at => <<"2026-07-11T00:00:00.000Z">>,
        finished_at => <<"2026-07-11T00:00:01.500Z">>,
        duration_ms => 1500,
        controller => self(),
        module_loaded_before_sample => true
    }.

item_for(Pid, Items) ->
    hd([Item || #{pid := {identifier, pid, ItemPid}} = Item <- Items, ItemPid =:= Pid]).

wait() ->
    receive
        stop -> ok
    end.
