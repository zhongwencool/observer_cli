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

quick_reductions_rate_uses_inventory_midpoints_test() ->
    Pid = spawn(fun wait/0),
    try
        FirstInventory = #{
            status => ok,
            values => #{Pid => facts(0, 0, 10)},
            audit => #{scan_started_monotonic_ms => 100, scan_finished_monotonic_ms => 300}
        },
        SecondInventory = #{
            status => ok,
            values => #{Pid => facts(0, 0, 31)},
            audit => #{scan_started_monotonic_ms => 2100, scan_finished_monotonic_ms => 2500}
        },
        Report = observer_cli_diagnostic:build_report(
            [
                sample(0, resources(10, 100), FirstInventory),
                sample(1, resources(10, 100), SecondInventory)
            ],
            [0, 1500],
            timing(),
            #{}
        ),
        Context = maps:get(hot_processes_by_reductions, maps:get(context, maps:get(data, Report))),
        [Item] = maps:get(items, Context),
        ?assertEqual(2100, maps:get(interval_ms, Context)),
        ?assertEqual(10.0, maps:get(reductions_per_second, Item))
    after
        exit(Pid, kill)
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
    [Evidence | _] = maps:get(evidence, Finding),
    ?assertEqual(0.85, maps:get(observed, Evidence)),
    ?assertEqual(<<">=">>, maps:get(operator, Evidence)),
    ?assertEqual(0.8, maps:get(threshold, Evidence)),
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
            },
            #{
                application => #{
                    status => ok,
                    identity_unavailable_count => 2,
                    children => [app_child(<<"atom:worker">>, FirstPid), unavailable_app_child()]
                }
            }
        ],
        Trend = observer_cli_diagnostic:application_trend(Samples),
        [Item] = maps:get(items, Trend),
        ?assertEqual(true, maps:get(pid_changed, Item)),
        ?assertEqual(6, maps:get(identity_unavailable_count, Trend))
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
    ?assertMatch(
        #{status := invalid, reason_code := sampling_gap},
        maps:get(trends, maps:get(context, maps:get(data, Gap)))
    ),
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

observation_trends_keep_hot_item_past_raw_id_limit_test() ->
    Pids = [spawn(fun wait/0) || _ <- lists:seq(1, 21)],
    Hot = lists:last(Pids),
    try
        Samples = [
            (observation_sample(Index, unavailable))#{
                process_inventory := #{
                    status => ok,
                    audit => #{},
                    values => maps:from_list([
                        {Pid,
                            facts(
                                case Pid =:= Hot of
                                    true -> Index * 100;
                                    false -> Index
                                end,
                                100,
                                Index
                            )}
                     || Pid <- Pids
                    ])
                }
            }
         || Index <- lists:seq(0, 4)
        ],
        Report = observer_cli_diagnostic:observation_report(
            observation,
            Samples,
            lists:seq(0, 4000, 1000),
            unavailable_holder(),
            timing(),
            #{}
        ),
        Trends = maps:get(trends, maps:get(context, maps:get(data, Report))),
        Processes = maps:get(processes, Trends),
        ?assertEqual(message_queue_len, maps:get(sort_metric, Processes)),
        ?assertEqual(20, length(maps:get(items, Processes))),
        [First | _] = maps:get(items, Processes),
        ?assertMatch(#{id := {identifier, pid, Hot}}, First)
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

invalid_scheduler_windows_make_report_partial_with_reason_test() ->
    Samples = [
        maps:without([scheduler_baseline, scheduler_end], observation_sample(I, unavailable))
     || I <- lists:seq(0, 4)
    ],
    Report = observer_cli_diagnostic:observation_report(
        observation,
        Samples,
        lists:seq(0, 4000, 1000),
        unavailable_holder(),
        timing(),
        #{}
    ),
    ?assertEqual(partial, maps:get(status, maps:get(capture, Report))),
    Scheduler = hd([
        Probe
     || #{id := scheduler_pressure} = Probe <- maps:get(probes, maps:get(capture, Report))
    ]),
    ?assertEqual(error, maps:get(status, Scheduler)),
    ?assertEqual(sampling_gap, maps:get(reason_code, Scheduler)).

quick_current_context_and_preenabled_scheduler_test() ->
    Table = make_ref(),
    Port = open_port({spawn, "cat"}, []),
    try
        First = (sample(0, resources(10, 100), unavailable_inventory()))#{
            ets_inventory => #{
                status => ok,
                values => #{Table => #{generation => make_ref(), size => 7, memory_words => 9}}
            },
            port_inventory => #{
                status => ok,
                values => #{Port => #{queue_size => 3, memory => 4, input => 5, output => 6}}
            },
            quick_scheduler_sample => observation_scheduler_sample(0)
        },
        Second = (sample(1, resources(10, 100), unavailable_inventory()))#{
            ets_inventory => #{status => unavailable, reason_code => observation_not_requested},
            port_inventory => #{status => unavailable, reason_code => observation_not_requested},
            quick_scheduler_sample => observation_scheduler_sample(1)
        },
        Report = observer_cli_diagnostic:build_report([First, Second], [0, 1500], timing(), #{}),
        Context = maps:get(context, maps:get(data, Report)),
        ?assertMatch(#{status := ok, items := [_]}, maps:get(ets, Context)),
        ?assertMatch(#{status := ok, items := [_]}, maps:get(ports, Context)),
        ?assertEqual(valid, maps:get(status, maps:get(scheduler, Context))),
        ?assertEqual([], [
            S
         || #{id := scheduler_pressure} = S <- maps:get(skipped, maps:get(data, Report))
        ])
    after
        port_close(Port)
    end.

quick_disabled_scheduler_is_skipped_test() ->
    Base = sample(0, resources(10, 100), unavailable_inventory()),
    Disabled = #{
        topology => #{}, wall_time => undefined, run_queue_lengths => [], monotonic_ms => 0
    },
    Report = observer_cli_diagnostic:build_report(
        [Base#{quick_scheduler_sample => Disabled}, Base#{quick_scheduler_sample => Disabled}],
        [0, 1500],
        timing(),
        #{}
    ),
    ?assertMatch(
        [#{reason_code := scheduler_wall_time_not_enabled}],
        [S || #{id := scheduler_pressure} = S <- maps:get(skipped, maps:get(data, Report))]
    ).

quick_current_context_refusal_and_error_test() ->
    Base = sample(0, resources(10, 100), unavailable_inventory()),
    Disabled = #{
        topology => #{}, wall_time => undefined, run_queue_lengths => [], monotonic_ms => 0
    },
    First = Base#{
        ets_inventory => #{status => unavailable, reason_code => scan_budget_exceeded},
        port_inventory => #{status => error, reason_code => port_inventory_failed},
        quick_scheduler_sample => Disabled
    },
    Second = Base#{
        ets_inventory => #{status => unavailable, reason_code => observation_not_requested},
        port_inventory => #{status => unavailable, reason_code => observation_not_requested},
        quick_scheduler_sample => Disabled
    },
    Report = observer_cli_diagnostic:build_report([First, Second], [0, 1500], timing(), #{}),
    Context = maps:get(context, maps:get(data, Report)),
    ?assertEqual(scan_budget_exceeded, maps:get(reason_code, maps:get(ets, Context))),
    ?assertEqual(port_inventory_failed, maps:get(reason_code, maps:get(ports, Context))),
    ?assertEqual(partial, maps:get(status, maps:get(capture, Report))).

field_gap_is_invalid_and_ets_generation_is_replaced_test() ->
    Key = named_table,
    FirstGeneration = make_ref(),
    SecondGeneration = make_ref(),
    Samples = [
        (observation_sample(I, unavailable))#{
            ets_inventory => #{
                status => ok,
                values => #{
                    Key => #{
                        generation =>
                            case I of
                                0 -> FirstGeneration;
                                _ -> SecondGeneration
                            end,
                        size => I,
                        memory_words => I
                    }
                }
            },
            socket_inventory =>
                case I of
                    2 -> #{status => unavailable, reason_code => scan_budget_exceeded};
                    _ -> #{status => ok, values => #{}}
                end
        }
     || I <- lists:seq(0, 4)
    ],
    Report = observer_cli_diagnostic:observation_report(
        observation, Samples, lists:seq(0, 4000, 1000), unavailable_holder(), timing(), #{}
    ),
    Trends = maps:get(trends, maps:get(context, maps:get(data, Report))),
    ?assertEqual(partial, maps:get(status, maps:get(capture, Report))),
    ?assertMatch(#{status := invalid, reason_code := sampling_gap}, maps:get(sockets, Trends)),
    Ets = maps:get(ets, Trends),
    ?assertEqual(1, maps:get(replaced_count, Ets)),
    ?assertEqual(0, maps:get(born_count, Ets)),
    ?assertEqual(0, maps:get(dead_count, Ets)).

port_trend_uses_inventory_midpoints_and_detects_intermediate_reset_test() ->
    Port = open_port({spawn, "cat"}, []),
    try
        Inputs = [10, 20, 5, 25, 30],
        Samples = [
            (observation_sample(Index, unavailable))#{
                port_inventory => #{
                    status => ok,
                    audit => port_audit(Index),
                    values => #{
                        Port => #{
                            queue_size => 10 - Index,
                            memory => 100 + Index,
                            input => lists:nth(Index + 1, Inputs),
                            output => Index + 1
                        }
                    }
                }
            }
         || Index <- lists:seq(0, 4)
        ],
        Report = observer_cli_diagnostic:observation_report(
            observation,
            Samples,
            lists:seq(0, 4000, 1000),
            unavailable_holder(),
            timing(),
            #{}
        ),
        Trends = maps:get(trends, maps:get(context, maps:get(data, Report))),
        Ports = maps:get(ports, Trends),
        [Item] = maps:get(items, Ports),
        ?assertEqual(4800, maps:get(interval_ms, Ports)),
        ?assertEqual(-4, maps:get(queue_size, maps:get(deltas, Item))),
        ?assertEqual(counter_reset, maps:get(<<"input_state">>, maps:get(deltas, Item))),
        ?assertEqual(4, maps:get(output, maps:get(deltas, Item))),
        ?assertEqual(4 * 1000 / 4800, maps:get(output, maps:get(rates_per_second, Item)))
    after
        port_close(Port)
    end.

port_audit(0) ->
    #{scan_started_monotonic_ms => 100, scan_finished_monotonic_ms => 300};
port_audit(4) ->
    #{scan_started_monotonic_ms => 4900, scan_finished_monotonic_ms => 5100};
port_audit(Index) ->
    Midpoint = 200 + Index * 1200,
    #{scan_started_monotonic_ms => Midpoint - 100, scan_finished_monotonic_ms => Midpoint + 100}.

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
        socket_inventory => #{status => unavailable, reason_code => scan_budget_exceeded},
        application => #{status => unavailable, reason_code => application_not_requested},
        scheduler_baseline => observation_scheduler_sample(Index),
        scheduler_end => observation_scheduler_sample(Index)
    }.

observation_scheduler_sample(Index) ->
    scheduler_sample(
        #{
            schedulers_configured => 1,
            schedulers_online => 1,
            dirty_cpu_schedulers_configured => 1,
            dirty_cpu_schedulers_online => 1
        },
        [{1, Index * 10, 100 + Index * 100}, {2, 0, 100 + Index * 100}],
        [0, 0],
        Index * 1000
    ).

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
