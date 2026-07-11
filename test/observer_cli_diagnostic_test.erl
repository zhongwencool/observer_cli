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
