-module(observer_cli_diagnostic).

-export([capture/2]).

-ifdef(TEST).
-export([build_report/4, limit_findings/1, reductions_context/2]).
-endif.

-define(DEFAULT_INTERVAL_MS, 1500).
-define(RULESET, <<"observer_cli.quick">>).
-define(RULESET_VERSION, 1).
-define(CONTEXT_LIMIT, 20).

-spec capture(map(), map()) -> map() | {probe_error, atom()}.
capture(Request, #{controller := Controller} = Context) when is_map(Request) ->
    StartedAt = erlang:system_time(millisecond),
    Started = erlang:monotonic_time(millisecond),
    ModuleLoaded = code:is_loaded(?MODULE) =/= false,
    Interval = maps:get(interval_ms, Request, ?DEFAULT_INTERVAL_MS),
    Plan = [Started, Started + Interval],
    Samples = capture_samples(Request, Context, Plan),
    Finished = erlang:monotonic_time(millisecond),
    build_report(
        Samples,
        Plan,
        #{
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(erlang:system_time(millisecond)),
            duration_ms => Finished - Started,
            controller => Controller,
            module_loaded_before_sample => ModuleLoaded
        },
        distribution(Controller)
    );
capture(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_samples(Request, Context, [First, Second]) ->
    FirstSample = capture_sample(Request, Context, 0, First),
    case maps:get(monotonic_finish_ms, FirstSample, First) > Second of
        true -> [FirstSample, #{status => error, reason_code => sampling_gap}];
        false -> [FirstSample, capture_sample(Request, Context, 1, Second)]
    end.

capture_sample(Request, Context, Index, Target) ->
    sleep_until(Target),
    try sample(Request, Context, Index) of
        Sample -> Sample
    catch
        _Class:_Reason:_Stacktrace -> #{status => error, reason_code => required_probe_failed}
    end.

distribution(Controller) ->
    try observer_cli_snapshot:diagnostic_distribution(Controller) of
        Context -> Context
    catch
        _Class:_Reason:_Stacktrace -> #{status => error, reason_code => distribution_probe_failed}
    end.

-ifdef(TEST).
sample(#{test_samples := Samples}, _Context, Index) ->
    lists:nth(Index + 1, Samples);
sample(Request, Context, _Index) ->
    observer_cli_snapshot:diagnostic_sample(Request, Context).
-else.
sample(Request, Context, _Index) ->
    observer_cli_snapshot:diagnostic_sample(Request, Context).
-endif.

sleep_until(Target) ->
    case Target - erlang:monotonic_time(millisecond) of
        Remaining when Remaining > 0 -> timer:sleep(Remaining);
        _ -> ok
    end.

-ifdef(TEST).
-spec build_report([map()], [integer()], map(), map()) -> map().
-endif.
build_report(Samples, Plan, Timing, Distribution) ->
    RequiredComplete = required_complete(Samples),
    ProcessStatus = optional_status(Samples),
    DistributionStatus = distribution_status(Distribution),
    Status = capture_status(RequiredComplete, [ProcessStatus, DistributionStatus]),
    RuntimeSamples = runtime_samples(Samples),
    Findings =
        case RequiredComplete of
            true -> limit_findings(RuntimeSamples);
            false -> []
        end,
    ProcessContext = process_context(Samples),
    Skipped = skipped_checks(Samples),
    #{
        schema => <<"observer_cli.cli/v1">>,
        command => diagnose,
        target => #{
            node => {identifier, node, node()},
            otp_release => unicode:characters_to_binary(erlang:system_info(otp_release))
        },
        capture => #{
            status => Status,
            started_at => maps:get(started_at, Timing),
            finished_at => maps:get(finished_at, Timing),
            duration_ms => maps:get(duration_ms, Timing),
            probes => probe_reports(Samples, RequiredComplete, ProcessStatus, DistributionStatus),
            observer_effects => observer_effects(Timing)
        },
        data => #{
            ruleset => ?RULESET,
            ruleset_version => ?RULESET_VERSION,
            sampling_plan => sampling_plan(Plan, Samples),
            findings => Findings,
            suspects => [],
            context => #{
                snapshot => #{runtime_samples => RuntimeSamples},
                hot_processes_by_reductions => ProcessContext,
                distribution => Distribution
            },
            skipped => Skipped,
            summary => summary(Status, Findings)
        },
        warnings => [],
        errors => capture_errors(RequiredComplete, ProcessStatus, DistributionStatus)
    }.

required_complete(Samples) ->
    length(Samples) =:= 2 andalso lists:all(fun valid_required_sample/1, Samples).

valid_required_sample(#{status := ok, resources := Resources}) ->
    lists:all(
        fun(Domain) -> valid_limit(maps:get(Domain, Resources, invalid)) end,
        [process, port, atom, ets]
    );
valid_required_sample(_Sample) ->
    false.

valid_limit(#{observed_count_including_observer := Count, limit := Limit}) ->
    is_integer(Count) andalso Count >= 0 andalso is_integer(Limit) andalso Limit > 0;
valid_limit(#{observed_count := Count, limit := Limit}) ->
    is_integer(Count) andalso Count >= 0 andalso is_integer(Limit) andalso Limit > 0;
valid_limit(_Value) ->
    false.

runtime_samples(Samples) ->
    [
        runtime_sample(Index, Sample)
     || {Index, Sample} <- indexed(Samples), valid_required_sample(Sample)
    ].

runtime_sample(Index, #{resources := Resources} = Sample) ->
    lists:foldl(
        fun(Domain, Acc) ->
            Limit = maps:get(Domain, Resources),
            Count = observed_count(Limit),
            Acc#{
                count_key(Domain) => Count,
                limit_key(Domain) => maps:get(limit, Limit),
                ratio_key(Domain) => Count / maps:get(limit, Limit)
            }
        end,
        #{
            sample_index => Index,
            monotonic_start_ms => maps:get(monotonic_start_ms, Sample),
            monotonic_finish_ms => maps:get(monotonic_finish_ms, Sample),
            monotonic_midpoint_ms => maps:get(monotonic_midpoint_ms, Sample)
        },
        [process, port, atom, ets]
    ).

observed_count(#{observed_count_including_observer := Count}) -> Count;
observed_count(#{observed_count := Count}) -> Count.

-ifdef(TEST).
-spec limit_findings([map()]) -> [map()].
-endif.
limit_findings(RuntimeSamples) ->
    lists:filtermap(fun(Domain) -> limit_finding(Domain, RuntimeSamples) end, [
        process, port, atom, ets
    ]).

limit_finding(Domain, Samples) ->
    Highest = lists:foldl(
        fun(Sample, Best) -> higher_sample(Domain, Sample, Best) end, none, Samples
    ),
    case Highest of
        none ->
            false;
        Sample ->
            Count = maps:get(count_key(Domain), Sample),
            Limit = maps:get(limit_key(Domain), Sample),
            case severity(Count, Limit) of
                none -> false;
                Severity -> {true, finding(Domain, Severity, Count, Limit, Sample)}
            end
    end.

higher_sample(_Domain, Sample, none) ->
    Sample;
higher_sample(Domain, Sample, Best) ->
    Count = maps:get(count_key(Domain), Sample),
    Limit = maps:get(limit_key(Domain), Sample),
    BestCount = maps:get(count_key(Domain), Best),
    BestLimit = maps:get(limit_key(Domain), Best),
    case Count * BestLimit > BestCount * Limit of
        true -> Sample;
        false -> Best
    end.

severity(Count, Limit) when Count * 100 >= Limit * 95 -> critical;
severity(Count, Limit) when Count * 100 > Limit * 85 -> warning;
severity(_Count, _Limit) -> none.

finding(Domain, Severity, Count, Limit, Sample) ->
    Index = maps:get(sample_index, Sample),
    Ratio = Count / Limit,
    #{
        id => rule_id(Domain),
        severity => Severity,
        entity => #{type => node, id => {identifier, node, node()}},
        summary => iolist_to_binary(
            io_lib:format(
                "~s table reached ~.1f% during capture.", [domain_name(Domain), Ratio * 100]
            )
        ),
        ruleset_version => ?RULESET_VERSION,
        evidence => [
            #{
                path => iolist_to_binary(
                    io_lib:format(
                        "/data/context/snapshot/runtime_samples/~B/~s_usage_ratio",
                        [Index, atom_to_list(Domain)]
                    )
                ),
                sample_index => Index,
                monotonic_midpoint_ms => maps:get(monotonic_midpoint_ms, Sample),
                observed => Ratio,
                observed_count_including_observer => Count,
                limit => Limit,
                observer_contaminated => Domain =/= ets,
                operator => operator(Severity),
                threshold => threshold(Severity)
            }
        ],
        recommendations => [recommendation(Domain)]
    }.

operator(critical) -> <<">=">>;
operator(warning) -> <<">">>.

threshold(critical) -> 0.95;
threshold(warning) -> 0.85.

rule_id(process) -> <<"vm.process_limit_pressure">>;
rule_id(port) -> <<"vm.port_limit_pressure">>;
rule_id(atom) -> <<"vm.atom_limit_pressure">>;
rule_id(ets) -> <<"vm.ets_limit_pressure">>.

domain_name(process) -> "Process";
domain_name(port) -> "Port";
domain_name(atom) -> "Atom";
domain_name(ets) -> "ETS".

recommendation(process) ->
    <<"Inspect process memory and message-queue rankings before changing the limit.">>;
recommendation(port) ->
    <<"Inspect Port ownership and queues before changing the limit.">>;
recommendation(atom) ->
    <<"Inspect atom creation paths before changing the limit.">>;
recommendation(ets) ->
    <<"Inspect ETS ownership and table counts before changing the limit.">>.

process_context([First, Second]) ->
    case {inventory(First), inventory(Second)} of
        {{ok, FirstInventory}, {ok, SecondInventory}} ->
            (reductions_context(FirstInventory, SecondInventory))#{
                interval_ms => maps:get(monotonic_midpoint_ms, Second) -
                    maps:get(monotonic_midpoint_ms, First)
            };
        {{unavailable, Reason}, _} ->
            unavailable_context(Reason);
        {_, {unavailable, Reason}} ->
            unavailable_context(Reason);
        _ ->
            #{status => partial, reason_code => process_inventory_failed, items => []}
    end;
process_context(_Samples) ->
    #{status => partial, reason_code => sampling_gap, items => []}.

inventory(#{process_inventory := #{status := ok, values := Values} = Inventory}) ->
    {ok, Inventory#{values := Values}};
inventory(#{process_inventory := #{status := unavailable, reason_code := Reason}}) ->
    {unavailable, Reason};
inventory(#{process_inventory := #{status := Status}}) when Status =:= error; Status =:= timeout ->
    {error, Status};
inventory(_Sample) ->
    {unavailable, capability_unavailable}.

unavailable_context(Reason) -> #{status => unavailable, reason_code => Reason, items => []}.

-ifdef(TEST).
-spec reductions_context(map(), map()) -> map().
-endif.
reductions_context(#{values := First} = FirstInventory, #{values := Second} = SecondInventory) ->
    FirstIds = lists:sort(maps:keys(First)),
    SecondIds = lists:sort(maps:keys(Second)),
    Stable = ordsets:intersection(FirstIds, SecondIds),
    Born = ordsets:subtract(SecondIds, FirstIds),
    Dead = ordsets:subtract(FirstIds, SecondIds),
    Items0 = [process_delta(Pid, maps:get(Pid, First), maps:get(Pid, Second)) || Pid <- Stable],
    Denominator = lists:sum([
        Delta
     || #{reductions_delta := Delta} <- Items0, is_integer(Delta), Delta > 0
    ]),
    Items1 = [add_share(Item, Denominator) || Item <- Items0],
    Items = lists:sublist(lists:sort(fun reduction_precedes/2, Items1), ?CONTEXT_LIMIT),
    #{
        status => ok,
        denominator_semantics => all_stable_scanned_positive_reductions,
        stable_positive_reductions_denominator => Denominator,
        born_count => length(Born),
        dead_count => length(Dead),
        reset_count => length([reset || #{reductions_state := counter_reset} <- Items0]),
        born_pids => [{identifier, pid, Pid} || Pid <- Born],
        dead_pids => [{identifier, pid, Pid} || Pid <- Dead],
        sample_audits => [
            maps:get(audit, Inventory, #{})
         || Inventory <- [
                FirstInventory, SecondInventory
            ]
        ],
        items => Items
    }.

process_delta(Pid, First, Second) ->
    Before = maps:get(reductions, First),
    After = maps:get(reductions, Second),
    Reduction =
        case After >= Before of
            true -> #{reductions_state => available, reductions_delta => After - Before};
            false -> #{reductions_state => counter_reset, reductions_delta => null}
        end,
    Reduction#{
        pid => {identifier, pid, Pid},
        message_queue_len_delta =>
            maps:get(message_queue_len, Second) - maps:get(message_queue_len, First),
        memory_bytes_delta => maps:get(memory_bytes, Second) - maps:get(memory_bytes, First)
    }.

add_share(#{reductions_delta := Delta} = Item, Denominator) when
    is_integer(Delta), Delta > 0, Denominator > 0
->
    Item#{share_of_stable_scanned_reductions => Delta / Denominator};
add_share(Item, _Denominator) ->
    Item#{share_of_stable_scanned_reductions => null}.

reduction_precedes(A, B) ->
    reduction_value(A) > reduction_value(B) orelse
        (reduction_value(A) =:= reduction_value(B) andalso maps:get(pid, A) < maps:get(pid, B)).

reduction_value(#{reductions_delta := Value}) when is_integer(Value) -> Value;
reduction_value(_Item) -> -1.

optional_status(Samples) ->
    Inventories = [inventory(Sample) || Sample <- Samples],
    case
        lists:any(
            fun
                ({error, _}) -> true;
                (_) -> false
            end,
            Inventories
        )
    of
        true ->
            error;
        false ->
            case
                lists:all(
                    fun
                        ({ok, _}) -> true;
                        (_) -> false
                    end,
                    Inventories
                )
            of
                true -> ok;
                false -> unavailable
            end
    end.

distribution_status(#{status := error}) -> error;
distribution_status(_Distribution) -> ok.

capture_status(true, OptionalStatuses) ->
    case lists:member(error, OptionalStatuses) of
        true -> partial;
        false -> complete
    end;
capture_status(false, _OptionalStatuses) ->
    partial.

probe_reports(Samples, RequiredComplete, ProcessStatus, DistributionStatus) ->
    [
        #{
            id => core_limits,
            required => true,
            status => status(RequiredComplete),
            reason_code => reason(RequiredComplete, required_coverage_incomplete),
            duration_ms => sample_duration(Samples),
            samples => length([ok || Sample <- Samples, valid_required_sample(Sample)]),
            coverage => [process_count_limit, port_count_limit, atom_count_limit, ets_count_limit]
        },
        #{
            id => process_inventory,
            required => false,
            status => ProcessStatus,
            reason_code => optional_reason(ProcessStatus, Samples),
            duration_ms => inventory_duration(Samples),
            samples => length([ok || Sample <- Samples, element(1, inventory(Sample)) =:= ok]),
            coverage => [shared_same_point_process_facts, stable_pid_intersection]
        },
        #{
            id => distribution,
            required => false,
            status => DistributionStatus,
            reason_code => reason(DistributionStatus =:= ok, distribution_probe_failed),
            duration_ms => 0,
            samples => 1,
            coverage => [public_connected_peers, context_only]
        }
    ].

status(true) -> ok;
status(false) -> error.

reason(true, _Reason) -> null;
reason(false, Reason) -> Reason.

optional_reason(ok, _Samples) ->
    null;
optional_reason(error, _Samples) ->
    process_inventory_failed;
optional_reason(unavailable, Samples) ->
    case [Reason || Sample <- Samples, {unavailable, Reason} <- [inventory(Sample)]] of
        [Reason | _] -> Reason;
        [] -> capability_unavailable
    end.

sample_duration([]) ->
    0;
sample_duration(Samples) ->
    Starts = [maps:get(monotonic_start_ms, S) || S <- Samples, maps:is_key(monotonic_start_ms, S)],
    Finishes = [
        maps:get(monotonic_finish_ms, S)
     || S <- Samples, maps:is_key(monotonic_finish_ms, S)
    ],
    case {Starts, Finishes} of
        {[], _} -> 0;
        {_, []} -> 0;
        _ -> lists:max(Finishes) - lists:min(Starts)
    end.

inventory_duration(Samples) ->
    lists:sum([
        maps:get(scan_finished_monotonic_ms, Audit, 0) -
            maps:get(scan_started_monotonic_ms, Audit, 0)
     || Sample <- Samples,
        #{process_inventory := #{audit := Audit}} <- [Sample]
    ]).

sampling_plan(Plan, Samples) ->
    #{
        mode => quick,
        planned_sample_count => 2,
        planned_interval_ms => lists:nth(2, Plan) - lists:nth(1, Plan),
        target_monotonic_times_ms => Plan,
        actual_samples => [
            #{
                sample_index => Index,
                target_monotonic_ms => lists:nth(Index + 1, Plan),
                started_monotonic_ms => maps:get(monotonic_start_ms, Sample, null),
                finished_monotonic_ms => maps:get(monotonic_finish_ms, Sample, null),
                midpoint_monotonic_ms => maps:get(monotonic_midpoint_ms, Sample, null)
            }
         || {Index, Sample} <- indexed(Samples)
        ]
    }.

skipped_checks(Samples) ->
    Growth = [
        mailbox_backlog_suspects,
        memory_growth_suspects,
        ets_growth_suspects,
        port_queue_backlog_suspects,
        binary_retention_suspects
    ],
    InventorySkips = lists:usort([
        #{id => hot_processes_by_reductions, reason_code => Reason}
     || Sample <- Samples, {unavailable, Reason} <- [inventory(Sample)]
    ]),
    [#{id => Id, reason_code => ruleset_not_calibrated} || Id <- Growth] ++
        [#{id => scheduler_pressure, reason_code => scheduler_wall_time_not_enabled}] ++
        InventorySkips.

summary(partial, _Findings) ->
    <<"Quick diagnostics capture is partial; findings suppressed.">>;
summary(complete, []) ->
    <<"Quick diagnostics completed with no limit findings.">>;
summary(complete, Findings) ->
    iolist_to_binary(
        io_lib:format("Quick diagnostics found ~B limit finding(s).", [length(Findings)])
    ).

capture_errors(false, _ProcessStatus, _DistributionStatus) ->
    [#{class => required_probe, probe => core_limits, reason_code => required_coverage_incomplete}];
capture_errors(true, ProcessStatus, DistributionStatus) ->
    [
        #{class => partial, probe => Probe, reason_code => Reason}
     || {Probe, Status, Reason} <- [
            {process_inventory, ProcessStatus, process_inventory_failed},
            {distribution, DistributionStatus, distribution_probe_failed}
        ],
        Status =:= error
    ].

observer_effects(Timing) ->
    [
        #{
            id => diagnostics_worker,
            affected_facts => [process_count, port_count, atom_count, memory, reductions]
        },
        #{
            id => module_load,
            module_loaded_before_sample => maps:get(module_loaded_before_sample, Timing)
        },
        #{
            id => distribution_controller,
            controller_peer => {identifier, peer, node(maps:get(controller, Timing))},
            dynamic_controller_name_atom => true
        }
    ].

count_key(process) -> process_observed_count_including_observer;
count_key(port) -> port_observed_count_including_observer;
count_key(atom) -> atom_observed_count_including_observer;
count_key(ets) -> ets_observed_count_including_observer.

limit_key(process) -> process_limit;
limit_key(port) -> port_limit;
limit_key(atom) -> atom_limit;
limit_key(ets) -> ets_limit.

ratio_key(process) -> process_usage_ratio;
ratio_key(port) -> port_usage_ratio;
ratio_key(atom) -> atom_usage_ratio;
ratio_key(ets) -> ets_usage_ratio.

indexed(List) -> indexed(List, 0).
indexed([Item | Rest], Index) -> [{Index, Item} | indexed(Rest, Index + 1)];
indexed([], _Index) -> [].

rfc3339(SystemTime) ->
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}, {offset, "Z"}])
    ).
