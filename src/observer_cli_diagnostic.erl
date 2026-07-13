-module(observer_cli_diagnostic).

-export([capture/2]).

-ifdef(TEST).
-export([
    build_report/4,
    limit_findings/1,
    reductions_context/2,
    scheduler_findings/1,
    application_trend/1,
    observation_report/6,
    valid_memory_sample/1,
    valid_application_sample/2,
    map_status/1,
    scheduler_status/1,
    scheduler_over_threshold/1,
    map_gauge_trend/3,
    entity_trends/4,
    rule_id/1,
    domain_name/1,
    recommendation/1,
    process_context/1,
    inventory/1,
    optional_status/1,
    distribution_status/1,
    optional_reason/2,
    sample_duration/1,
    pressure_window/2,
    same_generation/1,
    reductions_rates/2,
    rates/2,
    positive_step_ratio/1,
    metric_series_delta/3,
    first_field_reason/2,
    required_complete/1,
    valid_limit/1,
    skipped_checks/1,
    capture_observation_samples/6,
    capture_samples/3
]).
-endif.

-define(DEFAULT_INTERVAL_MS, 1500).
-define(RULESET, <<"observer_cli.quick">>).
-define(RULESET_VERSION, 1).
-define(CONTEXT_LIMIT, 20).

-spec capture(map(), map()) -> map() | {probe_error, atom()}.
capture(Request, #{controller := Controller} = Context) when is_map(Request) ->
    case maps:find(observe, Request) of
        {ok, Observe} -> capture_observation(Request, Context, Controller, Observe);
        error -> capture_quick(Request, Context, Controller)
    end;
capture(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_quick(Request, Context, Controller) ->
    StartedAt = erlang:system_time(millisecond),
    Started = erlang:monotonic_time(millisecond),
    ModuleLoaded = is_tuple(code:is_loaded(?MODULE)),
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
    ).

capture_observation(Request, Context, Controller, Observe) ->
    case observation_duration(Observe) of
        {ok, Duration} ->
            Mode = observation_mode(Request),
            Count =
                case Mode of
                    deep -> 7;
                    _ -> 5
                end,
            StartedAt = erlang:system_time(millisecond),
            Started = erlang:monotonic_time(millisecond),
            Plan = observation_plan(Started, Duration, Count),
            ModuleLoaded = is_tuple(code:is_loaded(?MODULE)),
            _ = observer_cli_snapshot:diagnostic_scheduler_flag(true),
            try
                Samples = capture_observation_samples(Request, Context, Plan),
                Holder = final_binary_holders(Mode, Request, Context),
                Finished = erlang:monotonic_time(millisecond),
                observation_report(
                    Mode,
                    Samples,
                    Plan,
                    Holder,
                    #{
                        started_at => rfc3339(StartedAt),
                        finished_at => rfc3339(erlang:system_time(millisecond)),
                        duration_ms => Finished - Started,
                        controller => Controller,
                        module_loaded_before_sample => ModuleLoaded
                    },
                    distribution(Controller)
                )
            after
                _ = observer_cli_snapshot:diagnostic_scheduler_flag(false)
            end;
        error ->
            {probe_error, invalid_duration}
    end.

observation_duration(Text) when is_list(Text) ->
    observation_duration(unicode:characters_to_binary(Text));
observation_duration(Text) when is_binary(Text) ->
    case re:run(Text, <<"^([0-9]+)(ms|s)?$">>, [{capture, [1, 2], binary}]) of
        {match, [Digits, <<"s">>]} ->
            valid_observation_ms(binary_to_integer(Digits) * 1000);
        {match, [Digits, _]} ->
            valid_observation_ms(binary_to_integer(Digits));
        nomatch ->
            error
    end;
observation_duration(_) ->
    error.

valid_observation_ms(Duration) when Duration >= 5000, Duration =< 60000 -> {ok, Duration};
valid_observation_ms(_Duration) -> error.

observation_mode(#{deep := true}) -> deep;
observation_mode(#{app := _}) -> application;
observation_mode(_) -> observation.

observation_plan(Started, Duration, Count) ->
    [Started + ((Duration * Index) div (Count - 1)) || Index <- lists:seq(0, Count - 1)].

capture_observation_samples(Request, Context, Plan) ->
    capture_observation_samples(Request, Context, Plan, 0, undefined, []).

capture_observation_samples(_Request, _Context, [], _Index, _PreviousFinish, Acc) ->
    lists:reverse(Acc);
capture_observation_samples(Request, Context, [Target | Rest], Index, PreviousFinish, Acc) ->
    case PreviousFinish =/= undefined andalso PreviousFinish > Target of
        true ->
            Gap = #{status => error, reason_code => sampling_gap, target_monotonic_ms => Target},
            capture_observation_samples(Request, Context, Rest, Index + 1, PreviousFinish, [
                Gap | Acc
            ]);
        false ->
            sleep_until(Target),
            SchedulerEnd =
                case Acc of
                    [] -> undefined;
                    _ -> safe_scheduler_sample()
                end,
            Sample0 = capture_sample(Request, Context, Index, Target),
            SchedulerBaseline = safe_scheduler_sample(),
            Sample = Sample0#{
                target_monotonic_ms => Target,
                scheduler_end => SchedulerEnd,
                scheduler_baseline => SchedulerBaseline
            },
            Finish = maps:get(monotonic_finish_ms, Sample, erlang:monotonic_time(millisecond)),
            NextRequest = retain_application_refusal(Request, Sample),
            capture_observation_samples(NextRequest, Context, Rest, Index + 1, Finish, [
                Sample | Acc
            ])
    end.

retain_application_refusal(
    #{app := _} = Request,
    #{application := #{status := unavailable, reason_code := scan_budget_exceeded} = Application}
) ->
    Request#{application_refusal => Application};
retain_application_refusal(Request, _Sample) ->
    Request.

safe_scheduler_sample() ->
    try
        observer_cli_snapshot:diagnostic_scheduler_sample()
    catch
        _:_ -> #{status => error, reason_code => scheduler_sample_failed}
    end.

final_binary_holders(deep, Request, Context) ->
    try
        observer_cli_snapshot:diagnostic_binary_holders(Request, Context)
    catch
        _:_ -> #{status => error, reason_code => binary_holder_scan_failed}
    end;
final_binary_holders(_Mode, _Request, _Context) ->
    #{status => unavailable, reason_code => deep_not_requested}.

capture_samples(Request, Context, [First, Second]) ->
    FirstSample = capture_sample(Request, Context, 0, First),
    case maps:get(monotonic_finish_ms, FirstSample, First) > Second of
        true -> [FirstSample, #{status => error, reason_code => sampling_gap}];
        false -> [FirstSample, capture_sample(Request, Context, 1, Second)]
    end.

capture_sample(Request, Context, Index, Target) ->
    sleep_until(Target),
    try sample_request(Request, Context, Index) of
        Sample -> Sample
    catch
        _Class:_Reason:_Stacktrace -> #{status => error, reason_code => required_probe_failed}
    end.

sample_request(#{app := _, application_refusal := Application} = Request, Context, Index) ->
    Sample = sample(maps:remove(app, maps:remove(application_refusal, Request)), Context, Index),
    Sample#{application => Application};
sample_request(Request, Context, Index) ->
    sample(Request, Context, Index).

distribution(Controller) ->
    try observer_cli_snapshot:diagnostic_distribution(Controller) of
        Context -> Context
    catch
        _Class:_Reason:_Stacktrace -> #{status => error, reason_code => distribution_probe_failed}
    end.

-ifdef(TEST).
sample(#{test_samples := Samples}, _Context, Index) ->
    lists:nth(Index + 1, Samples);
sample(Request, Context, Index) ->
    observer_cli_snapshot:diagnostic_sample(Request#{sample_index => Index}, Context).
-else.
sample(Request, Context, Index) ->
    observer_cli_snapshot:diagnostic_sample(Request#{sample_index => Index}, Context).
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
    EtsContext = current_entity_context(Samples, ets_inventory, [size, memory_words]),
    PortContext = current_entity_context(Samples, port_inventory, [
        queue_size, memory, input, output
    ]),
    SchedulerContext = quick_scheduler_context(Samples),
    ExtraStatuses = [
        quick_context_status(Context)
     || Context <- [
            EtsContext, PortContext, SchedulerContext
        ]
    ],
    Status = capture_status(RequiredComplete, [ProcessStatus, DistributionStatus | ExtraStatuses]),
    RuntimeSamples = runtime_samples(Samples),
    Findings =
        case RequiredComplete of
            true -> limit_findings(RuntimeSamples);
            false -> []
        end,
    ProcessContext = process_context(Samples),
    Skipped = skipped_checks(Samples),
    observer_cli_cli:response(
        diagnose,
        Status,
        #{
            node => {identifier, node, node()},
            otp_release => unicode:characters_to_binary(erlang:system_info(otp_release))
        },
        #{
            started_at => maps:get(started_at, Timing),
            finished_at => maps:get(finished_at, Timing),
            duration_ms => maps:get(duration_ms, Timing),
            probes => probe_reports(Samples, RequiredComplete, ProcessStatus, DistributionStatus) ++
                quick_context_probes(EtsContext, PortContext, SchedulerContext),
            observer_effects => observer_effects(Timing)
        },
        #{
            ruleset => ?RULESET,
            ruleset_version => ?RULESET_VERSION,
            sampling_plan => sampling_plan(Plan, Samples),
            findings => Findings,
            suspects => [],
            context => #{
                snapshot => #{runtime_samples => RuntimeSamples},
                hot_processes_by_reductions => ProcessContext,
                ets => EtsContext,
                ports => PortContext,
                scheduler => SchedulerContext,
                distribution => Distribution
            },
            skipped => Skipped,
            summary => summary(Status, Findings)
        },
        []
    ).

quick_context_status(#{status := Status}) when Status =:= error; Status =:= invalid -> error;
quick_context_status(#{status := Status}) when Status =:= ok; Status =:= valid -> ok;
quick_context_status(_) -> unavailable.

quick_context_probes(Ets, Ports, Scheduler) ->
    [
        quick_context_probe(Id, Context)
     || {Id, Context} <- [
            {ets_inventory, Ets}, {port_inventory, Ports}, {scheduler_pressure, Scheduler}
        ]
    ].

quick_context_probe(Id, Context) ->
    Status = quick_context_status(Context),
    #{
        id => Id,
        required => false,
        status => Status,
        reason_code => maps:get(reason_code, Context, null),
        duration_ms => 0,
        samples =>
            case Status of
                ok -> 1;
                _ -> 0
            end,
        coverage => [current_context_only]
    }.

current_entity_context(Samples, Field, Metrics) ->
    case
        [
            Value
         || Sample <- Samples,
            Value <- [maps:get(Field, Sample, #{})],
            maps:get(status, Value, unavailable) =:= ok
        ]
    of
        [#{values := Values} | _] ->
            Sort = hd(Metrics),
            Items0 = [
                (maps:with(Metrics, Item))#{id => diagnostic_identifier(Id)}
             || {Id, Item} <- maps:to_list(Values)
            ],
            Items = recon_top_n(Items0, Sort, ?CONTEXT_LIMIT),
            #{status => ok, sort_metric => Sort, sort_semantics => current, items => Items};
        [] ->
            current_field_status(Samples, Field)
    end.

current_field_status(Samples, Field) ->
    Values = [maps:get(Field, Sample, #{}) || Sample <- Samples],
    case [Value || #{status := error} = Value <- Values] of
        [Error | _] ->
            Error;
        [] ->
            case [Value || #{status := unavailable} = Value <- Values] of
                [Unavailable | _] -> Unavailable;
                [] -> #{status => unavailable, reason_code => capability_unavailable}
            end
    end.

quick_scheduler_context(Samples) ->
    SchedulerSamples = [Scheduler || #{quick_scheduler_sample := Scheduler} <- Samples],
    case SchedulerSamples of
        [#{wall_time := undefined}, #{wall_time := undefined}] ->
            #{status => unavailable, reason_code => scheduler_wall_time_not_enabled};
        [First, Second] ->
            observer_cli_snapshot:diagnostic_scheduler_window(First, Second);
        _ ->
            #{status => unavailable, reason_code => scheduler_wall_time_not_enabled}
    end.

observation_report(Mode, Samples, Plan, Holder, Timing, Distribution) ->
    RequiredComplete = observation_required_complete(Mode, Samples),
    OptionalStatuses = observation_optional_statuses(Mode, Samples, Holder, Distribution),
    Status = capture_status(RequiredComplete, OptionalStatuses),
    RuntimeSamples = runtime_samples(Samples),
    Windows = scheduler_windows(Samples),
    Findings =
        case RequiredComplete of
            true -> limit_findings(RuntimeSamples) ++ scheduler_findings(Windows);
            false -> []
        end,
    observer_cli_cli:response(
        diagnose,
        Status,
        #{
            node => {identifier, node, node()},
            otp_release => unicode:characters_to_binary(erlang:system_info(otp_release))
        },
        #{
            started_at => maps:get(started_at, Timing),
            finished_at => maps:get(finished_at, Timing),
            duration_ms => maps:get(duration_ms, Timing),
            probes => observation_probe_reports(Mode, Samples, Holder, RequiredComplete),
            observer_effects => [
                #{
                    id => scheduler_wall_time,
                    temporary_enable => true,
                    observer_contaminated => true
                }
                | observer_effects(Timing)
            ]
        },
        #{
            ruleset => ruleset(Mode),
            ruleset_version => ?RULESET_VERSION,
            sampling_plan => observation_sampling_plan(Mode, Plan, Samples),
            findings => Findings,
            suspects => [],
            context => #{
                snapshot => #{runtime_samples => RuntimeSamples},
                trends => observation_trends(Samples),
                scheduler_windows => Windows,
                application => application_trend(Samples),
                binary_holders => Holder,
                distribution => Distribution
            },
            skipped => observation_skipped(Mode, Samples, Holder),
            summary => observation_summary(Mode, Status, Findings)
        },
        []
    ).

ruleset(observation) -> <<"observer_cli.observation">>;
ruleset(deep) -> <<"observer_cli.deep_observation">>;
ruleset(application) -> <<"observer_cli.application_observation">>.

observation_required_complete(Mode, Samples) ->
    Expected =
        case Mode of
            deep -> 7;
            _ -> 5
        end,
    length(Samples) =:= Expected andalso
        lists:all(
            fun(Sample) ->
                valid_required_sample(Sample) andalso valid_memory_sample(Sample) andalso
                    valid_application_sample(Mode, Sample)
            end,
            Samples
        ).

valid_memory_sample(#{memory := #{status := ok, values := Values}}) ->
    is_map(Values) andalso maps:is_key(total_bytes, Values) andalso
        maps:is_key(binary_bytes, Values);
valid_memory_sample(_) ->
    false.

valid_application_sample(application, #{application := #{status := Status}}) ->
    Status =:= ok orelse Status =:= not_running;
valid_application_sample(application, _) ->
    false;
valid_application_sample(_Mode, _Sample) ->
    true.

observation_optional_statuses(Mode, Samples, Holder, Distribution) ->
    Statuses =
        [
            series_optional_status(Samples, Field)
         || Field <- [
                process_inventory, ets_inventory, port_inventory, socket_inventory
            ]
        ] ++
            [scheduler_status(scheduler_windows(Samples)), distribution_status(Distribution)],
    case Mode of
        deep -> [map_status(Holder) | Statuses];
        _ -> Statuses
    end.

series_optional_status(Samples, Field) ->
    case field_series_status(Samples, Field) of
        ok -> ok;
        unavailable -> unavailable;
        invalid -> error
    end.
map_status(#{status := Status}) when Status =:= error; Status =:= timeout -> error;
map_status(#{status := ok}) -> ok;
map_status(_) -> unavailable.

scheduler_status(Windows) ->
    case lists:any(fun(#{status := Status}) -> Status =/= valid end, Windows) of
        true ->
            error;
        false ->
            case Windows of
                [] -> unavailable;
                _ -> ok
            end
    end.

scheduler_windows(Samples) ->
    scheduler_windows(Samples, []).

scheduler_windows([First, Second | Rest], Acc) ->
    Window =
        case {maps:find(scheduler_baseline, First), maps:find(scheduler_end, Second)} of
            {{ok, Baseline}, {ok, End}} ->
                (observer_cli_snapshot:diagnostic_scheduler_window(Baseline, End))#{
                    heavy_probe_overlap => false,
                    from_sample_index => length(Acc),
                    to_sample_index => length(Acc) + 1,
                    monotonic_midpoint_ms => maps:get(monotonic_midpoint_ms, Second, 0)
                };
            _ ->
                #{status => invalid, reason_code => sampling_gap, heavy_probe_overlap => false}
        end,
    scheduler_windows([Second | Rest], [Window | Acc]);
scheduler_windows(_, Acc) ->
    lists:reverse(Acc).

-ifdef(TEST).
-spec scheduler_findings([map()]) -> [map()].
-endif.
scheduler_findings(Windows) ->
    lists:filtermap(fun(Pool) -> scheduler_finding(Pool, Windows) end, [normal, dirty_cpu]).

scheduler_finding(Pool, Windows) ->
    case consecutive_pressure(Pool, Windows, 0, []) of
        {true, Evidence} ->
            {true, #{
                id => <<"vm.scheduler_pressure">>,
                severity => warning,
                entity => #{type => scheduler_pool, id => atom_to_binary(Pool)},
                summary => <<"Scheduler pool reached sustained pressure during capture.">>,
                ruleset_version => ?RULESET_VERSION,
                evidence => Evidence,
                recommendations => [
                    <<"Inspect runnable work and reductions context before tuning schedulers.">>
                ]
            }};
        false ->
            false
    end.

consecutive_pressure(_Pool, [], _Run, _Evidence) ->
    false;
consecutive_pressure(Pool, [Window | Rest], Run, Evidence) ->
    case pressure_window(Pool, Window) of
        {true, Item} when Run + 1 >= 2 -> {true, lists:reverse([Item | Evidence])};
        {true, Item} -> consecutive_pressure(Pool, Rest, Run + 1, [Item | Evidence]);
        false -> consecutive_pressure(Pool, Rest, 0, [])
    end.

pressure_window(
    Pool,
    #{
        status := valid,
        heavy_probe_overlap := false,
        run_queues := RunQueues
    } = Window
) ->
    PoolData = maps:get(Pool, Window, #{}),
    Queue = maps:get(Pool, RunQueues, #{}),
    Ratio = maps:get(utilization_ratio, PoolData, 0),
    Runnable = maps:get(end_observed_runnable_count_including_observer, Queue, 0),
    case
        maps:get(status, PoolData, unavailable) =:= available andalso
            scheduler_over_threshold(PoolData) andalso
            Runnable > 0
    of
        true ->
            {true, #{
                path => <<"/data/context/scheduler_windows">>,
                sample_index => maps:get(to_sample_index, Window, 0),
                monotonic_midpoint_ms => maps:get(monotonic_midpoint_ms, Window, 0),
                observed => Ratio,
                operator => <<">=">>,
                threshold => 0.8,
                pool => Pool,
                utilization_ratio => Ratio,
                observed_runnable_count_including_observer => Runnable,
                wall_time_unit => opaque_same_window,
                run_queue_snapshot_atomic => false,
                heavy_probe_overlap => false
            }};
        false ->
            false
    end;
pressure_window(_Pool, _Window) ->
    false.

scheduler_over_threshold(#{active_delta := #{value := Active}, total_delta := #{value := Total}}) when
    is_integer(Active), is_integer(Total), Total > 0
->
    Active * 100 >= Total * 80;
scheduler_over_threshold(#{utilization_ratio := Ratio}) ->
    Ratio >= 0.8;
scheduler_over_threshold(_) ->
    false.

observation_trends(Samples) ->
    case lists:all(fun(Sample) -> maps:get(status, Sample, error) =:= ok end, Samples) of
        false ->
            #{status => invalid, reason_code => sampling_gap};
        true ->
            valid_observation_trends(Samples)
    end.

valid_observation_trends(Samples) ->
    #{
        status => ok,
        global_memory => map_gauge_trend(Samples, memory, values),
        processes => entity_trends(
            Samples,
            process_inventory,
            values,
            [message_queue_len, memory_bytes]
        ),
        ets => entity_trends(Samples, ets_inventory, values, [size, memory_words]),
        ports => entity_trends(Samples, port_inventory, values, [queue_size, memory, input, output]),
        sockets => socket_trends(Samples)
    }.

map_gauge_trend([], _Field, _Values) ->
    #{status => unavailable};
map_gauge_trend(Samples, Field, ValuesKey) ->
    First = maps:get(ValuesKey, maps:get(Field, hd(Samples), #{}), #{}),
    Last = maps:get(ValuesKey, maps:get(Field, lists:last(Samples), #{}), #{}),
    Interval = inventory_interval(Samples, Field),
    Deltas = gauge_deltas(First, Last),
    #{
        status => ok,
        sample_count => length(Samples),
        interval_ms => Interval,
        deltas => Deltas,
        rates_per_second => rates(Deltas, Interval)
    }.

gauge_deltas(First, Last) ->
    maps:from_list([
        {Key, maps:get(Key, Last) - maps:get(Key, First)}
     || Key <- maps:keys(First),
        is_integer(maps:get(Key, First)),
        is_integer(maps:get(Key, Last, undefined))
    ]).

entity_trends([], _Field, _Values, _Metrics) ->
    #{status => unavailable, items => []};
entity_trends(Samples, Field, ValuesKey, Metrics) ->
    case field_series_status(Samples, Field) of
        ok -> valid_entity_trends(Samples, Field, ValuesKey, Metrics);
        unavailable -> #{status => unavailable, items => []};
        invalid -> #{status => invalid, reason_code => sampling_gap, items => []}
    end.

valid_entity_trends(Samples, Field, ValuesKey, Metrics) ->
    ValueMaps = [maps:get(ValuesKey, maps:get(Field, Sample, #{}), #{}) || Sample <- Samples],
    First = hd(ValueMaps),
    Last = lists:last(ValueMaps),
    Shared = lists:foldl(
        fun(Values, Ids) ->
            ordsets:intersection(Ids, lists:sort(maps:keys(Values)))
        end,
        lists:sort(maps:keys(First)),
        tl(ValueMaps)
    ),
    Stable = [Id || Id <- Shared, same_generation([maps:get(Id, Values) || Values <- ValueMaps])],
    Replaced = ordsets:subtract(Shared, Stable),
    Interval = inventory_interval(Samples, Field),
    Items0 = [
        entity_trend_item(Id, [maps:get(Id, Values) || Values <- ValueMaps], Metrics, Interval)
     || Id <- Stable
    ],
    SortMetric = hd(Metrics),
    Items = recon_top_n(Items0, SortMetric, ?CONTEXT_LIMIT),
    #{
        status => ok,
        sample_count => length(Samples),
        born_count => length(
            ordsets:subtract(lists:sort(maps:keys(Last)), lists:sort(maps:keys(First)))
        ),
        dead_count => length(
            ordsets:subtract(lists:sort(maps:keys(First)), lists:sort(maps:keys(Last)))
        ),
        replaced_count => length(Replaced),
        replaced => [diagnostic_identifier(Id) || Id <- Replaced],
        interval_ms => Interval,
        sort_metric => SortMetric,
        sort_semantics => delta_descending,
        items => [maps:remove(raw_id, Item) || Item <- Items]
    }.

field_series_status(Samples, Field) ->
    Statuses = [maps:get(status, maps:get(Field, Sample, #{}), missing) || Sample <- Samples],
    case
        {
            lists:all(fun(Status) -> Status =:= ok end, Statuses),
            lists:all(fun(Status) -> Status =:= unavailable end, Statuses)
        }
    of
        {true, _} -> ok;
        {_, true} -> unavailable;
        _ -> invalid
    end.

socket_trends(Samples) ->
    case field_series_status(Samples, socket_inventory) of
        ok ->
            observer_cli_snapshot:diagnostic_socket_trend([
                maps:get(values, maps:get(socket_inventory, Sample))
             || Sample <- Samples
            ]);
        unavailable ->
            #{status => unavailable, items => []};
        invalid ->
            #{status => invalid, reason_code => sampling_gap, items => []}
    end.

same_generation([#{generation := Generation} | Rest]) ->
    lists:all(
        fun
            (#{generation := ItemGeneration}) -> ItemGeneration =:= Generation;
            (_) -> false
        end,
        Rest
    );
same_generation(_Items) ->
    true.

entity_trend_item(Id, Values, Metrics, Interval) ->
    Deltas = metric_series_deltas(Values, Metrics),
    Base = #{
        raw_id => Id,
        id => diagnostic_identifier(Id),
        deltas => Deltas,
        rates_per_second => rates(Deltas, Interval)
    },
    case lists:member(message_queue_len, Metrics) of
        true ->
            Base#{
                message_queue_positive_step_ratio => positive_step_ratio(
                    [maps:get(message_queue_len, Value, undefined) || Value <- Values]
                )
            };
        false ->
            Base
    end.

sample_interval(Samples) ->
    maps:get(monotonic_midpoint_ms, lists:last(Samples), 0) -
        maps:get(monotonic_midpoint_ms, hd(Samples), 0).

inventory_interval(Samples, Field) ->
    case
        {
            inventory_midpoint(hd(Samples), Field),
            inventory_midpoint(lists:last(Samples), Field)
        }
    of
        {First, Last} when is_integer(First), is_integer(Last) -> Last - First;
        _ -> sample_interval(Samples)
    end.

inventory_midpoint(Sample, Field) ->
    Audit = maps:get(audit, maps:get(Field, Sample, #{}), #{}),
    case
        {
            maps:get(scan_started_monotonic_ms, Audit, undefined),
            maps:get(scan_finished_monotonic_ms, Audit, undefined)
        }
    of
        {Started, Finished} when is_integer(Started), is_integer(Finished) ->
            Started + ((Finished - Started) div 2);
        _ ->
            undefined
    end.

reductions_rates(Items, Interval) when Interval > 0 ->
    [
        case maps:get(reductions_delta, Item, null) of
            Delta when is_integer(Delta) ->
                Item#{reductions_per_second => Delta * 1000 / Interval};
            _ ->
                Item#{reductions_per_second => null}
        end
     || Item <- Items
    ];
reductions_rates(Items, _Interval) ->
    [Item#{reductions_per_second => null} || Item <- Items].

rates(Deltas, Interval) when Interval > 0 ->
    maps:from_list([
        {Key, Value * 1000 / Interval}
     || {Key, Value} <- maps:to_list(Deltas),
        is_integer(Value)
    ]);
rates(_Deltas, _Interval) ->
    #{}.

positive_step_ratio(Values) ->
    Steps = [
        {A, B}
     || {A, B} <- lists:zip(lists:droplast(Values), tl(Values)), is_integer(A), is_integer(B)
    ],
    case Steps of
        [] -> null;
        _ -> length([ok || {A, B} <- Steps, B > A]) / length(Steps)
    end.

diagnostic_identifier(Id) when is_pid(Id) -> {identifier, pid, Id};
diagnostic_identifier(Id) when is_port(Id) -> {identifier, port, Id};
diagnostic_identifier(Id) -> {identifier, table, Id}.

metric_series_deltas(Values, Metrics) ->
    lists:foldl(fun(Key, Acc) -> metric_series_delta(Key, Values, Acc) end, #{}, Metrics).

metric_series_delta(Key, Values, Acc) when Key =:= input; Key =:= output ->
    Series = [maps:get(Key, Value, undefined) || Value <- Values],
    case lists:all(fun is_integer/1, Series) of
        true when length(Series) >= 2 ->
            case
                lists:any(
                    fun({Before, After}) -> After < Before end,
                    lists:zip(lists:droplast(Series), tl(Series))
                )
            of
                true ->
                    Acc#{
                        Key => null,
                        iolist_to_binary([atom_to_binary(Key), <<"_state">>]) => counter_reset
                    };
                false ->
                    Acc#{Key => lists:last(Series) - hd(Series)}
            end;
        _ ->
            Acc
    end;
metric_series_delta(Key, Values, Acc) ->
    case {maps:get(Key, hd(Values), undefined), maps:get(Key, lists:last(Values), undefined)} of
        {Before, After} when is_integer(Before), is_integer(After) ->
            Acc#{Key => After - Before};
        _ ->
            Acc
    end.

-ifdef(TEST).
-spec application_trend([map()]) -> map().
-endif.
application_trend(Samples) ->
    AppSamples = [maps:get(application, Sample, #{}) || Sample <- Samples],
    Statuses = [maps:get(status, App, unavailable) || App <- AppSamples],
    ScanRefused = lists:any(
        fun(App) -> maps:get(reason_code, App, undefined) =:= scan_budget_exceeded end,
        AppSamples
    ),
    case
        {
            ScanRefused,
            lists:all(fun(Status) -> Status =:= ok orelse Status =:= not_running end, Statuses),
            lists:all(fun(Status) -> Status =:= unavailable end, Statuses)
        }
    of
        {true, _Complete, _Unavailable} ->
            #{status => unavailable, reason_code => scan_budget_exceeded, items => []};
        {false, false, true} ->
            #{status => unavailable, items => []};
        {false, false, false} ->
            #{status => invalid, reason_code => sampling_gap, items => []};
        {false, true, _} ->
            ChildSamples = [application_children(App) || App <- AppSamples],
            First = application_children(hd(AppSamples)),
            Last = application_children(lists:last(AppSamples)),
            Stable = lists:foldl(
                fun(Children, Ids) ->
                    ordsets:intersection(Ids, lists:sort(maps:keys(Children)))
                end,
                lists:sort(maps:keys(First)),
                tl(ChildSamples)
            ),
            Items = [
                #{
                    id => Id,
                    pid_changed =>
                        case lists:usort([maps:get(Id, Children) || Children <- ChildSamples]) of
                            [_First, _Second | _Rest] -> true;
                            _ -> false
                        end,
                    change_semantics => restart_deploy_or_manual_change
                }
             || Id <- Stable
            ],
            #{
                status => ok,
                sample_count => length(AppSamples),
                items => Items,
                born_count => maps:size(Last) - length(Stable),
                dead_count => maps:size(First) - length(Stable),
                identity_unavailable_count => lists:sum([
                    maps:get(identity_unavailable_count, A, 0)
                 || A <- AppSamples
                ])
            }
    end.

application_children(App) ->
    maps:from_list([
        {Id, maps:get(pid, Child, null)}
     || #{identity := available, id := Id, child := Child} <- maps:get(children, App, [])
    ]).

observation_sampling_plan(Mode, Plan, Samples) ->
    #{
        mode => Mode,
        planned_sample_count => length(Plan),
        planned_duration_ms => lists:last(Plan) - hd(Plan),
        target_monotonic_times_ms => Plan,
        actual_samples => [
            #{
                sample_index => Index,
                target_monotonic_ms => lists:nth(Index + 1, Plan),
                started_monotonic_ms => maps:get(monotonic_start_ms, Sample, null),
                finished_monotonic_ms => maps:get(monotonic_finish_ms, Sample, null),
                status => maps:get(status, Sample, error),
                reason_code => maps:get(reason_code, Sample, null)
            }
         || {Index, Sample} <- indexed(Samples)
        ]
    }.

observation_probe_reports(Mode, Samples, Holder, RequiredComplete) ->
    [
        #{
            id => core_limits_and_memory,
            required => true,
            status => status(RequiredComplete),
            reason_code => reason(RequiredComplete, required_coverage_incomplete),
            duration_ms => sample_duration(Samples),
            samples => length([
                ok
             || S <- Samples, valid_required_sample(S) andalso valid_memory_sample(S)
            ]),
            coverage => [
                target_identity,
                process_count_limit,
                port_count_limit,
                atom_count_limit,
                ets_count_limit,
                global_memory
            ]
        },
        #{
            id => scheduler_pressure,
            required => false,
            status => scheduler_status(scheduler_windows(Samples)),
            reason_code => scheduler_reason(scheduler_windows(Samples)),
            duration_ms => sample_duration(Samples),
            samples => length(scheduler_windows(Samples)),
            coverage => [low_cost_windows, online_topology, opaque_same_window_units]
        },
        #{
            id => binary_holders,
            required => false,
            status =>
                case Mode of
                    deep -> map_status(Holder);
                    _ -> unavailable
                end,
            reason_code => maps:get(reason_code, Holder, null),
            duration_ms => 0,
            samples =>
                case maps:get(status, Holder, unavailable) of
                    ok -> 1;
                    _ -> 0
                end,
            coverage => [final_independent_admission, current_context_only]
        }
    ] ++
        [
            observation_inventory_probe(Field, Samples)
         || Field <- [
                process_inventory, ets_inventory, port_inventory, socket_inventory
            ]
        ].

observation_inventory_probe(Field, Samples) ->
    case field_series_status(Samples, Field) of
        ok ->
            #{
                id => Field,
                required => false,
                status => ok,
                reason_code => null,
                duration_ms => sample_duration(Samples),
                samples => length(Samples),
                coverage => [exact_generation_trend]
            };
        unavailable ->
            #{
                id => Field,
                required => false,
                status => unavailable,
                reason_code => first_field_reason(Samples, Field),
                duration_ms => 0,
                samples => 0,
                coverage => []
            };
        invalid ->
            #{
                id => Field,
                required => false,
                status => error,
                reason_code => sampling_gap,
                duration_ms => sample_duration(Samples),
                samples => length(Samples),
                coverage => []
            }
    end.

first_field_reason(Samples, Field) ->
    case
        [
            Reason
         || Sample <- Samples,
            #{reason_code := Reason} <- [maps:get(Field, Sample, #{})]
        ]
    of
        [Reason | _] -> Reason;
        [] -> capability_unavailable
    end.

scheduler_reason(Windows) ->
    case
        [
            maps:get(reason_code, Window, scheduler_window_invalid)
         || Window <- Windows,
            maps:get(status, Window, invalid) =/= valid
        ]
    of
        [Reason | _] -> Reason;
        [] -> null
    end.

observation_skipped(Mode, _Samples, _Holder) ->
    Growth = [
        mailbox_backlog_suspects,
        memory_growth_suspects,
        ets_growth_suspects,
        port_queue_backlog_suspects,
        supervisor_restart_suspects
    ],
    Binary =
        case Mode of
            deep -> [#{id => binary_retention_suspects, reason_code => ruleset_not_calibrated}];
            _ -> [#{id => binary_retention_suspects, reason_code => deep_not_requested}]
        end,
    [#{id => Id, reason_code => ruleset_not_calibrated} || Id <- Growth] ++
        Binary.

observation_summary(Mode, partial, _Findings) ->
    iolist_to_binary(
        io_lib:format("~p diagnostics capture is partial; findings suppressed.", [Mode])
    );
observation_summary(Mode, complete, Findings) ->
    iolist_to_binary(
        io_lib:format("~p diagnostics completed with ~B finding(s).", [Mode, length(Findings)])
    ).

required_complete([_, _] = Samples) ->
    lists:all(fun valid_required_sample/1, Samples);
required_complete(_Samples) ->
    false.

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
            Interval = inventory_interval([First, Second], process_inventory),
            Context = reductions_context(FirstInventory, SecondInventory),
            Context#{
                interval_ms => Interval,
                items => reductions_rates(maps:get(items, Context), Interval)
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
    Reset = [
        Pid
     || #{pid := {identifier, pid, Pid}, reductions_state := counter_reset} <- Items0
    ],
    {BornPids, BornPidsTruncated} = pid_sample(Born, ?CONTEXT_LIMIT),
    {DeadPids, DeadPidsTruncated} = pid_sample(Dead, ?CONTEXT_LIMIT),
    {ResetPids, ResetPidsTruncated} = pid_sample(Reset, ?CONTEXT_LIMIT),
    Denominator = lists:sum([
        Delta
     || #{reductions_delta := Delta} <- Items0, is_integer(Delta), Delta > 0
    ]),
    Items1 = [add_share(Item, Denominator) || Item <- Items0],
    Items = recon_top_n(Items1, reductions_delta, ?CONTEXT_LIMIT),
    #{
        status => ok,
        denominator_semantics => all_stable_scanned_positive_reductions,
        stable_positive_reductions_denominator => Denominator,
        born_count => length(Born),
        dead_count => length(Dead),
        reset_count => length(Reset),
        born_pids => BornPids,
        born_pids_truncated => BornPidsTruncated,
        dead_pids => DeadPids,
        dead_pids_truncated => DeadPidsTruncated,
        reset_pids => ResetPids,
        reset_pids_truncated => ResetPidsTruncated,
        sample_audits => [
            maps:get(audit, Inventory, #{})
         || Inventory <- [
                FirstInventory, SecondInventory
            ]
        ],
        items => Items
    }.

pid_sample(Pids, Limit) ->
    {[{identifier, pid, Pid} || Pid <- lists:sublist(Pids, Limit)], length(Pids) > Limit}.

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

skipped_checks(_Samples) ->
    Growth = [
        mailbox_backlog_suspects,
        memory_growth_suspects,
        ets_growth_suspects,
        port_queue_backlog_suspects,
        binary_retention_suspects
    ],
    [#{id => Id, reason_code => ruleset_not_calibrated} || Id <- Growth].

summary(partial, _Findings) ->
    <<"Quick diagnostics capture is partial; findings suppressed.">>;
summary(complete, []) ->
    <<"Quick diagnostics completed with no limit findings.">>;
summary(complete, Findings) ->
    iolist_to_binary(
        io_lib:format("Quick diagnostics found ~B limit finding(s).", [length(Findings)])
    ).

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

recon_top_n(Items, Sort, Limit) ->
    [
        Item
     || {_, _, [Item]} <- recon_lib:sublist_top_n_attrs(
            [{top_n_identity(Item), top_n_value(Item, Sort), [Item]} || Item <- Items], Limit
        )
    ].

top_n_identity(#{pid := {identifier, pid, Pid}}) -> Pid;
top_n_identity(_Item) -> 0.

top_n_value(#{deltas := Deltas}, Sort) ->
    maps:get(Sort, Deltas, -1);
top_n_value(Item, Sort) ->
    case maps:get(Sort, Item, null) of
        Value when is_number(Value) -> Value;
        _ -> -1
    end.

indexed(List) -> indexed(List, 0).
indexed([Item | Rest], Index) -> [{Index, Item} | indexed(Rest, Index + 1)];
indexed([], _Index) -> [].

rfc3339(SystemTime) ->
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}, {offset, "Z"}])
    ).
