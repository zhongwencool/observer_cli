-module(observer_cli_snapshot).

-export([capabilities/0, dispatch/4, normalize/2, truncate/1]).

-ifdef(TEST).
-export([
    scheduler_window/2,
    measure_scheduler/4,
    distribution_context/7,
    stable_process_window/3,
    resolve_process_target/2
]).
-endif.

-define(PROTOCOL_VERSION, 1).
-define(TARGET_MARGIN_MS, 1000).
-define(MAX_HEAP_WORDS, 8 * 1024 * 1024).
-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_RESULT_BYTES, ?MAX_RESPONSE_BYTES - 1024).
-define(MAX_FIELD_BYTES, 64 * 1024).
-define(MAX_DEPTH, 32).
-define(PROCESS_SCAN_BUDGET, 100000).
-define(BINARY_PROCESS_SCAN_BUDGET, 20000).
-define(APPLICATION_SCAN_BUDGET, 5000).
-define(WORKING_SET_BYTES_PER_FIELD, 64).
-define(MAX_WORKING_SET_BYTES, 64 * 1024 * 1024).

-spec capabilities() -> #{protocol_version := pos_integer()}.
capabilities() ->
    #{protocol_version => ?PROTOCOL_VERSION}.

-spec dispatch(pid(), atom(), term(), map()) -> map().
dispatch(Controller, Command, Request, Options) when is_pid(Controller), is_atom(Command) ->
    case dispatch_options(Options) of
        {ok, Policy, ControllerTimeout, MaxHeapWords} ->
            TargetTimeout = ControllerTimeout - ?TARGET_MARGIN_MS,
            case TargetTimeout > 0 of
                true ->
                    run_worker(Controller, Command, Request, Policy, TargetTimeout, MaxHeapWords);
                false ->
                    error_result(target_timeout)
            end;
        error ->
            error_result(invalid_request)
    end;
dispatch(_Controller, _Command, _Request, _Options) ->
    error_result(invalid_request).

-spec normalize(term(), redact | include) -> {ok, term()} | {error, atom()}.
normalize(Term, Policy) when Policy =:= redact; Policy =:= include ->
    case normalize_value(Term, Policy, 0, #{ids => #{}, counts => #{}}) of
        {ok, Value, _State} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    end;
normalize(_Term, _Policy) ->
    {error, invalid_identifier_policy}.

-spec truncate(term()) -> {ok, term()} | {error, atom()}.
truncate(Response) ->
    case evidence_pointers(Response) of
        {ok, Pointers} ->
            case pointers_exist(Response, Pointers) of
                true -> truncate_to_cap(Response, Pointers);
                false -> {error, invalid_evidence_pointer}
            end;
        error ->
            {error, invalid_evidence_pointer}
    end.

dispatch_options(Options) when is_map(Options) ->
    Policy = maps:get(identifier_policy, Options, include),
    Timeout = maps:get(timeout_ms, Options, 10000),
    MaxHeapWords = maps:get(max_heap_words, Options, ?MAX_HEAP_WORDS),
    case
        (Policy =:= redact orelse Policy =:= include) andalso
            is_integer(Timeout) andalso Timeout > 0 andalso
            is_integer(MaxHeapWords) andalso MaxHeapWords >= 1024 andalso
            MaxHeapWords =< ?MAX_HEAP_WORDS
    of
        true -> {ok, Policy, Timeout, MaxHeapWords};
        false -> error
    end;
dispatch_options(_Options) ->
    error.

run_worker(Controller, Command, Request, Policy, TargetTimeout, MaxHeapWords) ->
    OldTrapExit = process_flag(trap_exit, true),
    ControllerRef = erlang:monitor(process, Controller),
    RunRef = make_ref(),
    Coordinator = self(),
    Deadline = erlang:monotonic_time(millisecond) + TargetTimeout,
    try
        {Worker, WorkerRef} = spawn_opt(
            fun() ->
                worker(Coordinator, Controller, RunRef, Command, Request, Policy, Deadline)
            end,
            [
                link,
                monitor,
                {max_heap_size, #{size => MaxHeapWords, kill => true, error_logger => false}}
            ]
        ),
        try
            coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline)
        catch
            _InnerClass:_InnerReason:_InnerStacktrace ->
                stop_worker(Worker, WorkerRef, internal_error)
        after
            drain_exit(Worker)
        end
    catch
        _OuterClass:_OuterReason:_OuterStacktrace -> error_result(internal_error)
    after
        erlang:demonitor(ControllerRef, [flush]),
        process_flag(trap_exit, OldTrapExit)
    end.

coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline) ->
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected);
        {RunRef, Worker, {ok, Result}} ->
            Response = success_result(Result),
            case
                json_safe(Response) andalso erlang:external_size(Response) =< ?MAX_RESPONSE_BYTES
            of
                true ->
                    finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result);
                false ->
                    stop_worker(Worker, WorkerRef, invalid_schema)
            end;
        {RunRef, Worker, {error, Reason}} when is_atom(Reason) ->
            stop_worker(Worker, WorkerRef, Reason);
        {'DOWN', WorkerRef, process, Worker, Reason} ->
            worker_down(WorkerRef, Reason);
        {'EXIT', Worker, _Reason} ->
            coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, target_timeout)
    end.

finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result) ->
    Worker ! {RunRef, finish},
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected);
        {'DOWN', WorkerRef, process, Worker, normal} ->
            success_result(Result);
        {'DOWN', WorkerRef, process, Worker, _Reason} ->
            error_result(cleanup_unconfirmed);
        {'EXIT', Worker, _Reason} ->
            finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, cleanup_unconfirmed)
    end.

stop_worker(Worker, WorkerRef, Reason) ->
    exit(Worker, kill),
    receive
        {'DOWN', WorkerRef, process, Worker, _WorkerReason} -> error_result(Reason)
    after ?TARGET_MARGIN_MS ->
        error_result(cleanup_unconfirmed)
    end.

worker_down(WorkerRef, Reason) ->
    erlang:demonitor(WorkerRef, [flush]),
    error_result(
        case Reason of
            killed -> worker_heap_limit_exceeded;
            _ -> probe_failed
        end
    ).

worker(Coordinator, Controller, RunRef, Command, Request, Policy, Deadline) ->
    Outcome =
        try
            probe(Command, Request, #{
                deadline => Deadline, controller => Controller, coordinator => Coordinator
            })
        of
            {probe_error, Reason} when is_atom(Reason) ->
                {error, Reason};
            Raw ->
                case normalize(Raw, Policy) of
                    {ok, Normalized} -> truncate(Normalized);
                    {error, Reason} -> {error, Reason}
                end
        catch
            _Class:_Reason:_Stacktrace -> {error, probe_failed}
        end,
    Coordinator ! {RunRef, self(), Outcome},
    receive
        {RunRef, finish} -> ok
    end.

-ifdef(TEST).
probe(test_echo, Request, _Context) ->
    Request;
probe(test_observed_echo, {Observer, Request}, _Context) ->
    Observer ! {test_worker, self()},
    Request;
probe(test_timeout, Observer, _Context) ->
    Observer ! {test_worker, self()},
    receive
        stop -> ok
    end;
probe(test_crash, Observer, _Context) ->
    Observer ! {test_worker, self()},
    erlang:error({fixture_secret, Observer});
probe(test_heap, Observer, _Context) ->
    Observer ! {test_worker, self()},
    lists:seq(1, 1000000);
probe(snapshot, Request, Context) ->
    capture_snapshot(Request, Context);
probe(memory, Request, Context) ->
    capture_memory(Request, Context);
probe(schedulers, Request, Context) ->
    capture_schedulers(Request, Context);
probe(distribution, Request, Context) ->
    capture_distribution(Request, Context);
probe(processes, Request, Context) ->
    capture_processes(Request, Context);
probe(process, Request, Context) ->
    capture_process(Request, Context);
probe(applications, Request, Context) ->
    capture_applications(Request, Context);
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-else.
probe(snapshot, Request, Context) ->
    capture_snapshot(Request, Context);
probe(memory, Request, Context) ->
    capture_memory(Request, Context);
probe(schedulers, Request, Context) ->
    capture_schedulers(Request, Context);
probe(distribution, Request, Context) ->
    capture_distribution(Request, Context);
probe(processes, Request, Context) ->
    capture_processes(Request, Context);
probe(process, Request, Context) ->
    capture_process(Request, Context);
probe(applications, Request, Context) ->
    capture_applications(Request, Context);
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-endif.

capture_snapshot(Request, #{deadline := Deadline, controller := Controller}) when is_map(Request) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = code:is_loaded(?MODULE) =/= false,
    Probes = [
        run_snapshot_probe(runtime, true, fun runtime_probe/0, Request, Deadline),
        run_snapshot_probe(resources, true, fun resources_probe/0, Request, Deadline),
        run_snapshot_probe(memory, true, fun memory_probe/0, Request, Deadline),
        run_snapshot_probe(schedulers, false, fun schedulers_probe/0, Request, Deadline),
        run_snapshot_probe(
            distribution,
            false,
            fun() -> distribution_probe(Controller) end,
            Request,
            Deadline
        )
    ],
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    ProbeReports = [Report || {Report, _Data} <- Probes],
    #{
        schema => <<"observer_cli.cli/v1">>,
        command => snapshot,
        target => target_from_probes(Probes),
        capture => #{
            status => capture_status(ProbeReports),
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(FinishedAt),
            duration_ms => FinishedMonotonic - StartedMonotonic,
            probes => ProbeReports,
            observer_effects => observer_effects(ModuleLoaded, Controller)
        },
        data => snapshot_data(Probes),
        warnings => probe_warnings(ProbeReports),
        errors => probe_errors(ProbeReports)
    };
capture_snapshot(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_memory(Request, Context) when is_map(Request) ->
    capture_inspection(memory, memory, 1, Context, fun() ->
        {ok, Runtime, _} = runtime_probe(),
        {ok, Memory, _Coverage} = memory_probe(),
        {Runtime, #{runtime => Runtime, memory => maps:with([beam, persistent_term], Memory)},
            [target_identity, otp_runtime, beam_memory, persistent_term_summary], []}
    end);
capture_memory(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_schedulers(#{duration_ms := Duration}, Context) when
    is_integer(Duration), Duration >= 250, Duration =< 10000
->
    capture_inspection(schedulers, scheduler_wall_time, 2, Context, fun() ->
        {ok, Runtime, _} = runtime_probe(),
        Window = measure_scheduler(
            Duration,
            fun(Enabled) -> erlang:system_flag(scheduler_wall_time, Enabled) end,
            fun scheduler_sample/0,
            fun timer:sleep/1
        ),
        {Runtime, Window, [scheduler_wall_time, scheduler_topology, run_queue_non_atomic], [
            #{
                id => scheduler_wall_time,
                temporary_enable => true,
                observer_contaminated => true
            }
        ]}
    end);
capture_schedulers(_Request, _Context) ->
    {probe_error, invalid_duration}.

capture_distribution(Request, #{controller := Controller} = Context) when is_map(Request) ->
    Limit = maps:get(limit, Request, 20),
    case is_integer(Limit) andalso Limit >= 1 andalso Limit =< 200 of
        true ->
            capture_inspection(distribution, distribution, 1, Context, fun() ->
                {ok, Runtime, _} = runtime_probe(),
                {ok, Distribution, Coverage} = distribution_probe(Controller, Limit),
                {Runtime, Distribution, Coverage, []}
            end);
        false ->
            {probe_error, invalid_limit}
    end;
capture_distribution(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_processes(Request, Context) when is_map(Request) ->
    Sort = maps:get(sort, Request, memory),
    Limit = maps:get(limit, Request, 20),
    Duration = maps:get(duration_ms, Request, undefined),
    case valid_process_request(Sort, Limit, Duration) of
        true ->
            Source = process_source(Request),
            Samples =
                case Duration of
                    undefined -> 1;
                    _ -> 2
                end,
            Keys = process_inventory_keys(Sort),
            Retained =
                case Duration of
                    undefined -> {top, Limit};
                    _ -> all
                end,
            case admit_process_scan(Source, Sort, length(Keys), Samples, Retained) of
                {ok, Admission} ->
                    capture_scan_inspection(
                        processes,
                        process_inventory,
                        Samples,
                        Context,
                        fun() ->
                            collect_processes(Source, Sort, Limit, Duration, Context, Admission)
                        end
                    );
                {unavailable, Details} ->
                    capture_scan_inspection(
                        processes,
                        process_inventory,
                        0,
                        Context,
                        fun() -> {unavailable, scan_budget_exceeded, Details} end
                    )
            end;
        false ->
            {probe_error, invalid_request}
    end;
capture_processes(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_process(#{target := Target} = Request, Context) ->
    Source = process_source(Request),
    capture_scan_inspection(
        process, process_info, 1, Context, fun() -> collect_process(Target, Source) end
    );
capture_process(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_applications(Request, Context) when is_map(Request) ->
    Sort = maps:get(sort, Request, memory),
    Limit = maps:get(limit, Request, 20),
    case valid_application_request(Sort, Limit) of
        true ->
            AppSource = application_source(Request),
            ProcessSource = process_source(Request),
            capture_scan_inspection(
                applications,
                application_inventory,
                1,
                Context,
                fun() -> collect_applications(AppSource, ProcessSource, Sort, Limit, Context) end
            );
        false ->
            {probe_error, invalid_request}
    end;
capture_applications(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_scan_inspection(Command, ProbeId, Samples, #{controller := Controller}, OutcomeFun) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = code:is_loaded(?MODULE) =/= false,
    Outcome = OutcomeFun(),
    {Status, Reason, Data, Coverage} =
        case Outcome of
            {ok, Value, Covered} -> {ok, null, Value, Covered};
            {unavailable, Why, Details} -> {unavailable, Why, Details, [admission_only]}
        end,
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    {ok, Runtime, _} = runtime_probe(),
    #{
        schema => <<"observer_cli.cli/v1">>,
        command => Command,
        target => target_from_runtime(Runtime),
        capture => #{
            status => complete,
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(FinishedAt),
            duration_ms => FinishedMonotonic - StartedMonotonic,
            probes => [
                probe_report(
                    ProbeId,
                    true,
                    Status,
                    Reason,
                    FinishedMonotonic - StartedMonotonic,
                    Samples,
                    Coverage
                )
            ],
            observer_effects => observer_effects(ModuleLoaded, Controller)
        },
        data => Data,
        warnings => [],
        errors => []
    }.

valid_process_request(Sort, Limit, Duration) ->
    lists:member(Sort, [memory, message_queue_len, reductions, binary_memory, total_heap_size]) andalso
        is_integer(Limit) andalso Limit >= 1 andalso Limit =< 200 andalso
        ((Duration =:= undefined) orelse
            (Sort =:= reductions andalso is_integer(Duration) andalso Duration >= 250 andalso
                Duration =< 10000)).

valid_application_request(Sort, Limit) ->
    lists:member(Sort, [memory, process_count, reductions, message_queue_len]) andalso
        is_integer(Limit) andalso Limit >= 1 andalso Limit =< 200.

process_inventory_keys(binary_memory) ->
    [registered_name, current_function, initial_call, memory, binary];
process_inventory_keys(Sort) ->
    lists:usort([registered_name, current_function, initial_call, memory, Sort]).

application_process_keys() ->
    [memory, message_queue_len, reductions, group_leader].

process_detail_keys() ->
    [
        registered_name,
        status,
        current_function,
        initial_call,
        memory,
        message_queue_len,
        reductions,
        heap_size,
        total_heap_size,
        stack_size,
        group_leader,
        garbage_collection_info
    ].

admit_process_scan(Source, Sort, TrackedFields, Samples, Retained) ->
    Count = (maps:get(count_fun, Source))(),
    Budget =
        case Sort of
            binary_memory -> ?BINARY_PROCESS_SCAN_BUDGET;
            _ -> ?PROCESS_SCAN_BUDGET
        end,
    RetainedCount =
        case Retained of
            all -> Count;
            {top, Limit} -> min(Count, Limit)
        end,
    Estimated = working_set_estimate(RetainedCount, TrackedFields, Samples),
    Details = #{
        status => unavailable,
        reason_code => scan_budget_exceeded,
        admission_stage => pre_enumeration,
        observed_count_including_observer => Count,
        scan_budget_count => Budget,
        tracked_field_count => TrackedFields,
        retained_sample_count => Samples,
        working_set_estimated_bytes => Estimated,
        working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
    },
    case Count =< Budget andalso Estimated =< ?MAX_WORKING_SET_BYTES of
        true -> {ok, Details#{status => admitted, reason_code => null}};
        false -> {unavailable, Details}
    end.

working_set_estimate(Count, Fields, Samples) ->
    Count * Fields * Samples * ?WORKING_SET_BYTES_PER_FIELD.

collect_processes(Source, Sort, Limit, undefined, Context, Admission) ->
    Keys = process_inventory_keys(Sort),
    Started = erlang:monotonic_time(millisecond),
    Initial = inventory_acc(Context, Limit),
    Acc = fold_processes(
        Source,
        fun(Pid, State) ->
            scan_ranked_process(Pid, Keys, Sort, Source, State)
        end,
        Initial
    ),
    Finished = erlang:monotonic_time(millisecond),
    Items = [public_process_item(Item) || Item <- maps:get(top, Acc)],
    Eligible = maps:get(eligible, Acc),
    Data = (audit_inventory(Acc, length(Items), Started, Finished))#{
        items => Items,
        dropped_count => Eligible - length(Items),
        sort => Sort,
        sort_semantics => total,
        baseline_count => 0,
        tracked_field_count => maps:get(tracked_field_count, Admission),
        retained_sample_count => 1,
        working_set_estimated_bytes => maps:get(working_set_estimated_bytes, Admission)
    },
    {ok, Data, [
        exact_top_n, stable_raw_pid_tie_break, explicit_process_info_keys, process_scan_admitted
    ]};
collect_processes(Source, reductions, Limit, Duration, Context, Admission) ->
    Started = erlang:monotonic_time(millisecond),
    First = collect_reduction_sample(Source, Context),
    (maps:get(sleep_fun, Source))(Duration),
    Second = collect_reduction_sample(Source, Context),
    Finished = erlang:monotonic_time(millisecond),
    Interval = max(1, maps:get(monotonic_ms, Second) - maps:get(monotonic_ms, First)),
    Window = stable_process_window(
        maps:get(values, First), maps:get(values, Second), Interval
    ),
    Ranked = rank_window(maps:get(stable, Window), Limit),
    Items = [window_process_item(Pid, Delta, Interval) || {Pid, Delta} <- Ranked],
    FirstAudit = maps:get(audit, First),
    SecondAudit = maps:get(audit, Second),
    Data = (audit_inventory(SecondAudit, length(Items), Started, Finished))#{
        items => Items,
        dropped_count => maps:size(maps:get(stable, Window)) - length(Items),
        sort => reductions,
        sort_semantics => delta,
        interval_ms => Interval,
        baseline_count => maps:size(maps:get(values, First)),
        born_count => length(maps:get(born, Window)),
        dead_count => length(maps:get(dead, Window)),
        reset_count => length(maps:get(reset, Window)),
        born_pids => [{identifier, pid, Pid} || Pid <- maps:get(born, Window)],
        dead_pids => [{identifier, pid, Pid} || Pid <- maps:get(dead, Window)],
        reset_pids => [{identifier, pid, Pid} || Pid <- maps:get(reset, Window)],
        baseline_exclusions => maps:get(exclusions, FirstAudit),
        tracked_field_count => maps:get(tracked_field_count, Admission),
        retained_sample_count => 2,
        working_set_estimated_bytes => maps:get(working_set_estimated_bytes, Admission)
    },
    {ok, Data, [
        exact_stable_pid_top_n,
        lifecycle_accounting,
        counter_reset_accounting,
        explicit_process_info_keys,
        process_scan_admitted
    ]}.

inventory_acc(Context, Limit) ->
    #{
        scanned => 0,
        eligible => 0,
        disappeared => 0,
        exclusions => [],
        excluded_pids => excluded_processes(Context),
        limit => Limit,
        top => []
    }.

scan_ranked_process(Pid, Keys, Sort, Source, Acc0) ->
    case scan_process(Pid, Keys, Source, Acc0) of
        {ok, Item, Acc} ->
            Acc#{top := insert_top(Item, Sort, maps:get(limit, Acc), maps:get(top, Acc))};
        {skip, Acc} ->
            Acc
    end.

scan_process(Pid, Keys, Source, Acc0) ->
    Acc1 = Acc0#{scanned := maps:get(scanned, Acc0) + 1},
    case maps:find(Pid, maps:get(excluded_pids, Acc1)) of
        {ok, Reason} ->
            {skip, Acc1#{
                exclusions := [
                    #{pid => {identifier, pid, Pid}, reason => Reason}
                    | maps:get(exclusions, Acc1)
                ]
            }};
        error ->
            Acc2 = Acc1#{eligible := maps:get(eligible, Acc1) + 1},
            case (maps:get(info_fun, Source))(Pid, Keys) of
                undefined ->
                    {skip, Acc2#{disappeared := maps:get(disappeared, Acc2) + 1}};
                Info when is_list(Info) ->
                    {ok, process_item(Pid, Info), Acc2}
            end
    end.

insert_top(Item, Sort, Limit, Items) ->
    lists:sublist(insert_ranked(Item, Sort, Items), Limit).

insert_ranked(Item, _Sort, []) ->
    [Item];
insert_ranked(Item, Sort, [Head | Rest] = Items) ->
    case process_precedes(Item, Head, Sort) of
        true -> [Item | Items];
        false -> [Head | insert_ranked(Item, Sort, Rest)]
    end.

process_precedes(Left, Right, Sort) ->
    LeftMetric = maps:get(Sort, Left),
    RightMetric = maps:get(Sort, Right),
    LeftMetric > RightMetric orelse
        (LeftMetric =:= RightMetric andalso maps:get(raw_pid, Left) < maps:get(raw_pid, Right)).

process_item(Pid, Info) ->
    WordSize = erlang:system_info(wordsize),
    Base = #{raw_pid => Pid, pid => {identifier, pid, Pid}},
    lists:foldl(fun({Key, Value}, Acc) -> process_field(Key, Value, WordSize, Acc) end, Base, Info).

process_field(registered_name, [], _WordSize, Acc) ->
    Acc#{registered_name => null};
process_field(registered_name, Name, _WordSize, Acc) ->
    Acc#{registered_name => {identifier, name, Name}};
process_field(current_function, {M, F, A}, _WordSize, Acc) ->
    Acc#{current_function => {mfa, M, F, A}};
process_field(initial_call, {M, F, A}, _WordSize, Acc) ->
    Acc#{initial_call => {mfa, M, F, A}};
process_field(memory, Value, _WordSize, Acc) ->
    Acc#{memory => Value, memory_bytes => Value};
process_field(message_queue_len, Value, _WordSize, Acc) ->
    Acc#{message_queue_len => Value};
process_field(reductions, Value, _WordSize, Acc) ->
    Acc#{reductions => Value};
process_field(heap_size, Value, WordSize, Acc) ->
    Acc#{heap_size_bytes => Value * WordSize};
process_field(total_heap_size, Value, WordSize, Acc) ->
    Acc#{total_heap_size => Value * WordSize, total_heap_size_bytes => Value * WordSize};
process_field(stack_size, Value, WordSize, Acc) ->
    Acc#{stack_size_bytes => Value * WordSize};
process_field(group_leader, Value, _WordSize, Acc) ->
    Acc#{group_leader => Value};
process_field(status, Value, _WordSize, Acc) ->
    Acc#{status => Value};
process_field(garbage_collection_info, Value, _WordSize, Acc) ->
    Acc#{garbage_collection_info => allowed_gc_info(Value)};
process_field(binary, Binaries, _WordSize, Acc) ->
    Acc#{binary_memory => binary_memory(Binaries)}.

binary_memory(Binaries) ->
    lists:sum([Size || {_Ref, Size, _RefCount} <- Binaries, is_integer(Size), Size >= 0]).

allowed_gc_info(Info) ->
    Allowed = [
        old_heap_block_size,
        heap_block_size,
        mbuf_size,
        recent_size,
        stack_size,
        old_heap_size,
        heap_size,
        bin_vheap_size,
        bin_vheap_block_size,
        bin_old_vheap_size,
        bin_old_vheap_block_size
    ],
    maps:from_list([
        {Key, Value}
     || {Key, Value} <- Info,
        lists:member(Key, Allowed),
        (is_integer(Value) andalso Value >= 0) orelse is_boolean(Value)
    ]).

public_process_item(Item) ->
    Public = maps:without([raw_pid, memory, total_heap_size, group_leader], Item),
    case maps:find(binary_memory, Public) of
        {ok, Bytes} -> maps:remove(binary_memory, Public#{binary_memory_bytes => Bytes});
        error -> Public
    end.

audit_inventory(Acc, Returned, Started, Finished) ->
    #{
        inventory_path => maps:get(path, Acc, unknown),
        scanned_count => maps:get(scanned, Acc),
        eligible_count => maps:get(eligible, Acc),
        returned_count => Returned,
        disappeared_count => maps:get(disappeared, Acc),
        exclusion_count => length(maps:get(exclusions, Acc)),
        exclusions => lists:reverse(maps:get(exclusions, Acc)),
        complete => true,
        scan_started_monotonic_ms => Started,
        scan_finished_monotonic_ms => Finished
    }.

collect_reduction_sample(Source, Context) ->
    Acc0 = (inventory_acc(Context, 1))#{values => #{}},
    Acc = fold_processes(
        Source,
        fun(Pid, State) ->
            case scan_process(Pid, [reductions], Source, State) of
                {ok, #{reductions := Reductions}, Next} ->
                    Next#{values := (maps:get(values, Next))#{Pid => Reductions}};
                {skip, Next} ->
                    Next
            end
        end,
        Acc0
    ),
    #{
        values => maps:get(values, Acc),
        audit => Acc,
        monotonic_ms => (maps:get(monotonic_fun, Source))()
    }.

stable_process_window(First, Second, _Interval) ->
    FirstPids = maps:keys(First),
    SecondPids = maps:keys(Second),
    StablePids = ordsets:intersection(lists:sort(FirstPids), lists:sort(SecondPids)),
    {Stable, Reset} = lists:foldl(
        fun(Pid, {Values, Resets}) ->
            Before = maps:get(Pid, First),
            After = maps:get(Pid, Second),
            case After >= Before of
                true -> {Values#{Pid => After - Before}, Resets};
                false -> {Values, [Pid | Resets]}
            end
        end,
        {#{}, []},
        StablePids
    ),
    #{
        stable => Stable,
        born => ordsets:subtract(lists:sort(SecondPids), lists:sort(FirstPids)),
        dead => ordsets:subtract(lists:sort(FirstPids), lists:sort(SecondPids)),
        reset => lists:sort(Reset)
    }.

rank_window(Values, Limit) ->
    lists:sublist(
        lists:sort(
            fun({PidA, ValueA}, {PidB, ValueB}) ->
                ValueA > ValueB orelse (ValueA =:= ValueB andalso PidA < PidB)
            end,
            maps:to_list(Values)
        ),
        Limit
    ).

window_process_item(Pid, Delta, Interval) ->
    #{
        pid => {identifier, pid, Pid},
        reductions_delta => Delta,
        reductions_per_second => Delta * 1000 / Interval
    }.

collect_process(Target, Source) ->
    case resolve_process_target(Target, Source) of
        {ok, Pid} ->
            case (maps:get(info_fun, Source))(Pid, process_detail_keys()) of
                undefined ->
                    {ok, #{status => not_found}, [
                        target_side_resolution, explicit_process_info_keys
                    ]};
                Info ->
                    RawItem = process_item(Pid, Info),
                    Item0 = public_process_item(RawItem),
                    Item =
                        case maps:find(group_leader, RawItem) of
                            {ok, Leader} -> Item0#{group_leader => {identifier, pid, Leader}};
                            error -> Item0
                        end,
                    {ok, Item#{status => maps:get(status, Item, unknown)}, [
                        target_side_resolution, explicit_process_info_keys, fixed_gc_allowlist
                    ]}
            end;
        not_found ->
            {ok, #{status => not_found}, [target_side_resolution, no_atom_creation]}
    end.

resolve_process_target(Target, Source) ->
    case target_binary(Target) of
        {ok, <<"<", _/binary>> = Text} -> resolve_pid_text(Text, Source);
        {ok, Name} -> resolve_registered_name(Name, Source);
        error -> not_found
    end.

target_binary(Target) when is_list(Target), length(Target) =< 255 ->
    target_binary(unicode:characters_to_binary(Target));
target_binary(Target) when is_binary(Target), byte_size(Target) =< 255 ->
    case unicode:characters_to_binary(Target) of
        Target -> {ok, Target};
        _ -> error
    end;
target_binary(_Target) ->
    error.

resolve_pid_text(Text, Source) ->
    case re:run(Text, <<"^<0\\.[0-9]+\\.[0-9]+>$">>, [{capture, none}]) of
        match ->
            try list_to_pid(binary_to_list(Text)) of
                Pid -> live_local_pid(Pid, Source)
            catch
                error:badarg -> not_found
            end;
        nomatch ->
            not_found
    end.

resolve_registered_name(Name, Source) ->
    try binary_to_existing_atom(Name, utf8) of
        Atom ->
            case (maps:get(whereis_fun, Source))(Atom) of
                Pid when is_pid(Pid) -> live_local_pid(Pid, Source);
                _ -> not_found
            end
    catch
        error:badarg -> not_found
    end.

live_local_pid(Pid, Source) ->
    case node(Pid) =:= node() andalso (maps:get(alive_fun, Source))(Pid) of
        true -> {ok, Pid};
        false -> not_found
    end.

collect_applications(AppSource, ProcessSource, Sort, Limit, Context) ->
    Loaded = (maps:get(loaded_fun, AppSource))(),
    Running = (maps:get(running_fun, AppSource))(remaining(maps:get(deadline, Context))),
    Apps = lists:usort([App || {App, _, _} <- Loaded] ++ [App || {App, _, _} <- Running]),
    AppEstimate = working_set_estimate(length(Apps), 4, 1),
    case length(Apps) =< ?APPLICATION_SCAN_BUDGET andalso AppEstimate =< ?MAX_WORKING_SET_BYTES of
        false ->
            {unavailable, scan_budget_exceeded, #{
                status => unavailable,
                reason_code => scan_budget_exceeded,
                admission_stage => post_enumeration,
                observed_application_count => length(Apps),
                scan_budget_count => ?APPLICATION_SCAN_BUDGET,
                working_set_estimated_bytes => AppEstimate,
                working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
            }};
        true ->
            collect_admitted_applications(
                Apps, Loaded, Running, AppSource, ProcessSource, Sort, Limit, Context, AppEstimate
            )
    end.

collect_admitted_applications(
    Apps, Loaded, Running, AppSource, ProcessSource, Sort, Limit, Context, AppEstimate
) ->
    case
        admit_process_scan(
            ProcessSource, memory, length(application_process_keys()), 1, all
        )
    of
        {unavailable, Details} ->
            {unavailable, scan_budget_exceeded, Details#{
                admission_stage => pre_process_enumeration
            }};
        {ok, ProcessAdmission} ->
            LeaderApps = application_leaders(Apps, AppSource),
            Acc0 = (inventory_acc(Context, 1))#{items => []},
            ProcessStarted = erlang:monotonic_time(millisecond),
            Acc = fold_processes(
                ProcessSource,
                fun(Pid, State) ->
                    case scan_process(Pid, application_process_keys(), ProcessSource, State) of
                        {ok, Item, Next} -> Next#{items := [Item | maps:get(items, Next)]};
                        {skip, Next} -> Next
                    end
                end,
                Acc0
            ),
            ProcessFinished = erlang:monotonic_time(millisecond),
            {Stats, Unattributed} = application_stats(maps:get(items, Acc), LeaderApps),
            RunningSet = maps:from_keys([App || {App, _, _} <- Running], true),
            LoadedSet = maps:from_keys([App || {App, _, _} <- Loaded], true),
            Items0 = [application_item(App, Stats, LoadedSet, RunningSet) || App <- Apps],
            RankedItems = lists:sublist(
                lists:sort(fun(A, B) -> application_precedes(A, B, Sort) end, Items0), Limit
            ),
            Items = [maps:remove(memory, Item) || Item <- RankedItems],
            Audit = audit_inventory(Acc, length(Items), ProcessStarted, ProcessFinished),
            Data = Audit#{
                items => Items,
                dropped_count => length(Items0) - length(Items),
                application_count => length(Apps),
                admission_stage => post_enumeration,
                attribution => group_leader_application,
                attribution_semantics => approximation,
                unattributed_process_count => Unattributed,
                baseline_count => 0,
                tracked_field_count => 4,
                retained_sample_count => 1,
                working_set_estimated_bytes => AppEstimate +
                    maps:get(working_set_estimated_bytes, ProcessAdmission)
            },
            {ok, Data, [
                public_application_inventory,
                shared_process_inventory,
                group_leader_application_approximation,
                process_scan_admitted
            ]}
    end.

application_leaders(Apps, Source) ->
    lists:foldl(
        fun(App, Acc) ->
            case (maps:get(supervisor_fun, Source))(App) of
                {ok, Root} when is_pid(Root) ->
                    case (maps:get(root_info_fun, Source))(Root, group_leader) of
                        {group_leader, Leader} when is_pid(Leader), node(Leader) =:= node() ->
                            Acc#{Leader => App};
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        Apps
    ).

application_stats(Items, Leaders) ->
    lists:foldl(
        fun(Item, {Stats, Unknown}) ->
            case maps:find(maps:get(group_leader, Item), Leaders) of
                {ok, App} ->
                    Current = maps:get(App, Stats, empty_application_stats()),
                    {
                        Stats#{
                            App => Current#{
                                process_count := maps:get(process_count, Current) + 1,
                                memory := maps:get(memory, Current) + maps:get(memory, Item),
                                reductions := maps:get(reductions, Current) +
                                    maps:get(reductions, Item),
                                message_queue_len := maps:get(message_queue_len, Current) +
                                    maps:get(message_queue_len, Item)
                            }
                        },
                        Unknown
                    };
                error ->
                    {Stats, Unknown + 1}
            end
        end,
        {#{}, 0},
        Items
    ).

empty_application_stats() ->
    #{process_count => 0, memory => 0, reductions => 0, message_queue_len => 0}.

application_item(App, Stats, Loaded, Running) ->
    Values = maps:get(App, Stats, empty_application_stats()),
    Values#{
        application => {identifier, application, App},
        memory_bytes => maps:get(memory, Values),
        loaded => maps:is_key(App, Loaded),
        running => maps:is_key(App, Running)
    }.

application_precedes(A, B, Sort) ->
    AValue = maps:get(Sort, A),
    BValue = maps:get(Sort, B),
    AValue > BValue orelse
        (AValue =:= BValue andalso maps:get(application, A) < maps:get(application, B)).

excluded_processes(Context) ->
    Base = #{self() => diagnostics_worker},
    WithCoordinator = maybe_exclude_pid(
        maps:get(coordinator, Context, undefined), diagnostics_coordinator, Base
    ),
    maybe_exclude_pid(
        maps:get(controller, Context, undefined), diagnostics_controller, WithCoordinator
    ).

maybe_exclude_pid(Pid, Reason, Acc) when is_pid(Pid), node(Pid) =:= node() -> Acc#{Pid => Reason};
maybe_exclude_pid(_Pid, _Reason, Acc) -> Acc.

fold_processes(Source, Fun, Acc) ->
    {Path, FoldFun} = maps:get(fold, Source),
    (FoldFun)(Fun, Acc#{path => Path}).

process_source(Request) ->
    process_source_test(Request, default_process_source()).

-ifdef(TEST).
process_source_test(#{test_process_source := Source}, _Default) -> Source;
process_source_test(_Request, Default) -> Default.
-else.
process_source_test(_Request, Default) -> Default.
-endif.

default_process_source() ->
    #{
        count_fun => fun() -> erlang:system_info(process_count) end,
        fold => process_fold(),
        info_fun => fun erlang:process_info/2,
        sleep_fun => fun timer:sleep/1,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end,
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    }.

process_fold() ->
    case
        erlang:function_exported(erlang, processes_iterator, 0) andalso
            erlang:function_exported(erlang, processes_next, 1)
    of
        true -> {otp_process_iterator, fun iterator_fold/2};
        false -> {bounded_process_list, fun list_process_fold/2}
    end.

iterator_fold(Fun, Acc) -> iterator_fold(erlang:processes_iterator(), Fun, Acc).

iterator_fold(Iterator, Fun, Acc) ->
    case erlang:processes_next(Iterator) of
        none -> Acc;
        {Pid, Next} -> iterator_fold(Next, Fun, Fun(Pid, Acc))
    end.

list_process_fold(Fun, Acc) -> lists:foldl(Fun, Acc, erlang:processes()).

application_source(Request) ->
    application_source_test(Request, default_application_source()).

-ifdef(TEST).
application_source_test(#{test_application_source := Source}, _Default) -> Source;
application_source_test(_Request, Default) -> Default.
-else.
application_source_test(_Request, Default) -> Default.
-endif.

default_application_source() ->
    #{
        loaded_fun => fun application:loaded_applications/0,
        running_fun => fun application:which_applications/1,
        supervisor_fun => fun application:get_supervisor/1,
        root_info_fun => fun erlang:process_info/2
    }.

capture_inspection(Command, ProbeId, Samples, #{controller := Controller}, Fun) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = code:is_loaded(?MODULE) =/= false,
    {Runtime, Data, Coverage, ExtraEffects} = Fun(),
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    #{
        schema => <<"observer_cli.cli/v1">>,
        command => Command,
        target => target_from_runtime(Runtime),
        capture => #{
            status => complete,
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(FinishedAt),
            duration_ms => FinishedMonotonic - StartedMonotonic,
            probes => [
                probe_report(
                    ProbeId, true, ok, null, FinishedMonotonic - StartedMonotonic, Samples, Coverage
                )
            ],
            observer_effects => observer_effects(ModuleLoaded, Controller) ++ ExtraEffects
        },
        data => Data,
        warnings => [],
        errors => []
    }.

run_snapshot_probe(Id, Required, Fun, Request, Deadline) ->
    Started = erlang:monotonic_time(millisecond),
    Outcome =
        case Started < Deadline of
            true -> snapshot_probe_outcome(Id, Fun, Request);
            false -> {timeout, target_timeout}
        end,
    Finished = erlang:monotonic_time(millisecond),
    probe_result(Id, Required, Outcome, Finished - Started).

-ifdef(TEST).
snapshot_probe_outcome(Id, Fun, #{test_probe_outcomes := Outcomes}) ->
    case maps:find(Id, Outcomes) of
        {ok, Outcome} -> Outcome;
        error -> call_snapshot_probe(Fun)
    end;
snapshot_probe_outcome(_Id, Fun, _Request) ->
    call_snapshot_probe(Fun).
-else.
snapshot_probe_outcome(_Id, Fun, _Request) ->
    call_snapshot_probe(Fun).
-endif.

call_snapshot_probe(Fun) ->
    try Fun() of
        {ok, _Data, _Coverage} = Result -> Result;
        {unavailable, _Reason} = Result -> Result;
        _Invalid -> {error, invalid_probe_result}
    catch
        error:badarg -> {unavailable, capability_unavailable};
        _Class:_Reason:_Stacktrace -> {error, probe_failed}
    end.

probe_result(Id, Required, {ok, Data, Coverage}, Duration) ->
    {probe_report(Id, Required, ok, null, Duration, 1, Coverage), Data};
probe_result(Id, Required, {unavailable, Reason}, Duration) ->
    {probe_report(Id, Required, unavailable, Reason, Duration, 0, []), undefined};
probe_result(Id, Required, {timeout, Reason}, Duration) ->
    {probe_report(Id, Required, timeout, Reason, Duration, 0, []), undefined};
probe_result(Id, Required, {error, Reason}, Duration) ->
    {probe_report(Id, Required, error, Reason, Duration, 0, []), undefined};
probe_result(Id, Required, _Invalid, Duration) ->
    {probe_report(Id, Required, error, invalid_probe_result, Duration, 0, []), undefined}.

probe_report(Id, Required, Status, Reason, Duration, Samples, Coverage) ->
    #{
        id => Id,
        required => Required,
        status => Status,
        reason_code => Reason,
        duration_ms => Duration,
        samples => Samples,
        coverage => Coverage
    }.

runtime_probe() ->
    {ok,
        #{
            node => {identifier, node, node()},
            otp_release => text_system_info(otp_release),
            runtime_version => text_system_info(version),
            system_architecture => text_system_info(system_architecture),
            word_size_bytes => erlang:system_info(wordsize)
        },
        [target_identity, otp_runtime]}.

resources_probe() ->
    {ok,
        #{
            process => contaminated_count(process_count, process_limit),
            port => contaminated_count(port_count, port_limit),
            atom => contaminated_count(atom_count, atom_limit),
            ets => #{
                observed_count => erlang:system_info(ets_count),
                limit => erlang:system_info(ets_limit)
            }
        },
        [global_counts, no_resource_enumeration]}.

contaminated_count(CountKey, LimitKey) ->
    #{
        observed_count_including_observer => erlang:system_info(CountKey),
        limit => erlang:system_info(LimitKey),
        observer_contaminated => true
    }.

memory_probe() ->
    {{input, Input}, {output, Output}} = erlang:statistics(io),
    {Collections, ReclaimedWords, _} = erlang:statistics(garbage_collection),
    WordSize = erlang:system_info(wordsize),
    PersistentTerm = persistent_term:info(),
    {ok,
        #{
            beam => (memory_map(erlang:memory()))#{observer_contaminated => true},
            io => #{
                input_bytes_total => Input,
                output_bytes_total => Output,
                observer_contaminated => true
            },
            garbage_collection => #{
                collections_total => Collections,
                reclaimed_words_total => ReclaimedWords,
                reclaimed_bytes_total => ReclaimedWords * WordSize,
                observer_contaminated => true
            },
            persistent_term => #{
                count => maps:get(count, PersistentTerm),
                memory_bytes => maps:get(memory, PersistentTerm)
            }
        },
        [beam_memory, runtime_io, runtime_gc, persistent_term_summary]}.

memory_map(Memory) ->
    maps:from_list([{memory_key(Key), Value} || {Key, Value} <- Memory]).

memory_key(total) -> total_bytes;
memory_key(processes) -> processes_bytes;
memory_key(processes_used) -> processes_used_bytes;
memory_key(system) -> system_bytes;
memory_key(atom) -> atom_bytes;
memory_key(atom_used) -> atom_used_bytes;
memory_key(binary) -> binary_bytes;
memory_key(code) -> code_bytes;
memory_key(ets) -> ets_bytes.

schedulers_probe() ->
    SchedulersOnline = erlang:system_info(schedulers_online),
    RunQueueLengths = erlang:statistics(run_queue_lengths),
    {ok,
        #{
            schedulers_configured => erlang:system_info(schedulers),
            schedulers_online => SchedulersOnline,
            dirty_cpu_schedulers_configured => erlang:system_info(dirty_cpu_schedulers),
            dirty_cpu_schedulers_online => erlang:system_info(dirty_cpu_schedulers_online),
            dirty_io_schedulers => erlang:system_info(dirty_io_schedulers),
            run_queue_lengths => RunQueueLengths,
            normal_observed_runnable_count_including_observer =>
                lists:sum(lists:sublist(RunQueueLengths, SchedulersOnline)),
            dirty_cpu_observed_runnable_count_including_observer => lists:last(
                RunQueueLengths
            ),
            run_queue_snapshot_atomic => false,
            scheduler_wall_time_enabled_by_observer_cli => false,
            observer_contaminated => true
        },
        [scheduler_topology, run_queue_non_atomic]}.

measure_scheduler(Duration, FlagFun, SampleFun, SleepFun) ->
    _ = FlagFun(true),
    try
        First = SampleFun(),
        ok = SleepFun(Duration),
        Second = SampleFun(),
        scheduler_window(First, Second)
    after
        _ = FlagFun(false)
    end.

scheduler_sample() ->
    #{
        topology => scheduler_topology(),
        wall_time => erlang:statistics(scheduler_wall_time),
        run_queue_lengths => erlang:statistics(run_queue_lengths),
        monotonic_ms => erlang:monotonic_time(millisecond)
    }.

scheduler_topology() ->
    #{
        schedulers_configured => erlang:system_info(schedulers),
        schedulers_online => erlang:system_info(schedulers_online),
        dirty_cpu_schedulers_configured => erlang:system_info(dirty_cpu_schedulers),
        dirty_cpu_schedulers_online => erlang:system_info(dirty_cpu_schedulers_online)
    }.

scheduler_window(#{topology := Topology} = First, #{topology := Topology} = Second) ->
    case scheduler_window_data(Topology, First, Second) of
        {ok, Window} -> Window#{status => valid};
        {error, Reason} -> invalid_scheduler_window(Reason)
    end;
scheduler_window(#{topology := _}, #{topology := _}) ->
    invalid_scheduler_window(topology_changed);
scheduler_window(_First, _Second) ->
    invalid_scheduler_window(invalid_sample).

scheduler_window_data(Topology, First, Second) ->
    Schedulers = maps:get(schedulers_configured, Topology),
    SchedulersOnline = maps:get(schedulers_online, Topology),
    DirtyOnline = maps:get(dirty_cpu_schedulers_online, Topology),
    NormalIds = lists:seq(1, SchedulersOnline),
    DirtyIds = lists:seq(Schedulers + 1, Schedulers + DirtyOnline),
    with_wall_maps(First, Second, fun(FirstWall, SecondWall) ->
        case
            {
                pool_delta(NormalIds, FirstWall, SecondWall),
                pool_delta(DirtyIds, FirstWall, SecondWall),
                run_queue_window(SchedulersOnline, First, Second),
                interval_ms(First, Second)
            }
        of
            {{ok, Normal}, {ok, Dirty}, {ok, RunQueues}, {ok, Interval}} ->
                {ok, #{
                    interval_ms => Interval,
                    topology => Topology,
                    normal => Normal,
                    dirty_cpu => Dirty,
                    run_queues => RunQueues,
                    wall_time_unit => opaque_same_window,
                    observer_effects => [scheduler_wall_time_worker, run_queue_sampler]
                }};
            {{error, Reason}, _, _, _} ->
                {error, Reason};
            {_, {error, Reason}, _, _} ->
                {error, Reason};
            {_, _, {error, Reason}, _} ->
                {error, Reason};
            {_, _, _, {error, Reason}} ->
                {error, Reason}
        end
    end).

with_wall_maps(First, Second, Fun) ->
    case
        {
            wall_map(maps:get(wall_time, First, invalid)),
            wall_map(maps:get(wall_time, Second, invalid))
        }
    of
        {{ok, FirstWall}, {ok, SecondWall}} -> Fun(FirstWall, SecondWall);
        {{error, Reason}, _} -> {error, Reason};
        {_, {error, Reason}} -> {error, Reason}
    end.

wall_map(WallTime) when is_list(WallTime) ->
    wall_map(WallTime, #{});
wall_map(_WallTime) ->
    {error, invalid_counter_shape}.

wall_map([{Id, Active, Total} | Rest], Acc) when
    is_integer(Id), is_integer(Active), Active >= 0, is_integer(Total), Total >= 0
->
    case maps:is_key(Id, Acc) of
        true -> {error, duplicate_scheduler_id};
        false -> wall_map(Rest, Acc#{Id => {Active, Total}})
    end;
wall_map([], Acc) ->
    {ok, Acc};
wall_map(_Invalid, _Acc) ->
    {error, invalid_counter_shape}.

pool_delta([], _First, _Second) ->
    {ok, #{status => unavailable, reason_code => no_online_schedulers}};
pool_delta(Ids, First, Second) ->
    case pool_counters(Ids, First, Second, 0, 0) of
        {ok, ActiveDelta, TotalDelta} when TotalDelta > 0 ->
            {ok, #{
                status => available,
                utilization_ratio => ActiveDelta / TotalDelta,
                active_delta => #{value => ActiveDelta, unit => opaque_same_window},
                total_delta => #{value => TotalDelta, unit => opaque_same_window}
            }};
        {ok, _ActiveDelta, _TotalDelta} ->
            {error, zero_denominator};
        Error ->
            Error
    end.

pool_counters([Id | Rest], First, Second, ActiveAcc, TotalAcc) ->
    case {maps:find(Id, First), maps:find(Id, Second)} of
        {{ok, {FirstActive, FirstTotal}}, {ok, {SecondActive, SecondTotal}}} when
            SecondActive >= FirstActive, SecondTotal >= FirstTotal
        ->
            pool_counters(
                Rest,
                First,
                Second,
                ActiveAcc + SecondActive - FirstActive,
                TotalAcc + SecondTotal - FirstTotal
            );
        {{ok, _}, {ok, _}} ->
            {error, counter_reset};
        _ ->
            {error, missing_scheduler_id}
    end;
pool_counters([], _First, _Second, ActiveAcc, TotalAcc) ->
    {ok, ActiveAcc, TotalAcc}.

run_queue_window(SchedulersOnline, First, Second) ->
    case
        {
            run_queue_sample(SchedulersOnline, maps:get(run_queue_lengths, First, invalid)),
            run_queue_sample(SchedulersOnline, maps:get(run_queue_lengths, Second, invalid))
        }
    of
        {{ok, FirstNormal, FirstDirty}, {ok, SecondNormal, SecondDirty}} ->
            {ok, #{
                snapshot_atomic => false,
                semantics => runnable_or_running_observation_not_backlog,
                observer_contaminated => true,
                normal => #{
                    start_observed_runnable_count_including_observer => FirstNormal,
                    end_observed_runnable_count_including_observer => SecondNormal
                },
                dirty_cpu => #{
                    start_observed_runnable_count_including_observer => FirstDirty,
                    end_observed_runnable_count_including_observer => SecondDirty
                }
            }};
        _ ->
            {error, invalid_run_queue_shape}
    end.

run_queue_sample(SchedulersOnline, Queues) when
    is_list(Queues), length(Queues) > SchedulersOnline
->
    case lists:all(fun(Value) -> is_integer(Value) andalso Value >= 0 end, Queues) of
        true ->
            {ok, lists:sum(lists:sublist(Queues, SchedulersOnline)), lists:last(Queues)};
        false ->
            error
    end;
run_queue_sample(_SchedulersOnline, _Queues) ->
    error.

interval_ms(#{monotonic_ms := First}, #{monotonic_ms := Second}) when Second >= First ->
    {ok, Second - First};
interval_ms(_First, _Second) ->
    {error, invalid_interval}.

invalid_scheduler_window(Reason) ->
    #{
        status => invalid,
        reason_code => Reason,
        wall_time_unit => opaque_same_window,
        run_queue_snapshot_atomic => false,
        run_queue_semantics => runnable_or_running_observation_not_backlog,
        observer_contaminated => true
    }.

distribution_probe(Controller) ->
    distribution_probe(Controller, infinity).

distribution_probe(Controller, Limit) ->
    DistCtrl = safe_system_info(dist_ctrl),
    BusyLimit = safe_system_info(dist_buf_busy_limit),
    Data = distribution_context(
        controller_node(Controller),
        erlang:nodes(connected),
        erlang:nodes(visible),
        erlang:nodes(hidden),
        DistCtrl,
        BusyLimit,
        fun(Port) -> erlang:port_info(Port, queue_size) end
    ),
    {ok, limit_distribution(Data, Limit), [
        public_connected_peers,
        visible_hidden_classification,
        documented_controller_queue_context
    ]}.

safe_system_info(Key) ->
    try erlang:system_info(Key) of
        Value -> {ok, Value}
    catch
        _:_ -> {unavailable, capability_unavailable}
    end.

distribution_context(
    ControllerNode, Connected, Visible, Hidden, DistCtrl, BusyLimit, PortInfoFun
) ->
    KeptConnected = lists:sort(exclude_node(ControllerNode, Connected)),
    KeptVisible = lists:sort(exclude_node(ControllerNode, Visible)),
    KeptHidden = lists:sort(exclude_node(ControllerNode, Hidden)),
    Exclusions =
        case ControllerNode =/= undefined andalso lists:member(ControllerNode, Connected) of
            true ->
                [#{peer => {identifier, peer, ControllerNode}, reason => diagnostics_controller}];
            false ->
                []
        end,
    #{
        state => peer_state(KeptConnected),
        connected_peer_count => length(KeptConnected),
        connected_peers => peer_identifiers(KeptConnected),
        visible_peers => peer_identifiers(KeptVisible),
        hidden_peers => peer_identifiers(KeptHidden),
        excluded_peers => Exclusions,
        controller_queue_capability => controller_queue_capability(DistCtrl, BusyLimit),
        controller_queues => controller_queues(
            KeptConnected, DistCtrl, BusyLimit, PortInfoFun
        ),
        queue_semantics => context_only_not_backlog_health
    }.

controller_queue_capability({ok, Controllers}, {ok, BusyLimit}) when
    is_list(Controllers), is_integer(BusyLimit), BusyLimit >= 0
->
    #{status => available};
controller_queue_capability(_DistCtrl, _BusyLimit) ->
    #{status => unavailable, reason_code => capability_unavailable}.

controller_queues(Peers, {ok, Controllers}, {ok, BusyLimit}, PortInfoFun) when
    is_list(Controllers), is_integer(BusyLimit), BusyLimit >= 0
->
    [controller_queue(Peer, Controllers, BusyLimit, PortInfoFun) || Peer <- Peers];
controller_queues(Peers, _DistCtrl, _BusyLimit, _PortInfoFun) ->
    [
        #{
            peer => {identifier, peer, Peer},
            status => unavailable,
            reason_code => capability_unavailable
        }
     || Peer <- Peers
    ].

controller_queue(Peer, Controllers, BusyLimit, PortInfoFun) ->
    case lists:keyfind(Peer, 1, Controllers) of
        {Peer, Port} when is_port(Port) ->
            try PortInfoFun(Port) of
                {queue_size, QueueSize} when is_integer(QueueSize), QueueSize >= 0 ->
                    #{
                        peer => {identifier, peer, Peer},
                        status => available,
                        observed_queue_size_bytes => QueueSize,
                        busy_limit_bytes => BusyLimit,
                        health_inference => unavailable
                    };
                _ ->
                    unavailable_controller_queue(Peer)
            catch
                _:_ -> unavailable_controller_queue(Peer)
            end;
        _ ->
            unavailable_controller_queue(Peer)
    end.

unavailable_controller_queue(Peer) ->
    #{
        peer => {identifier, peer, Peer},
        status => unavailable,
        reason_code => capability_unavailable
    }.

limit_distribution(Data, infinity) ->
    Data#{truncated => false};
limit_distribution(Data, Limit) ->
    Connected = maps:get(connected_peers, Data),
    Kept = lists:sublist(Connected, Limit),
    Data#{
        connected_peers => Kept,
        visible_peers => [Peer || Peer <- maps:get(visible_peers, Data), lists:member(Peer, Kept)],
        hidden_peers => [Peer || Peer <- maps:get(hidden_peers, Data), lists:member(Peer, Kept)],
        controller_queues => [
            Queue
         || #{peer := Peer} = Queue <- maps:get(controller_queues, Data),
            lists:member(Peer, Kept)
        ],
        returned_peer_count => length(Kept),
        truncated => length(Connected) > Limit
    }.

controller_node(Controller) when is_pid(Controller) -> node(Controller);
controller_node(_Controller) -> undefined.

exclude_node(undefined, Nodes) -> Nodes;
exclude_node(Node, Nodes) -> lists:delete(Node, Nodes).

peer_identifiers(Nodes) -> [{identifier, peer, Peer} || Peer <- Nodes].

peer_state([]) -> empty;
peer_state(_Peers) -> connected.

text_system_info(Key) ->
    unicode:characters_to_binary(erlang:system_info(Key)).

target_from_probes(Probes) ->
    case probe_data(runtime, Probes) of
        Runtime when is_map(Runtime) ->
            target_from_runtime(Runtime);
        _ ->
            null
    end.

target_from_runtime(#{node := Node, otp_release := OtpRelease}) ->
    #{node => Node, otp_release => OtpRelease}.

snapshot_data(Probes) ->
    lists:foldl(
        fun
            ({#{id := Id, status := ok}, Data}, Acc) -> Acc#{Id => Data};
            (_Probe, Acc) -> Acc
        end,
        #{snapshot_version => 1},
        Probes
    ).

probe_data(Id, [{#{id := Id, status := ok}, Data} | _Rest]) -> Data;
probe_data(Id, [_Probe | Rest]) -> probe_data(Id, Rest);
probe_data(_Id, []) -> undefined.

capture_status(ProbeReports) ->
    case lists:any(fun probe_makes_partial/1, ProbeReports) of
        true -> partial;
        false -> complete
    end.

probe_makes_partial(#{required := true, status := Status}) ->
    Status =/= ok;
probe_makes_partial(#{required := false, status := Status}) ->
    Status =:= timeout orelse Status =:= error.

probe_warnings(ProbeReports) ->
    [
        #{probe => Id, reason_code => Reason}
     || #{id := Id, required := false, status := unavailable, reason_code := Reason} <-
            ProbeReports
    ].

probe_errors(ProbeReports) ->
    [
        #{
            class => probe_error_class(Required),
            probe => Id,
            reason_code => Reason
        }
     || #{
            id := Id,
            required := Required,
            status := Status,
            reason_code := Reason
        } <- ProbeReports,
        Status =:= timeout orelse Status =:= error orelse
            (Required =:= true andalso Status =:= unavailable)
    ].

probe_error_class(true) -> required_probe;
probe_error_class(false) -> partial.

observer_effects(ModuleLoaded, Controller) ->
    Base = [
        #{
            id => diagnostics_worker,
            affected_facts => [process_count, port_count, memory, io, garbage_collection]
        },
        #{id => module_load, module_loaded_before_sample => ModuleLoaded}
    ],
    case controller_node(Controller) of
        undefined ->
            Base;
        ControllerNode ->
            Base ++
                [
                    #{
                        id => distribution_controller,
                        controller_peer => {identifier, peer, ControllerNode},
                        dynamic_controller_name_atom => true
                    }
                ]
    end.

rfc3339(SystemTime) ->
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(SystemTime, [{unit, millisecond}, {offset, "Z"}])
    ).

normalize_value(_Term, _Policy, Depth, _State) when Depth > ?MAX_DEPTH ->
    {error, response_too_deep};
normalize_value(true, _Policy, _Depth, State) ->
    {ok, true, State};
normalize_value(false, _Policy, _Depth, State) ->
    {ok, false, State};
normalize_value(null, _Policy, _Depth, State) ->
    {ok, null, State};
normalize_value(Term, _Policy, _Depth, State) when is_integer(Term); is_float(Term) ->
    {ok, Term, State};
normalize_value(Term, _Policy, _Depth, State) when is_binary(Term) ->
    normalize_binary(Term, State);
normalize_value(Term, _Policy, _Depth, State) when is_atom(Term) ->
    normalize_binary(atom_to_binary(Term), State);
normalize_value(Term, Policy, _Depth, State) when is_pid(Term) ->
    normalize_identifier(pid, Term, Policy, State);
normalize_value(Term, Policy, _Depth, State) when is_port(Term) ->
    normalize_identifier(port, Term, Policy, State);
normalize_value(Term, Policy, _Depth, State) when is_reference(Term) ->
    normalize_identifier(ref, Term, Policy, State);
normalize_value({identifier, Type, Value}, Policy, _Depth, State) when is_atom(Type) ->
    normalize_identifier(Type, Value, Policy, State);
normalize_value({mfa, Module, Function, Arity}, Policy, _Depth, State) when
    is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0
->
    case normalize_identifier(module, Module, Policy, State) of
        {ok, NormalizedModule, State1} ->
            case normalize_identifier(function, Function, Policy, State1) of
                {ok, NormalizedFunction, State2} ->
                    {ok,
                        #{
                            <<"module">> => NormalizedModule,
                            <<"function">> => NormalizedFunction,
                            <<"arity">> => Arity
                        },
                        State2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
normalize_value(Term, Policy, Depth, State) when is_map(Term) ->
    normalize_map(maps:to_list(Term), Policy, Depth + 1, State, #{});
normalize_value(Term, Policy, Depth, State) when is_list(Term) ->
    normalize_list(Term, Policy, Depth + 1, State, []);
normalize_value(_Term, _Policy, _Depth, _State) ->
    {error, invalid_schema}.

normalize_binary(Binary, State) when byte_size(Binary) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Binary) of
        Binary ->
            {ok, Binary, State};
        {_Error, _Valid, _Rest} ->
            Encoded = base64:encode(Binary),
            case byte_size(Encoded) =< ?MAX_FIELD_BYTES of
                true ->
                    {ok,
                        #{
                            <<"encoding">> => <<"base64">>,
                            <<"data">> => Encoded
                        },
                        State};
                false ->
                    {error, field_too_large}
            end
    end;
normalize_binary(_Binary, _State) ->
    {error, field_too_large}.

normalize_map([], _Policy, _Depth, State, Acc) ->
    {ok, Acc, State};
normalize_map([{Key, Value} | Rest], Policy, Depth, State, Acc) ->
    case normalize_key(Key) of
        {ok, NormalizedKey} when not is_map_key(NormalizedKey, Acc) ->
            case normalize_value(Value, Policy, Depth, State) of
                {ok, NormalizedValue, State1} ->
                    normalize_map(
                        Rest, Policy, Depth, State1, Acc#{NormalizedKey => NormalizedValue}
                    );
                Error ->
                    Error
            end;
        _ ->
            {error, invalid_schema}
    end.

normalize_key(Key) when is_atom(Key) ->
    normalize_key(atom_to_binary(Key));
normalize_key(Key) when is_binary(Key), byte_size(Key) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Key) of
        Key -> {ok, Key};
        _ -> error
    end;
normalize_key(_Key) ->
    error.

normalize_list([], _Policy, _Depth, State, Acc) ->
    {ok, lists:reverse(Acc), State};
normalize_list([Value | Rest], Policy, Depth, State, Acc) ->
    case normalize_value(Value, Policy, Depth, State) of
        {ok, NormalizedValue, State1} ->
            normalize_list(Rest, Policy, Depth, State1, [NormalizedValue | Acc]);
        Error ->
            Error
    end;
normalize_list(_Improper, _Policy, _Depth, _State, _Acc) ->
    {error, invalid_schema}.

normalize_identifier(Type, Value, Policy, State) ->
    case identifier_binary(Type, Value) of
        {ok, Binary} when Policy =:= include ->
            normalize_binary(Binary, State);
        {ok, _Binary} when Policy =:= redact ->
            stable_identifier(Type, Value, State);
        error ->
            {error, invalid_identifier}
    end.

identifier_binary(pid, Value) when is_pid(Value) ->
    {ok, list_to_binary(pid_to_list(Value))};
identifier_binary(port, Value) when is_port(Value) ->
    {ok, list_to_binary(port_to_list(Value))};
identifier_binary(ref, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(table, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(table, Value) when is_integer(Value) ->
    {ok, integer_to_binary(Value)};
identifier_binary(socket, {'$socket', Value}) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(socket, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(Type, Value) when
    Type =:= node;
    Type =:= name;
    Type =:= module;
    Type =:= function;
    Type =:= peer;
    Type =:= table;
    Type =:= application
->
    identifier_text(Value);
identifier_binary(_Type, _Value) ->
    error.

identifier_text(Value) when is_atom(Value) ->
    {ok, atom_to_binary(Value)};
identifier_text(Value) when is_binary(Value), byte_size(Value) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Value) of
        Value -> {ok, Value};
        _ -> error
    end;
identifier_text(_Value) ->
    error.

stable_identifier(Type, Value, #{ids := Ids, counts := Counts} = State) ->
    Key = {Type, Value},
    case maps:find(Key, Ids) of
        {ok, Identifier} ->
            {ok, Identifier, State};
        error ->
            Number = maps:get(Type, Counts, 0) + 1,
            Identifier = iolist_to_binary([atom_to_binary(Type), $-, integer_to_binary(Number)]),
            {ok, Identifier, State#{
                ids := Ids#{Key => Identifier},
                counts := Counts#{Type => Number}
            }}
    end.

truncate_to_cap(Response, Pointers) ->
    %% ponytail: tail trimming is O(n^2); report lists cap at 200, batch it if that cap grows.
    case erlang:external_size(Response) =< ?MAX_RESULT_BYTES of
        true ->
            {ok, Response};
        false ->
            case trim_once(Response, [], Pointers) of
                {ok, Trimmed} ->
                    case pointers_exist(Trimmed, Pointers) of
                        true -> truncate_to_cap(Trimmed, Pointers);
                        false -> {error, invalid_evidence_pointer}
                    end;
                none ->
                    {error, response_too_large}
            end
    end.

trim_once(Map, Path, Pointers) when is_map(Map) ->
    case trim_items(Map, Path, Pointers) of
        {ok, Trimmed} -> {ok, Trimmed};
        none -> trim_map_values(maps:to_list(Map), Map, Path, Pointers)
    end;
trim_once(List, Path, Pointers) when is_list(List) ->
    trim_list_values(List, [], Path, Pointers, 0);
trim_once(_Value, _Path, _Pointers) ->
    none.

trim_items(#{<<"items">> := Items} = Map, Path, Pointers) when Items =/= [] ->
    Index = length(Items) - 1,
    ItemsPath = Path ++ [<<"items">>],
    case pointer_protects_tail(Pointers, ItemsPath, Index) of
        true ->
            none;
        false ->
            NewItems = lists:sublist(Items, Index),
            {ok, update_item_counts(Map#{<<"items">> := NewItems, <<"truncated">> => true})}
    end;
trim_items(_Map, _Path, _Pointers) ->
    none.

update_item_counts(Map) ->
    Items = maps:get(<<"items">>, Map),
    Map1 =
        case maps:is_key(<<"returned_count">>, Map) of
            true -> Map#{<<"returned_count">> := length(Items)};
            false -> Map
        end,
    case maps:find(<<"dropped_count">>, Map1) of
        {ok, Count} when is_integer(Count) -> Map1#{<<"dropped_count">> := Count + 1};
        _ -> Map1
    end.

trim_map_values([], _Map, _Path, _Pointers) ->
    none;
trim_map_values([{<<"items">>, _Value} | Rest], Map, Path, Pointers) ->
    trim_map_values(Rest, Map, Path, Pointers);
trim_map_values([{Key, Value} | Rest], Map, Path, Pointers) ->
    case trim_once(Value, Path ++ [Key], Pointers) of
        {ok, TrimmedValue} -> {ok, Map#{Key := TrimmedValue}};
        none -> trim_map_values(Rest, Map, Path, Pointers)
    end.

trim_list_values([], _Before, _Path, _Pointers, _Index) ->
    none;
trim_list_values([Value | Rest], Before, Path, Pointers, Index) ->
    case trim_once(Value, Path ++ [integer_to_binary(Index)], Pointers) of
        {ok, TrimmedValue} -> {ok, lists:reverse(Before, [TrimmedValue | Rest])};
        none -> trim_list_values(Rest, [Value | Before], Path, Pointers, Index + 1)
    end.

evidence_pointers(Term) ->
    evidence_pointers(Term, []).

evidence_pointers(Map, Acc) when is_map(Map) ->
    case maps:find(<<"evidence">>, Map) of
        {ok, Evidence} when is_list(Evidence) ->
            case evidence_paths(Evidence, Acc) of
                {ok, Acc1} -> evidence_pointers_values(maps:values(Map), Acc1);
                error -> error
            end;
        {ok, _Invalid} ->
            error;
        error ->
            evidence_pointers_values(maps:values(Map), Acc)
    end;
evidence_pointers(List, Acc) when is_list(List) ->
    evidence_pointers_values(List, Acc);
evidence_pointers(_Value, Acc) ->
    {ok, Acc}.

evidence_pointers_values([], Acc) ->
    {ok, Acc};
evidence_pointers_values([Value | Rest], Acc) ->
    case evidence_pointers(Value, Acc) of
        {ok, Acc1} -> evidence_pointers_values(Rest, Acc1);
        error -> error
    end.

evidence_paths([], Acc) ->
    {ok, Acc};
evidence_paths([#{<<"path">> := Pointer} | Rest], Acc) when is_binary(Pointer) ->
    case parse_pointer(Pointer) of
        {ok, Segments} -> evidence_paths(Rest, [Segments | Acc]);
        error -> error
    end;
evidence_paths([_Invalid | _Rest], _Acc) ->
    error.

parse_pointer(<<>>) ->
    {ok, []};
parse_pointer(<<"/", Rest/binary>>) ->
    parse_pointer_segments(binary:split(Rest, <<"/">>, [global]), []);
parse_pointer(_Pointer) ->
    error.

parse_pointer_segments([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_pointer_segments([Segment | Rest], Acc) ->
    case unescape_pointer(Segment, <<>>) of
        {ok, Unescaped} -> parse_pointer_segments(Rest, [Unescaped | Acc]);
        error -> error
    end.

unescape_pointer(<<>>, Acc) ->
    {ok, Acc};
unescape_pointer(<<"~0", Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, "~">>);
unescape_pointer(<<"~1", Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, "/">>);
unescape_pointer(<<"~", _Rest/binary>>, _Acc) ->
    error;
unescape_pointer(<<Byte, Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, Byte>>).

pointers_exist(Response, Pointers) ->
    lists:all(fun(Pointer) -> pointer_exists(Response, Pointer) end, Pointers).

pointer_exists(_Value, []) ->
    true;
pointer_exists(Map, [Key | Rest]) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> pointer_exists(Value, Rest);
        error -> false
    end;
pointer_exists(List, [Index | Rest]) when is_list(List) ->
    case pointer_index(Index) of
        {ok, Number} when Number < length(List) ->
            pointer_exists(lists:nth(Number + 1, List), Rest);
        _ ->
            false
    end;
pointer_exists(_Value, _Segments) ->
    false.

pointer_index(<<"0">>) ->
    {ok, 0};
pointer_index(<<First, _/binary>> = Binary) when First >= $1, First =< $9 ->
    try binary_to_integer(Binary) of
        Number when Number >= 0 -> {ok, Number};
        _ -> error
    catch
        error:badarg -> error
    end;
pointer_index(_Index) ->
    error.

pointer_protects_tail(Pointers, Path, Index) ->
    lists:any(
        fun(Pointer) ->
            case length(Pointer) >= length(Path) of
                true ->
                    case lists:split(length(Path), Pointer) of
                        {Path, []} -> true;
                        {Path, [Segment | _]} -> Segment =:= integer_to_binary(Index);
                        _ -> false
                    end;
                false ->
                    false
            end
        end,
        Pointers
    ).

json_safe(Map) when is_map(Map) ->
    lists:all(
        fun({Key, Value}) -> is_binary(Key) andalso json_safe(Value) end,
        maps:to_list(Map)
    );
json_safe(List) when is_list(List) ->
    lists:all(fun json_safe/1, List);
json_safe(Value) when is_binary(Value); is_integer(Value); is_float(Value) ->
    true;
json_safe(Value) when Value =:= true; Value =:= false; Value =:= null ->
    true;
json_safe(_Value) ->
    false.

remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

success_result(Result) ->
    #{
        <<"status">> => <<"ok">>,
        <<"result">> => Result,
        <<"cleanup_confirmed">> => true
    }.

error_result(Reason) ->
    #{
        <<"status">> => <<"error">>,
        <<"reason_code">> => atom_to_binary(Reason),
        <<"cleanup_confirmed">> => true
    }.

drain_exit(Worker) ->
    receive
        {'EXIT', Worker, _Reason} -> ok
    after 0 ->
        ok
    end.
