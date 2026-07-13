-module(observer_cli_snapshot).

-dialyzer(
    {nowarn_function, [
        diagnostic_memory/1,
        call_snapshot_probe/1,
        probe_result/4,
        iterator_fold/2,
        iterator_fold/3
    ]}
).

-export([
    capabilities/0,
    diagnostic_binary_holders/2,
    diagnostic_distribution/1,
    diagnostic_scheduler_flag/1,
    diagnostic_scheduler_sample/0,
    diagnostic_scheduler_window/2,
    diagnostic_socket_trend/1,
    diagnostic_sample/2,
    dispatch/4,
    normalize/2,
    truncate/1
]).

-ifdef(TEST).
-export([
    allocator_data/1,
    scheduler_window/2,
    measure_scheduler/4,
    distribution_context/7,
    stable_process_window/3,
    resolve_process_target/2,
    counter_window/3,
    socket_metrics/1,
    shape_type/1,
    stacktrace_arity/1,
    target_binary/1,
    inet_protocol/1,
    port_list_field/1,
    stat_value/1,
    bounded_identifier_text/1,
    port_identifier/1,
    raw_resource_identifier/1,
    pointer_index/1,
    json_safe/1,
    deep_worker_reason/1,
    composed_probe/1,
    deep_probe_result/3,
    application_name_binary/1,
    state_shape/1,
    sanitize_signal_list/1,
    sanitize_suspending_list/1,
    sanitize_error_handler/1,
    sanitize_stacktrace_frame/1,
    port_endpoint/1,
    port_option/3,
    safe_resource_count/1,
    resource_precedes/3,
    top_n_value/2,
    controller_node/1,
    update_item_counts/1,
    pointer_protects_tail/3,
    memory_command_data/2,
    binary_ref_stats/1,
    sanitize_stacktrace_location/1,
    default_mnesia_source/0,
    safe_ports/0,
    safe_sockets/0,
    safe_port_info/2,
    network_sample/2,
    network_port_field/3,
    parse_network_counters/1,
    socket_sample/1,
    network_io_counters/1,
    port_statistics/2,
    safe_port_option_value/2,
    socket_optional_coverage/1,
    counter_series_delta/1,
    counter_deltas/2,
    call_snapshot_probe/1,
    probe_result/4,
    scheduler_window_data/3,
    with_wall_maps/3,
    wall_map/1,
    pool_delta/3,
    run_queue_window/3,
    run_queue_sample/2,
    safe_system_info/1,
    controller_queue/4,
    limit_distribution/2,
    observer_effects/2,
    supervisor_counts/1,
    valid_supervisor_children/1,
    child_pid_item/2,
    resolve_loaded_application/2,
    shape_term/3,
    shape_children/4,
    shape_list/5,
    resolve_pid_text/2,
    resolve_registered_name/2,
    application_leaders/2,
    mnesia_info/3,
    mnesia_correlation/3,
    process_fold/0,
    list_process_fold/2,
    socket_available/0,
    delta_resource_audit/3,
    vm_io_metrics/2,
    observer_port_exclusions/1,
    port_exclusion/2,
    port_resource/2,
    resolve_port_target/1,
    call_port_fun/3,
    socket_series_trend/1,
    normalize_value/4,
    normalize_map/5,
    normalize_key/1,
    identifier_text/1,
    trim_map_values/4,
    trim_list_values/5,
    evidence_pointers_values/2,
    parse_pointer/1,
    dispatch_options/1,
    stop_worker/3,
    worker_down/2,
    diagnostic_memory/1,
    diagnostic_ets/1,
    diagnostic_ports/2,
    diagnostic_sockets/1,
    diagnostic_port/2,
    diagnostic_application/1,
    finish_deep_probe/4,
    coordinate/5,
    finish_worker/6,
    capture_logs/2,
    capture_trace/2,
    trace_response/3,
    collect_root_children/4,
    collect_admitted_root_children/5,
    collect_otp_state/4,
    capture_applications/2,
    capture_ets/2,
    capture_mnesia/2,
    capture_ports/2,
    capture_counter_resources/5,
    run_snapshot_probe/5,
    scan_process/4,
    collect_process_sample/3,
    collect_process/2,
    collect_available_mnesia/4,
    scan_mnesia_table/5,
    collect_counter_resources/6,
    collect_admitted_counter_resources/7,
    network_resource/2,
    collect_ports/4,
    default_process_source/0,
    probe/3,
    collect_admitted_applications/8
]).
-endif.

-define(PROTOCOL_VERSION, 1).
-define(BUNDLE_VERSION, <<"2.0.0">>).
-define(TARGET_MARGIN_MS, 1000).
-define(WORKER_DOWN_TIMEOUT_MS, 100).
-define(DEEP_FINISH_MARGIN_MS, 250).
-define(MAX_HEAP_WORDS, 8 * 1024 * 1024).
-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_RESULT_BYTES, ?MAX_RESPONSE_BYTES - 1024).
-define(MAX_FIELD_BYTES, 64 * 1024).
-define(MAX_DEPTH, 32).
-define(STATE_SHAPE_MAX_BYTES, 64 * 1024).
-define(STATE_SHAPE_MAX_DEPTH, 6).
-define(STATE_SHAPE_MAX_NODES, 10000).
-define(STATE_SHAPE_PREFIX, 2).
-define(STATE_TIMEOUT_MS, 5000).
-define(PROCESS_SCAN_BUDGET, 100000).
-define(BINARY_PROCESS_SCAN_BUDGET, 20000).
-define(APPLICATION_SCAN_BUDGET, 5000).
-define(SUPERVISOR_SCAN_BUDGET, 300).
-define(SUPERVISOR_OUTPUT_CAP, 100).
-define(CHILD_ID_MAX_BYTES, 128).
-define(CHILD_ID_MAX_BITS, 1024).
-define(ETS_SCAN_BUDGET, 100000).
-define(MNESIA_SCAN_BUDGET, 10000).
-define(PORT_SCAN_BUDGET, 100000).
-define(SOCKET_SCAN_BUDGET, 100000).
-define(WORKING_SET_BYTES_PER_FIELD, 64).
-define(MAX_WORKING_SET_BYTES, 64 * 1024 * 1024).
-define(PROCESS_LABEL_CHARS_LIMIT, 256).

-spec capabilities() -> #{bundle_version := binary(), protocol_version := pos_integer()}.
capabilities() ->
    #{bundle_version => ?BUNDLE_VERSION, protocol_version => ?PROTOCOL_VERSION}.

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
                worker(
                    Coordinator,
                    Controller,
                    RunRef,
                    Command,
                    Request,
                    Policy,
                    Deadline,
                    MaxHeapWords
                )
            end,
            [
                link,
                monitor,
                {max_heap_size, #{size => MaxHeapWords, kill => true, error_logger => false}}
            ]
        ),
        coordinate_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Command)
    catch
        _OuterClass:_OuterReason:_OuterStacktrace -> error_result(internal_error)
    after
        erlang:demonitor(ControllerRef, [flush]),
        process_flag(trap_exit, OldTrapExit)
    end.

coordinate_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Command) ->
    try
        coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Command)
    catch
        _Class:_Reason:_Stacktrace -> stop_worker(Worker, WorkerRef, internal_error, Command)
    after
        drain_exit(Worker)
    end.

-ifdef(TEST).
coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline) ->
    coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline, unknown).
-endif.

coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Command) ->
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected, Command);
        {RunRef, Worker, {ok, Result}} ->
            Response = success_result(Result),
            case
                json_safe(Response) andalso erlang:external_size(Response) =< ?MAX_RESPONSE_BYTES
            of
                true ->
                    finish_worker(
                        ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result, Command
                    );
                false ->
                    stop_worker(Worker, WorkerRef, invalid_schema, Command)
            end;
        {RunRef, Worker, {error, Reason}} when is_atom(Reason) ->
            stop_worker(Worker, WorkerRef, Reason, Command);
        {'DOWN', WorkerRef, process, Worker, Reason} ->
            worker_down(WorkerRef, Reason, Command);
        {'EXIT', Worker, _Reason} ->
            coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Command)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, target_timeout, Command)
    end.

-ifdef(TEST).
finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result) ->
    finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result, unknown).
-endif.

finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result, Command) ->
    Worker ! {RunRef, finish},
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected, Command);
        {'DOWN', WorkerRef, process, Worker, normal} ->
            success_result(Result);
        {'DOWN', WorkerRef, process, Worker, _Reason} ->
            error_result(cleanup_unconfirmed);
        {'EXIT', Worker, _Reason} ->
            finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result, Command)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, cleanup_unconfirmed, Command)
    end.

-ifdef(TEST).
stop_worker(Worker, WorkerRef, Reason) ->
    stop_worker(Worker, WorkerRef, Reason, unknown).
-endif.

stop_worker(Worker, WorkerRef, Reason, Command) ->
    exit(Worker, kill),
    receive
        {'DOWN', WorkerRef, process, Worker, _WorkerReason} -> cleanup_result(Command, Reason)
    after ?WORKER_DOWN_TIMEOUT_MS ->
        error_result(cleanup_unconfirmed, false)
    end.

cleanup_result(trace, Reason) when
    Reason =:= target_timeout;
    Reason =:= controller_disconnected;
    Reason =:= cleanup_unconfirmed;
    Reason =:= internal_error;
    Reason =:= worker_heap_limit_exceeded;
    Reason =:= probe_failed
->
    error_result(cleanup_unconfirmed, false);
cleanup_result(_Command, Reason) ->
    error_result(Reason).

-ifdef(TEST).
worker_down(WorkerRef, Reason) ->
    worker_down(WorkerRef, Reason, unknown).
-endif.

worker_down(WorkerRef, Reason, Command) ->
    erlang:demonitor(WorkerRef, [flush]),
    cleanup_result(
        Command,
        case Reason of
            killed -> worker_heap_limit_exceeded;
            _ -> probe_failed
        end
    ).

worker(Coordinator, Controller, RunRef, Command, Request, Policy, Deadline, MaxHeapWords) ->
    Outcome =
        try
            probe(Command, Request, #{
                deadline => Deadline,
                controller => Controller,
                coordinator => Coordinator,
                max_heap_words => MaxHeapWords
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
probe(diagnose, Request, Context) ->
    observer_cli_diagnostic:capture(Request, Context);
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
probe(port, Request, Context) ->
    capture_port(Request, Context);
probe(applications, Request, Context) ->
    capture_applications(Request, Context);
probe(ets, Request, Context) ->
    capture_ets(Request, Context);
probe(mnesia, Request, Context) ->
    capture_mnesia(Request, Context);
probe(network, Request, Context) ->
    capture_network(Request, Context);
probe(ports, Request, Context) ->
    capture_ports(Request, Context);
probe(sockets, Request, Context) ->
    capture_sockets(Request, Context);
probe(otp_state, Request, Context) ->
    capture_otp_state(Request, Context);
probe(supervision_tree, Request, Context) ->
    capture_supervision_tree(Request, Context);
probe(logs, Request, Context) ->
    capture_logs_test(Request, Context);
probe(trace, Request, Context) ->
    capture_trace(Request, Context);
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-else.
probe(snapshot, Request, Context) ->
    capture_snapshot(Request, Context);
probe(diagnose, Request, Context) ->
    observer_cli_diagnostic:capture(Request, Context);
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
probe(port, Request, Context) ->
    capture_port(Request, Context);
probe(applications, Request, Context) ->
    capture_applications(Request, Context);
probe(ets, Request, Context) ->
    capture_ets(Request, Context);
probe(mnesia, Request, Context) ->
    capture_mnesia(Request, Context);
probe(network, Request, Context) ->
    capture_network(Request, Context);
probe(ports, Request, Context) ->
    capture_ports(Request, Context);
probe(sockets, Request, Context) ->
    capture_sockets(Request, Context);
probe(otp_state, Request, Context) ->
    capture_otp_state(Request, Context);
probe(supervision_tree, Request, Context) ->
    capture_supervision_tree(Request, Context);
probe(logs, Request, Context) ->
    capture_logs(Request, Context);
probe(trace, Request, Context) ->
    capture_trace(Request, Context);
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-endif.

capture_trace(#{action := call} = Request, #{controller := Controller} = Context) ->
    trace_response(
        trace_call,
        fun() -> observer_cli_trace:call(Controller, maps:remove(action, Request)) end,
        Context
    );
capture_trace(#{action := stop_all, all := true}, Context) ->
    trace_response(trace_stop_all, fun observer_cli_trace:stop_all/0, Context);
capture_trace(_Request, _Context) ->
    {probe_error, invalid_request}.

-ifdef(TEST).
capture_logs_test(#{test_log_env := Env} = RawRequest, Context) when is_map(Env) ->
    Request = maps:remove(test_log_env, RawRequest),
    case observer_cli_log:valid_request(Request) of
        true ->
            capture_scan_inspection(logs, log_file_tail, 1, Context, fun() ->
                observer_cli_log:capture(Request, Env)
            end);
        false ->
            {probe_error, invalid_request}
    end;
capture_logs_test(Request, Context) ->
    capture_logs(Request, Context).
-endif.
capture_logs(Request, Context) ->
    case observer_cli_log:valid_request(Request) of
        true ->
            capture_scan_inspection(logs, log_file_tail, 1, Context, fun() ->
                observer_cli_log:capture(Request)
            end);
        false ->
            {probe_error, invalid_request}
    end.

trace_response(Command, Fun, #{controller := Controller}) ->
    StartedAt = erlang:system_time(millisecond),
    Started = erlang:monotonic_time(millisecond),
    Result = Fun(),
    Finished = erlang:monotonic_time(millisecond),
    {ok, Runtime, _} = runtime_probe(),
    Status = maps:get(status, Result),
    Category = maps:get(category, Result),
    Reason = maps:get(reason, Result),
    Warnings = trace_issues(maps:get(warnings, Result)),
    TraceCapture = trace_capture(Command, maps:get(capture, Result)),
    Capture =
        case TraceCapture of
            null ->
                null;
            _ ->
                #{
                    started_at => rfc3339(StartedAt),
                    finished_at => rfc3339(erlang:system_time(millisecond)),
                    duration_ms => Finished - Started,
                    probes => [
                        probe_report(
                            trace,
                            true,
                            case Status of
                                ok -> ok;
                                _ -> error
                            end,
                            case Status of
                                ok -> null;
                                _ -> Reason
                            end,
                            Finished - Started,
                            1,
                            [recon_2_5_6, external_global_calls_only]
                        )
                    ],
                    observer_effects => [
                        #{
                            id => global_trace_replacement,
                            controller => {identifier, pid, Controller}
                        }
                    ]
                }
        end,
    observer_cli_cli:response(
        Command,
        case Status of
            ok -> complete;
            _ -> error
        end,
        target_from_runtime(Runtime),
        Capture,
        case TraceCapture of
            null -> null;
            _ -> #{reason => Reason, trace => TraceCapture}
        end,
        case Status of
            ok -> Warnings;
            _ when Capture =:= null -> [observer_cli_cli:error(Category, Reason) | Warnings];
            _ -> Warnings
        end
    ).

trace_capture(trace_stop_all, #{events := _Events} = Capture) ->
    Capture#{events := []};
trace_capture(_Command, Capture) ->
    Capture.

trace_issues(Warnings) ->
    [
        #{
            <<"severity">> => <<"warning">>,
            <<"class">> => <<"safety_refusal">>,
            <<"reason_code">> => atom_to_binary(Code),
            <<"message">> => Message
        }
     || #{code := Code, message := Message} <- Warnings
    ].

capture_snapshot(Request, #{deadline := Deadline, controller := Controller} = Context) when
    is_map(Request)
->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = module_loaded(),
    RuntimeProbe = runtime_probe(),
    {ok, Runtime, _} = RuntimeProbe,
    CoreProbes = [
        run_snapshot_probe(runtime, true, fun() -> RuntimeProbe end, Request, Deadline),
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
    Probes = CoreProbes ++ deep_snapshot_probes(Request, Context),
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    ProbeReports = [Report || {Report, _Data} <- Probes],
    Status = capture_status(ProbeReports),
    observer_cli_cli:response(
        snapshot,
        Status,
        target_from_runtime(Runtime),
        #{
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(FinishedAt),
            duration_ms => FinishedMonotonic - StartedMonotonic,
            probes => ProbeReports,
            observer_effects => observer_effects(ModuleLoaded, Controller)
        },
        snapshot_data(Probes),
        []
    );
capture_snapshot(_Request, _Context) ->
    {probe_error, invalid_request}.

-spec diagnostic_sample(map(), map()) -> map().
diagnostic_sample(Request, Context) when is_map(Request), is_map(Context) ->
    Started = erlang:monotonic_time(millisecond),
    {ok, Resources, _} = resources_probe(),
    ResourceFinished = erlang:monotonic_time(millisecond),
    Inventory =
        try diagnostic_process_inventory(Request, Context) of
            Result -> Result
        catch
            _Class:_Reason:_Stacktrace ->
                #{status => error, reason_code => process_inventory_failed}
        end,
    Memory = diagnostic_memory(Request),
    Ets = diagnostic_ets(Request),
    Ports = diagnostic_ports(Request, Context),
    Sockets = diagnostic_sockets(Request),
    Application = diagnostic_application(Request),
    Finished = erlang:monotonic_time(millisecond),
    #{
        status => ok,
        monotonic_start_ms => Started,
        monotonic_finish_ms => Finished,
        monotonic_midpoint_ms => Started + ((ResourceFinished - Started) div 2),
        resources => Resources,
        process_inventory => Inventory,
        memory => Memory,
        ets_inventory => Ets,
        port_inventory => Ports,
        socket_inventory => Sockets,
        quick_scheduler_sample => scheduler_sample(),
        application => Application
    }.

diagnostic_memory(#{observe := _}) ->
    case memory_probe() of
        {ok, Memory, _} -> #{status => ok, values => maps:get(beam, Memory)};
        _ -> #{status => error, reason_code => memory_probe_failed}
    end;
diagnostic_memory(_Request) ->
    #{status => unavailable, reason_code => observation_not_requested}.

diagnostic_ets(#{observe := _} = Request) ->
    diagnostic_ets_inventory(Request);
diagnostic_ets(#{sample_index := 0} = Request) ->
    diagnostic_ets_inventory(Request);
diagnostic_ets(_Request) ->
    #{status => unavailable, reason_code => observation_not_requested}.

diagnostic_ets_inventory(Request) ->
    Source = ets_source(Request),
    Count = (maps:get(count_fun, Source))(),
    Samples = diagnostic_retained_samples(Request),
    case
        Count =< ?ETS_SCAN_BUDGET andalso
            working_set_estimate(Count, 3, Samples) =< ?MAX_WORKING_SET_BYTES
    of
        true ->
            Started = erlang:monotonic_time(millisecond),
            Tables = (maps:get(all_fun, Source))(),
            Values = maps:from_list(
                lists:filtermap(
                    fun(Table) -> diagnostic_ets_item(Table, Source) end,
                    Tables
                )
            ),
            Finished = erlang:monotonic_time(millisecond),
            #{
                status => ok,
                values => Values,
                audit => diagnostic_scan_audit(Started, Finished)
            };
        false ->
            #{status => unavailable, reason_code => scan_budget_exceeded}
    end.

diagnostic_ets_item(Table, Source) ->
    Info = maps:get(info_fun, Source),
    try
        FirstId = Info(Table, id),
        Size = Info(Table, size),
        Memory = Info(Table, memory),
        LastId = Info(Table, id),
        case
            valid_raw_table_id(FirstId) andalso FirstId =:= LastId andalso
                is_integer(Size) andalso is_integer(Memory)
        of
            true ->
                {true, {Table, #{generation => FirstId, size => Size, memory_words => Memory}}};
            false ->
                false
        end
    catch
        _:_ -> false
    end.

diagnostic_ports(#{observe := _} = Request, Context) ->
    diagnostic_ports_inventory(Request, Context);
diagnostic_ports(#{sample_index := 0} = Request, Context) ->
    diagnostic_ports_inventory(Request, Context);
diagnostic_ports(_Request, _Context) ->
    #{status => unavailable, reason_code => observation_not_requested}.

diagnostic_ports_inventory(Request, Context) ->
    Source = port_source(Request),
    Count = safe_resource_count(Source),
    case
        Count =< ?PORT_SCAN_BUDGET andalso
            working_set_estimate(Count, 4, diagnostic_retained_samples(Request)) =<
                ?MAX_WORKING_SET_BYTES
    of
        true ->
            Started = erlang:monotonic_time(millisecond),
            case (maps:get(all_fun, Source))() of
                {ok, Ports} ->
                    Excluded = observer_port_exclusions(Context),
                    Values = maps:from_list([
                        {Port, diagnostic_port(Port, Source)}
                     || Port <- Ports, not maps:is_key(Port, Excluded)
                    ]),
                    Finished = erlang:monotonic_time(millisecond),
                    #{
                        status => ok,
                        values => Values,
                        audit => diagnostic_scan_audit(Started, Finished)
                    };
                _ ->
                    #{status => error, reason_code => port_inventory_failed}
            end;
        false ->
            #{status => unavailable, reason_code => scan_budget_exceeded}
    end.

diagnostic_sockets(#{observe := _} = Request) ->
    Source = socket_source(Request),
    case (maps:get(available_fun, Source))() of
        true -> diagnostic_socket_scan(Source, diagnostic_retained_samples(Request));
        false -> #{status => unavailable, reason_code => capability_unavailable}
    end;
diagnostic_sockets(_Request) ->
    #{status => unavailable, reason_code => observation_not_requested}.

diagnostic_socket_scan(Source, Samples) ->
    Count = safe_resource_count(Source),
    case
        Count =< ?SOCKET_SCAN_BUDGET andalso
            working_set_estimate(Count, tracked_counter_fields(sockets), Samples) =<
                ?MAX_WORKING_SET_BYTES
    of
        true ->
            case socket_sample(Source) of
                {ok, Values, Audit, Coverage} ->
                    #{
                        status => ok,
                        values => Values,
                        audit => Audit,
                        coverage => lists:usort(Coverage)
                    };
                {error, Reason} ->
                    #{status => error, reason_code => Reason}
            end;
        false ->
            #{status => unavailable, reason_code => scan_budget_exceeded}
    end.

diagnostic_retained_samples(#{deep := true}) -> 7;
diagnostic_retained_samples(#{observe := _}) -> 5;
diagnostic_retained_samples(_Request) -> 1.

diagnostic_scan_audit(Started, Finished) ->
    #{scan_started_monotonic_ms => Started, scan_finished_monotonic_ms => Finished}.

diagnostic_port(Port, Source) ->
    lists:foldl(
        fun(Key, Acc) ->
            case (maps:get(info_fun, Source))(Port, Key) of
                {ok, Value} when is_integer(Value), Value >= 0 -> Acc#{Key => Value};
                _ -> Acc
            end
        end,
        #{},
        [queue_size, memory, input, output]
    ).

diagnostic_application(#{observe := _, app := App} = Request) ->
    case collect_supervision_tree(App, application_source(Request)) of
        {ok, Data, _} -> Data;
        {unavailable, Reason, Data} -> Data#{status => unavailable, reason_code => Reason};
        {error, Reason, Data} -> Data#{status => error, reason_code => Reason}
    end;
diagnostic_application(_Request) ->
    #{status => unavailable, reason_code => application_not_requested}.

-spec diagnostic_scheduler_flag(boolean()) -> term().
diagnostic_scheduler_flag(Enabled) ->
    erlang:system_flag(scheduler_wall_time, Enabled).

-spec diagnostic_scheduler_sample() -> map().
diagnostic_scheduler_sample() ->
    scheduler_sample().

-spec diagnostic_scheduler_window(map(), map()) -> map().
diagnostic_scheduler_window(First, Second) ->
    scheduler_window(First, Second).

-spec diagnostic_socket_trend([map()]) -> map().
diagnostic_socket_trend(ValueMaps) ->
    socket_series_trend(ValueMaps).

-spec diagnostic_binary_holders(map(), map()) -> map().
diagnostic_binary_holders(Request, Context) ->
    Source = process_source(Request),
    case admit_process_scan(Source, binary_memory, 1, 1, {top, 20}) of
        {ok, Admission} ->
            Acc = fold_processes(
                Source,
                fun(Pid, State) ->
                    case scan_process(Pid, [binary], Source, State) of
                        {ok, Item, Next} ->
                            Bytes = maps:get(binary_memory, Item, 0),
                            Ranked = insert_top(
                                #{
                                    raw_pid => Pid,
                                    pid => {identifier, pid, Pid},
                                    binary_reference_bytes => Bytes
                                },
                                binary_reference_bytes,
                                20,
                                maps:get(top, Next)
                            ),
                            Next#{top := Ranked};
                        {skip, Next} ->
                            Next
                    end
                end,
                (inventory_acc(Context, 20))#{top => []}
            ),
            #{
                status => ok,
                admission => Admission,
                retention_semantics => current_context_only,
                items => [maps:remove(raw_pid, Item) || Item <- maps:get(top, Acc)]
            };
        {unavailable, Details} ->
            Details
    end.

-spec diagnostic_distribution(pid()) -> map().
diagnostic_distribution(Controller) ->
    {ok, Distribution, _Coverage} = distribution_probe(Controller),
    Distribution.

diagnostic_process_inventory(Request, Context) ->
    Source = process_source(Request),
    Samples =
        case maps:get(deep, Request, false) of
            true ->
                7;
            false ->
                case maps:is_key(observe, Request) of
                    true -> 5;
                    false -> 2
                end
        end,
    case admit_process_scan(Source, reductions, 3, Samples, all) of
        {ok, Admission} ->
            Started = erlang:monotonic_time(millisecond),
            Initial = inventory_acc(Context, 1),
            Acc = fold_processes(
                Source,
                fun(Pid, State) ->
                    case
                        scan_process(Pid, [message_queue_len, memory, reductions], Source, State)
                    of
                        {ok, Item, Next} ->
                            #{
                                raw_pid := RawPid,
                                message_queue_len := Queue,
                                memory := Memory,
                                reductions := Reductions
                            } = Item,
                            Next#{
                                values =>
                                    (maps:get(values, Next, #{}))#{
                                        RawPid => #{
                                            message_queue_len => Queue,
                                            memory_bytes => Memory,
                                            reductions => Reductions
                                        }
                                    }
                            };
                        {skip, Next} ->
                            Next
                    end
                end,
                Initial#{values => #{}}
            ),
            Finished = erlang:monotonic_time(millisecond),
            #{
                status => ok,
                values => maps:get(values, Acc),
                audit => audit_inventory(Acc, maps:size(maps:get(values, Acc)), Started, Finished),
                admission => Admission
            };
        {unavailable, Details} ->
            Details
    end.

deep_snapshot_probes(#{deep := true} = Request, Context) ->
    OldTrapExit = process_flag(trap_exit, true),
    try
        [
            run_deep_probe(Command, deep_probe_request(Command, Request), Context)
         || Command <- [processes, applications, ets, mnesia, network, ports, sockets]
        ]
    after
        process_flag(trap_exit, OldTrapExit)
    end;
deep_snapshot_probes(_Request, _Context) ->
    [].

deep_probe_request(Command, Request) ->
    FixtureKeys = [
        test_process_source,
        test_application_source,
        test_ets_source,
        test_mnesia_source,
        test_network_source,
        test_port_source,
        test_socket_source,
        test_deep_probe_outcomes
    ],
    maps:merge(maps:with(FixtureKeys, Request), deep_probe_defaults(Command)).

deep_probe_defaults(processes) -> #{sort => memory, limit => 20};
deep_probe_defaults(applications) -> #{sort => memory, limit => 20};
deep_probe_defaults(ets) -> #{sort => memory, limit => 20};
deep_probe_defaults(mnesia) -> #{sort => memory, limit => 20};
deep_probe_defaults(network) -> #{sort => oct, limit => 20};
deep_probe_defaults(ports) -> #{sort => queue_size, limit => 20};
deep_probe_defaults(sockets) -> #{sort => io, limit => 20}.

run_deep_probe(Command, ProbeRequest, #{deadline := Deadline} = Context) ->
    Started = erlang:monotonic_time(millisecond),
    Outcome = deep_probe_outcome(Command, ProbeRequest, Context, Deadline),
    Finished = erlang:monotonic_time(millisecond),
    deep_probe_result(Command, Outcome, Finished - Started).

-ifdef(TEST).
deep_probe_outcome(
    Command, #{test_deep_probe_outcomes := Outcomes} = ProbeRequest, Context, Deadline
) ->
    case maps:find(Command, Outcomes) of
        {ok, Outcome} -> Outcome;
        error -> run_deep_probe_worker(Command, ProbeRequest, Context, Deadline)
    end;
deep_probe_outcome(Command, ProbeRequest, Context, Deadline) ->
    run_deep_probe_worker(Command, ProbeRequest, Context, Deadline).
-else.
deep_probe_outcome(Command, ProbeRequest, Context, Deadline) ->
    run_deep_probe_worker(Command, ProbeRequest, Context, Deadline).
-endif.

run_deep_probe_worker(Command, ProbeRequest, #{max_heap_words := MaxHeapWords} = Context, Deadline) ->
    ProbeDeadline = Deadline - ?DEEP_FINISH_MARGIN_MS,
    case remaining(ProbeDeadline) of
        0 ->
            {timeout, target_timeout};
        _ ->
            Parent = self(),
            Ref = make_ref(),
            ChildContext = Context#{deadline => ProbeDeadline, diagnostics_worker => Parent},
            {Pid, Monitor} = spawn_opt(
                fun() ->
                    Parent ! {Ref, self(), deep_probe_capture(Command, ProbeRequest, ChildContext)}
                end,
                [
                    link,
                    monitor,
                    {max_heap_size, #{size => MaxHeapWords, kill => true, error_logger => false}}
                ]
            ),
            await_deep_probe(Pid, Monitor, Ref, ProbeDeadline)
    end.

await_deep_probe(Pid, Monitor, Ref, Deadline) ->
    receive
        {Ref, Pid, Outcome} ->
            finish_deep_probe(Pid, Monitor, Outcome, Deadline);
        {'DOWN', Monitor, process, Pid, Reason} ->
            drain_exit(Pid),
            {error, deep_worker_reason(Reason)};
        {'EXIT', Pid, _Reason} ->
            await_deep_probe(Pid, Monitor, Ref, Deadline)
    after remaining(Deadline) ->
        exit(Pid, kill),
        receive
            {'DOWN', Monitor, process, Pid, _Reason} -> ok
        end,
        drain_exit(Pid),
        {timeout, target_timeout}
    end.

finish_deep_probe(Pid, Monitor, Outcome, Deadline) ->
    receive
        {'DOWN', Monitor, process, Pid, normal} ->
            drain_exit(Pid),
            Outcome;
        {'DOWN', Monitor, process, Pid, Reason} ->
            drain_exit(Pid),
            {error, deep_worker_reason(Reason)};
        {'EXIT', Pid, _Reason} ->
            finish_deep_probe(Pid, Monitor, Outcome, Deadline)
    after remaining(Deadline) ->
        exit(Pid, kill),
        receive
            {'DOWN', Monitor, process, Pid, _Reason} -> ok
        end,
        drain_exit(Pid),
        {timeout, cleanup_unconfirmed}
    end.

deep_worker_reason(killed) -> worker_heap_limit_exceeded;
deep_worker_reason(_) -> probe_failed.

deep_probe_capture(processes, Request, Context) ->
    composed_probe(capture_processes(Request, Context));
deep_probe_capture(applications, Request, Context) ->
    composed_probe(capture_applications(Request, Context));
deep_probe_capture(ets, Request, Context) ->
    composed_probe(capture_ets(Request, Context));
deep_probe_capture(mnesia, Request, Context) ->
    composed_probe(capture_mnesia(Request, Context));
deep_probe_capture(network, Request, Context) ->
    composed_probe(capture_network(Request, Context));
deep_probe_capture(ports, Request, Context) ->
    composed_probe(capture_ports(Request, Context));
deep_probe_capture(sockets, Request, Context) ->
    composed_probe(capture_sockets(Request, Context)).

composed_probe(#{
    <<"meta">> := #{<<"capture">> := #{probes := [Probe]}}, <<"data">> := Data
}) ->
    {
        maps:get(status, Probe),
        maps:get(reason_code, Probe),
        Data,
        maps:get(samples, Probe),
        maps:get(coverage, Probe)
    };
composed_probe(_Invalid) ->
    {error, invalid_probe_result}.

deep_probe_result(Id, {ok, _Reason, Data, Samples, Coverage}, Duration) ->
    {probe_report(Id, false, ok, null, Duration, Samples, Coverage), Data};
deep_probe_result(Id, {unavailable, Reason, Data, Samples, Coverage}, Duration) ->
    {probe_report(Id, false, unavailable, Reason, Duration, Samples, Coverage), Data};
deep_probe_result(Id, {timeout, Reason}, Duration) ->
    {probe_report(Id, false, timeout, Reason, Duration, 0, []), undefined};
deep_probe_result(Id, {error, Reason}, Duration) ->
    {probe_report(Id, false, error, Reason, Duration, 0, []), undefined};
deep_probe_result(Id, _Invalid, Duration) ->
    {probe_report(Id, false, error, invalid_probe_result, Duration, 0, []), undefined}.

capture_memory(Request, #{deadline := Deadline, controller := Controller}) when is_map(Request) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = module_loaded(),
    {ok, Runtime, _} = runtime_probe(),
    Probes = [
        run_snapshot_probe(
            memory, true, fun() -> memory_command_probe(Runtime) end, Request, Deadline
        ),
        run_snapshot_probe(allocator, true, fun allocator_probe/0, Request, Deadline)
    ],
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    ProbeReports = [Report || {Report, _Data} <- Probes],
    MemoryData = memory_command_data(Probes, Runtime),
    Status = capture_status(ProbeReports),
    observer_cli_cli:response(
        memory,
        Status,
        target_from_runtime(Runtime),
        #{
            started_at => rfc3339(StartedAt),
            finished_at => rfc3339(FinishedAt),
            duration_ms => FinishedMonotonic - StartedMonotonic,
            probes => ProbeReports,
            observer_effects => observer_effects(ModuleLoaded, Controller)
        },
        MemoryData,
        []
    );
capture_memory(_Request, _Context) ->
    {probe_error, invalid_request}.

memory_command_probe(Runtime) ->
    {ok, Memory, _} = memory_probe(),
    {ok, #{runtime => Runtime, memory => maps:with([beam, persistent_term], Memory)}, [
        target_identity, otp_runtime, beam_memory, persistent_term_summary
    ]}.

allocator_probe() ->
    {ok, allocator_data(observer_cli_system:collect_allocator_info()), [
        allocator_average_block_sizes, allocator_sbcs_to_mbcs, allocator_cache_hit_rates
    ]}.

memory_command_data(Probes, Runtime) ->
    case probe_data(memory, Probes) of
        #{memory := Memory} = Data ->
            Data#{memory := Memory#{allocator => allocator_probe_data(Probes)}};
        _ ->
            #{runtime => Runtime, memory => #{allocator => allocator_probe_data(Probes)}}
    end.

allocator_probe_data(Probes) ->
    case probe_data(allocator, Probes) of
        Data when is_map(Data) -> Data;
        _ -> null
    end.

allocator_data(#{
    average_block_curs := Current,
    average_block_maxes := Max,
    sbcs_to_mbcs_curs := CurrentRatios,
    sbcs_to_mbcs_maxes := MaxRatios,
    cache_hit_info := CacheHitInfo
}) ->
    CurrentMap = maps:from_list(Current),
    MaxMap = maps:from_list(Max),
    CurrentRatioMap = maps:from_list(CurrentRatios),
    MaxRatioMap = maps:from_list(MaxRatios),
    #{
        util_allocators => [
            allocator_row(Type, CurrentMap, MaxMap, CurrentRatioMap, MaxRatioMap)
         || Type <- lists:sort(maps:keys(CurrentMap))
        ],
        cache_hit_rates => [cache_hit_row(Item) || Item <- lists:keysort(1, CacheHitInfo)]
    }.

allocator_row(Type, Current, Max, CurrentRatios, MaxRatios) ->
    CurrentValues = maps:get(Type, Current, []),
    MaxValues = maps:get(Type, Max, []),
    #{
        allocator => Type,
        current_mbcs_average_block_size_bytes => proplists:get_value(mbcs, CurrentValues, null),
        max_mbcs_average_block_size_bytes => proplists:get_value(mbcs, MaxValues, null),
        current_sbcs_average_block_size_bytes => proplists:get_value(sbcs, CurrentValues, null),
        max_sbcs_average_block_size_bytes => proplists:get_value(sbcs, MaxValues, null),
        current_sbcs_to_mbcs_ratio => maps:get(Type, CurrentRatios, null),
        max_sbcs_to_mbcs_ratio => maps:get(Type, MaxRatios, null)
    }.

cache_hit_row({{instance, Instance}, Values}) ->
    #{
        instance => Instance,
        hits => proplists:get_value(hits, Values),
        calls => proplists:get_value(calls, Values),
        cache_hit_rate => proplists:get_value(hit_rate, Values)
    }.

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
            Keys =
                case Duration of
                    undefined -> process_inventory_keys(Sort);
                    _ -> process_sample_keys(Sort)
                end,
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

capture_port(#{target := Target} = Request, Context) ->
    Source = port_source(Request),
    capture_scan_inspection(
        port, port_info, 1, Context, fun() -> collect_port(Target, Source) end
    );
capture_port(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_otp_state(#{target := Target, behavior := Behavior} = Request, Context) ->
    case otp_state_limit(Behavior, Request) of
        {ok, Limit} ->
            Source = state_source(Request),
            Response = capture_scan_inspection(
                otp_state,
                otp_state,
                1,
                Context,
                fun() -> collect_otp_state(Target, Behavior, Limit, Source) end
            ),
            Response#{<<"issues">> := risk_issues(state_risk_warnings())};
        error ->
            {probe_error, invalid_request}
    end;
capture_otp_state(_Request, _Context) ->
    {probe_error, invalid_request}.

otp_state_limit(gen_event, Request) ->
    Limit = maps:get(limit, Request, 20),
    case valid_list_limit(Limit) of
        true -> {ok, Limit};
        false -> error
    end;
otp_state_limit(Behavior, Request) when Behavior =:= gen_server; Behavior =:= gen_statem ->
    case maps:is_key(limit, Request) of
        true -> error;
        false -> {ok, undefined}
    end;
otp_state_limit(_Behavior, _Request) ->
    error.

capture_supervision_tree(#{app := App} = Request, Context) ->
    Source = application_source(Request),
    Response = capture_scan_inspection(
        supervision_tree,
        supervision_tree,
        1,
        Context,
        fun() -> collect_supervision_tree(App, Source) end
    ),
    Response#{<<"issues">> := risk_issues(supervision_tree_warnings())};
capture_supervision_tree(_Request, _Context) ->
    {probe_error, invalid_request}.

collect_supervision_tree(AppText, Source) ->
    Risk = supervision_tree_risk(),
    case resolve_loaded_application(AppText, (maps:get(loaded_fun, Source))()) of
        not_found ->
            {ok, Risk#{status => not_found, root => null, children => []}, [
                public_application_supervisor, one_level_only
            ]};
        {ok, App} ->
            collect_application_root(App, Source, Risk)
    end.

collect_application_root(App, Source, Risk) ->
    try (maps:get(supervisor_fun, Source))(App) of
        undefined ->
            {ok,
                Risk#{
                    status => not_running,
                    application => {identifier, application, App},
                    root => null,
                    children => []
                },
                [public_application_supervisor, one_level_only]};
        {ok, Root} when is_pid(Root), node(Root) =:= node() ->
            case (maps:get(alive_fun, Source))(Root) of
                true ->
                    collect_root_children(App, Root, Source, Risk);
                false ->
                    {ok,
                        Risk#{
                            status => not_running,
                            application => {identifier, application, App},
                            root => null,
                            children => []
                        },
                        [public_application_supervisor, local_live_root, one_level_only]}
            end;
        {ok, Root} when is_pid(Root) ->
            {error, remote_supervisor_root, Risk#{
                status => error, reason_code => remote_supervisor_root
            }};
        _Other ->
            {error, supervisor_resolution_failed, Risk#{
                status => error, reason_code => supervisor_resolution_failed
            }}
    catch
        _Class:_Reason:_Stacktrace ->
            {error, supervisor_resolution_failed, Risk#{
                status => error, reason_code => supervisor_resolution_failed
            }}
    end.

collect_root_children(App, Root, Source, Risk) ->
    try (maps:get(count_children_fun, Source))(Root) of
        Counts ->
            case supervisor_counts(Counts) of
                {ok, CountMap} ->
                    preflight_root_children(App, Root, Source, Risk, CountMap);
                error ->
                    {error, supervisor_count_failed, Risk#{
                        status => error, reason_code => supervisor_count_failed
                    }}
            end
    catch
        _Class:_Reason:_Stacktrace ->
            {error, supervisor_count_failed, Risk#{
                status => error, reason_code => supervisor_count_failed
            }}
    end.

preflight_root_children(App, Root, Source, Risk, Counts) ->
    Observed = max(maps:get(active, Counts), maps:get(specs, Counts)),
    case Observed =< ?SUPERVISOR_SCAN_BUDGET of
        false ->
            supervisor_scan_refusal(App, Root, Risk, Counts, Observed);
        true ->
            collect_admitted_root_children(App, Root, Source, Risk, Counts)
    end.

collect_admitted_root_children(App, Root, Source, Risk, Counts) ->
    try (maps:get(which_children_fun, Source))(Root) of
        Children ->
            case bounded_supervisor_child_count(Children, 0) of
                {ok, Observed} ->
                    collect_bounded_root_children(
                        App, Root, Source, Risk, Counts, Children, Observed
                    );
                exceeded ->
                    supervisor_scan_refusal(
                        App, Root, Risk, Counts, ?SUPERVISOR_SCAN_BUDGET + 1
                    );
                error ->
                    supervisor_children_error(Risk)
            end
    catch
        _Class:_Reason:_Stacktrace ->
            supervisor_children_error(Risk)
    end.

bounded_supervisor_child_count([_Child | _Rest], ?SUPERVISOR_SCAN_BUDGET) ->
    exceeded;
bounded_supervisor_child_count([_Child | Rest], Count) ->
    bounded_supervisor_child_count(Rest, Count + 1);
bounded_supervisor_child_count([], Count) ->
    {ok, Count};
bounded_supervisor_child_count(_Improper, _Count) ->
    error.

collect_bounded_root_children(App, Root, Source, Risk, Counts, Children, Observed) ->
    case valid_supervisor_children(Children) of
        true ->
            ChildCounts = child_identity_counts(Children),
            Returned = lists:sublist(Children, ?SUPERVISOR_OUTPUT_CAP),
            Items = [supervision_child(Child, ChildCounts, Source) || Child <- Returned],
            Unavailable = length([
                unavailable
             || {Id, _Child, _Type, _Modules} <- Children,
                not child_identity_available(Id, ChildCounts)
            ]),
            {ok,
                Risk#{
                    status => ok,
                    application => {identifier, application, App},
                    root => {identifier, pid, Root},
                    preflight => Counts,
                    observed_child_count => Observed,
                    returned_count => length(Items),
                    dropped_count => Observed - length(Items),
                    identity_unavailable_count => Unavailable,
                    children => Items
                },
                [
                    public_application_supervisor,
                    local_live_root,
                    count_children_preflight,
                    direct_children_only,
                    soft_output_cap
                ]};
        false ->
            supervisor_children_error(Risk)
    end.

supervisor_scan_refusal(App, Root, Risk, Counts, Observed) ->
    {unavailable, scan_budget_exceeded, Risk#{
        status => unavailable,
        reason_code => scan_budget_exceeded,
        application => {identifier, application, App},
        root => {identifier, pid, Root},
        preflight => Counts,
        observed_child_count => Observed,
        scan_budget_count => ?SUPERVISOR_SCAN_BUDGET,
        children => []
    }}.

supervisor_children_error(Risk) ->
    {error, supervisor_children_failed, Risk#{
        status => error, reason_code => supervisor_children_failed
    }}.

supervisor_counts(Counts) when is_list(Counts) ->
    Values = maps:from_list(Counts),
    case Values of
        #{specs := Specs, active := Active, supervisors := Supervisors, workers := Workers} when
            is_integer(Specs),
            Specs >= 0,
            is_integer(Active),
            Active >= 0,
            is_integer(Supervisors),
            Supervisors >= 0,
            is_integer(Workers),
            Workers >= 0
        ->
            {ok, #{
                specs => Specs, active => Active, supervisors => Supervisors, workers => Workers
            }};
        _ ->
            error
    end;
supervisor_counts(_Counts) ->
    error.

valid_supervisor_children(Children) ->
    lists:all(
        fun
            ({_Id, Child, Type, _Modules}) when
                (is_pid(Child) orelse Child =:= restarting orelse Child =:= undefined) andalso
                    (Type =:= worker orelse Type =:= supervisor)
            ->
                true;
            (_) ->
                false
        end,
        Children
    ).

child_identity_counts(Children) ->
    lists:foldl(
        fun({Id, _Child, _Type, _Modules}, Acc) ->
            case child_identity(Id) of
                {ok, Key, _} -> Acc#{Key => maps:get(Key, Acc, 0) + 1};
                {error, _Reason} -> Acc
            end
        end,
        #{},
        Children
    ).

supervision_child({Id, Child, Type, _Modules}, Counts, Source) ->
    (child_identity_item(Id, Counts))#{
        type => Type,
        child => child_pid_item(Child, Source),
        leaf => true,
        churn_semantics => aggregate_only
    }.

child_identity_available(Id, Counts) ->
    case child_identity(Id) of
        {ok, Key, _Type} -> maps:get(Key, Counts) =:= 1;
        {error, _Reason} -> false
    end.

child_identity_item(Id, Counts) ->
    case child_identity(Id) of
        {ok, Key, Type} ->
            case maps:get(Key, Counts) of
                1 -> #{identity => available, id_type => Type, id => {identifier, child, Key}};
                _ -> #{identity => unavailable, identity_reason => duplicate_id, id => null}
            end;
        {error, Reason} ->
            #{identity => unavailable, identity_reason => Reason, id => null}
    end.

child_identity(undefined) ->
    {error, dynamic_id};
child_identity(Id) when is_atom(Id) ->
    bounded_child_identity(atom, atom_to_binary(Id));
child_identity(Id) when is_binary(Id) ->
    bounded_child_identity(binary, Id);
child_identity(Id) when
    is_integer(Id),
    Id >= -(1 bsl ?CHILD_ID_MAX_BITS),
    Id =< (1 bsl ?CHILD_ID_MAX_BITS)
->
    bounded_child_identity(integer, integer_to_binary(Id));
child_identity(Id) when is_integer(Id) ->
    {error, oversized_id};
child_identity(_Id) ->
    {error, complex_id}.

bounded_child_identity(Type, Value) ->
    Encoded = <<(atom_to_binary(Type))/binary, $:, Value/binary>>,
    case byte_size(Encoded) =< ?CHILD_ID_MAX_BYTES of
        true -> {ok, Encoded, Type};
        false -> {error, oversized_id}
    end.

child_pid_item(Pid, Source) when is_pid(Pid), node(Pid) =:= node() ->
    #{
        pid => {identifier, pid, Pid},
        location => local,
        state =>
            case (maps:get(alive_fun, Source))(Pid) of
                true -> alive;
                false -> dead
            end
    };
child_pid_item(Pid, _Source) when is_pid(Pid) ->
    #{pid => {identifier, pid, Pid}, location => remote, state => remote};
child_pid_item(restarting, _Source) ->
    #{pid => null, location => local, state => restarting};
child_pid_item(undefined, _Source) ->
    #{pid => null, location => local, state => undefined}.

resolve_loaded_application(AppText, Loaded) ->
    case application_name_binary(AppText) of
        {ok, Name} ->
            case [App || {App, _Description, _Version} <- Loaded, atom_to_binary(App) =:= Name] of
                [App] -> {ok, App};
                _ -> not_found
            end;
        error ->
            not_found
    end.

application_name_binary(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 255 ->
    case unicode:characters_to_binary(Name) of
        Name -> {ok, Name};
        _ -> error
    end;
application_name_binary(Name) when is_list(Name), Name =/= [], length(Name) =< 255 ->
    case unicode:characters_to_binary(Name) of
        Binary when is_binary(Binary), byte_size(Binary) =< 255 -> {ok, Binary};
        _ -> error
    end;
application_name_binary(_Name) ->
    error.

supervision_tree_risk() ->
    #{
        risk_level => high,
        scope => root_and_direct_children,
        depth => 1,
        acquisition => #{
            count_children_complexity => o_children,
            which_children_complexity => o_children,
            calls_are_infinity => true,
            deadline_retracts_delivered_request => false,
            snapshot_atomic => false
        },
        limits => #{
            scan_budget_count => ?SUPERVISOR_SCAN_BUDGET,
            output_child_count => ?SUPERVISOR_OUTPUT_CAP,
            child_id_canonical_bytes => ?CHILD_ID_MAX_BYTES
        },
        correlation => #{ambiguous_identity => aggregate_only}
    }.

supervision_tree_warnings() ->
    [
        #{reason_code => count_children_preflight_is_o_children},
        #{reason_code => supervisor_snapshot_is_non_atomic},
        #{reason_code => deadline_does_not_retract_infinity_calls},
        #{reason_code => direct_child_limit_is_output_soft_cap}
    ].

risk_issues(Warnings) ->
    [
        #{
            <<"severity">> => <<"warning">>,
            <<"class">> => <<"safety_refusal">>,
            <<"reason_code">> => atom_to_binary(Reason),
            <<"message">> => null
        }
     || #{reason_code := Reason} <- Warnings
    ].

collect_otp_state(Target, Behavior, Limit, Source) ->
    Base = otp_state_base(Behavior, Limit),
    ProcessSource = maps:get(process_source, Source),
    case resolve_process_target(Target, ProcessSource) of
        not_found ->
            {ok, Base#{status => not_found}, [target_side_resolution, no_atom_creation]};
        {ok, Pid} ->
            try (maps:get(get_state_fun, Source))(Pid, ?STATE_TIMEOUT_MS) of
                State ->
                    collect_otp_state_value(Behavior, State, Base)
            catch
                Class:Reason:_Stacktrace ->
                    otp_state_acquisition_failure(Pid, ProcessSource, Class, Reason, Base)
            end
    end.

otp_state_base(gen_server, _Limit) ->
    (state_risk_data())#{
        behavior => gen_server,
        behavior_source => operator_asserted,
        structural_validation => not_applicable,
        truncated => false,
        truncation_reason => null,
        visited_node_count => 0,
        state_shape => null
    };
otp_state_base(gen_statem, _Limit) ->
    (state_risk_data())#{
        behavior => gen_statem,
        behavior_source => operator_asserted,
        structural_validation => not_performed,
        truncated => false,
        truncation_reason => null,
        visited_node_count => 0,
        current_state => null,
        current_state_identity => unavailable,
        current_state_shape => null,
        data_shape => null
    };
otp_state_base(gen_event, Limit) ->
    Risk = state_risk_data(),
    Limits = (maps:get(limits, Risk))#{
        handler_output_count => Limit, max_handler_output_count => 200
    },
    Risk#{
        behavior => gen_event,
        behavior_source => operator_asserted,
        structural_validation => not_performed,
        limits := Limits,
        truncated => false,
        truncation_reason => null,
        visited_node_count => 0,
        observed_handler_count => 0,
        returned_count => 0,
        dropped_count => 0,
        shape_budget_exhausted_count => 0,
        handlers => []
    }.

collect_otp_state_value(gen_server, State, Base) ->
    case budgeted_state_shape(State, state_shape_acc()) of
        {Tag, Shape, Acc} when Tag =:= ok; Tag =:= exhausted ->
            otp_state_success(Base, Acc, #{state_shape => Shape});
        {error, Reason} ->
            otp_state_error(Base, Reason)
    end;
collect_otp_state_value(gen_statem, {CurrentState, Data}, Base) ->
    Acc0 = state_shape_acc(),
    case budgeted_state_shape(CurrentState, Acc0) of
        {Tag, CurrentShape, Acc1} when Tag =:= ok; Tag =:= exhausted ->
            case remaining_state_shape(Data, Acc1) of
                {ok, DataShape, Acc2} ->
                    {Identity, PublicState} = state_identity(CurrentState),
                    otp_state_success(
                        Base#{structural_validation := passed},
                        Acc2,
                        #{
                            current_state => PublicState,
                            current_state_identity => Identity,
                            current_state_shape => CurrentShape,
                            data_shape => DataShape
                        }
                    );
                {error, Reason} ->
                    otp_state_error(Base#{structural_validation := passed}, Reason)
            end;
        {error, Reason} ->
            otp_state_error(Base#{structural_validation := passed}, Reason)
    end;
collect_otp_state_value(gen_statem, _State, Base) ->
    otp_state_mismatch(Base);
collect_otp_state_value(gen_event, Handlers, Base) ->
    case event_handler_count(Handlers, 0) of
        {ok, Count} ->
            collect_event_state(Handlers, Count, Base#{structural_validation := passed});
        error ->
            otp_state_mismatch(Base)
    end.

collect_event_state(Handlers, Observed, Base) ->
    Limit = maps:get(handler_output_count, maps:get(limits, Base)),
    Planned = lists:sublist(Handlers, Limit),
    PlannedCount = length(Planned),
    case collect_event_handlers(Planned, 1, state_shape_acc(), []) of
        {ok, Items, Acc0, Exhaustion} ->
            Returned = length(Items),
            BudgetExhausted = Exhaustion =/= none,
            Acc =
                case Observed > Returned andalso not BudgetExhausted of
                    true -> mark_shape_truncation(Acc0, output_cap);
                    false -> Acc0
                end,
            ExhaustedCount =
                case Exhaustion of
                    included -> PlannedCount - Returned + 1;
                    excluded -> PlannedCount - Returned;
                    none -> 0
                end,
            otp_state_success(Base, Acc, #{
                observed_handler_count => Observed,
                returned_count => Returned,
                dropped_count => Observed - Returned,
                shape_budget_exhausted_count => ExhaustedCount,
                handlers => Items
            });
        {error, Reason} ->
            otp_state_error(Base, Reason)
    end.

collect_event_handlers([], _Index, Acc, Items) ->
    {ok, lists:reverse(Items), Acc, none};
collect_event_handlers([{Module, Id, State} | Rest], Index, Acc0, Items) ->
    case budgeted_state_shape(State, Acc0) of
        {ok, Shape, Acc} ->
            collect_event_handlers(
                Rest, Index + 1, Acc, [event_handler(Index, Module, Id, Shape) | Items]
            );
        {exhausted, null, Acc} ->
            {ok, lists:reverse(Items), Acc, excluded};
        {exhausted, Shape, Acc} ->
            {ok, lists:reverse([event_handler(Index, Module, Id, Shape) | Items]), Acc, included};
        {error, Reason} ->
            {error, Reason}
    end.

event_handler(Index, Module, Id, Shape) ->
    {Identity, PublicId} = state_identity(Id),
    #{
        index => Index,
        module => {identifier, module, Module},
        id => PublicId,
        id_identity => Identity,
        state_shape => Shape
    }.

event_handler_count([], Count) ->
    {ok, Count};
event_handler_count([{Module, _Id, _State} | Rest], Count) when is_atom(Module) ->
    event_handler_count(Rest, Count + 1);
event_handler_count(_Handlers, _Count) ->
    error.

remaining_state_shape(_State, Acc) when
    map_get(truncation_reason, Acc) =:= node_cap;
    map_get(truncation_reason, Acc) =:= output_cap
->
    {ok, null, Acc};
remaining_state_shape(State, Acc) ->
    case budgeted_state_shape(State, Acc) of
        {Tag, Shape, Next} when Tag =:= ok; Tag =:= exhausted -> {ok, Shape, Next};
        {error, Reason} -> {error, Reason}
    end.

otp_state_success(Base, Acc, Fields) ->
    Reason = maps:get(truncation_reason, Acc),
    {ok,
        maps:merge(
            Base#{
                status => ok,
                truncated => Reason =/= null,
                truncation_reason => Reason,
                visited_node_count => maps:get(nodes, Acc)
            },
            Fields
        ),
        [
            target_side_resolution,
            operator_asserted_behavior,
            target_side_value_free_shape,
            bounded_state_shape
        ]}.

otp_state_mismatch(Base) ->
    otp_state_error(Base#{structural_validation := failed}, behavior_shape_mismatch).

otp_state_error(Base, Reason) ->
    {error, Reason, Base#{status => error, reason_code => Reason}}.

otp_state_acquisition_failure(Pid, Source, Class, Reason, Base) ->
    case target_alive(Pid, Source) of
        false ->
            {ok, Base#{status => not_found}, [target_side_resolution, no_atom_creation]};
        true when Class =:= exit, element(1, Reason) =:= timeout ->
            otp_state_error(Base, state_timeout);
        true when Class =:= exit, Reason =:= timeout ->
            otp_state_error(Base, state_timeout);
        true ->
            otp_state_error(Base, state_probe_failed)
    end.

target_alive(Pid, Source) ->
    try (maps:get(alive_fun, Source))(Pid) of
        false -> false;
        _ -> true
    catch
        _:_ -> true
    end.

state_identity(Value) when is_atom(Value) ->
    bounded_state_identity(atom, atom_to_binary(Value));
state_identity(Value) when is_binary(Value), byte_size(Value) =< ?CHILD_ID_MAX_BYTES ->
    case unicode:characters_to_binary(Value) of
        Value -> bounded_state_identity(binary, Value);
        _ -> {unavailable, null}
    end;
state_identity(Value) when
    is_integer(Value),
    Value >= -(1 bsl ?CHILD_ID_MAX_BITS),
    Value =< (1 bsl ?CHILD_ID_MAX_BITS)
->
    bounded_state_identity(integer, integer_to_binary(Value));
state_identity(_Value) ->
    {unavailable, null}.

bounded_state_identity(Type, Value) ->
    Encoded = <<(atom_to_binary(Type))/binary, $:, Value/binary>>,
    case byte_size(Encoded) =< ?CHILD_ID_MAX_BYTES of
        true -> {available, {identifier, label, Encoded}};
        false -> {unavailable, null}
    end.

state_risk_data() ->
    #{
        risk_level => high,
        acquisition => #{
            full_state_copy_risk => true,
            timeout_ms => ?STATE_TIMEOUT_MS,
            timeout_retracts_delivered_request => false
        },
        limits => #{
            output_bytes => ?STATE_SHAPE_MAX_BYTES,
            depth => ?STATE_SHAPE_MAX_DEPTH,
            nodes => ?STATE_SHAPE_MAX_NODES,
            container_prefix => ?STATE_SHAPE_PREFIX,
            semantic_identifier_bytes => ?CHILD_ID_MAX_BYTES
        }
    }.

state_risk_warnings() ->
    [
        #{reason_code => sys_get_state_copies_full_state},
        #{reason_code => timeout_does_not_retract_delivered_request}
    ].

-ifdef(TEST).
state_shape(State) ->
    case budgeted_state_shape(State, state_shape_acc()) of
        {Tag, Shape, Acc} when Tag =:= ok; Tag =:= exhausted ->
            {ok, Shape, maps:get(nodes, Acc)};
        {error, Reason} ->
            {error, Reason}
    end.
-endif.

state_shape_acc() ->
    #{nodes => 0, bytes => 0, truncation_reason => null}.

budgeted_state_shape(State, Acc0) ->
    {Shape, Acc1} = shape_term(State, 0, Acc0),
    case normalize(Shape, include) of
        {ok, Normalized} ->
            Bytes = erlang:external_size(Normalized),
            case maps:get(bytes, Acc1) + Bytes =< ?STATE_SHAPE_MAX_BYTES of
                true ->
                    Acc = Acc1#{bytes := maps:get(bytes, Acc1) + Bytes},
                    case shape_budget_exhausted(Acc) of
                        true -> {exhausted, Shape, Acc};
                        false -> {ok, Shape, Acc}
                    end;
                false ->
                    Acc = Acc1#{bytes := maps:get(bytes, Acc0)},
                    {exhausted, null, mark_shape_truncation(Acc, output_cap)}
            end;
        {error, _Reason} ->
            {error, state_shape_failed}
    end.

shape_budget_exhausted(Acc) ->
    Reason = maps:get(truncation_reason, Acc),
    Reason =:= node_cap orelse Reason =:= output_cap.

mark_shape_truncation(Acc, node_cap) ->
    Acc#{truncation_reason => node_cap};
mark_shape_truncation(#{truncation_reason := node_cap} = Acc, _Reason) ->
    Acc;
mark_shape_truncation(Acc, output_cap) ->
    Acc#{truncation_reason => output_cap};
mark_shape_truncation(#{truncation_reason := null} = Acc, depth_cap) ->
    Acc#{truncation_reason := depth_cap};
mark_shape_truncation(Acc, depth_cap) ->
    Acc.

shape_term(_Term, _Depth, #{nodes := Nodes} = Acc) when Nodes >= ?STATE_SHAPE_MAX_NODES ->
    {#{type => truncated, truncation_reason => node_cap}, mark_shape_truncation(Acc, node_cap)};
shape_term(Term, Depth, Acc0) ->
    Acc = Acc0#{nodes := maps:get(nodes, Acc0) + 1},
    case Depth >= ?STATE_SHAPE_MAX_DEPTH of
        true ->
            {
                #{type => shape_type(Term), truncated => true, truncation_reason => depth_cap},
                mark_shape_truncation(Acc, depth_cap)
            };
        false ->
            shape_value(Term, Depth, Acc)
    end.

shape_value(Term, _Depth, Acc) when is_atom(Term) -> {#{type => atom}, Acc};
shape_value(Term, _Depth, Acc) when is_integer(Term); is_float(Term) ->
    {#{type => number}, Acc};
shape_value(Term, _Depth, Acc) when is_binary(Term) ->
    {#{type => binary, size_bytes => byte_size(Term)}, Acc};
shape_value(Term, _Depth, Acc) when is_bitstring(Term) ->
    {#{type => bitstring, size_bits => bit_size(Term)}, Acc};
shape_value(Term, Depth, Acc) when is_map(Term) ->
    {Children, Acc1} = shape_children(
        map_prefix_values(maps:iterator(Term), ?STATE_SHAPE_PREFIX, []),
        Depth + 1,
        Acc,
        []
    ),
    Size = map_size(Term),
    {container_shape(map, Size, Children, Size > length(Children), Acc1), Acc1};
shape_value(Term, Depth, Acc) when is_tuple(Term) ->
    Size = tuple_size(Term),
    Values = [element(Index, Term) || Index <- lists:seq(1, min(Size, ?STATE_SHAPE_PREFIX))],
    {Children, Acc1} = shape_children(Values, Depth + 1, Acc, []),
    {container_shape(tuple, Size, Children, Size > length(Children), Acc1), Acc1};
shape_value(Term, Depth, Acc) when is_list(Term) ->
    {Children, Size, Complete, Acc1} = shape_list(Term, Depth + 1, Acc, [], 0),
    {container_shape(list, Size, Children, not Complete, Acc1), Acc1};
shape_value(_Term, _Depth, Acc) ->
    {#{type => other}, Acc}.

map_prefix_values(_Iterator, 0, Values) ->
    lists:reverse(Values);
map_prefix_values(Iterator, Remaining, Values) ->
    case maps:next(Iterator) of
        {_Key, Value, Next} -> map_prefix_values(Next, Remaining - 1, [Value | Values]);
        none -> lists:reverse(Values)
    end.

shape_children([], _Depth, Acc, Children) ->
    {lists:reverse(Children), Acc};
shape_children(_Values, _Depth, #{nodes := Nodes} = Acc, Children) when
    Nodes >= ?STATE_SHAPE_MAX_NODES
->
    {lists:reverse(Children), mark_shape_truncation(Acc, node_cap)};
shape_children([Value | Rest], Depth, Acc, Children) ->
    {Shape, Acc1} = shape_term(Value, Depth, Acc),
    shape_children(Rest, Depth, Acc1, [Shape | Children]).

shape_list([], _Depth, Acc, Children, Size) ->
    {lists:reverse(Children), Size, true, Acc};
shape_list(_List, _Depth, #{nodes := Nodes} = Acc, Children, _Size) when
    Nodes >= ?STATE_SHAPE_MAX_NODES
->
    {lists:reverse(Children), null, false, mark_shape_truncation(Acc, node_cap)};
shape_list([Value | Rest], Depth, Acc, Children, Size) when Size < ?STATE_SHAPE_PREFIX ->
    {Shape, Acc1} = shape_term(Value, Depth, Acc),
    shape_list(Rest, Depth, Acc1, [Shape | Children], Size + 1);
shape_list([_Value | Rest], Depth, Acc0, Children, Size) ->
    Acc = Acc0#{nodes := maps:get(nodes, Acc0) + 1},
    shape_list(Rest, Depth, Acc, Children, Size + 1);
shape_list(_Improper, _Depth, Acc, Children, _Size) ->
    {lists:reverse(Children), null, false, Acc}.

container_shape(Type, Size, Children, Truncated0, #{nodes := Nodes}) ->
    Truncated = Truncated0 orelse Nodes >= ?STATE_SHAPE_MAX_NODES,
    Base = #{
        type => Type,
        size => Size,
        children => Children,
        returned_count => length(Children),
        truncated => Truncated
    },
    case Nodes >= ?STATE_SHAPE_MAX_NODES of
        true -> Base#{truncation_reason => node_cap};
        false -> Base
    end.

shape_type(Term) when is_atom(Term) -> atom;
shape_type(Term) when is_integer(Term); is_float(Term) -> number;
shape_type(Term) when is_binary(Term) -> binary;
shape_type(Term) when is_bitstring(Term) -> bitstring;
shape_type(Term) when is_map(Term) -> map;
shape_type(Term) when is_tuple(Term) -> tuple;
shape_type(Term) when is_list(Term) -> list;
shape_type(_Term) -> other.

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

capture_ets(Request, Context) when is_map(Request) ->
    Sort = maps:get(sort, Request, memory),
    Limit = maps:get(limit, Request, 20),
    case valid_table_request(Sort, Limit) of
        true ->
            Source = ets_source(Request),
            Count = (maps:get(count_fun, Source))(),
            Estimate = working_set_estimate(min(Count, Limit), 10, 1),
            Outcome =
                case Count =< ?ETS_SCAN_BUDGET andalso Estimate =< ?MAX_WORKING_SET_BYTES of
                    true ->
                        fun() -> collect_ets(Source, Sort, Limit, Context, Estimate) end;
                    false ->
                        fun() ->
                            {unavailable, scan_budget_exceeded, #{
                                status => unavailable,
                                reason_code => scan_budget_exceeded,
                                admission_stage => pre_enumeration,
                                observed_table_count => Count,
                                scan_budget_count => ?ETS_SCAN_BUDGET,
                                working_set_estimated_bytes => Estimate,
                                working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
                            }}
                        end
                end,
            capture_scan_inspection(ets, ets_inventory, 1, Context, Outcome);
        false ->
            {probe_error, invalid_request}
    end;
capture_ets(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_mnesia(Request, Context) when is_map(Request) ->
    Sort = maps:get(sort, Request, memory),
    Limit = maps:get(limit, Request, 20),
    case valid_table_request(Sort, Limit) of
        true ->
            Source = mnesia_source(Request),
            capture_scan_inspection(
                mnesia,
                mnesia_inventory,
                1,
                Context,
                fun() -> collect_mnesia(Source, Sort, Limit, Context) end
            );
        false ->
            {probe_error, invalid_request}
    end;
capture_mnesia(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_network(Request, Context) when is_map(Request) ->
    capture_counter_resources(
        network, network_inventory, Request, Context, network_source(Request)
    );
capture_network(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_ports(Request, Context) when is_map(Request) ->
    Sort = maps:get(sort, Request, queue_size),
    Limit = maps:get(limit, Request, 20),
    case valid_port_request(Sort, Limit) of
        true ->
            Source = port_source(Request),
            capture_scan_inspection(ports, port_inventory, 1, Context, fun() ->
                collect_ports(Source, Sort, Limit, Context)
            end);
        false ->
            {probe_error, invalid_request}
    end;
capture_ports(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_sockets(Request, Context) when is_map(Request) ->
    Source = socket_source(Request),
    case (maps:get(available_fun, Source))() of
        true ->
            capture_counter_resources(sockets, socket_inventory, Request, Context, Source);
        false ->
            capture_scan_inspection(sockets, socket_inventory, 0, Context, fun() ->
                {unavailable, capability_unavailable, #{
                    status => unavailable, reason_code => capability_unavailable
                }}
            end)
    end;
capture_sockets(_Request, _Context) ->
    {probe_error, invalid_request}.

capture_counter_resources(Command, ProbeId, Request, Context, Source) ->
    Sort = maps:get(sort, Request, default_counter_sort(Command)),
    Limit = maps:get(limit, Request, 20),
    Duration = maps:get(duration_ms, Request, undefined),
    case valid_counter_request(Command, Sort, Limit, Duration) of
        true ->
            Samples =
                case Duration of
                    undefined -> 1;
                    _ -> 2
                end,
            capture_scan_inspection(Command, ProbeId, Samples, Context, fun() ->
                collect_counter_resources(Command, Source, Sort, Limit, Duration, Context)
            end);
        false ->
            {probe_error, invalid_request}
    end.

capture_scan_inspection(Command, ProbeId, Samples, #{controller := Controller}, OutcomeFun) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = module_loaded(),
    Outcome = OutcomeFun(),
    {Status, Reason, Data, Coverage, ExtraEffects} =
        case Outcome of
            {ok, Value, Covered} ->
                {ok, null, Value, Covered, []};
            {unavailable, Why, Details} ->
                {unavailable, Why, Details, [admission_only], []};
            {error, Why, Details} ->
                {error, Why, Details, [], []};
            {ok, Value, Covered, Effects} ->
                {ok, null, Value, Covered, Effects};
            {unavailable, Why, Details, Covered, Effects} ->
                {unavailable, Why, Details, Covered, Effects};
            {error, Why, Details, Covered, Effects} ->
                {error, Why, Details, Covered, Effects}
        end,
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    {ok, Runtime, _} = runtime_probe(),
    observer_cli_cli:response(
        Command,
        case Status of
            ok -> complete;
            unavailable -> error;
            _ -> partial
        end,
        target_from_runtime(Runtime),
        #{
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
            observer_effects => observer_effects(ModuleLoaded, Controller) ++ ExtraEffects
        },
        Data,
        []
    ).

valid_process_request(Sort, Limit, Duration) ->
    lists:member(
        Sort, [memory, message_queue_len, reductions, binary_memory, total_heap_size]
    ) andalso
        valid_list_limit(Limit) andalso valid_sample_duration(Duration).

valid_application_request(Sort, Limit) ->
    lists:member(Sort, [memory, process_count, reductions, message_queue_len]) andalso
        valid_list_limit(Limit).

valid_table_request(Sort, Limit) ->
    lists:member(Sort, [memory, size]) andalso valid_list_limit(Limit).

valid_counter_request(network, Sort, Limit, Duration) ->
    valid_counter_values(Sort, [oct, recv_oct, send_oct, cnt, recv_cnt, send_cnt], Limit, Duration);
valid_counter_request(sockets, Sort, Limit, Duration) ->
    valid_counter_values(
        Sort, [io, read_bytes, write_bytes, packets, waits, fails], Limit, Duration
    ).

valid_counter_values(Sort, Sorts, Limit, Duration) ->
    lists:member(Sort, Sorts) andalso valid_list_limit(Limit) andalso
        valid_sample_duration(Duration).

valid_port_request(Sort, Limit) ->
    lists:member(Sort, [queue_size, memory, input, output, io]) andalso
        valid_list_limit(Limit).

valid_list_limit(Limit) ->
    is_integer(Limit) andalso Limit >= 1 andalso Limit =< 200.

valid_sample_duration(undefined) ->
    true;
valid_sample_duration(Duration) ->
    is_integer(Duration) andalso Duration >= 250 andalso Duration =< 10000.

default_counter_sort(network) -> oct;
default_counter_sort(sockets) -> io.

process_inventory_keys(binary_memory) ->
    lists:usort([binary | process_context_keys()]);
process_inventory_keys(Sort) ->
    lists:usort([Sort | process_context_keys()]).

process_context_keys() ->
    [registered_name, current_function, initial_call, memory, message_queue_len, reductions].

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
        binary,
        garbage_collection,
        garbage_collection_info,
        priority,
        links,
        monitors,
        monitored_by,
        catchlevel,
        suspending,
        error_handler,
        trap_exit,
        current_stacktrace
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
    Items = [
        public_process_item(add_process_label(Item, Source))
     || Item <- maps:get(top, Acc)
    ],
    Eligible = maps:get(eligible, Acc),
    Data = (audit_inventory(Acc, length(Items), Started, Finished))#{
        items => Items,
        dropped_count => Eligible - length(Items),
        truncated => false,
        sort => Sort,
        sort_semantics => total,
        baseline_count => 0,
        tracked_field_count => maps:get(tracked_field_count, Admission),
        retained_sample_count => 1,
        working_set_estimated_bytes => maps:get(working_set_estimated_bytes, Admission)
    },
    {ok, Data, [
        exact_top_n, recon_top_n_order, explicit_process_info_keys, process_scan_admitted
    ]};
collect_processes(Source, Sort, Limit, Duration, Context, Admission) ->
    Started = erlang:monotonic_time(millisecond),
    First = collect_process_sample(Sort, Source, Context),
    (maps:get(sleep_fun, Source))(Duration),
    Second = collect_process_sample(Sort, Source, Context),
    Finished = erlang:monotonic_time(millisecond),
    Interval = max(1, maps:get(monotonic_ms, Second) - maps:get(monotonic_ms, First)),
    Window = stable_process_window(
        maps:get(values, First), maps:get(values, Second), Interval
    ),
    Born = maps:get(born, Window),
    Dead = maps:get(dead, Window),
    Reset = maps:get(reset, Window),
    {BornPids, BornPidsTruncated} = pid_sample(Born, Limit),
    {DeadPids, DeadPidsTruncated} = pid_sample(Dead, Limit),
    {ResetPids, ResetPidsTruncated} = pid_sample(Reset, Limit),
    Ranked = rank_window(maps:get(stable, Window), Limit),
    Items = [
        public_process_item(window_process_item(Pid, Sort, Delta, Interval, Source))
     || {Pid, Delta} <- Ranked
    ],
    FirstAudit = maps:get(audit, First),
    SecondAudit = maps:get(audit, Second),
    Data = (audit_inventory(SecondAudit, length(Items), Started, Finished))#{
        items => Items,
        dropped_count => maps:size(maps:get(stable, Window)) - length(Items),
        truncated => false,
        sort => Sort,
        sort_semantics => delta,
        interval_ms => Interval,
        baseline_count => maps:size(maps:get(values, First)),
        born_count => length(Born),
        dead_count => length(Dead),
        reset_count => length(Reset),
        born_pids => BornPids,
        born_pids_truncated => BornPidsTruncated,
        dead_pids => DeadPids,
        dead_pids_truncated => DeadPidsTruncated,
        reset_pids => ResetPids,
        reset_pids_truncated => ResetPidsTruncated,
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

pid_sample(Pids, Limit) ->
    {[{identifier, pid, Pid} || Pid <- lists:sublist(Pids, Limit)], length(Pids) > Limit}.

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
    recon_top_n([Item | Items], Sort, Limit).

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
    Acc#{
        garbage_collection_info => maps:merge(
            maps:get(garbage_collection_info, Acc, #{}), allowed_gc_info(Value)
        )
    };
process_field(garbage_collection, Value, _WordSize, Acc) ->
    Acc#{
        garbage_collection_info => maps:merge(
            maps:get(garbage_collection_info, Acc, #{}), allowed_gc_info(Value)
        )
    };
process_field(binary, Binaries, _WordSize, Acc) ->
    {BinaryRefsCount, BinaryRefsBytes} = binary_ref_stats(Binaries),
    Acc#{
        binary_memory => BinaryRefsBytes,
        binary_refs_count => BinaryRefsCount,
        binary_refs_bytes => BinaryRefsBytes
    };
process_field(priority, Value, _WordSize, Acc) ->
    Acc#{priority => Value};
process_field(Key, Values, _WordSize, Acc) when
    Key =:= links; Key =:= monitors; Key =:= monitored_by
->
    bounded_process_list(Key, Values, fun sanitize_signal_list/1, Acc);
process_field(catchlevel, Value, _WordSize, Acc) ->
    Acc#{catchlevel => Value};
process_field(suspending, Values, _WordSize, Acc) ->
    bounded_process_list(suspending, Values, fun sanitize_suspending_list/1, Acc);
process_field(error_handler, Value, _WordSize, Acc) ->
    Acc#{error_handler => sanitize_error_handler(Value)};
process_field(trap_exit, Value, _WordSize, Acc) ->
    Acc#{trap_exit => Value};
process_field(current_stacktrace, Stack, _WordSize, Acc) ->
    Acc#{current_stacktrace => sanitize_stacktrace(Stack)}.

bounded_process_list(Key, Values, Sanitizer, Acc) when is_list(Values) ->
    Items = Sanitizer(Values),
    Total = length(Values),
    {CountKey, TruncatedKey} = bounded_process_list_keys(Key),
    Acc#{
        Key => Items,
        CountKey => Total,
        TruncatedKey => Total > length(Items)
    };
bounded_process_list(Key, _Values, _Sanitizer, Acc) ->
    {CountKey, TruncatedKey} = bounded_process_list_keys(Key),
    Acc#{Key => [], CountKey => 0, TruncatedKey => false}.

bounded_process_list_keys(links) -> {links_total_count, links_truncated};
bounded_process_list_keys(monitors) -> {monitors_total_count, monitors_truncated};
bounded_process_list_keys(monitored_by) -> {monitored_by_total_count, monitored_by_truncated};
bounded_process_list_keys(suspending) -> {suspending_total_count, suspending_truncated}.

binary_ref_stats(Binaries) when is_list(Binaries) ->
    lists:foldl(
        fun
            ({_Ref, Bytes, Count}, {Refs, TotalBytes}) when
                is_integer(Bytes), Bytes >= 0, is_integer(Count), Count >= 0
            ->
                {Refs + 1, TotalBytes + Bytes};
            (_Ref, Acc) ->
                Acc
        end,
        {0, 0},
        Binaries
    );
binary_ref_stats(_Binaries) ->
    {0, 0}.

sanitize_signal_list(Signals) when is_list(Signals) ->
    [sanitize_signal_item(Signal) || Signal <- lists:sublist(Signals, 30)];
sanitize_signal_list(_Signals) ->
    [].

sanitize_signal_item({process, Pid}) when is_pid(Pid) ->
    #{
        <<"type">> => <<"process">>,
        <<"target">> => {identifier, pid, Pid}
    };
sanitize_signal_item({port, Port}) when is_port(Port) ->
    #{
        <<"type">> => <<"port">>,
        <<"target">> => {identifier, port, Port}
    };
sanitize_signal_item({process, {Name, Node}}) when is_atom(Name), is_atom(Node) ->
    #{
        <<"type">> => <<"process">>,
        <<"registered_name">> => {identifier, name, Name},
        <<"node">> => {identifier, node, Node}
    };
sanitize_signal_item({port, {Name, Node}}) when is_atom(Name), is_atom(Node) ->
    #{
        <<"type">> => <<"port">>,
        <<"registered_name">> => {identifier, name, Name},
        <<"node">> => {identifier, node, Node}
    };
sanitize_signal_item(Pid) when is_pid(Pid) ->
    #{
        <<"type">> => <<"process">>,
        <<"target">> => {identifier, pid, Pid}
    };
sanitize_signal_item(Port) when is_port(Port) ->
    #{
        <<"type">> => <<"port">>,
        <<"target">> => {identifier, port, Port}
    };
sanitize_signal_item(_Other) ->
    #{<<"type">> => <<"other">>}.

sanitize_suspending_list(Suspending) when is_list(Suspending) ->
    [sanitize_suspending_item(Item) || Item <- lists:sublist(Suspending, 30)];
sanitize_suspending_list(_Suspending) ->
    [].

sanitize_suspending_item({Pid, Active, Outstanding}) when
    is_pid(Pid), is_integer(Active), Active >= 0, is_integer(Outstanding), Outstanding >= 0
->
    #{
        <<"type">> => <<"process">>,
        <<"target">> => {identifier, pid, Pid},
        <<"active_suspend_count">> => Active,
        <<"outstanding_suspend_count">> => Outstanding
    };
sanitize_suspending_item(_Item) ->
    #{<<"type">> => <<"other">>}.

sanitize_error_handler(Value) when is_atom(Value) ->
    {identifier, module, Value};
sanitize_error_handler(_Value) ->
    null.

sanitize_stacktrace(Stack) when is_list(Stack) ->
    [sanitize_stacktrace_frame(Frame) || Frame <- lists:sublist(Stack, 30)];
sanitize_stacktrace(_Stack) ->
    [].

sanitize_stacktrace_frame({Mod, Fun, ArityOrArgs, Location}) when
    is_atom(Mod), is_atom(Fun), is_list(Location)
->
    #{
        module => {identifier, module, Mod},
        function => {identifier, function, Fun},
        arity => stacktrace_arity(ArityOrArgs),
        location => sanitize_stacktrace_location(Location)
    };
sanitize_stacktrace_frame(_Frame) ->
    #{<<"type">> => <<"other">>}.

stacktrace_arity(Arity) when is_integer(Arity), Arity >= 0 -> Arity;
stacktrace_arity(Args) when is_list(Args) -> length(Args);
stacktrace_arity(_ArityOrArgs) -> null.

sanitize_stacktrace_location(Location) when is_list(Location) ->
    Line = proplists:get_value(line, Location),
    case is_integer(Line) andalso Line > 0 of
        true -> #{<<"line">> => Line};
        false -> null
    end;
sanitize_stacktrace_location(_Location) ->
    null.

allowed_gc_info(Info) ->
    Allowed = [
        min_bin_vheap_size,
        min_heap_size,
        fullsweep_after,
        minor_gcs,
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

collect_process_sample(Sort, Source, Context) ->
    SampleKeys = process_sample_keys(Sort),
    ValueKey = process_sample_key(Sort),
    Acc0 = (inventory_acc(Context, 1))#{values => #{}},
    Acc = fold_processes(
        Source,
        fun(Pid, State) ->
            case scan_process(Pid, SampleKeys, Source, State) of
                {ok, Item, Next} ->
                    case maps:find(ValueKey, Item) of
                        {ok, Value} when is_integer(Value) ->
                            Next#{values := (maps:get(values, Next))#{Pid => Value}};
                        _ ->
                            Next
                    end;
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

process_sample_keys(binary_memory) -> [binary];
process_sample_keys(Sort) -> [Sort].

process_sample_key(binary_memory) -> binary_memory;
process_sample_key(Sort) -> Sort.

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
    [
        Item
     || {_, _, [Item]} <- recon_lib:sublist_top_n_attrs(
            [{Pid, Value, [{Pid, Value}]} || {Pid, Value} <- maps:to_list(Values)], Limit
        )
    ].

window_process_item(Pid, Sort, Delta, Interval, Source) ->
    Context = process_context(Pid, Source),
    (add_process_label(Context, Source))#{
        pid => {identifier, pid, Pid},
        process_window_field_key(Sort, delta) => Delta,
        process_window_field_key(Sort, per_second) => Delta * 1000 / Interval
    }.

process_context(Pid, Source) ->
    Empty = #{
        raw_pid => Pid,
        pid => {identifier, pid, Pid},
        registered_name => null,
        current_function => null,
        initial_call => null,
        memory_bytes => null,
        message_queue_len => null,
        reductions => null
    },
    try (maps:get(info_fun, Source))(Pid, process_context_keys()) of
        Info when is_list(Info) -> maps:merge(Empty, process_item(Pid, Info));
        _ -> Empty
    catch
        _:_ -> Empty
    end.

add_process_label(Item, Source) ->
    Item#{label => process_label(maps:get(raw_pid, Item), Source)}.

process_label(Pid, Source) ->
    LabelFun = maps:get(label_fun, Source, fun default_process_label/1),
    try LabelFun(Pid) of
        undefined ->
            null;
        false ->
            null;
        Label ->
            Text = unicode:characters_to_binary(
                io_lib:write(Label, [
                    {chars_limit, ?PROCESS_LABEL_CHARS_LIMIT},
                    {depth, 8},
                    {encoding, unicode}
                ])
            ),
            {identifier, label, Text}
    catch
        _:_ -> null
    end.

default_process_label(Pid) ->
    case erlang:function_exported(proc_lib, get_label, 1) of
        true -> erlang:apply(proc_lib, get_label, [Pid]);
        false -> undefined
    end.

process_window_field_key(Sort, delta) ->
    maps:get(Sort, #{
        memory => memory_delta,
        message_queue_len => message_queue_len_delta,
        reductions => reductions_delta,
        binary_memory => binary_memory_delta,
        total_heap_size => total_heap_size_delta
    });
process_window_field_key(Sort, per_second) ->
    maps:get(Sort, #{
        memory => memory_per_second,
        message_queue_len => message_queue_len_per_second,
        reductions => reductions_per_second,
        binary_memory => binary_memory_per_second,
        total_heap_size => total_heap_size_per_second
    }).

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
                Apps,
                Loaded,
                Running,
                AppSource,
                ProcessSource,
                Sort,
                Limit,
                {Context, AppEstimate}
            )
    end.

collect_admitted_applications(
    Apps, Loaded, Running, AppSource, ProcessSource, Sort, Limit, {Context, AppEstimate}
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
            {Stats, Unattributed} = application_stats(
                maps:get(items, Acc), LeaderApps, maps:get(info_fun, ProcessSource)
            ),
            RunningSet = maps:from_keys([App || {App, _, _} <- Running], true),
            LoadedSet = maps:from_keys([App || {App, _, _} <- Loaded], true),
            Versions = maps:from_list([{App, Version} || {App, _, Version} <- Loaded]),
            Items0 = [
                application_item(App, Stats, LoadedSet, RunningSet, Versions)
             || App <- Apps ++ [no_group]
            ],
            Eligible = length(Items0),
            RankedItems = recon_top_n(Items0, Sort, Limit),
            Items = [maps:remove(memory, Item) || Item <- RankedItems],
            Audit = audit_inventory(Acc, length(Items), ProcessStarted, ProcessFinished),
            Data = Audit#{
                items => Items,
                scanned_count => Eligible,
                eligible_count => Eligible,
                process_scanned_count => maps:get(scanned, Acc),
                process_eligible_count => maps:get(eligible, Acc),
                dropped_count => Eligible - length(Items),
                truncated => false,
                sort => Sort,
                sort_semantics => current,
                application_count => length(Apps),
                admission_stage => post_enumeration,
                attribution => group_leader_application,
                attribution_semantics => group_leader_chain,
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
                group_leader_chain_attribution,
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

application_stats(Items, Leaders, InfoFun) ->
    lists:foldl(
        fun(Item, {Stats, Unknown}) ->
            App = application_for_group(maps:get(group_leader, Item), Leaders, InfoFun, #{}),
            {
                increment_application_stats(App, Item, Stats),
                Unknown +
                    case App of
                        no_group -> 1;
                        _ -> 0
                    end
            }
        end,
        {#{}, 0},
        Items
    ).

application_for_group(Group, Leaders, InfoFun, Seen) when is_pid(Group) ->
    case maps:find(Group, Leaders) of
        {ok, App} ->
            App;
        error when node(Group) =/= node() ->
            no_group;
        error ->
            case maps:is_key(Group, Seen) of
                true ->
                    no_group;
                false ->
                    try InfoFun(Group, group_leader) of
                        {group_leader, Parent} when is_pid(Parent) ->
                            application_for_group(Parent, Leaders, InfoFun, Seen#{Group => true});
                        _ ->
                            no_group
                    catch
                        _:_ -> no_group
                    end
            end
    end;
application_for_group(_Group, _Leaders, _InfoFun, _Seen) ->
    no_group.

increment_application_stats(App, Item, Stats) ->
    Current = maps:get(App, Stats, empty_application_stats()),
    Stats#{
        App => Current#{
            process_count := maps:get(process_count, Current) + 1,
            memory := maps:get(memory, Current) + maps:get(memory, Item),
            reductions := maps:get(reductions, Current) + maps:get(reductions, Item),
            message_queue_len := maps:get(message_queue_len, Current) +
                maps:get(message_queue_len, Item)
        }
    }.

empty_application_stats() ->
    #{process_count => 0, memory => 0, reductions => 0, message_queue_len => 0}.

application_item(App, Stats, Loaded, Running, Versions) ->
    Values = maps:get(App, Stats, empty_application_stats()),
    Values#{
        application => {identifier, application, App},
        memory_bytes => maps:get(memory, Values),
        loaded => maps:is_key(App, Loaded),
        running => maps:is_key(App, Running),
        version => application_version(App, Versions)
    }.

application_version(App, Versions) ->
    case maps:find(App, Versions) of
        {ok, Version} ->
            case bounded_identifier_text(Version) of
                {ok, Text} -> Text;
                error -> null
            end;
        error ->
            null
    end.

collect_ets(Source, Sort, Limit, Context, Estimate) ->
    Started = erlang:monotonic_time(millisecond),
    Tables = (maps:get(all_fun, Source))(),
    Acc0 = table_inventory_acc(Context),
    Acc = lists:foldl(
        fun(Table, State) -> scan_ets_table(Table, Source, Sort, Limit, State) end,
        Acc0,
        Tables
    ),
    Finished = erlang:monotonic_time(millisecond),
    Items = [public_ets_item(Item) || Item <- maps:get(top, Acc)],
    Eligible = maps:get(eligible, Acc),
    Data = (audit_table_inventory(Acc, length(Items), Started, Finished))#{
        status => table_status(Eligible),
        items => Items,
        dropped_count => Eligible - length(Items),
        truncated => false,
        sort => Sort,
        sort_semantics => current,
        tracked_field_count => 10,
        retained_sample_count => 1,
        working_set_estimated_bytes => Estimate
    },
    {ok, Data, [
        metadata_only,
        explicit_ets_info_keys,
        raw_table_generation,
        exact_top_n,
        recon_top_n_order,
        ets_scan_admitted
    ]}.

scan_ets_table(Table, Source, Sort, Limit, Acc0) ->
    Acc1 = Acc0#{scanned := maps:get(scanned, Acc0) + 1},
    case ets_table_item(Table, Source) of
        {ok, Item} ->
            Acc1#{
                eligible := maps:get(eligible, Acc1) + 1,
                top := insert_table_top(Item, Sort, Limit, maps:get(top, Acc1))
            };
        disappeared ->
            Acc1#{disappeared := maps:get(disappeared, Acc1) + 1}
    end.

ets_table_item(Table, Source) ->
    Info = maps:get(info_fun, Source),
    FirstId = Info(Table, id),
    Fields = [
        name,
        size,
        memory,
        owner,
        type,
        protection,
        keypos,
        write_concurrency,
        read_concurrency
    ],
    Values = [{Key, Info(Table, Key)} || Key <- Fields],
    LastId = Info(Table, id),
    case
        valid_raw_table_id(FirstId) andalso FirstId =:= LastId andalso
            lists:all(fun({_Key, Value}) -> Value =/= undefined end, Values)
    of
        true ->
            WordSize = (maps:get(word_size_fun, Source))(),
            Map = maps:from_list(Values),
            {ok, Map#{
                raw_id => FirstId,
                table_id => {identifier, table, FirstId},
                name => {identifier, table, maps:get(name, Map)},
                memory => maps:get(memory, Map) * WordSize,
                memory_bytes => maps:get(memory, Map) * WordSize,
                owner => {identifier, pid, maps:get(owner, Map)},
                management => management_unknown
            }};
        false ->
            disappeared
    end.

valid_raw_table_id(Id) -> is_reference(Id) orelse is_integer(Id).

public_ets_item(Item) -> maps:without([raw_id, memory], Item).

insert_table_top(Item, Sort, Limit, Items) ->
    recon_top_n([Item | Items], Sort, Limit).

collect_mnesia(Source, Sort, Limit, Context) ->
    case (maps:get(available_fun, Source))() of
        false ->
            {unavailable, capability_unavailable, #{
                status => unavailable, reason_code => capability_unavailable
            }};
        true ->
            collect_available_mnesia(Source, Sort, Limit, Context)
    end.

collect_available_mnesia(Source, Sort, Limit, Context) ->
    case (maps:get(running_fun, Source))() of
        no ->
            {ok, empty_mnesia_data(not_running, Sort), [mnesia_not_running]};
        yes ->
            Tables = (maps:get(local_tables_fun, Source))(),
            Estimate = working_set_estimate(min(length(Tables), Limit), 4, 1),
            case
                length(Tables) =< ?MNESIA_SCAN_BUDGET andalso
                    Estimate =< ?MAX_WORKING_SET_BYTES
            of
                true ->
                    collect_admitted_mnesia(Tables, Source, Sort, Limit, Context, Estimate);
                false ->
                    {unavailable, scan_budget_exceeded, #{
                        status => unavailable,
                        reason_code => scan_budget_exceeded,
                        admission_stage => post_enumeration,
                        observed_local_table_count => length(Tables),
                        scan_budget_count => ?MNESIA_SCAN_BUDGET,
                        working_set_estimated_bytes => Estimate,
                        working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
                    }}
            end;
        _Other ->
            {unavailable, capability_unavailable, #{
                status => unavailable, reason_code => capability_unavailable
            }}
    end.

empty_mnesia_data(Status, Sort) ->
    #{
        status => Status,
        items => [],
        scanned_count => 0,
        eligible_count => 0,
        returned_count => 0,
        dropped_count => 0,
        truncated => false,
        disappeared_count => 0,
        complete => true,
        admission_stage => post_enumeration,
        sort => Sort,
        sort_semantics => current
    }.

collect_admitted_mnesia([], _Source, Sort, _Limit, _Context, _Estimate) ->
    {ok, empty_mnesia_data(empty, Sort), [local_tables_only, metadata_only]};
collect_admitted_mnesia(Tables, Source, Sort, Limit, Context, Estimate) ->
    Started = erlang:monotonic_time(millisecond),
    Acc0 = table_inventory_acc(Context),
    Acc = lists:foldl(
        fun(Table, State) -> scan_mnesia_table(Table, Source, Sort, Limit, State) end,
        Acc0,
        Tables
    ),
    Finished = erlang:monotonic_time(millisecond),
    Items = [public_mnesia_item(Item) || Item <- maps:get(top, Acc)],
    Eligible = maps:get(eligible, Acc),
    Data = (audit_table_inventory(Acc, length(Items), Started, Finished))#{
        status => ok,
        items => Items,
        dropped_count => Eligible - length(Items),
        truncated => false,
        admission_stage => post_enumeration,
        sort => Sort,
        sort_semantics => current,
        tracked_field_count => 4,
        retained_sample_count => 1,
        working_set_estimated_bytes => Estimate
    },
    {ok, Data, [
        local_tables_only,
        metadata_only,
        staged_admission,
        storage_type_units,
        exact_main_ets_correlation,
        recon_top_n_order
    ]}.

scan_mnesia_table(Table, Source, Sort, Limit, Acc0) ->
    Acc1 = Acc0#{scanned := maps:get(scanned, Acc0) + 1},
    case mnesia_table_item(Table, Source) of
        disappeared ->
            Acc1#{disappeared := maps:get(disappeared, Acc1) + 1};
        {ok, Item} ->
            case maps:get(Sort, Item, null) of
                Value when is_integer(Value), Value >= 0 ->
                    Acc1#{
                        eligible := maps:get(eligible, Acc1) + 1,
                        top := insert_table_top(Item, Sort, Limit, maps:get(top, Acc1))
                    };
                _ ->
                    Acc1
            end
    end.

mnesia_table_item(Table, Source) ->
    Storage = mnesia_info(Source, Table, storage_type),
    Size = mnesia_info(Source, Table, size),
    Memory = mnesia_info(Source, Table, memory),
    case Storage =/= undefined andalso is_integer(Size) andalso Size >= 0 of
        true ->
            WordSize = (maps:get(word_size_fun, Source))(),
            Units = mnesia_storage_units(Storage, Memory, WordSize),
            {Management, RawId} = mnesia_correlation(Table, Storage, Source),
            {ok, Units#{
                raw_id => RawId,
                table => {identifier, table, Table},
                storage_type => public_storage_type(Storage),
                size => Size,
                management => Management
            }};
        false ->
            disappeared
    end.

mnesia_info(Source, Table, Key) ->
    try (maps:get(info_fun, Source))(Table, Key) of
        Value -> Value
    catch
        _:_ -> undefined
    end.

mnesia_storage_units(ram_copies, Value, WordSize) when is_integer(Value), Value >= 0 ->
    #{memory => Value * WordSize, memory_bytes => Value * WordSize, disk_bytes => null};
mnesia_storage_units(disc_copies, Value, WordSize) when is_integer(Value), Value >= 0 ->
    #{memory => Value * WordSize, memory_bytes => Value * WordSize, disk_bytes => null};
mnesia_storage_units(disc_only_copies, Value, _WordSize) when is_integer(Value), Value >= 0 ->
    #{memory => null, memory_bytes => null, disk_bytes => Value};
mnesia_storage_units(_Storage, _Value, _WordSize) ->
    #{
        memory => null,
        memory_bytes => null,
        disk_bytes => null,
        storage_semantics_unavailable => true
    }.

public_storage_type(ram_copies) -> ram_copies;
public_storage_type(disc_copies) -> disc_copies;
public_storage_type(disc_only_copies) -> disc_only_copies;
public_storage_type(_ExternalOrUnknown) -> external_or_unknown.

mnesia_correlation(Table, Storage, Source) when
    Storage =:= ram_copies; Storage =:= disc_copies
->
    case (maps:get(whereis_fun, Source))(Table) of
        Tid when is_reference(Tid); is_integer(Tid) ->
            case (maps:get(ets_info_fun, Source))(Tid, id) of
                Tid -> {mnesia_main_table, Tid};
                _ -> {management_unknown, Table}
            end;
        _ ->
            {management_unknown, Table}
    end;
mnesia_correlation(Table, _Storage, _Source) ->
    {management_unknown, Table}.

public_mnesia_item(Item) ->
    Public = maps:without([raw_id, memory], Item),
    case maps:get(management, Item) of
        mnesia_main_table ->
            Public#{ets_table_id => {identifier, table, maps:get(raw_id, Item)}};
        management_unknown ->
            Public
    end.

table_inventory_acc(_Context) ->
    #{scanned => 0, eligible => 0, disappeared => 0, top => []}.

audit_table_inventory(Acc, Returned, Started, Finished) ->
    #{
        scanned_count => maps:get(scanned, Acc),
        eligible_count => maps:get(eligible, Acc),
        returned_count => Returned,
        disappeared_count => maps:get(disappeared, Acc),
        complete => true,
        scan_started_monotonic_ms => Started,
        scan_finished_monotonic_ms => Finished
    }.

table_status(0) -> empty;
table_status(_Count) -> ok.

excluded_processes(Context) ->
    Base = #{self() => diagnostics_worker},
    WithWorker = maybe_exclude_pid(
        maps:get(diagnostics_worker, Context, undefined), diagnostics_worker, Base
    ),
    WithCoordinator = maybe_exclude_pid(
        maps:get(coordinator, Context, undefined), diagnostics_coordinator, WithWorker
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

state_source(Request) ->
    state_source_test(Request, #{
        process_source => default_process_source(),
        get_state_fun => fun sys:get_state/2
    }).

-ifdef(TEST).
process_source_test(#{test_process_source := Source}, _Default) -> Source;
process_source_test(_Request, Default) -> Default.
state_source_test(#{test_state_source := Source}, _Default) -> Source;
state_source_test(_Request, Default) -> Default.
-else.
process_source_test(_Request, Default) -> Default.
state_source_test(_Request, Default) -> Default.
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
        root_info_fun => fun erlang:process_info/2,
        alive_fun => fun erlang:is_process_alive/1,
        count_children_fun => fun supervisor:count_children/1,
        which_children_fun => fun supervisor:which_children/1
    }.

ets_source(Request) ->
    ets_source_test(Request, default_ets_source()).

-ifdef(TEST).
ets_source_test(#{test_ets_source := Source}, _Default) -> Source;
ets_source_test(_Request, Default) -> Default.
-else.
ets_source_test(_Request, Default) -> Default.
-endif.

default_ets_source() ->
    #{
        count_fun => fun() -> erlang:system_info(ets_count) end,
        all_fun => fun ets:all/0,
        info_fun => fun ets:info/2,
        word_size_fun => fun() -> erlang:system_info(wordsize) end
    }.

mnesia_source(Request) ->
    mnesia_source_test(Request, default_mnesia_source()).

-ifdef(TEST).
mnesia_source_test(#{test_mnesia_source := Source}, _Default) -> Source;
mnesia_source_test(_Request, Default) -> Default.
-else.
mnesia_source_test(_Request, Default) -> Default.
-endif.

default_mnesia_source() ->
    #{
        available_fun => fun() ->
            case code:ensure_loaded(mnesia) of
                {module, mnesia} ->
                    erlang:function_exported(mnesia, system_info, 1) andalso
                        erlang:function_exported(mnesia, table_info, 2);
                _ ->
                    false
            end
        end,
        running_fun => fun() -> erlang:apply(mnesia, system_info, [is_running]) end,
        local_tables_fun => fun() -> erlang:apply(mnesia, system_info, [local_tables]) end,
        info_fun => fun(Table, Item) -> erlang:apply(mnesia, table_info, [Table, Item]) end,
        whereis_fun => fun ets:whereis/1,
        ets_info_fun => fun ets:info/2,
        word_size_fun => fun() -> erlang:system_info(wordsize) end
    }.

network_source(Request) ->
    network_source_test(Request, default_network_source()).

-ifdef(TEST).
network_source_test(#{test_network_source := Source}, _Default) -> Source;
network_source_test(_Request, Default) -> Default.
-else.
network_source_test(_Request, Default) -> Default.
-endif.

default_network_source() ->
    #{
        count_fun => fun() -> erlang:system_info(port_count) end,
        all_fun => fun safe_ports/0,
        name_fun => fun(Port) -> safe_port_info(Port, name) end,
        stat_fun => fun(Port) -> inet:getstat(Port, [recv_oct, recv_cnt, send_oct, send_cnt]) end,
        info_fun => fun safe_port_info/2,
        peername_fun => fun inet:peername/1,
        io_fun => fun() -> erlang:statistics(io) end,
        sleep_fun => fun timer:sleep/1,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    }.

port_source(Request) ->
    port_source_test(Request, default_port_source()).

-ifdef(TEST).
port_source_test(#{test_port_source := Source}, _Default) -> Source;
port_source_test(_Request, Default) -> Default.
-else.
port_source_test(_Request, Default) -> Default.
-endif.

default_port_source() ->
    #{
        count_fun => fun() -> erlang:system_info(port_count) end,
        all_fun => fun safe_ports/0,
        info_fun => fun safe_port_info/2,
        sockname_fun => fun inet:sockname/1,
        peername_fun => fun inet:peername/1,
        stat_fun => fun inet:getstat/2,
        getopts_fun => fun inet:getopts/2
    }.

socket_source(Request) ->
    socket_source_test(Request, default_socket_source()).

-ifdef(TEST).
socket_source_test(#{test_socket_source := Source}, _Default) -> Source;
socket_source_test(_Request, Default) -> Default.
-else.
socket_source_test(_Request, Default) -> Default.
-endif.

default_socket_source() ->
    #{
        available_fun => fun socket_available/0,
        count_fun => fun socket:number_of/0,
        global_fun => fun socket:info/0,
        all_fun => fun safe_sockets/0,
        info_fun => fun socket:info/1,
        sleep_fun => fun timer:sleep/1,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    }.

safe_ports() ->
    try
        {ok, erlang:ports()}
    catch
        _:_ -> {error, enumeration_error}
    end.

safe_sockets() ->
    try
        {ok, socket:which_sockets()}
    catch
        _:_ -> {error, enumeration_error}
    end.

safe_port_info(Port, Key) ->
    try erlang:port_info(Port, Key) of
        {Key, undefined} -> missing;
        {Key, Value} -> {ok, Value};
        undefined -> missing
    catch
        _:_ -> missing
    end.

socket_available() ->
    case code:ensure_loaded(socket) of
        {module, socket} ->
            erlang:function_exported(socket, info, 0) andalso
                erlang:function_exported(socket, number_of, 0) andalso
                erlang:function_exported(socket, which_sockets, 0);
        _ ->
            false
    end.

collect_counter_resources(Command, Source, Sort, Limit, Duration, Context) ->
    Count = safe_resource_count(Source),
    Budget =
        case Command of
            network -> ?PORT_SCAN_BUDGET;
            sockets -> ?SOCKET_SCAN_BUDGET
        end,
    Fields = tracked_counter_fields(Command),
    Retained = Count,
    Estimate = working_set_estimate(
        Retained,
        Fields,
        case Duration of
            undefined -> 1;
            _ -> 2
        end
    ),
    case Count =< Budget andalso Estimate =< ?MAX_WORKING_SET_BYTES of
        false ->
            {unavailable, scan_budget_exceeded, #{
                status => unavailable,
                reason_code => scan_budget_exceeded,
                admission_stage => pre_enumeration,
                observed_resource_count => Count,
                scan_budget_count => Budget,
                working_set_estimated_bytes => Estimate,
                working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
            }};
        true ->
            collect_admitted_counter_resources(
                Command, Source, Sort, Limit, Duration, Context, Estimate
            )
    end.

safe_resource_count(Source) ->
    try (maps:get(count_fun, Source))() of
        Count when is_integer(Count), Count >= 0 -> Count;
        _ -> ?SOCKET_SCAN_BUDGET + 1
    catch
        _:_ -> ?SOCKET_SCAN_BUDGET + 1
    end.

collect_admitted_counter_resources(Command, Source, Sort, Limit, undefined, Context, Estimate) ->
    case resource_sample(Command, Source, Context) of
        {ok, Sample, Audit, Coverage} ->
            Items0 = [total_resource_item(Command, Item) || Item <- maps:values(Sample)],
            Items = rank_resource_items(Items0, Sort, Limit),
            {ok,
                (total_resource_audit(Command, Audit))#{
                    status => resource_status(length(Items0)),
                    items => [public_resource_item(Item) || Item <- Items],
                    returned_count => length(Items),
                    dropped_count => length(Items0) - length(Items),
                    truncated => false,
                    sort => Sort,
                    sort_semantics => total,
                    baseline_count => 0,
                    tracked_field_count => tracked_counter_fields(Command),
                    retained_sample_count => 1,
                    working_set_estimated_bytes => Estimate
                },
                resource_coverage(Command, Coverage)};
        {error, Reason} ->
            enumeration_error(Command, Reason)
    end;
collect_admitted_counter_resources(Command, Source, Sort, Limit, Duration, Context, Estimate) ->
    case resource_sample(Command, Source, Context) of
        {ok, First, FirstAudit, FirstCoverage} ->
            (maps:get(sleep_fun, Source))(Duration),
            case resource_sample(Command, Source, Context) of
                {ok, Second, SecondAudit, SecondCoverage} ->
                    Interval =
                        (maps:get(monotonic_fun, Source))() -
                            maps:get(sample_monotonic_ms, FirstAudit),
                    Window = counter_window(Command, First, Second),
                    Items = rank_resource_items(maps:get(items, Window), Sort, Limit),
                    {ok,
                        (delta_resource_audit(Command, FirstAudit, SecondAudit))#{
                            status => resource_status(length(maps:get(items, Window))),
                            items => [public_resource_item(Item) || Item <- Items],
                            returned_count => length(Items),
                            dropped_count => length(maps:get(items, Window)) - length(Items),
                            truncated => false,
                            sort => Sort,
                            sort_semantics => delta,
                            requested_duration_ms => Duration,
                            interval_ms => Interval,
                            lifecycle => public_lifecycle(Window),
                            baseline_count => map_size(First),
                            tracked_field_count => tracked_counter_fields(Command),
                            retained_sample_count => 2,
                            working_set_estimated_bytes => Estimate
                        },
                        resource_coverage(Command, FirstCoverage ++ SecondCoverage)};
                {error, Reason} ->
                    enumeration_error(Command, Reason)
            end;
        {error, Reason} ->
            enumeration_error(Command, Reason)
    end.

resource_sample(network, Source, Context) -> network_sample(Source, Context);
resource_sample(sockets, Source, _Context) -> socket_sample(Source).

network_sample(Source, Context) ->
    case (maps:get(all_fun, Source))() of
        {ok, Ports} when is_list(Ports) ->
            Started = (maps:get(monotonic_fun, Source))(),
            Excluded = observer_port_exclusions(Context),
            {Items, Disappeared, Exclusions} = lists:foldl(
                fun(Port, {Acc, Gone, Removed}) ->
                    case maps:find(Port, Excluded) of
                        {ok, Reason} ->
                            {Acc, Gone, [port_exclusion(Port, Reason) | Removed]};
                        error ->
                            network_resource_acc(Port, Source, Acc, Gone, Removed)
                    end
                end,
                {#{}, 0, []},
                Ports
            ),
            Audit = (resource_audit(length(Ports), map_size(Items), Disappeared, Started))#{
                vm_io_counters => network_io_counters(Source),
                exclusion_count => length(Exclusions),
                exclusions => lists:reverse(Exclusions)
            },
            {ok, Items, Audit, []};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, invalid_enumeration_shape}
    end.

network_resource_acc(Port, Source, Acc, Gone, Removed) ->
    case network_resource(Port, Source) of
        skip -> {Acc, Gone, Removed};
        disappeared -> {Acc, Gone + 1, Removed};
        Item -> {Acc#{Port => Item}, Gone, Removed}
    end.

network_resource(Port, Source) ->
    case (maps:get(name_fun, Source))(Port) of
        {ok, Name} ->
            case inet_protocol(Name) of
                undefined ->
                    skip;
                Protocol ->
                    try (maps:get(stat_fun, Source))(Port) of
                        {ok, Stats} ->
                            network_resource_from_stats(Port, Protocol, Stats, Source);
                        _ ->
                            disappeared
                    catch
                        _:_ -> disappeared
                    end
            end;
        missing ->
            disappeared
    end.

network_resource_from_stats(Port, Protocol, Stats, Source) ->
    case parse_network_counters(Stats) of
        {ok, Counters} ->
            Fields = [queue_size, memory, input, output],
            Values = maps:from_list([
                {Key, network_port_field(Port, Key, Source)}
             || Key <- Fields
            ]),
            {Peername, PeerErrors} = network_peername(Port, Source),
            maps:merge(Values, #{
                raw_id => Port,
                resource => {identifier, port, Port},
                protocol => Protocol,
                peername => Peername,
                field_errors =>
                    [Key || Key <- Fields, maps:get(Key, Values) =:= null] ++ PeerErrors,
                counters => Counters,
                counter_shape => lists:sort(maps:keys(Counters))
            });
        error ->
            disappeared
    end.

network_port_field(Port, Key, Source) ->
    try (maps:get(info_fun, Source))(Port, Key) of
        Value -> port_field(Value)
    catch
        _:_ -> null
    end.

network_peername(Port, Source) ->
    Result = call_port_fun(peername_fun, Port, Source),
    case {Result, port_endpoint(Result)} of
        {{error, enotconn}, Peername} -> {Peername, []};
        {_Result, null} -> {null, [peername]};
        {_Result, Peername} -> {Peername, []}
    end.

parse_network_counters(Stats) ->
    case
        is_list(Stats) andalso
            lists:all(
                fun
                    ({Key, _Value}) when is_atom(Key) -> true;
                    (_Other) -> false
                end,
                Stats
            )
    of
        true -> {ok, maps:from_list(Stats)};
        false -> error
    end.

inet_protocol("tcp_inet") -> tcp;
inet_protocol("udp_inet") -> udp;
inet_protocol("sctp_inet") -> sctp;
inet_protocol(<<"tcp_inet">>) -> tcp;
inet_protocol(<<"udp_inet">>) -> udp;
inet_protocol(<<"sctp_inet">>) -> sctp;
inet_protocol(_) -> undefined.

socket_sample(Source) ->
    case (maps:get(all_fun, Source))() of
        {ok, Sockets} when is_list(Sockets) ->
            Started = (maps:get(monotonic_fun, Source))(),
            {Items, Disappeared, Coverage} = lists:foldl(
                fun(Socket, {Acc, Gone, Covered}) ->
                    try (maps:get(info_fun, Source))(Socket) of
                        #{counters := RawCounters} = Info when is_map(RawCounters) ->
                            Counters = maps:with(socket_counter_keys(), RawCounters),
                            Item = #{
                                raw_id => Socket,
                                resource => {identifier, socket, Socket},
                                owner => socket_owner(Info),
                                domain => maps:get(domain, Info, null),
                                type => maps:get(type, Info, null),
                                protocol => maps:get(protocol, Info, null),
                                rstate => maps:get(rstates, Info, maps:get(rstate, Info, null)),
                                wstate => maps:get(wstates, Info, maps:get(wstate, Info, null)),
                                counters => Counters,
                                counter_shape => lists:sort(maps:keys(Counters))
                            },
                            {
                                Acc#{Socket => Item},
                                Gone,
                                socket_optional_coverage(Counters) ++ Covered
                            };
                        _ ->
                            {Acc, Gone + 1, Covered}
                    catch
                        _:_ -> {Acc, Gone + 1, Covered}
                    end
                end,
                {#{}, 0, []},
                Sockets
            ),
            Global =
                try
                    (maps:get(global_fun, Source))()
                catch
                    _:_ -> #{}
                end,
            UseRegistry = maps:get(use_registry, Global, unknown),
            {ok, Items,
                (resource_audit(length(Sockets), map_size(Items), Disappeared, Started))#{
                    registry_known_count => map_size(Items),
                    use_registry => UseRegistry,
                    empty_meaning => no_registry_known_sockets
                },
                Coverage};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, invalid_enumeration_shape}
    end.

socket_owner(#{owner := Owner}) when is_pid(Owner) -> {identifier, pid, Owner};
socket_owner(_Info) -> null.

resource_audit(Scanned, Eligible, Disappeared, Started) ->
    #{
        scanned_count => Scanned,
        eligible_count => Eligible,
        disappeared_count => Disappeared,
        complete => true,
        admission_stage => post_enumeration,
        sample_monotonic_ms => Started
    }.

network_io_counters(Source) ->
    try (maps:get(io_fun, Source))() of
        {{input, Input}, {output, Output}} when
            is_integer(Input), Input >= 0, is_integer(Output), Output >= 0
        ->
            #{input => Input, output => Output};
        _ ->
            #{}
    catch
        _:_ -> #{}
    end.

total_resource_audit(network, Audit) ->
    Counters = maps:get(vm_io_counters, Audit, #{}),
    (maps:remove(vm_io_counters, Audit))#{vm_port_driver_io => vm_io_metrics(Counters, total)};
total_resource_audit(sockets, Audit) ->
    Audit.

delta_resource_audit(network, First, Second) ->
    FirstCounters = maps:get(vm_io_counters, First, #{}),
    SecondCounters = maps:get(vm_io_counters, Second, #{}),
    Metrics =
        case lists:sort(maps:keys(FirstCounters)) =:= lists:sort(maps:keys(SecondCounters)) of
            true ->
                vm_io_metrics(counter_deltas(FirstCounters, SecondCounters), delta);
            false ->
                #{
                    status => shape_change,
                    input_bytes_delta => null,
                    output_bytes_delta => null,
                    io_bytes_delta => null
                }
        end,
    (maps:without([sample_monotonic_ms, vm_io_counters], Second))#{vm_port_driver_io => Metrics};
delta_resource_audit(sockets, _First, Second) ->
    maps:without([sample_monotonic_ms], Second).

vm_io_metrics(Counters, Semantics) ->
    Input = counter_metric(Counters, [input], []),
    Output = counter_metric(Counters, [output], []),
    Io = counter_metric(Counters, [input, output], []),
    Suffix =
        case Semantics of
            total -> <<"_bytes_total">>;
            delta -> <<"_bytes_delta">>
        end,
    maps:fold(
        fun
            (Key, #{status := available, value := Value}, Acc) ->
                Acc#{iolist_to_binary([atom_to_binary(Key), Suffix]) => Value};
            (Key, #{status := Status}, Acc) ->
                Acc#{status => Status, iolist_to_binary([atom_to_binary(Key), Suffix]) => null}
        end,
        #{status => available},
        #{input => Input, output => Output, io => Io}
    ).

collect_ports(Source, Sort, Limit, Context) ->
    Count = safe_resource_count(Source),
    Estimate = working_set_estimate(min(Count, Limit), 9, 1),
    case Count =< ?PORT_SCAN_BUDGET andalso Estimate =< ?MAX_WORKING_SET_BYTES of
        false ->
            {unavailable, scan_budget_exceeded, #{
                status => unavailable,
                reason_code => scan_budget_exceeded,
                admission_stage => pre_enumeration,
                observed_resource_count => Count,
                scan_budget_count => ?PORT_SCAN_BUDGET,
                working_set_estimated_bytes => Estimate,
                working_set_budget_bytes => ?MAX_WORKING_SET_BYTES
            }};
        true ->
            case (maps:get(all_fun, Source))() of
                {ok, Ports} when is_list(Ports) ->
                    collect_port_items(Ports, Source, Sort, Limit, Context, Estimate);
                {error, Reason} ->
                    enumeration_error(ports, Reason);
                _ ->
                    enumeration_error(ports, invalid_enumeration_shape)
            end
    end.

collect_port_items(Ports, Source, Sort, Limit, Context, Estimate) ->
    Excluded = observer_port_exclusions(Context),
    {Items0, Disappeared, Exclusions} = lists:foldl(
        fun(Port, {Items, Gone, Removed}) ->
            case maps:find(Port, Excluded) of
                {ok, Reason} ->
                    {Items, Gone, [port_exclusion(Port, Reason) | Removed]};
                error ->
                    case port_resource(Port, Source) of
                        skip -> {Items, Gone, Removed};
                        disappeared -> {Items, Gone + 1, Removed};
                        Item -> {[Item | Items], Gone, Removed}
                    end
            end
        end,
        {[], 0, []},
        Ports
    ),
    Items = rank_resource_items(Items0, Sort, Limit),
    {ok,
        #{
            status => resource_status(length(Items0)),
            items => [public_resource_item(Item) || Item <- Items],
            scanned_count => length(Ports),
            eligible_count => length(Items0),
            returned_count => length(Items),
            dropped_count => length(Items0) - length(Items),
            truncated => false,
            disappeared_count => Disappeared,
            exclusion_count => length(Exclusions),
            exclusions => lists:reverse(Exclusions),
            complete => true,
            sort => Sort,
            sort_semantics => current_or_lifetime,
            tracked_field_count => 9,
            retained_sample_count => 1,
            working_set_estimated_bytes => Estimate
        },
        [documented_port_info_keys, name_only_inet_classification, raw_port_identity]}.

observer_port_exclusions(Context) ->
    case {controller_node(maps:get(controller, Context, undefined)), safe_system_info(dist_ctrl)} of
        {ControllerNode, {ok, Controllers}} when is_atom(ControllerNode), is_list(Controllers) ->
            case lists:keyfind(ControllerNode, 1, Controllers) of
                {ControllerNode, Port} when is_port(Port) -> #{Port => diagnostics_controller};
                _ -> #{}
            end;
        _ ->
            #{}
    end.

port_exclusion(Port, Reason) ->
    #{resource => {identifier, port, Port}, reason => Reason}.

port_resource(Port, Source) ->
    Info = maps:get(info_fun, Source),
    case Info(Port, name) of
        {ok, Name} ->
            case inet_protocol(Name) of
                undefined ->
                    Fields = [
                        connected, queue_size, memory, id, input, output, parallelism, locking
                    ],
                    Values = maps:from_list([{Key, port_field(Info(Port, Key))} || Key <- Fields]),
                    Input = maps:get(input, Values),
                    Output = maps:get(output, Values),
                    Io =
                        case {Input, Output} of
                            {I, O} when is_integer(I), is_integer(O) -> I + O;
                            _ -> null
                        end,
                    #{
                        raw_id => Port,
                        resource => {identifier, port, Port},
                        name => Name,
                        controls => Name,
                        connected_pid => port_identifier(maps:get(connected, Values)),
                        queue_size => maps:get(queue_size, Values),
                        memory => maps:get(memory, Values),
                        display_id => maps:get(id, Values),
                        slot => maps:get(id, Values),
                        input => Input,
                        output => Output,
                        io => Io,
                        parallelism => maps:get(parallelism, Values),
                        locking => maps:get(locking, Values),
                        field_errors => [Key || Key <- Fields, maps:get(Key, Values) =:= null]
                    };
                _ ->
                    skip
            end;
        missing ->
            disappeared
    end.

collect_port(Target, Source) ->
    case resolve_port_target(Target) of
        {ok, Port} -> collect_port_detail(Port, Source);
        not_found -> {ok, #{status => not_found}, [target_side_resolution, raw_port_text_only]}
    end.

resolve_port_target(Target) ->
    case target_binary(Target) of
        {ok, Text} when byte_size(Text) =< 64 ->
            case re:run(Text, <<"^#Port<0\\.[0-9]+>$">>, [{capture, none}]) of
                match ->
                    canonical_port(Text);
                nomatch ->
                    not_found
            end;
        _ ->
            not_found
    end.

canonical_port(Text) ->
    try list_to_port(binary_to_list(Text)) of
        Port ->
            case list_to_binary(port_to_list(Port)) =:= Text of
                true -> {ok, Port};
                false -> not_found
            end
    catch
        error:badarg -> not_found
    end.

collect_port_detail(Port, Source) ->
    Info = maps:get(info_fun, Source),
    case Info(Port, name) of
        {ok, Name} ->
            Fields = [
                connected,
                queue_size,
                memory,
                id,
                input,
                output,
                parallelism,
                locking,
                os_pid,
                links,
                monitors,
                monitored_by
            ],
            Values = maps:from_list([{Key, port_field(Info(Port, Key))} || Key <- Fields]),
            Base0 = #{
                status => running,
                resource => {identifier, port, Port},
                name => Name,
                controls => Name,
                display_id => maps:get(id, Values),
                slot => maps:get(id, Values),
                connected_pid => port_identifier(maps:get(connected, Values)),
                os_pid => maps:get(os_pid, Values),
                queue_size => maps:get(queue_size, Values),
                memory => maps:get(memory, Values),
                input => maps:get(input, Values),
                output => maps:get(output, Values),
                parallelism => maps:get(parallelism, Values),
                locking => maps:get(locking, Values),
                field_errors => [Key || Key <- Fields, maps:get(Key, Values) =:= null]
            },
            Base1 = bounded_process_list(
                links, port_list_field(maps:get(links, Values)), fun sanitize_signal_list/1, Base0
            ),
            Base2 = bounded_process_list(
                monitors,
                port_list_field(maps:get(monitors, Values)),
                fun sanitize_signal_list/1,
                Base1
            ),
            Base = bounded_process_list(
                monitored_by,
                port_list_field(maps:get(monitored_by, Values)),
                fun sanitize_signal_list/1,
                Base2
            ),
            {ok, Base#{inet => collect_port_inet(Port, Name, Source)}, [
                target_side_resolution,
                documented_port_info_keys,
                bounded_signal_lists,
                fixed_inet_allowlists
            ]};
        missing ->
            {ok, #{status => not_found}, [target_side_resolution, raw_port_text_only]}
    end.

port_list_field(Value) when is_list(Value) -> Value;
port_list_field(_Value) -> [].

collect_port_inet(Port, Name, Source) ->
    case inet_protocol(Name) of
        undefined ->
            #{
                status => not_inet,
                protocol => null,
                sockname => null,
                peername => null,
                statistics => #{status => not_inet},
                options => []
            };
        Protocol ->
            #{
                status => ok,
                protocol => Protocol,
                sockname => port_endpoint(call_port_fun(sockname_fun, Port, Source)),
                peername => port_endpoint(call_port_fun(peername_fun, Port, Source)),
                statistics => port_statistics(Port, Source),
                options => [port_option(Port, Opt, Source) || Opt <- observer_cli_port:sock_opts()]
            }
    end.

call_port_fun(Key, Port, Source) ->
    try (maps:get(Key, Source))(Port) of
        Result -> Result
    catch
        _:_ -> {error, failed}
    end.

port_endpoint({ok, {Address, Port}}) when is_integer(Port), Port >= 0, Port =< 65535 ->
    case inet:ntoa(Address) of
        {error, einval} ->
            null;
        AddressText ->
            {identifier, endpoint, iolist_to_binary([AddressText, $:, integer_to_binary(Port)])}
    end;
port_endpoint({ok, {local, Path}}) ->
    case bounded_identifier_text(Path) of
        {ok, Text} -> {identifier, endpoint, Text};
        error -> null
    end;
port_endpoint(_Result) ->
    null.

port_statistics(Port, Source) ->
    Keys = [
        recv_oct,
        recv_cnt,
        recv_max,
        recv_avg,
        recv_dvi,
        send_oct,
        send_cnt,
        send_max,
        send_avg,
        send_pend
    ],
    Result =
        try (maps:get(stat_fun, Source))(Port, Keys) of
            Value -> Value
        catch
            _:_ -> {error, failed}
        end,
    case Result of
        {ok, Stats} when is_list(Stats) ->
            Values = maps:from_list(Stats),
            (maps:from_list([
                {Key, stat_value(maps:get(Key, Values, null))}
             || Key <- Keys
            ]))#{
                status => available
            };
        _ ->
            (maps:from_list([{Key, null} || Key <- Keys]))#{status => error}
    end.

stat_value(Value) when is_integer(Value), Value >= 0 -> Value;
stat_value(_Value) -> null.

port_option(Port, Option, Source) ->
    Result =
        try (maps:get(getopts_fun, Source))(Port, [Option]) of
            GetoptsResult -> GetoptsResult
        catch
            _:_ -> {error, failed}
        end,
    case Result of
        {ok, [{Option, Value}]} ->
            case safe_port_option_value(Option, Value) of
                {ok, Safe} -> #{name => Option, status => available, value => Safe};
                error -> #{name => Option, status => error, value => null}
            end;
        {ok, []} ->
            #{name => Option, status => unsupported, value => null};
        {error, einval} ->
            #{name => Option, status => unsupported, value => null};
        {error, Reason} when is_atom(Reason) ->
            #{name => Option, status => error, reason => Reason, value => null};
        _ ->
            #{name => Option, status => error, value => null}
    end.

safe_port_option_value(linger, {Enabled, Seconds}) when
    is_boolean(Enabled), is_integer(Seconds), Seconds >= 0
->
    {ok, #{enabled => Enabled, seconds => Seconds}};
safe_port_option_value(bind_to_device, Value) ->
    port_option_identifier(interface, Value);
safe_port_option_value(netns, Value) ->
    port_option_identifier(netns, Value);
safe_port_option_value(_Option, Value) when
    is_boolean(Value); is_integer(Value); is_atom(Value); is_binary(Value)
->
    {ok, Value};
safe_port_option_value(_Option, Value) when is_list(Value) ->
    case bounded_identifier_text(Value) of
        {ok, Text} -> {ok, Text};
        error -> error
    end;
safe_port_option_value(_Option, _Value) ->
    error.

port_option_identifier(Type, Value) ->
    case bounded_identifier_text(Value) of
        {ok, Text} -> {ok, {identifier, Type, Text}};
        error -> error
    end.

bounded_identifier_text(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Text when is_binary(Text), byte_size(Text) =< ?MAX_FIELD_BYTES -> {ok, Text};
        _ -> error
    catch
        _:_ -> error
    end;
bounded_identifier_text(Value) ->
    identifier_text(Value).

port_field({ok, Value}) -> Value;
port_field(missing) -> null.

port_identifier(Pid) when is_pid(Pid) -> {identifier, pid, Pid};
port_identifier(_) -> null.

total_resource_item(network, Item) ->
    add_network_metrics(Item, maps:get(counters, Item));
total_resource_item(sockets, Item) ->
    Counters = maps:get(counters, Item),
    add_socket_current_metrics(add_socket_metrics(Item, Counters), Counters).

add_network_metrics(Item, Counters) ->
    add_metrics(Item, #{
        recv_oct => counter_metric(Counters, [recv_oct], []),
        send_oct => counter_metric(Counters, [send_oct], []),
        oct => counter_metric(Counters, [recv_oct, send_oct], []),
        recv_cnt => counter_metric(Counters, [recv_cnt], []),
        send_cnt => counter_metric(Counters, [send_cnt], []),
        cnt => counter_metric(Counters, [recv_cnt, send_cnt], [])
    }).

add_socket_metrics(Item, Counters) ->
    add_metrics(Item, socket_metrics(Counters)).

add_socket_current_metrics(Item, Counters) ->
    add_metrics(Item, #{
        max_packet => counter_max_metric(
            Counters, [read_pkg_max, write_pkg_max], [sendfile_pkg_max]
        )
    }).

add_metrics(Item, Metrics) ->
    maps:fold(
        fun
            (Key, #{status := available, value := Value}, Acc) ->
                Acc#{
                    Key => Value,
                    metric_states := (maps:get(metric_states, Acc, #{}))#{Key => available}
                };
            (Key, #{status := Status}, Acc) ->
                Acc#{
                    Key => null,
                    metric_states := (maps:get(metric_states, Acc, #{}))#{Key => Status}
                }
        end,
        Item#{metric_states => maps:get(metric_states, Item, #{})},
        Metrics
    ).

socket_metrics(Counters) when is_map(Counters) ->
    #{
        read_bytes => socket_metric(Counters, [read_byte], []),
        write_bytes => socket_metric(Counters, [write_byte], [sendfile_byte]),
        io => socket_metric(Counters, [read_byte, write_byte], [sendfile_byte]),
        packets => socket_metric(Counters, [read_pkg, write_pkg], [sendfile_pkg]),
        accepts => socket_metric(Counters, [acc_success, acc_tries], []),
        waits => socket_metric(Counters, [acc_waits, read_waits, write_waits], [sendfile_waits]),
        fails => socket_metric(Counters, [acc_fails, read_fails, write_fails], [sendfile_fails])
    }.

socket_metric(Counters, Required, Optional) ->
    counter_metric(Counters, Required, Optional).

counter_metric(Counters, Required, Optional) ->
    counter_metric(Counters, Required, Optional, fun lists:sum/1).

counter_max_metric(Counters, Required, Optional) ->
    counter_metric(Counters, Required, Optional, fun lists:max/1).

counter_metric(Counters, Required, Optional, Aggregate) ->
    RequiredValues = [maps:get(Key, Counters, missing) || Key <- Required],
    OptionalValues = [maps:get(Key, Counters, missing) || Key <- Optional],
    case lists:member(counter_reset, RequiredValues ++ OptionalValues) of
        true ->
            #{status => counter_reset};
        false ->
            case
                {
                    lists:all(fun valid_counter/1, RequiredValues),
                    lists:all(
                        fun(Value) -> Value =:= missing orelse valid_counter(Value) end,
                        OptionalValues
                    )
                }
            of
                {true, true} ->
                    Value = Aggregate(
                        RequiredValues ++ [V || V <- OptionalValues, valid_counter(V)]
                    ),
                    #{status => available, value => Value};
                {false, _} ->
                    #{status => missing_core};
                {true, false} ->
                    #{status => invalid_optional}
            end
    end.

valid_counter(Value) -> is_integer(Value) andalso Value >= 0.

socket_optional_coverage(Counters) ->
    Optional = [
        sendfile_byte, sendfile_pkg, sendfile_pkg_max, sendfile_waits, sendfile_fails
    ],
    case lists:any(fun(Key) -> not maps:is_key(Key, Counters) end, Optional) of
        true -> [optional_sendfile_counter_absent];
        false -> []
    end.

counter_window(Command, First, Second) ->
    FirstIds = lists:sort(maps:keys(First)),
    SecondIds = lists:sort(maps:keys(Second)),
    StableIds = ordsets:intersection(FirstIds, SecondIds),
    Born = ordsets:subtract(SecondIds, FirstIds),
    Gone = ordsets:subtract(FirstIds, SecondIds),
    StableItems = [
        counter_delta_item(Command, maps:get(Id, First), maps:get(Id, Second))
     || Id <- StableIds
    ],
    BornItems = [
        unavailable_delta_item(Command, maps:get(Id, Second), baseline_missing)
     || Id <- Born
    ],
    #{
        items => StableItems ++ BornItems,
        born => Born,
        gone => Gone,
        reset => [
            maps:get(raw_id, Item)
         || Item <- StableItems, resource_item_state(Item) =:= counter_reset
        ],
        shape_changed => [
            maps:get(raw_id, Item)
         || Item <- StableItems,
            resource_item_state(Item) =:= shape_change
        ]
    }.

socket_series_trend([]) ->
    #{status => unavailable, items => []};
socket_series_trend(ValueMaps) ->
    First = hd(ValueMaps),
    Last = lists:last(ValueMaps),
    Shared = lists:foldl(
        fun(Values, Ids) -> ordsets:intersection(Ids, lists:sort(maps:keys(Values))) end,
        lists:sort(maps:keys(First)),
        tl(ValueMaps)
    ),
    Items0 = [socket_series_item([maps:get(Id, Values) || Values <- ValueMaps]) || Id <- Shared],
    Items = rank_resource_items(Items0, io, 20),
    #{
        status => ok,
        sample_count => length(ValueMaps),
        born_count => length(ordsets:subtract(lists:sort(maps:keys(Last)), Shared)),
        dead_count => length(ordsets:subtract(lists:sort(maps:keys(First)), Shared)),
        sort_metric => io,
        sort_semantics => delta_descending,
        items => [public_resource_item(Item) || Item <- Items]
    }.

socket_series_item(Items) ->
    Last = lists:last(Items),
    Shapes = [maps:get(counter_shape, Item) || Item <- Items],
    case lists:usort(Shapes) of
        [_Shape] ->
            Deltas = socket_series_deltas([maps:get(counters, Item) || Item <- Items]),
            Result = add_socket_current_metrics(
                add_socket_metrics(Last, Deltas), maps:get(counters, Last)
            ),
            States = maps:values(maps:get(metric_states, Result)),
            Result#{
                state =>
                    case lists:member(counter_reset, States) of
                        true -> counter_reset;
                        false -> available
                    end
            };
        _ ->
            unavailable_delta_item(sockets, Last, shape_change)
    end.

socket_series_deltas(Counters) ->
    maps:from_list([
        {Key, counter_series_delta([maps:get(Key, Values) || Values <- Counters])}
     || Key <- maps:keys(hd(Counters))
    ]).

counter_series_delta(Values) ->
    case lists:all(fun valid_counter/1, Values) of
        false ->
            invalid_counter;
        true ->
            case
                lists:any(
                    fun({Before, After}) -> After < Before end,
                    lists:zip(lists:droplast(Values), tl(Values))
                )
            of
                true -> counter_reset;
                false -> lists:last(Values) - hd(Values)
            end
    end.

counter_delta_item(Command, First, Second) ->
    FirstCounters = maps:get(counters, First),
    SecondCounters = maps:get(counters, Second),
    case maps:get(counter_shape, First) =:= maps:get(counter_shape, Second) of
        false ->
            unavailable_delta_item(Command, Second, shape_change);
        true ->
            Deltas = counter_deltas(FirstCounters, SecondCounters),
            Item =
                case Command of
                    network ->
                        add_network_metrics(Second, Deltas);
                    sockets ->
                        add_socket_current_metrics(
                            add_socket_metrics(Second, Deltas), SecondCounters
                        )
                end,
            States = maps:values(maps:get(metric_states, Item)),
            Item#{
                state =>
                    case lists:member(counter_reset, States) of
                        true -> counter_reset;
                        false -> available
                    end
            }
    end.

counter_deltas(First, Second) ->
    lists:foldl(
        fun(Key, Acc) ->
            Before = maps:get(Key, First),
            After = maps:get(Key, Second),
            case valid_counter(Before) andalso valid_counter(After) andalso After >= Before of
                true ->
                    Acc#{Key => After - Before};
                false when is_integer(Before), is_integer(After), After < Before ->
                    Acc#{Key => counter_reset};
                false ->
                    Acc#{Key => invalid_counter}
            end
        end,
        #{},
        maps:keys(First)
    ).

unavailable_delta_item(network, Item, State) ->
    Item#{
        state => State,
        recv_oct => null,
        send_oct => null,
        oct => null,
        recv_cnt => null,
        send_cnt => null,
        cnt => null,
        metric_states => maps:from_keys(
            [recv_oct, send_oct, oct, recv_cnt, send_cnt, cnt], State
        )
    };
unavailable_delta_item(sockets, Item, State) ->
    Keys = [io, read_bytes, write_bytes, packets, accepts, waits, fails],
    add_socket_current_metrics(
        lists:foldl(
            fun(Key, Acc) -> Acc#{Key => null} end,
            Item#{state => State, metric_states => maps:from_keys(Keys, State)},
            Keys
        ),
        maps:get(counters, Item)
    ).

resource_item_state(Item) -> maps:get(state, Item, available).

rank_resource_items(Items, Sort, Limit) ->
    lists:sublist(lists:sort(fun(A, B) -> resource_precedes(A, B, Sort) end, Items), Limit).

resource_precedes(A, B, Sort) ->
    AValue = maps:get(Sort, A, null),
    BValue = maps:get(Sort, B, null),
    case {is_integer(AValue), is_integer(BValue)} of
        {true, true} ->
            AValue > BValue orelse
                (AValue =:= BValue andalso maps:get(raw_id, A) < maps:get(raw_id, B));
        {true, false} ->
            true;
        {false, true} ->
            false;
        {false, false} ->
            maps:get(raw_id, A) < maps:get(raw_id, B)
    end.

recon_top_n(Items, Sort, Limit) ->
    [
        Item
     || {_, _, [Item]} <- recon_lib:sublist_top_n_attrs(
            [{top_n_identity(Item), top_n_value(Item, Sort), [Item]} || Item <- Items], Limit
        )
    ].

top_n_identity(#{raw_pid := Pid}) -> Pid;
top_n_identity(_Item) -> 0.

top_n_value(Item, Sort) ->
    case maps:get(Sort, Item, null) of
        Value when is_number(Value) -> Value;
        _ -> -1
    end.

public_resource_item(Item) -> maps:without([raw_id, counters, counter_shape], Item).

public_lifecycle(Window) ->
    #{
        baseline_missing => resource_identifiers(maps:get(born, Window)),
        gone => resource_identifiers(maps:get(gone, Window)),
        counter_reset => resource_identifiers(maps:get(reset, Window)),
        shape_change => resource_identifiers(maps:get(shape_changed, Window))
    }.

resource_identifiers(Ids) -> [raw_resource_identifier(Id) || Id <- Ids].
raw_resource_identifier(Id) when is_port(Id) -> {identifier, port, Id};
raw_resource_identifier({'$socket', _} = Id) -> {identifier, socket, Id};
raw_resource_identifier(Id) when is_reference(Id) -> {identifier, socket, Id};
raw_resource_identifier(Id) -> Id.

resource_status(0) -> empty;
resource_status(_) -> ok.

tracked_counter_fields(network) -> 4;
tracked_counter_fields(sockets) -> 19.

socket_counter_keys() ->
    [
        read_byte,
        write_byte,
        read_pkg,
        write_pkg,
        read_pkg_max,
        write_pkg_max,
        acc_success,
        acc_tries,
        acc_waits,
        read_waits,
        write_waits,
        acc_fails,
        read_fails,
        write_fails,
        sendfile_byte,
        sendfile_pkg,
        sendfile_pkg_max,
        sendfile_waits,
        sendfile_fails
    ].

resource_coverage(network, _Coverage) ->
    [legacy_inet_ports_only, vm_port_driver_counters, raw_port_identity, port_context, peername];
resource_coverage(sockets, Coverage) ->
    lists:usort([registry_known_sockets, opaque_raw_socket_identity | Coverage]).

enumeration_error(Command, Reason) ->
    {error, enumeration_error, #{
        status => error,
        reason_code => enumeration_error,
        resource => Command,
        enumeration_error => Reason,
        items => []
    }}.

capture_inspection(Command, ProbeId, Samples, #{controller := Controller}, Fun) ->
    StartedAt = erlang:system_time(millisecond),
    StartedMonotonic = erlang:monotonic_time(millisecond),
    ModuleLoaded = module_loaded(),
    {Runtime, Data, Coverage, ExtraEffects} = Fun(),
    FinishedMonotonic = erlang:monotonic_time(millisecond),
    FinishedAt = erlang:system_time(millisecond),
    observer_cli_cli:response(
        Command,
        complete,
        target_from_runtime(Runtime),
        #{
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
        Data,
        []
    ).

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

module_loaded() ->
    is_tuple(code:is_loaded(?MODULE)).

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
identifier_binary(child, Value) when is_binary(Value) ->
    {ok, Value};
identifier_binary(Type, Value) when
    Type =:= node;
    Type =:= name;
    Type =:= module;
    Type =:= function;
    Type =:= peer;
    Type =:= endpoint;
    Type =:= interface;
    Type =:= netns;
    Type =:= table;
    Type =:= application;
    Type =:= label
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
json_safe(true) ->
    true;
json_safe(false) ->
    true;
json_safe(null) ->
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
    error_result(Reason, true).

error_result(Reason, CleanupConfirmed) ->
    #{
        <<"status">> => <<"error">>,
        <<"reason_code">> => atom_to_binary(Reason),
        <<"cleanup_confirmed">> => CleanupConfirmed
    }.

drain_exit(Worker) ->
    receive
        {'EXIT', Worker, _Reason} -> ok
    after 0 ->
        ok
    end.
