-module(observer_cli_trace).

-export([call/2, stop_all/0, wait_cleanup/1]).

-ifdef(TEST).
-export([
    test_helpers/0,
    clear_trace/0,
    forced_reason/1,
    outcome_events/1,
    io_reply/1,
    module_md5/1,
    parse_pid/1,
    format_event/6,
    final_drain/1,
    wait_formatter/3,
    checked_helper_shutdown/3,
    drain_forced/2,
    outcome_payload/1,
    owner_result/3,
    finalize_failure_result/4,
    owner_init/0,
    verify_cleanup/1,
    wait_fixed_names/0,
    trace_pattern_off/1,
    stop_helper_checked/2,
    stop_helper/1,
    await_timeout_cleanup/6,
    await_owner_down/4,
    fallback_cleanup/3,
    wait_trace/3,
    stop_owner/2,
    await_stop_ack/6,
    await_helper_down/2,
    await_killed_helper/2,
    monitor_failure/3,
    wait_fixed_names/1,
    silent_io/0
]).
-endif.

-define(OWNER, observer_cli_trace_owner).
-define(RECON_VERSION, "2.5.6").
-define(MAX_EVENTS, 1000).
-define(ACK_TIMEOUT_MS, 1000).
-define(STOP_TIMEOUT_MS, 2000).
-define(CLEAR_RESULT_KEY, observer_cli_trace_clear_result).

-spec call(pid(), map()) -> map().
call(Controller, Request) when is_pid(Controller), is_map(Request) ->
    case validate(Request) of
        {ok, Trace} -> admit(Controller, Trace);
        {error, Category, Reason} -> response(error, Category, Reason, null, [])
    end;
call(_Controller, _Request) ->
    response(error, argument, invalid_request, null, []).

-spec stop_all() -> map().
stop_all() ->
    Warning = global_warning(),
    case recon_capability() of
        ok -> do_stop_all(Warning);
        {error, Reason} -> response(error, capability, Reason, null, [Warning])
    end.

do_stop_all(Warning) ->
    case whereis(?OWNER) of
        undefined ->
            clear_trace(),
            _ = wait_fixed_names(),
            response(
                error,
                cleanup,
                cleanup_unconfirmed,
                (forced_capture(cleanup_unconfirmed, []))#{cleanup_confirmed => false},
                [Warning]
            );
        Owner ->
            stop_owner(Owner, Warning)
    end.

validate(Request) ->
    case maps:get(replace_existing_trace, Request, false) of
        true ->
            case recon_capability(Request) of
                ok -> validate_mfa(Request);
                {error, Reason} -> {error, capability, Reason}
            end;
        false ->
            {error, argument, replace_existing_trace_required}
    end.

recon_capability() ->
    recon_capability(#{}).

recon_capability(Request) ->
    _ = application:load(recon),
    ExpectedVersion = expected_recon_version(Request),
    case
        {
            application:get_key(recon, vsn),
            code:ensure_loaded(recon_trace)
        }
    of
        {{ok, ExpectedVersion}, {module, recon_trace}} ->
            case
                erlang:function_exported(recon_trace, calls, 3) andalso
                    erlang:function_exported(recon_trace, clear, 0)
            of
                true -> ok;
                false -> {error, capability_unavailable}
            end;
        _ ->
            {error, capability_unavailable}
    end.

validate_mfa(#{mfa := Text} = Request) ->
    case parse_mfa(Text) of
        {ok, {Module, Function, Arity} = MFA} ->
            case
                is_tuple(code:is_loaded(Module)) andalso
                    erlang:function_exported(Module, Function, Arity)
            of
                true -> validate_pid(Request, MFA);
                false -> {error, capability, mfa_unavailable}
            end;
        error ->
            {error, argument, invalid_mfa}
    end;
validate_mfa(_Request) ->
    {error, argument, invalid_mfa}.

parse_mfa(Text) when is_binary(Text) ->
    parse_mfa(binary_to_list(Text));
parse_mfa(Text) when is_list(Text), length(Text) =< 768 ->
    case string:split(Text, ":", all) of
        [ModuleText, FunctionArity] ->
            case string:split(FunctionArity, "/", all) of
                [FunctionText, ArityText] -> existing_mfa(ModuleText, FunctionText, ArityText);
                _ -> error
            end;
        _ ->
            error
    end;
parse_mfa(_Text) ->
    error.

existing_mfa(ModuleText, FunctionText, ArityText) ->
    try
        true = exact_name(ModuleText),
        true = exact_name(FunctionText),
        Module = list_to_existing_atom(ModuleText),
        Function = list_to_existing_atom(FunctionText),
        Arity = list_to_integer(ArityText),
        true = Arity >= 0 andalso Arity =< 255,
        {ok, {Module, Function, Arity}}
    catch
        _:_ -> error
    end.

exact_name(Name) ->
    Name =/= [] andalso Name =/= "_" andalso Name =/= "*".

validate_pid(#{pid := Text} = Request, MFA) ->
    case parse_pid(Text) of
        {ok, Pid} -> validate_bounds(Request, MFA, Pid);
        error -> {error, safety_refusal, invalid_trace_pid}
    end;
validate_pid(_Request, _MFA) ->
    {error, argument, trace_pid_required}.

parse_pid(Text) when is_binary(Text) ->
    parse_pid(binary_to_list(Text));
parse_pid(Text) when is_list(Text), length(Text) =< 128 ->
    try list_to_pid(Text) of
        Pid when node(Pid) =:= node() ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> error
            end;
        _ ->
            error
    catch
        _:_ -> error
    end;
parse_pid(_Text) ->
    error.

validate_bounds(Request, MFA, Pid) ->
    Duration = maps:get(duration_ms, Request, 10000),
    Max = maps:get(max, Request, 100),
    case valid_duration(Duration) andalso valid_max(Max) of
        true ->
            Trace = #{
                mfa => MFA,
                pid => Pid,
                duration_ms => Duration,
                max => Max,
                started_timestamp => os:timestamp(),
                module_md5 => module_md5(element(1, MFA))
            },
            {ok, test_request_options(Request, Trace)};
        false ->
            {error, argument, invalid_trace_bounds}
    end.

valid_duration(Duration) ->
    is_integer(Duration) andalso Duration >= 100 andalso Duration =< 60000.

valid_max(Max) when is_integer(Max) -> Max >= 1 andalso Max =< 1000;
valid_max({Count, 1000}) -> is_integer(Count) andalso Count >= 1 andalso Count =< 200;
valid_max(_Max) -> false.

module_md5(Module) ->
    try erlang:get_module_info(Module, md5) of
        Md5 when is_binary(Md5) -> Md5
    catch
        error:badarg -> null
    end.

admit(Controller, Trace) ->
    Dispatcher = self(),
    Owner = spawn(fun owner_init/0),
    try register(?OWNER, Owner) of
        true ->
            Ref = make_ref(),
            OwnerRef = erlang:monitor(process, Owner),
            Owner ! {start, Dispatcher, Controller, Ref, Trace},
            dispatch_loop(Owner, OwnerRef, Ref, Trace, [], false)
    catch
        error:badarg ->
            exit(Owner, kill),
            response(error, safety_refusal, trace_busy, null, [global_warning()])
    end.

dispatch_loop(Owner, OwnerRef, Ref, Trace, Helpers, Entered) ->
    receive
        {Ref, ready, Manifest} ->
            dispatch_loop(Owner, OwnerRef, Ref, Trace, maps:values(Manifest), Entered);
        {Ref, calls_entered} ->
            dispatch_loop(Owner, OwnerRef, Ref, Trace, Helpers, true);
        {Ref, result, Result} ->
            await_owner_down(Owner, OwnerRef, Ref, Result);
        {'DOWN', OwnerRef, process, Owner, _Reason} ->
            fallback_cleanup(Trace, Helpers, Entered)
    after dispatch_timeout(Trace) ->
        Owner ! {force_stop, Ref, dispatcher_timeout},
        await_timeout_cleanup(Owner, OwnerRef, Ref, Trace, Helpers, Entered)
    end.

await_timeout_cleanup(Owner, OwnerRef, Ref, Trace, Helpers, Entered) ->
    receive
        {Ref, result, Result} ->
            await_owner_down(Owner, OwnerRef, Ref, Result);
        {'DOWN', OwnerRef, process, Owner, _Reason} ->
            fallback_cleanup(Trace, Helpers, Entered)
    after ?STOP_TIMEOUT_MS ->
        response(
            error,
            cleanup,
            cleanup_unconfirmed,
            (with_selector(Trace, forced_capture(cleanup_unconfirmed, [])))#{
                cleanup_confirmed => false
            },
            [global_warning()]
        )
    end.

await_owner_down(Owner, OwnerRef, Ref, Result) ->
    receive
        {'DOWN', OwnerRef, process, Owner, _Reason} ->
            case whereis(?OWNER) of
                undefined -> Result;
                _ -> response(error, cleanup, cleanup_unconfirmed, null, [global_warning()])
            end;
        {Ref, ready, _Manifest} ->
            await_owner_down(Owner, OwnerRef, Ref, Result)
    after ?STOP_TIMEOUT_MS ->
        response(error, cleanup, cleanup_unconfirmed, null, [global_warning()])
    end.

fallback_cleanup(Trace, Helpers, Entered) ->
    case Entered of
        true -> clear_trace();
        false -> ok
    end,
    stop_helpers(Helpers),
    case Entered andalso verify_cleanup(Trace) =:= ok of
        true ->
            Capture = with_selector(Trace, forced_capture(capture_internal_error, [])),
            response(error, internal, capture_internal_error, Capture, [global_warning()]);
        false ->
            Capture = (with_selector(Trace, forced_capture(cleanup_unconfirmed, [])))#{
                cleanup_confirmed => false
            },
            response(error, cleanup, cleanup_unconfirmed, Capture, [global_warning()])
    end.

owner_init() ->
    receive
        {start, Dispatcher, Controller, Ref, Trace} ->
            owner_start(Dispatcher, Controller, Ref, Trace)
    after ?ACK_TIMEOUT_MS ->
        ok
    end.

owner_start(Dispatcher, Controller, Ref, Trace) ->
    process_flag(trap_exit, true),
    DispatcherMon = erlang:monitor(process, Dispatcher),
    ControllerMon = erlang:monitor(process, Controller),
    TraceeMon = erlang:monitor(process, maps:get(pid, Trace)),
    Session = make_ref(),
    CollectorCap = maps:get(test_event_cap, Trace, ?MAX_EVENTS),
    Collector = spawn_link(fun() -> collector([], 0, 0, false, CollectorCap) end),
    SilentIO = spawn_link(fun silent_io/0),
    try
        CollectorMon = erlang:monitor(process, Collector),
        SilentMon = erlang:monitor(process, SilentIO),
        true = group_leader(SilentIO, self()),
        Manifest = #{collector => Collector, silent_io => SilentIO},
        put(observer_cli_trace_helpers, Manifest),
        State = Trace#{
            dispatcher => Dispatcher,
            dispatcher_mon => DispatcherMon,
            controller_mon => ControllerMon,
            tracee_mon => TraceeMon,
            collector => Collector,
            collector_mon => CollectorMon,
            silent_io => SilentIO,
            silent_mon => SilentMon,
            session => Session,
            ref => Ref
        },
        test_before_ready(State),
        Dispatcher ! {Ref, ready, Manifest},
        Result = run_owner(State),
        Dispatcher ! {Ref, result, Result}
    after
        stop_helpers([Collector, SilentIO])
    end.

run_owner(State) ->
    Dispatcher = maps:get(dispatcher, State),
    Ref = maps:get(ref, State),
    case prepare_owner(State) of
        ok ->
            Dispatcher ! {Ref, calls_entered},
            Outcome = entered_trace(State),
            finish_owner_safely(State, Outcome);
        error ->
            stop_helpers([maps:get(collector, State), maps:get(silent_io, State)]),
            response(error, internal, helper_setup_failed, null, [global_warning()])
    end.

prepare_owner(State) ->
    try test_before_calls(State) of
        _ -> ok
    catch
        _:_ -> error
    end.

entered_trace(State) ->
    Outcome =
        try
            Matches = start_recon(State),
            test_after_calls(State),
            case Matches of
                0 ->
                    {error, capability, mfa_not_traceable};
                _ ->
                    Recon = recon_processes(),
                    case {maps:get(tracer, Recon), maps:get(formatter, Recon)} of
                        {Tracer, Formatter} when is_pid(Tracer), is_pid(Formatter) ->
                            wait_trace(State, Recon, deadline(State));
                        {undefined, Formatter} when is_pid(Formatter) ->
                            wait_formatter(State, Recon, deadline(State));
                        {_Tracer, undefined} ->
                            completed_before_attach(State);
                        _ ->
                            setup_failure_outcome(State)
                    end
            end
        catch
            _:_ -> setup_failure_outcome(State)
        after
            clear_trace()
        end,
    drain_forced(State, Outcome).

setup_failure_outcome(State) ->
    receive
        {'DOWN', Mon, process, _Pid, Reason} -> monitor_failure(State, Mon, Reason)
    after 0 ->
        {forced, internal, capture_internal_error}
    end.

finish_owner_safely(State, Outcome) ->
    try finish_owner(State, Outcome) of
        Result -> Result
    catch
        _:_ -> finalize_failure(State, Outcome)
    end.

finalize_failure(State, Outcome) ->
    clear_trace(),
    stop_helpers([maps:get(collector, State), maps:get(silent_io, State)]),
    {Events, Rejected} = outcome_payload(Outcome),
    finalize_failure_result(State, Events, Rejected, verify_cleanup(State)).

finalize_failure_result(State, Events, Rejected, Verification) ->
    case Verification of
        ok ->
            Capture = with_selector(
                State, forced_capture(capture_internal_error, Events, Rejected)
            ),
            response(error, internal, capture_internal_error, Capture, [global_warning()]);
        {error, _Reason} ->
            Capture = (with_selector(
                State, forced_capture(cleanup_unconfirmed, Events, Rejected)
            ))#{
                cleanup_confirmed => false
            },
            response(
                error,
                cleanup,
                cleanup_unconfirmed,
                Capture,
                [global_warning()]
            )
    end.

start_recon(State) ->
    Collector = maps:get(collector, State),
    Session = maps:get(session, State),
    Started = maps:get(started_timestamp, State),
    ExpectedPid = maps:get(pid, State),
    ExpectedMFA = maps:get(mfa, State),
    Formatter = fun(Message) ->
        format_event(Message, ExpectedPid, ExpectedMFA, Collector, Session, Started)
    end,
    ok = clear_fixed_name_ports(),
    clear_extended_patterns(),
    recon_trace:calls(
        ExpectedMFA,
        maps:get(max, State),
        [
            {pid, ExpectedPid},
            {args, arity},
            {timestamp, trace},
            {scope, global},
            {formatter, Formatter},
            {io_server, maps:get(silent_io, State)}
        ]
    ).

recon_processes() ->
    Tracer = whereis(recon_trace_tracer),
    Formatter = whereis(recon_trace_formatter),
    #{
        tracer => Tracer,
        tracer_mon => monitor_if_pid(Tracer),
        formatter => Formatter,
        formatter_mon => monitor_if_pid(Formatter)
    }.

monitor_if_pid(Pid) when is_pid(Pid) -> erlang:monitor(process, Pid);
monitor_if_pid(_Pid) -> undefined.

deadline(State) ->
    erlang:monotonic_time(millisecond) + maps:get(duration_ms, State).

wait_trace(State, Recon, Deadline) ->
    Tracer = maps:get(tracer, Recon),
    TracerMon = maps:get(tracer_mon, Recon),
    Ref = maps:get(ref, State),
    receive
        {'DOWN', TracerMon, process, Tracer, normal} when is_reference(TracerMon) ->
            wait_formatter(State, Recon#{tracer_exit_reason => normal}, Deadline);
        {'DOWN', TracerMon, process, Tracer, noproc} when is_reference(TracerMon) ->
            wait_formatter(State, Recon, Deadline);
        {'DOWN', TracerMon, process, Tracer, _Reason} when is_reference(TracerMon) ->
            {forced, internal, capture_internal_error};
        {'EXIT', Tracer, Reason} when is_pid(Tracer) ->
            wait_trace(State, Recon#{tracer_exit_reason => Reason}, Deadline);
        {stop_request, Stopper, StopRef} ->
            Stopper ! {StopRef, armed, self()},
            {forced, success, stopped, {Stopper, StopRef}};
        {force_stop, Ref, Reason} ->
            forced_reason(Reason);
        {'DOWN', Mon, process, _Pid, Reason} ->
            monitor_failure(State, Mon, Reason)
    after remaining(Deadline) ->
        {forced, success, duration_elapsed}
    end.

wait_formatter(State, Recon, Deadline) ->
    Formatter = maps:get(formatter, Recon),
    FormatterMon = maps:get(formatter_mon, Recon),
    StateRef = maps:get(ref, State),
    case FormatterMon of
        undefined ->
            completed_before_attach(State, maps:get(tracer_exit_reason, Recon, unknown));
        _ ->
            receive
                {'DOWN', FormatterMon, process, Formatter, normal} ->
                    completed_before_attach(State, normal);
                {'DOWN', FormatterMon, process, Formatter, noproc} ->
                    completed_before_attach(
                        State, maps:get(tracer_exit_reason, Recon, unknown)
                    );
                {'DOWN', FormatterMon, process, Formatter, _Reason} ->
                    {forced, internal, capture_internal_error};
                {stop_request, Stopper, StopRef} ->
                    Stopper ! {StopRef, armed, self()},
                    {forced, success, stopped, {Stopper, StopRef}};
                {force_stop, StateRef, Reason} ->
                    forced_reason(Reason);
                {'DOWN', Mon, process, _Pid, Reason} ->
                    monitor_failure(State, Mon, Reason)
            after remaining(Deadline) ->
                {forced, success, duration_elapsed}
            end
    end.

final_drain(State) ->
    Collector = maps:get(collector, State),
    CollectorMon = maps:get(collector_mon, State),
    Ref = make_ref(),
    Collector ! {final, self(), Ref},
    receive
        {Ref, Events, Count, Rejected, Truncated} ->
            {natural, natural_reason(maps:get(max, State)), Events, Truncated,
                Count - length(Events), Rejected};
        {'DOWN', CollectorMon, process, Collector, _Reason} ->
            {forced, internal, capture_internal_error}
    after ?ACK_TIMEOUT_MS ->
        {forced, internal, capture_internal_error}
    end.

completed_before_attach(State) ->
    completed_before_attach(State, unknown).

completed_before_attach(State, ExitReason) ->
    case final_drain(State) of
        {natural, _Reason, Events, _Truncated, Dropped, Rejected} = Natural ->
            Forwarded = length(Events) + Dropped + Rejected,
            case
                natural_completion_confirmed(
                    maps:get(max, State), Forwarded, State, ExitReason
                )
            of
                true -> Natural;
                false -> with_forced_payload(setup_failure_outcome(State), Events, Rejected)
            end;
        Failure ->
            Failure
    end.

natural_threshold_reached(Max, Forwarded) when is_integer(Max) ->
    Forwarded >= Max;
natural_threshold_reached({Max, _Window}, Forwarded) ->
    Forwarded >= Max + 1.

natural_completion_confirmed(Max, Forwarded, _State, _ExitReason) when is_integer(Max) ->
    natural_threshold_reached(Max, Forwarded);
natural_completion_confirmed(Max, Forwarded, State, ExitReason) ->
    natural_threshold_reached(Max, Forwarded) andalso
        recon_exit_reason(State, ExitReason) =:= normal.

recon_exit_reason(_State, Reason) when Reason =/= unknown ->
    Reason;
recon_exit_reason(State, unknown) ->
    Collector = maps:get(collector, State),
    SilentIO = maps:get(silent_io, State, undefined),
    receive
        {'EXIT', Pid, Reason} when Pid =/= Collector, Pid =/= SilentIO -> Reason
    after ?ACK_TIMEOUT_MS ->
        unknown
    end.

with_forced_payload({forced, Category, Reason}, Events, Rejected) ->
    {forced, Category, Reason, Events, Rejected}.

drain_forced(State, {forced, Category, Reason}) ->
    case final_drain(State) of
        {natural, _NaturalReason, Events, _Truncated, _Dropped, Rejected} ->
            {forced, Category, Reason, Events, Rejected};
        Failure ->
            Failure
    end;
drain_forced(State, {forced, Category, Reason, Stopper}) ->
    case final_drain(State) of
        {natural, _NaturalReason, Events, _Truncated, _Dropped, Rejected} ->
            {forced, Category, Reason, Events, Rejected, Stopper};
        Failure ->
            Failure
    end;
drain_forced(_State, Outcome) ->
    Outcome.

natural_reason(Max) when is_integer(Max) -> limit_reached;
natural_reason({_Count, 1000}) -> rate_exceeded.

forced_reason(controller_disconnected) -> {forced, controller, controller_disconnected};
forced_reason(dispatcher_timeout) -> {forced, internal, capture_internal_error};
forced_reason(_Reason) -> {forced, internal, capture_internal_error}.

monitor_failure(State, Mon, _Reason) ->
    ControllerMon = maps:get(controller_mon, State),
    DispatcherMon = maps:get(dispatcher_mon, State),
    TraceeMon = maps:get(tracee_mon, State),
    case Mon of
        ControllerMon ->
            {forced, controller, controller_disconnected};
        DispatcherMon ->
            {forced, internal, dispatcher_disconnected};
        TraceeMon ->
            {forced, safety_refusal, tracee_exited};
        _ ->
            {forced, internal, capture_internal_error}
    end.

finish_owner(State, Outcome) ->
    Verification = verify_cleanup(State),
    test_before_helper_stop(State),
    {CheckedOutcome, CheckedVerification} = checked_helper_shutdown(
        State, Outcome, Verification
    ),
    Result = owner_result(State, CheckedOutcome, CheckedVerification),
    notify_stopper(Outcome, Result),
    Result.

checked_helper_shutdown(State, Outcome, Verification) ->
    Collector = stop_helper_checked(maps:get(collector, State), maps:get(collector_mon, State)),
    SilentIO = stop_helper_checked(maps:get(silent_io, State), maps:get(silent_mon, State)),
    case {Verification, Collector, SilentIO} of
        {ok, ok, ok} -> {Outcome, ok};
        {{error, _Reason}, _, _} -> {Outcome, {error, cleanup_unconfirmed}};
        {_, cleanup_unconfirmed, _} -> {Outcome, {error, cleanup_unconfirmed}};
        {_, _, cleanup_unconfirmed} -> {Outcome, {error, cleanup_unconfirmed}};
        {ok, helper_failed, _} -> {helper_failure_outcome(Outcome), ok};
        {ok, _, helper_failed} -> {helper_failure_outcome(Outcome), ok}
    end.

helper_failure_outcome(Outcome) ->
    {Events, Rejected} = outcome_payload(Outcome),
    {forced, internal, capture_internal_error, Events, Rejected}.

outcome_payload({natural, _Reason, Events, _Truncated, _Dropped, Rejected}) ->
    {Events, Rejected};
outcome_payload({forced, _Category, _Reason, Events, Rejected}) ->
    {Events, Rejected};
outcome_payload({forced, _Category, _Reason, Events, Rejected, _Stopper}) ->
    {Events, Rejected};
outcome_payload(_Outcome) ->
    {[], 0}.

owner_result(State, Outcome, ok) ->
    EndMd5 = end_module_md5(State),
    Partial = EndMd5 =/= maps:get(module_md5, State),
    case Outcome of
        {natural, Reason, Events, Truncated, Dropped, Rejected} ->
            Incomplete = Partial orelse Truncated orelse Dropped > 0 orelse Rejected > 0,
            Capture = #{
                status =>
                    case Incomplete of
                        true -> partial;
                        false -> complete
                    end,
                reason => Reason,
                trace_complete => not Incomplete,
                truncated => Truncated orelse Rejected > 0,
                dropped_count =>
                    case Rejected of
                        0 -> Dropped;
                        _ -> null
                    end,
                events => Events,
                module_reloaded => Partial,
                interference_detected => Rejected > 0,
                coverage => external_global_calls_only,
                cleanup_confirmed => true
            },
            response(ok, success, Reason, with_selector(State, Capture), [global_warning()]);
        {forced, Category, Reason, Events, Rejected} ->
            response(
                forced_status(Category),
                Category,
                Reason,
                with_selector(State, forced_capture(Reason, Events, Rejected)),
                [global_warning()]
            );
        {forced, Category, Reason, Events, Rejected, _Stopper} ->
            response(
                forced_status(Category),
                Category,
                Reason,
                with_selector(State, forced_capture(Reason, Events, Rejected)),
                [global_warning()]
            );
        {error, Category, Reason} ->
            response(error, Category, Reason, null, [global_warning()])
    end;
owner_result(State, Outcome, {error, _Reason}) ->
    {Events, Rejected} = outcome_payload(Outcome),
    Capture = (with_selector(State, forced_capture(cleanup_unconfirmed, Events, Rejected)))#{
        cleanup_confirmed => false
    },
    response(error, cleanup, cleanup_unconfirmed, Capture, [global_warning()]).

forced_status(success) -> ok;
forced_status(_Category) -> error.

-ifdef(TEST).
outcome_events({natural, _Reason, Events, _Truncated, _Dropped, _Rejected}) -> Events;
outcome_events({forced, _Category, _Reason, Events, _Rejected}) -> Events;
outcome_events({forced, _Category, _Reason, Events, _Rejected, _Stopper}) -> Events;
outcome_events(_Outcome) -> [].
-endif.

forced_capture(Reason, Events) ->
    forced_capture(Reason, Events, 0).

forced_capture(Reason, Events, Rejected) ->
    #{
        status => partial,
        reason => Reason,
        trace_complete => false,
        truncated => true,
        dropped_count => null,
        events => Events,
        interference_detected => Rejected > 0,
        coverage => external_global_calls_only,
        cleanup_confirmed => true
    }.

with_selector(#{mfa := {Module, Function, Arity}, pid := Pid}, Capture) ->
    Capture#{
        tracee => {identifier, pid, Pid},
        mfa => {mfa, Module, Function, Arity}
    };
with_selector(_State, Capture) ->
    Capture.

notify_stopper({forced, _Category, _Reason, _Events, _Rejected, {Stopper, StopRef}}, Result) ->
    Stopper ! {StopRef, cleanup_ack, Result};
notify_stopper(_Outcome, _Result) ->
    ok.

verify_cleanup(State) ->
    case get(?CLEAR_RESULT_KEY) of
        {error, _Reason} = Error ->
            Error;
        _ ->
            case wait_fixed_names() of
                ok ->
                    Pid = maps:get(pid, State),
                    MFA = maps:get(mfa, State),
                    case {call_flag_off(Pid), trace_pattern_off(MFA)} of
                        {true, true} -> ok;
                        _ -> {error, cleanup_unconfirmed}
                    end;
                Error ->
                    Error
            end
    end.

wait_fixed_names() ->
    wait_fixed_names(100).

wait_fixed_names(0) ->
    {error, cleanup_unconfirmed};
wait_fixed_names(Attempts) ->
    case {whereis(recon_trace_tracer), whereis(recon_trace_formatter)} of
        {undefined, undefined} ->
            ok;
        _ ->
            timer:sleep(10),
            wait_fixed_names(Attempts - 1)
    end.

call_flag_off(Pid) ->
    case erlang:trace_info(Pid, flags) of
        undefined -> true;
        {flags, Flags} -> not lists:member(call, Flags)
    end.

trace_pattern_off(MFA) ->
    case erlang:trace_info(MFA, traced) of
        undefined -> true;
        {traced, false} -> true;
        {traced, undefined} -> true;
        _ -> false
    end.

format_event(
    {trace_ts, Tracee, call, {Module, Function, Arity} = MFA, Timestamp},
    Tracee,
    MFA,
    Collector,
    Session,
    Started
) when
    is_pid(Tracee), is_atom(Module), is_atom(Function), is_integer(Arity)
->
    EventRef = make_ref(),
    Offset = max(0, timer:now_diff(Timestamp, Started) div 1000),
    Event = #{
        tracee => {identifier, pid, Tracee},
        mfa => {mfa, Module, Function, Arity},
        offset_ms => Offset
    },
    Collector ! {event, self(), Session, EventRef, Event},
    await_collector(Collector, Session, EventRef);
format_event(_Message, _ExpectedPid, _ExpectedMFA, Collector, Session, _Started) ->
    EventRef = make_ref(),
    Collector ! {interference, self(), Session, EventRef},
    await_collector(Collector, Session, EventRef).

await_collector(Collector, Session, EventRef) ->
    receive
        {Session, EventRef, ack} -> [];
        {'DOWN', _Mon, process, Collector, _Reason} -> erlang:error(collector_down)
    after ?ACK_TIMEOUT_MS ->
        erlang:error(collector_ack_timeout)
    end.

collector(Events, Count, Rejected, Truncated, Cap) ->
    receive
        {event, Formatter, Session, EventRef, Event} ->
            {NextEvents, NextTruncated} =
                case Count < Cap of
                    true -> {[Event | Events], Truncated};
                    false -> {Events, true}
                end,
            Formatter ! {Session, EventRef, ack},
            collector(NextEvents, Count + 1, Rejected, NextTruncated, Cap);
        {interference, Formatter, Session, EventRef} ->
            Formatter ! {Session, EventRef, ack},
            collector(Events, Count, Rejected + 1, Truncated, Cap);
        {final, Owner, Ref} ->
            Owner ! {Ref, lists:reverse(Events), Count, Rejected, Truncated},
            collector(Events, Count, Rejected, Truncated, Cap);
        {test_count, From, Ref} ->
            From ! {Ref, Count},
            collector(Events, Count, Rejected, Truncated, Cap);
        {stop, Owner, Ref} ->
            Owner ! {Ref, stopping},
            ok;
        stop ->
            ok
    end.

silent_io() ->
    receive
        {io_request, From, ReplyAs, Request} ->
            From ! {io_reply, ReplyAs, io_reply(Request)},
            silent_io();
        {stop, Owner, Ref} ->
            Owner ! {Ref, stopping},
            ok;
        stop ->
            ok;
        _Other ->
            silent_io()
    end.

io_reply({put_chars, _Characters}) -> ok;
io_reply({put_chars, _Encoding, _Characters}) -> ok;
io_reply({put_chars, Module, Function, Args}) -> io_apply(Module, Function, Args);
io_reply({put_chars, _Encoding, Module, Function, Args}) -> io_apply(Module, Function, Args);
io_reply({requests, Requests}) -> io_requests(Requests);
io_reply({get_geometry, _}) -> {error, enotsup};
io_reply(_Request) -> {error, enotsup}.

io_apply(Module, Function, Args) ->
    try erlang:apply(Module, Function, Args) of
        _Characters -> ok
    catch
        _:_ -> {error, request}
    end.

io_requests([]) ->
    ok;
io_requests([Request | Rest]) ->
    case io_reply(Request) of
        ok -> io_requests(Rest);
        Error -> Error
    end.

stop_helpers(Pids) ->
    lists:foreach(fun stop_helper/1, Pids).

stop_helper_checked(Pid, Mon) ->
    case is_process_alive(Pid) of
        false ->
            erlang:demonitor(Mon, [flush]),
            helper_failed;
        true ->
            Ref = make_ref(),
            Pid ! {stop, self(), Ref},
            receive
                {Ref, stopping} -> await_helper_down(Pid, Mon);
                {'DOWN', Mon, process, Pid, _Reason} -> helper_failed
            after 50 ->
                exit(Pid, kill),
                await_killed_helper(Pid, Mon)
            end
    end.

await_helper_down(Pid, Mon) ->
    receive
        {'DOWN', Mon, process, Pid, normal} -> ok;
        {'DOWN', Mon, process, Pid, _Reason} -> helper_failed
    after ?ACK_TIMEOUT_MS ->
        exit(Pid, kill),
        await_killed_helper(Pid, Mon)
    end.

await_killed_helper(Pid, Mon) ->
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> cleanup_unconfirmed
    after ?ACK_TIMEOUT_MS ->
        erlang:demonitor(Mon, [flush]),
        cleanup_unconfirmed
    end.

stop_helper(Pid) when is_pid(Pid) ->
    Mon = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> ok
    after 50 ->
        exit(Pid, kill),
        receive
            {'DOWN', Mon, process, Pid, _Reason} -> ok
        after ?ACK_TIMEOUT_MS ->
            erlang:demonitor(Mon, [flush])
        end
    end;
stop_helper(_Other) ->
    ok.

stop_owner(Owner, Warning) ->
    OwnerMon = erlang:monitor(process, Owner),
    StopRef = make_ref(),
    Owner ! {stop_request, self(), StopRef},
    receive
        {StopRef, armed, Owner} ->
            clear_trace(),
            await_stop_ack(Owner, OwnerMon, StopRef, Warning, undefined, false);
        {'DOWN', OwnerMon, process, Owner, _Reason} ->
            clear_trace(),
            _ = wait_fixed_names(),
            response(
                error,
                cleanup,
                cleanup_unconfirmed,
                (forced_capture(cleanup_unconfirmed, []))#{cleanup_confirmed => false},
                [Warning]
            )
    after 500 ->
        clear_trace(),
        _ = wait_fixed_names(),
        erlang:demonitor(OwnerMon, [flush]),
        response(
            error,
            cleanup,
            cleanup_unconfirmed,
            (forced_capture(cleanup_unconfirmed, []))#{cleanup_confirmed => false},
            [Warning]
        )
    end.

await_stop_ack(Owner, OwnerMon, StopRef, Warning, Result, Down) ->
    case {Result, Down} of
        {Response, true} when is_map(Response) -> Response;
        _ ->
            receive
                {StopRef, cleanup_ack, Response} ->
                    await_stop_ack(Owner, OwnerMon, StopRef, Warning, Response, Down);
                {'DOWN', OwnerMon, process, Owner, _Reason} ->
                    await_stop_ack(Owner, OwnerMon, StopRef, Warning, Result, true)
            after ?STOP_TIMEOUT_MS ->
                response(
                    error,
                    cleanup,
                    cleanup_unconfirmed,
                    (forced_capture(cleanup_unconfirmed, []))#{cleanup_confirmed => false},
                    [Warning]
                )
            end
    end.

remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

dispatch_timeout(Trace) ->
    maps:get(test_dispatch_timeout_ms, Trace, maps:get(duration_ms, Trace) + 5000).

response(Status, Category, Reason, Capture, Warnings) ->
    #{
        status => Status,
        category => Category,
        reason => Reason,
        capture => Capture,
        warnings => Warnings
    }.

global_warning() ->
    #{
        code => global_trace_replacement,
        message => <<
            "This command clears legacy process/port trace flags and tracers plus static call ",
            "patterns, including on-load and call-memory patterns; recon 2.5.6 may also kill ",
            "processes or ports occupying its fixed tracer or formatter names. Dynamic trace ",
            "sessions are not directly cleared, but killing such an occupant can disable one."
        >>
    }.

clear_trace() ->
    PortResult = clear_fixed_name_ports(),
    LegacyResult =
        case PortResult of
            ok ->
                try
                    recon_trace:clear()
                catch
                    _:_ -> {error, cleanup_unconfirmed}
                end;
            PortError ->
                PortError
        end,
    ExtendedResult =
        try clear_extended_patterns() of
            ok -> ok
        catch
            _:_ -> {error, cleanup_unconfirmed}
        end,
    ClearResult =
        case {LegacyResult, ExtendedResult} of
            {ok, ok} -> ok;
            _ -> {error, cleanup_unconfirmed}
        end,
    case ClearResult of
        ok -> erase(?CLEAR_RESULT_KEY);
        ClearError -> put(?CLEAR_RESULT_KEY, ClearError)
    end,
    ClearResult.

clear_fixed_name_ports() ->
    case
        {
            clear_fixed_name_port(recon_trace_tracer),
            clear_fixed_name_port(recon_trace_formatter)
        }
    of
        {ok, ok} -> ok;
        _ -> {error, cleanup_unconfirmed}
    end.

clear_fixed_name_port(Name) ->
    case whereis(Name) of
        Port when is_port(Port) ->
            _ =
                try
                    erlang:port_close(Port)
                catch
                    _:_ -> false
                end,
            wait_fixed_name_port(Name, Port, 100);
        _ ->
            ok
    end.

wait_fixed_name_port(_Name, _Port, 0) ->
    {error, cleanup_unconfirmed};
wait_fixed_name_port(Name, Port, Attempts) ->
    case {whereis(Name), erlang:port_info(Port)} of
        {Current, undefined} when Current =/= Port ->
            ok;
        _ ->
            timer:sleep(10),
            wait_fixed_name_port(Name, Port, Attempts - 1)
    end.

clear_extended_patterns() ->
    _ = erlang:trace_pattern(
        on_load, false, [local, meta, call_count, call_time, call_memory]
    ),
    _ = erlang:trace_pattern(on_load, false, []),
    _ = erlang:trace_pattern({'_', '_', '_'}, false, [call_memory]),
    ok.

-spec wait_cleanup(non_neg_integer()) -> boolean().
wait_cleanup(Timeout) when is_integer(Timeout), Timeout >= 0 ->
    wait_cleanup_until(erlang:monotonic_time(millisecond) + Timeout).

wait_cleanup_until(Deadline) ->
    case {whereis(?OWNER), whereis(recon_trace_tracer), whereis(recon_trace_formatter)} of
        {undefined, undefined, undefined} ->
            true;
        _ ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true ->
                    timer:sleep(10),
                    wait_cleanup_until(Deadline);
                false ->
                    false
            end
    end.

-ifdef(TEST).
test_request_options(Request, Trace) ->
    maps:merge(
        Trace,
        maps:with(
            [
                test_after_calls,
                test_before_calls,
                test_before_ready,
                test_before_helper_stop,
                test_dispatch_timeout_ms,
                test_end_module_md5,
                test_event_cap
            ],
            Request
        )
    ).

expected_recon_version(Request) ->
    maps:get(test_recon_version, Request, ?RECON_VERSION).

end_module_md5(State) ->
    maps:get(test_end_module_md5, State, module_md5(element(1, maps:get(mfa, State)))).

test_before_calls(State) ->
    case maps:find(test_before_calls, State) of
        {ok, Fun} -> Fun();
        error -> ok
    end.

test_after_calls(State) ->
    case maps:find(test_after_calls, State) of
        {ok, Fun} -> Fun();
        error -> ok
    end.

test_before_ready(State) ->
    case maps:find(test_before_ready, State) of
        {ok, Fun} -> Fun();
        error -> ok
    end.

test_before_helper_stop(State) ->
    case maps:find(test_before_helper_stop, State) of
        {ok, Fun} -> Fun();
        error -> ok
    end.

test_helpers() ->
    case whereis(?OWNER) of
        undefined ->
            undefined;
        Owner ->
            case process_info(Owner, dictionary) of
                {dictionary, Dictionary} ->
                    proplists:get_value(observer_cli_trace_helpers, Dictionary);
                undefined ->
                    undefined
            end
    end.
-else.
test_request_options(_Request, Trace) ->
    Trace.

test_before_calls(_State) ->
    ok.

test_after_calls(_State) ->
    ok.

test_before_ready(_State) ->
    ok.

test_before_helper_stop(_State) ->
    ok.

expected_recon_version(_Request) ->
    ?RECON_VERSION.

end_module_md5(State) ->
    module_md5(element(1, maps:get(mfa, State))).
-endif.
