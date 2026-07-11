-module(observer_cli_trace).

-export([call/2, stop_all/0]).

-ifdef(TEST).
-export([test_helpers/0]).
-endif.

-define(OWNER, observer_cli_trace_owner).
-define(RECON_VERSION, "2.5.6").
-define(MAX_EVENTS, 1000).
-define(ACK_TIMEOUT_MS, 1000).
-define(STOP_TIMEOUT_MS, 2000).

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
            recon_trace:clear(),
            _ = wait_fixed_names(),
            response(
                error,
                cleanup,
                cleanup_unconfirmed,
                (forced_capture(stopped, []))#{cleanup_confirmed => false},
                [Warning]
            );
        Owner ->
            stop_owner(Owner, Warning)
    end.

validate(Request) ->
    case maps:get(replace_existing_trace, Request, false) of
        true ->
            case recon_capability() of
                ok -> validate_mfa(Request);
                {error, Reason} -> {error, capability, Reason}
            end;
        false ->
            {error, argument, replace_existing_trace_required}
    end.

recon_capability() ->
    _ = application:load(recon),
    case
        {
            application:get_key(recon, vsn),
            code:ensure_loaded(recon_trace)
        }
    of
        {{ok, ?RECON_VERSION}, {module, recon_trace}} ->
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
    case code:get_object_code(Module) of
        {Module, Beam, _Path} -> crypto:hash(md5, Beam);
        error -> null
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
    after maps:get(duration_ms, Trace) + 5000 ->
        Owner ! {force_stop, Ref, dispatcher_timeout},
        dispatch_loop(Owner, OwnerRef, Ref, Trace, Helpers, Entered)
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
    stop_helpers(Helpers),
    case Entered of
        true -> recon_trace:clear();
        false -> ok
    end,
    Capture = (forced_capture(owner_failed, []))#{cleanup_confirmed => false},
    case Entered andalso verify_cleanup(Trace) =:= ok of
        true -> response(error, internal, capture_internal_error, Capture, [global_warning()]);
        false -> response(error, cleanup, cleanup_unconfirmed, Capture, [global_warning()])
    end.

owner_init() ->
    receive
        {start, Dispatcher, Controller, Ref, Trace} ->
            owner_start(Dispatcher, Controller, Ref, Trace)
    end.

owner_start(Dispatcher, Controller, Ref, Trace) ->
    process_flag(trap_exit, true),
    DispatcherMon = erlang:monitor(process, Dispatcher),
    ControllerMon = erlang:monitor(process, Controller),
    TraceeMon = erlang:monitor(process, maps:get(pid, Trace)),
    Session = make_ref(),
    CollectorCap = maps:get(test_event_cap, Trace, ?MAX_EVENTS),
    Collector = spawn(fun() -> collector([], 0, false, CollectorCap) end),
    CollectorMon = erlang:monitor(process, Collector),
    SilentIO = spawn(fun silent_io/0),
    SilentMon = erlang:monitor(process, SilentIO),
    true = group_leader(SilentIO, self()),
    Manifest = #{collector => Collector, silent_io => SilentIO},
    put(observer_cli_trace_helpers, Manifest),
    Dispatcher ! {Ref, ready, Manifest},
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
    Result =
        try run_owner(State) of
            OwnerResult -> OwnerResult
        catch
            _:_ ->
                stop_helpers([Collector, SilentIO]),
                response(error, internal, helper_setup_failed, null, [global_warning()])
        end,
    Dispatcher ! {Ref, result, Result}.

run_owner(State) ->
    Dispatcher = maps:get(dispatcher, State),
    Ref = maps:get(ref, State),
    test_before_calls(State),
    Dispatcher ! {Ref, calls_entered},
    Outcome =
        try
            Matches = start_recon(State),
            case Matches of
                0 -> {error, capability, mfa_not_traceable};
                _ -> wait_trace(State, recon_processes(), deadline(State))
            end
        catch
            _:_ -> {error, internal, capture_internal_error}
        after
            recon_trace:clear()
        end,
    finish_owner(State, Outcome).

start_recon(State) ->
    Collector = maps:get(collector, State),
    Session = maps:get(session, State),
    Started = maps:get(started_timestamp, State),
    Formatter = fun(Message) -> format_event(Message, Collector, Session, Started) end,
    recon_trace:calls(
        maps:get(mfa, State),
        maps:get(max, State),
        [
            {pid, maps:get(pid, State)},
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
            wait_formatter(State, Recon, Deadline);
        {'DOWN', TracerMon, process, Tracer, _Reason} when is_reference(TracerMon) ->
            {forced, internal, capture_internal_error};
        {'EXIT', Tracer, _Reason} when is_pid(Tracer) ->
            wait_trace(State, Recon, Deadline);
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
            final_drain(State);
        _ ->
            receive
                {'DOWN', FormatterMon, process, Formatter, normal} ->
                    final_drain(State);
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
        {Ref, Events, Truncated} ->
            {natural, natural_reason(maps:get(max, State)), Events, Truncated};
        {'DOWN', CollectorMon, process, Collector, _Reason} ->
            {forced, internal, capture_internal_error}
    after ?ACK_TIMEOUT_MS ->
        {forced, internal, capture_internal_error}
    end.

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
    Result = owner_result(State, Outcome, Verification),
    notify_stopper(Outcome, Result),
    stop_helpers([maps:get(collector, State), maps:get(silent_io, State)]),
    Result.

owner_result(State, Outcome, ok) ->
    EndMd5 = module_md5(element(1, maps:get(mfa, State))),
    Partial = EndMd5 =/= maps:get(module_md5, State),
    case Outcome of
        {natural, Reason, Events, Truncated} ->
            Capture = #{
                status =>
                    case Partial of
                        true -> partial;
                        false -> complete
                    end,
                reason => Reason,
                trace_complete => not Partial,
                truncated => Truncated,
                dropped_count =>
                    case Truncated of
                        true -> null;
                        false -> 0
                    end,
                events => Events,
                module_reloaded => Partial,
                coverage => external_global_calls_only,
                cleanup_confirmed => true
            },
            response(ok, success, Reason, Capture, [global_warning()]);
        {forced, Category, Reason} ->
            response(forced_status(Category), Category, Reason, forced_capture(Reason, []), [
                global_warning()
            ]);
        {forced, Category, Reason, _Stopper} ->
            response(forced_status(Category), Category, Reason, forced_capture(Reason, []), [
                global_warning()
            ]);
        {error, Category, Reason} ->
            response(error, Category, Reason, null, [global_warning()])
    end;
owner_result(_State, Outcome, {error, _Reason}) ->
    Events = outcome_events(Outcome),
    Capture = (forced_capture(cleanup_unconfirmed, Events))#{cleanup_confirmed => false},
    response(error, cleanup, cleanup_unconfirmed, Capture, [global_warning()]).

forced_status(success) -> ok;
forced_status(_Category) -> error.

outcome_events({natural, _Reason, Events, _Truncated}) -> Events;
outcome_events(_Outcome) -> [].

forced_capture(Reason, Events) ->
    #{
        status => partial,
        reason => Reason,
        trace_complete => false,
        truncated => true,
        dropped_count => null,
        events => Events,
        coverage => external_global_calls_only,
        cleanup_confirmed => true
    }.

notify_stopper({forced, _Category, _Reason, {Stopper, StopRef}}, Result) ->
    Stopper ! {StopRef, cleanup_ack, Result};
notify_stopper(_Outcome, _Result) ->
    ok.

verify_cleanup(State) ->
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
    {trace_ts, Tracee, call, {Module, Function, Arity}, Timestamp}, Collector, Session, Started
) when
    is_pid(Tracee), is_atom(Module), is_atom(Function), is_integer(Arity)
->
    EventRef = make_ref(),
    Offset = max(0, timer:now_diff(Timestamp, Started) div 1000),
    Event = #{
        tracee => list_to_binary(pid_to_list(Tracee)),
        mfa => #{
            module => atom_to_binary(Module),
            function => atom_to_binary(Function),
            arity => Arity
        },
        offset_ms => Offset
    },
    Collector ! {event, self(), Session, EventRef, Event},
    receive
        {Session, EventRef, ack} -> [];
        {'DOWN', _Mon, process, Collector, _Reason} -> erlang:error(collector_down)
    after ?ACK_TIMEOUT_MS ->
        erlang:error(collector_ack_timeout)
    end;
format_event(_Message, _Collector, _Session, _Started) ->
    [].

collector(Events, Count, Truncated, Cap) ->
    receive
        {event, Formatter, Session, EventRef, Event} ->
            {NextEvents, NextTruncated} =
                case Count < Cap of
                    true -> {[Event | Events], Truncated};
                    false -> {Events, true}
                end,
            Formatter ! {Session, EventRef, ack},
            collector(NextEvents, Count + 1, NextTruncated, Cap);
        {final, Owner, Ref} ->
            Owner ! {Ref, lists:reverse(Events), Truncated},
            collector(Events, Count, Truncated, Cap);
        stop ->
            ok
    end.

silent_io() ->
    receive
        {io_request, From, ReplyAs, Request} ->
            From ! {io_reply, ReplyAs, io_reply(Request)},
            silent_io();
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
            recon_trace:clear(),
            await_stop_ack(Owner, OwnerMon, StopRef, Warning, undefined, false);
        {'DOWN', OwnerMon, process, Owner, _Reason} ->
            recon_trace:clear(),
            _ = wait_fixed_names(),
            response(
                error,
                cleanup,
                cleanup_unconfirmed,
                (forced_capture(stopped, []))#{cleanup_confirmed => false},
                [Warning]
            )
    after 500 ->
        recon_trace:clear(),
        _ = wait_fixed_names(),
        erlang:demonitor(OwnerMon, [flush]),
        response(
            error,
            cleanup,
            cleanup_unconfirmed,
            (forced_capture(stopped, []))#{cleanup_confirmed => false},
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
                    (forced_capture(stopped, []))#{cleanup_confirmed => false},
                    [Warning]
                )
            end
    end.

remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

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
            "This command clears all node-static tracing and recon 2.5.6 may kill processes ",
            "occupying its fixed tracer or formatter names."
        >>
    }.

-ifdef(TEST).
test_request_options(Request, Trace) ->
    maps:merge(Trace, maps:with([test_before_calls, test_event_cap], Request)).

test_before_calls(State) ->
    case maps:find(test_before_calls, State) of
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
-endif.
