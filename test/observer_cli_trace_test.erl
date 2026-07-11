-module(observer_cli_trace_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-export([fixture/0, recursive_fixture/1]).

fixture() ->
    ok.

recursive_fixture(0) ->
    ok;
recursive_fixture(Count) ->
    recursive_fixture(Count - 1).

validation_and_admission_do_not_clear_test_() ->
    {timeout, 10, fun validation_and_admission_do_not_clear/0}.

validation_and_admission_do_not_clear() ->
    cleanup(),
    MFA = {?MODULE, fixture, 0},
    1 = erlang:trace_pattern(MFA, true, []),
    Unconfirmed = observer_cli_trace:call(
        self(), maps:remove(replace_existing_trace, request(self()))
    ),
    ?assertEqual(replace_existing_trace_required, maps:get(reason, Unconfirmed)),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    Invalid = observer_cli_trace:call(
        self(), (request(self()))#{mfa => <<"missing:fixture/0">>}
    ),
    ?assertEqual(capability, maps:get(category, Invalid)),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    Collision = spawn(fun collision/0),
    true = register(observer_cli_trace_owner, Collision),
    Busy = observer_cli_trace:call(self(), request(self())),
    ?assertEqual(trace_busy, maps:get(reason, Busy)),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    unregister(observer_cli_trace_owner),
    Collision ! stop,
    1 = erlang:trace_pattern(MFA, false, []).

natural_count_drain_and_cleanup_test_() ->
    {timeout, 10, fun() -> natural_capture(1, 1, limit_reached) end}.

natural_rate_drain_and_cleanup_test_() ->
    {timeout, 10, fun() -> natural_capture({1, 1000}, 2, rate_exceeded) end}.

setup_replaces_global_trace_and_fixed_collision_test_() ->
    {timeout, 10, fun setup_replaces_global_trace_and_fixed_collision/0}.

setup_replaces_global_trace_and_fixed_collision() ->
    cleanup(),
    Tracee = tracee(),
    UnrelatedMFA = {erlang, node, 0},
    1 = erlang:trace_pattern(UnrelatedMFA, true, []),
    Collision = spawn(fun collision/0),
    true = register(recon_trace_formatter, Collision),
    {Caller, Ref} = start_call(self(), request(Tracee)),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    ?assertEqual(false, is_process_alive(Collision)),
    ?assertEqual({traced, false}, erlang:trace_info(UnrelatedMFA, traced)),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    [Warning] = maps:get(warnings, Result),
    ?assertEqual(global_trace_replacement, maps:get(code, Warning)),
    assert_clean(Tracee),
    Tracee ! stop.

response_cap_continues_natural_drain_test_() ->
    {timeout, 10, fun response_cap_continues_natural_drain/0}.

helper_finalize_failure_is_not_complete_test_() ->
    [
        {atom_to_list(Helper), fun() -> helper_finalize_failure(Helper) end}
     || Helper <- [collector, silent_io]
    ].

missing_recon_process_after_calls_fails_immediately_test_() ->
    [
        {atom_to_list(Name), fun() -> missing_recon_process_after_calls(Name) end}
     || Name <- [recon_trace_tracer, recon_trace_formatter]
    ].

setup_failure_prefers_queued_tracee_down_test_() ->
    {timeout, 10, fun setup_failure_prefers_queued_tracee_down/0}.

global_scope_excludes_local_recursion_test_() ->
    {timeout, 10, fun global_scope_excludes_local_recursion/0}.

formatter_crash_forces_cleanup_test_() ->
    {timeout, 10, fun formatter_crash_forces_cleanup/0}.

dispatcher_loss_forces_cleanup_test_() ->
    {timeout, 10, fun dispatcher_loss_forces_cleanup/0}.

module_reload_marks_capture_partial_test_() ->
    {timeout, 10, fun module_reload_marks_capture_partial/0}.

owner_absent_stop_clears_static_trace_conservatively_test_() ->
    {timeout, 10, fun owner_absent_stop_clears_static_trace_conservatively/0}.

silent_io_handles_protocol_without_output_test_() ->
    {timeout, 10, fun silent_io_handles_protocol_without_output/0}.

active_session_busy_does_not_clear_first_test_() ->
    {timeout, 10, fun active_session_busy_does_not_clear_first/0}.

unverified_recon_version_is_rejected_before_clear_test_() ->
    {timeout, 10, fun unverified_recon_version_is_rejected_before_clear/0}.

finalize_exception_is_capture_failure_test_() ->
    {timeout, 10, fun finalize_exception_is_capture_failure/0}.

entered_exception_is_capture_failure_test_() ->
    {timeout, 10, fun entered_exception_is_capture_failure/0}.

dispatcher_timeout_is_bounded_and_owner_cleans_later_test_() ->
    {timeout, 10, fun dispatcher_timeout_is_bounded_and_owner_cleans_later/0}.

response_cap_continues_natural_drain() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{max => 3, test_event_cap => 2, duration_ms => 3000},
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 3},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    Capture = maps:get(capture, Result),
    ?assertEqual(true, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(truncated, Capture)),
    ?assertEqual(2, length(maps:get(events, Capture))),
    ?assertEqual(null, maps:get(dropped_count, Capture)),
    assert_clean(Tracee),
    Tracee ! stop.

helper_finalize_failure(Helper) ->
    cleanup(),
    Tracee = tracee(),
    CrashHelper = fun() ->
        Helpers = get(observer_cli_trace_helpers),
        exit(maps:get(Helper, Helpers), kill)
    end,
    Request = (request(Tracee))#{
        duration_ms => 3000,
        test_before_helper_stop => CrashHelper
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

missing_recon_process_after_calls(Name) ->
    cleanup(),
    Tracee = tracee(),
    KillRegistered = fun() -> kill_registered(Name) end,
    Started = erlang:monotonic_time(millisecond),
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{duration_ms => 3000, test_after_calls => KillRegistered}
    ),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assert(erlang:monotonic_time(millisecond) - Started < 2000),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

setup_failure_prefers_queued_tracee_down() ->
    cleanup(),
    Tracee = tracee(),
    FailSetup = fun() ->
        exit(Tracee, kill),
        wait_until(fun() -> not is_process_alive(Tracee) end),
        kill_if_registered(recon_trace_formatter)
    end,
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{test_after_calls => FailSetup}
    ),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(tracee_exited, maps:get(reason, Result)),
    assert_forced(Result),
    assert_clean(Tracee).

global_scope_excludes_local_recursion() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{
        mfa => <<"observer_cli_trace_test:recursive_fixture/1">>,
        max => 10,
        duration_ms => 5000
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, recursive_fixture, 1}),
    Tracee ! {recursive, self(), 5},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    Stop = observer_cli_trace:stop_all(),
    ?assertEqual(ok, maps:get(status, Stop)),
    Result = receive_result(Caller, Ref),
    ?assertEqual(external_global_calls_only, maps:get(coverage, maps:get(capture, Result))),
    assert_clean_mfa(Tracee, {?MODULE, recursive_fixture, 1}),
    Tracee ! stop.

formatter_crash_forces_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    exit(whereis(recon_trace_formatter), kill),
    Result = receive_result(Caller, Ref),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

dispatcher_loss_forces_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Dispatcher, _Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    DispatcherMon = erlang:monitor(process, Dispatcher),
    exit(Dispatcher, kill),
    receive
        {'DOWN', DispatcherMon, process, Dispatcher, _Reason} -> ok
    after 1000 -> error(dispatcher_down_timeout)
    end,
    wait_until(fun() -> whereis(observer_cli_trace_owner) =:= undefined end),
    assert_clean(Tracee),
    Tracee ! stop.

module_reload_marks_capture_partial() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{
        duration_ms => 3000,
        test_end_module_md5 => <<"changed-generation">>
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    Capture = maps:get(capture, Result),
    ?assertEqual(partial, maps:get(status, Capture)),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(module_reloaded, Capture)),
    assert_clean(Tracee),
    Tracee ! stop.

owner_absent_stop_clears_static_trace_conservatively() ->
    cleanup(),
    MFA = {erlang, node, 0},
    1 = erlang:trace_pattern(MFA, true, []),
    Result = observer_cli_trace:stop_all(),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
    ?assertEqual({traced, false}, erlang:trace_info(MFA, traced)),
    ?assertEqual(undefined, whereis(observer_cli_trace_owner)).

silent_io_handles_protocol_without_output() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    #{silent_io := SilentIO} = wait_helpers(),
    ?assertEqual(ok, io_request(SilentIO, {put_chars, <<27, "]0;secret", 7>>})),
    ?assertEqual(ok, io_request(SilentIO, {put_chars, unicode, <<"secret">>})),
    ?assertEqual({error, enotsup}, io_request(SilentIO, {get_geometry, columns})),
    ?assertEqual(
        {error, enotsup},
        io_request(SilentIO, {requests, [{put_chars, <<"hidden">>}, {get_geometry, rows}]})
    ),
    _ = observer_cli_trace:stop_all(),
    Result = receive_result(Caller, Ref),
    ?assertEqual(external_global_calls_only, maps:get(coverage, maps:get(capture, Result))),
    assert_clean(Tracee),
    Tracee ! stop.

active_session_busy_does_not_clear_first() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Busy = observer_cli_trace:call(self(), request(Tracee)),
    ?assertEqual(trace_busy, maps:get(reason, Busy)),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    First = receive_result(Caller, Ref),
    ?assertEqual(ok, maps:get(status, First)),
    ?assertEqual(true, maps:get(trace_complete, maps:get(capture, First))),
    assert_clean(Tracee),
    Tracee ! stop.

unverified_recon_version_is_rejected_before_clear() ->
    cleanup(),
    MFA = {?MODULE, fixture, 0},
    1 = erlang:trace_pattern(MFA, true, []),
    Result = observer_cli_trace:call(
        self(), (request(self()))#{test_recon_version => "0.0.0"}
    ),
    ?assertEqual(capability_unavailable, maps:get(reason, Result)),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    1 = erlang:trace_pattern(MFA, false, []).

finalize_exception_is_capture_failure() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{
        duration_ms => 3000,
        test_before_helper_stop => fun() -> error(finalize_failed) end
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assert(is_map(maps:get(capture, Result))),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

entered_exception_is_capture_failure() ->
    cleanup(),
    Tracee = tracee(),
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{test_after_calls => fun() -> error(entered_failed) end}
    ),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assert(is_map(maps:get(capture, Result))),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

dispatcher_timeout_is_bounded_and_owner_cleans_later() ->
    cleanup(),
    Tracee = tracee(),
    Test = self(),
    BlockAfterCalls = fun() ->
        Test ! {after_calls, self()},
        receive
            release -> ok
        end
    end,
    Request = (request(Tracee))#{
        duration_ms => 5000,
        test_after_calls => BlockAfterCalls,
        test_dispatch_timeout_ms => 50
    },
    {Caller, Ref} = start_call(self(), Request),
    CallerMon = erlang:monitor(process, Caller),
    Owner =
        receive
            {after_calls, Pid} -> Pid
        after 1000 -> error(after_calls_timeout)
        end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
    receive
        {'DOWN', CallerMon, process, Caller, _Reason} -> ok
    after 1000 -> error(dispatcher_down_timeout)
    end,
    Owner ! release,
    wait_until(fun() -> whereis(observer_cli_trace_owner) =:= undefined end),
    assert_clean(Tracee),
    Tracee ! stop.

kill_registered(Name) ->
    Pid = whereis(Name),
    Mon = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> ok
    after 1000 -> error(recon_process_down_timeout)
    end.

kill_if_registered(Name) ->
    case whereis(Name) of
        undefined -> ok;
        _Pid -> kill_registered(Name)
    end.

collector_count(Collector) ->
    Ref = make_ref(),
    Collector ! {test_count, self(), Ref},
    receive
        {Ref, Count} -> Count
    after 1000 -> error(collector_count_timeout)
    end.

io_request(IO, Request) ->
    Ref = make_ref(),
    IO ! {io_request, self(), Ref, Request},
    receive
        {io_reply, Ref, Reply} -> Reply
    after 1000 -> error(io_reply_timeout)
    end.

natural_capture(Max, Calls, Reason) ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{max => Max, duration_ms => 3000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), Calls},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual(Reason, maps:get(reason, Result)),
    Capture = maps:get(capture, Result),
    ?assertEqual(true, maps:get(trace_complete, Capture)),
    ?assertEqual(external_global_calls_only, maps:get(coverage, Capture)),
    ?assertEqual(Calls, length(maps:get(events, Capture))),
    assert_clean(Tracee),
    Tracee ! stop.

duration_forces_loss_marking_test_() ->
    {timeout, 10, fun duration_forces_loss_marking/0}.

duration_forces_loss_marking() ->
    cleanup(),
    Tracee = tracee(),
    Result = observer_cli_trace:call(self(), (request(Tracee))#{duration_ms => 100}),
    ?assertEqual(duration_elapsed, maps:get(reason, Result)),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

stop_all_waits_for_owner_cleanup_test_() ->
    {timeout, 10, fun stop_all_waits_for_owner_cleanup/0}.

stop_all_waits_for_owner_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    Stop = observer_cli_trace:stop_all(),
    ?assertEqual(ok, maps:get(status, Stop)),
    ?assertEqual(stopped, maps:get(reason, Stop)),
    Call = receive_result(Caller, Ref),
    assert_forced(Call),
    assert_clean(Tracee),
    Tracee ! stop.

tracee_and_controller_loss_force_cleanup_test_() ->
    {timeout, 15, fun tracee_and_controller_loss_force_cleanup/0}.

tracee_and_controller_loss_force_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    exit(Tracee, kill),
    TraceeResult = receive_result(Caller, Ref),
    ?assertEqual(tracee_exited, maps:get(reason, TraceeResult)),
    assert_forced(TraceeResult),
    cleanup(),
    Tracee2 = tracee(),
    Controller = spawn(fun collision/0),
    {Caller2, Ref2} = start_call(Controller, (request(Tracee2))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    exit(Controller, kill),
    ControllerResult = receive_result(Caller2, Ref2),
    ?assertEqual(controller_disconnected, maps:get(reason, ControllerResult)),
    assert_forced(ControllerResult),
    assert_clean(Tracee2),
    Tracee2 ! stop.

helper_crash_and_owner_fallback_test_() ->
    {timeout, 15, fun helper_crash_and_owner_fallback/0}.

helper_crash_and_owner_fallback() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    #{collector := Collector} = wait_helpers(),
    exit(Collector, kill),
    HelperResult = receive_result(Caller, Ref),
    ?assertEqual(capture_internal_error, maps:get(reason, HelperResult)),
    assert_forced(HelperResult),
    assert_clean(Tracee),
    cleanup(),
    {CallerIO, RefIO} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    #{silent_io := SilentIO} = wait_helpers(),
    exit(SilentIO, kill),
    IOResult = receive_result(CallerIO, RefIO),
    ?assertEqual(capture_internal_error, maps:get(reason, IOResult)),
    assert_forced(IOResult),
    assert_clean(Tracee),
    cleanup(),
    {Caller2, Ref2} = start_call(self(), (request(Tracee))#{duration_ms => 5000}),
    wait_registered(recon_trace_tracer),
    exit(whereis(observer_cli_trace_owner), kill),
    OwnerResult = receive_result(Caller2, Ref2),
    ?assert(
        lists:member(maps:get(reason, OwnerResult), [capture_internal_error, cleanup_unconfirmed])
    ),
    assert_clean(Tracee),
    Tracee ! stop.

helper_setup_failure_does_not_clear_test_() ->
    {timeout, 10, fun helper_setup_failure_does_not_clear/0}.

helper_setup_failure_does_not_clear() ->
    cleanup(),
    Tracee = tracee(),
    MFA = {?MODULE, fixture, 0},
    1 = erlang:trace_pattern(MFA, true, []),
    Result = observer_cli_trace:call(
        self(), (request(Tracee))#{test_before_calls => fun() -> error(setup_failed) end}
    ),
    ?assertEqual(helper_setup_failed, maps:get(reason, Result)),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    1 = erlang:trace_pattern(MFA, false, []),
    Tracee ! stop.

matches_zero_cleans_trace_state_test_() ->
    {timeout, 10, fun matches_zero_cleans_trace_state/0}.

matches_zero_cleans_trace_state() ->
    cleanup(),
    Tracee = tracee(),
    {module, observer_cli_trace_fixture} = code:ensure_loaded(observer_cli_trace_fixture),
    BeforeCalls = fun() ->
        true = code:delete(observer_cli_trace_fixture),
        false = code:purge(observer_cli_trace_fixture)
    end,
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{
            mfa => <<"observer_cli_trace_fixture:call/0">>,
            test_before_calls => BeforeCalls
        }
    ),
    ?assertEqual(mfa_not_traceable, maps:get(reason, Result)),
    assert_clean_mfa(Tracee, {observer_cli_trace_fixture, call, 0}),
    Tracee ! stop.

stop_collision_never_kills_unproved_owner_test_() ->
    {timeout, 10, fun stop_collision_never_kills_unproved_owner/0}.

stop_collision_never_kills_unproved_owner() ->
    cleanup(),
    Collision = spawn(fun collision/0),
    true = register(observer_cli_trace_owner, Collision),
    Result = observer_cli_trace:stop_all(),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
    ?assert(is_process_alive(Collision)),
    unregister(observer_cli_trace_owner),
    Collision ! stop.

request(Pid) ->
    #{
        mfa => <<"observer_cli_trace_test:fixture/0">>,
        pid => list_to_binary(pid_to_list(Pid)),
        duration_ms => 1000,
        max => 1,
        replace_existing_trace => true
    }.

tracee() ->
    spawn(fun tracee_loop/0).

tracee_loop() ->
    receive
        {call, Caller, Count} ->
            lists:foreach(fun(_) -> erlang:apply(?MODULE, fixture, []) end, lists:seq(1, Count)),
            Caller ! called,
            tracee_loop();
        {recursive, Caller, Count} ->
            erlang:apply(?MODULE, recursive_fixture, [Count]),
            Caller ! called,
            tracee_loop();
        stop ->
            ok
    end.

collision() ->
    receive
        stop -> ok
    end.

start_call(Controller, Request) ->
    Parent = self(),
    Ref = make_ref(),
    Caller = spawn(fun() -> Parent ! {Ref, self(), observer_cli_trace:call(Controller, Request)} end),
    {Caller, Ref}.

receive_result(Caller, Ref) ->
    receive
        {Ref, Caller, Result} -> Result
    after 7000 ->
        error(trace_result_timeout)
    end.

wait_registered(Name) ->
    wait_until(fun() -> is_pid(whereis(Name)) end).

wait_trace_active(Pid, MFA) ->
    wait_until(fun() ->
        erlang:trace_info(MFA, traced) =:= {traced, global} andalso
            case erlang:trace_info(Pid, flags) of
                {flags, Flags} -> lists:member(call, Flags);
                _ -> false
            end
    end).

wait_helpers() ->
    wait_value(fun observer_cli_trace:test_helpers/0).

wait_until(Fun) ->
    _ = wait_value(fun() ->
        case Fun() of
            true -> ready;
            false -> undefined
        end
    end),
    ok.

wait_value(Fun) ->
    wait_value(Fun, 200).

wait_value(_Fun, 0) ->
    error(wait_timeout);
wait_value(Fun, Attempts) ->
    case Fun() of
        undefined ->
            timer:sleep(10),
            wait_value(Fun, Attempts - 1);
        Value ->
            Value
    end.

assert_forced(Result) ->
    Capture = maps:get(capture, Result),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(truncated, Capture)),
    ?assertEqual(null, maps:get(dropped_count, Capture)).

assert_clean(Pid) ->
    assert_clean_mfa(Pid, {?MODULE, fixture, 0}).

assert_clean_mfa(Pid, MFA) ->
    ?assertEqual(undefined, whereis(observer_cli_trace_owner)),
    ?assertEqual(undefined, whereis(recon_trace_tracer)),
    ?assertEqual(undefined, whereis(recon_trace_formatter)),
    case erlang:trace_info(Pid, flags) of
        undefined -> ok;
        {flags, Flags} -> ?assertEqual(false, lists:member(call, Flags))
    end,
    ?assert(
        lists:member(erlang:trace_info(MFA, traced), [
            undefined, {traced, false}, {traced, undefined}
        ])
    ).

cleanup() ->
    case whereis(observer_cli_trace_owner) of
        undefined ->
            ok;
        Owner ->
            unregister(observer_cli_trace_owner),
            exit(Owner, kill)
    end,
    recon_trace:clear(),
    ok.

-endif.
