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

input_validation_contract_test() ->
    Base = request(self()),
    Dead = spawn(fun() -> ok end),
    Mon = erlang:monitor(process, Dead),
    receive
        {'DOWN', Mon, process, Dead, normal} -> ok
    end,
    Cases = [
        {not_a_pid, Base, invalid_request},
        {self(), not_a_map, invalid_request},
        {self(), maps:remove(mfa, Base), invalid_mfa},
        {self(), Base#{mfa => 1}, invalid_mfa},
        {self(), Base#{mfa => <<"bad">>}, invalid_mfa},
        {self(), Base#{mfa => <<"erlang:node/0/1">>}, invalid_mfa},
        {self(), Base#{mfa => <<"_:node/0">>}, invalid_mfa},
        {self(), Base#{mfa => <<"erlang:node/256">>}, invalid_mfa},
        {self(), maps:remove(pid, Base), trace_pid_required},
        {self(), Base#{pid => <<"not-a-pid">>}, invalid_trace_pid},
        {self(), Base#{pid => list_to_binary(pid_to_list(Dead))}, invalid_trace_pid},
        {self(), Base#{pid => binary:copy(<<"x">>, 129)}, invalid_trace_pid},
        {self(), Base#{duration_ms => 99}, invalid_trace_bounds},
        {self(), Base#{duration_ms => 60001}, invalid_trace_bounds},
        {self(), Base#{duration_ms => invalid}, invalid_trace_bounds},
        {self(), Base#{max => 0}, invalid_trace_bounds},
        {self(), Base#{max => 1001}, invalid_trace_bounds},
        {self(), Base#{max => {0, 1000}}, invalid_trace_bounds},
        {self(), Base#{max => {201, 1000}}, invalid_trace_bounds},
        {self(), Base#{max => invalid}, invalid_trace_bounds}
    ],
    lists:foreach(
        fun({Controller, Request, Reason}) ->
            ?assertEqual(Reason, maps:get(reason, observer_cli_trace:call(Controller, Request)))
        end,
        Cases
    ).

trace_protocol_helper_contract_test() ->
    ?assertEqual(
        {forced, controller, controller_disconnected},
        observer_cli_trace:forced_reason(controller_disconnected)
    ),
    ?assertEqual(
        {forced, internal, capture_internal_error}, observer_cli_trace:forced_reason(unexpected)
    ),
    Events = [#{mfa => <<"erlang:node/0">>}],
    ?assertEqual(Events, observer_cli_trace:outcome_events({natural, stopped, Events, false})),
    ?assertEqual([], observer_cli_trace:outcome_events({forced, success, stopped})),
    ?assertEqual(null, observer_cli_trace:module_md5(observer_cli_missing_module)),
    lists:foreach(
        fun(Request) -> ?assertEqual(ok, observer_cli_trace:io_reply(Request)) end,
        [
            {put_chars, <<"text">>},
            {put_chars, unicode, <<"text">>},
            {put_chars, io_lib, format, ["~s", ["text"]]},
            {put_chars, unicode, io_lib, format, ["~s", ["text"]]},
            {requests, [{put_chars, <<"one">>}, {put_chars, <<"two">>}]}
        ]
    ),
    ?assertEqual(
        {error, request}, observer_cli_trace:io_reply({put_chars, missing, function, []})
    ),
    ?assertEqual(
        {error, request},
        observer_cli_trace:io_reply({requests, [{put_chars, missing, function, []}]})
    ),
    ?assertEqual({error, enotsup}, observer_cli_trace:io_reply({get_geometry, rows})),
    ?assertEqual({error, enotsup}, observer_cli_trace:io_reply(unsupported)),
    ?assertEqual(
        error,
        observer_cli_trace:parse_pid("<0.999999999999999999999999999999999999999.0>")
    ).

trace_cleanup_helper_contract_test_() ->
    {timeout, 15, fun trace_cleanup_helper_contract/0}.

trace_cleanup_helper_contract() ->
    ?assertEqual({ok, self()}, observer_cli_trace:parse_pid(pid_to_list(self()))),
    ?assertEqual({ok, self()}, observer_cli_trace:parse_pid(list_to_binary(pid_to_list(self())))),
    ?assertEqual(error, observer_cli_trace:parse_pid(invalid)),
    Session = make_ref(),
    Collector = spawn(fun() -> formatter_collector(self()) end),
    ?assertEqual(
        [],
        observer_cli_trace:format_event(
            {trace_ts, self(), call, {erlang, node, 0}, os:timestamp()},
            Collector,
            Session,
            os:timestamp()
        )
    ),
    ?assertEqual([], observer_cli_trace:format_event(invalid, Collector, Session, os:timestamp())),
    Collector ! stop,
    DrainCollector = spawn(fun final_collector/0),
    ?assertEqual(
        {natural, limit_reached, [event], false},
        observer_cli_trace:final_drain(#{
            collector => DrainCollector,
            collector_mon => erlang:monitor(process, DrainCollector),
            max => 1
        })
    ),
    RateCollector = spawn(fun final_collector/0),
    ?assertEqual(
        {natural, rate_exceeded, [event], false},
        observer_cli_trace:wait_formatter(
            #{
                collector => RateCollector,
                collector_mon => erlang:monitor(process, RateCollector),
                max => {1, 1000},
                ref => make_ref()
            },
            #{formatter => undefined, formatter_mon => undefined},
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    helper_shutdown_contract(),
    owner_result_contract(),
    ?assertEqual(
        ok,
        observer_cli_trace:verify_cleanup(#{
            pid => self(), mfa => {?MODULE, fixture, 0}
        })
    ),
    ?assertEqual(true, observer_cli_trace:trace_pattern_off({?MODULE, fixture, 0})),
    ?assertEqual(true, observer_cli_trace:trace_pattern_off({missing_module, missing, 0})),
    1 = erlang:trace_pattern({?MODULE, fixture, 0}, true, []),
    ?assertEqual(false, observer_cli_trace:trace_pattern_off({?MODULE, fixture, 0})),
    1 = erlang:trace(self(), true, [call]),
    ?assertEqual(
        {error, cleanup_unconfirmed},
        observer_cli_trace:verify_cleanup(#{
            pid => self(), mfa => {?MODULE, fixture, 0}
        })
    ),
    1 = erlang:trace(self(), false, [call]),
    1 = erlang:trace_pattern({?MODULE, fixture, 0}, false, []),
    ?assertEqual(ok, observer_cli_trace:wait_fixed_names()),
    ?assertEqual(ok, observer_cli_trace:stop_helper(not_a_pid)),
    Normal = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(ok, observer_cli_trace:stop_helper(Normal)),
    Stubborn = spawn(fun stubborn_helper/0),
    ?assertEqual(ok, observer_cli_trace:stop_helper(Stubborn)),
    wait_formatter_contract(),
    drain_failure_contract(),
    FixedName = spawn(fun stubborn_helper/0),
    true = register(recon_trace_formatter, FixedName),
    spawn(fun() ->
        timer:sleep(20),
        exit(FixedName, kill)
    end),
    ?assertEqual(ok, observer_cli_trace:wait_fixed_names()),
    trace_owner_protocol_contract().

trace_owner_protocol_contract() ->
    ?assertEqual(
        {forced, internal, capture_internal_error},
        run_wait_trace_case(force_stop)
    ),
    ?assertMatch(
        {forced, success, stopped, {_Stopper, _StopRef}},
        run_wait_trace_case(stop_request)
    ),
    ?assertEqual({forced, success, duration_elapsed}, run_wait_trace_case(timeout)),
    ?assertEqual(
        cleanup_unconfirmed,
        maps:get(reason, observer_cli_trace:fallback_cleanup(#{}, [], false))
    ),
    Owner = spawn(fun stubborn_helper/0),
    OwnerMon = erlang:monitor(process, Owner),
    exit(Owner, kill),
    ?assertEqual(
        result, observer_cli_trace:await_owner_down(Owner, OwnerMon, make_ref(), result)
    ),
    TimeoutOwner = spawn(fun stubborn_helper/0),
    TimeoutMon = erlang:monitor(process, TimeoutOwner),
    exit(TimeoutOwner, kill),
    TimeoutResult = observer_cli_trace:await_timeout_cleanup(
        TimeoutOwner, TimeoutMon, make_ref(), #{}, [], false
    ),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, TimeoutResult)),
    Warning = #{code => warning},
    Response = #{status => ok},
    Cooperative = spawn(fun() ->
        receive
            {stop_request, Stopper, RequestRef} ->
                Stopper ! {RequestRef, armed, self()},
                Stopper ! {RequestRef, cleanup_ack, Response}
        end
    end),
    ?assertEqual(Response, observer_cli_trace:stop_owner(Cooperative, Warning)),
    Dying = spawn(fun() ->
        receive
            _ -> exit(failed)
        end
    end),
    ?assertEqual(
        cleanup_unconfirmed, maps:get(reason, observer_cli_trace:stop_owner(Dying, Warning))
    ),
    ?assertEqual(
        Response,
        observer_cli_trace:await_stop_ack(
            self(), make_ref(), make_ref(), Warning, Response, true
        )
    ),
    ControllerMon = make_ref(),
    DispatcherMon = make_ref(),
    TraceeMon = make_ref(),
    MonitorState = #{
        controller_mon => ControllerMon,
        dispatcher_mon => DispatcherMon,
        tracee_mon => TraceeMon
    },
    ?assertEqual(
        {forced, controller, controller_disconnected},
        observer_cli_trace:monitor_failure(MonitorState, ControllerMon, failed)
    ),
    ?assertEqual(
        {forced, internal, dispatcher_disconnected},
        observer_cli_trace:monitor_failure(MonitorState, DispatcherMon, failed)
    ),
    ?assertEqual(
        {forced, safety_refusal, tracee_exited},
        observer_cli_trace:monitor_failure(MonitorState, TraceeMon, failed)
    ),
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:monitor_failure(MonitorState, make_ref(), failed)
    ),
    Collision = spawn(fun stubborn_helper/0),
    true = register(observer_cli_trace_owner, Collision),
    DeadOwner = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    DeadOwnerMon = erlang:monitor(process, DeadOwner),
    DeadOwner ! stop,
    ?assertEqual(
        cleanup_unconfirmed,
        maps:get(
            reason,
            observer_cli_trace:await_owner_down(
                DeadOwner, DeadOwnerMon, make_ref(), result
            )
        )
    ),
    exit(Collision, kill),
    ResultOwner = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ResultOwnerMon = erlang:monitor(process, ResultOwner),
    ResultRef = make_ref(),
    self() ! {ResultRef, result, result},
    spawn(fun() ->
        timer:sleep(10),
        ResultOwner ! stop
    end),
    ?assertEqual(
        result,
        observer_cli_trace:await_timeout_cleanup(
            ResultOwner, ResultOwnerMon, ResultRef, #{}, [], false
        )
    ),
    ?assertEqual({error, cleanup_unconfirmed}, observer_cli_trace:wait_fixed_names(0)),
    Silent = spawn(fun observer_cli_trace:silent_io/0),
    Silent ! unknown,
    Silent ! stop,
    SilentMon = erlang:monitor(process, Silent),
    receive
        {'DOWN', SilentMon, process, Silent, normal} -> ok
    end,
    ?assertEqual(
        cleanup_unconfirmed,
        observer_cli_trace:await_killed_helper(self(), make_ref())
    ).

run_wait_trace_case(Kind) ->
    Parent = self(),
    Pid = spawn(fun() ->
        Ref = make_ref(),
        State = #{
            ref => Ref,
            duration_ms => 100,
            controller_mon => make_ref(),
            dispatcher_mon => make_ref(),
            tracee_mon => make_ref()
        },
        Recon = #{tracer => self(), tracer_mon => make_ref()},
        Parent ! {wait_trace_ready, self(), Ref},
        Deadline =
            case Kind of
                timeout -> erlang:monotonic_time(millisecond);
                _ -> erlang:monotonic_time(millisecond) + 1000
            end,
        Parent ! {wait_trace_result, self(), observer_cli_trace:wait_trace(State, Recon, Deadline)}
    end),
    ReadyRef =
        receive
            {wait_trace_ready, Pid, Ref} ->
                case Kind of
                    force_stop -> Pid ! {force_stop, Ref, dispatcher_timeout};
                    stop_request -> Pid ! {stop_request, Parent, Ref};
                    timeout -> ok
                end,
                Ref
        end,
    receive
        {ReadyRef, armed, Pid} -> ok
    after 0 ->
        ok
    end,
    receive
        {wait_trace_result, Pid, Result} -> Result
    end.

helper_shutdown_contract() ->
    Dead = spawn(fun() -> ok end),
    DeadMon = erlang:monitor(process, Dead),
    receive
        {'DOWN', DeadMon, process, Dead, normal} -> ok
    end,
    ?assertEqual(helper_failed, observer_cli_trace:stop_helper_checked(Dead, DeadMon)),
    Normal = spawn(fun checked_helper/0),
    NormalMon = erlang:monitor(process, Normal),
    ?assertEqual(ok, observer_cli_trace:stop_helper_checked(Normal, NormalMon)),
    Abnormal = spawn(fun() ->
        receive
            {stop, Owner, Ref} ->
                Owner ! {Ref, stopping},
                exit(abnormal)
        end
    end),
    AbnormalMon = erlang:monitor(process, Abnormal),
    ?assertEqual(helper_failed, observer_cli_trace:stop_helper_checked(Abnormal, AbnormalMon)),
    First = spawn(fun checked_helper/0),
    Second = spawn(fun checked_helper/0),
    State = #{
        collector => First,
        collector_mon => erlang:monitor(process, First),
        silent_io => Second,
        silent_mon => erlang:monitor(process, Second)
    },
    ?assertEqual(
        {{forced, success, stopped}, ok},
        observer_cli_trace:checked_helper_shutdown(State, {forced, success, stopped}, ok)
    ),
    Stubborn = spawn(fun stubborn_helper/0),
    StubbornMon = erlang:monitor(process, Stubborn),
    ?assertEqual(
        cleanup_unconfirmed,
        observer_cli_trace:stop_helper_checked(Stubborn, StubbornMon)
    ),
    VerificationA = spawn(fun checked_helper/0),
    VerificationB = spawn(fun checked_helper/0),
    VerificationState = #{
        collector => VerificationA,
        collector_mon => erlang:monitor(process, VerificationA),
        silent_io => VerificationB,
        silent_mon => erlang:monitor(process, VerificationB)
    },
    ?assertMatch(
        {_, {error, cleanup_unconfirmed}},
        observer_cli_trace:checked_helper_shutdown(
            VerificationState, outcome, {error, cleanup_unconfirmed}
        )
    ),
    DeadCollector = spawn(fun() -> ok end),
    DeadCollectorMon = erlang:monitor(process, DeadCollector),
    receive
        {'DOWN', DeadCollectorMon, process, DeadCollector, normal} -> ok
    end,
    LiveSilent = spawn(fun checked_helper/0),
    ?assertEqual(
        {{forced, internal, capture_internal_error}, ok},
        observer_cli_trace:checked_helper_shutdown(
            #{
                collector => DeadCollector,
                collector_mon => DeadCollectorMon,
                silent_io => LiveSilent,
                silent_mon => erlang:monitor(process, LiveSilent)
            },
            outcome,
            ok
        )
    ),
    LiveCollector = spawn(fun checked_helper/0),
    DeadSilent = spawn(fun() -> ok end),
    DeadSilentMon = erlang:monitor(process, DeadSilent),
    receive
        {'DOWN', DeadSilentMon, process, DeadSilent, normal} -> ok
    end,
    ?assertEqual(
        {{forced, internal, capture_internal_error}, ok},
        observer_cli_trace:checked_helper_shutdown(
            #{
                collector => LiveCollector,
                collector_mon => erlang:monitor(process, LiveCollector),
                silent_io => DeadSilent,
                silent_mon => DeadSilentMon
            },
            outcome,
            ok
        )
    ),
    Awaited = spawn(fun stubborn_helper/0),
    AwaitedMon = erlang:monitor(process, Awaited),
    ?assertEqual(
        cleanup_unconfirmed,
        observer_cli_trace:await_helper_down(Awaited, AwaitedMon)
    ),
    NoAck = spawn(fun() ->
        receive
            {stop, _, _} -> exit(failed)
        end
    end),
    NoAckMon = erlang:monitor(process, NoAck),
    ?assertEqual(helper_failed, observer_cli_trace:stop_helper_checked(NoAck, NoAckMon)),
    StubbornCollector = spawn(fun stubborn_helper/0),
    NormalSilent = spawn(fun checked_helper/0),
    ?assertMatch(
        {_, {error, cleanup_unconfirmed}},
        observer_cli_trace:checked_helper_shutdown(
            #{
                collector => StubbornCollector,
                collector_mon => erlang:monitor(process, StubbornCollector),
                silent_io => NormalSilent,
                silent_mon => erlang:monitor(process, NormalSilent)
            },
            outcome,
            ok
        )
    ),
    NormalCollector = spawn(fun checked_helper/0),
    StubbornSilent = spawn(fun stubborn_helper/0),
    ?assertMatch(
        {_, {error, cleanup_unconfirmed}},
        observer_cli_trace:checked_helper_shutdown(
            #{
                collector => NormalCollector,
                collector_mon => erlang:monitor(process, NormalCollector),
                silent_io => StubbornSilent,
                silent_mon => erlang:monitor(process, StubbornSilent)
            },
            outcome,
            ok
        )
    ).

wait_formatter_contract() ->
    Base = #{
        max => 1,
        ref => make_ref(),
        controller_mon => make_ref(),
        dispatcher_mon => make_ref(),
        tracee_mon => make_ref()
    },
    NormalCollector = spawn(fun final_collector/0),
    Normal = spawn(fun() -> ok end),
    NormalMon = erlang:monitor(process, Normal),
    ?assertMatch(
        {natural, limit_reached, _, _},
        observer_cli_trace:wait_formatter(
            Base#{
                collector => NormalCollector,
                collector_mon => erlang:monitor(process, NormalCollector)
            },
            #{formatter => Normal, formatter_mon => NormalMon},
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    Abnormal = spawn(fun() -> exit(abnormal) end),
    AbnormalMon = erlang:monitor(process, Abnormal),
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:wait_formatter(
            Base,
            #{formatter => Abnormal, formatter_mon => AbnormalMon},
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    StopRef = make_ref(),
    self() ! {stop_request, self(), StopRef},
    ?assertEqual(
        {forced, success, stopped, {self(), StopRef}},
        observer_cli_trace:wait_formatter(
            Base,
            #{formatter => self(), formatter_mon => make_ref()},
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    receive
        {StopRef, armed, _} -> ok
    end,
    StateRef = maps:get(ref, Base),
    self() ! {force_stop, StateRef, controller_disconnected},
    ?assertEqual(
        {forced, controller, controller_disconnected},
        observer_cli_trace:wait_formatter(
            Base,
            #{formatter => self(), formatter_mon => make_ref()},
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    ?assertEqual(
        {forced, success, duration_elapsed},
        observer_cli_trace:wait_formatter(
            Base,
            #{formatter => self(), formatter_mon => make_ref()},
            erlang:monotonic_time(millisecond)
        )
    ),
    UnknownMon = make_ref(),
    self() ! {'DOWN', UnknownMon, process, self(), failed},
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:wait_formatter(
            Base,
            #{formatter => self(), formatter_mon => make_ref()},
            erlang:monotonic_time(millisecond) + 1000
        )
    ).

drain_failure_contract() ->
    Dead = spawn(fun() -> ok end),
    DeadMon = erlang:monitor(process, Dead),
    receive
        {'DOWN', DeadMon, process, Dead, normal} = Down -> self() ! Down
    end,
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:final_drain(#{collector => Dead, collector_mon => DeadMon, max => 1})
    ),
    Silent = spawn(fun stubborn_helper/0),
    SilentMon = erlang:monitor(process, Silent),
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:final_drain(#{collector => Silent, collector_mon => SilentMon, max => 1})
    ),
    exit(Silent, kill),
    DownCollector = spawn(fun() -> ok end),
    DownMon = erlang:monitor(process, DownCollector),
    receive
        {'DOWN', DownMon, process, DownCollector, normal} = Down2 -> self() ! Down2
    end,
    ?assertException(
        error,
        collector_down,
        observer_cli_trace:format_event(
            {trace_ts, self(), call, {erlang, node, 0}, os:timestamp()},
            DownCollector,
            make_ref(),
            os:timestamp()
        )
    ),
    NoAck = spawn(fun stubborn_helper/0),
    ?assertException(
        error,
        collector_ack_timeout,
        observer_cli_trace:format_event(
            {trace_ts, self(), call, {erlang, node, 0}, os:timestamp()},
            NoAck,
            make_ref(),
            os:timestamp()
        )
    ),
    exit(NoAck, kill).

owner_result_contract() ->
    Md5 = observer_cli_trace:module_md5(?MODULE),
    State = #{
        mfa => {?MODULE, fixture, 0}, module_md5 => Md5, test_end_module_md5 => Md5
    },
    Complete = observer_cli_trace:owner_result(
        State, {natural, limit_reached, [event], false}, ok
    ),
    ?assertEqual(ok, maps:get(status, Complete)),
    ?assertEqual(true, maps:get(trace_complete, maps:get(capture, Complete))),
    Partial = observer_cli_trace:owner_result(
        State#{test_end_module_md5 := changed}, {natural, limit_reached, [], true}, ok
    ),
    ?assertEqual(false, maps:get(trace_complete, maps:get(capture, Partial))),
    Forced = observer_cli_trace:owner_result(State, {forced, success, stopped}, ok),
    ?assertEqual(ok, maps:get(status, Forced)),
    ?assertEqual(
        error,
        maps:get(
            status,
            observer_cli_trace:owner_result(
                State, {error, internal, capture_internal_error}, ok
            )
        )
    ),
    Cleanup = observer_cli_trace:owner_result(
        State, {natural, limit_reached, [event], false}, {error, cleanup_unconfirmed}
    ),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Cleanup)).

formatter_collector(_Parent) ->
    receive
        {event, Formatter, Session, Ref, _Event} ->
            Formatter ! {Session, Ref, ack},
            formatter_collector(undefined);
        stop ->
            ok
    end.

final_collector() ->
    receive
        {final, Owner, Ref} ->
            Owner ! {Ref, [event], false},
            final_collector();
        stop ->
            ok
    end.

checked_helper() ->
    receive
        {stop, Owner, Ref} ->
            Owner ! {Ref, stopping},
            ok
    end.

stubborn_helper() ->
    receive
        _ -> stubborn_helper()
    end.

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

preloaded_erlang_mfa_trace_test_() ->
    {timeout, 10, fun preloaded_erlang_mfa_trace/0}.

preloaded_erlang_mfa_trace() ->
    cleanup(),
    Parent = self(),
    Tracee = spawn(fun() ->
        receive
            call ->
                Parent ! {node_result, node()},
                receive
                    stop -> ok
                end
        end
    end),
    Request = (request(Tracee))#{mfa => <<"erlang:node/0">>, max => 1, duration_ms => 1000},
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {erlang, node, 0}),
    Tracee ! call,
    receive
        {node_result, Node} -> ?assertEqual(node(), Node)
    after 1000 ->
        erlang:error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(ok, maps:get(status, Result)),
    ?assert(is_list(maps:get(events, maps:get(capture, Result)))),
    assert_clean(Tracee),
    Tracee ! stop.

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
