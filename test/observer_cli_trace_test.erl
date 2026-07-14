-module(observer_cli_trace_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-export([fixture/0, other_fixture/0, recursive_fixture/1]).

-define(RELOAD_FIXTURE, observer_cli_trace_reload_fixture).

trace_runtime_failure_paths_test() ->
    cleanup(),
    try
        ?assertEqual(undefined, observer_cli_trace:test_helpers()),
        Owner = spawn(fun stubborn_helper/0),
        OwnerMon = erlang:monitor(process, Owner),
        Ref = make_ref(),
        self() ! {Ref, ready, #{}},
        exit(Owner, kill),
        ?assertEqual(
            result,
            observer_cli_trace:await_owner_down(Owner, OwnerMon, Ref, result)
        ),

        Tracer = spawn(fun() ->
            receive
                crash -> exit(failed)
            end
        end),
        TracerMon = erlang:monitor(process, Tracer),
        Tracer ! crash,
        ?assertEqual(
            {forced, internal, capture_internal_error},
            observer_cli_trace:wait_trace(
                #{ref => make_ref()},
                #{tracer => Tracer, tracer_mon => TracerMon},
                erlang:monotonic_time(millisecond) + 1000
            )
        ),

        BlockingOwner = spawn(fun stubborn_helper/0),
        true = register(observer_cli_trace_owner, BlockingOwner),
        spawn(fun() ->
            timer:sleep(20),
            exit(BlockingOwner, kill)
        end),
        ?assert(observer_cli_trace:wait_cleanup(500))
    after
        cleanup()
    end.

remote_trace_pid_is_rejected_test_() ->
    {timeout, 10, fun() ->
        with_distribution(fun() ->
            {ok, Peer, Node} = peer:start_link(#{
                name => peer:random_name("observer_cli_trace_pid")
            }),
            try
                RemotePid = erpc:call(Node, erlang, whereis, [init]),
                ?assertEqual(error, observer_cli_trace:parse_pid(pid_to_list(RemotePid))),
                ?assertEqual(ok, observer_cli:update_net_ticktime_from(Node))
            after
                peer:stop(Peer)
            end
        end)
    end}.

trace_protocol_timeouts_are_bounded_test_() ->
    {timeout, 10, fun trace_protocol_timeouts_are_bounded/0}.

fixed_name_collision_blocks_cleanup_confirmation_test_() ->
    {timeout, 5, fun fixed_name_collision_blocks_cleanup_confirmation/0}.

formatter_only_attachment_completes_test_() ->
    {timeout, 10, fun formatter_only_attachment_completes/0}.

post_setup_port_collision_is_rejected_test_() ->
    {timeout, 10, fun post_setup_port_collision_is_rejected/0}.

pre_attach_collector_crash_is_reported_test_() ->
    {timeout, 10, fun pre_attach_collector_crash_is_reported/0}.

trace_protocol_timeouts_are_bounded() ->
    cleanup(),
    try
        Owner = spawn(fun stubborn_helper/0),
        OwnerMon = erlang:monitor(process, Owner),
        OwnerResult = observer_cli_trace:await_owner_down(
            Owner, OwnerMon, make_ref(), result
        ),
        ?assertEqual(cleanup_unconfirmed, maps:get(reason, OwnerResult)),
        exit(Owner, kill),
        await_down(Owner, OwnerMon),

        AckOwner = spawn(fun stubborn_helper/0),
        AckOwnerMon = erlang:monitor(process, AckOwner),
        AckResult = observer_cli_trace:await_stop_ack(
            AckOwner, AckOwnerMon, make_ref(), #{code => warning}, undefined, false
        ),
        ?assertEqual(cleanup_unconfirmed, maps:get(reason, AckResult)),
        exit(AckOwner, kill),
        await_down(AckOwner, AckOwnerMon)
    after
        cleanup()
    end.

fixed_name_collision_blocks_cleanup_confirmation() ->
    cleanup(),
    Collision = spawn(fun stubborn_helper/0),
    CollisionMon = erlang:monitor(process, Collision),
    true = register(recon_trace_formatter, Collision),
    try
        ?assertEqual(
            {error, cleanup_unconfirmed},
            observer_cli_trace:verify_cleanup(#{})
        )
    after
        exit(Collision, kill),
        await_down(Collision, CollisionMon),
        cleanup()
    end.

formatter_only_attachment_completes() ->
    cleanup(),
    Tracee = tracee(),
    Coordinator = spawn(fun formatter_suspend_resume/0),
    try
        AfterCalls = fun() ->
            Formatter = whereis(recon_trace_formatter),
            Coordinator ! {suspend, Formatter, self()},
            receive
                formatter_suspended -> ok
            after 1000 ->
                error(formatter_suspend_timeout)
            end,
            Tracee ! {call, self(), 1},
            receive
                called -> ok
            after 1000 ->
                error(tracee_timeout)
            end,
            wait_until(fun() -> whereis(recon_trace_tracer) =:= undefined end),
            Coordinator ! resume_after_attach,
            ok
        end,
        Result = observer_cli_trace:call(
            self(),
            (request(Tracee))#{max => 1, duration_ms => 3000, test_after_calls => AfterCalls}
        ),
        ?assertEqual(ok, maps:get(status, Result)),
        ?assertEqual(limit_reached, maps:get(reason, Result)),
        assert_clean(Tracee)
    after
        Coordinator ! stop,
        Tracee ! stop,
        cleanup()
    end.

formatter_suspend_resume() ->
    receive
        {suspend, Formatter, Owner} ->
            true = erlang:suspend_process(Formatter),
            Owner ! formatter_suspended,
            receive
                resume_after_attach ->
                    timer:sleep(100),
                    true = erlang:resume_process(Formatter);
                stop ->
                    _ = resume_formatter(Formatter)
            end;
        stop ->
            ok
    end.

resume_formatter(Formatter) ->
    try erlang:resume_process(Formatter) of
        Result -> Result
    catch
        error:badarg -> false
    end.

post_setup_port_collision_is_rejected() ->
    cleanup(),
    Tracee = tracee(),
    try
        AfterCalls = fun() ->
            ok = recon_trace:clear(),
            TracerPort = open_port({spawn, "cat"}, []),
            true = register(recon_trace_tracer, TracerPort),
            FormatterPort = open_port({spawn, "cat"}, []),
            true = register(recon_trace_formatter, FormatterPort)
        end,
        Result = observer_cli_trace:call(
            self(), (request(Tracee))#{test_after_calls => AfterCalls}
        ),
        ?assertEqual(capture_internal_error, maps:get(reason, Result)),
        ?assertEqual(undefined, whereis(recon_trace_tracer)),
        ?assertEqual(undefined, whereis(recon_trace_formatter)),
        assert_clean(Tracee)
    after
        Tracee ! stop,
        cleanup()
    end.

pre_attach_collector_crash_is_reported() ->
    cleanup(),
    Tracee = tracee(),
    try
        AfterCalls = fun() ->
            #{collector := Collector} = get(observer_cli_trace_helpers),
            exit(Collector, kill),
            ok = recon_trace:clear(),
            wait_until(fun() ->
                not is_process_alive(Collector) andalso
                    whereis(recon_trace_tracer) =:= undefined andalso
                    whereis(recon_trace_formatter) =:= undefined
            end)
        end,
        Result = observer_cli_trace:call(
            self(), (request(Tracee))#{test_after_calls => AfterCalls}
        ),
        ?assertEqual(capture_internal_error, maps:get(reason, Result)),
        assert_forced(Result),
        assert_clean(Tracee)
    after
        Tracee ! stop,
        cleanup()
    end.

with_distribution(Fun) ->
    WasAlive = erlang:is_alive(),
    case WasAlive of
        true ->
            Fun();
        false ->
            Name = list_to_atom(peer:random_name("observer_cli_trace_origin")),
            {ok, _} = net_kernel:start([Name, shortnames]),
            try
                Fun()
            after
                net_kernel:stop()
            end
    end.

fixture() ->
    ok.

other_fixture() ->
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
    ?assertEqual(
        Events,
        observer_cli_trace:outcome_events({natural, stopped, Events, false, 0, 0})
    ),
    ?assertEqual([], observer_cli_trace:outcome_events({forced, success, stopped, [], 0})),
    ?assertEqual(
        Events,
        observer_cli_trace:outcome_events(
            {forced, success, stopped, Events, 0, {self(), make_ref()}}
        )
    ),
    ?assertEqual([], observer_cli_trace:outcome_events(invalid)),
    ?assertEqual(
        {Events, 1},
        observer_cli_trace:outcome_payload(
            {forced, success, stopped, Events, 1, {self(), make_ref()}}
        )
    ),
    State = #{mfa => {?MODULE, fixture, 0}, pid => self()},
    Finalized = observer_cli_trace:finalize_failure_result(State, Events, 1, ok),
    ?assertEqual(capture_internal_error, maps:get(reason, Finalized)),
    ?assertEqual(
        maps:get(reason, Finalized), maps:get(reason, maps:get(capture, Finalized))
    ),
    Unconfirmed = observer_cli_trace:finalize_failure_result(
        State, Events, 1, {error, cleanup_unconfirmed}
    ),
    UnconfirmedCapture = maps:get(capture, Unconfirmed),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Unconfirmed)),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, UnconfirmedCapture)),
    ?assertEqual(false, maps:get(cleanup_confirmed, UnconfirmedCapture)),
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

owner_init_timeout_is_bounded_test_() ->
    {timeout, 5, fun owner_init_timeout_is_bounded/0}.

owner_init_timeout_is_bounded() ->
    {Owner, Mon} = spawn_monitor(fun observer_cli_trace:owner_init/0),
    receive
        {'DOWN', Mon, process, Owner, normal} -> ok
    after 2000 ->
        error(owner_init_timeout)
    end.

monitor_attachment_noproc_contract_test() ->
    ?assertEqual(
        {natural, limit_reached, [event], false, 0, 0},
        monitor_attachment_outcome(1, [event], 1, unknown)
    ),
    ?assertEqual(
        {natural, rate_exceeded, [event, event], false, 0, 0},
        monitor_attachment_outcome({1, 1000}, [event, event], 2, normal)
    ),
    ?assertEqual(
        {forced, internal, capture_internal_error, [event], 0},
        monitor_attachment_outcome(2, [event], 1, unknown)
    ).

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
            self(),
            {erlang, node, 0},
            Collector,
            Session,
            os:timestamp()
        )
    ),
    ?assertEqual(
        [],
        observer_cli_trace:format_event(
            invalid, self(), {erlang, node, 0}, Collector, Session, os:timestamp()
        )
    ),
    Collector ! stop,
    DrainCollector = spawn(fun final_collector/0),
    ?assertEqual(
        {natural, limit_reached, [event], false, 0, 0},
        observer_cli_trace:final_drain(#{
            collector => DrainCollector,
            collector_mon => erlang:monitor(process, DrainCollector),
            max => 1
        })
    ),
    RateCollector = spawn(fun() -> final_collector([event, event], 2, 0, false) end),
    ?assertEqual(
        {natural, rate_exceeded, [event, event], false, 0, 0},
        observer_cli_trace:wait_formatter(
            #{
                collector => RateCollector,
                collector_mon => erlang:monitor(process, RateCollector),
                max => {1, 1000},
                ref => make_ref()
            },
            #{formatter => undefined, formatter_mon => undefined, tracer_exit_reason => normal},
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
    case Kind of
        stop_request ->
            receive
                {ReadyRef, armed, Pid} -> ok
            after 1000 ->
                error(stop_arm_timeout)
            end;
        _ ->
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
        {{forced, internal, capture_internal_error, [], 0}, ok},
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
        {{forced, internal, capture_internal_error, [event], 1}, ok},
        observer_cli_trace:checked_helper_shutdown(
            #{
                collector => LiveCollector,
                collector_mon => erlang:monitor(process, LiveCollector),
                silent_io => DeadSilent,
                silent_mon => DeadSilentMon
            },
            {natural, limit_reached, [event], false, 0, 1},
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
        {natural, limit_reached, _, _, _, _},
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
    FailedStopCollector = spawn(fun() -> ok end),
    FailedStopMon = erlang:monitor(process, FailedStopCollector),
    receive
        {'DOWN', FailedStopMon, process, FailedStopCollector, normal} = FailedStopDown ->
            self() ! FailedStopDown
    end,
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:drain_forced(
            #{collector => FailedStopCollector, collector_mon => FailedStopMon},
            {forced, success, stopped, {self(), make_ref()}}
        )
    ),
    Silent = spawn(fun stubborn_helper/0),
    SilentMon = erlang:monitor(process, Silent),
    ?assertEqual(
        {forced, internal, capture_internal_error},
        observer_cli_trace:final_drain(#{collector => Silent, collector_mon => SilentMon, max => 1})
    ),
    exit(Silent, kill),
    await_down(Silent, SilentMon),
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
            self(),
            {erlang, node, 0},
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
            self(),
            {erlang, node, 0},
            NoAck,
            make_ref(),
            os:timestamp()
        )
    ),
    exit(NoAck, kill).

owner_result_contract() ->
    Md5 = observer_cli_trace:module_md5(?MODULE),
    State = #{
        mfa => {?MODULE, fixture, 0},
        pid => self(),
        module_md5 => Md5,
        test_end_module_md5 => Md5
    },
    Complete = observer_cli_trace:owner_result(
        State, {natural, limit_reached, [event], false, 0, 0}, ok
    ),
    ?assertEqual(ok, maps:get(status, Complete)),
    ?assertEqual(true, maps:get(trace_complete, maps:get(capture, Complete))),
    Partial = observer_cli_trace:owner_result(
        State#{test_end_module_md5 := changed}, {natural, limit_reached, [], true, 1, 0}, ok
    ),
    ?assertEqual(false, maps:get(trace_complete, maps:get(capture, Partial))),
    Forced = observer_cli_trace:owner_result(State, {forced, success, stopped, [], 0}, ok),
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
        State,
        {natural, limit_reached, [event], false, 0, 1},
        {error, cleanup_unconfirmed}
    ),
    CleanupCapture = maps:get(capture, Cleanup),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Cleanup)),
    ?assertEqual([event], maps:get(events, CleanupCapture)),
    ?assertEqual(true, maps:get(interference_detected, CleanupCapture)).

formatter_collector(_Parent) ->
    receive
        {event, Formatter, Session, Ref, _Event} ->
            Formatter ! {Session, Ref, ack},
            formatter_collector(undefined);
        {interference, Formatter, Session, Ref} ->
            Formatter ! {Session, Ref, ack},
            formatter_collector(undefined);
        stop ->
            ok
    end.

final_collector() ->
    final_collector([event], 1, 0, false).

final_collector(Events, Count, Rejected, Truncated) ->
    receive
        {final, Owner, Ref} ->
            Owner ! {Ref, Events, Count, Rejected, Truncated},
            final_collector(Events, Count, Rejected, Truncated);
        stop ->
            ok
    end.

monitor_attachment_outcome(Max, Events, Count, ExitReason) ->
    Tracer = dead_process(),
    Formatter = dead_process(),
    TracerMon = erlang:monitor(process, Tracer),
    FormatterMon = erlang:monitor(process, Formatter),
    Collector = spawn(fun() -> final_collector(Events, Count, 0, false) end),
    CollectorMon = erlang:monitor(process, Collector),
    State = #{
        collector => Collector,
        collector_mon => CollectorMon,
        max => Max,
        ref => make_ref(),
        controller_mon => make_ref(),
        dispatcher_mon => make_ref(),
        tracee_mon => make_ref()
    },
    Recon = #{
        tracer => Tracer,
        tracer_mon => TracerMon,
        formatter => Formatter,
        formatter_mon => FormatterMon
    },
    case ExitReason of
        unknown -> ok;
        _ -> self() ! {'EXIT', Tracer, ExitReason}
    end,
    try
        observer_cli_trace:wait_trace(
            State, Recon, erlang:monotonic_time(millisecond) + 1000
        )
    after
        Collector ! stop,
        receive
            {'DOWN', CollectorMon, process, Collector, _Reason} -> ok
        after 1000 ->
            error(collector_down_timeout)
        end
    end.

dead_process() ->
    {Pid, Mon} = spawn_monitor(fun() -> ok end),
    receive
        {'DOWN', Mon, process, Pid, normal} -> Pid
    after 1000 ->
        error(dead_process_timeout)
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

preloaded_otp_mfa_trace_test_() ->
    {timeout, 10, fun preloaded_otp_mfa_trace/0}.

preloaded_otp_mfa_trace() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{mfa => <<"lists:reverse/1">>, max => 1, duration_ms => 1000},
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {lists, reverse, 1}),
    Tracee ! {apply, self(), lists, reverse, [[a, b]]},
    receive
        {called, [b, a]} -> ok
    after 1000 ->
        erlang:error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(ok, maps:get(status, Result)),
    [Event] = maps:get(events, maps:get(capture, Result)),
    ?assertEqual({mfa, lists, reverse, 1}, maps:get(mfa, Event)),
    assert_clean_mfa(Tracee, {lists, reverse, 1}),
    Tracee ! stop.

setup_replaces_global_trace_and_fixed_collision_test_() ->
    {timeout, 15, [
        {atom_to_list(Name), fun() -> setup_replaces_global_trace_and_fixed_collision(Name) end}
     || Name <- [recon_trace_tracer, recon_trace_formatter]
    ]}.

fixed_name_port_collision_test_() ->
    {timeout, 15, [
        {atom_to_list(Name), fun() -> fixed_name_port_collision(Name) end}
     || Name <- [recon_trace_tracer, recon_trace_formatter]
    ]}.

fixed_name_port_stop_is_structured_test_() ->
    {timeout, 10, [
        {atom_to_list(Name), fun() -> fixed_name_port_stop_is_structured(Name) end}
     || Name <- [recon_trace_tracer, recon_trace_formatter]
    ]}.

setup_replaces_global_trace_and_fixed_collision(Name) ->
    cleanup(),
    Tracee = tracee(),
    UnrelatedMFA = {erlang, node, 0},
    1 = erlang:trace_pattern(UnrelatedMFA, true, []),
    Collision = spawn(fun collision/0),
    true = register(Name, Collision),
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

fixed_name_port_collision(Name) ->
    cleanup(),
    Tracee = tracee(),
    {PortOwner, PortOwnerMon, Port} = start_fixed_name_port_owner(Name),
    try
        {Caller, Ref} = start_call(self(), request(Tracee)),
        wait_trace_active(Tracee, {?MODULE, fixture, 0}),
        ?assertEqual(undefined, erlang:port_info(Port)),
        ?assert(is_process_alive(PortOwner)),
        Tracee ! {call, self(), 1},
        receive
            called -> ok
        after 1000 -> error(tracee_timeout)
        end,
        Result = receive_result(Caller, Ref),
        ?assertEqual(ok, maps:get(status, Result)),
        [Warning] = maps:get(warnings, Result),
        ?assertNotEqual(
            nomatch, binary:match(maps:get(message, Warning), <<"processes or ports">>)
        ),
        assert_clean(Tracee)
    after
        stop_fixed_name_port_owner(PortOwner, PortOwnerMon),
        Tracee ! stop
    end.

fixed_name_port_stop_is_structured(Name) ->
    cleanup(),
    {PortOwner, PortOwnerMon, Port} = start_fixed_name_port_owner(Name),
    try
        Result = observer_cli_trace:stop_all(),
        ?assertEqual(error, maps:get(status, Result)),
        ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
        ?assertEqual(false, maps:get(cleanup_confirmed, maps:get(capture, Result))),
        ?assertEqual(undefined, whereis(Name)),
        ?assertEqual(undefined, erlang:port_info(Port)),
        ?assert(is_process_alive(PortOwner))
    after
        stop_fixed_name_port_owner(PortOwner, PortOwnerMon)
    end.

start_fixed_name_port_owner(Name) ->
    Parent = self(),
    {Owner, Mon} = spawn_monitor(fun() ->
        Port = open_port({spawn, "cat"}, []),
        true = register(Name, Port),
        Parent ! {self(), fixed_name_port, Port},
        receive
            stop ->
                _ =
                    try
                        port_close(Port)
                    catch
                        _:_ -> false
                    end,
                ok
        end
    end),
    receive
        {Owner, fixed_name_port, Port} -> {Owner, Mon, Port};
        {'DOWN', Mon, process, Owner, Reason} -> error({port_owner_down, Reason})
    after 1000 ->
        error(port_owner_timeout)
    end.

stop_fixed_name_port_owner(Owner, Mon) ->
    Owner ! stop,
    receive
        {'DOWN', Mon, process, Owner, normal} -> ok;
        {'DOWN', Mon, process, Owner, Reason} -> error({port_owner_down, Reason})
    after 1000 ->
        exit(Owner, kill),
        receive
            {'DOWN', Mon, process, Owner, _Reason} -> ok
        end,
        error(port_owner_timeout)
    end.

response_cap_continues_natural_drain_test_() ->
    {timeout, 10, fun response_cap_continues_natural_drain/0}.

response_cap_marks_rate_capture_partial_test_() ->
    {timeout, 10, fun response_cap_marks_rate_capture_partial/0}.

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

fallback_response_survives_snapshot_validation_test_() ->
    {timeout, 10, fun fallback_response_survives_snapshot_validation/0}.

real_event_survives_include_and_redact_validation_test_() ->
    {timeout, 15, [
        {atom_to_list(Policy), fun() -> real_event_survives_validation(Policy) end}
     || Policy <- [include, redact]
    ]}.

external_trace_interference_is_partial_test_() ->
    {timeout, 15, [
        {"other pid with expected mfa", fun other_pid_interference_is_partial/0},
        {"expected pid with other mfa", fun other_mfa_interference_is_partial/0}
    ]}.

extended_patterns_and_legacy_flags_are_cleared_test_() ->
    {timeout, 10, fun extended_patterns_and_legacy_flags_are_cleared/0}.

preexisting_on_load_cannot_pollute_capture_test_() ->
    {timeout, 15, [
        {atom_to_list(Scope), fun() -> preexisting_on_load_cannot_pollute_capture(Scope) end}
     || Scope <- [local, global]
    ]}.

recon_rate_breaker_boundary_test_() ->
    {timeout, 12, fun recon_rate_breaker_boundary/0}.

completion_before_monitor_attachment_test_() ->
    {timeout, 15, [
        {"count limit", fun() -> completion_before_monitor_attachment(1, 1, limit_reached) end},
        {"rate breaker", fun() ->
            completion_before_monitor_attachment({1, 1000}, 2, rate_exceeded)
        end},
        {"insufficient events", fun insufficient_completion_before_monitor_attachment/0},
        {"rate across windows", fun rate_across_windows_is_not_inferred_complete/0}
    ]}.

actual_module_reload_marks_capture_partial_test_() ->
    {timeout, 10, fun actual_module_reload_marks_capture_partial/0}.

pre_ready_owner_crash_kills_linked_helpers_test_() ->
    {timeout, 10, fun pre_ready_owner_crash_kills_linked_helpers/0}.

pre_ready_owner_normal_exit_stops_helpers_test_() ->
    {timeout, 10, fun pre_ready_owner_normal_exit_stops_helpers/0}.

pre_recon_tracee_exit_is_classified_test_() ->
    {timeout, 10, fun pre_recon_tracee_exit_is_classified/0}.

snapshot_timeout_reports_unconfirmed_trace_cleanup_test_() ->
    {timeout, 10, fun snapshot_timeout_reports_unconfirmed_trace_cleanup/0}.

snapshot_worker_death_reports_unconfirmed_trace_cleanup_test_() ->
    {timeout, 10, fun snapshot_worker_death_reports_unconfirmed_trace_cleanup/0}.

snapshot_stop_omits_historical_events_test_() ->
    {timeout, 10, fun snapshot_stop_omits_historical_events/0}.

concurrent_stop_is_bounded_test_() ->
    {timeout, 10, fun concurrent_stop_is_bounded/0}.

unknown_mfa_does_not_grow_atom_table_test() ->
    Base = request(self()),
    _ = observer_cli_trace:call(self(), Base#{mfa => <<"missing:warmup/0">>}),
    Before = erlang:system_info(atom_count),
    lists:foreach(
        fun(Index) ->
            Unique = iolist_to_binary(io_lib:format("missing_~B:function_~B/0", [Index, Index])),
            ?assertEqual(
                invalid_mfa, maps:get(reason, observer_cli_trace:call(self(), Base#{mfa => Unique}))
            )
        end,
        lists:seq(1, 100)
    ),
    ?assertEqual(Before, erlang:system_info(atom_count)).

stop_all_requires_target_acknowledgement_test() ->
    cleanup(),
    MFA = {?MODULE, fixture, 0},
    1 = erlang:trace_pattern(MFA, true, []),
    ?assertEqual(
        {probe_error, invalid_request},
        observer_cli_snapshot:capture_trace(#{action => stop_all}, #{controller => self()})
    ),
    ?assertEqual({traced, global}, erlang:trace_info(MFA, traced)),
    1 = erlang:trace_pattern(MFA, false, []).

dynamic_trace_session_survives_legacy_cleanup_test() ->
    case code:ensure_loaded(trace) of
        {module, trace} ->
            case erlang:function_exported(trace, session_create, 3) of
                true -> dynamic_trace_session_survives_legacy_cleanup();
                false -> ok
            end;
        {error, _Reason} ->
            ok
    end.

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
    ?assertEqual(partial, maps:get(status, Capture)),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(truncated, Capture)),
    ?assertEqual(2, length(maps:get(events, Capture))),
    ?assertEqual(1, maps:get(dropped_count, Capture)),
    assert_clean(Tracee),
    Tracee ! stop.

response_cap_marks_rate_capture_partial() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{
        max => {1, 1000}, test_event_cap => 1, duration_ms => 3000
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 2},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(rate_exceeded, maps:get(reason, Result)),
    Capture = maps:get(capture, Result),
    ?assertEqual(partial, maps:get(status, Capture)),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(truncated, Capture)),
    ?assertEqual(1, length(maps:get(events, Capture))),
    ?assertEqual(1, maps:get(dropped_count, Capture)),
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
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Result)))),
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
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    exit(whereis(recon_trace_formatter), kill),
    Result = receive_result(Caller, Ref),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Result)))),
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
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Result)))),
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
    assert_forced(Result),
    receive
        {'DOWN', CallerMon, process, Caller, _Reason} -> ok
    after 1000 -> error(dispatcher_down_timeout)
    end,
    Owner ! release,
    wait_until(fun() -> whereis(observer_cli_trace_owner) =:= undefined end),
    assert_clean(Tracee),
    Tracee ! stop.

fallback_response_survives_snapshot_validation() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{
        action => call,
        test_after_calls => fun() -> exit(self(), kill) end
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            trace,
            Request,
            #{timeout_ms => 4000, identifier_policy => include}
        ),
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(trace_call, include, node(), Response)
    ),
    Trace = maps:get(<<"trace">>, maps:get(<<"data">>, Response)),
    ?assertEqual(
        maps:get(<<"reason">>, maps:get(<<"data">>, Response)),
        maps:get(<<"reason">>, Trace)
    ),
    assert_clean(Tracee),
    Tracee ! stop.

real_event_survives_validation(Policy) ->
    cleanup(),
    Tracee = tracee(),
    Parent = self(),
    Ref = make_ref(),
    Request = #{
        action => call,
        mfa => <<"observer_cli_trace_test:fixture/0">>,
        pid => list_to_binary(pid_to_list(Tracee)),
        duration_ms => 1000,
        max => 1,
        replace_existing_trace => true
    },
    Caller = spawn(fun() ->
        Parent !
            {
                Ref,
                self(),
                observer_cli_snapshot:dispatch(
                    Parent,
                    trace,
                    Request,
                    #{timeout_ms => 4000, identifier_policy => Policy}
                )
            }
    end),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        receive
            {Ref, Caller, DispatchResult} -> DispatchResult
        after 5000 ->
            error(dispatch_timeout)
        end,
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(trace_call, Policy, node(), Response)
    ),
    Trace = maps:get(<<"trace">>, maps:get(<<"data">>, Response)),
    [Event] = maps:get(<<"events">>, Trace),
    assert_normalized_selector(Policy, Tracee, Trace),
    assert_normalized_selector(Policy, Tracee, Event),
    assert_clean(Tracee),
    Tracee ! stop.

assert_normalized_selector(include, Tracee, Value) ->
    ?assertEqual(list_to_binary(pid_to_list(Tracee)), maps:get(<<"tracee">>, Value)),
    ?assertEqual(
        #{
            <<"module">> => <<"observer_cli_trace_test">>,
            <<"function">> => <<"fixture">>,
            <<"arity">> => 0
        },
        maps:get(<<"mfa">>, Value)
    );
assert_normalized_selector(redact, _Tracee, Value) ->
    ?assertMatch(<<"pid-", _/binary>>, maps:get(<<"tracee">>, Value)),
    MFA = maps:get(<<"mfa">>, Value),
    ?assertMatch(<<"module-", _/binary>>, maps:get(<<"module">>, MFA)),
    ?assertMatch(<<"function-", _/binary>>, maps:get(<<"function">>, MFA)),
    ?assertEqual(0, maps:get(<<"arity">>, MFA)).

other_pid_interference_is_partial() ->
    cleanup(),
    Tracee = tracee(),
    Other = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 3000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracer = whereis(recon_trace_tracer),
    1 = erlang:trace(Other, true, [call, timestamp, {tracer, Tracer}]),
    Other ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    assert_interference_result(receive_result(Caller, Ref)),
    assert_clean(Tracee),
    Other ! stop,
    Tracee ! stop.

other_mfa_interference_is_partial() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 3000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    1 = erlang:trace_pattern({?MODULE, other_fixture, 0}, true, []),
    Tracee ! {other, self()},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    assert_interference_result(receive_result(Caller, Ref)),
    assert_clean(Tracee),
    ?assertEqual({traced, false}, erlang:trace_info({?MODULE, other_fixture, 0}, traced)),
    Tracee ! stop.

assert_interference_result(Result) ->
    ?assertEqual(ok, maps:get(status, Result)),
    Capture = maps:get(capture, Result),
    ?assertEqual(partial, maps:get(status, Capture)),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    ?assertEqual(true, maps:get(truncated, Capture)),
    ?assertEqual(null, maps:get(dropped_count, Capture)),
    ?assertEqual(true, maps:get(interference_detected, Capture)),
    ?assertEqual([], maps:get(events, Capture)).

extended_patterns_and_legacy_flags_are_cleared() ->
    cleanup(),
    Tracee = tracee(),
    Victim = tracee(),
    DummyTracer = spawn(fun stubborn_helper/0),
    VictimPort = open_port({spawn, "cat"}, []),
    1 = erlang:trace(Victim, true, [send, {tracer, DummyTracer}]),
    1 = erlang:trace(VictimPort, true, [ports, {tracer, DummyTracer}]),
    0 = erlang:trace_pattern(on_load, true, [local, call_memory]),
    1 = erlang:trace_pattern({erlang, node, 0}, true, [call_memory]),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 3000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    ?assertEqual({all, false}, erlang:trace_info(on_load, all)),
    ?assertEqual({call_memory, false}, erlang:trace_info({erlang, node, 0}, call_memory)),
    {flags, VictimFlags} = erlang:trace_info(Victim, flags),
    ?assertEqual(false, lists:member(send, VictimFlags)),
    ?assertEqual({flags, []}, erlang:trace_info(VictimPort, flags)),
    ?assertEqual({tracer, []}, erlang:trace_info(VictimPort, tracer)),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    ?assertEqual(ok, maps:get(status, receive_result(Caller, Ref))),
    assert_clean(Tracee),
    0 = erlang:trace_pattern(on_load, true, []),
    ?assertEqual({traced, global}, erlang:trace_info(on_load, traced)),
    observer_cli_trace:clear_trace(),
    ?assertEqual({all, false}, erlang:trace_info(on_load, all)),
    true = port_close(VictimPort),
    Victim ! stop,
    DummyTracer ! stop,
    Tracee ! stop.

preexisting_on_load_cannot_pollute_capture(Scope) ->
    cleanup(),
    unload_reload_fixture(),
    OnLoadFlags =
        case Scope of
            local -> [local];
            global -> []
        end,
    0 = erlang:trace_pattern(on_load, true, OnLoadFlags),
    Tracee = tracee(),
    {Caller, Ref} = start_call(self(), (request(Tracee))#{duration_ms => 3000}),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    load_reload_fixture(1),
    Tracee ! {apply, self(), ?RELOAD_FIXTURE, hit, []},
    receive
        {called, 1} -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    Capture = maps:get(capture, Result),
    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual(complete, maps:get(status, Capture)),
    ?assertEqual(false, maps:get(interference_detected, Capture)),
    [Event] = maps:get(events, Capture),
    ?assertEqual({mfa, ?MODULE, fixture, 0}, maps:get(mfa, Event)),
    ?assertEqual({all, false}, erlang:trace_info(on_load, all)),
    assert_clean(Tracee),
    Tracee ! stop,
    unload_reload_fixture().

dynamic_trace_session_survives_legacy_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    Session = trace:session_create(observer_cli_trace_dynamic_test, self(), []),
    try
        1 = trace:process(Session, Tracee, true, [call]),
        1 = trace:function(Session, {?MODULE, fixture, 0}, [], []),
        observer_cli_trace:clear_trace(),
        ?assertEqual({flags, [call]}, trace:info(Session, Tracee, flags)),
        ?assertEqual({traced, global}, trace:info(Session, {?MODULE, fixture, 0}, traced)),
        Tracee ! {call, self(), 1},
        receive
            called -> ok
        after 1000 ->
            error(tracee_timeout)
        end,
        receive
            {trace, Tracee, call, {?MODULE, fixture, []}} -> ok
        after 1000 ->
            error(dynamic_trace_event_timeout)
        end
    after
        true = trace:session_destroy(Session),
        Tracee ! stop
    end.

recon_rate_breaker_boundary() ->
    cleanup(),
    Tracee = tracee(),
    Request = (request(Tracee))#{max => {1, 1000}, duration_ms => 4000},
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    timer:sleep(1100),
    Tracee ! {call, self(), 3},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    ?assertEqual(rate_exceeded, maps:get(reason, Result)),
    ?assertEqual(4, length(maps:get(events, maps:get(capture, Result)))),
    assert_clean(Tracee),
    Tracee ! stop.

completion_before_monitor_attachment(Max, Calls, Reason) ->
    cleanup(),
    Tracee = tracee(),
    AfterCalls = fun() ->
        Tracee ! {call, self(), Calls},
        receive
            called -> ok
        after 1000 ->
            error(tracee_timeout)
        end,
        wait_until(fun() ->
            whereis(recon_trace_tracer) =:= undefined andalso
                whereis(recon_trace_formatter) =:= undefined
        end)
    end,
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{max => Max, duration_ms => 3000, test_after_calls => AfterCalls}
    ),
    ?assertEqual(ok, maps:get(status, Result)),
    ?assertEqual(Reason, maps:get(reason, Result)),
    Capture = maps:get(capture, Result),
    ?assertEqual(complete, maps:get(status, Capture)),
    ?assertEqual(true, maps:get(trace_complete, Capture)),
    ?assertEqual(Calls, length(maps:get(events, Capture))),
    assert_clean(Tracee),
    Tracee ! stop.

insufficient_completion_before_monitor_attachment() ->
    cleanup(),
    Tracee = tracee(),
    AfterCalls = fun() ->
        recon_trace:clear(),
        wait_until(fun() ->
            whereis(recon_trace_tracer) =:= undefined andalso
                whereis(recon_trace_formatter) =:= undefined
        end)
    end,
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{max => 2, duration_ms => 3000, test_after_calls => AfterCalls}
    ),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assertEqual([], maps:get(events, maps:get(capture, Result))),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

rate_across_windows_is_not_inferred_complete() ->
    cleanup(),
    Tracee = tracee(),
    AfterCalls = fun() ->
        Tracee ! {call, self(), 1},
        receive
            called -> ok
        after 1000 ->
            error(tracee_timeout)
        end,
        timer:sleep(1100),
        Tracee ! {call, self(), 1},
        receive
            called -> ok
        after 1000 ->
            error(tracee_timeout)
        end,
        recon_trace:clear(),
        wait_until(fun() ->
            whereis(recon_trace_tracer) =:= undefined andalso
                whereis(recon_trace_formatter) =:= undefined
        end)
    end,
    Result = observer_cli_trace:call(
        self(),
        (request(Tracee))#{
            max => {1, 1000}, duration_ms => 3000, test_after_calls => AfterCalls
        }
    ),
    ?assertEqual(error, maps:get(status, Result)),
    ?assertEqual(capture_internal_error, maps:get(reason, Result)),
    ?assertEqual(2, length(maps:get(events, maps:get(capture, Result)))),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

actual_module_reload_marks_capture_partial() ->
    cleanup(),
    load_reload_fixture(1),
    Before = observer_cli_trace:module_md5(?RELOAD_FIXTURE),
    Tracee = tracee(),
    Test = self(),
    Reload = fun() ->
        load_reload_fixture(2),
        Test ! reloaded
    end,
    Request = (request(Tracee))#{
        mfa => <<"observer_cli_trace_reload_fixture:hit/0">>,
        duration_ms => 3000,
        test_before_helper_stop => Reload
    },
    {Caller, Ref} = start_call(self(), Request),
    wait_trace_active(Tracee, {?RELOAD_FIXTURE, hit, 0}),
    Tracee ! {apply, self(), ?RELOAD_FIXTURE, hit, []},
    receive
        {called, 1} -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    Result = receive_result(Caller, Ref),
    receive
        reloaded -> ok
    after 1000 ->
        error(reload_timeout)
    end,
    Capture = maps:get(capture, Result),
    ?assertNotEqual(Before, observer_cli_trace:module_md5(?RELOAD_FIXTURE)),
    ?assertEqual(partial, maps:get(status, Capture)),
    ?assertEqual(true, maps:get(module_reloaded, Capture)),
    ?assertEqual(false, maps:get(trace_complete, Capture)),
    assert_clean_mfa(Tracee, {?RELOAD_FIXTURE, hit, 0}),
    Tracee ! stop,
    unload_reload_fixture().

pre_ready_owner_crash_kills_linked_helpers() ->
    cleanup(),
    Tracee = tracee(),
    Test = self(),
    BeforeReady = fun() ->
        Test ! {before_ready, self()},
        receive
            continue -> ok
        end
    end,
    {Caller, Ref} = start_call(self(), (request(Tracee))#{test_before_ready => BeforeReady}),
    Owner =
        receive
            {before_ready, Pid} -> Pid
        after 1000 ->
            error(before_ready_timeout)
        end,
    #{collector := Collector, silent_io := SilentIO} = wait_helpers(),
    CollectorMon = erlang:monitor(process, Collector),
    SilentMon = erlang:monitor(process, SilentIO),
    exit(Owner, kill),
    await_down(Collector, CollectorMon),
    await_down(SilentIO, SilentMon),
    Result = receive_result(Caller, Ref),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
    assert_clean(Tracee),
    Tracee ! stop.

pre_ready_owner_normal_exit_stops_helpers() ->
    cleanup(),
    Tracee = tracee(),
    Test = self(),
    BeforeReady = fun() ->
        Test ! {before_ready_helpers, self(), observer_cli_trace:test_helpers()},
        receive
            exit_normal -> exit(normal)
        end
    end,
    {Caller, Ref} = start_call(self(), (request(Tracee))#{test_before_ready => BeforeReady}),
    {Owner, #{collector := Collector, silent_io := SilentIO}} =
        receive
            {before_ready_helpers, OwnerPid, Helpers} -> {OwnerPid, Helpers}
        after 1000 ->
            error(before_ready_timeout)
        end,
    ?assert(is_process_alive(Collector)),
    ?assert(is_process_alive(SilentIO)),
    CollectorMon = erlang:monitor(process, Collector),
    SilentMon = erlang:monitor(process, SilentIO),
    Owner ! exit_normal,
    await_down(Collector, CollectorMon),
    await_down(SilentIO, SilentMon),
    Result = receive_result(Caller, Ref),
    ?assertEqual(cleanup, maps:get(category, Result)),
    ?assertEqual(cleanup_unconfirmed, maps:get(reason, Result)),
    ?assertEqual(false, maps:get(cleanup_confirmed, maps:get(capture, Result))),
    assert_clean(Tracee),
    Tracee ! stop.

pre_recon_tracee_exit_is_classified() ->
    cleanup(),
    Tracee = tracee(),
    ExitTracee = fun() ->
        exit(Tracee, kill),
        wait_until(fun() -> not is_process_alive(Tracee) end)
    end,
    Result = observer_cli_trace:call(
        self(), (request(Tracee))#{test_before_calls => ExitTracee}
    ),
    ?assertEqual(tracee_exited, maps:get(reason, Result)),
    assert_forced(Result),
    assert_clean(Tracee).

snapshot_timeout_reports_unconfirmed_trace_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    Test = self(),
    Block = fun() ->
        Test ! {blocked_owner, self()},
        receive
            release -> ok
        end
    end,
    Parent = self(),
    Ref = make_ref(),
    Request = (request(Tracee))#{
        action => call,
        duration_ms => 5000,
        test_after_calls => Block
    },
    Caller = spawn(fun() ->
        Parent !
            {
                Ref,
                self(),
                observer_cli_snapshot:dispatch(
                    Parent,
                    trace,
                    Request,
                    #{timeout_ms => 1100, identifier_policy => include}
                )
            }
    end),
    Owner =
        receive
            {blocked_owner, Pid} -> Pid
        after 1000 ->
            error(blocked_owner_timeout)
        end,
    Response =
        receive
            {Ref, Caller, DispatchResult} -> DispatchResult
        after 3000 ->
            error(dispatch_timeout)
        end,
    ?assertMatch(
        #{
            <<"status">> := <<"error">>,
            <<"reason_code">> := <<"cleanup_unconfirmed">>,
            <<"cleanup_confirmed">> := false
        },
        Response
    ),
    ?assert(is_process_alive(Owner)),
    Owner ! release,
    wait_until(fun() -> observer_cli_trace:wait_cleanup(0) end),
    assert_clean(Tracee),
    Tracee ! stop.

snapshot_worker_death_reports_unconfirmed_trace_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    Test = self(),
    Block = fun() ->
        Test ! {blocked_owner, self()},
        receive
            release -> ok
        end
    end,
    Parent = self(),
    Ref = make_ref(),
    Request = (request(Tracee))#{
        action => call,
        duration_ms => 5000,
        test_after_calls => Block
    },
    Caller = spawn(fun() ->
        Parent !
            {
                Ref,
                self(),
                observer_cli_snapshot:dispatch(
                    Parent,
                    trace,
                    Request,
                    #{timeout_ms => 5000, identifier_policy => include}
                )
            }
    end),
    Owner =
        receive
            {blocked_owner, Pid} -> Pid
        after 1000 ->
            error(blocked_owner_timeout)
        end,
    try
        #{collector := Collector, silent_io := SilentIO} = wait_helpers(),
        {monitors, Monitors} = process_info(Owner, monitors),
        Excluded = [Parent, Tracee, Collector, SilentIO],
        [Worker] = [Pid || {process, Pid} <- Monitors, not lists:member(Pid, Excluded)],
        WorkerMon = erlang:monitor(process, Worker),
        exit(Worker, kill),
        receive
            {'DOWN', WorkerMon, process, Worker, killed} -> ok
        end,
        exit(Owner, kill),
        wait_until(fun() -> observer_cli_trace:wait_cleanup(0) end),
        ?assertEqual(
            {traced, global}, erlang:trace_info({?MODULE, fixture, 0}, traced)
        ),
        Response =
            receive
                {Ref, Caller, DispatchResult} -> DispatchResult
            after 3000 ->
                error(dispatch_timeout)
            end,
        ?assertMatch(
            #{
                <<"status">> := <<"error">>,
                <<"reason_code">> := <<"cleanup_unconfirmed">>,
                <<"cleanup_confirmed">> := false
            },
            Response
        )
    after
        cleanup(),
        Tracee ! stop
    end.

snapshot_stop_omits_historical_events() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    StopResponse = observer_cli_snapshot:capture_trace(
        #{action => stop_all, all => true}, #{controller => self()}
    ),
    StopTrace = maps:get(trace, maps:get(<<"data">>, StopResponse)),
    ?assertEqual([], maps:get(events, StopTrace)),
    Original = receive_result(Caller, Ref),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Original)))),
    assert_clean(Tracee),
    Tracee ! stop.

concurrent_stop_is_bounded() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Parent = self(),
    Stoppers = [
        spawn(fun() ->
            receive
                go -> Parent ! {self(), observer_cli_trace:stop_all()}
            end
        end)
     || _ <- lists:seq(1, 2)
    ],
    lists:foreach(fun(Pid) -> Pid ! go end, Stoppers),
    Results = [
        receive
            {Pid, Result} -> Result
        after 4000 ->
            error(stop_timeout)
        end
     || Pid <- Stoppers
    ],
    ?assertEqual(
        [cleanup_unconfirmed, stopped],
        lists:sort([maps:get(reason, Result) || Result <- Results])
    ),
    Call = receive_result(Caller, Ref),
    ?assertEqual(stopped, maps:get(reason, Call)),
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

duration_preserves_collected_events_test_() ->
    {timeout, 10, fun duration_preserves_collected_events/0}.

duration_preserves_collected_events() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 200, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    Result = receive_result(Caller, Ref),
    ?assertEqual(duration_elapsed, maps:get(reason, Result)),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Result)))),
    assert_forced(Result),
    assert_clean(Tracee),
    Tracee ! stop.

stop_all_waits_for_owner_cleanup_test_() ->
    {timeout, 10, fun stop_all_waits_for_owner_cleanup/0}.

stop_all_waits_for_owner_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    Stop = observer_cli_trace:stop_all(),
    ?assertEqual(ok, maps:get(status, Stop)),
    ?assertEqual(stopped, maps:get(reason, Stop)),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Stop)))),
    Call = receive_result(Caller, Ref),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, Call)))),
    assert_forced(Call),
    assert_clean(Tracee),
    Tracee ! stop.

tracee_and_controller_loss_force_cleanup_test_() ->
    {timeout, 15, fun tracee_and_controller_loss_force_cleanup/0}.

tracee_and_controller_loss_force_cleanup() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee, {?MODULE, fixture, 0}),
    Tracee ! {call, self(), 1},
    receive
        called -> ok
    after 1000 ->
        error(tracee_timeout)
    end,
    #{collector := TraceeCollector} = wait_helpers(),
    wait_until(fun() -> collector_count(TraceeCollector) =:= 1 end),
    exit(Tracee, kill),
    TraceeResult = receive_result(Caller, Ref),
    ?assertEqual(tracee_exited, maps:get(reason, TraceeResult)),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, TraceeResult)))),
    assert_forced(TraceeResult),
    cleanup(),
    Tracee2 = tracee(),
    Controller = spawn(fun collision/0),
    {Caller2, Ref2} = start_call(
        Controller, (request(Tracee2))#{duration_ms => 5000, max => 10}
    ),
    wait_trace_active(Tracee2, {?MODULE, fixture, 0}),
    Tracee2 ! {call, self(), 1},
    receive
        called -> ok
    after 1000 -> error(tracee_timeout)
    end,
    #{collector := Collector} = wait_helpers(),
    wait_until(fun() -> collector_count(Collector) =:= 1 end),
    exit(Controller, kill),
    ControllerResult = receive_result(Caller2, Ref2),
    ?assertEqual(controller_disconnected, maps:get(reason, ControllerResult)),
    ?assertEqual(1, length(maps:get(events, maps:get(capture, ControllerResult)))),
    assert_forced(ControllerResult),
    assert_clean(Tracee2),
    Tracee2 ! stop.

helper_crash_and_owner_fallback_test_() ->
    {timeout, 15, fun helper_crash_and_owner_fallback/0}.

helper_crash_and_owner_fallback() ->
    cleanup(),
    Tracee = tracee(),
    {Caller, Ref} = start_call(
        self(), (request(Tracee))#{duration_ms => 5000, max => 10}
    ),
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
        {other, Caller} ->
            erlang:apply(?MODULE, other_fixture, []),
            Caller ! called,
            tracee_loop();
        {node, Caller} ->
            _ = erlang:node(),
            Caller ! called,
            tracee_loop();
        {apply, Caller, Module, Function, Args} ->
            Result = erlang:apply(Module, Function, Args),
            Caller ! {called, Result},
            tracee_loop();
        stop ->
            ok
    end.

load_reload_fixture(Value) ->
    Forms = [
        {attribute, 1, module, ?RELOAD_FIXTURE},
        {attribute, 2, export, [{hit, 0}]},
        {function, 3, hit, 0, [{clause, 3, [], [], [{integer, 3, Value}]}]}
    ],
    {ok, ?RELOAD_FIXTURE, Binary} = compile:forms(Forms, [binary]),
    {module, ?RELOAD_FIXTURE} = code:load_binary(
        ?RELOAD_FIXTURE, "observer_cli_trace_reload_fixture.erl", Binary
    ),
    ok.

unload_reload_fixture() ->
    _ = code:purge(?RELOAD_FIXTURE),
    _ = code:delete(?RELOAD_FIXTURE),
    _ = code:purge(?RELOAD_FIXTURE),
    ok.

await_down(Pid, Mon) ->
    receive
        {'DOWN', Mon, process, Pid, _Reason} -> ok
    after 1000 ->
        error(helper_down_timeout)
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
    ?assertEqual(maps:get(reason, Result), maps:get(reason, Capture)),
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
    observer_cli_trace:clear_trace(),
    ok.

-endif.
