-module(observer_cli_snapshot_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

capabilities_test() ->
    ?assertEqual(#{protocol_version => 1}, observer_cli_snapshot:capabilities()).

default_snapshot_is_scan_free_fact_package_test() ->
    Response = snapshot(#{}),
    ?assertEqual(<<"snapshot">>, maps:get(<<"command">>, Response)),
    ?assertMatch(
        #{<<"node">> := <<"node-1">>, <<"otp_release">> := _},
        maps:get(<<"target">>, Response)
    ),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, Capture)),
    ?assert(is_binary(maps:get(<<"started_at">>, Capture))),
    ?assert(is_binary(maps:get(<<"finished_at">>, Capture))),
    ?assert(maps:get(<<"duration_ms">>, Capture) >= 0),
    assert_probe(<<"runtime">>, true, <<"ok">>, Capture),
    assert_probe(<<"resources">>, true, <<"ok">>, Capture),
    assert_probe(<<"memory">>, true, <<"ok">>, Capture),
    assert_probe(<<"schedulers">>, false, <<"ok">>, Capture),
    assert_probe(<<"distribution">>, false, <<"ok">>, Capture),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(1, maps:get(<<"snapshot_version">>, Data)),
    Resources = maps:get(<<"resources">>, Data),
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                true,
                maps:get(<<"observer_contaminated">>, maps:get(Key, Resources))
            )
        end,
        [<<"process">>, <<"port">>, <<"atom">>]
    ),
    Memory = maps:get(<<"memory">>, Data),
    ?assertEqual(
        true,
        maps:get(<<"observer_contaminated">>, maps:get(<<"beam">>, Memory))
    ),
    GC = maps:get(<<"garbage_collection">>, Memory),
    ?assertEqual(
        maps:get(<<"reclaimed_words_total">>, GC) * erlang:system_info(wordsize),
        maps:get(<<"reclaimed_bytes_total">>, GC)
    ),
    ?assertEqual(
        false,
        maps:get(
            <<"scheduler_wall_time_enabled_by_observer_cli">>,
            maps:get(<<"schedulers">>, Data)
        )
    ),
    Distribution = maps:get(<<"distribution">>, Data),
    ?assertEqual(<<"empty">>, maps:get(<<"state">>, Distribution)),
    ?assertEqual([], maps:get(<<"connected_peers">>, Distribution)),
    [ModuleEffect] = [
        Effect
     || #{<<"id">> := <<"module_load">>} = Effect <-
            maps:get(<<"observer_effects">>, Capture)
    ],
    ?assertEqual(true, maps:get(<<"module_loaded_before_sample">>, ModuleEffect)),
    ?assertEqual([], maps:get(<<"errors">>, Response)),
    ?assertEqual([], maps:get(<<"warnings">>, Response)),
    ?assertEqual(
        nomatch,
        binary:match(term_to_binary(Response), atom_to_binary(node()))
    ),
    assert_json_safe(Response).

default_snapshot_does_not_call_full_enumerators_test() ->
    Parent = self(),
    Tracer = spawn(fun() -> trace_forwarder(Parent) end),
    Enumerators = [
        {erlang, processes, 0},
        {erlang, ports, 0},
        {ets, all, 0},
        {socket, which_sockets, 0},
        {application, loaded_applications, 0},
        {application, which_applications, 0},
        {application, which_applications, 1},
        {mnesia, system_info, 1}
    ],
    lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, true, [local]) end, Enumerators),
    erlang:trace(new, true, [call, {tracer, Tracer}]),
    try
        _ = snapshot(#{}),
        receive
            {enumerator_called, Call} -> ?assertEqual(no_full_enumerator_call, Call)
        after 100 ->
            ok
        end
    after
        erlang:trace(new, false, [call]),
        lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, false, [local]) end, Enumerators),
        exit(Tracer, kill)
    end.

snapshot_probe_failure_semantics_test() ->
    Unavailable = snapshot(#{
        test_probe_outcomes => #{schedulers => {unavailable, capability_unavailable}}
    }),
    UnavailableCapture = maps:get(<<"capture">>, Unavailable),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, UnavailableCapture)),
    assert_probe(<<"schedulers">>, false, <<"unavailable">>, UnavailableCapture),
    ?assertNot(is_map_key(<<"schedulers">>, maps:get(<<"data">>, Unavailable))),
    ?assertMatch(
        [#{<<"probe">> := <<"schedulers">>, <<"reason_code">> := <<"capability_unavailable">>}],
        maps:get(<<"warnings">>, Unavailable)
    ),
    OptionalTimeout = snapshot(#{
        test_probe_outcomes => #{schedulers => {timeout, target_timeout}}
    }),
    ?assertEqual(
        <<"partial">>,
        maps:get(<<"status">>, maps:get(<<"capture">>, OptionalTimeout))
    ),
    ?assertMatch(
        [#{<<"class">> := <<"partial">>, <<"probe">> := <<"schedulers">>}],
        maps:get(<<"errors">>, OptionalTimeout)
    ),
    RequiredError = snapshot(#{
        test_probe_outcomes => #{resources => {error, probe_failed}}
    }),
    ?assertEqual(
        <<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, RequiredError))
    ),
    ?assertNot(is_map_key(<<"resources">>, maps:get(<<"data">>, RequiredError))),
    ?assert(is_map_key(<<"memory">>, maps:get(<<"data">>, RequiredError))),
    ?assertMatch(
        [#{<<"class">> := <<"required_probe">>, <<"probe">> := <<"resources">>}],
        maps:get(<<"errors">>, RequiredError)
    ).

local_snapshot_text_and_term_envelopes_test() ->
    Response = snapshot(#{}),
    {ok, Text} = observer_cli_cli:encode(text, Response),
    ?assertNotEqual(nomatch, binary:match(Text, <<"observer_cli.cli/v1">>)),
    {ok, Term} = observer_cli_cli:encode(term, Response),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)).

normalization_and_identifier_policy_test() ->
    Reference = make_ref(),
    Raw = #{
        node => {identifier, node, 'target@host'},
        pid => self(),
        same_pid => self(),
        reference => Reference,
        table => {identifier, table, Reference},
        socket => {identifier, socket, {'$socket', Reference}},
        mfa => {mfa, observer_cli_snapshot, capabilities, 0},
        values => [1, 1.5, true, false, null, value]
    },
    {ok, Redacted} = observer_cli_snapshot:normalize(Raw, redact),
    ?assertEqual(<<"node-1">>, maps:get(<<"node">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"pid">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"same_pid">>, Redacted)),
    ?assertEqual(<<"ref-1">>, maps:get(<<"reference">>, Redacted)),
    ?assertEqual(<<"table-1">>, maps:get(<<"table">>, Redacted)),
    ?assertEqual(<<"socket-1">>, maps:get(<<"socket">>, Redacted)),
    ?assertEqual(
        #{<<"module">> => <<"module-1">>, <<"function">> => <<"function-1">>, <<"arity">> => 0},
        maps:get(<<"mfa">>, Redacted)
    ),
    assert_json_safe(Redacted),
    {ok, Included} = observer_cli_snapshot:normalize(Raw, include),
    ?assertEqual(<<"target@host">>, maps:get(<<"node">>, Included)),
    ?assertEqual(list_to_binary(pid_to_list(self())), maps:get(<<"pid">>, Included)),
    ?assertEqual(
        #{
            <<"module">> => <<"observer_cli_snapshot">>,
            <<"function">> => <<"capabilities">>,
            <<"arity">> => 0
        },
        maps:get(<<"mfa">>, Included)
    ).

invalid_utf8_and_field_cap_test() ->
    {ok, Tagged} = observer_cli_snapshot:normalize(<<16#FF, 0, 16#FE>>, include),
    ?assertEqual(<<"base64">>, maps:get(<<"encoding">>, Tagged)),
    ?assertEqual(<<16#FF, 0, 16#FE>>, base64:decode(maps:get(<<"data">>, Tagged))),
    ?assertEqual(
        {error, field_too_large},
        observer_cli_snapshot:normalize(binary:copy(<<"x">>, 64 * 1024 + 1), include)
    ).

dispatch_success_and_schema_failures_test() ->
    ?assertMatch(
        #{
            <<"status">> := <<"ok">>,
            <<"result">> := #{<<"pid">> := <<"pid-1">>},
            <<"cleanup_confirmed">> := true
        },
        dispatch_observed(#{pid => self()}, 2000, redact)
    ),
    assert_error(
        <<"field_too_large">>,
        dispatch_observed(binary:copy(<<"x">>, 64 * 1024 + 1), 2000, include)
    ),
    Oversized = #{
        required => lists:duplicate(20, binary:copy(<<"x">>, 60 * 1024))
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(Oversized, 3000, include)
    ),
    assert_error(
        <<"invalid_evidence_pointer">>,
        dispatch_observed(
            #{data => #{value => 1}, findings => [#{evidence => [#{path => <<"/data/missing">>}]}]},
            2000,
            include
        )
    ).

evidence_preserving_truncation_test() ->
    Item = #{value => binary:copy(<<"x">>, 60 * 1024)},
    Items = lists:duplicate(20, Item),
    Report = #{
        items => Items,
        returned_count => 20,
        dropped_count => 0,
        truncated => false,
        findings => [#{evidence => [#{path => <<"/items/0/value">>}]}]
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = dispatch_observed(
        Report, 3000, include
    ),
    ResultItems = maps:get(<<"items">>, Result),
    ?assert(length(ResultItems) < 20),
    ?assertEqual(true, maps:get(<<"truncated">>, Result)),
    ?assertEqual(length(ResultItems), maps:get(<<"returned_count">>, Result)),
    ?assertEqual(20 - length(ResultItems), maps:get(<<"dropped_count">>, Result)),
    ?assertMatch(#{<<"value">> := _}, hd(ResultItems)),
    ?assert(erlang:external_size(Result) =< 1024 * 1024),
    ProtectedTail = Report#{
        findings := [#{evidence => [#{path => <<"/items/19/value">>}]}]
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(ProtectedTail, 3000, include)
    ).

timeout_crash_and_heap_cleanup_test_() ->
    {timeout, 10, fun timeout_crash_and_heap_cleanup/0}.

timeout_crash_and_heap_cleanup() ->
    TimeoutResult = observer_cli_snapshot:dispatch(
        self(), test_timeout, self(), options(1050, include)
    ),
    TimeoutWorker = receive_worker(),
    assert_error(<<"target_timeout">>, TimeoutResult),
    ?assertNot(is_process_alive(TimeoutWorker)),
    CrashResult = observer_cli_snapshot:dispatch(
        self(), test_crash, self(), options(2000, include)
    ),
    CrashWorker = receive_worker(),
    assert_error(<<"probe_failed">>, CrashResult),
    ?assertNot(is_process_alive(CrashWorker)),
    ?assertEqual(nomatch, binary:match(term_to_binary(CrashResult), <<"fixture_secret">>)),
    HeapResult = observer_cli_snapshot:dispatch(
        self(), test_heap, self(), (options(3000, include))#{max_heap_words => 4096}
    ),
    HeapWorker = receive_worker(),
    assert_error(<<"worker_heap_limit_exceeded">>, HeapResult),
    ?assertNot(is_process_alive(HeapWorker)).

controller_disconnect_cleanup_test_() ->
    {timeout, 10, fun controller_disconnect_cleanup/0}.

controller_disconnect_cleanup() ->
    Parent = self(),
    Controller = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Coordinator = spawn(fun() ->
        Result = observer_cli_snapshot:dispatch(
            Controller, test_timeout, Parent, options(5000, include)
        ),
        Parent ! {dispatch_result, self(), Result}
    end),
    CoordinatorRef = erlang:monitor(process, Coordinator),
    Worker = receive_worker(),
    exit(Controller, kill),
    Result =
        receive
            {dispatch_result, Coordinator, DispatchResult} -> DispatchResult
        after 2000 ->
            erlang:error(dispatch_timeout)
        end,
    assert_error(<<"controller_disconnected">>, Result),
    receive
        {'DOWN', CoordinatorRef, process, Coordinator, normal} -> ok
    after 2000 ->
        erlang:error(coordinator_cleanup_timeout)
    end,
    ?assertNot(is_process_alive(Worker)),
    ?assertNot(is_process_alive(Coordinator)).

options(Timeout, Policy) ->
    #{timeout_ms => Timeout, identifier_policy => Policy}.

dispatch_observed(Request, Timeout, Policy) ->
    Result = observer_cli_snapshot:dispatch(
        self(), test_observed_echo, {self(), Request}, options(Timeout, Policy)
    ),
    Worker = receive_worker(),
    ?assertNot(is_process_alive(Worker)),
    Result.

snapshot(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            Request,
            options(3000, redact)
        ),
    Response.

assert_probe(Id, Required, Status, Capture) ->
    Probes = maps:get(<<"probes">>, Capture),
    [Probe] = [Item || #{<<"id">> := ProbeId} = Item <- Probes, ProbeId =:= Id],
    ?assertMatch(
        #{
            <<"required">> := Required,
            <<"status">> := Status,
            <<"reason_code">> := _,
            <<"duration_ms">> := _,
            <<"samples">> := _,
            <<"coverage">> := _
        },
        Probe
    ).

trace_forwarder(Parent) ->
    receive
        {trace, _Pid, call, Call} ->
            Parent ! {enumerator_called, Call},
            trace_forwarder(Parent);
        _Other ->
            trace_forwarder(Parent)
    end.

receive_worker() ->
    receive
        {test_worker, Worker} -> Worker
    after 2000 ->
        erlang:error(worker_start_timeout)
    end.

assert_error(ReasonCode, Result) ->
    ?assertEqual(
        #{
            <<"status">> => <<"error">>,
            <<"reason_code">> => ReasonCode,
            <<"cleanup_confirmed">> => true
        },
        Result
    ).

assert_json_safe(Map) when is_map(Map) ->
    lists:foreach(
        fun({Key, Value}) ->
            ?assert(is_binary(Key)),
            assert_json_safe(Value)
        end,
        maps:to_list(Map)
    );
assert_json_safe(List) when is_list(List) ->
    lists:foreach(fun assert_json_safe/1, List);
assert_json_safe(Value) when is_binary(Value); is_integer(Value); is_float(Value) ->
    ok;
assert_json_safe(Value) when Value =:= true; Value =:= false; Value =:= null ->
    ok.

-endif.
