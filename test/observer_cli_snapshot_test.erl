-module(observer_cli_snapshot_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

capabilities_test() ->
    ?assertEqual(#{protocol_version => 1}, observer_cli_snapshot:capabilities()).

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
