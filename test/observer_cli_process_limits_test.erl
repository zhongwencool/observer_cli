-module(observer_cli_process_limits_test).

-ifdef(TEST).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([
    format/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

bounded_process_detail_test_() ->
    {timeout, 15, fun bounded_process_detail/0}.

bounded_process_detail() ->
    Small = idle_process(),
    Small ! hello,
    try
        {ok, SmallOutput} = observer_cli_process:bounded_process_detail(message, Small, 2000),
        ?assertNotEqual(nomatch, binary:match(SmallOutput, <<"hello">>)),
        ?assertEqual(nomatch, binary:match(SmallOutput, <<"too_large">>))
    after
        exit(Small, kill)
    end,

    assert_large_message(binary:copy(<<"x">>, 2 * 1024 * 1024)),
    assert_large_message(lists:seq(1, 100)),
    assert_large_dictionary(),
    assert_large_state(),
    assert_unsupported_state(),
    assert_custom_formatter_limits().

assert_large_message(Message) ->
    Target = idle_process(),
    Target ! Message,
    try
        assert_too_large(observer_cli_process:bounded_process_detail(message, Target, 2000)),
        ?assert(erlang:is_process_alive(Target))
    after
        exit(Target, kill)
    end.

assert_large_dictionary() ->
    Parent = self(),
    Target = spawn(fun() ->
        put(large, binary:copy(<<"x">>, 2 * 1024 * 1024)),
        Parent ! dictionary_ready,
        idle()
    end),
    receive
        dictionary_ready -> ok
    after 1000 ->
        error(dictionary_timeout)
    end,
    try
        assert_too_large(observer_cli_process:bounded_process_detail(dict, Target, 2000)),
        ?assert(erlang:is_process_alive(Target))
    after
        exit(Target, kill)
    end.

assert_large_state() ->
    {ok, Target} = gen_server:start_link(
        ?MODULE,
        binary:copy(<<"x">>, 2 * 1024 * 1024),
        []
    ),
    try
        {ok, Output} = observer_cli_process:bounded_process_detail(state, Target, 3000),
        assert_too_large({ok, Output}),
        ?assert(erlang:is_process_alive(Target))
    after
        gen_server:stop(Target)
    end.

assert_unsupported_state() ->
    Target = idle_process(),
    try
        ?assertEqual(error, observer_cli_process:bounded_process_detail(state, Target, 3000))
    after
        exit(Target, kill)
    end.

assert_custom_formatter_limits() ->
    Previous = application:get_env(observer_cli, formatter),
    application:set_env(observer_cli, formatter, #{mod => ?MODULE}),
    try
        Oversized = idle_process(),
        Oversized ! {oversized_output, 70000},
        try
            assert_too_large(
                observer_cli_process:bounded_process_detail(message, Oversized, 2000)
            )
        after
            exit(Oversized, kill)
        end,

        Hanging = idle_process(),
        Hanging ! {hang, self()},
        try
            assert_too_large(
                observer_cli_process:bounded_process_detail(message, Hanging, 50)
            ),
            receive
                {formatter_worker, Worker} -> ?assertNot(erlang:is_process_alive(Worker))
            after 1000 ->
                error(formatter_worker_missing)
            end
        after
            exit(Hanging, kill)
        end,

        ParentDeathTarget = idle_process(),
        ParentDeathTarget ! {hang, self()},
        try
            Caller = spawn(fun() ->
                observer_cli_process:bounded_process_detail(message, ParentDeathTarget, 100)
            end),
            CallerMonitor = erlang:monitor(process, Caller),
            FormatterWorker =
                receive
                    {formatter_worker, WorkerPid} -> WorkerPid
                after 1000 ->
                    error(parent_death_formatter_worker_missing)
                end,
            FormatterMonitor = erlang:monitor(process, FormatterWorker),
            exit(Caller, kill),
            receive
                {'DOWN', CallerMonitor, process, Caller, _Reason} -> ok
            after 1000 ->
                error(formatter_caller_still_alive)
            end,
            receive
                {'DOWN', FormatterMonitor, process, FormatterWorker, _Reason} -> ok
            after 1000 ->
                error(orphaned_formatter_worker)
            end
        after
            exit(ParentDeathTarget, kill)
        end,

        HeapLimited = idle_process(),
        HeapLimited ! {heap_limit, self()},
        try
            assert_too_large(
                observer_cli_process:bounded_process_detail(message, HeapLimited, 2000)
            ),
            HeapWorker =
                receive
                    {formatter_worker, WorkerPid} -> WorkerPid
                after 1000 ->
                    error(heap_formatter_worker_missing)
                end,
            ?assertNot(erlang:is_process_alive(HeapWorker)),
            receive
                {formatter_survived, HeapWorker} -> error(heap_limit_not_enforced)
            after 0 ->
                ok
            end
        after
            exit(HeapLimited, kill)
        end
    after
        restore_formatter_env(Previous)
    end.

assert_too_large({ok, Output}) ->
    ?assert(is_binary(Output)),
    ?assert(byte_size(Output) =< 64 * 1024),
    ?assertNotEqual(nomatch, binary:match(Output, <<"too_large">>)).

idle_process() ->
    spawn(fun idle/0).

idle() ->
    receive
    after infinity -> ok
    end.

restore_formatter_env({ok, Formatter}) ->
    application:set_env(observer_cli, formatter, Formatter);
restore_formatter_env(undefined) ->
    application:unset_env(observer_cli, formatter).

format(_Pid, [{oversized_output, Count}]) ->
    lists:duplicate(Count, $x);
format(_Pid, [{hang, Owner}]) ->
    Owner ! {formatter_worker, self()},
    receive
    after infinity -> ok
    end;
format(_Pid, [{heap_limit, Owner}]) ->
    Owner ! {formatter_worker, self()},
    Binary = binary:copy(<<"x">>, 8 * 1024 * 1024),
    erlang:garbage_collect(),
    Owner ! {formatter_survived, self()},
    binary_to_list(Binary);
format(Pid, Term) ->
    observer_cli_formatter_default:format(Pid, Term).

init(State) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-endif.
