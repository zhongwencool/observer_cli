#!/usr/bin/env escript
%%! +S 4:4 +sbwt none +sbwtdcpu none +sbwtdio none
-mode(compile).

main([BeamDir]) ->
    true = code:add_patha(filename:absname(BeamDir)),
    io:format("Runtime metrics smoke: OTP ~s, ~p~n", [erlang:system_info(otp_release), os:type()]),
    io:format("BEAM PID: ~s~n", [os:getpid()]),
    FlagBefore = erlang:statistics(scheduler_wall_time),
    Context = observer_cli_runtime_metrics:init(1500),
    Idle = measure("idle", Context),
    One = [spawn(fun busy/0)],
    Single = try measure("one busy process", Context) after stop(One) end,
    Many = [spawn(fun busy/0) || _ <- lists:seq(1, 4)],
    Multi = try measure("four busy processes", Context) after stop(Many) end,
    Back = measure("idle again", Context),
    true = percent(Single) > percent(Idle) + 5,
    true = percent(Multi) > percent(Idle) + 5,
    true = percent(Back) < percent(Single),
    memory_smoke(Context),
    FlagBefore = erlang:statistics(scheduler_wall_time),
    remote_smoke(BeamDir),
    io:format("ok: CPU reacts to work, RSS/VM allocations observed, target identity verified; no scheduler registration~n");
main(_) -> erlang:error("usage: runtime-metrics-smoke.escript PATH_TO_OBSERVER_CLI_EBIN").

measure(Label, Context) ->
    First = observer_cli_runtime_metrics:sample(Context),
    timer:sleep(1500),
    Last = observer_cli_runtime_metrics:sample(Context),
    {Metrics, _} = observer_cli_runtime_metrics:window(First, Last),
    io:format("~s: ~s | ~s | RSS ~s~n", [Label,
        observer_cli_runtime_metrics:format_cpu(Metrics),
        observer_cli_runtime_metrics:format_window(Metrics),
        observer_cli_runtime_metrics:format_rss(Metrics)]),
    Metrics.

percent(#{cpu := #{status := available, percent := P}}) -> P.

busy() ->
    receive stop -> ok after 0 -> erlang:phash2({self(), make_ref()}), busy() end.

stop(Pids) ->
    lists:foreach(fun(P) ->
        M = erlang:monitor(process, P),
        P ! stop,
        receive {'DOWN', M, process, P, _} -> ok after 1000 -> erlang:error(worker_did_not_stop) end
    end, Pids).

memory_smoke(Context) ->
    Before = observer_cli_runtime_metrics:sample(Context),
    VmBefore = erlang:memory(binary),
    Parent = self(),
    {Holder, Monitor} = spawn_monitor(fun() ->
        Binary = binary:copy(<<42>>, 64 * 1048576),
        Parent ! {allocated, self()},
        receive release -> Parent ! {released, byte_size(Binary)} end
    end),
    After = try
        receive {allocated, Holder} -> ok after 5000 -> erlang:error(allocation_timeout) end,
        VmAfter = erlang:memory(binary),
        SampleAfter = observer_cli_runtime_metrics:sample(Context),
        true = VmAfter - VmBefore >= 63 * 1048576,
        true = maps:get(rss_bytes, SampleAfter) > maps:get(rss_bytes, Before),
        io:format("memory: RSS change ~B bytes; BEAM binary change ~B bytes~n",
            [maps:get(rss_bytes, SampleAfter) - maps:get(rss_bytes, Before), VmAfter - VmBefore]),
        SampleAfter
    after
        Holder ! release,
        receive {'DOWN', Monitor, process, Holder, _} -> ok after 1000 -> exit(Holder, kill) end
    end,
    {Metrics, _} = observer_cli_runtime_metrics:window(After, observer_cli_runtime_metrics:sample(Context)),
    io:format("after release: RSS ~s (immediate OS return is not required)~n",
        [observer_cli_runtime_metrics:format_rss(Metrics)]).

remote_smoke(BeamDir) ->
    Name = list_to_atom("runtime_metrics_controller_" ++ os:getpid()),
    {ok, _} = net_kernel:start([Name, shortnames]),
    {ok, Peer, Node} = peer:start_link(#{name => runtime_metrics_target, connection => standard_io,
        args => ["+S", "2:2", "-pa", filename:absname(BeamDir)]}),
    try
        Context = erpc:call(Node, observer_cli_runtime_metrics, init, [1500]),
        Sample = erpc:call(Node, observer_cli_runtime_metrics, sample, [Context]),
        TargetPid = erpc:call(Node, os, getpid, []),
        {Node, TargetPid} = maps:get(identity, Sample),
        true = TargetPid =/= os:getpid(),
        true = is_integer(maps:get(cpu_time_us, Sample)),
        undefined = erpc:call(Node, erlang, statistics, [scheduler_wall_time]),
        io:format("remote: sampled ~p PID ~s; controller PID ~s~n", [Node, TargetPid, os:getpid()])
    after peer:stop(Peer), net_kernel:stop()
    end.
