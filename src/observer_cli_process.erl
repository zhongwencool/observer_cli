-module(observer_cli_process).

-include("observer_cli.hrl").

-export([start/2]).

-spec start(pid(), view_opts()) -> no_return.
start(ProcessPid, Opts) ->
    #view_opts{process = #process{interval = RefreshMillSecond}} = Opts,
    ChildPid = spawn(fun() ->
        ?output(?CLEAR),
        InitQ = lists:foldl(fun(_X, Acc) -> queue:in(waiting, Acc) end, queue:new(), lists:seq(1, 5)),
        render_worker(RefreshMillSecond, ProcessPid, undefined, InitQ, InitQ)
                     end),
    manager(ChildPid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, #view_opts{process = ProcOpts} = Opts) ->
    case observer_cli_lib:parse_cmd(Opts, ChildPid) of
        quit -> erlang:send(ChildPid, quit);
        {proc_count, binary_memory, no_change} -> %% shared with home 'b'
            erlang:exit(ChildPid, stop),
            observer_cli:start(Opts);
        {new_interval, NewInterval} ->
            erlang:send(ChildPid, {new_interval, NewInterval}),
            manager(ChildPid, Opts#view_opts{process = ProcOpts#process{interval = NewInterval}});
        _ -> manager(ChildPid, Opts)
    end.

render_worker(Interval, ProcessPid, TimeRef, OldRedus, OldMems) ->
    ProcessInfo = recon:info(ProcessPid),
    Meta = proplists:get_value(meta, ProcessInfo),
    RegisteredName = proplists:get_value(registered_name, Meta),
    Dictionary = proplists:get_value(dictionary, Meta),
    GroupLeader = proplists:get_value(group_leader, Meta),
    Status = proplists:get_value(status, Meta),
    
    Signals = proplists:get_value(signals, ProcessInfo),
    Link = proplists:get_value(links, Signals),
    Monitors = proplists:get_value(monitors, Signals),
    MonitoredBy = proplists:get_value(monitored_by, Signals),
    TrapExit = proplists:get_value(trap_exit, Signals),
    
    Location = proplists:get_value(location, ProcessInfo),
    InitialCall = proplists:get_value(initial_call, Location),
    CurrentStacktrace = proplists:get_value(current_stacktrace, Location),
    
    MemoryUsed = proplists:get_value(memory_used, ProcessInfo),
    Memory = proplists:get_value(memory, MemoryUsed),
    MessageQueueLen = proplists:get_value(message_queue_len, MemoryUsed),
    HeapSize = proplists:get_value(heap_size, MemoryUsed),
    TotalHeapSize = proplists:get_value(total_heap_size, MemoryUsed),
    GarbageCollection = proplists:get_value(garbage_collection, MemoryUsed),
    
    Work = proplists:get_value(work, ProcessInfo),
    Reductions = proplists:get_value(reductions, Work),
    
    Line1 = render_line(RegisteredName, GroupLeader, Status, TrapExit, InitialCall,
        MessageQueueLen, HeapSize, TotalHeapSize, GarbageCollection),
    {NewRedus, NewMems, Line2} = render_reductions_memory(Reductions, Memory, OldRedus, OldMems),
    Line3 = render_pid_info(ProcessPid, Dictionary, Link, Monitors, MonitoredBy, CurrentStacktrace),
    Line4 = render_last_line(Interval),
    redraw_screen_to_blank(),
    ?output([?CURSOR_TOP, Line1, Line2, Line3, Line4]),
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    receive
        redraw -> render_worker(Interval, ProcessPid, NewTimeRef, NewRedus, NewMems);
        {new_interval, NewInterval} -> render_worker(NewInterval, ProcessPid, NewTimeRef, NewRedus, NewMems);
        quit -> quit
    end.

redraw_screen_to_blank() ->
    ?output(?CURSOR_TOP),
    BlankLine = lists:concat(lists:duplicate(?COLUMN, " ")),
    [begin io:format(BlankLine) end || _Q <- lists:seq(1, 35)].

render_line(RegisteredName, GroupLeader, Status, TrapExit, InitialCall,
    MessageQueueLen, HeapSize, TotalHeapSize, GarbageCollection) ->
    MinBinVHeapSize = integer_to_list(proplists:get_value(min_bin_vheap_size, GarbageCollection)),
    MinHeapSize = integer_to_list(proplists:get_value(min_heap_size, GarbageCollection)),
    FullsweepAfter = integer_to_list(proplists:get_value(fullsweep_after, GarbageCollection)),
    MinorGcs = integer_to_list(proplists:get_value(minor_gcs, GarbageCollection)),
    
    InitialCallStr = observer_cli_lib:mfa_to_list(InitialCall),
    MessageQueueLenStr = integer_to_list(MessageQueueLen),
    HeapSizeStr = integer_to_list(HeapSize),
    TotalHeapSizeStr = integer_to_list(TotalHeapSize),
    GroupLeaderStr = erlang:pid_to_list(GroupLeader),
    
    Format = "|~-15.15s|~52.52s|~17.17s|~12.12s|~18.18s|~12.12s|~n",
    [io_lib:format("|\e[0m\e[7m~-15.15s|~-52.52s|~-17.17s|~-12.12s|~-18.18s|~-12.12s\e[0m|~n",
        ["Meta", "Value", "Memory Used", "Value", "Garbage Collection", "Value"]),
    io_lib:format(Format, ["registered_name", RegisteredName, "message_queue_len",
        MessageQueueLenStr, "min_bin_vheap_size", MinBinVHeapSize]),
    io_lib:format(Format, ["initial_call", InitialCallStr, "heap_size", HeapSizeStr, "min_heap_size", MinHeapSize]),
    io_lib:format(Format, ["group_leader", GroupLeaderStr, "total_heap_size", TotalHeapSizeStr, "fullsweep_after", FullsweepAfter]),
    io_lib:format(Format, ["status", Status, "trap_exit", TrapExit, "minor_gcs", MinorGcs])].


render_pid_info(Pid, Dictionary, Link, Monitors, MonitoredBy, CurrentStacktrace) ->
    PidStr = pid_to_list(Pid),
    DictionaryStr = dict_to_str(Dictionary, 4),
    LinkStr = pids_to_str(Link, 8),
    MonitorsStr = pids_to_str(Monitors, 8),
    MonitoredByStr = pids_to_str(MonitoredBy, 8),
    CurrentStacktraceStr = stacktrace_to_str(CurrentStacktrace),
    [io_lib:format("|Pid: ~-125s |~n", [PidStr]),
    io_lib:format("|Dictionary: ~-119.119s|~n", [DictionaryStr]),
    io_lib:format("|Link: ~-125.125s|~n", [LinkStr]),
    io_lib:format("|Monitors: ~-121.121s|~n", [MonitorsStr]),
    io_lib:format("|MonitoredBy: ~-118.118s|~n", [MonitoredByStr]),
    io_lib:format("|CurrentStackTraces: ~p~n", [CurrentStacktraceStr])].

render_reductions_memory(Reduction, Memory, Reductions, Memorys) ->
    {NewRedus, NewMems}
        = case queue:len(Reductions) >= 20 of
              true ->
                  RestRedus = queue:tail(Reductions),
                  RestMems = queue:tail(Memorys),
                  {queue:in(Reduction, RestRedus), queue:in(Memory, RestMems)};
              false -> {queue:in(Reduction, Reductions), queue:in(Memory, Memorys)}
          end,
    View =
        [
            io_lib:format("|Reductions: ~119.119s|~n", [get_chart_format(NewRedus)]),
            io_lib:format("|Memorys: ~122.122s|~n", [get_chart_format(NewMems)])
        ],
    {NewRedus, NewMems, View}.

render_last_line(Interval) ->
    Format =
        case Interval >= 10000 of
            true -> "|\e[31;1mINPUT: \e[0m\e[7mq(quit)        b(back)      i~w(refresh every ~wms)  ~78.78s\e[0m~n";
            false -> "|\e[31;1mINPUT: \e[0m\e[7mq(quit)        b(back)      i~w(refresh every ~wms)  ~80.80s\e[0m~n"
        end,
    io_lib:format(Format, [Interval, Interval, ?render(observer_cli_lib:uptime())]).

get_chart_format(Queue) ->
    List = queue:to_list(Queue),
    chart_format(List, "").

chart_format([_R], Lines) -> Lines;
chart_format([R, R | RestRedus], Lines) ->
    chart_format([R | RestRedus], Lines ++ observer_cli_lib:to_list(R) ++ "==>");
chart_format([R1, R2 | RestRedus], Lines) when R1 > R2 ->
    chart_format([R2 | RestRedus], Lines ++ observer_cli_lib:to_list(R1) ++ "==>");
chart_format([R1, R2 | RestRedus], Lines) when R1 < R2 ->
    chart_format([R2 | RestRedus], Lines ++ observer_cli_lib:to_list(R1) ++ "==>").

dict_to_str(Dicts, 10) -> lists:flatten(io_lib:format("~P", [Dicts, 10]));
dict_to_str(Dicts, Len) ->
    Str = lists:flatten(io_lib:format("~P", [Dicts, Len])),
    case length(Str) >= ?COLUMN of
        true -> string:sub_string(Str, 1, ?COLUMN - 7) ++ ".....";
        false -> dict_to_str(Dicts, Len + 1)
    end.

pids_to_str(Link, 13) -> lists:flatten(io_lib:format("~P", [Link, 13]));
pids_to_str(Link, Len) ->
    Str = lists:flatten(io_lib:format("~P", [Link, Len])),
    case length(Str) >= ?COLUMN of
        true -> string:sub_string(Str, 1, ?COLUMN - 7) ++ ".....";
        false -> dict_to_str(Link, Len + 1)
    end.

stacktrace_to_str(CurrentStacktrace) ->
    CurrentStacktrace.
