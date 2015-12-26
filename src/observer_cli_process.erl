-module(observer_cli_process).

-include("observer_cli.hrl").

-export([start/3]).

-spec start(atom(), pid(), view_opts()) -> no_return.
start(Node, ProcessPid, Opts) ->
  #view_opts{process = #process{ interval = RefreshMillSecond}} = Opts,
    ParentPid = self(),
  ChildPid = spawn(fun() ->
    observer_cli_lib:clear_screen(),
    InitQ = lists:foldl(fun(_X, Acc) -> queue:in(waiting, Acc) end, queue:new(), lists:seq(1, 5)),
    loop(Node, RefreshMillSecond, ProcessPid, ParentPid, erlang:make_ref(), InitQ, InitQ) end),
  waiting(Node, ChildPid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Node, Interval, ProcessPid, ParentPid, TimeRef, OldRedus, OldMems) ->
  redraw_screen_to_blank(),
  observer_cli_lib:move_cursor_to_top_line(),
  erlang:cancel_timer(TimeRef),
  ProcessInfo = get_process_info(Node, ProcessPid),

  Meta = proplists:get_value(meta, ProcessInfo),
  RegisteredName = proplists:get_value(registered_name, Meta),
  Dictionary= proplists:get_value(dictionary, Meta),
  GroupLeader= proplists:get_value(group_leader, Meta),
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

  draw_line(RegisteredName, GroupLeader, Status, TrapExit, InitialCall,
    MessageQueueLen, HeapSize, TotalHeapSize, GarbageCollection),
  {NewRedus, NewMems} = draw_reductions_memory(Reductions, Memory, OldRedus, OldMems),
  draw_pid_info(Dictionary, Link, Monitors, MonitoredBy, CurrentStacktrace),
  draw_last_line(Node, Interval),
  NewTimeRef = erlang:send_after(Interval, self(), refresh),
  receive
    refresh -> loop(Node, Interval, ProcessPid, ParentPid, NewTimeRef, NewRedus, NewMems);
    {new_interval, NewInterval} -> loop(Node, NewInterval, ProcessPid, ParentPid, NewTimeRef, NewRedus, NewMems);
    quit -> quit;
    go_back_to_home_view -> erlang:send(ParentPid, draw_work_done_to_home_view)
  end.

redraw_screen_to_blank() ->
  observer_cli_lib:move_cursor_to_top_line(),
  BlankLine = lists:concat(lists:duplicate(?COLUMN_WIDTH, " ")),
  [begin io:format(BlankLine) end || _Q <- lists:seq(1, 35)].

draw_line(RegisteredName, GroupLeader, Status, TrapExit, InitialCall,
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
  io:format("|\e[46m~-15.15s|~-52.52s|~-17.17s|~-12.12s|~-18.18s|~-12.12s\e[49m|~n",
    ["Meta", "Value", "Memory Used", "Value", "Garbage Collection", "Value"]),
  io:format(Format, ["registered_name", RegisteredName, "message_queue_len", MessageQueueLenStr, "min_bin_vheap_size", MinBinVHeapSize]),
  io:format(Format , ["initial_call", InitialCallStr, "heap_size", HeapSizeStr, "min_heap_size", MinHeapSize]),
  io:format(Format , ["group_leader", GroupLeaderStr, "total_heap_size", TotalHeapSizeStr, "fullsweep_after", FullsweepAfter]),
  io:format(Format , ["status", Status, "trap_exit", TrapExit, "minor_gcs", MinorGcs]).


draw_pid_info(Dictionary, Link, Monitors, MonitoredBy, CurrentStacktrace) ->
  DictionaryStr = dict_to_str(Dictionary, 4),
  LinkStr = pids_to_str(Link, 8),
  MonitorsStr = pids_to_str(Monitors, 8),
  MonitoredByStr = pids_to_str(MonitoredBy, 8),
  CurrentStacktraceStr = stacktrace_to_str(CurrentStacktrace),
  io:format("|Dictionary: ~-119.119s|~n", [DictionaryStr]),
  io:format("|Link: ~-125.125s|~n", [LinkStr]),
  io:format("|Monitors: ~-121.121s|~n", [MonitorsStr]),
  io:format("|MonitoredBy: ~-118.118s|~n", [MonitoredByStr]),
  io:format("|CurrentStatckTraces: ~p~n", [CurrentStacktraceStr]).

draw_reductions_memory(Reduction, Memory, Reductions, Memorys) ->
  {NewRedus, NewMems}
    = case queue:len(Reductions) >= 20 of
        true ->
          RestRedus = queue:tail(Reductions),
          RestMems = queue:tail(Memorys),
          {queue:in(Reduction, RestRedus), queue:in(Memory, RestMems)};
        false -> {queue:in(Reduction, Reductions), queue:in(Memory, Memorys)}
      end,
  io:format("|Reductions: ~119.119s|~n", [get_chart_format(NewRedus)]),
  io:format("|Memorys: ~122.122s|~n", [get_chart_format(NewMems)]),
  {NewRedus, NewMems}.

draw_last_line(Node, Interval) ->
  Format =
    case Interval >= 10000 of
      true -> "|\e[31;1mINPUT: \e[0m\e[44mq(quit)        b(back)      r~w(refresh every ~wms)  ~65.65s\e[49m|~n";
      false -> "|\e[31;1mINPUT: \e[0m\e[44mq(quit)        b(back)      r~w(refresh every ~wms)  ~67.67s\e[49m|~n"
    end,
  io:format(Format, [Interval, Interval, "UpTime:" ++ observer_cli_lib:uptime(Node)]).

get_chart_format(Queue) ->
  List = queue:to_list(Queue),
  chart_format(List, "").

chart_format([_R], Lines) -> Lines;
chart_format([R, R|RestRedus], Lines) ->
  chart_format([R| RestRedus], Lines ++ observer_cli_lib:to_list(R) ++ "==>");
chart_format([R1, R2|RestRedus], Lines)when R1 > R2 ->
  chart_format([R2| RestRedus], Lines ++ observer_cli_lib:to_list(R1) ++ "==>");
chart_format([R1, R2|RestRedus], Lines)when R1 < R2 ->
  chart_format([R2| RestRedus], Lines ++ observer_cli_lib:to_list(R1) ++ "==>").

dict_to_str(Dicts, 10) -> lists:flatten(io_lib:format("~P", [Dicts, 10]));
dict_to_str(Dicts, Len) ->
  Str = lists:flatten(io_lib:format("~P", [Dicts, Len])),
  case length(Str) >= ?COLUMN_WIDTH of
    true -> string:sub_string(Str, 1, ?COLUMN_WIDTH - 7) ++ ".....";
    false -> dict_to_str(Dicts, Len + 1)
  end.

pids_to_str(Link, 13) -> lists:flatten(io_lib:format("~P", [Link, 13]));
pids_to_str(Link, Len) ->
  Str = lists:flatten(io_lib:format("~P", [Link, Len])),
  case length(Str) >= ?COLUMN_WIDTH of
    true -> string:sub_string(Str, 1, ?COLUMN_WIDTH - 7) ++ ".....";
    false -> dict_to_str(Link, Len + 1)
  end.

stacktrace_to_str(CurrentStacktrace) ->
  CurrentStacktrace.

waiting(Node, ChildPid, #view_opts{process = ProcOpts} = Opts) ->
  Input = observer_cli_lib:get_line(""),
  case  Input of
    "q\n" -> erlang:send(ChildPid, quit);
    "b\n" ->
      erlang:exit(ChildPid, stop),
      observer_cli:start_node(Node, Opts);
    [$r| RefreshInterval] ->
      case string:to_integer(RefreshInterval) of
        {error, no_integer} -> waiting(Node, ChildPid, Opts);
        {NewInterval, _} when NewInterval >= ?PROCESS_MIN_INTERVAL ->
          erlang:send(ChildPid, {new_interval, NewInterval}),
          waiting(Node, ChildPid, Opts#view_opts{process = ProcOpts#process{interval = NewInterval}})
      end;
    _ -> waiting(Node, ChildPid, Opts)
  end.

get_process_info(local_node, Pid) -> recon:info(Pid);
get_process_info(Node, Pid) -> rpc:call(Node, recon, info, [Pid]).
