%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli).

-include("observer_cli.hrl").

%% API
-export([start/0]).
-export([start/1]).
-export([start/2]).

%% for switch views
-export([start_node/2]).

%% for rpc
-export([get_stable_system_info/1]).
-export([get_change_system_info/1]).
-export([get_node_stats_list/2]).
-export([get_ranklist_and_cost_time/6]).
-export([get_reductions_and_msg_queue_len/2]).
-export([get_reductions_and_memory/2]).
-export([get_memory_and_msg_queue_len/2]).

-define(DEFAULT_RANK_NUM, 28). %%fill full in 13.3 inch screen(24 core)
-define(CPU_ALARM_THRESHOLD, 0.8). %% cpu >= this value will be highlight
-define(COUNT_ALARM_THRESHOLD, 0.85). %% port or process reach max_limit * 0.85 will be highlight
-define(FAST_COLLECT_INTERVAL, 0). %% collect should be fast when we push the keyboard to switch mode

-define(STABLE_SYSTEM_ITEM, [system_version, process_limit, smp_support,
  port_limit, ets_limit, logical_processors, multi_scheduling]).

%% @doc observer in erlang shell the refresh time is Milliseconds

-spec start() -> no_return.
start() -> start_node(local_node, #view_opts{}).

-spec start(Node) -> no_return when
  Node:: atom().
start(Node)when is_atom(Node) ->
  case Node == local_node orelse Node == node() of
    true -> start_node(local_node, #view_opts{});
    false ->
      case net_kernel:connect_node(Node) of
        true -> start_node(Node, #view_opts{});
        false -> io:format("remote node ~p(cookie:~p) refuse to be connected ~n", [Node, erlang:get_cookie()]);
        ignored -> io:format("Ignore remote node~p(cookie:~p) connecting~n", [Node, erlang:get_cookie()])
      end
  end.

-spec start(Node, Cookies) -> no_return when
  Node:: atom(),
  Cookies:: atom().
start(Node, Cookie)when is_atom(Node) andalso is_atom(Cookie) ->
  case Node == node() of
    true -> start_node(local_node, #view_opts{});
    false ->
      erlang:set_cookie(Node, Cookie),
      case net_kernel:connect_node(Node) of
        true -> start_node(Node, #view_opts{});
        false -> io:format("remote node ~p(cookie:~p) refuse to be connected ~n", [Node, Cookie]);
        ignored -> io:format("Ignore remote node~p(cookie:~p) connecting~n", [Node, Cookie])
      end
  end.

-spec start_node(Node, Opts) -> no_return when
  Node:: atom(),
  Opts:: tuple().
start_node(Node, Opts)when is_atom(Node) andalso is_record(Opts, view_opts) ->
  ParentPid = self(),
  Tid = ets:new(process_info, [public, set]),
  ChildPid = spawn(fun() -> loop(Tid, ParentPid, Node, Opts#view_opts.home, Opts#view_opts.incr_rows) end),
  waiting(Tid, ChildPid, Node, Opts).

%%for fetching data from remote data by rpc:call/4
-spec get_stable_system_info(atom()) -> list().
get_stable_system_info(local_node) ->
  [begin observer_cli_lib:to_list(erlang:system_info(Item))end ||Item<- ?STABLE_SYSTEM_ITEM];
get_stable_system_info(Node) ->
  rpc:call(Node, ?MODULE, get_stable_system_info, [local_node]).

-spec get_change_system_info(Node)  -> [pos_integer()|pos_integer()|pos_integer()] when
  Node:: atom().
get_change_system_info(local_node) ->
  UsedMem = recon_alloc:memory(used),
  AllocatedMem = recon_alloc:memory(allocated),
  [UsedMem, AllocatedMem, AllocatedMem - UsedMem];
get_change_system_info(Node) ->
  rpc:call(Node, ?MODULE, get_change_system_info, [local_node]).

-spec get_reductions_and_msg_queue_len(Pid, Atom) -> [{reducions, Reductions}|{message_queue_len, Len}]when
  Pid:: pid(),
  Atom:: atom(),
  Reductions:: pos_integer(),
  Len:: pos_integer().
get_reductions_and_msg_queue_len(Pid, local_node) ->
  case recon:info(Pid, [reductions, message_queue_len]) of
    undefined -> [{reductions, "die"}, {message_queue_len, "die"}];
    Info -> Info
  end;
get_reductions_and_msg_queue_len(Pid, Node) ->
  rpc:call(Node, ?MODULE, get_reductions_and_msg_queue_len, [Pid, local_node]).

-spec get_memory_and_msg_queue_len(Pid, Node) -> [{memory, Memory}|{message_queue_len, MessageLen}] when
  Pid:: pid(),
  Node:: atom(),
  Memory:: pos_integer(),
  MessageLen:: pos_integer().
get_memory_and_msg_queue_len(Pid, local_node) ->
  case recon:info(Pid, [memory, message_queue_len]) of
    undefined -> [{memory, "die"}, {message_queue_len, "die"}];
    Info -> Info
  end;
get_memory_and_msg_queue_len(Pid, Node) ->
  rpc:call(Node, ?MODULE, get_memory_and_msg_queue_len, [Pid, local_node]).

-spec get_reductions_and_memory(Pid, Node) -> [{reductions, Reductions}| {memory, Memory}] when
  Pid:: pid(),
  Node:: atom(),
  Reductions:: pos_integer(),
  Memory:: pos_integer().
get_reductions_and_memory(Pid, local_node) ->
  case recon:info(Pid, [reductions, memory]) of
    undefined -> [{reductions, "die"}, {memory, "die"}];
    Info -> Info
  end;
get_reductions_and_memory(Pid, Node) ->
  rpc:call(Node, ?MODULE, get_reductions_and_memory, [Pid, local_node]).

-spec get_node_stats_list(Node, NodeStatsCostTime) -> [{[Absolutes::{atom(), term()}],
  [Increments::{atom(), term()}]}] when
  Node :: atom(),
  NodeStatsCostTime :: pos_integer().
get_node_stats_list(local_node, NodeStatsCostTime) ->
  recon:node_stats_list(1, NodeStatsCostTime);
get_node_stats_list(Node, NodeStatsCostTime) ->
  rpc:call(Node, ?MODULE, get_node_stats_list, [local_node, NodeStatsCostTime]).

-spec get_ranklist_and_cost_time(Node, Func, Type, Interval, IncRows, Time) -> {Status, RemainTime} when
  Node:: atom(),
  Func:: proc_window|proc_count,
  Type:: memory|binary_memory|reductions|total_heap_size|message_queue_len,
  Interval:: pos_integer(),
  IncRows:: integer(),
  Time:: pos_integer(),
  Status:: {pid(),
            Attr::_,
            [Name::atom()
            |{current_function, mfa()}
            |{initial_call, mfa()}, ...]},
  RemainTime:: pos_integer().
get_ranklist_and_cost_time(local_node, proc_window, Type, Interval, IncRows, Time) when Time =/= ?FAST_COLLECT_INTERVAL ->
  RemainTime = 2 * Interval - Time,
  {recon:proc_window(Type, ?DEFAULT_RANK_NUM + IncRows, RemainTime), RemainTime};
get_ranklist_and_cost_time(local_node, _, Type, _Interval, IncRows, _CollectTime) ->
  {recon:proc_count(Type, ?DEFAULT_RANK_NUM + IncRows), 0};
get_ranklist_and_cost_time(Node, Func, Type, Interval, IncRows, CollectTime) ->
  rpc:call(Node, ?MODULE, get_ranklist_and_cost_time, [local_node, Func, Type, Interval, IncRows, CollectTime]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restart_node(Tid, Node, Opts) ->
  ParentPid = self(),
  ChildPid = spawn(fun() -> loop(Tid, ParentPid, Node, Opts#view_opts.home, Opts#view_opts.incr_rows) end),
  waiting(Tid, ChildPid, Node, Opts).

waiting(Tid, ChildPid, Node, Opts) ->
  #view_opts{home = Home = #home{cur_pos = CurPos}} = Opts,
  Input = observer_cli_lib:get_line(""),
  Operate = input_to_operation(Input),

  case  Operate of
    quit -> erlang:send(ChildPid, quit);
    go_to_ets_view ->
      erlang:exit(ChildPid, stop),
      observer_cli_system:start(Node, Opts);
    go_to_allocator_view ->
      erlang:exit(ChildPid, stop),
      observer_cli_allocator:start(Node, Opts);
    go_to_help_view ->
      erlang:exit(ChildPid, stop),
      observer_cli_help:start(Node, Opts);
    go_to_mnesia_view ->
      erlang:exit(ChildPid, stop),
      observer_cli_mnesia:start(Node, Opts);
    pause_or_resume ->
      erlang:send(ChildPid, pause_or_resume),
      waiting(Tid, ChildPid, Node, Opts);
    {Func, Type, no_change} ->
      erlang:exit(ChildPid, stop),
      NewHomeView = Home#home{func = Func, type = Type},
      restart_node(Tid, Node, Opts#view_opts{home = NewHomeView});
    {Func, Type, RefreshInterval} ->
      case string:to_integer(RefreshInterval) of
        {error, no_integer} -> waiting(Tid, ChildPid, Node, Opts);
        {NewInterval, _} when NewInterval >= ?HOME_MIN_INTERVAL ->
          erlang:exit(ChildPid, stop),
          NewHomeView = Home#home{func = Func, type = Type, interval = NewInterval},
          restart_node(Tid, Node, Opts#view_opts{home = NewHomeView});
        {_, _} -> waiting(Tid, ChildPid, Node, Opts)
      end;
    {jump_to_process, Pos} ->
      case string:to_integer(Pos) of
        {error, no_integer} -> waiting(Tid, ChildPid, Node, Opts);
        {NewPos, _} ->
          case ets:lookup(Tid, NewPos) of
            [] -> waiting(Tid, ChildPid, Node, Opts);
            [{_, ChoosePid}] ->
              erlang:exit(ChildPid, stop),
              NewHomeView = Home#home{cur_pos = NewPos},
              observer_cli_process:start(Node, ChoosePid, Opts#view_opts{home = NewHomeView})
          end
      end;
    go_to_process_view ->
      case ets:lookup(Tid, CurPos) of
        [] -> waiting(Tid, ChildPid, Node, Opts);
        [{_, ChoosePid}] ->
          erlang:exit(ChildPid, stop),
          observer_cli_process:start(Node, ChoosePid, Opts)
      end;
    {incr_rows, IncrRows} ->
      case string:to_integer(IncrRows) of
        {error, no_integer} -> waiting(Tid, ChildPid, Node, Opts);
        {NewIncrRows, _} ->
          erlang:exit(ChildPid, stop),
          restart_node(Tid, Node, Opts#view_opts{incr_rows = NewIncrRows})
      end;
    error_input -> waiting(Tid, ChildPid, Node, Opts)
  end.

input_to_operation("q\n") ->  quit;
input_to_operation("p\n") -> pause_or_resume;
input_to_operation("e\n") -> go_to_ets_view;
input_to_operation("a\n") -> go_to_allocator_view;
input_to_operation("h\n") -> go_to_help_view;
input_to_operation("db\n") -> go_to_mnesia_view;

input_to_operation("r\n") -> {proc_count, reductions, no_change};
input_to_operation("b\n") -> {proc_count, binary_memory, no_change};
input_to_operation("t\n") -> {proc_count, total_heap_size, no_change};
input_to_operation("m\n") -> {proc_count, memory, no_change};
input_to_operation("mq\n") -> {proc_count, message_queue_len, no_change};

input_to_operation("rr\n") -> {proc_window, reductions, no_change};
input_to_operation("bb\n") -> {proc_window, binary_memory, no_change};
input_to_operation("tt\n") -> {proc_window, total_heap_size, no_change};
input_to_operation("mm\n") -> {proc_window, memory, no_change};
input_to_operation("mmq\n") -> {proc_window, message_queue_len, no_change};

input_to_operation([$r, $r| RefreshInterval]) -> {proc_window, reductions, RefreshInterval};
input_to_operation([$b, $b| RefreshInterval]) -> {proc_window, binary_memory, RefreshInterval};
input_to_operation([$t, $t| RefreshInterval]) -> {proc_window, total_heap_size, RefreshInterval};
input_to_operation([$m, $m, $q| RefreshInterval]) -> {proc_window, message_queue_len, RefreshInterval};
input_to_operation([$m, $m| RefreshInterval]) -> {proc_window, memory, RefreshInterval};

input_to_operation([$r| RefreshInterval]) -> {proc_count, reductions, RefreshInterval};
input_to_operation([$b| RefreshInterval]) -> {proc_count, binary_memory, RefreshInterval};
input_to_operation([$t| RefreshInterval]) -> {proc_count, total_heap_size, RefreshInterval};
input_to_operation([$m, $q| RefreshInterval]) -> {proc_count, message_queue_len, RefreshInterval};
input_to_operation([$m| RefreshInterval]) -> {proc_count, memory, RefreshInterval};

input_to_operation([$j| Pos]) -> {jump_to_process, Pos};
input_to_operation("\n") -> go_to_process_view;
input_to_operation([$i|IncrRow]) -> {incr_rows, IncrRow};
input_to_operation(_)-> error_input.

loop(Tid, ParentPid, Node, #home{cur_pos = RankPos, func = Func, type = Type, interval = Interval}, IncRows) ->
  observer_cli_lib:clear_screen(),
  StableInfo = get_stable_system_info(Node), %%don't refresh the stable information everytime
  refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, erlang:make_ref(), 0, RankPos, IncRows, running).

%% pause status waiting to be resume
refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, LastTimeRef, _, RankPos, IncRows, pause) ->
  notify_pause_status(),
  erlang:cancel_timer(LastTimeRef),
  receive
    quit -> quit;
    pause_or_resume ->
      observer_cli_lib:clear_screen(),
      refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, LastTimeRef, ?FAST_COLLECT_INTERVAL, RankPos, IncRows, running);
    {Func, Type} ->
      refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, LastTimeRef, ?FAST_COLLECT_INTERVAL, RankPos, IncRows, running)
  end;
%% running status
refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, LastTimeRef, NodeStatsCostTime, RankPos, IncRows, running) ->
  [Version, ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling] = StableInfo,
  erlang:cancel_timer(LastTimeRef),
  [{ProcSum, MemSum}] = get_node_stats_list(Node, NodeStatsCostTime),
  {RankList, RankCostTime} = get_ranklist_and_cost_time(Node, Func, Type, Interval, IncRows, NodeStatsCostTime),
  [UseMemInt, AllocatedMemInt, UnusedMemInt] = get_change_system_info(Node),
  NewNodeStatsCostTime = Interval div 2,
  %% draw
  observer_cli_lib:move_cursor_to_top_line(),
  draw_menu(Func, Type, Interval, Node, IncRows),
  draw_first_line(Version),
  draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling,
    UseMemInt, AllocatedMemInt, UnusedMemInt, ProcSum),
  draw_memory_process_line(ProcSum, MemSum, NewNodeStatsCostTime),
  SchedulerLineLen = draw_scheduler_usage(MemSum),
  PidList = draw_process_rank(Type, RankList, ?DEFAULT_RANK_NUM + IncRows - SchedulerLineLen, Node, RankPos),
  draw_last_line(),

  ets:insert(Tid, PidList),

  TimeRef = refresh_next_time(Func, Type, Interval, RankCostTime, NodeStatsCostTime),
  receive
    quit -> quit;
    pause_or_resume ->
      refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, TimeRef, ?FAST_COLLECT_INTERVAL, RankPos, IncRows, pause);
    {Func, Type} ->
      refresh(Tid, Node, ParentPid, Interval, Func, Type, StableInfo, TimeRef, NewNodeStatsCostTime, RankPos, IncRows, running)
  end.

draw_menu(Func, Type, Interval, Node, IncrRows) ->
  [Home, Ets, Alloc, Mnesia, Help]  = observer_cli_lib:get_menu_title(home),
  RefreshStr = get_refresh_cost_info(Func, Type, Interval, IncrRows),
  UpTime = observer_cli_lib:green(" " ++ observer_cli_lib:uptime(Node)) ++ "|",
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "|", Mnesia, "|", Help, "|", RefreshStr]),
  SpaceLen = ?COLUMN_WIDTH - erlang:length(Title) - erlang:length(UpTime) + 110,
  Space = case SpaceLen > 0 of  true -> lists:duplicate(SpaceLen, " "); false -> [] end,
  io:format("~s~n", [Title ++ Space ++ UpTime]).

draw_first_line(Version) -> io:format("|~-131.131s|~n", [Version -- "\n"]).

%System     | Count/Limit        | System Switch      | State                  | Memory Info          | Megabyte                 |
%Proc Count | 42/262144          | Smp Support        | true                   | Allocted Mem         | 32.0698M           100.0%|
%Port Count | 6/65536            | Multi Scheduling   | enabled                | Use Mem              | 19.0814M           60.59%|
%Ets Limit  | 2053               | Logical Processors | 4                      | Unuse Mem            | 12.0537M           38.34%|
draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling,
    UseMemInt, AllocatedMemInt, UnusedMemInt, ProcSum) ->
  UseMem = observer_cli_lib:to_megabyte_str(UseMemInt),
  AllocatedMem = observer_cli_lib:to_megabyte_str(AllocatedMemInt),
  UnUsedMem = observer_cli_lib:to_megabyte_str(UnusedMemInt),
  UsePercent = observer_cli_lib:float_to_percent_with_two_digit(UseMemInt/AllocatedMemInt),
  UnUsePercent = observer_cli_lib:float_to_percent_with_two_digit(UnusedMemInt/AllocatedMemInt),
  {ProcFormat, ProcCount, PortFormat, PortCount} = get_port_proc_count_info(PortLimit, ProcLimit, ProcSum),
  io:format("|\e[0m\e[44m~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-26.26s\e[49m|~n",
    ["System ", "Count/Limit", "System Switch", "State", "Memory Info", "Megabyte"]),
  io:format(ProcFormat,
    ["Proc Count", ProcCount, "Smp Support", SmpSupport, "Allocted Mem", AllocatedMem]),
  io:format(PortFormat,
    ["Port Count", PortCount, "Multi Scheduling", MultiScheduling, "Use Mem", UseMem, UsePercent]),
  io:format("|~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s~6.6s|~n",
    ["Ets Limit", EtsLimit, "Logical Processors", LogicalProc, "Unuse Mem", UnUsedMem, UnUsePercent]).

%Memory     | Megabyte           | Process State      | Count                  | Memory               | State                    |
%Total      | 19.0999M       100%| Binary             | 0.0182M          00.91%| IO Output            | 0.0000M                  |
%Process    | 5.0683M      28.41%| Code               | 7.0150M          35.75%| IO Input             | 0.0000M                  |
%Atom       | 0.0301M      01.50%| Reductions         | 176686                 | Run Queue            | 0                        |
%Ets        | 0.0697M      03.49%| Gc Count           | 2                      | Gc Words Reclaimed   | 10173                    |
draw_memory_process_line(ProcSum, MemSum, Interval) ->
  TotalMemInt = proplists:get_value(memory_total, ProcSum),
  TotalMem = observer_cli_lib:to_megabyte_str(TotalMemInt),
  ProcMemInt = proplists:get_value(memory_procs, ProcSum),
  ProcMem = observer_cli_lib:to_megabyte_str(ProcMemInt),
  ProcMemPercent = observer_cli_lib:float_to_percent_with_two_digit(ProcMemInt/TotalMemInt),
  AtomMemInt = proplists:get_value(memory_atoms, ProcSum),
  AtomMem = observer_cli_lib:to_megabyte_str(AtomMemInt),
  AtomMemPercent = observer_cli_lib:float_to_percent_with_two_digit(AtomMemInt/TotalMemInt),
  BinMemInt = proplists:get_value(memory_bin, ProcSum),
  BinMem = observer_cli_lib:to_megabyte_str(BinMemInt),
  BinMemPercent = observer_cli_lib:float_to_percent_with_two_digit(BinMemInt/TotalMemInt),
  CodeMemInt = erlang:memory(code),
  CodeMem = observer_cli_lib:to_megabyte_str(CodeMemInt),
  CodeMemPercent = observer_cli_lib:float_to_percent_with_two_digit(CodeMemInt/TotalMemInt),
  EtsMemInt = proplists:get_value(memory_ets, ProcSum),
  EtsMem = observer_cli_lib:to_megabyte_str(EtsMemInt),
  EtsMemPercent = observer_cli_lib:float_to_percent_with_two_digit(EtsMemInt/TotalMemInt),
  RunQueue = integer_to_list(proplists:get_value(run_queue, ProcSum)),
  BytesIn = observer_cli_lib:to_megabyte_str(proplists:get_value(bytes_in, MemSum)),
  BytesOut = observer_cli_lib:to_megabyte_str(proplists:get_value(bytes_out, MemSum)),
  GcCount = observer_cli_lib:to_list(proplists:get_value(gc_count, MemSum)),
  GcWordsReclaimed = observer_cli_lib:to_list(proplists:get_value(gc_words_reclaimed, MemSum)),
  Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),
  io:format("|\e[0m\e[44m~-10.10s | ~-19.19s | ~-18.18s | ~-23.23s | ~-20.20s | ~-26.26s\e[49m|~n", %%cyan background
    ["Memory", "State", "Memory ", "State", "Memory", "Interval: " ++ integer_to_list(Interval) ++ "ms"]),
  io:format("|~-10.10s | ~-14.14s~6.6s| ~-18.18s | ~-18.18s~6.6s| ~-20.20s | ~-26.26s|~n",
    ["Total", TotalMem, "100%", "Binary", BinMem, BinMemPercent, "IO Output", BytesOut]),
  io:format("|~-10.10s | ~-14.14s~6.6s| ~-18.18s | ~-18.18s~6.6s| ~-20.20s | ~-26.26s|~n",
    ["Process", ProcMem, ProcMemPercent, "Code", CodeMem, CodeMemPercent, "IO Input", BytesIn]),
  io:format("|~-10.10s | ~-14.14s~6.6s| ~-18.18s | ~-23.23s | ~-20.20s | ~-26.26s|~n",
    ["Atom", AtomMem, AtomMemPercent, "Reductions", Reductions, "Gc Count", GcCount]),
  io:format("|~-10.10s | ~-14.14s~6.6s| ~-18.18s | ~-23.23s | ~-20.20s | ~-26.26s|~n",
    ["Ets", EtsMem, EtsMemPercent, "Run Queue", RunQueue, "Gc Words Reclaimed", GcWordsReclaimed]).

%|01[|||||||||||||||||||||||||||||||                     59.66%]  |03[||||||||||                                          19.59%]|
%|02[|||||||||||||||||||||||||||||||                     61.02%]  |04[|||||||                                             14.44%]|
draw_scheduler_usage(MemSum) ->
  SchedulerUsage = proplists:get_value(scheduler_usage, MemSum),
  SchedulerNum = erlang:length(SchedulerUsage),
  draw_scheduler_usage(SchedulerUsage, SchedulerNum).

%% < 24 core will split 2 part
draw_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum < 24 ->
  HalfSchedulerNum = SchedulerNum div 2,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + HalfSchedulerNum, SchedulerUsage),
     CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
     CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + HalfSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 54), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 54), "|"),
     Format = cpu_format_alarm_color(Percent1, Percent2),
     io:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
   end|| Seq <- lists:seq(1, HalfSchedulerNum)],
  HalfSchedulerNum;
%% >= 24 will split 3 part
draw_scheduler_usage(SchedulerUsage, SchedulerNum) ->
  PosSchedulerNum = SchedulerNum div 3,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + PosSchedulerNum, SchedulerUsage),
     Percent3 = proplists:get_value(Seq + 2 * PosSchedulerNum, SchedulerUsage),
     CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
     CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
     CPU3 = observer_cli_lib:float_to_percent_with_two_digit(Percent3),
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + PosSchedulerNum])),
     CPUSeq3 = lists:flatten(io_lib:format("~2..0w", [Seq + 2 * PosSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 32), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 32), "|"),
     Process3 = lists:duplicate(trunc(Percent2 * 32), "|"),
     Format = cpu_format_alarm_color(Percent1, Percent2, Percent3),
     io:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2, CPUSeq3, Process3, CPU3])
   end|| Seq <- lists:seq(1, PosSchedulerNum)],
  PosSchedulerNum.

%| Pid            | Reductions| Name or Initial Call        |    Memory|Msg Queue |Current Function                              |
%| <0.3.0>        |     220329| erl_prim_loader             |    220329|0         |erl_prim_loader:loop/3                        |
%| <0.26.0>       |     177050| group:server/3              |    177897|0         |group:more_data/5                             |
%| <0.12.0>       |     124753| code_server                 |    124753|0         |code_server:loop/1                            |
%| <0.24.0>       |      77505| user_drv                    |     78193|0         |user_drv:server_loop/6                        |
%| <0.27.0>       |      10036| erlang:apply/2              |     10036|0         |shell:shell_rep/4                             |
%| <0.0.0>        |       3685| init                        |      3685|0         |init:loop/1                                   |
%| <0.11.0>       |       2016| kernel_sup                  |      2016|0         |gen_server:loop/6                             |
%| <0.33.0>       |        564| erlang:apply/2              |       564|0         |io:execute_request/2                          |
%| <0.7.0>        |        404| application_controller      |       404|0         |gen_server:loop/6                             |
%| <0.18.0>       |        240| inet_db                     |       240|0         |gen_server:loop/6                             |
%| <0.28.0>       |        233| proc_lib:init_p/5           |       233|0         |gen_server:loop/6                             |
%| <0.6.0>        |        227| error_logger                |       227|0         |gen_event:fetch_msg/5                         |
%| <0.20.0>       |         89| file_server_2               |        89|0         |gen_server:loop/6                             |
%| <0.23.0>       |         87| proc_lib:init_p/5           |        87|0         |gen_server:loop/6                             |
%| <0.10.0>       |         73| application_master:start_it/|        73|0         |application_master:loop_it/4                  |
%| <0.29.0>       |         56| kernel_safe_sup             |        56|0         |gen_server:loop/6                             |
%| <0.19.0>       |         53| global_group                |        53|0         |gen_server:loop/6                             |
%| <0.15.0>       |         45| global_name_server          |        45|0         |gen_server:loop/6                             |
%| <0.9.0>        |         44| proc_lib:init_p/5           |        44|0         |application_master:main_loop/2                |
%| <0.25.0>       |         36| user                        |        36|0         |group:server_loop/3                           |
draw_process_rank(memory, MemoryList, Num, Node, RankPos) ->
  io:format("\e[0m\e[44m|Pos|~-12.12s|~11.11s| ~-30.30s|~11.11s|~-10.10s|~-47.47s\e[49m|~n", %%cyan background
    ["Pid", "Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid, Node),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_initial_call(IsName, InitialCall),
     Format = get_choose_format(RankPos, Pos),
     io:format(Format,
       [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
     {Pos, Pid}
   end|| Pos <- lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))];
draw_process_rank(binary_memory, MemoryList, Num, Node, RankPos) ->
  io:format("\e[0m\e[44m|Pos|~-12.12s|~11.11s| ~-30.30s|~11.11s|~-10.10s|~-47.47s\e[49m|~n", %%cyan background
    ["Pid", "Bin Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid, Node),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_initial_call(IsName, InitialCall),
     Format = get_choose_format(RankPos, Pos),
     io:format(Format,
       [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
     {Pos, Pid}
   end|| Pos <- lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))];
draw_process_rank(reductions, ReductionList, Num, Node, RankPos) ->
  io:format("\e[0m\e[44m|Pos|~-12.12s|~11.11s| ~-30.30s|~10.10s|~-11.11s|~-47.47s\e[49m|~n", %%cyan background
    ["Pid", "Reductions", "Name or Initial Call", "Memory", "Msg Queue", "Current Function"]),
  [begin
     {Pid, Reductions, Call = [IsName|_]} = lists:nth(Pos, ReductionList),
     [{_, Memory}, {_, MsgQueueLen}] = get_memory_and_msg_queue_len(Pid, Node),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_initial_call(IsName, InitialCall),
     Format = get_choose_format(RankPos, Pos),
     io:format(Format,
       [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(Reductions), NameOrCall,
         observer_cli_lib:to_list(Memory), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
     {Pos, Pid}
   end|| Pos <- lists:seq(1, erlang:min(Num, erlang:length(ReductionList)))];
draw_process_rank(total_heap_size, HeapList, Num, Node, RankPos) ->
  io:format("\e[0m\e[44m|Pos|~-12.12s|~11.11s| ~-30.30s|~11.11s|~-10.10s|~-47.47s\e[49m|~n", %%cyan background
    ["Pid", "Total Heap Size", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, HeapSize, Call = [IsName|_]} = lists:nth(Pos, HeapList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid, Node),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_initial_call(IsName, InitialCall),
     Format = get_choose_format(RankPos, Pos),
     io:format(Format,
       [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(HeapSize), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
     {Pos, Pid}
   end|| Pos <- lists:seq(1, erlang:min(Num, erlang:length(HeapList)))];
draw_process_rank(message_queue_len, MQLenList, Num, Node, RankPos) ->
  io:format("\e[0m\e[44m|Pos|~-12.12s|~11.11s| ~-30.30s|~10.10s|~-11.11s|~-47.47s\e[49m|~n", %%cyan background
    ["Pid", "Msg Queue", "Name or Initial Call", "Memory", "Reductions", "Current Function"]),
  [begin
     {Pid, MQLen, Call = [IsName|_]} = lists:nth(Pos, MQLenList),
     [{_, Reductions}, {_, Memory}]= get_reductions_and_memory(Pid, Node),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_initial_call(IsName, InitialCall),
     Format = get_choose_format(RankPos, Pos),
     io:format(Format,
       [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MQLen), NameOrCall,
         observer_cli_lib:to_list(Memory), observer_cli_lib:to_list(Reductions), CurFun]),
     {Pos, Pid}
   end|| Pos <- lists:seq(1, erlang:min(Num, erlang:length(MQLenList)))].

draw_last_line() ->
  io:format("|\e[0m\e[44m~s~s~s~s~s~s~s~s\e[49m|~n",
    ["q(quit) ", "p(pause) ", "r/rr(reduction) ",
      "m/mm(memory) ", "b/bb(binary memory) ", "t/tt(total heap size) ", "mq/mmq(message queue) ", "j9(jump to process 9)"]).

notify_pause_status() ->
  io:format("\e[31;1m PAUSE  INPUT (p, r/rr, b/bb, t/tt, m/mm) to resume or q to quit \e[0m~n").

get_choose_format(Pos, Pos) ->
  "|\e[33;1m~-3.3s|~-12.12s|~11.11s| ~-30.30s|~11.11s|~-10.10s|~-47.47s\e[0m|~n";
get_choose_format(_Pos, _RankPos) ->
  "|~-3.3s|~-12.12s|~11.11s| ~-30.30s|~11.11s|~-10.10s|~-47.47s|~n".

refresh_next_time(proc_count, Type, Interval, RankCostTime, NodeStatsCostTime) ->
  erlang:send_after(Interval - RankCostTime - NodeStatsCostTime, self(), {proc_count, Type});
refresh_next_time(proc_window, Type, _Interval, _RankCostTime, _CollectTime) ->
  erlang:send_after(100, self(), {proc_window, Type}).

get_current_initial_call(Call) ->
  {_, CurFun} = lists:keyfind(current_function, 1, Call),
  {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
  {observer_cli_lib:mfa_to_list(CurFun), observer_cli_lib:mfa_to_list(InitialCall)}.

get_port_proc_count_info(PortLimit, ProcLimit, ProcSum) ->
  ProcCountInt = proplists:get_value(process_count, ProcSum),
  PortLimitInt = list_to_integer(PortLimit),
  ProcLimitInt = list_to_integer(ProcLimit),
  PortCountInt = erlang:system_info(port_count),
  PortCount = integer_to_list(PortCountInt) ++ "/" ++ PortLimit,
  ProcCount = integer_to_list(ProcCountInt) ++ "/" ++ ProcLimit,
  {ProcFormat, PortFormat} =
    count_format_alarm_color(PortLimitInt * ?COUNT_ALARM_THRESHOLD, PortCountInt,
      ProcLimitInt * ?COUNT_ALARM_THRESHOLD, ProcCountInt),
  {ProcFormat, ProcCount, PortFormat, PortCount}.

cpu_format_alarm_color(Percent1, Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[31m|~-2.2s[~-54.54s\e[0m\e[41m~s\e[49m]\e[31m |~-2.2s[~-54.54s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, _Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[31m|~-2.2s[~-54.54s\e[0m\e[41m~s\e[49m]\e[32m |~-2.2s[~-54.54s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, Percent2)when Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[32m|~-2.2s[~-54.54s\e[0m\e[42m~s\e[49m]\e[31m |~-2.2s[~-54.54s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, _Percent2) ->
  "|\e[32m|~-2.2s[~-54.54s\e[0m\e[42m~s\e[49m]\e[32m |~-2.2s[~-54.54s\e[0m\e[42m~s\e[49m]|~n".

cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[31m | ~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[31m | ~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[31m | ~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3) when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[32m | ~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[32m | ~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[31m | ~2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-32.32s\e[0m\e[41m~s\e[49m]\e[32m | ~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3) when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[32m | ~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-32.32s\e[0m\e[42m~s\e[49m]|~n".

count_format_alarm_color(PortThreshold, PortCount, ProcThreshold, ProcCount)when PortCount > PortThreshold
  andalso ProcCount > ProcThreshold ->
  {"|~-10.10s | \e[31m~-20.20s\e[0m| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s100.0%|~n",
    "|~-10.10s | \e[31m~-20.20s\e[0m| ~-18.18s | ~-20.20s | ~-23.23s | ~-20.20s~6.6s|~n"
  };
count_format_alarm_color(PortThreshold, PortCount, _ProcThreshold, _ProcCount)when PortCount > PortThreshold ->
  {"|~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s100.0%|~n",
    "|~-10.10s | \e[31m~-20.20s\e[0m| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, ProcThreshold, ProcCount)when ProcCount > ProcThreshold ->
  {"|~-10.10s | \e[31m~-19.19s\e[0m| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s100.0%|~n",
    "|~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, _ProcThreshold, _ProcCount) ->
  {"|~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s100.0%|~n",
    "|~-10.10s | ~-20.20s| ~-18.18s | ~-23.23s | ~-20.20s | ~-20.20s~6.6s|~n"
  }.

display_name_or_initial_call(IsName, _Call)when is_atom(IsName) -> atom_to_list(IsName);
display_name_or_initial_call(_IsName, Call) -> Call.

get_refresh_cost_info(proc_count, Type, Interval, IncrRow) ->
  io_lib:format("recon:proc_count(~s,~w,0) Refresh:~wms", [atom_to_list(Type), IncrRow + ?DEFAULT_RANK_NUM, Interval]);
get_refresh_cost_info(proc_window, Type, Interval, IncrRow) ->
  io_lib:format("recon:proc_window(~s,~w,~w) Refresh:~wms",
    [atom_to_list(Type), IncrRow + ?DEFAULT_RANK_NUM, Interval*2 - Interval div 2, Interval*2]).
