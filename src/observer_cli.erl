%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli).

-export([start/0]).
-export([start/1]).
-export([system/0]).
-export([allocator/0]).
-export([table/0]).
-export([help/0]).

%% @doc a top tool in erlang shell the reflushtime is Milliseconds
-define(TOP_MIN_REFRESH_INTERVAL, 2000).

-spec start() -> quit.
start() -> start(?TOP_MIN_REFRESH_INTERVAL).
-spec start(pos_integer()) -> quit.
start(RefreshMillSecond)when RefreshMillSecond >= ?TOP_MIN_REFRESH_INTERVAL ->
  ParentPid = self(),
  ChildPid = spawn_link(fun() -> loop(RefreshMillSecond, ParentPid) end),
  waiting(ChildPid, RefreshMillSecond).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system
-spec system() -> quit.
system() -> observer_cli_system:start().

%% @doc List Memory Allocators: std, ll, eheap, ets, fix, binary, driver.
-spec allocator() -> quit.
allocator() -> observer_cli_allocator:start().

%% @doc List include all metrics in observer's Table Viewer.
-spec table()-> ok.
table() -> observer_cli_table:start().

%% @doc Parameter description
-spec help() -> quit.
help() -> observer_cli_help:start().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(BROAD, 129).
-define(DEFAULT_RANK_NUM, 20). %%fill full in 13.3 inch screen(24 core)
-define(CPU_ALARM_THRESHOLD, 0.8). %% cpu >= will be highlight
-define(COUNT_ALARM_THRESHOLD, 0.85). %% port or process reach max_limit * 0.85 will be highlight
-define(FAST_COLLECT_INTENAL, 0). %% collect should be fast when we push the keyboard to switch mode

-define(STABLE_SYSTEM_ITEM, [system_version, process_limit, smp_support,
  port_limit, ets_limit, logical_processors, multi_scheduling]).
-define(CHANGE_SYSTEM_ITEM, [used, allocated, unused]).

waiting(Pid, Interval) ->
  Input = io:get_line(""),
  Operate = input_to_operation(Input),
  case  Operate of
    quit -> erlang:send(Pid, quit);
    go_to_ets_view ->
      erlang:send(Pid, go_to_ets_view),
      waiting_last_draw_done_to_other_view(Interval);
    go_to_allocator_view ->
      erlang:send(Pid, go_to_allocator_view),
      waiting_last_draw_done_to_other_view(Interval);
    go_to_help_view ->
      erlang:send(Pid, go_to_help_view),
      waiting_last_draw_done_to_other_view(Interval);
    pause_or_resume ->
      erlang:send(Pid, pause_or_resume),
      waiting(Pid, Interval);
    error_input -> waiting(Pid, Interval);
    {Func, Type, no_change} ->
      erlang:send(Pid, {Func, Type}),
      waiting(Pid, Interval);
    {Func, Type, RefreshInterval} ->
      case string:to_integer(RefreshInterval) of
        {error, no_integer} -> waiting(Pid, Interval);
        {NewInterval, _} when NewInterval >= ?TOP_MIN_REFRESH_INTERVAL ->
          erlang:send(Pid, {Func, Type, NewInterval}),
          waiting(Pid, NewInterval);
        {_, _} -> waiting(Pid, Interval)
      end
  end.

waiting_last_draw_done_to_other_view(Interval) ->
  receive
    draw_work_done_to_ets_view ->
      observer_cli_system:start();
    draw_work_done_to_allocator_view ->
      observer_cli_allocator:start();
    draw_work_done_to_help_view ->
      observer_cli_help:start()
  after Interval -> time_out
  end.

input_to_operation("q\n") ->  quit;
input_to_operation("p\n") -> pause_or_resume;
input_to_operation("e\n") -> go_to_ets_view;
input_to_operation("a\n") -> go_to_allocator_view;
input_to_operation("h\n") -> go_to_help_view;
input_to_operation([$r, $:| RefreshInteral]) -> {proc_count, reductions, RefreshInteral};
input_to_operation([$b, $:| RefreshInteral]) -> {proc_count, binary_memory, RefreshInteral};
input_to_operation([$t, $:| RefreshInteral]) -> {proc_count, total_heap_size, RefreshInteral};
input_to_operation([$m, $:| RefreshInteral]) -> {proc_count, memory, RefreshInteral};
input_to_operation("r\n") -> {proc_count, reductions, no_change};
input_to_operation("b\n") -> {proc_count, binary_memory, no_change};
input_to_operation("t\n") -> {proc_count, total_heap_size, no_change};
input_to_operation("m\n") -> {proc_count, memory, no_change};
input_to_operation([$r, $r, $:| RefreshInteral]) -> {proc_window, reductions, RefreshInteral};
input_to_operation([$b, $b, $:| RefreshInteral]) -> {proc_window, binary_memory, RefreshInteral};
input_to_operation([$t, $t, $:| RefreshInteral]) -> {proc_window, total_heap_size, RefreshInteral};
input_to_operation([$m, $m, $:| RefreshInteral]) -> {proc_window, memory, RefreshInteral};
input_to_operation("rr\n") -> {proc_window, reductions, no_change};
input_to_operation("bb\n") -> {proc_window, binary_memory, no_change};
input_to_operation("tt\n") -> {proc_window, total_heap_size, no_change};
input_to_operation("mm\n") -> {proc_window, memory, no_change};
input_to_operation(_)-> error_input.

loop(Interal, ParentPid) ->
  observer_cli_lib:clear_screen(),
  StableInfo = get_stable_system_info(), %%don't need reflush the stable info everytime
  refresh(ParentPid, Interal, proc_count, memory, StableInfo, erlang:make_ref(), 0, running).

%% refresh  UI
refresh(ParentPid, Interal, Func, Type, StableInfo, LastTimeRef, _, pause) ->
  notify_pause_status(),
  erlang:cancel_timer(LastTimeRef),
  receive
    quit -> quit;
    go_to_ets_view ->  erlang:send(ParentPid, draw_work_done_to_ets_view), quit;
    go_to_allocator_view -> erlang:send(ParentPid, draw_work_done_to_allocator_view), quit;
    go_to_help_view ->  erlang:send(ParentPid, draw_work_done_to_help_view), quit;
    pause_or_resume ->
      observer_cli_lib:clear_screen(),
      refresh(ParentPid, Interal, Func, Type, StableInfo, LastTimeRef, ?FAST_COLLECT_INTENAL, running);
    {NewFunc, NewType, NewInteral} ->
      observer_cli_lib:clear_screen(),
      refresh(ParentPid, NewInteral, NewFunc, NewType, StableInfo, LastTimeRef, ?FAST_COLLECT_INTENAL, running);
    {NewFunc, NewType} ->
      observer_cli_lib:clear_screen(),
      refresh(ParentPid, Interal, NewFunc, NewType, StableInfo, LastTimeRef, ?FAST_COLLECT_INTENAL, running)
  end;
refresh(ParentPid, Interal, Func, Type, StableInfo, LastTimeRef, NodeStatsCostTime, running) ->
  [Version, ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling] = StableInfo,
  erlang:cancel_timer(LastTimeRef),
  [{ProcSum, MemSum}] = recon:node_stats_list(1, NodeStatsCostTime), %%alway get node stats before ranklist
  {RankList, RankCostTime} = get_ranklist_and_cost_time(Func, Type, Interal, NodeStatsCostTime),
  [UseMemInt, AlloctedMemInt, UnusedMemInt] = get_change_system_info(),
  NewNodeStatsCostTime = Interal div 2,
  %% draw
  observer_cli_lib:move_cursor_to_top_line(),
  draw_menu(Func, Type, Interal),
  draw_first_line(Version),
  draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling,
    UseMemInt, AlloctedMemInt, UnusedMemInt, ProcSum),
  draw_memory_process_line(ProcSum, MemSum, NewNodeStatsCostTime),
  draw_scheduler_usage(MemSum),
  draw_process_rank(Type, RankList, ?DEFAULT_RANK_NUM),
  draw_last_line(),

  TimeRef = refresh_next_time(Func, Type, Interal, RankCostTime, NodeStatsCostTime),
  receive
    quit -> quit;
    go_to_ets_view ->  erlang:send(ParentPid, draw_work_done_to_ets_view), quit;
    go_to_allocator_view ->  erlang:send(ParentPid, draw_work_done_to_allocator_view), quit;
    go_to_help_view ->  erlang:send(ParentPid, draw_work_done_to_help_view), quit;
    pause_or_resume -> refresh(ParentPid, Interal, Func, Type, StableInfo, TimeRef, ?FAST_COLLECT_INTENAL, pause);
    {Func, Type} -> refresh(ParentPid, Interal, Func, Type, StableInfo, TimeRef, NewNodeStatsCostTime, running);
    {NewFunc, NewType} -> refresh(ParentPid, Interal, NewFunc, NewType, StableInfo, TimeRef, ?FAST_COLLECT_INTENAL, running);
    {NewFunc, NewType, NewInteral} -> refresh(ParentPid, NewInteral, NewFunc, NewType, StableInfo, TimeRef, ?FAST_COLLECT_INTENAL, running)
  end.

draw_menu(Func, Type, Interal) ->
  [Home, Ets, Alloc, Help]  = observer_cli_lib:get_menu_title(home),
  RefreshStr = get_refresh_cost_info(Func, Type, Interal),
  UpTime = observer_cli_lib:green(" " ++ observer_cli_lib:uptime()) ++ "|",
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Help, "|", RefreshStr]),
  Space = lists:duplicate(?BROAD - erlang:length(Title) - erlang:length(UpTime) + 90, " "),
  io:format("~s~n", [Title ++ Space ++ UpTime]).

draw_first_line(Version) -> io:format("|~-127.127s|~n", [Version -- "\n"]).

%System     | Count/Limit        | System Switch      | State                  | Memory Info          | Megabyte                 |
%Proc Count | 42/262144          | Smp Support        | true                   | Allocted Mem         | 32.0698M           100.0%|
%Port Count | 6/65536            | Multi Scheduling   | enabled                | Use Mem              | 19.0814M           60.59%|
%Ets Limit  | 2053               | Logical Processors | 4                      | Unuse Mem            | 12.0537M           38.34%|
draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling,
    UseMemInt, AlloctedMemInt, UnusedMemInt, ProcSum) ->
  UseMem = observer_cli_lib:to_megabyte_list(UseMemInt),
  AlloctedMem = observer_cli_lib:to_megabyte_list(AlloctedMemInt),
  UnunsedMem = observer_cli_lib:to_megabyte_list(UnusedMemInt),
  UsePrce = observer_cli_lib:float_to_percent_with_two_digit(UseMemInt/AlloctedMemInt),
  UnusePrce = observer_cli_lib:float_to_percent_with_two_digit(UnusedMemInt/AlloctedMemInt),
  {ProcFormat, ProcCount, PortFormat, PortCount} = get_port_proc_count_info(PortLimit, ProcLimit, ProcSum),
  io:format("|\e[46m~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s\e[49m|~n", %%cyan background
    ["System ", "Count/Limit", "System Switch", "State", "Memory Info", "Megabyte"]),
  io:format(ProcFormat,
    ["ProcCount", ProcCount, "Smp Support", SmpSupport, "Allocted Mem", AlloctedMem]),
  io:format(PortFormat,
    ["PortCount", PortCount, "Multi Scheduling", MultiScheduling, "Use Mem", UseMem, UsePrce]),
  io:format("|~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n",
    ["Ets Limit", EtsLimit, "Logical Processors", LogicalProc, "Unuse Mem", UnunsedMem, UnusePrce]).

%Memory     | Megabyte           | Process State      | Count                  | Memory               | State                    |
%Total      | 19.0999M       100%| Binary             | 0.0182M          00.91%| IO Output            | 0.0000M                  |
%Process    | 5.0683M      28.41%| Code               | 7.0150M          35.75%| IO Input             | 0.0000M                  |
%Atom       | 0.0301M      01.50%| Reductions         | 176686                 | Run Queue            | 0                        |
%Ets        | 0.0697M      03.49%| Gc Count           | 2                      | Gc Words Reclaimed   | 10173                    |
draw_memory_process_line(ProcSum, MemSum, Interal) ->
  TotalMemInt = proplists:get_value(memory_total, ProcSum),
  TotalMem = observer_cli_lib:to_megabyte_list(TotalMemInt),
  ProcMemInt = proplists:get_value(memory_procs, ProcSum),
  ProcMem = observer_cli_lib:to_megabyte_list(ProcMemInt),
  ProcMemPerc = observer_cli_lib:float_to_percent_with_two_digit(ProcMemInt/TotalMemInt),
  AtomMemInt = proplists:get_value(memory_atoms, ProcSum),
  AtomMem = observer_cli_lib:to_megabyte_list(AtomMemInt),
  AtomMemPerc = observer_cli_lib:float_to_percent_with_two_digit(AtomMemInt/TotalMemInt),
  BinMemInt = proplists:get_value(memory_bin, ProcSum),
  BinMem = observer_cli_lib:to_megabyte_list(BinMemInt),
  BinMemPerc = observer_cli_lib:float_to_percent_with_two_digit(BinMemInt/TotalMemInt),
  CodeMemInt = erlang:memory(code),
  CodeMem = observer_cli_lib:to_megabyte_list(CodeMemInt),
  CodeMemPerc = observer_cli_lib:float_to_percent_with_two_digit(CodeMemInt/TotalMemInt),
  EtsMemInt = proplists:get_value(memory_ets, ProcSum),
  EtsMem = observer_cli_lib:to_megabyte_list(EtsMemInt),
  EtsMemPerc = observer_cli_lib:float_to_percent_with_two_digit(EtsMemInt/TotalMemInt),
  Runqueue = integer_to_list(proplists:get_value(run_queue, ProcSum)),
  BytesIn = observer_cli_lib:to_megabyte_list(proplists:get_value(bytes_in, MemSum)),
  BytesOut = observer_cli_lib:to_megabyte_list(proplists:get_value(bytes_out, MemSum)),
  GcCount = observer_cli_lib:to_list(proplists:get_value(gc_count, MemSum)),
  GcWordsReclaimed = observer_cli_lib:to_list(proplists:get_value(gc_words_reclaimed, MemSum)),
  Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),
  io:format("|\e[46m~-9.9s | ~-18.18s | ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s\e[49m|~n", %%cyan background
    ["Memory", "State", "Memory ", "State", "Memory", "Interval: " ++ integer_to_list(Interal) ++ "ms"]),
  io:format("|~-9.9s | ~-13.13s~6.6s| ~-18.18s | ~-17.17s~6.6s| ~-20.20s | ~-25.25s|~n",
    ["Total", TotalMem, "100%", "Binary", BinMem, BinMemPerc, "IO Output", BytesOut]),
  io:format("|~-9.9s | ~-13.13s~6.6s| ~-18.18s | ~-17.17s~6.6s| ~-20.20s | ~-25.25s|~n",
    ["Process", ProcMem, ProcMemPerc, "Code", CodeMem, CodeMemPerc, "IO Input", BytesIn]),
  io:format("|~-9.9s | ~-13.13s~6.6s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s|~n",
    ["Atom", AtomMem, AtomMemPerc, "Reductions", Reductions, "Gc Count", GcCount]),
  io:format("|~-9.9s | ~-13.13s~6.6s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s|~n",
    ["Ets", EtsMem, EtsMemPerc, "Run Queue", Runqueue, "Gc Words Reclaimed", GcWordsReclaimed]).

%|01[|||||||||||||||||||||||||||||||                     59.66%]  |03[||||||||||                                          19.59%]|
%|02[|||||||||||||||||||||||||||||||                     61.02%]  |04[|||||||                                             14.44%]|
draw_scheduler_usage(MemSum) ->
  SchedulerUsage = proplists:get_value(scheduler_usage, MemSum),
  SchedulerNum = erlang:length(SchedulerUsage),
  draw_scheduler_user(SchedulerUsage, SchedulerNum).

%% < 24 core will split 2 part
draw_scheduler_user(SchedulerUsage, SchedulerNum) when SchedulerNum < 24 ->
  HalfSchedulerNum = SchedulerNum div 2,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + HalfSchedulerNum, SchedulerUsage),
     CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
     CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + HalfSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 52), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 52), "|"),
     Format = cpu_format_alarm_color(Percent1, Percent2),
     io:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
   end|| Seq <- lists:seq(1, HalfSchedulerNum)];

%% >= 24 will split 3 part
draw_scheduler_user(SchedulerUsage, SchedulerNum) ->
  PotSchedulerNum = SchedulerNum div 3,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + PotSchedulerNum, SchedulerUsage),
     Percent3 = proplists:get_value(Seq + 2 * PotSchedulerNum, SchedulerUsage),
     CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
     CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
     CPU3 = observer_cli_lib:float_to_percent_with_two_digit(Percent3),
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + PotSchedulerNum])),
     CPUSeq3 = lists:flatten(io_lib:format("~2..0w", [Seq + 2 * PotSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 31), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 31), "|"),
     Process3 = lists:duplicate(trunc(Percent2 * 31), "|"),
     Format = cpu_format_alarm_color(Percent1, Percent2, Percent3),
     io:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2, CPUSeq3, Process3, CPU3])
   end|| Seq <- lists:seq(1, PotSchedulerNum)].

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
draw_process_rank(memory, MemoryList, Num) ->
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s\e[49m|~n", %%cyan background
    ["Pid", "Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_inital_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s|~n",
       [pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(binary_memory, MemoryList, Num) ->
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s\e[49m|~n", %%cyan background
    ["Pid", "Bin Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_inital_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s|~n",
       [pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(reductions, ReductionList, Num) ->
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-11.11s|~-45.45s\e[49m|~n", %%cyan background
    ["Pid", "Reductions", "Name or Initial Call", "Memory", "Msg Queue", "Current Function"]),
  [begin
     {Pid, Reductions, Call = [IsName|_]} = lists:nth(Pos, ReductionList),
     [{_, Memory}, {_, MsgQueueLen}] = get_memory_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_inital_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-11.11s|~-45.45s|~n",
       [pid_to_list(Pid), observer_cli_lib:to_list(Reductions), NameOrCall,
         observer_cli_lib:to_list(Memory), observer_cli_lib:to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(total_heap_size, HeapList, Num) ->
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s\e[49m|~n", %%cyan background
    ["Pid", "Total Heap Size", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, HeapSize, Call = [IsName|_]} = lists:nth(Pos, HeapList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = display_name_or_inital_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~11.11s|~-10.10s|~-45.45s|~n",
       [pid_to_list(Pid), observer_cli_lib:to_list(HeapSize), NameOrCall,
         observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)].

draw_last_line() ->
  io:format("|\e[31;1mINPUT: \e[0m\e[44m~s~s~s~s~s~s\e[49m|~n",
    ["q(quit)      ", "p(pause/unpause)      ", "r/rr(reduction)      ",
      "m/mm(memory)      ", "b/bb(binary memory)     ", "t/tt(total heap  size)"]).

notify_pause_status() ->
  io:format("\e[31;1m PAUSE  INPUT (p, r/rr, b/bb, h/hh, m/mm) to resume or q to quit \e[0m~n").

refresh_next_time(proc_count, Type, Interal, RankCostTime, NodeStatsCostTime) ->
  erlang:send_after(Interal - RankCostTime - NodeStatsCostTime, self(), {proc_count, Type});
refresh_next_time(proc_window, Type, _Interal, _RankCostTime, _CollectTime) ->
  erlang:send_after(100, self(), {proc_window, Type}).

get_stable_system_info() ->
  [begin observer_cli_lib:to_list(erlang:system_info(Item))end ||Item<- ?STABLE_SYSTEM_ITEM].

get_change_system_info() ->
  [begin recon_alloc:memory(Item) end || Item <- ?CHANGE_SYSTEM_ITEM].

get_reductions_and_msg_queue_len(Pid) ->
  case recon:info(Pid, [reductions, message_queue_len]) of
    undefined -> [{reductions, "die"}, {message_queue_len, "die"}];
    Info -> Info
  end.
get_memory_and_msg_queue_len(Pid) ->
  case recon:info(Pid, [memory, message_queue_len]) of
    undefined -> [{memory, "die"}, {message_queue_len, "die"}];
    Info -> Info
  end.

get_current_initial_call(Call) ->
  {_, CurFun} = lists:keyfind(current_function, 1, Call),
  {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
  {mfa_to_list(CurFun), mfa_to_list(InitialCall)}.

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

get_ranklist_and_cost_time(proc_window, Type, Interval, Time) when Time =/= ?FAST_COLLECT_INTENAL ->
  RemainTime = 2 * Interval - Time,
  {recon:proc_window(Type, ?DEFAULT_RANK_NUM, RemainTime), RemainTime};
get_ranklist_and_cost_time(_, Type, _Interval, _CollectTime) ->
  {recon:proc_count(Type, ?DEFAULT_RANK_NUM), 0}.

cpu_format_alarm_color(Percent1, Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]\e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, _Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]\e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, Percent2)when Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "|\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]\e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, _Percent2) ->
  "|\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]\e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n".

cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3) when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 >= ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[31m |~2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3)when Percent1 >= ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-31.31s\e[0m\e[41m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, Percent2, Percent3) when Percent1 < ?CPU_ALARM_THRESHOLD
  andalso Percent2 < ?CPU_ALARM_THRESHOLD andalso Percent3 < ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]\e[32m |~2.2s[~-31.31s\e[0m\e[42m~s\e[49m]|~n".

count_format_alarm_color(PortThreshold, PortCount, ProcThreshold, ProcCount)when PortCount > PortThreshold
  andalso ProcCount > ProcThreshold ->
  {"|~-9.9s | \e[31m~-19.19s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "|~-9.9s | ~-17.17s| ~-18.18s | ~-20.20s | ~-22.22s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(PortThreshold, PortCount, _ProcThreshold, _ProcCount)when PortCount > PortThreshold ->
  {"|~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "|~-9.9s | \e[31m~-17.17s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, ProcThreshold, ProcCount)when ProcCount > ProcThreshold ->
  {"|~-9.9s | \e[31m~-19.19s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "|~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, _ProcThreshold, _ProcCount) ->
  {"|~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "|~-9.9s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  }.

display_name_or_inital_call(IsName, _Call)when is_atom(IsName) -> atom_to_list(IsName);
display_name_or_inital_call(_IsName, Call) -> Call.


get_refresh_cost_info(proc_count, Type, Interval) ->
  io_lib:format(" recon:proc_count(~s, ~w) Refresh:~wms", [atom_to_list(Type), Interval div 2, Interval]);
get_refresh_cost_info(proc_window, Type, Interval) ->
  io_lib:format(" recon:proc_window(~s, ~w) Refresh:~wms",
    [atom_to_list(Type), Interval*2 - Interval div 2, Interval]).

mfa_to_list({Module, Fun, Arg}) ->
  atom_to_list(Module) ++ ":" ++
    atom_to_list(Fun) ++ "/" ++
    integer_to_list(Arg).
