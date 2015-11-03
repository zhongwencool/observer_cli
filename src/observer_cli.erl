%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli).

-export([start/0]).
-export([start/1]).

-define(DEFAULT_RANK_NUM, 20). %%fill full in 13.3 inch screen(24 core)
-define(CPU_ALARM_THRESHOLD, 0.8).
-define(COUNT_ALARM_THRESHOLD, 0.85).
-define(DEFAULT_COLLECT_INTENAL, 1000).
-define(FAST_COLLECT_INTENAL, 0). %% collect should be fast when we push the keyboard to switch mode

-define(STABLE_SYSTEM_ITEM, [system_version, process_limit, smp_support,
  port_limit, ets_limit, logical_processors, multi_scheduling]).
-define(CHANGE_SYSTEM_ITEM, [used, allocated, unused]).

%% @doc a top tool in erlang shell the reflushtime is Milliseconds
-define(TOP_MIN_REFLUSH_INTERAL, 2000).
-spec start() -> quit.
start() -> start(?TOP_MIN_REFLUSH_INTERAL).
-spec start(pos_integer()) -> quit.
start(ReflushMillSecond)when ReflushMillSecond >= ?TOP_MIN_REFLUSH_INTERAL ->
  Pid = spawn_link(fun() -> loop(ReflushMillSecond) end),
  top(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top(Pid) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, quit);
    "r\n" -> erlang:send(Pid, reductions), top(Pid);
    "b\n" -> erlang:send(Pid, binary_memory), top(Pid);
    "h\n" -> erlang:send(Pid, total_heap_size), top(Pid);
    "m\n" -> erlang:send(Pid, memory), top(Pid);
    "p\n" -> erlang:send(Pid, pause_or_resume), top(Pid);
    _ -> top(Pid)
  end.

loop(Interal) ->
  clear_screen(),
  {UpTime, _} = erlang:statistics(wall_clock),
  StableInfo = get_stable_system_info(), %%don't need reflush the stable info everytime
  refresh(Interal, memory, UpTime, StableInfo, erlang:make_ref(), 0, running).

%% refresh the shell UI
refresh(Interal, Type, _UpTime, StableInfo, LastTimeRef, _, pause) ->
  notify_pause_status(),
  erlang:cancel_timer(LastTimeRef),
  receive
    quit -> quit;
    pause_or_resume ->
      clear_screen(),
      {NewUpTime, _} = erlang:statistics(wall_clock),
      refresh(Interal, Type, NewUpTime, StableInfo, LastTimeRef, ?FAST_COLLECT_INTENAL, running);
    NewType ->
      {NewUpTime, _} = erlang:statistics(wall_clock),
      refresh(Interal, NewType, NewUpTime, StableInfo, LastTimeRef, ?FAST_COLLECT_INTENAL, running)
  end;
refresh(Interal, Type, UpTime, StableInfo, LastTimeRef, CollectTime, running) ->
  [Version, ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling] = StableInfo,
  erlang:cancel_timer(LastTimeRef),
  [{ProcSum, MemSum}] = recon:node_stats_list(1, CollectTime),%% collection must cost time
  [UseMemInt, AlloctedMemInt, UnusedMemInt] = get_change_system_info(),
  move_cursor_to_top_line(),
  draw_first_line(Version, UpTime),
  draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc,
    MultiScheduling, UseMemInt, AlloctedMemInt, UnusedMemInt, ProcSum),
  draw_memory_process_line(ProcSum, MemSum),
  draw_scheduler_usage(MemSum),
  draw_process_rank(Type, ?DEFAULT_RANK_NUM),
  draw_last_line(),
  TimeRef = erlang:send_after(Interal div 2, self(), Type),%% collection will cost time
  receive
    quit -> quit;
    pause_or_resume -> refresh(Interal, Type, UpTime, StableInfo, TimeRef, ?FAST_COLLECT_INTENAL, pause);
    Type -> refresh(Interal, Type, UpTime + Interal, StableInfo, TimeRef, Interal - (Interal div 2), running);
    NewType ->
      {NewUpTime, _} = erlang:statistics(wall_clock),
      refresh(Interal, NewType, NewUpTime, StableInfo, TimeRef, ?FAST_COLLECT_INTENAL, running)
  end.

%System     | Count/Limit        | System Switch      | State                  | Memory Info          | Megabyte                 |
%Proc Count | 42/262144          | Smp Support        | true                   | Allocted Mem         | 32.0698M           100.0%|
%Port Count | 6/65536            | Multi Scheduling   | enabled                | Use Mem              | 19.0814M           60.59%|
%Ets Limit  | 2053               | Logical Processors | 4                      | Unuse Mem            | 12.0537M           38.34%|
draw_system_line(ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling,
    UseMemInt, AlloctedMemInt, UnusedMemInt, ProcSum) ->
  UseMem = to_megabyte_list(UseMemInt),
  AlloctedMem = to_megabyte_list(AlloctedMemInt),
  UnunsedMem = to_megabyte_list(UnusedMemInt),
  UsePrce = float_to_list_with_two_digit(UseMemInt/AlloctedMemInt),
  UnusePrce = float_to_list_with_two_digit(UnusedMemInt/AlloctedMemInt),
  {ProcFormat, ProcCount, PortFormat, PortCount} =
    get_port_proc_count_info(PortLimit, ProcLimit, ProcSum),
  io:format("\e[46m~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s\e[49m|~n", %%cyan background
    ["System ", "Count/Limit", "System Switch", "State", "Memory Info", "Megabyte"]),
  io:format(ProcFormat,
    ["Proc Count", ProcCount, "Smp Support", SmpSupport, "Allocted Mem", AlloctedMem]),
  io:format(PortFormat,
    ["Port Count", PortCount, "Multi Scheduling", MultiScheduling, "Use Mem", UseMem, UsePrce]),
  io:format("~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n",
    ["Ets Limit", EtsLimit, "Logical Processors", LogicalProc, "Unuse Mem", UnunsedMem, UnusePrce]).

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
draw_process_rank(memory, Num) ->
  MemoryList = recon:proc_count(memory, Num),
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s\e[49m|~n", %%cyan background
    ["Pid", "Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = get_display_name_or_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s|~n",
       [pid_to_list(Pid), to_list(MemVal), NameOrCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(binary_memory, Num) ->
  MemoryList = recon:proc_count(binary_memory, Num),
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s\e[49m|~n", %%cyan background
    ["Pid", "Bin Memory", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, MemVal, Call = [IsName|_]} = lists:nth(Pos, MemoryList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = get_display_name_or_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s|~n",
       [pid_to_list(Pid), to_list(MemVal), NameOrCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(reductions, Num) ->
  ReductionList = recon:proc_count(reductions, Num),
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s\e[49m|~n", %%cyan background
    ["Pid", "Reductions", "Name or Initial Call", "Memory", "Msg Queue", "Current Function"]),
  [begin
     {Pid, Reductions, Call = [IsName|_]} = lists:nth(Pos, ReductionList),
     [{_, Memory}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = get_display_name_or_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s|~n",
       [pid_to_list(Pid), to_list(Reductions), NameOrCall, to_list(Memory), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)];
draw_process_rank(total_heap_size, Num) ->
  HeapList = recon:proc_count(total_heap_size, Num),
  io:format("\e[46m| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s\e[49m|~n", %%cyan background
    ["Pid", "Total Heap Size", "Name or Initial Call", "Reductions", "Msg Queue", "Current Function"]),
  [begin
     {Pid, HeapSize, Call = [IsName|_]} = lists:nth(Pos, HeapList),
     [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
     {CurFun, InitialCall} = get_current_initial_call(Call),
     NameOrCall = get_display_name_or_call(IsName, InitialCall),
     io:format("| ~-15.15s|~11.11s| ~-28.28s|~10.10s|~-10.10s|~-46.46s|~n",
       [pid_to_list(Pid), to_list(HeapSize), NameOrCall, to_list(Reductions), to_list(MsgQueueLen), CurFun])
   end|| Pos <- lists:seq(1, Num)].

%Memory     | Megabyte           | Process State      | Count                  | Memory               | State                    |
%Total      | 19.0999M       100%| Binary             | 0.0182M          00.91%| IO Output            | 0.0000M                  |
%Process    | 5.0683M      28.41%| Code               | 7.0150M          35.75%| IO Input             | 0.0000M                  |
%Atom       | 0.0301M      01.50%| Reductions         | 176686                 | Run Queue            | 0                        |
%Ets        | 0.0697M      03.49%| Gc Count           | 2                      | Gc Words Reclaimed   | 10173                    |
draw_memory_process_line(ProcSum, MemSum) ->
  TotalMemInteger = proplists:get_value(memory_total, ProcSum),
  TotalMem = to_megabyte_list(TotalMemInteger),
  ProcMemInteger = proplists:get_value(memory_procs, ProcSum),
  ProcMem = to_megabyte_list(ProcMemInteger),
  ProcMemPerc = float_to_list_with_two_digit(ProcMemInteger/TotalMemInteger),
  AtomMemInteger = proplists:get_value(memory_atoms, ProcSum),
  AtomMem = to_megabyte_list(AtomMemInteger),
  AtomMemPerc = float_to_list_with_two_digit(AtomMemInteger/TotalMemInteger),
  BinMemInteger = proplists:get_value(memory_bin, ProcSum),
  BinMem = to_megabyte_list(BinMemInteger),
  BinMemPerc = float_to_list_with_two_digit(BinMemInteger/TotalMemInteger),
  EtsMemInteger = proplists:get_value(memory_ets, ProcSum),
  CodeMemInteger = erlang:memory(code),
  CodeMem = to_megabyte_list(CodeMemInteger),
  CodeMemPerc = float_to_list_with_two_digit(CodeMemInteger/TotalMemInteger),
  EtsMem = to_megabyte_list(EtsMemInteger),
  EtsMemPerc = float_to_list_with_two_digit(EtsMemInteger/TotalMemInteger),
  Runqueue = integer_to_list(proplists:get_value(run_queue, ProcSum)),
  BytesIn = to_megabyte_list(proplists:get_value(bytes_in, MemSum)),
  BytesOut = to_megabyte_list(proplists:get_value(bytes_out, MemSum)),
  GcCount = to_list(proplists:get_value(gc_count, MemSum)),
  GcWordsReclaimed = to_list(proplists:get_value(gc_words_reclaimed, MemSum)),
  Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),
  io:format("\e[46m~-10.10s | ~-18.18s | ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s\e[49m|~n", %%cyan background
    ["Memory", "State", "Memory ", "Stae", "Memory", "Accumulate RefreshTime/2"]),
  io:format("~-10.10s | ~-13.13s~6.6s| ~-18.18s | ~-17.17s~6.6s| ~-20.20s | ~-25.25s|~n",
    ["Total", TotalMem, "100%", "Binary", BinMem, BinMemPerc, "IO Output", BytesOut]),
  io:format("~-10.10s | ~-13.13s~6.6s| ~-18.18s | ~-17.17s~6.6s| ~-20.20s | ~-25.25s|~n",
    ["Process", ProcMem, ProcMemPerc, "Code", CodeMem, CodeMemPerc, "IO Input", BytesIn]),
  io:format("~-10.10s | ~-13.13s~6.6s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s|~n",
    ["Atom", AtomMem, AtomMemPerc, "Reductions", Reductions, "Run Queue", Runqueue]),
  io:format("~-10.10s | ~-13.13s~6.6s| ~-18.18s | ~-22.22s | ~-20.20s | ~-25.25s|~n",
    ["Ets", EtsMem, EtsMemPerc, "Gc Count", GcCount, "Gc Words Reclaimed", GcWordsReclaimed]).

%|01[|||||||||||||||||||||||||||||||                     59.66%]  |03[||||||||||                                          19.59%]|
%|02[|||||||||||||||||||||||||||||||                     61.02%]  |04[|||||||                                             14.44%]|
draw_scheduler_usage(MemSum) ->
  SchedulerUsage = proplists:get_value(scheduler_usage, MemSum),
  SchedulerNum = erlang:length(SchedulerUsage),
  HalfSchedulerNum = SchedulerNum div 2,
  [begin
     Percent1 = proplists:get_value(Seq, SchedulerUsage),
     Percent2 = proplists:get_value(Seq + HalfSchedulerNum, SchedulerUsage),
     CPU1 = float_to_list_with_two_digit(Percent1),
     CPU2 = float_to_list_with_two_digit(Percent2),
     CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq])),
     CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq + HalfSchedulerNum])),
     Process1 = lists:duplicate(trunc(Percent1 * 52), "|"),
     Process2 = lists:duplicate(trunc(Percent2 * 52), "|"),
     Format = cpu_format_alarm_color(Percent1, Percent2),
     io:format(Format,
       [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
   end|| Seq <- lists:seq(1, HalfSchedulerNum)].

get_reductions_and_msg_queue_len(Pid) ->
  case recon:info(Pid, [reductions, message_queue_len]) of
    undefined -> [{reductions, "die"}, {message_queue_len, "die"}];
    Ok -> Ok
  end.

get_current_initial_call(Call) ->
  {_, CurFun} = lists:keyfind(current_function, 1, Call),
  {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
  {mfa_to_list(CurFun), mfa_to_list(InitialCall)}.

get_stable_system_info() ->
  [begin to_list(erlang:system_info(Item))end ||Item<- ?STABLE_SYSTEM_ITEM].

get_change_system_info() ->
  [begin recon_alloc:memory(Item) end || Item <- ?CHANGE_SYSTEM_ITEM].

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
  "\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m] \e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(Percent1, _Percent2)when Percent1 >= ?CPU_ALARM_THRESHOLD ->
  "\e[31m|~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m] \e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, Percent2)when Percent2 >= ?CPU_ALARM_THRESHOLD ->
  "\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m] \e[31m |~-2.2s[~-52.52s\e[0m\e[41m~s\e[49m]|~n";
cpu_format_alarm_color(_Percent1, _Percent2) ->
  "\e[32m|~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m] \e[32m |~-2.2s[~-52.52s\e[0m\e[42m~s\e[49m]|~n".

count_format_alarm_color(PortThreshold, PortCount, ProcThreshold, ProcCount)when PortCount > PortThreshold
  andalso ProcCount > ProcThreshold ->
  {"~-10.10s | \e[31m~-19.19s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "~-10.10s | ~-17.17s| ~-18.18s | ~-20.20s | ~-22.22s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(PortThreshold, PortCount, _ProcThreshold, _ProcCount)when PortCount > PortThreshold ->
  {"~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "~-10.10s | \e[31m~-17.17s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, ProcThreshold, ProcCount)when ProcCount > ProcThreshold ->
  {"~-10.10s | \e[31m~-19.19s\e[0m| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  };
count_format_alarm_color(_PortThreshold, _PortCount, _ProcThreshold, _ProcCount) ->
  {"~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s100.0%|~n",
    "~-10.10s | ~-19.19s| ~-18.18s | ~-22.22s | ~-20.20s | ~-19.19s~6.6s|~n"
  }.

get_display_name_or_call(IsName, _Call)when is_atom(IsName) -> atom_to_list(IsName);
get_display_name_or_call(_IsName, Call) -> Call.

to_megabyte_list(M) ->
  Val = trunc(M/(1024*1024)*1000),
  Integer = Val div 1000,
  Decmial = Val - Integer * 1000,
  lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decmial])).

clear_screen() ->
  io:format("\e[H\e[J").

move_cursor_to_top_line() ->
  io:format("\e[H").

draw_first_line(Version, UpTime) ->
  FirstLine = (Version --"\n") ++ green(" Uptime:" ++ uptime(UpTime)),
  io:format("~s~n", [FirstLine]).

notify_pause_status() ->
  io:format("\e[31;1m PAUSE  INPUT (p, r, b, h, m ) to resume or q to quit \e[0m~n").

draw_last_line() ->
  io:format("\e[31;1mINPUT:\e[0m\e[44m  ~s |  ~s |   ~s       |~s|   ~s    |      ~s         \e[49m|~n",
    ["r(reduction)", "q(quit)", "b(binary memory)", "h(total heap  size)", "m(memory)", "p(pause/unpause)"]).

green(String) ->
  "\e[32;1m" ++ String ++ "\e[0m".

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list(Val) -> Val.

float_to_list_with_two_digit(Float) ->
  Val = trunc(Float*10000),
  Integer = Val div 100,
  Decmial = Val - Integer * 100,
  case Integer of
    100 -> "100.0%";
    _ -> lists:flatten(io_lib:format("~2..0w.~2..0w%", [Integer, Decmial]))
  end.

mfa_to_list({Module, Fun, Arg}) ->
  atom_to_list(Module) ++ ":" ++
    atom_to_list(Fun) ++ "/" ++
    integer_to_list(Arg).

uptime(UpTime) ->
  {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
  lists:flatten(io_lib:format("~pDays ~p:~p:~p", [D, H, M, S])).
