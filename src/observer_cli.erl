%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli).

-include("observer_cli.hrl").

%% API
-export([start/0]).
-export([start/1]).
-export([start/2]).

-define(CPU_ALARM_THRESHOLD, 0.8). %% cpu >= this value will be highlight
-define(COUNT_ALARM_THRESHOLD, 0.85). %% port or process reach max_limit * 0.85 will be highlight
-define(FAST_COLLECT_INTERVAL, 0). %% collect should be fast when we push the keyboard to switch mode
-define(DEFAULT_ROW_SIZE, 47). %% the number from 13' mbp

-define(STABLE_SYSTEM_ITEM, [system_version, process_limit, smp_support,
    port_limit, ets_limit, logical_processors, multi_scheduling]).

-spec start() -> no_return.
start() -> start(#view_opts{}).

-spec start(Node) -> no_return when Node :: atom().
start(Node) when Node =:= node() -> start(#view_opts{});
start(Node) when is_atom(Node) -> rpc_start(Node);
start(#view_opts{home = Home = #home{tid = undefined},
    terminal_row = TerminalRow} = Opts) ->
    Tid = ets:new(process_info, [public, set]),
    NewHome = Home#home{tid = Tid},
    ChildPid = spawn(fun() -> render_worker(TerminalRow, NewHome) end),
    manager(ChildPid, Opts#view_opts{home = NewHome});
start(#view_opts{home = Home, terminal_row = TerminalRow} = Opts) ->
    ChildPid = spawn(fun() -> render_worker(TerminalRow, Home) end),
    manager(ChildPid, Opts).

-spec start(Node, Cookies) -> no_return when
    Node :: atom(),
    Cookies :: atom().
start(Node, _Cookie) when Node =:= node() -> start(#view_opts{});
start(Node, Cookie) when is_atom(Node) andalso is_atom(Cookie) ->
    erlang:set_cookie(Node, Cookie),
    rpc_start(Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rpc_start(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            TerminalRow =
                case io:rows() of
                    {error, _} -> ?DEFAULT_ROW_SIZE;
                    {ok, _} -> undefined
                end,
            rpc:call(Node, ?MODULE, start, [#view_opts{terminal_row = TerminalRow}]);
        false -> connect_error(<<"Remote node ~p(cookie:~p) refuse to be connected ~n">>, Node);
        ignored -> connect_error(<<"Ignore remote node~p(cookie:~p) connecting~n">>, Node)
    end.

manager(ChildPid, Opts) ->
    #view_opts{home = Home = #home{cur_pos = CurPos, tid = Tid}} = Opts,
    case observer_cli_lib:parse_cmd(Opts, ChildPid) of
        quit -> erlang:send(ChildPid, quit);
        pause_or_resume ->
            erlang:send(ChildPid, pause_or_resume),
            manager(ChildPid, Opts);
        {new_interval, NewInterval} ->
            erlang:exit(ChildPid, stop),
            start(Opts#view_opts{home = Home#home{interval = NewInterval}});
        {Func, Type, no_change} ->
            erlang:exit(ChildPid, stop),
            start(Opts#view_opts{home = Home#home{func = Func, type = Type}});
        {Func, Type, Interval} ->
            erlang:exit(ChildPid, stop),
            start(Opts#view_opts{home = Home#home{func = Func, type = Type, interval = Interval}});
        {jump_to_process, Pos} -> start_process_view(Tid, Pos, ChildPid, Opts);
        jump_to_process -> start_process_view(Tid, CurPos, ChildPid, Opts);
        _ -> manager(ChildPid, Opts)
    end.

render_worker(TerminalRow, #home{} = Home) ->
    ?output(?CLEAR),
    StableInfo = get_stable_system_info(),
    redraw(running, StableInfo, erlang:make_ref(), 0, Home, TerminalRow).

%% pause status waiting to be resume
redraw(pause, StableInfo, LastTimeRef, _, #home{func = Func, type = Type} = Home, TerminalRow) ->
    notify_pause_status(),
    erlang:cancel_timer(LastTimeRef),
    receive
        quit -> quit;
        pause_or_resume ->
            ?output(?CLEAR),
            redraw(running, StableInfo, LastTimeRef, ?FAST_COLLECT_INTERVAL, Home, TerminalRow);
        {Func, Type} ->
            redraw(running, StableInfo, LastTimeRef, ?FAST_COLLECT_INTERVAL, Home, TerminalRow)
    end;
%% running status
redraw(running, StableInfo, LastTimeRef, NodeStatsCostTime, #home{tid = Tid,
    interval = Interval, func = Func, type = Type, cur_pos = RankPos} = Home, TerminalRow0) ->
    TerminalRow = observer_cli_lib:to_row(TerminalRow0),
    erlang:cancel_timer(LastTimeRef),
    [{ProcSum, MemSum}] = recon:node_stats_list(1, NodeStatsCostTime),
    {CPURow, CPULine} = render_scheduler_usage(MemSum),
    Rows = max(TerminalRow - 14 - CPURow, 0),
    {RankList, RankCostTime} = get_ranklist_and_cost_time(Func, Type, Interval, Rows, NodeStatsCostTime),
    [UseMemInt, AllocatedMemInt, UnusedMemInt] = get_change_system_info(),
    NewNodeStatsCostTime = Interval div 2,
    Text = get_refresh_cost_info(Func, Type, Interval, Rows),
    MenuLine = observer_cli_lib:render_menu(home, Text, 133),
    SystemLine = render_system_line(StableInfo, UseMemInt, AllocatedMemInt, UnusedMemInt, ProcSum),
    MemLine = render_memory_process_line(ProcSum, MemSum, NewNodeStatsCostTime),
    {PidList, RankLine} = render_process_rank(Type, RankList, Rows, RankPos),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, MenuLine, SystemLine, MemLine, CPULine, RankLine, LastLine]),
    
    catch ets:insert(Tid, PidList),
    TimeRef = refresh_next_time(Func, Type, Interval, RankCostTime, NodeStatsCostTime),
    receive
        quit -> quit;
        pause_or_resume -> redraw(pause, StableInfo, TimeRef, ?FAST_COLLECT_INTERVAL, Home, TerminalRow0);
        {Func, Type} -> redraw(running, StableInfo, TimeRef, NewNodeStatsCostTime, Home, TerminalRow0)
    end.

render_system_line(StableInfo, UseMemInt, AllocatedMemInt, UnusedMemInt, ProcSum) ->
    [Version, ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling] = StableInfo,
    UseMem = observer_cli_lib:to_megabyte_str(UseMemInt),
    AllocatedMem = observer_cli_lib:to_megabyte_str(AllocatedMemInt),
    UnUsedMem = observer_cli_lib:to_megabyte_str(UnusedMemInt),
    UsePercent = observer_cli_lib:float_to_percent_with_two_digit(UseMemInt / AllocatedMemInt),
    UnUsePercent = observer_cli_lib:float_to_percent_with_two_digit(UnusedMemInt / AllocatedMemInt),
    {PortWarning, ProcWarning, PortCount, ProcCount} = get_port_proc_count_info(PortLimit, ProcLimit, ProcSum),
    VersionLine = ?render([?W(Version -- "\n", 138)]),
    Title = ?render([?GRAY_BG,
        ?W("System", 10), ?W("Count/Limit", 21),
        ?W("System Switch", 20), ?W("State", 24),
        ?W("Memory Info", 20), ?W("Megabyte", 28),
        ?RESET]),
    Row1 = ?render([
        ?W("Proc Count", 10), ?W(ProcWarning, ProcCount, 21), ?W("Smp Support", 20),
        ?W(SmpSupport, 24), ?W("Allocted Mem", 20), ?W(AllocatedMem, 19), ?W("100.0%", 6)
    ]),
    Row2 = ?render([
        ?W("Port Count", 10), ?W(PortWarning, PortCount, 21), ?W("Multi Scheduling", 20),
        ?W(MultiScheduling, 24), ?W("Use Mem", 20), ?W(UseMem, 19), ?W(UsePercent, 6)
    ]),
    Row3 = ?render([
        ?UNDERLINE, ?W("Ets Limit", 10), ?W(EtsLimit, 21), ?W("Logical Processors", 20),
        ?W(LogicalProc, 24), ?W("Unuse Mem", 20), ?W(UnUsedMem, 19),
        ?W(UnUsePercent, 6), ?RESET]),
    [VersionLine, Title, Row1, Row2, Row3].

render_memory_process_line(ProcSum, MemSum, Interval) ->
    TotalMemInt = proplists:get_value(memory_total, ProcSum),
    TotalMem = observer_cli_lib:to_megabyte_str(TotalMemInt),
    ProcMemInt = proplists:get_value(memory_procs, ProcSum),
    ProcMem = observer_cli_lib:to_megabyte_str(ProcMemInt),
    ProcMemPercent = observer_cli_lib:float_to_percent_with_two_digit(ProcMemInt / TotalMemInt),
    AtomMemInt = proplists:get_value(memory_atoms, ProcSum),
    AtomMem = observer_cli_lib:to_megabyte_str(AtomMemInt),
    AtomMemPercent = observer_cli_lib:float_to_percent_with_two_digit(AtomMemInt / TotalMemInt),
    BinMemInt = proplists:get_value(memory_bin, ProcSum),
    BinMem = observer_cli_lib:to_megabyte_str(BinMemInt),
    BinMemPercent = observer_cli_lib:float_to_percent_with_two_digit(BinMemInt / TotalMemInt),
    CodeMemInt = erlang:memory(code),
    CodeMem = observer_cli_lib:to_megabyte_str(CodeMemInt),
    CodeMemPercent = observer_cli_lib:float_to_percent_with_two_digit(CodeMemInt / TotalMemInt),
    EtsMemInt = proplists:get_value(memory_ets, ProcSum),
    EtsMem = observer_cli_lib:to_megabyte_str(EtsMemInt),
    EtsMemPercent = observer_cli_lib:float_to_percent_with_two_digit(EtsMemInt / TotalMemInt),
    RunQueue = integer_to_list(proplists:get_value(run_queue, ProcSum)),
    BytesIn = observer_cli_lib:to_megabyte_str(proplists:get_value(bytes_in, MemSum)),
    BytesOut = observer_cli_lib:to_megabyte_str(proplists:get_value(bytes_out, MemSum)),
    GcCount = observer_cli_lib:to_list(proplists:get_value(gc_count, MemSum)),
    GcWordsReclaimed = observer_cli_lib:to_list(proplists:get_value(gc_words_reclaimed, MemSum)),
    Reductions = integer_to_list(proplists:get_value(reductions, MemSum)),
    Row1 = ?render([
        ?GRAY_BG, ?W("Memory", 10), ?W("State", 21), ?W("Memory", 20), ?W("State", 24),
        ?W("Memory", 20), ?W("Interval: " ++ integer_to_list(Interval) ++ "ms", 28), ?RESET]),
    Row2 = ?render([
        ?W("Total", 10), ?W(TotalMem, 12), ?W("100%", 6), ?W("Binary", 20),
        ?W(BinMem, 15), ?W(BinMemPercent, 6), ?W("IO Output", 20), ?W(BytesOut, 28)]),
    Row3 = ?render([
        ?W("Process", 10), ?W(ProcMem, 12), ?W(ProcMemPercent, 6), ?W("Code", 20),
        ?W(CodeMem, 15), ?W(CodeMemPercent, 6), ?W("IO Input", 20), ?W(BytesIn, 28)]),
    Row4 = ?render([
        ?W("Atom", 10), ?W(AtomMem, 12), ?W(AtomMemPercent, 6), ?W("Reductions", 20),
        ?W(Reductions, 24), ?W("Gc Count", 20), ?W(GcCount, 28)]),
    Row5 = ?render([
        ?W("Ets", 10), ?W(EtsMem, 12), ?W(EtsMemPercent, 6), ?W("Run Queue", 20),
        ?W(RunQueue, 24), ?W("Gc Words Reclaimed", 20), ?W(GcWordsReclaimed, 28)]),
    [Row1, Row2, Row3, Row4, Row5].

render_scheduler_usage(MemSum) ->
    SchedulerUsage = proplists:get_value(scheduler_usage, MemSum),
    SchedulerNum = erlang:length(SchedulerUsage),
    render_scheduler_usage(SchedulerUsage, SchedulerNum).

%% =< 8 core will split 2 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum =< 8 ->
    HalfSchedulerNum = SchedulerNum div 2,
    CPU =
        [begin
             Seq2 = Seq1 + HalfSchedulerNum,
             Percent1 = proplists:get_value(Seq1, SchedulerUsage),
             Percent2 = proplists:get_value(Seq2, SchedulerUsage),
             CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
             CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
             CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq1])),
             CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq2])),
             Process1 = lists:duplicate(trunc(Percent1 * 57), "|"),
             Process2 = lists:duplicate(trunc(Percent2 * 57), "|"),
             Format = cpu_format_alarm_color(Percent1, Percent2),
             case Seq1 =:= HalfSchedulerNum of
                 false -> io_lib:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2]);
                 true ->
                     io_lib:format(<<?UNDERLINE/binary, Format/binary, ?RESET/binary>>,
                         [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2])
             end
         end || Seq1 <- lists:seq(1, HalfSchedulerNum)],
    {HalfSchedulerNum, CPU};
%% >= 8 will split 4 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) ->
    PosSchedulerNum = SchedulerNum div 4,
    CPU =
        [begin
             Seq2 = Seq1 + PosSchedulerNum,
             Seq3 = Seq2 + PosSchedulerNum,
             Seq4 = Seq3 + PosSchedulerNum,
             Percent1 = proplists:get_value(Seq1, SchedulerUsage),
             Percent2 = proplists:get_value(Seq2, SchedulerUsage),
             Percent3 = proplists:get_value(Seq3, SchedulerUsage),
             Percent4 = proplists:get_value(Seq4, SchedulerUsage),
             CPU1 = observer_cli_lib:float_to_percent_with_two_digit(Percent1),
             CPU2 = observer_cli_lib:float_to_percent_with_two_digit(Percent2),
             CPU3 = observer_cli_lib:float_to_percent_with_two_digit(Percent3),
             CPU4 = observer_cli_lib:float_to_percent_with_two_digit(Percent4),
             CPUSeq1 = lists:flatten(io_lib:format("~2..0w", [Seq1])),
             CPUSeq2 = lists:flatten(io_lib:format("~2..0w", [Seq2])),
             CPUSeq3 = lists:flatten(io_lib:format("~2..0w", [Seq3])),
             CPUSeq4 = lists:flatten(io_lib:format("~2..0w", [Seq4])),
             Process1 = lists:duplicate(trunc(Percent1 * 23), "|"),
             Process2 = lists:duplicate(trunc(Percent2 * 22), "|"),
             Process3 = lists:duplicate(trunc(Percent3 * 22), "|"),
             Process4 = lists:duplicate(trunc(Percent4 * 23), "|"),
             Format = cpu_format_alarm_color(Percent1, Percent2, Percent3, Percent4),
             case Seq1 =:= PosSchedulerNum of
                 false ->
                     io_lib:format(Format, [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2,
                         CPUSeq3,Process3, CPU3, CPUSeq4,Process4, CPU4]);
                 true ->
                     io_lib:format(<<?UNDERLINE/binary, Format/binary, ?RESET/binary>>,
                         [CPUSeq1, Process1, CPU1, CPUSeq2, Process2, CPU2,
                             CPUSeq3, Process3, CPU3, CPUSeq4,Process4, CPU4])
             end
         end || Seq1 <- lists:seq(1, PosSchedulerNum)],
    {PosSchedulerNum, CPU}.

render_process_rank(memory, MemoryList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Memory", 11),
        ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MemVal, Call = [IsName | _]} = lists:nth(Pos, MemoryList),
            [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
            {CurFun, InitialCall} = get_current_initial_call(Call),
            NameOrCall = display_name_or_initial_call(IsName, InitialCall),
            Format = get_choose_format(RankPos, Pos),
            R = io_lib:format(Format,
                [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
                    observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))),
    {ProcList, [Title | Rows]};
render_process_rank(binary_memory, MemoryList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "BinMemory", 11),
        ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MemVal, Call = [IsName | _]} = lists:nth(Pos, MemoryList),
            [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
            {CurFun, InitialCall} = get_current_initial_call(Call),
            NameOrCall = display_name_or_initial_call(IsName, InitialCall),
            Format = get_choose_format(RankPos, Pos),
            R = io_lib:format(Format,
                [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MemVal), NameOrCall,
                    observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))),
    {ProcList, [Title | Rows]};
render_process_rank(reductions, ReductionList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Reductions", 11),
        ?W("Name or Initial Call", 30),
        ?W("Memory", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, Reductions, Call = [IsName | _]} = lists:nth(Pos, ReductionList),
            [{_, Memory}, {_, MsgQueueLen}] = get_memory_and_msg_queue_len(Pid),
            {CurFun, InitialCall} = get_current_initial_call(Call),
            NameOrCall = display_name_or_initial_call(IsName, InitialCall),
            Format = get_choose_format(RankPos, Pos),
            R = io_lib:format(Format,
                [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(Reductions), NameOrCall,
                    observer_cli_lib:to_list(Memory), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(ReductionList)))),
    {ProcList, [Title | Rows]};
render_process_rank(total_heap_size, HeapList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "TotalHeap", 11),
        ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, HeapSize, Call = [IsName | _]} = lists:nth(Pos, HeapList),
            [{_, Reductions}, {_, MsgQueueLen}] = get_reductions_and_msg_queue_len(Pid),
            {CurFun, InitialCall} = get_current_initial_call(Call),
            NameOrCall = display_name_or_initial_call(IsName, InitialCall),
            Format = get_choose_format(RankPos, Pos),
            R = io_lib:format(Format,
                [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(HeapSize), NameOrCall,
                    observer_cli_lib:to_list(Reductions), observer_cli_lib:to_list(MsgQueueLen), CurFun]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(HeapList)))),
    {ProcList, [Title | Rows]};
render_process_rank(message_queue_len, MQLenList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Msg Queue", 11),
        ?W("Name or Initial Call", 30),
        ?W("Memory", 10), ?W("Reductions", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MQLen, Call = [IsName | _]} = lists:nth(Pos, MQLenList),
            [{_, Reductions}, {_, Memory}] = get_reductions_and_memory(Pid),
            {CurFun, InitialCall} = get_current_initial_call(Call),
            NameOrCall = display_name_or_initial_call(IsName, InitialCall),
            Format = get_choose_format(RankPos, Pos),
            R = io_lib:format(Format,
                [observer_cli_lib:to_list(Pos), pid_to_list(Pid), observer_cli_lib:to_list(MQLen), NameOrCall,
                    observer_cli_lib:to_list(Memory), observer_cli_lib:to_list(Reductions), CurFun]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(MQLenList)))),
    {ProcList, [Title | Rows]}.

render_last_line() ->
    Text = "q(quit) p(pause) r/rr(reduction) " ++
        "m/mm(memory) b/bb(binary memory) t/tt(total heap size) mq/mmq(message queue) j9(jump to process 9)",
    ?render([?UNDERLINE, ?RED, "INPUT:", ?RESET, ?UNDERLINE, ?GRAY_BG,
        ?W(Text, ?COLUMN - 3), ?RESET]).

notify_pause_status() ->
    ?output("\e[31;1m PAUSE  INPUT (p, r/rr, b/bb, h/hh, m/mm) to resume or q to quit \e[0m~n").

get_choose_format(Pos, Pos) ->
    "|\e[33m~-3.3s|~-12.12s|~12.12s | ~-30.30s | ~11.11s| ~-11.11s| ~-47.47s\e[0m|~n";
get_choose_format(_Pos, _RankPos) ->
    "|~-3.3s|~-12.12s|~12.12s | ~-30.30s | ~11.11s| ~-11.11s| ~-47.47s|~n".

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
    PortThreshold = PortLimitInt * ?COUNT_ALARM_THRESHOLD,
    ProcThreshold = ProcLimitInt * ?COUNT_ALARM_THRESHOLD,
    PortWarning =
        case PortCountInt > PortThreshold of
            true -> ?RED;
            false -> <<"">>
        end,
    ProcWarning =
        case ProcCountInt > ProcThreshold of
            true -> ?RED;
            false -> <<"">>
        end,
    {PortWarning, ProcWarning, PortCount, ProcCount}.

cpu_format_alarm_color(Percent1, Percent2) ->
    Warning1 =
        case Percent1 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    Warning2 =
        case Percent2 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    <<"|", Warning1/binary, "|~-2.2s ~-57.57s", "~s", Warning2/binary, " |~-2.2s ~-57.57s", " ~s", "  |~n">>.

cpu_format_alarm_color(Percent1, Percent2, Percent3, Percent4) ->
    Warning1 =
        case Percent1 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    Warning2 =
        case Percent2 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    Warning3 =
        case Percent3 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    Warning4 =
        case Percent4 >= ?CPU_ALARM_THRESHOLD of
            true -> ?RED;
            false -> ?GREEN
        end,
    <<"|",
        Warning1/binary, "|~-2.2s ~-23.23s", " ~s",
        Warning2/binary, " |~-2.2s ~-22.22s", " ~s",
        Warning3/binary, " |~-2.2s ~-22.22s", " ~s",
        Warning4/binary, " |~-2.2s ~-23.23s", " ~s",
        " |~n">>.

display_name_or_initial_call(IsName, _Call) when is_atom(IsName) -> atom_to_list(IsName);
display_name_or_initial_call(_IsName, Call) -> Call.

get_refresh_cost_info(proc_count, Type, Interval, Rows) ->
    io_lib:format("recon:proc_count(~p,~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_cost_info(proc_window, Type, Interval, Rows) ->
    io_lib:format("recon:proc_window(~p,~w,~w) Interval:~wms", [Type, Rows, Interval * 2 - Interval div 2, Interval * 2]).


get_stable_system_info() ->
    [begin observer_cli_lib:to_list(erlang:system_info(Item)) end || Item <- ?STABLE_SYSTEM_ITEM].

get_change_system_info() ->
    UsedMem = recon_alloc:memory(used),
    AllocatedMem = recon_alloc:memory(allocated),
    [UsedMem, AllocatedMem, AllocatedMem - UsedMem].

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

get_reductions_and_memory(Pid) ->
    case recon:info(Pid, [reductions, memory]) of
        undefined -> [{reductions, "die"}, {memory, "die"}];
        Info -> Info
    end.

get_ranklist_and_cost_time(proc_window, Type, Interval, Rows, Time) when Time =/= ?FAST_COLLECT_INTERVAL ->
    RemainTime = 2 * Interval - Time,
    {recon:proc_window(Type, Rows, RemainTime), RemainTime};
get_ranklist_and_cost_time(_, Type, _Interval, Rows, _CollectTime) ->
    {recon:proc_count(Type, Rows), 0}.

connect_error(Prompt, Node) ->
    Prop = <<?RED/binary, Prompt/binary, ?RESET/binary>>,
    ?output(Prop, [Node, erlang:get_cookie()]).

start_process_view(Tid, Pos, ChildPid, Opts) ->
    case ets:lookup(Tid, Pos) of
        [] -> manager(ChildPid, Opts);
        [{_, ChoosePid}] ->
            erlang:exit(ChildPid, stop),
            NewHomeOpt = Opts#view_opts.home#home{cur_pos = Pos},
            observer_cli_process:start(ChoosePid, Opts#view_opts{home = NewHomeOpt})
    end.
