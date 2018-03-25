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

-define(STABLE_SYSTEM_KEY, [system_version, process_limit, smp_support,
    port_limit, ets_limit, logical_processors, multi_scheduling]).

-spec start() -> no_return.
start() -> start(#view_opts{}).

-spec start(Node) -> no_return when Node :: atom().
start(Node) when Node =:= node() -> start(#view_opts{});
start(Node) when is_atom(Node) -> rpc_start(Node);
start(#view_opts{home = #home{tid = undefined}} = Opts) ->
    Tid = ets:new(observer_cli_top_n, [public, set]),
    AutoRow = check_auto_row(),
    NewHome = #home{tid = Tid},
    ChildPid = spawn(fun() -> render_worker(NewHome, AutoRow) end),
    manager(ChildPid, Opts#view_opts{home = NewHome, auto_row = AutoRow});
start(#view_opts{home = Home} = Opts) ->
    AutoRow = check_auto_row(),
    ChildPid = spawn(fun() -> render_worker(Home, AutoRow) end),
    manager(ChildPid, Opts#view_opts{auto_row = AutoRow}).

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
            rpc:call(Node, ?MODULE, start, [#view_opts{}]);
        false -> connect_error(<<"Node(~p) refuse to be connected, make sure cookie is valid~n">>, Node);
        ignored -> connect_error(<<"Ignored by node(~p), local node is not alive!~n">>, Node)
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

render_worker(#home{} = Home, AutoRow) ->
    ?output(?CLEAR),
    StableInfo = get_stable_system_info(),
    redraw_running(Home, StableInfo, erlang:make_ref(), AutoRow, true).

%% pause status waiting to be resume
redraw_pause(#home{func = Func, type = Type} = Home, StableInfo, LastTimeRef, AutoRow) ->
    notify_pause_status(),
    erlang:cancel_timer(LastTimeRef),
    receive
        quit -> quit;
        pause_or_resume ->
            ?output(?CLEAR),
            redraw_running(Home, StableInfo, LastTimeRef, AutoRow, true);
        {Func, Type} ->
            redraw_running(Home, StableInfo, LastTimeRef, AutoRow, false)
    end.

%% running status
redraw_running(#home{tid = Tid, interval = Interval, func = Func, type = Type, cur_pos = RankPos} = Home,
    StableInfo, LastTimeRef, AutoRow, IsFirstTime) ->
    erlang:cancel_timer(LastTimeRef),
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    [{Processes, Schedulers}] = recon:node_stats(1, 0, fun(X, Acc) -> [X|Acc] end, []),
    {CPURow, CPULine} = render_scheduler_usage(Schedulers),
    ProcessRows = max(TerminalRow - 14 - CPURow, 0),
    TopList = get_top_n(Func, Type, Interval, ProcessRows, IsFirstTime),
    {UseMemInt, AllocatedMemInt, UnusedMemInt} = get_change_system_info(),
    Text = get_refresh_prompt(Func, Type, Interval, ProcessRows),
    MenuLine = observer_cli_lib:render_menu(home, Text, 133),
    SystemLine = render_system_line(StableInfo, UseMemInt, AllocatedMemInt, UnusedMemInt, Processes),
    MemLine = render_memory_process_line(Processes, Schedulers, Interval),
    {PidList, RankLine} = render_top_n_view(Type, TopList, ProcessRows, RankPos),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, MenuLine, SystemLine, MemLine, CPULine, RankLine, LastLine]),
    
    catch ets:insert(Tid, PidList),
    TimeRef = refresh_next_time(Func, Type, Interval),
    receive
        quit -> quit;
        pause_or_resume -> redraw_pause(Home, StableInfo, TimeRef, AutoRow);
        {Func, Type} -> redraw_running(Home, StableInfo, TimeRef, AutoRow, false)
    end.

render_system_line(StableInfo, UseMem, AllocatedMem, UnusedMem, ProcSum) ->
    [Version, ProcLimit, SmpSupport, PortLimit, EtsLimit, LogicalProc, MultiScheduling] = StableInfo,
    UsePercent = observer_cli_lib:to_percent(UseMem / AllocatedMem),
    UnUsePercent = observer_cli_lib:to_percent(UnusedMem / AllocatedMem),
    {PortWarning, ProcWarning, PortCount, ProcCount} = get_port_proc_info(PortLimit, ProcLimit, ProcSum),
    VersionLine = ?render([?W(Version -- "\n", 138)]),
    Title = ?render([?GRAY_BG,
        ?W("System", 10), ?W("Count/Limit", 21),
        ?W("System Switch", 25), ?W("Status", 21),
        ?W("Memory Info", 20), ?W("Size", 26),
        ?RESET]),
    Row1 = ?render([
        ?W("Proc Count", 10), ?W(ProcWarning, ProcCount, 21),
        ?W("Smp Support", 25), ?W(SmpSupport, 21),
        ?W("Allocted Mem", 20), ?W({byte, AllocatedMem}, 17), ?W("100.0%", 6)
    ]),
    Row2 = ?render([
        ?W("Port Count", 10), ?W(PortWarning, PortCount, 21),
        ?W("Multi Scheduling", 25), ?W(MultiScheduling, 21),
        ?W("Use Mem", 20), ?W({byte, UseMem}, 17), ?W(UsePercent, 6)
    ]),
    Row3 = ?render([
        ?UNDERLINE, ?W("Ets Limit", 10), ?W(EtsLimit, 21),
        ?W("Logical Processors", 25), ?W(LogicalProc, 21),
        ?W("Unuse Mem", 20), ?W({byte, UnusedMem}, 17), ?W(UnUsePercent, 6),
        ?RESET]),
    [VersionLine, Title, Row1, Row2, Row3].

render_memory_process_line(ProcSum, MemSum, Interval) ->
    CodeMem = erlang:memory(code),
    [
        {process_count, _ProcC},
        {run_queue, RunQ},
        {error_logger_queue_len, LogQInt},
        {memory_total, TotalMem},
        {memory_procs, ProcMem},
        {memory_atoms, AtomMem},
        {memory_bin, BinMem},
        {memory_ets, EtsMem}|_
    ] = ProcSum,
    [
        {bytes_in, BytesIn},
        {bytes_out, BytesOut},
        {gc_count, GcCount},
        {gc_words_reclaimed, GcWordsReclaimed},
        {reductions, Reductions}|_
    ] = MemSum,
    
    ProcMemPercent = observer_cli_lib:to_percent(ProcMem / TotalMem),
    AtomMemPercent = observer_cli_lib:to_percent(AtomMem / TotalMem),
    BinMemPercent = observer_cli_lib:to_percent(BinMem / TotalMem),
    CodeMemPercent = observer_cli_lib:to_percent(CodeMem / TotalMem),
    EtsMemPercent = observer_cli_lib:to_percent(EtsMem / TotalMem),
    
    Queue = erlang:integer_to_list(RunQ) ++ "/" ++ erlang:integer_to_list(LogQInt),
    Row1 = ?render([
        ?GRAY_BG, ?W("Mem Type", 10), ?W("Size", 21),
        ?W("Mem Type", 25), ?W("Size", 21),
        ?W("IO/GC", 20), ?W(["Interval: ", integer_to_binary(Interval), "ms"], 26),
        ?RESET]),
    Row2 = ?render([
        ?W("Total", 10), ?W({byte, TotalMem}, 12), ?W("100.0%", 6),
        ?W("Binary", 25), ?W({byte, BinMem}, 12), ?W(BinMemPercent, 6),
        ?W("IO Output", 20), ?W({byte, BytesOut}, 26)]),
    Row3 = ?render([
        ?W("Process", 10), ?W({byte, ProcMem}, 12), ?W(ProcMemPercent, 6),
        ?W("Code", 25), ?W({byte, CodeMem}, 12), ?W(CodeMemPercent, 6),
        ?W("IO Input", 20), ?W({byte, BytesIn}, 26)]),
    Row4 = ?render([
        ?W("Atom", 10), ?W({byte, AtomMem}, 12), ?W(AtomMemPercent, 6),
        ?W("Reductions", 25), ?W(Reductions, 21),
        ?W("Gc Count", 20), ?W(GcCount, 26)]),
    Row5 = ?render([
        ?W("Ets", 10), ?W({byte, EtsMem}, 12), ?W(EtsMemPercent, 6),
        ?W("RunQueue/ErrorLoggerQueue", 25), ?W(Queue, 21),
        ?W("Gc Words Reclaimed", 20), ?W(GcWordsReclaimed, 26)]),
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
             CPU1 = observer_cli_lib:to_percent(Percent1),
             CPU2 = observer_cli_lib:to_percent(Percent2),
             Process1 = lists:duplicate(trunc(Percent1 * 57), "|"),
             Process2 = lists:duplicate(trunc(Percent2 * 57), "|"),
             IsLastLine = Seq1 =:= HalfSchedulerNum,
             Format = process_bar_format_style(Percent1, Percent2, IsLastLine),
             io_lib:format(Format, [
                 Seq1, Process1, CPU1,
                 Seq2, Process2, CPU2
             ])
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
             CPU1 = observer_cli_lib:to_percent(Percent1),
             CPU2 = observer_cli_lib:to_percent(Percent2),
             CPU3 = observer_cli_lib:to_percent(Percent3),
             CPU4 = observer_cli_lib:to_percent(Percent4),
             Process1 = lists:duplicate(trunc(Percent1 * 23), "|"),
             Process2 = lists:duplicate(trunc(Percent2 * 22), "|"),
             Process3 = lists:duplicate(trunc(Percent3 * 22), "|"),
             Process4 = lists:duplicate(trunc(Percent4 * 23), "|"),
             IsLastLine = Seq1 =:= PosSchedulerNum,
             Format = process_bar_format_style(Percent1, Percent2, Percent3, Percent4, IsLastLine),
             io_lib:format(Format, [
                 Seq1, Process1, CPU1,
                 Seq2, Process2, CPU2,
                 Seq3, Process3, CPU3,
                 Seq4, Process4, CPU4
             ])
         end || Seq1 <- lists:seq(1, PosSchedulerNum)],
    {PosSchedulerNum, CPU}.

render_top_n_view(memory, MemoryList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Memory", 11), ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MemVal, CurFun, NameOrCall} = get_top_n_info(Pos, MemoryList),
            {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
            Format = get_top_n_format(RankPos, Pos),
            R = io_lib:format(Format,
                [
                    Pos, erlang:pid_to_list(Pid),
                    observer_cli_lib:to_byte(MemVal), NameOrCall,
                    observer_cli_lib:to_list(Reductions),
                    observer_cli_lib:to_list(MsgQueueLen), CurFun
                ]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))),
    {ProcList, [Title | Rows]};
render_top_n_view(binary_memory, MemoryList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "BinMemory", 11), ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MemVal, CurFun, NameOrCall} = get_top_n_info(Pos, MemoryList),
            {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
            Format = get_top_n_format(RankPos, Pos),
            R = io_lib:format(Format,
                [
                    Pos, pid_to_list(Pid),
                    observer_cli_lib:to_byte(MemVal), NameOrCall,
                    observer_cli_lib:to_list(Reductions),
                    observer_cli_lib:to_list(MsgQueueLen), CurFun
                ]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(MemoryList)))),
    {ProcList, [Title | Rows]};
render_top_n_view(reductions, ReductionList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Reductions", 11), ?W("Name or Initial Call", 30),
        ?W("Memory", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, Reductions, CurFun, NameOrCall} = get_top_n_info(Pos, ReductionList),
            {Memory, MsgQueueLen} = get_pid_info(Pid, [memory, message_queue_len]),
            Format = get_top_n_format(RankPos, Pos),
            R = io_lib:format(Format,
                [
                    Pos, pid_to_list(Pid),
                    observer_cli_lib:to_list(Reductions), NameOrCall,
                    observer_cli_lib:to_byte(Memory),
                    observer_cli_lib:to_list(MsgQueueLen), CurFun
                ]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(ReductionList)))),
    {ProcList, [Title | Rows]};
render_top_n_view(total_heap_size, HeapList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "TotalHeap", 11),
        ?W("Name or Initial Call", 30),
        ?W("Reductions", 10), ?W("Msg Queue", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, HeapSize, CurFun, NameOrCall} = get_top_n_info(Pos, HeapList),
            {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
            Format = get_top_n_format(RankPos, Pos),
            R = io_lib:format(Format,
                [
                    Pos, pid_to_list(Pid),
                    observer_cli_lib:to_byte(HeapSize), NameOrCall,
                    observer_cli_lib:to_list(Reductions),
                    observer_cli_lib:to_list(MsgQueueLen), CurFun
                ]),
            {[R | Acc1], [{Pos, Pid} | Acc2]}
                    end, {[], []}, lists:seq(1, erlang:min(Num, erlang:length(HeapList)))),
    {ProcList, [Title | Rows]};
render_top_n_view(message_queue_len, MQLenList, Num, RankPos) ->
    Title = ?render([
        ?W("Pid", 15), ?W(?RED, "Msg Queue", 11), ?W("Name or Initial Call", 30),
        ?W("Memory", 10), ?W("Reductions", 10), ?W("Current Function", 47),
        ?RESET]),
    {Rows, ProcList} =
        lists:foldr(fun(Pos, {Acc1, Acc2}) ->
            {Pid, MQLen, CurFun, NameOrCall} = get_top_n_info(Pos, MQLenList),
            {Reductions, Memory} = get_pid_info(Pid, [reductions, memory]),
            Format = get_top_n_format(RankPos, Pos),
            R = io_lib:format(Format,
                [
                    Pos, pid_to_list(Pid),
                    observer_cli_lib:to_list(MQLen), NameOrCall,
                    observer_cli_lib:to_byte(Memory),
                    observer_cli_lib:to_list(Reductions), CurFun
                ]),
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

get_top_n_format(Pos, Pos) ->
    "|\e[33m~-3.3w|~-12.12s|~12.12s | ~-30.30s | ~11.11s| ~-11.11s| ~-47.47s\e[0m|~n";
get_top_n_format(_Pos, _RankPos) ->
    "|~-3.3w|~-12.12s|~12.12s | ~-30.30s | ~11.11s| ~-11.11s| ~-47.47s|~n".

refresh_next_time(proc_count, Type, Interval) ->
    erlang:send_after(Interval, self(), {proc_count, Type});
refresh_next_time(proc_window, Type, _Interval) ->
    erlang:send_after(100, self(), {proc_window, Type}).

get_current_initial_call(Call) ->
    {_, CurFun} = lists:keyfind(current_function, 1, Call),
    {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
    {observer_cli_lib:mfa_to_list(CurFun), observer_cli_lib:mfa_to_list(InitialCall)}.

get_port_proc_info(PortLimit, ProcLimit, ProcSum) ->
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

process_bar_format_style(Percent1, Percent2, IsLastLine) ->
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
    Format = <<"|", Warning1/binary, "|~2..0w ~-57.57s", "~s", Warning2/binary, " |~2..0w ~-57.57s", " ~s", "  |~n">>,
    case IsLastLine of
        true -> <<?UNDERLINE/binary, Format/binary, ?RESET/binary>>;
        false -> Format
    end.

process_bar_format_style(Percent1, Percent2, Percent3, Percent4, IsLastLine) ->
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
    Format =
        <<"|",
            Warning1/binary, "|~-2.2w ~-23.23s", " ~s",
            Warning2/binary, " |~-2.2w ~-22.22s", " ~s",
            Warning3/binary, " |~-2.2w ~-22.22s", " ~s",
            Warning4/binary, " |~-2.2w ~-23.23s", " ~s",
            " |~n">>,
    case IsLastLine of
        true -> <<?UNDERLINE/binary, Format/binary, ?RESET/binary>>;
        false -> Format
    end.

get_top_n_info(Pos, List) ->
    {Pid, Val, Call = [IsName | _]} = lists:nth(Pos, List),
    {CurFun, InitialCall} = get_current_initial_call(Call),
    NameOrCall = display_name_or_initial_call(IsName, InitialCall),
    {Pid, Val, CurFun, NameOrCall}.

display_name_or_initial_call(IsName, _Call) when is_atom(IsName) -> atom_to_list(IsName);
display_name_or_initial_call(_IsName, Call) -> Call.

get_refresh_prompt(proc_count, Type, Interval, Rows) ->
    io_lib:format("recon:proc_count(~p, ~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_prompt(proc_window, Type, Interval, Rows) ->
    io_lib:format("recon:proc_window(~p, ~w, ~w) Interval:~wms", [Type, Rows, Interval, Interval]).


get_stable_system_info() ->
    [begin observer_cli_lib:to_list(erlang:system_info(Item)) end || Item <- ?STABLE_SYSTEM_KEY].

get_change_system_info() ->
    UsedMem = recon_alloc:memory(used),
    AllocatedMem = recon_alloc:memory(allocated),
    {UsedMem, AllocatedMem, AllocatedMem - UsedMem}.

get_pid_info(Pid, Keys) ->
    case recon:info(Pid, Keys) of
        undefined -> {"die", "die"};
        [{_, Val1}, {_, Val2}] -> {Val1, Val2}
    end.

get_top_n(proc_window, Type, Interval, Rows, IsFirstTime)when not IsFirstTime ->
    recon:proc_window(Type, Rows, Interval);
get_top_n(_Func, Type, _Interval, Rows, _FirstTime) ->
    recon:proc_count(Type, Rows).

connect_error(Prompt, Node) ->
    Prop = <<?RED/binary, Prompt/binary, ?RESET/binary>>,
    ?output(Prop, [Node]).

start_process_view(Tid, Pos, ChildPid, Opts) ->
    case ets:lookup(Tid, Pos) of
        [] -> manager(ChildPid, Opts);
        [{_, ChoosePid}] ->
            erlang:exit(ChildPid, stop),
            NewHomeOpt = Opts#view_opts.home#home{cur_pos = Pos},
            observer_cli_process:start(ChoosePid, Opts#view_opts{home = NewHomeOpt})
    end.

check_auto_row() ->
    case io:rows() of
        {ok, _} -> true;
        {error, _} -> false
    end.
