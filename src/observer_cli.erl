%%% @author zhongwen <zhongwencool@gmail.com>

-module(observer_cli).

-include("observer_cli.hrl").

%% proc_lib:get_label/1 is not exported before OTP 27
-dialyzer([{nowarn_function, [choose_label/1]}]).

-ignore_xref({proc_lib, get_label, 1}).

%% API
-export([start/0]).
-export([start/1]).
-export([start/2]).
-export([start_plugin/0]).
-export([clean/1]).

-ifdef(TEST).

-export([
    render_system_line/2, render_system_line/3,
    render_memory_process_line/3,
    render_scheduler_usage/1,
    render_footer/0,
    render_top_n_view/5, render_top_n_view/6,
    transform_seq/3,
    process_bar_format_style/2,
    warning_color/1,
    format_atom_info/2,
    accept_net_ticktime_result/2,
    get_refresh_prompt/4,
    collect_top_n/5,
    get_current_initial_call/1,
    get_port_proc_info/2,
    get_top_n_info/1,
    display_unique_flag/3,
    get_stable_system_info/0,
    get_atom_status/0,
    get_pid_info/2,
    collect_home_snapshot/6,
    node_stats/2,
    get_incremental_stats/1,
    check_auto_row/0,
    select_home_process/3
]).

-endif.

%% cpu >= this value will be highlight
-define(CPU_ALARM_THRESHOLD, 0.8).
%% port or process reach max_limit * 0.85 will be highlight
-define(COUNT_ALARM_THRESHOLD, 0.85).
-define(LAST_LINE,
    "q(quit) p(pause) r/rr(reduction) m/mm(mem)b/bb(binary mem) "
    "t/tt(total heap size) mq/mmq(msg queue) 9(proc 9 info) F/B(page "
    "forward/back)"
).

-spec start() -> no_return() | {badrpc, term()}.
start() ->
    start(#view_opts{}).

-spec start(Node) -> no_return() | {badrpc, term()} when
    Node :: atom() | non_neg_integer() | view_opts().
start(Node) when Node =:= node() ->
    start(#view_opts{});
start(Node) when is_atom(Node) ->
    rpc_start(Node, ?DEFAULT_INTERVAL);
start(Opts = #view_opts{home = Home}) ->
    erlang:process_flag(trap_exit, true),
    AutoRow = check_auto_row(),
    #home{scheduler_usage = SchUsage} = Home,
    StorePid = observer_cli_store:start(),
    LastSchWallFlag = set_scheduler_wall_time(true, SchUsage),
    PsCmd = io_lib:format("ps -o pcpu,pmem ~s", [os:getpid()]),
    RenderPid = spawn_link(fun() -> render_worker(PsCmd, StorePid, Home, AutoRow) end),
    manager(StorePid, RenderPid, Opts#view_opts{auto_row = AutoRow}, LastSchWallFlag);
start(Interval) when is_integer(Interval), Interval >= ?MIN_INTERVAL ->
    start(#view_opts{
        home = #home{interval = Interval},
        ets = #ets{interval = Interval},
        sys = #system{interval = Interval},
        db = #db{interval = Interval},
        help = #help{interval = Interval},
        inet = #inet{interval = Interval},
        process = #process{interval = Interval},
        port = Interval
    }).

-spec start(Node, Cookies | Options) -> no_return() | {badrpc, term()} when
    Node :: atom(),
    Cookies :: atom(),
    Options :: proplists:proplist().
start(Node, _Cookie) when Node =:= node() ->
    start(#view_opts{});
start(Node, Cookie) when is_atom(Node) andalso is_atom(Cookie) ->
    start(Node, [{cookie, Cookie}]);
start(Node, Options) when is_atom(Node) andalso is_list(Options) ->
    case proplists:get_value(cookie, Options) of
        undefined ->
            ok;
        Cookie ->
            erlang:set_cookie(Node, Cookie)
    end,
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    rpc_start(Node, Interval).

-spec start_plugin() -> no_return().
start_plugin() ->
    erlang:process_flag(trap_exit, true),
    application:ensure_all_started(observer_cli),
    observer_cli_plugin:start(#view_opts{}).

-spec clean(list()) -> boolean().
clean([RenderPid, StorePid, SchWallFlag, SchUsage]) ->
    observer_cli_lib:exit_processes([RenderPid, StorePid]),
    set_scheduler_wall_time(SchWallFlag, SchUsage).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpc_start(Node, Interval) ->
    case net_kernel:hidden_connect_node(Node) of
        true ->
            update_net_ticktime_from(Node),
            rpc:call(Node, ?MODULE, start, [Interval]);
        false ->
            Msg = <<"Node(~p) refuse to be connected, make sure cookie is valid~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown};
        ignored ->
            Msg = <<"Ignored by node(~p), local node is not alive!~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown}
    end.

manager(StorePid, RenderPid, Opts, LastSchWallFlag) ->
    Resource = home_resource(RenderPid, StorePid, LastSchWallFlag, Opts),
    Action = observer_cli_lib:parse_cmd(Opts, ?MODULE, Resource),
    handle_home_action(Action, StorePid, RenderPid, Opts, LastSchWallFlag, Resource).

home_resource(RenderPid, StorePid, LastSchWallFlag, #view_opts{
    home = #home{scheduler_usage = SchUsage}
}) ->
    [RenderPid, StorePid, LastSchWallFlag, SchUsage].

handle_home_action(
    quit,
    StorePid,
    RenderPid,
    #view_opts{
        home =
            #home{
                scheduler_usage = SchUsage
            }
    },
    LastSchWallFlag,
    _Resource
) ->
    erlang:unlink(RenderPid),
    erlang:send(RenderPid, quit),
    set_scheduler_wall_time(LastSchWallFlag, SchUsage),
    observer_cli_lib:exit_processes([StorePid]),
    quit;
handle_home_action(pause_or_resume, StorePid, RenderPid, Opts, LastSchWallFlag, _Resource) ->
    erlang:send(RenderPid, pause_or_resume),
    manager(StorePid, RenderPid, Opts, LastSchWallFlag);
handle_home_action(
    {new_interval, NewInterval},
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
    _LastSchWallFlag,
    Resource
) ->
    restart_home(Opts#view_opts{home = Home#home{interval = NewInterval}}, Resource);
handle_home_action(
    scheduler_usage,
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
    _LastSchWallFlag,
    Resource
) ->
    #home{scheduler_usage = SchUsage} = Home,
    NewSchUsage = toggle_scheduler_usage(SchUsage),
    restart_home(Opts#view_opts{home = Home#home{scheduler_usage = NewSchUsage}}, Resource);
handle_home_action(
    {jump, NewPos},
    StorePid,
    RenderPid,
    Opts = #view_opts{
        home =
            Home = #home{
                cur_page = CurPage,
                pages = Pages
            }
    },
    LastSchWallFlag,
    Resource
) ->
    NewPages = observer_cli_lib:update_page_pos(CurPage, NewPos, Pages),
    NewOpts = Opts#view_opts{home = Home#home{pages = NewPages}},
    start_process_view(StorePid, RenderPid, NewOpts, LastSchWallFlag, Resource, false);
handle_home_action(jump, StorePid, RenderPid, Opts, LastSchWallFlag, Resource) ->
    start_process_view(StorePid, RenderPid, Opts, LastSchWallFlag, Resource, true);
handle_home_action(
    {func, Func, Type},
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
    _LastSchWallFlag,
    Resource
) ->
    restart_home(Opts#view_opts{home = Home#home{func = Func, type = Type}}, Resource);
handle_home_action(page_down_top_n, StorePid, _RenderPid, Opts, _LastSchWallFlag, Resource) ->
    restart_home_page(1, StorePid, Opts, Resource);
handle_home_action(page_up_top_n, StorePid, _RenderPid, Opts, _LastSchWallFlag, Resource) ->
    restart_home_page(-1, StorePid, Opts, Resource);
handle_home_action({go_to_pid, Pid}, _StorePid, _RenderPid, Opts, _LastSchWallFlag, Resource) ->
    open_process_view(Pid, Opts, Resource);
handle_home_action(_Action, StorePid, RenderPid, Opts, LastSchWallFlag, _Resource) ->
    manager(StorePid, RenderPid, Opts, LastSchWallFlag).

toggle_scheduler_usage(SchUsage) ->
    case SchUsage of
        ?DISABLE ->
            ?ENABLE;
        ?ENABLE ->
            ?DISABLE
    end.

restart_home_page(Delta, StorePid, Opts = #view_opts{home = Home}, Resource) ->
    #home{cur_page = CurPage, pages = Pages} = Home,
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
    restart_home(Opts#view_opts{home = Home#home{cur_page = NewPage, pages = NewPages}}, Resource).

restart_home(Opts, Resource) ->
    clean(Resource),
    start(Opts).

render_worker(PsCmd, Manager, Home = #home{scheduler_usage = SchUsage}, AutoRow) ->
    ?output(?CLEAR),
    StableInfo = get_stable_system_info(),
    LastStats = get_incremental_stats(SchUsage),
    redraw_running(
        PsCmd,
        Manager,
        Home,
        StableInfo,
        LastStats,
        erlang:make_ref(),
        AutoRow,
        true
    ).

%% pause status waiting to be resume
redraw_pause(PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow) ->
    notify_pause_status(),
    erlang:cancel_timer(LastTimeRef),
    #home{func = Func, type = Type} = Home,
    receive
        quit ->
            quit;
        {Func, Type} ->
            redraw_running(
                PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow, false
            );
        pause_or_resume ->
            ?output(?CLEAR),
            redraw_running(PsCmd, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow, true)
    end.

%% running status
redraw_running(
    PsCmd,
    StorePid,
    Home,
    StableInfo,
    LastStats,
    LastTimeRef,
    AutoRow,
    IsFirstTime
) ->
    #home{
        interval = Interval,
        func = Func,
        type = Type
    } =
        Home,
    erlang:cancel_timer(LastTimeRef),
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    {Snapshot, NewStats} =
        collect_home_snapshot(PsCmd, Home, StableInfo, LastStats, TerminalRow, IsFirstTime),
    {TopNList, Lines} = render_home_snapshot(Home, Snapshot),
    ?output([?CURSOR_TOP | Lines]),

    observer_cli_store:update(StorePid, maps:get(process_rows, Snapshot), TopNList),
    TimeRef = refresh_next_time(Func, Type, Interval),
    receive
        quit ->
            quit;
        pause_or_resume ->
            redraw_pause(PsCmd, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow);
        {Func, Type} ->
            redraw_running(PsCmd, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow, false)
    end.

collect_home_snapshot(PsCmd, Home, StableInfo, LastStats, TerminalRows, IsFirstTime) ->
    #home{
        interval = Interval,
        scheduler_usage = SchUsage
    } =
        Home,
    {Diffs, SchedulerUsage, NewStats} = node_stats(LastStats, SchUsage),
    ProcessRows = max(
        TerminalRows - 14 - scheduler_usage_rows(SchedulerUsage), 0
    ),
    ProcessRanking = collect_home_processes(Home, ProcessRows, IsFirstTime),
    Runtime = sample_home_runtime(PsCmd, StableInfo, Diffs, SchedulerUsage, Interval),
    {maps:merge(Runtime#{process_rows => ProcessRows}, ProcessRanking), NewStats}.

sample_home_runtime(PsCmd, {StableInfo, PortParallelism}, Diffs, SchedulerUsage, Interval) ->
    #{
        system_summary => system_summary(PsCmd, StableInfo, get_atom_status()),
        memory_summary => memory_process_summary(Diffs, PortParallelism, Interval),
        scheduler_usage => SchedulerUsage
    }.

collect_home_processes(
    #home{interval = Interval, func = Func, type = Type, cur_page = CurPage},
    ProcessRows,
    IsFirstTime
) ->
    TopLen = ProcessRows * CurPage,
    #{
        top_processes => collect_top_n(Func, Type, Interval, TopLen, IsFirstTime),
        refresh_prompt => get_refresh_prompt(Func, Type, Interval, TopLen)
    }.

render_home_snapshot(Home, Snapshot) ->
    #home{
        type = Type,
        pages = RankPos,
        cur_page = CurPage
    } =
        Home,
    ProcessRows = maps:get(process_rows, Snapshot),
    TopList = maps:get(top_processes, Snapshot),
    Text = maps:get(refresh_prompt, Snapshot),
    {_, CPULine} = render_scheduler_usage(maps:get(scheduler_usage, Snapshot)),
    {TopNList, RankLine} = render_top_n_view(Type, TopList, ProcessRows, RankPos, CurPage),
    {
        TopNList,
        [
            observer_cli_lib:render_top_menu(home, Text),
            render_home_summary(maps:get(system_summary, Snapshot)),
            render_home_summary(maps:get(memory_summary, Snapshot)),
            CPULine,
            RankLine,
            render_footer()
        ]
    }.

render_footer() ->
    observer_cli_lib:render_footer(?LAST_LINE).

-ifdef(TEST).

render_system_line(PsCmd, StableInfo) ->
    render_system_line(PsCmd, StableInfo, get_atom_status()).

render_system_line(PsCmd, StableInfo, AtomStatus) ->
    render_home_summary(system_summary(PsCmd, StableInfo, AtomStatus)).

-endif.

system_summary(PsCmd, StableInfo, AtomStatus) ->
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra} = home_summary_extras(),
    [Version, SysVersion, ProcLimit, PortLimit, EtsLimit] = StableInfo,
    ActiveTask = erlang:statistics(total_active_tasks),
    {ContextSwitch, _} = erlang:statistics(context_switches),
    Reductions = erlang:statistics(reductions),
    {PortWarning, ProcWarning, PortCount, ProcCount} =
        get_port_proc_info(PortLimit, ProcLimit),
    CmdValue =
        case
            string:split(
                os:cmd(PsCmd), "\n", all
            )
        of
            [_, CmdValueTmp | _] ->
                CmdValueTmp;
            _ ->
                ""
        end,

    [CpuPsV, MemPsV] =
        case lists:filter(fun(Y) -> Y =/= [] end, string:split(CmdValue, " ", all)) of
            [V1, V2] ->
                [V1, V2];
            _ ->
                ["--", "--"]
        end,
    {Reds, AddReds} = Reductions,
    ReductionsText = [integer_to_list(Reds), "/", integer_to_list(AddReds)],
    [
        [
            {normal, [{SysVersion, observer_cli_lib:layout_width() - 3}]},
            {?GRAY_BG, [
                {"System", 10 + LeftLabelExtra},
                {"Count/Limit", 21 + LeftValueExtra},
                {"System", 25 + MiddleLabelExtra},
                {"Status", 21 + MiddleValueExtra},
                {"Stat Info", 20 + RightLabelExtra},
                {"Size", 25 + RightValueExtra}
            ]}
        ],
        [
            {normal, [
                {"Proc Count", 10 + LeftLabelExtra},
                {ProcWarning, ProcCount, 22 + LeftValueExtra},
                {" Version", 26 + MiddleLabelExtra},
                {Version, 21 + MiddleValueExtra},
                {"Active Task", 20 + RightLabelExtra},
                {ActiveTask, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Port Count", 10 + LeftLabelExtra},
                {PortWarning, PortCount, 22 + LeftValueExtra},
                {" ps -o pcpu", 26 + MiddleLabelExtra},
                {[CpuPsV, "%"], 21 + MiddleValueExtra},
                {"Context Switch", 20 + RightLabelExtra},
                {ContextSwitch, 24 + RightValueExtra}
            ]}
        ],
        [
            system_atom_summary_row(
                AtomStatus,
                EtsLimit,
                MemPsV,
                ReductionsText,
                {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra,
                    RightLabelExtra, RightValueExtra}
            )
        ]
    ].

system_atom_summary_row(
    {ok, AtomLimit, AtomCount},
    _EtsLimit,
    MemPsV,
    ReductionsText,
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra}
) ->
    {AtomWarning, Atom} = format_atom_info(AtomLimit, AtomCount),
    {?UNDERLINE, [
        {"Atom Count", 10 + LeftLabelExtra},
        {AtomWarning, Atom, 22 + LeftValueExtra},
        {" ps -o pmem", 26 + MiddleLabelExtra},
        {[MemPsV, "%"], 21 + MiddleValueExtra},
        {"Reds(Total/SinceLastCall)", 20 + RightLabelExtra},
        {ReductionsText, 24 + RightValueExtra}
    ]};
system_atom_summary_row(
    {error, unsupported},
    EtsLimit,
    MemPsV,
    ReductionsText,
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra}
) ->
    {?UNDERLINE, [
        {"Ets Limit", 10 + LeftLabelExtra},
        {EtsLimit, 21 + LeftValueExtra},
        {" ps -o pmem", 25 + MiddleLabelExtra},
        {[MemPsV, "%"], 21 + MiddleValueExtra},
        {"Reductions", 20 + RightLabelExtra},
        {ReductionsText, 24 + RightValueExtra}
    ]}.

-ifdef(TEST).

render_memory_process_line(MemSum, PortParallelism, Interval) ->
    render_home_summary(memory_process_summary(MemSum, PortParallelism, Interval)).

-endif.

memory_process_summary(MemSum, PortParallelism, Interval) ->
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra} = home_summary_extras(),
    RunQ = erlang:statistics(run_queue),
    Mem = erlang:memory(),
    TotalMem = proplists:get_value(total, Mem),
    ProcMem = proplists:get_value(processes_used, Mem),
    CodeMem = proplists:get_value(code, Mem),
    AtomMem = proplists:get_value(atom_used, Mem),
    BinMem = proplists:get_value(binary, Mem),
    EtsMem = proplists:get_value(ets, Mem),
    EtsLen =
        erlang:length(
            ets:all()
        ),
    {BytesIn, BytesOut, GcCount, GcWordsReclaimed} = MemSum,

    {Queue, LogKey} =
        case whereis(error_logger) of
            undefined ->
                {erlang:integer_to_list(RunQ), "RunQueue"};
            Pid ->
                {_, Q} = process_info(Pid, message_queue_len),
                {
                    [erlang:integer_to_list(RunQ), "/", erlang:integer_to_list(Q)],
                    "RunQueue/ErrorLoggerQueue"
                }
        end,
    ProcMemPercent = observer_cli_lib:to_percent(ProcMem / TotalMem),
    AtomMemPercent = observer_cli_lib:to_percent(AtomMem / TotalMem),
    BinMemPercent = observer_cli_lib:to_percent(BinMem / TotalMem),
    CodeMemPercent = observer_cli_lib:to_percent(CodeMem / TotalMem),
    EtsMemPercent = observer_cli_lib:to_percent(EtsMem / TotalMem),
    [
        [
            {?GRAY_BG, [
                {"Mem Type", 10 + LeftLabelExtra},
                {"Size", 21 + LeftValueExtra},
                {"Mem Type", 25 + MiddleLabelExtra},
                {"Size", 21 + MiddleValueExtra},
                {["IO/GC:(", integer_to_binary(Interval), "ms)"], 20 + RightLabelExtra},
                {"Total/Increments", 25 + RightValueExtra}
            ]}
        ],
        [
            {normal, [
                {"Total", 10 + LeftLabelExtra},
                {{byte, TotalMem}, 12},
                {"100.0%", 6 + LeftValueExtra},
                {"Binary", 25 + MiddleLabelExtra},
                {{byte, BinMem}, 12},
                {BinMemPercent, 6 + MiddleValueExtra},
                {"IO Output", 20 + RightLabelExtra},
                {BytesOut, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Process", 10 + LeftLabelExtra},
                {{byte, ProcMem}, 12},
                {ProcMemPercent, 6 + LeftValueExtra},
                {"Code", 25 + MiddleLabelExtra},
                {{byte, CodeMem}, 12},
                {CodeMemPercent, 6 + MiddleValueExtra},
                {"IO Input", 20 + RightLabelExtra},
                {BytesIn, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Atom", 10 + LeftLabelExtra},
                {{byte, AtomMem}, 12},
                {AtomMemPercent, 6 + LeftValueExtra},
                {"Port Parallelism (+spp)", 25 + MiddleLabelExtra},
                {PortParallelism, 21 + MiddleValueExtra},
                {"Gc Count", 20 + RightLabelExtra},
                {GcCount, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Ets/" ++ erlang:integer_to_list(EtsLen), 10 + LeftLabelExtra},
                {{byte, EtsMem}, 12},
                {EtsMemPercent, 6 + LeftValueExtra},
                {LogKey, 25 + MiddleLabelExtra},
                {Queue, 21 + MiddleValueExtra},
                {"Gc Words Reclaimed", 20 + RightLabelExtra},
                {GcWordsReclaimed, 24 + RightValueExtra}
            ]}
        ]
    ].

render_home_summary(Blocks) ->
    [render_home_summary_block(Block) || Block <- Blocks].

render_home_summary_block(Rows) ->
    ?render(join_home_summary_rows(Rows)).

join_home_summary_rows([]) ->
    [];
join_home_summary_rows([Row]) ->
    render_home_summary_row(Row);
join_home_summary_rows([Row | Rows]) ->
    render_home_summary_row(Row) ++ [?NEW_LINE | join_home_summary_rows(Rows)].

render_home_summary_row({normal, Cells}) ->
    render_home_summary_cells(Cells);
render_home_summary_row({Style, Cells}) ->
    [Style | render_home_summary_cells(Cells)].

render_home_summary_cells(Cells) ->
    [render_home_summary_cell(Cell) || Cell <- Cells].

render_home_summary_cell({Value, Width}) ->
    ?W(Value, Width);
render_home_summary_cell({Color, Value, Width}) ->
    ?W2(Color, Value, Width).

home_summary_extras() ->
    Extra = observer_cli_lib:layout_extra_width(observer_cli_lib:layout_base_width() + 1),
    PerColumn = Extra div 6,
    Remainder = Extra rem 6,
    RawMiddleValueExtra = PerColumn + extra_bit(Remainder, 4),
    RightValueShift = erlang:min(RawMiddleValueExtra, 1),
    {
        PerColumn + extra_bit(Remainder, 1),
        PerColumn + extra_bit(Remainder, 2),
        PerColumn + extra_bit(Remainder, 3),
        RawMiddleValueExtra - RightValueShift,
        PerColumn + extra_bit(Remainder, 5),
        PerColumn + RightValueShift
    }.

extra_bit(Rem, Pos) when Rem >= Pos ->
    1;
extra_bit(_Rem, _Pos) ->
    0.

scheduler_usage_rows(undefined) ->
    0;
scheduler_usage_rows(SchedulerUsage) ->
    scheduler_usage_rows_by_count(erlang:length(SchedulerUsage)).

scheduler_usage_rows_by_count(SchedulerNum) when SchedulerNum < 8 ->
    ceil_div(SchedulerNum, 2);
scheduler_usage_rows_by_count(SchedulerNum) when SchedulerNum =< 100 ->
    ceil_div(SchedulerNum, 4);
scheduler_usage_rows_by_count(SchedulerNum) ->
    ceil_div(SchedulerNum, 10).

ceil_div(0, _Divisor) ->
    0;
ceil_div(Number, Divisor) ->
    (Number + Divisor - 1) div Divisor.

render_scheduler_usage(undefined) ->
    {0, []};
render_scheduler_usage(SchedulerUsage) ->
    SchedulerNum = erlang:length(SchedulerUsage),
    render_scheduler_usage(SchedulerUsage, SchedulerNum).

%% < 8 core split 2 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum < 8 ->
    Column = scheduler_usage_rows_by_count(SchedulerNum),
    CPU =
        [
            begin
                Seq2 = transform_seq(Seq1, Column, SchedulerNum),
                Percent1 = proplists:get_value(Seq1, SchedulerUsage, 0.0),
                Percent2 = proplists:get_value(Seq2, SchedulerUsage, 0.0),
                CPU1 = observer_cli_lib:to_percent(Percent1),
                CPU2 = observer_cli_lib:to_percent(Percent2),
                Process1 = lists:duplicate(trunc(Percent1 * 57), "|"),
                Process2 = lists:duplicate(trunc(Percent2 * 57), "|"),
                IsLastLine = Seq1 =:= Column,
                Format = process_bar_format_style([Percent1, Percent2], IsLastLine),
                io_lib:format(Format, [Seq1, Process1, CPU1, Seq2, Process2, CPU2])
            end
         || Seq1 <- lists:seq(1, Column)
        ],
    {Column, pad_scheduler_lines(CPU)};
%% 100 >= scheduler >= 8 split 4 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum =< 100 ->
    Column = scheduler_usage_rows_by_count(SchedulerNum),
    CPU =
        [
            begin
                Seq2 = transform_seq(Seq1, Column, SchedulerNum),
                Seq3 = transform_seq(Seq2, Column, SchedulerNum),
                Seq4 = transform_seq(Seq3, Column, SchedulerNum),
                Percent1 = proplists:get_value(Seq1, SchedulerUsage, 0.0),
                Percent2 = proplists:get_value(Seq2, SchedulerUsage, 0.0),
                Percent3 = proplists:get_value(Seq3, SchedulerUsage, 0.0),
                Percent4 = proplists:get_value(Seq4, SchedulerUsage, 0.0),
                CPU1 = observer_cli_lib:to_percent(Percent1),
                CPU2 = observer_cli_lib:to_percent(Percent2),
                CPU3 = observer_cli_lib:to_percent(Percent3),
                CPU4 = observer_cli_lib:to_percent(Percent4),
                Process1 = lists:duplicate(trunc(Percent1 * 22), "|"),
                Process2 = lists:duplicate(trunc(Percent2 * 22), "|"),
                Process3 = lists:duplicate(trunc(Percent3 * 22), "|"),
                Process4 = lists:duplicate(trunc(Percent4 * 23), "|"),
                IsLastLine = Seq1 =:= Column,
                Format = process_bar_format_style(
                    [Percent1, Percent2, Percent3, Percent4], IsLastLine
                ),
                io_lib:format(
                    Format,
                    [
                        Seq1,
                        Process1,
                        CPU1,
                        Seq2,
                        Process2,
                        CPU2,
                        Seq3,
                        Process3,
                        CPU3,
                        Seq4,
                        Process4,
                        CPU4
                    ]
                )
            end
         || Seq1 <- lists:seq(1, Column)
        ],
    {Column, pad_scheduler_lines(CPU)};
%% scheduler > 100 don't show process bar.
render_scheduler_usage(SchedulerUsage, SchedulerNum) ->
    Column = scheduler_usage_rows_by_count(SchedulerNum),
    CPU =
        [
            begin
                Seq2 = transform_seq(Seq1, Column, SchedulerNum),
                Seq3 = transform_seq(Seq2, Column, SchedulerNum),
                Seq4 = transform_seq(Seq3, Column, SchedulerNum),
                Seq5 = transform_seq(Seq4, Column, SchedulerNum),
                Seq6 = transform_seq(Seq5, Column, SchedulerNum),
                Seq7 = transform_seq(Seq6, Column, SchedulerNum),
                Seq8 = transform_seq(Seq7, Column, SchedulerNum),
                Seq9 = transform_seq(Seq8, Column, SchedulerNum),
                Seq10 = transform_seq(Seq9, Column, SchedulerNum),
                Percent1 = proplists:get_value(Seq1, SchedulerUsage),
                Percent2 = proplists:get_value(Seq2, SchedulerUsage),
                Percent3 = proplists:get_value(Seq3, SchedulerUsage),
                Percent4 = proplists:get_value(Seq4, SchedulerUsage),
                Percent5 = proplists:get_value(Seq5, SchedulerUsage),
                Percent6 = proplists:get_value(Seq6, SchedulerUsage),
                Percent7 = proplists:get_value(Seq7, SchedulerUsage),
                Percent8 = proplists:get_value(Seq8, SchedulerUsage),
                Percent9 = proplists:get_value(Seq9, SchedulerUsage),
                Percent10 = proplists:get_value(Seq10, SchedulerUsage),
                CPU1 = observer_cli_lib:to_percent(Percent1),
                CPU2 = observer_cli_lib:to_percent(Percent2),
                CPU3 = observer_cli_lib:to_percent(Percent3),
                CPU4 = observer_cli_lib:to_percent(Percent4),
                CPU5 = observer_cli_lib:to_percent(Percent5),
                CPU6 = observer_cli_lib:to_percent(Percent6),
                CPU7 = observer_cli_lib:to_percent(Percent7),
                CPU8 = observer_cli_lib:to_percent(Percent8),
                CPU9 = observer_cli_lib:to_percent(Percent9),
                CPU10 = observer_cli_lib:to_percent(Percent10),
                IsLastLine = Seq1 =:= Column,
                Percents =
                    [
                        Percent1,
                        Percent2,
                        Percent3,
                        Percent4,
                        Percent5,
                        Percent6,
                        Percent7,
                        Percent8,
                        Percent9,
                        Percent10
                    ],
                Format = process_bar_format_style(Percents, IsLastLine),
                io_lib:format(
                    Format,
                    [
                        Seq1,
                        CPU1,
                        Seq2,
                        CPU2,
                        Seq3,
                        CPU3,
                        Seq4,
                        CPU4,
                        Seq5,
                        CPU5,
                        Seq6,
                        CPU6,
                        Seq7,
                        CPU7,
                        Seq8,
                        CPU8,
                        Seq9,
                        CPU9,
                        Seq10,
                        CPU10
                    ]
                )
            end
         || Seq1 <- lists:seq(1, Column)
        ],
    {Column, pad_scheduler_lines(CPU)}.

pad_scheduler_lines(Lines) ->
    [observer_cli_lib:pad_rendered(Line) || Line <- Lines].

transform_seq(Seq, Column, Total) ->
    Num = Seq + Column,
    case Num > Total of
        true ->
            1000;
        false ->
            Num
    end.

render_top_n_view(Type, List, Num, Pages, Page) ->
    render_top_n_view(Type, List, Num, Pages, Page, observer_cli_lib:layout_width()).

render_top_n_view(Type, List, Num, Pages, Page, LayoutWidth) ->
    Spec = top_n_spec(Type),
    #{text_widths := {NameBaseWidth, CurrentTitleBaseWidth, CurrentBaseWidth}} = Spec,
    {NameWidth, CurrentTitleWidth, CurrentWidth} =
        top_n_text_widths(NameBaseWidth, CurrentTitleBaseWidth, CurrentBaseWidth, LayoutWidth),
    Title = render_top_n_title(Spec, NameWidth, CurrentTitleWidth),
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(List)),
    FormatFunc =
        fun(Item, {Acc, Acc1, Pos}) ->
            {Pid, Row} = render_top_n_row(
                Type, Spec, Item, ChoosePos, Pos, NameWidth, CurrentWidth
            ),
            {[Row | Acc], [{Pos, Pid} | Acc1], Pos + 1}
        end,
    {Rows, PidList} = top_n_rows(FormatFunc, Start, lists:sublist(List, Start, Num)),
    {PidList, [Title | lists:reverse(Rows)]}.

top_n_spec(memory) ->
    #{
        text_widths => {45, 32, 33},
        metric_columns => [
            {"     Memory", 14},
            {"    Reductions", 14},
            {" MsgQueue", 10}
        ],
        row_formats => {"~13.13s ", "~14.14s", " ~-9.9s"}
    };
top_n_spec(binary_memory) ->
    #{
        text_widths => {45, 32, 33},
        metric_columns => [
            {"  BinMemory", 14},
            {"    Reductions", 14},
            {" MsgQueue", 10}
        ],
        row_formats => {"~13.13s ", "~14.14s", " ~-9.9s"}
    };
top_n_spec(reductions) ->
    #{
        text_widths => {45, 33, 34},
        metric_columns => [
            {"   Reductions", 15},
            {"      Memory", 13},
            {" MsgQueue", 10}
        ],
        row_formats => {"~-15.15s", "~12.12s", " ~-9.9s"}
    };
top_n_spec(total_heap_size) ->
    #{
        text_widths => {45, 32, 33},
        metric_columns => [
            {" TotalHeapSize", 14},
            {"    Reductions", 14},
            {" MsgQueue", 10}
        ],
        row_formats => {"~13.13s ", "~14.14s", " ~-9.9s"}
    };
top_n_spec(message_queue_len) ->
    #{
        text_widths => {44, 33, 34},
        metric_columns => [
            {" MsgQueue", 11},
            {"      Memory", 13},
            {"    Reductions", 14}
        ],
        row_formats => {"~-11.11s", "~13.13s", " ~-13.13s"}
    }.

render_top_n_title(Spec, NameWidth, CurrentTitleWidth) ->
    #{
        metric_columns := [
            {ValueTitle, ValueWidth},
            {MiddleTitle, MiddleWidth},
            {RightTitle, RightWidth}
        ]
    } = Spec,
    ?render([
        ?W2(?GRAY_BG, "No | Pid", 16),
        ?W2(?RED_BG, ValueTitle, ValueWidth),
        ?W(?GRAY_BG, "Name|>Label|>Initial Call", NameWidth),
        ?W(?GRAY_BG, MiddleTitle, MiddleWidth),
        ?W(?GRAY_BG, RightTitle, RightWidth),
        ?W(?GRAY_BG, "Current Function", CurrentTitleWidth)
    ]).

render_top_n_row(Type, Spec, Item, ChoosePos, Pos, NameWidth, CurrentWidth) ->
    {Pid, Val, CurFun, NameOrCall} = get_top_n_info(Item),
    {Value, Middle, Right} = top_n_row_values(Type, Pid, Val),
    Format = get_rank_format(Spec, ChoosePos, Pos, NameWidth, CurrentWidth),
    Row = io_lib:format(
        Format,
        [
            Pos,
            erlang:pid_to_list(Pid),
            Value,
            NameOrCall,
            Middle,
            Right,
            CurFun
        ]
    ),
    {Pid, Row}.

top_n_row_values(reductions, Pid, Reductions) ->
    {Memory, MsgQueueLen} = get_pid_info(Pid, [memory, message_queue_len]),
    {
        observer_cli_lib:to_list(Reductions),
        observer_cli_lib:to_byte(Memory),
        observer_cli_lib:to_list(MsgQueueLen)
    };
top_n_row_values(message_queue_len, Pid, MQLen) ->
    {Reductions, Memory} = get_pid_info(Pid, [reductions, memory]),
    {
        observer_cli_lib:to_list(MQLen),
        observer_cli_lib:to_byte(Memory),
        observer_cli_lib:to_list(Reductions)
    };
top_n_row_values(_Type, Pid, Bytes) ->
    {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
    {
        observer_cli_lib:to_byte(Bytes),
        observer_cli_lib:to_list(Reductions),
        observer_cli_lib:to_list(MsgQueueLen)
    }.

top_n_rows(FormatFunc, Start, List) ->
    {Row, PidList, _} = lists:foldl(FormatFunc, {[], [], Start}, List),
    {Row, PidList}.

top_n_text_widths(NameWidth, CurrentTitleWidth, CurrentWidth, LayoutWidth) ->
    Extra = observer_cli_lib:layout_extra_width(LayoutWidth, observer_cli_lib:layout_base_width()),
    NameExtra = Extra div 2,
    CurrentExtra = Extra - NameExtra,
    {NameWidth + NameExtra, CurrentTitleWidth + CurrentExtra, CurrentWidth + CurrentExtra}.

notify_pause_status() ->
    ?output(
        "\e[31;1m PAUSE  INPUT (p, r/rr, b/bb, h/hh, m/mm) to resume "
        "or q to quit \e[0m\n"
    ).

get_rank_format(Spec, ChoosePos, Pos, NameWidth, CurrentWidth) ->
    #{row_formats := {ValueFormat, MiddleFormat, RightFormat}} = Spec,
    {SelectedFormat, NormalFormat} =
        rank_format(ValueFormat, MiddleFormat, RightFormat, NameWidth, CurrentWidth),
    case Pos =:= ChoosePos of
        true ->
            SelectedFormat;
        false ->
            NormalFormat
    end.

rank_format(ValueFormat, MidFormat, QueueFormat, NameWidth, CurrentWidth) ->
    NameBin = integer_to_list(NameWidth),
    CurrentBin = integer_to_list(CurrentWidth),
    Normal =
        [
            "|~-3.3w|~-12.12s|",
            ValueFormat,
            "|~-",
            NameBin,
            ".",
            NameBin,
            "s|",
            MidFormat,
            "|",
            QueueFormat,
            "|~-",
            CurrentBin,
            ".",
            CurrentBin,
            "s|~n"
        ],
    Selected =
        [
            "|\e[7m~-3.3w|~-12.12s|",
            ValueFormat,
            "|~-",
            NameBin,
            ".",
            NameBin,
            "s|",
            MidFormat,
            "|",
            QueueFormat,
            "|~-",
            CurrentBin,
            ".",
            CurrentBin,
            "s\e[0m|~n"
        ],
    {lists:flatten(Selected), lists:flatten(Normal)}.

refresh_next_time(proc_count, Type, Interval) ->
    erlang:send_after(Interval, self(), {proc_count, Type});
refresh_next_time(proc_window, Type, _Interval) ->
    erlang:send_after(10, self(), {proc_window, Type}).

get_current_initial_call(Call) ->
    {_, CurFun} = lists:keyfind(current_function, 1, Call),
    {_, InitialCall} = lists:keyfind(initial_call, 1, Call),
    {observer_cli_lib:mfa_to_list(CurFun), InitialCall}.

get_port_proc_info(PortLimit, ProcLimit) ->
    ProcCount = erlang:system_info(process_count),
    PortCount = erlang:system_info(port_count),
    PortCountStr = [integer_to_list(PortCount), "/", integer_to_list(PortLimit)],
    ProcCountStr = [integer_to_list(ProcCount), "/", integer_to_list(ProcLimit)],
    PortWarning =
        case PortCount > PortLimit * ?COUNT_ALARM_THRESHOLD of
            true ->
                ?RED;
            false ->
                <<"">>
        end,
    ProcWarning =
        case ProcCount > ProcLimit * ?COUNT_ALARM_THRESHOLD of
            true ->
                ?RED;
            false ->
                <<"">>
        end,
    {PortWarning, ProcWarning, PortCountStr, ProcCountStr}.

format_atom_info(AtomLimit, AtomCount) ->
    Atom = [integer_to_list(AtomCount), "/", integer_to_list(AtomLimit)],
    case AtomCount > AtomLimit * ?COUNT_ALARM_THRESHOLD of
        true ->
            {?RED, Atom};
        false ->
            {<<"">>, Atom}
    end.

warning_color(Percent) when Percent >= ?CPU_ALARM_THRESHOLD ->
    ?RED;
warning_color(_Percent) ->
    ?GREEN.

process_bar_format_style(Percents, IsLastLine) ->
    Format =
        case [warning_color(P) || P <- Percents] of
            [W1, W2] ->
                <<"|", W1/binary, "|~2..0w ~-57.57s~s", W2/binary,
                    " |~2..0w ~-57.57s ~s \e[0m|~n">>;
            [W1, W2, W3, W4] ->
                <<"|", W1/binary, "|~-2.2w ~-22.22s ~s", W2/binary, " |~-2.2w ~-22.22s ~s",
                    W3/binary, " |~-2.2w ~-22.22s ~s", W4/binary, " |~-2.2w ~-23.23s ~s \e[0m|~n">>;
            [W1, W2, W3, W4, W5, W6, W7, W8, W9, W10] ->
                <<"|", W1/binary, " | ~-3.3w ~s", W2/binary, " | ~-3.3w ~s", W3/binary,
                    " | ~-3.3w ~s", W4/binary, " | ~-3.3w ~s", W5/binary, " | ~-3.3w ~s", W6/binary,
                    " |=====| ~-3.3w ~s", W7/binary, " | ~-3.3w ~s", W8/binary, " | ~-3.3w ~s",
                    W9/binary, " | ~-3.3w ~s", W10/binary, " | ~-3.3w ~s \e[0m|~n">>
        end,
    case IsLastLine of
        true ->
            <<?UNDERLINE/binary, Format/binary>>;
        false ->
            Format
    end.

get_top_n_info(Item) ->
    {Pid, Val, Call = [IsName | _]} = Item,
    {CurFun, InitialCall} = get_current_initial_call(Call),
    Flag = display_unique_flag(IsName, InitialCall, Pid),
    {Pid, Val, CurFun, Flag}.

display_unique_flag(IsName, Call, Pid) ->
    case choose_name(IsName) of
        undefined ->
            case choose_label(Pid) of
                undefined ->
                    choose_call(Call, Pid);
                Label ->
                    Label
            end;
        Name ->
            Name
    end.

choose_name(IsName) when is_atom(IsName) ->
    atom_to_list(IsName);
choose_name(_) ->
    undefined.

choose_label(Pid) ->
    case erlang:function_exported(proc_lib, get_label, 1) andalso proc_lib:get_label(Pid) of
        false ->
            undefined;
        undefined ->
            undefined;
        Label ->
            io_lib:format("~p", [Label])
    end.

choose_call({proc_lib, init_p, 5}, Pid) ->
    %% translate gen_xxx behavior
    observer_cli_lib:mfa_to_list(
        proc_lib:translate_initial_call(Pid)
    );
choose_call(Call, _Pid) ->
    observer_cli_lib:mfa_to_list(Call).

get_refresh_prompt(proc_count, Type, Interval, Rows) ->
    io_lib:format("recon:proc_count(~p, ~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_prompt(proc_window, Type, Interval, Rows) ->
    io_lib:format(
        "recon:proc_window(~p, ~w, ~w) Interval:~wms",
        [Type, Rows, Interval, Interval]
    ).

get_stable_system_info() ->
    OtpRelease = erlang:system_info(otp_release),
    SysVersion = erlang:system_info(system_version) -- "\n",
    {
        [
            OtpRelease,
            SysVersion ++ " " ++ atom_to_list(erlang:node()),
            erlang:system_info(process_limit),
            erlang:system_info(port_limit),
            erlang:system_info(ets_limit)
        ],
        erlang:system_info(port_parallelism)
    }.

get_atom_status() ->
    try erlang:system_info(atom_limit) of
        Limit ->
            Count = erlang:system_info(atom_count),
            {ok, Limit, Count}
    catch
        _:badarg ->
            {error, unsupported}
    end.

get_pid_info(Pid, Keys) ->
    case recon:info(Pid, Keys) of
        undefined ->
            {"dead", "dead"};
        [{_, Val1}, {_, Val2}] ->
            {Val1, Val2}
    end.

collect_top_n(proc_window, Type, Interval, Rows, IsFirstTime) when not IsFirstTime ->
    recon:proc_window(Type, Rows, Interval);
collect_top_n(_Func, Type, _Interval, Rows, _FirstTime) ->
    recon:proc_count(Type, Rows).

connect_error(Prompt, Node) ->
    ?output(observer_cli_lib:ansi_red(Prompt), [Node]).

start_process_view(
    StorePid,
    RenderPid,
    Opts,
    LastSchWallFlag,
    Resource,
    AutoJump
) ->
    case select_home_process(StorePid, Opts, AutoJump) of
        {ok, ChoosePid} ->
            open_process_view(ChoosePid, Opts, Resource);
        error ->
            manager(StorePid, RenderPid, Opts, LastSchWallFlag)
    end.

select_home_process(StorePid, #view_opts{home = Home}, AutoJump) ->
    #home{
        cur_page = CurPage,
        pages = Pages
    } =
        Home,
    {_, CurPos} = lists:keyfind(CurPage, 1, Pages),
    case observer_cli_store:lookup_pos(StorePid, CurPos) of
        {CurPos, ChoosePid} ->
            {ok, ChoosePid};
        {_, ChoosePid} when AutoJump ->
            {ok, ChoosePid};
        _ ->
            error
    end.

open_process_view(Pid, Opts, Resource) ->
    clean(Resource),
    observer_cli_process:start(home, Pid, Opts).

set_scheduler_wall_time(_Flag, ?DISABLE) ->
    false;
set_scheduler_wall_time(Flag, ?ENABLE) ->
    erlang:system_flag(scheduler_wall_time, Flag).

check_auto_row() ->
    case io:rows() of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

node_stats(LastStats, SchUsage) ->
    New = get_incremental_stats(SchUsage),
    {
        io_gc_stats_diff(LastStats, New),
        scheduler_usage_diff(LastStats, New),
        New
    }.

io_gc_stats_diff({LastIn, LastOut, LastGCs, LastWords, _}, {In, Out, GCs, Words, _}) ->
    BytesInDiff = In - LastIn,
    BytesOutDiff = Out - LastOut,
    GCCountDiff = GCs - LastGCs,
    GCWordsDiff = Words - LastWords,
    {
        [observer_cli_lib:to_byte(In), "/", observer_cli_lib:to_byte(BytesInDiff)],
        [observer_cli_lib:to_byte(Out), "/", observer_cli_lib:to_byte(BytesOutDiff)],
        [integer_to_list(GCs), "/", integer_to_list(GCCountDiff)],
        [integer_to_list(Words), "/", integer_to_list(GCWordsDiff)]
    }.

scheduler_usage_diff({_, _, _, _, LastScheduleWall}, {_, _, _, _, ScheduleWall}) ->
    recon_lib:scheduler_usage_diff(LastScheduleWall, ScheduleWall).

get_incremental_stats(SchUsage) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    ScheduleWall =
        case SchUsage of
            ?ENABLE ->
                erlang:statistics(scheduler_wall_time);
            ?DISABLE ->
                undefined
        end,
    {In, Out, GCs, Words, ScheduleWall}.

update_net_ticktime_from(Node) ->
    NetTickTime = rpc:call(Node, net_kernel, get_net_ticktime, []),
    accept_net_ticktime_result(net_kernel:set_net_ticktime(NetTickTime), NetTickTime).

accept_net_ticktime_result(change_initiated, _NetTickTime) ->
    ok;
accept_net_ticktime_result({ongoing_change_to, NetTickTime}, NetTickTime) ->
    ok;
accept_net_ticktime_result(unchanged, _NetTickTime) ->
    ok.
