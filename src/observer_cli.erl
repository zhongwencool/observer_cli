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
    render_memory_process_line/2,
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
    process_window/4,
    process_sample_status/3,
    redraw_pause/7,
    get_current_initial_call/1,
    get_port_proc_info/2,
    get_top_n_info/1,
    display_unique_flag/3,
    get_stable_system_info/0,
    get_atom_status/0,
    get_pid_info/2,
    collect_home_snapshot/6,
    node_stats/2,
    scheduler_stats/2,
    render_scheduler_summary/1,
    run_home_worker/4,
    get_incremental_stats/1,
    check_auto_row/0,
    select_home_process/3,
    join_home_summary_rows/1,
    scheduler_usage_rows/1,
    update_net_ticktime_from/1
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

-spec start() -> quit.
start() ->
    start(#view_opts{}).

%% Remote starts pass through RPC replies; the caller must validate their outcome.
-spec start(Node) -> term() when
    Node :: atom() | non_neg_integer() | view_opts().
start(Node) when Node =:= node() ->
    start(#view_opts{});
start(Node) when is_atom(Node) ->
    rpc_start(Node, ?DEFAULT_INTERVAL);
start(Opts = #view_opts{home = Home}) ->
    erlang:process_flag(trap_exit, true),
    AutoRow = check_auto_row(),
    StorePid = observer_cli_store:start(),
    MetricsContext = observer_cli_runtime_metrics:init(Home#home.interval),
    RenderPid = spawn_link(fun() -> run_home_worker(MetricsContext, StorePid, Home, AutoRow) end),
    manager(StorePid, RenderPid, Opts#view_opts{auto_row = AutoRow});
start(Interval) when is_integer(Interval), Interval >= ?MIN_INTERVAL ->
    start(#view_opts{
        home = #home{interval = Interval},
        ets = #ets{interval = Interval},
        sys = #system{interval = Interval},
        db = #db{interval = Interval},
        help = #help{interval = Interval},
        inet = #inet{interval = Interval},
        ports = #ports{interval = Interval},
        sockets = #sockets{interval = Interval},
        process = #process{interval = Interval},
        port = Interval
    }).

-spec start(Node, Cookies | Options) -> term() when
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
clean([RenderPid, StorePid]) ->
    observer_cli_lib:exit_processes([RenderPid, StorePid]),
    false;
%% Keep the exported cleanup entrypoint compatible with resources from the old manager.
clean([RenderPid, StorePid, LegacyToken, _SchUsage]) ->
    observer_cli_lib:exit_processes([RenderPid, StorePid]),
    release_scheduler_wall_time(LegacyToken).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpc_start(Node, Interval) ->
    case net_kernel:hidden_connect_node(Node) of
        true ->
            case update_net_ticktime_from(Node) of
                ok -> rpc:call(Node, ?MODULE, start, [Interval]);
                Error -> Error
            end;
        false ->
            Msg = <<"Node(~p) refuse to be connected, make sure cookie is valid~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown};
        ignored ->
            Msg = <<"Ignored by node(~p), local node is not alive!~n">>,
            connect_error(Msg, Node),
            {badrpc, nodedown}
    end.

manager(StorePid, RenderPid, Opts) ->
    Resource = [RenderPid, StorePid],
    Action = observer_cli_lib:parse_cmd(Opts, ?MODULE, Resource),
    handle_home_action(Action, StorePid, RenderPid, Opts, Resource).

handle_home_action(quit, StorePid, RenderPid, _Opts, _Resource) ->
    observer_cli_lib:exit_processes([RenderPid, StorePid]),
    quit;
handle_home_action(pause_or_resume, StorePid, RenderPid, Opts, _Resource) ->
    erlang:send(RenderPid, pause_or_resume),
    manager(StorePid, RenderPid, Opts);
handle_home_action(
    {new_interval, NewInterval},
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
    Resource
) ->
    restart_home(Opts#view_opts{home = Home#home{interval = NewInterval}}, Resource);
handle_home_action(
    scheduler_usage,
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
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
    Resource
) ->
    NewPages = observer_cli_lib:update_page_pos(CurPage, NewPos, Pages),
    NewOpts = Opts#view_opts{home = Home#home{pages = NewPages}},
    start_process_view(StorePid, RenderPid, NewOpts, Resource, false);
handle_home_action(jump, StorePid, RenderPid, Opts, Resource) ->
    start_process_view(StorePid, RenderPid, Opts, Resource, true);
handle_home_action(
    {func, Func, Type},
    _StorePid,
    _RenderPid,
    Opts = #view_opts{home = Home},
    Resource
) ->
    restart_home(Opts#view_opts{home = Home#home{func = Func, type = Type}}, Resource);
handle_home_action(page_down_top_n, StorePid, _RenderPid, Opts, Resource) ->
    restart_home_page(1, StorePid, Opts, Resource);
handle_home_action(page_up_top_n, StorePid, _RenderPid, Opts, Resource) ->
    restart_home_page(-1, StorePid, Opts, Resource);
handle_home_action({go_to_pid, Pid}, _StorePid, _RenderPid, Opts, Resource) ->
    open_process_view(Pid, Opts, Resource);
handle_home_action(_Action, StorePid, RenderPid, Opts, _Resource) ->
    manager(StorePid, RenderPid, Opts).

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

run_home_worker(MetricsContext, StorePid, Home = #home{scheduler_usage = SchUsage}, AutoRow) ->
    %% The existing sampling worker owns the one registration. OTP also releases it on death.
    Token = enable_scheduler_wall_time(SchUsage),
    try
        render_worker(MetricsContext, StorePid, Home, AutoRow)
    after
        release_scheduler_wall_time(Token)
    end.

render_worker(MetricsContext, Manager, Home, AutoRow) ->
    ?output(?CLEAR),
    StableInfo = get_stable_system_info(),
    LastStats = #{node => undefined, os => undefined},
    redraw_running(
        MetricsContext,
        Manager,
        Home,
        StableInfo,
        LastStats,
        erlang:make_ref(),
        AutoRow,
        true
    ).

%% pause status waiting to be resume
redraw_pause(MetricsContext, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow) ->
    notify_pause_status(),
    erlang:cancel_timer(LastTimeRef),
    #home{func = Func, type = Type} = Home,
    receive
        quit ->
            quit;
        {Func, Type} ->
            %% A timer already in the mailbox must not resume a paused view.
            redraw_pause(
                MetricsContext, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow
            );
        pause_or_resume ->
            ?output(?CLEAR),
            redraw_running(
                MetricsContext, StorePid, Home, StableInfo, LastStats, LastTimeRef, AutoRow, true
            )
    end.

%% running status
redraw_running(
    MetricsContext,
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
        collect_home_snapshot(
            MetricsContext, Home, StableInfo, LastStats, TerminalRow, IsFirstTime
        ),
    {TopNList, Lines} = render_home_snapshot(Home, Snapshot),
    ?output([?CURSOR_TOP, Lines, "\e[J"]),

    observer_cli_store:update(StorePid, maps:get(process_rows, Snapshot), TopNList),
    TimeRef = refresh_next_time(Func, Type, Interval),
    receive
        quit ->
            quit;
        pause_or_resume ->
            redraw_pause(MetricsContext, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow);
        {Func, Type} ->
            redraw_running(
                MetricsContext, StorePid, Home, StableInfo, NewStats, TimeRef, AutoRow, false
            )
    end.

collect_home_snapshot(MetricsContext, Home, StableInfo, LastStats, TerminalRows, IsFirstTime) ->
    #home{
        interval = Interval,
        scheduler_usage = SchUsage
    } =
        Home,
    {Diffs, SchedulerUsage, SchedulerSummary, NewStats} =
        case IsFirstTime of
            true ->
                Summary =
                    case SchUsage of
                        ?DISABLE -> undefined;
                        ?ENABLE -> #{status => warming_up}
                    end,
                {
                    {"warming up", "warming up", "warming up", "warming up"},
                    undefined,
                    Summary,
                    get_incremental_stats(SchUsage)
                };
            false ->
                node_stats(maps:get(node, LastStats), SchUsage)
        end,
    ProcessRows = max(
        TerminalRows - 14 - scheduler_usage_rows(SchedulerUsage), 0
    ),
    ProcessRanking = collect_home_processes(Home, ProcessRows, IsFirstTime),
    PreviousOs =
        case IsFirstTime of
            true -> undefined;
            false -> maps:get(os, LastStats)
        end,
    {Metrics, NextOs} = observer_cli_runtime_metrics:window(
        PreviousOs, observer_cli_runtime_metrics:sample(MetricsContext)
    ),
    Runtime = sample_home_runtime(
        Metrics#{scheduler_summary => SchedulerSummary},
        StableInfo,
        Diffs,
        SchedulerUsage,
        Interval
    ),
    RefreshPrompt = [
        maps:get(refresh_prompt, ProcessRanking),
        " | ",
        observer_cli_runtime_metrics:format_window(Metrics)
    ],
    {OriginalExtra, OriginalMenu} = observer_cli_lib:render_sampling_menu(
        home, maps:get(refresh_prompt, ProcessRanking)
    ),
    {NewExtra, NewMenu} = observer_cli_lib:render_sampling_menu(home, RefreshPrompt),
    %% Do not trade a process row for metadata. The summary header always carries CPU window.
    {ExtraRows, Menu} =
        case NewExtra =< OriginalExtra of
            true -> {NewExtra, NewMenu};
            false -> {OriginalExtra, OriginalMenu}
        end,
    {
        maps:merge(
            Runtime#{process_rows => max(ProcessRows - ExtraRows, 0), menu => Menu},
            ProcessRanking#{refresh_prompt := RefreshPrompt}
        ),
        #{node => NewStats, os => NextOs}
    }.

sample_home_runtime(Metrics, StableInfo, Diffs, SchedulerUsage, Interval) ->
    #{
        system_summary => system_summary(Metrics, StableInfo, get_atom_status()),
        memory_summary => memory_process_summary(Diffs, Interval),
        scheduler_usage => SchedulerUsage
    }.

collect_home_processes(
    #home{interval = Interval, func = Func, type = Type, cur_page = CurPage},
    ProcessRows,
    IsFirstTime
) ->
    TopLen = ProcessRows * CurPage,
    {Items, Status} =
        case {Func, IsFirstTime} of
            {proc_window, true} -> {[], " | warming up"};
            {proc_window, false} -> collect_process_window(Type, Interval, TopLen);
            _ -> {collect_top_n(Func, Type, Interval, TopLen, IsFirstTime), ""}
        end,
    #{
        top_processes => Items,
        refresh_prompt => [get_refresh_prompt(Func, Type, Interval, TopLen), Status]
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
    {_, CPULine} = render_scheduler_usage(maps:get(scheduler_usage, Snapshot)),
    MetricType =
        case Home#home.func of
            proc_window -> {window, Type};
            proc_count -> Type
        end,
    {TopNList, RankLine} = render_top_n_view(MetricType, TopList, ProcessRows, RankPos, CurPage),
    {
        TopNList,
        [
            maps:get(menu, Snapshot),
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

render_system_line(Metrics, StableInfo) ->
    render_system_line(Metrics, StableInfo, get_atom_status()).

render_system_line(Metrics, StableInfo, AtomStatus) ->
    render_home_summary(system_summary(Metrics, StableInfo, AtomStatus)).

-endif.

system_summary(Metrics, StableInfo, AtomStatus) ->
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra} = home_summary_extras(),
    [Version, SysVersion, ProcLimit, PortLimit, EtsLimit] = StableInfo,
    ActiveTask = erlang:statistics(total_active_tasks),
    {ContextSwitch, _} = erlang:statistics(context_switches),
    Reductions = erlang:statistics(reductions),
    {PortWarning, ProcWarning, PortCount, ProcCount} =
        get_port_proc_info(PortLimit, ProcLimit),
    %% The legacy normal rows overran the 139-column layout by one count-cell character.
    NarrowTrim = max(140 - observer_cli_lib:layout_width(), 0),
    CountValueWidth = 22 + LeftValueExtra - NarrowTrim,
    HeaderValueWidth = 25 + RightValueExtra - NarrowTrim,
    CpuText = observer_cli_runtime_metrics:format_cpu(Metrics),
    RssText = observer_cli_runtime_metrics:format_rss(Metrics),
    {VersionLabel, VersionValue} =
        case maps:get(scheduler_summary, Metrics, undefined) of
            undefined -> {" Version", Version};
            Summary -> {" Sched busy", render_scheduler_summary(Summary)}
        end,
    {Reds, _SinceLastCall} = Reductions,
    ReductionsText = integer_to_list(Reds),
    [
        [
            {normal, [{SysVersion, observer_cli_lib:layout_width() - 3}]},
            {?GRAY_BG, [
                {"System", 10 + LeftLabelExtra},
                {"Count/Limit", 21 + LeftValueExtra},
                {"System", 25 + MiddleLabelExtra},
                {observer_cli_runtime_metrics:format_window(Metrics), 21 + MiddleValueExtra},
                {"Stat Info", 20 + RightLabelExtra},
                {"Size", HeaderValueWidth}
            ]}
        ],
        [
            {normal, [
                {"Proc Count", 10 + LeftLabelExtra},
                {ProcWarning, ProcCount, CountValueWidth},
                {VersionLabel, 26 + MiddleLabelExtra},
                {VersionValue, 21 + MiddleValueExtra},
                {"Active Task", 20 + RightLabelExtra},
                {ActiveTask, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Port Count", 10 + LeftLabelExtra},
                {PortWarning, PortCount, CountValueWidth},
                {" BEAM CPU", 26 + MiddleLabelExtra},
                {CpuText, 21 + MiddleValueExtra},
                {"Context Switch", 20 + RightLabelExtra},
                {ContextSwitch, 24 + RightValueExtra}
            ]}
        ],
        [
            system_atom_summary_row(
                AtomStatus,
                EtsLimit,
                RssText,
                ReductionsText,
                {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra,
                    RightLabelExtra, RightValueExtra}
            )
        ]
    ].

system_atom_summary_row(
    {ok, AtomLimit, AtomCount},
    _EtsLimit,
    RssText,
    ReductionsText,
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra}
) ->
    {AtomWarning, Atom} = format_atom_info(AtomLimit, AtomCount),
    {?UNDERLINE, [
        {"Atom Count", 10 + LeftLabelExtra},
        {AtomWarning, Atom, 22 + LeftValueExtra},
        {" BEAM RSS", 26 + MiddleLabelExtra},
        {RssText, 21 + MiddleValueExtra},
        {"Reds total", 20 + RightLabelExtra},
        {ReductionsText, 24 + RightValueExtra}
    ]};
system_atom_summary_row(
    {error, unsupported},
    EtsLimit,
    RssText,
    ReductionsText,
    {LeftLabelExtra, LeftValueExtra, MiddleLabelExtra, MiddleValueExtra, RightLabelExtra,
        RightValueExtra}
) ->
    {?UNDERLINE, [
        {"Ets Limit", 10 + LeftLabelExtra},
        {EtsLimit, 21 + LeftValueExtra},
        {" BEAM RSS", 25 + MiddleLabelExtra},
        {RssText, 21 + MiddleValueExtra},
        {"Reductions", 20 + RightLabelExtra},
        {ReductionsText, 24 + RightValueExtra}
    ]}.

-ifdef(TEST).

render_memory_process_line(MemSum, Interval) ->
    render_home_summary(memory_process_summary(MemSum, Interval)).

-endif.

memory_process_summary(MemSum, _Interval) ->
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
    PersistentTermInfo = persistent_term:info(),
    PersistentTermCount = maps:get(count, PersistentTermInfo),
    PersistentTermMemory = maps:get(memory, PersistentTermInfo),

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
                {"IO/GC since sample", 20 + RightLabelExtra},
                {"Total/Delta", 25 + RightValueExtra}
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
                {"Proc used", 10 + LeftLabelExtra},
                {{byte, ProcMem}, 12},
                {ProcMemPercent, 6 + LeftValueExtra},
                {"Code", 25 + MiddleLabelExtra},
                {{byte, CodeMem}, 12},
                {CodeMemPercent, 6 + MiddleValueExtra},
                {"IO Input", 20 + RightLabelExtra},
                {BytesIn, 25 + RightValueExtra}
            ]},
            {normal, [
                {"Atom used", 10 + LeftLabelExtra},
                {{byte, AtomMem}, 12},
                {AtomMemPercent, 6 + LeftValueExtra},
                {
                    ["Persistent Terms/", erlang:integer_to_list(PersistentTermCount)],
                    25 + MiddleLabelExtra
                },
                {{byte, PersistentTermMemory}, 21 + MiddleValueExtra},
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
    [Width1, Width2] = scheduler_bar_widths(2),
    CPU =
        [
            begin
                Seq2 = transform_seq(Seq1, Column, SchedulerNum),
                Percent1 = proplists:get_value(Seq1, SchedulerUsage, 0.0),
                Percent2 = proplists:get_value(Seq2, SchedulerUsage, 0.0),
                CPU1 = observer_cli_lib:to_percent(Percent1),
                CPU2 = observer_cli_lib:to_percent(Percent2),
                Process1 = scheduler_bar(Percent1, Width1),
                Process2 = scheduler_bar(Percent2, Width2),
                IsLastLine = Seq1 =:= Column,
                Format = process_bar_format_style(
                    [Percent1, Percent2], [Width1, Width2], IsLastLine
                ),
                io_lib:format(Format, [Seq1, Process1, CPU1, Seq2, Process2, CPU2])
            end
         || Seq1 <- lists:seq(1, Column)
        ],
    {Column, pad_scheduler_lines(CPU)};
%% 100 >= scheduler >= 8 split 4 part
render_scheduler_usage(SchedulerUsage, SchedulerNum) when SchedulerNum =< 100 ->
    Column = scheduler_usage_rows_by_count(SchedulerNum),
    [Width1, Width2, Width3, Width4] = scheduler_bar_widths(4),
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
                Process1 = scheduler_bar(Percent1, Width1),
                Process2 = scheduler_bar(Percent2, Width2),
                Process3 = scheduler_bar(Percent3, Width3),
                Process4 = scheduler_bar(Percent4, Width4),
                IsLastLine = Seq1 =:= Column,
                Format = process_bar_format_style(
                    [Percent1, Percent2, Percent3, Percent4],
                    [Width1, Width2, Width3, Width4],
                    IsLastLine
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

scheduler_bar_widths(2) ->
    observer_cli_lib:weighted_widths([57, 57], [1, 1]);
scheduler_bar_widths(4) ->
    observer_cli_lib:weighted_widths([22, 22, 22, 23], [1, 1, 1, 1]);
scheduler_bar_widths(_Parts) ->
    [].

scheduler_bar(Percent, Width) ->
    lists:duplicate(trunc(Percent * Width), "|").

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

top_n_spec({window, Type}) ->
    Spec = top_n_spec(Type),
    [{_, Width} | Rest] = maps:get(metric_columns, Spec),
    Label =
        case Type of
            reductions -> "Reds/s";
            memory -> "Mem change";
            binary_memory -> "BinMem change";
            total_heap_size -> "Heap change";
            message_queue_len -> "Queue change"
        end,
    case Type of
        message_queue_len ->
            %% Make room for the full label without widening the table.
            {NameWidth, TitleWidth, CurrentWidth} = maps:get(text_widths, Spec),
            Spec#{
                metric_columns := [{Label, Width + 1} | Rest],
                text_widths := {NameWidth - 1, TitleWidth, CurrentWidth},
                row_formats := {"~-12.12s", "~13.13s", " ~-13.13s"}
            };
        _ ->
            Spec#{metric_columns := [{Label, Width} | Rest]}
    end;
top_n_spec(memory) ->
    #{
        text_widths => {45, 32, 33},
        metric_columns => [
            {"     Memory", 14},
            {"    Reds total", 14},
            {" MsgQueue", 10}
        ],
        row_formats => {"~13.13s ", "~14.14s", " ~-9.9s"}
    };
top_n_spec(binary_memory) ->
    #{
        text_widths => {45, 32, 33},
        metric_columns => [
            {"  BinMemory", 14},
            {"    Reds total", 14},
            {" MsgQueue", 10}
        ],
        row_formats => {"~13.13s ", "~14.14s", " ~-9.9s"}
    };
top_n_spec(reductions) ->
    #{
        text_widths => {45, 33, 34},
        metric_columns => [
            {"   Reds total", 15},
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
            {"    Reds total", 14},
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
            {"    Reds total", 14}
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

top_n_row_values({window, Type}, Pid, Value) ->
    top_n_row_values(Type, Pid, Value);
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
top_n_row_values(Type, Pid, Value) ->
    {Reductions, MsgQueueLen} = get_pid_info(Pid, [reductions, message_queue_len]),
    Bytes =
        case Type of
            total_heap_size when is_number(Value) -> Value * erlang:system_info(wordsize);
            _ -> Value
        end,
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
    process_bar_format_style(Percents, scheduler_bar_widths(length(Percents)), IsLastLine).

process_bar_format_style(Percents, Widths, IsLastLine) ->
    Format =
        case [warning_color(P) || P <- Percents] of
            [W1, W2] ->
                [Width1, Width2] = Widths,
                Bar1 = scheduler_bar_format(Width1),
                Bar2 = scheduler_bar_format(Width2),
                <<"|", W1/binary, "|~2..0w ", Bar1/binary, "~s", W2/binary, " |~2..0w ",
                    Bar2/binary, " ~s \e[0m|~n">>;
            [W1, W2, W3, W4] ->
                [Width1, Width2, Width3, Width4] = Widths,
                Bar1 = scheduler_bar_format(Width1),
                Bar2 = scheduler_bar_format(Width2),
                Bar3 = scheduler_bar_format(Width3),
                Bar4 = scheduler_bar_format(Width4),
                <<"|", W1/binary, "|~-2.2w ", Bar1/binary, " ~s", W2/binary, " |~-2.2w ",
                    Bar2/binary, " ~s", W3/binary, " |~-2.2w ", Bar3/binary, " ~s", W4/binary,
                    " |~-2.2w ", Bar4/binary, " ~s \e[0m|~n">>;
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

scheduler_bar_format(Width) ->
    WidthBin = erlang:integer_to_binary(Width),
    <<"~-", WidthBin/binary, ".", WidthBin/binary, "s">>.

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
    io_lib:format("recon:proc_count(~p, ~w) | Refresh:~wms", [Type, Rows, Interval]);
get_refresh_prompt(proc_window, Type, Interval, Rows) ->
    io_lib:format("recon:proc_window(~p, ~w, ~w) | Refresh:~wms", [Type, Rows, Interval, Interval]).

get_stable_system_info() ->
    OtpRelease = erlang:system_info(otp_release),
    SysVersion = erlang:system_info(system_version) -- "\n",
    [
        OtpRelease,
        SysVersion ++ " " ++ atom_to_list(erlang:node()),
        erlang:system_info(process_limit),
        erlang:system_info(port_limit),
        erlang:system_info(ets_limit)
    ].

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

collect_top_n(proc_window, _Type, _Interval, _Rows, true) ->
    [];
collect_top_n(proc_window, Type, Interval, Rows, false) ->
    {Items, _Status} = collect_process_window(Type, Interval, Rows),
    Items;
collect_top_n(_Func, Type, _Interval, Rows, _FirstTime) ->
    recon:proc_count(Type, Rows).

%% Keep both samples explicit: recon's window helper cannot expose the elapsed
%% monotonic time and treats a newly observed process as a delta from zero.
collect_process_window(Type, Interval, Rows) ->
    First = recon_lib:proc_attrs(Type),
    Start = erlang:monotonic_time(microsecond),
    timer:sleep(Interval),
    Last = recon_lib:proc_attrs(Type),
    Elapsed = erlang:monotonic_time(microsecond) - Start,
    {Items, Missing, Reset} = process_window(Type, First, Last, Elapsed),
    Ranked = recon_lib:sublist_top_n_attrs(Items, Rows),
    {Ranked, process_sample_status(Elapsed, Missing, Reset)}.

process_sample_status(Elapsed, Missing, Reset) ->
    [
        io_lib:format(" | Sample:~wms", [round(Elapsed / 1000)]),
        [
            io_lib:format(" | ~s:~w", [Label, Count])
         || {Label, Count} <-
                [{"missing", Missing}, {"reset", Reset}],
            Count > 0
        ]
    ].

process_window(Type, First, Last, Elapsed) ->
    Before = maps:from_list([{Pid, Value} || {Pid, Value, _} <- First]),
    Gone = length(First) - length([Pid || {Pid, _, _} <- Last, maps:is_key(Pid, Before)]),
    lists:foldl(
        fun({Pid, Value, Info}, {Acc, Missing, Reset}) ->
            case maps:find(Pid, Before) of
                error ->
                    {Acc, Missing + 1, Reset};
                {ok, Previous} when Type =:= reductions, Value < Previous ->
                    {Acc, Missing, Reset + 1};
                {ok, _Previous} when Elapsed =< 0 -> {Acc, Missing + 1, Reset};
                {ok, Previous} ->
                    Delta = Value - Previous,
                    Metric =
                        case Type of
                            reductions -> round(Delta * 1000000 / Elapsed);
                            _ -> Delta
                        end,
                    {[{Pid, Metric, Info} | Acc], Missing, Reset}
            end
        end,
        {[], Gone, 0},
        Last
    ).

connect_error(Prompt, Node) ->
    ?output(observer_cli_lib:ansi_red(Prompt), [Node]).

start_process_view(StorePid, RenderPid, Opts, Resource, AutoJump) ->
    case select_home_process(StorePid, Opts, AutoJump) of
        {ok, ChoosePid} ->
            open_process_view(ChoosePid, Opts, Resource);
        error ->
            manager(StorePid, RenderPid, Opts)
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

enable_scheduler_wall_time(?DISABLE) ->
    false;
enable_scheduler_wall_time(?ENABLE) ->
    Token = atomics:new(1, []),
    _ = erlang:system_flag(scheduler_wall_time, true),
    Token.

release_scheduler_wall_time(false) ->
    false;
release_scheduler_wall_time(Token) ->
    case atomics:exchange(Token, 1, 1) of
        0 -> erlang:system_flag(scheduler_wall_time, false);
        1 -> false
    end.

check_auto_row() ->
    case io:rows() of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

node_stats(LastStats, SchUsage) ->
    New = get_incremental_stats(SchUsage),
    {Usage, Summary, NextSchedule} = scheduler_stats(LastStats, New),
    {
        io_gc_stats_diff(LastStats, New),
        Usage,
        Summary,
        setelement(5, New, NextSchedule)
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

scheduler_stats(_Last, {_, _, _, _, undefined}) ->
    {undefined, undefined, undefined};
scheduler_stats(_Last, {_, _, _, _, #{wall_time := undefined} = Current}) ->
    {undefined, #{status => unavailable}, Current};
scheduler_stats({_, _, _, _, undefined}, {_, _, _, _, Current}) ->
    {undefined, #{status => warming_up}, Current};
scheduler_stats({_, _, _, _, #{wall_time := undefined}}, {_, _, _, _, Current}) ->
    {undefined, #{status => warming_up}, Current};
scheduler_stats({_, _, _, _, Previous}, {_, _, _, _, Current}) ->
    case observer_cli_snapshot:scheduler_busy_window(Previous, Current) of
        #{status := valid} = Summary ->
            try
                recon_lib:scheduler_usage_diff(
                    maps:get(wall_time, Previous), maps:get(wall_time, Current)
                )
            of
                Usage -> {Usage, Summary, Current}
            catch
                error:_ ->
                    {undefined, #{status => unavailable}, Current#{wall_time := undefined}}
            end;
        #{status := invalid} ->
            {undefined, #{status => unavailable}, Current#{wall_time := undefined}}
    end.

render_scheduler_summary(#{status := warming_up}) ->
    "warming up";
render_scheduler_summary(#{status := unavailable}) ->
    "unavailable";
render_scheduler_summary(#{status := valid, normal := Normal, dirty_cpu := Dirty}) ->
    ["N ", scheduler_pool_percent(Normal), " / D ", scheduler_pool_percent(Dirty)].

scheduler_pool_percent(#{status := available, utilization_ratio := Ratio}) ->
    %% Compact enough for the existing 21-character value cell, including two 100% pools.
    io_lib:format("~B%", [round(Ratio * 100)]);
scheduler_pool_percent(#{status := unavailable}) ->
    "n/a".

get_incremental_stats(SchUsage) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    ScheduleWall =
        case SchUsage of
            ?ENABLE ->
                #{
                    topology => #{
                        schedulers_configured => erlang:system_info(schedulers),
                        schedulers_online => erlang:system_info(schedulers_online),
                        dirty_cpu_schedulers_online => erlang:system_info(
                            dirty_cpu_schedulers_online
                        )
                    },
                    wall_time => erlang:statistics(scheduler_wall_time)
                };
            ?DISABLE ->
                undefined
        end,
    {In, Out, GCs, Words, ScheduleWall}.

update_net_ticktime_from(Node) ->
    case rpc:call(Node, net_kernel, get_net_ticktime, [], 5000) of
        NetTickTime when is_integer(NetTickTime), NetTickTime > 0 ->
            accept_net_ticktime_result(net_kernel:set_net_ticktime(NetTickTime), NetTickTime);
        _Invalid ->
            {error, connection, connection_failed}
    end.

accept_net_ticktime_result(change_initiated, _NetTickTime) ->
    ok;
accept_net_ticktime_result({ongoing_change_to, NetTickTime}, NetTickTime) ->
    ok;
accept_net_ticktime_result({ongoing_change_to, _Other}, _NetTickTime) ->
    {error, connection, connection_failed};
accept_net_ticktime_result(unchanged, _NetTickTime) ->
    ok.
