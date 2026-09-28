%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").
%% net_address record from OTP lacks typed fields; keep warning local to this module.
-compile(nowarn_untyped_record).
-include_lib("kernel/include/net_address.hrl").

%% API
-export([start/1, collect_allocator_info/0]).

-ifdef(TEST).
-export([
    collect_distribution_info/0,
    collect_os_process_info/1,
    collect_system_info/1,
    collect_sys_info/1,
    fill_info/2,
    to_list/1,
    info_fields/0,
    get_cachehit_info/2,
    render_system_sections/1,
    render_sys_info/1,
    render_sys_info/4,
    render_cache_hit_rates/2,
    render_block_size_info/4,
    get_alloc/5,
    render_dist_node_info/1,
    get_address/1,
    render_worker/3,
    get_dist_stats/1,
    sample_distribution/2,
    format_count_limit/2,
    collect_runtime_info/0,
    alloc_info/0,
    maybe_system_info/1
]).
-endif.

-define(UTIL_ALLOCATORS, [
    binary_alloc,
    driver_alloc,
    eheap_alloc,
    ets_alloc,
    fix_alloc,
    ll_alloc,
    sl_alloc,
    std_alloc,
    temp_alloc
]).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system
%%  List Memory Allocators: std, ll, eheap, ets, fix, binary, driver.
-spec start(ViewOpts) -> no_return() when ViewOpts :: view_opts().
start(#view_opts{sys = #system{interval = Interval}} = ViewOpts) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        Cmd = io_lib:format("ps -o pcpu,pmem,rss,vsz ~s", [os:getpid()]),
        render_worker(Cmd, Interval, ?INIT_TIME_REF)
    end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, #view_opts{sys = AllocatorOpts} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [Pid]) of
        quit ->
            erlang:send(Pid, quit);
        {new_interval, NewInterval} ->
            erlang:send(Pid, {new_interval, NewInterval}),
            NewAllocate = AllocatorOpts#system{interval = NewInterval},
            manager(Pid, ViewOpts#view_opts{sys = NewAllocate});
        _ ->
            manager(Pid, ViewOpts)
    end.

render_worker(Cmd, Interval, LastTimeRef) ->
    render_worker(Cmd, Interval, LastTimeRef, #{}).

render_worker(Cmd, Interval, LastTimeRef, Previous) ->
    Collected = collect_system_info(Cmd),
    {DistInfo, NextSamples} = sample_distribution(maps:get(dist_nodes_info, Collected), Previous),
    SystemInfo = Collected#{dist_nodes_info := DistInfo},
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_top_menu(allocator, Text),
    LastLine = observer_cli_lib:render_footer("q(quit)"),
    ?output([?CURSOR_TOP, Menu] ++ render_system_sections(SystemInfo) ++ [LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> render_worker(Cmd, NewInterval, NextTimeRef, NextSamples);
        redraw -> render_worker(Cmd, Interval, NextTimeRef, NextSamples)
    end.

collect_system_info(Cmd) ->
    {OsProcessInfo, SysInfo} = split_os_process_info(collect_sys_info(Cmd)),
    #{
        os_process_info => OsProcessInfo,
        sys_info => SysInfo,
        allocator_info => collect_allocator_info(),
        dist_nodes_info => collect_distribution_info()
    }.

split_os_process_info(SysInfo) ->
    lists:partition(
        fun({Key, _}) -> lists:member(Key, [ps_cpu, ps_mem, ps_rss, ps_vsz]) end,
        SysInfo
    ).

-spec collect_allocator_info() -> map().
collect_allocator_info() ->
    #{
        cache_hit_info => recon_alloc:cache_hit_rates(),
        average_block_curs => recon_alloc:average_block_sizes(current),
        average_block_maxes => recon_alloc:average_block_sizes(max),
        sbcs_to_mbcs_curs =>
            observer_cli_lib:sbcs_to_mbcs(?UTIL_ALLOCATORS, recon_alloc:sbcs_to_mbcs(current)),
        sbcs_to_mbcs_maxes =>
            observer_cli_lib:sbcs_to_mbcs(?UTIL_ALLOCATORS, recon_alloc:sbcs_to_mbcs(max))
    }.

render_system_sections(SystemInfo) ->
    [
        render_system_info_section(SystemInfo),
        render_allocator_section(SystemInfo),
        render_distribution_node_section(SystemInfo),
        render_cache_hit_section(SystemInfo)
    ].

render_system_info_section(#{os_process_info := OsProcessInfo, sys_info := SysInfo}) ->
    render_sys_info(OsProcessInfo ++ SysInfo).

render_allocator_section(#{
    allocator_info := #{
        average_block_curs := AverageBlockCurs,
        average_block_maxes := AverageBlockMaxes,
        sbcs_to_mbcs_curs := SbcsToMbcsCurs,
        sbcs_to_mbcs_maxes := SbcsToMbcsMaxs
    }
}) ->
    render_block_size_info(
        AverageBlockCurs,
        AverageBlockMaxes,
        SbcsToMbcsCurs,
        SbcsToMbcsMaxs
    ).

render_distribution_node_section(#{dist_nodes_info := DistNodesInfo}) ->
    render_dist_node_info(DistNodesInfo).

render_cache_hit_section(#{allocator_info := #{cache_hit_info := CacheHitInfo}}) ->
    render_cache_hit_rates(CacheHitInfo, erlang:length(CacheHitInfo)).

collect_distribution_info() ->
    try
        case ets:info(sys_dist, size) of
            undefined ->
                [empty_distribution_info("dist disabled")];
            _ ->
                case net_kernel:nodes_info() of
                    {ok, []} ->
                        [empty_distribution_info("no connected nodes")];
                    {ok, NodesInfo} ->
                        [collect_distribution_node_info(NodeInfo) || NodeInfo <- NodesInfo];
                    _ ->
                        [empty_distribution_info("dist unavailable")]
                end
        end
    catch
        _:_ -> [empty_distribution_info("dist unavailable")]
    end.

empty_distribution_info(Detail) ->
    {node(), #{
        stats => unavailable,
        pending_packets => unavailable,
        address => Detail,
        type => "-",
        state => "-"
    }}.

collect_distribution_node_info({Node, Info}) ->
    Stats = get_dist_stats(Node),
    Pending =
        case Stats of
            {ok, _, _, _, Count} -> Count;
            unavailable -> unavailable
        end,
    {Node, #{
        stats => Stats,
        sampled_at => erlang:monotonic_time(millisecond),
        pending_packets => Pending,
        address => get_address(Info),
        type => proplists:get_value(type, Info, "-"),
        state => proplists:get_value(state, Info, "-")
    }}.

%% Keep only the current peers and at most three observations per connection.
%% Rates use the same packet counters as pending_packets, not carrier-specific
%% net_kernel In/Out counters. Missing samples break continuity rather than
%% interpolating across an unobserved gap or a reconnected node.
sample_distribution(NodesInfo, Previous) ->
    Sampled = [
        {Node, sample_distribution_node(Info, maps:get(Node, Previous, #{}))}
     || {Node, Info} <- NodesInfo
    ],
    {Sampled, maps:from_list(Sampled)}.

sample_distribution_node(#{state := up, stats := {ok, _, _, _, Pending}} = Info, Previous) ->
    Initial = Info#{rx_rate => "-", tx_rate => "-", recent_pending => [Pending]},
    case {Info, Previous} of
        {#{stats := {ok, Conn, Rx, Tx, _}, sampled_at := Now}, #{
            state := up,
            stats := {ok, Conn, OldRx, OldTx, _},
            sampled_at := Then,
            recent_pending := Recent
        }} when
            Now > Then, Rx >= OldRx, Tx >= OldTx
        ->
            Initial#{
                rx_rate := (Rx - OldRx) * 1000 / (Now - Then),
                tx_rate := (Tx - OldTx) * 1000 / (Now - Then),
                recent_pending := lists:nthtail(max(0, length(Recent) - 2), Recent) ++ [Pending]
            };
        _ ->
            Initial
    end;
sample_distribution_node(Info, _Previous) ->
    Info#{rx_rate => "N/A", tx_rate => "N/A", recent_pending => []}.

render_dist_node_info([]) ->
    render_dist_node_info([empty_distribution_info("no connected nodes")]);
render_dist_node_info(DistNodesInfo) ->
    Widths = observer_cli_lib:weighted_widths(
        [28, 8, 11, 11, 14, 25, 25, 7],
        [4, 0, 0, 0, 0, 2, 4, 0]
    ),
    Headers = [
        "Node",
        "State",
        "Rx pkt/s",
        "Tx pkt/s",
        "Pending pkts",
        "Recent pkts (old>new)",
        "Address",
        "Type"
    ],
    Title = ?render(
        lists:append([
            [?UNDERLINE, ?W2(?GRAY_BG, Header, Width)]
         || {Header, Width} <- lists:zip(Headers, Widths)
        ])
    ),
    View = [
        render_dist_node_row(Node, Info, Widths)
     || {Node, Info} <- lists:sort(DistNodesInfo)
    ],
    [Title | View].

render_dist_node_row(Node, Info, Widths) ->
    Values = [
        Node,
        maps:get(state, Info),
        dist_rate_text(maps:get(rx_rate, Info, "-")),
        dist_rate_text(maps:get(tx_rate, Info, "-")),
        dist_packet_text(maps:get(pending_packets, Info, unavailable)),
        dist_recent_text(maps:get(recent_pending, Info, []), lists:nth(6, Widths)),
        maps:get(address, Info),
        maps:get(type, Info)
    ],
    ?render([
        ?W2(?RESET, Value, Width)
     || {Value, Width} <- lists:zip(Values, Widths)
    ]).

dist_rate_text(Rate) when is_float(Rate) ->
    float_to_list(Rate, [{decimals, 1}]);
dist_rate_text(Text) ->
    Text.

dist_packet_text(Count) when is_integer(Count), Count >= 0 -> integer_to_list(Count);
dist_packet_text(_) -> "N/A".

dist_recent_text([], _Width) ->
    "-";
dist_recent_text(Recent, Width) ->
    Text = lists:flatten(lists:join(" > ", [integer_to_list(N) || N <- Recent])),
    case length(Text) =< Width of
        true -> Text;
        false -> "... > " ++ integer_to_list(lists:last(Recent))
    end.

get_address(Info) ->
    #net_address{address = Address} = proplists:get_value(address, Info, #net_address{}),
    case Address of
        {Addr, Port} ->
            case inet:ntoa(Addr) of
                {error, _} ->
                    AddrStr = lists:flatten(io_lib:format("~p", [Addr])),
                    AddrStr ++ ":" ++ integer_to_list(Port);
                AddrStr ->
                    AddrStr ++ ":" ++ integer_to_list(Port)
            end;
        _ ->
            "unknown"
    end.

%% Both sys_dist and dist_get_stat are OTP internals. A disappearing
%% connection, unsupported runtime, or changed tuple shape must not kill the UI.
get_dist_stats(Node) ->
    try
        case ets:lookup(sys_dist, Node) of
            [Dist] when tuple_size(Dist) >= 3, element(1, Dist) =:= connection ->
                ConnId = element(3, Dist),
                case erlang:dist_get_stat(ConnId) of
                    {ok, Rx, Tx, Pending} when
                        is_integer(Rx),
                        Rx >= 0,
                        is_integer(Tx),
                        Tx >= 0,
                        is_integer(Pending),
                        Pending >= 0
                    ->
                        {ok, ConnId, Rx, Tx, Pending};
                    _ ->
                        unavailable
                end;
            _ ->
                unavailable
        end
    catch
        _:_ -> unavailable
    end.

render_cache_hit_rates(CacheHitInfo, Len) when Len =< 8 ->
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("Instance", 8),
        ?W("Hits", 10),
        ?W("Calls", 11),
        ?W("Hit Rate", 98)
    ]),
    View = [
        begin
            [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value(
                {instance, Seq},
                CacheHitInfo
            ),
            HitRateStr = observer_cli_lib:to_percent(HitRate),
            SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
            TrueHitRate =
                case Hit == 0 andalso Call == 0 of
                    true -> 0;
                    false -> HitRate
                end,
            Process = lists:duplicate(trunc(TrueHitRate * 91), "|"),
            ?render([
                ?W(SeqStr, 8),
                ?W(Hit, 10),
                ?W(Call, 11),
                ?W(Process, 89),
                ?W(HitRateStr, 6)
            ])
        end
     || Seq <- lists:seq(0, Len - 1)
    ],
    [Title | View];
render_cache_hit_rates(CacheHitInfo, Len) ->
    [HitCalls1W, HitCalls2W, HitCalls3W, HitCalls4W] = cache_hit_title_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        "IN|",
        ?W(" Hits/Calls", HitCalls1W),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", HitCalls2W),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", HitCalls3W),
        ?W("HitRate", 6),
        "IN|",
        ?W(" Hits/Calls", HitCalls4W),
        ?W("HitRate", 6)
    ]),
    [HitCallsRow1W, HitCallsRow2W, HitCallsRow3W, HitCallsRow4W] = cache_hit_row_widths(),
    Num = Len div 4,
    Rows = [
        begin
            Seq2 = Seq1 + Num,
            Seq3 = Seq2 + Num,
            Seq4 = Seq3 + Num,
            {SeqStr1, Hit1, Call1, HitRateStr1} = get_cachehit_info(Seq1, CacheHitInfo),
            {SeqStr2, Hit2, Call2, HitRateStr2} = get_cachehit_info(Seq2, CacheHitInfo),
            {SeqStr3, Hit3, Call3, HitRateStr3} = get_cachehit_info(Seq3, CacheHitInfo),
            {SeqStr4, Hit4, Call4, HitRateStr4} = get_cachehit_info(Seq4, CacheHitInfo),
            ?render([
                SeqStr1,
                ?W([Hit1, "/", Call1], HitCallsRow1W),
                ?W(HitRateStr1, 6),
                SeqStr2,
                ?W([Hit2, "/", Call2], HitCallsRow2W),
                ?W(HitRateStr2, 6),
                SeqStr3,
                ?W([Hit3, "/", Call3], HitCallsRow3W),
                ?W(HitRateStr3, 6),
                SeqStr4,
                ?W([Hit4, "/", Call4], HitCallsRow4W),
                ?W(HitRateStr4, 6)
            ])
        end
     || Seq1 <- lists:seq(1, Num)
    ],
    [Title | Rows].

render_block_size_info(AverageBlockCurs, AverageBlockMaxes, SbcsToMbcsCurs, SbcsToMbcsMaxs) ->
    [AllocatorW, CurrentMbcsW, MaxMbcsW, CurrentSbcsW, MaxSbcsW, CurrentSbcsToMbcsW, MaxSbcsToMbcsW] =
        block_size_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("Allocator Type", AllocatorW),
        ?W("Current Mbcs", CurrentMbcsW),
        ?W("Max Mbcs", MaxMbcsW),
        ?W("Current Sbcs", CurrentSbcsW),
        ?W("Max Sbcs", MaxSbcsW),
        ?W("Current SbcsToMbcs", CurrentSbcsToMbcsW),
        ?W("Max SbcsToMbcs", MaxSbcsToMbcsW)
    ]),
    View = [
        begin
            [Type, CMC, MMC, CSC, MSBC, CSTM, MSTM] =
                get_alloc(
                    AllocKey,
                    AverageBlockCurs,
                    AverageBlockMaxes,
                    SbcsToMbcsCurs,
                    SbcsToMbcsMaxs
                ),
            ?render([
                ?W(Type, AllocatorW),
                ?W(CMC, CurrentMbcsW),
                ?W(MMC, MaxMbcsW),
                ?W(CSC, CurrentSbcsW),
                ?W(MSBC, MaxSbcsW),
                ?W(CSTM, CurrentSbcsToMbcsW),
                ?W(MSTM, MaxSbcsToMbcsW)
            ])
        end
     || AllocKey <- ?UTIL_ALLOCATORS
    ],
    [Title | View].

cache_hit_title_widths() ->
    observer_cli_lib:weighted_widths([20, 20, 20, 19], [1, 1, 1, 1]).

cache_hit_row_widths() ->
    observer_cli_lib:weighted_widths([19, 19, 19, 18], [1, 1, 1, 1]).

block_size_widths() ->
    observer_cli_lib:weighted_widths(
        [16, 16, 16, 16, 16, 19, 19],
        [0, 1, 1, 1, 1, 2, 2]
    ).

get_alloc(Key, Curs, Maxes, STMCurs, STMMaxes) ->
    CurRes = proplists:get_value(Key, Curs),
    MaxRes = proplists:get_value(Key, Maxes),
    CurMbcs = proplists:get_value(mbcs, CurRes),
    CurSbcs = proplists:get_value(sbcs, CurRes),
    MaxMbcs = proplists:get_value(mbcs, MaxRes),
    MaxSbcs = proplists:get_value(sbcs, MaxRes),
    [
        atom_to_list(Key),
        observer_cli_lib:to_byte(CurMbcs),
        observer_cli_lib:to_byte(MaxMbcs),
        observer_cli_lib:to_byte(CurSbcs),
        observer_cli_lib:to_byte(MaxSbcs),
        proplists:get_value(Key, STMCurs),
        proplists:get_value(Key, STMMaxes)
    ].

render_sys_info(SysInfo) ->
    {Info, Stat} = info_fields(),
    SystemAndCPU = fill_info(Info, SysInfo),
    MemAndStatistics = fill_info(Stat, SysInfo),
    System = proplists:get_value("System and Architecture", SystemAndCPU),
    CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
    {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
    {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
    render_sys_info(System, CPU, Memory, Statistics) ++ render_runtime_limit_info(SysInfo).

collect_sys_info(Cmd) ->
    collect_os_process_info(Cmd) ++ collect_runtime_info().

render_sys_info(System, CPU, Memory, Statistics) ->
    [
        SystemTitleW,
        SystemStateTitleW,
        CpuTitleW,
        CpuStateTitleW,
        MemoryTitleW,
        MemoryStateTitleW,
        StatisticsTitleW,
        StatisticsStateTitleW
    ] =
        sys_info_title_widths(),
    Title = ?render([
        ?GRAY_BG,
        ?UNDERLINE,
        ?W("System/Architecture", SystemTitleW),
        ?W("State", SystemStateTitleW),
        ?W("CPU's and Threads", CpuTitleW),
        ?W("State", CpuStateTitleW),
        ?W("Memory Usage", MemoryTitleW),
        ?W("State", MemoryStateTitleW),
        ?W("Statistics", StatisticsTitleW),
        ?W("State", StatisticsStateTitleW)
    ]),
    [
        SystemW,
        SystemStateW,
        CpuW,
        CpuStateW,
        MemoryW,
        MemoryValueW,
        MemoryPercentW,
        StatisticsW,
        StatisticsStateW
    ] =
        sys_info_row_widths(),
    NewSystem = [
        begin
            {Key, Value}
        end
     || {Key, Value} <- System,
        Key =/= "Compiled for" andalso Key =/= "smp Support"
    ],
    [{_, TotalMem} | _R] = Memory,
    {bytes, TotalMemInt} = TotalMem,
    Row = [
        begin
            {SysKey, SysVal} = lists:nth(Pos, NewSystem),
            {CpuKey, CpuVal} = lists:nth(Pos, CPU),
            {MemKey, MemVal} = lists:nth(Pos, Memory),
            {StatisticsKey, StatisticsVal} = lists:nth(Pos, Statistics),

            {bytes, MemValInt} = MemVal,
            Percent = observer_cli_lib:to_percent(MemValInt / TotalMemInt),
            ?render([
                ?W(SysKey, SystemW),
                ?W(to_list(SysVal), SystemStateW),
                ?W(CpuKey, CpuW),
                ?W(to_list(CpuVal), CpuStateW),
                ?W(MemKey, MemoryW),
                ?W(observer_cli_lib:to_byte(MemValInt), MemoryValueW),
                ?W(Percent, MemoryPercentW),
                ?W(StatisticsKey, StatisticsW),
                ?W(to_list(StatisticsVal), StatisticsStateW)
            ])
        end
     || Pos <- lists:seq(1, 6)
    ],
    [CompiledForW, CompiledValueW] = compiled_for_widths(),
    Compile = ?render([
        ?UNDERLINE,
        ?W("compiled for", CompiledForW),
        ?W(to_list(proplists:get_value("Compiled for", System)), CompiledValueW)
    ]),
    [Title | (Row ++ [Compile])].

sys_info_title_widths() ->
    observer_cli_lib:weighted_widths(
        [22, 8, 23, 7, 11, 22, 11, 11],
        [0, 1, 0, 1, 0, 1, 0, 1]
    ).

sys_info_row_widths() ->
    observer_cli_lib:weighted_widths(
        [22, 8, 23, 7, 11, 13, 6, 11, 11],
        [0, 1, 0, 1, 0, 1, 0, 0, 1]
    ).

compiled_for_widths() ->
    observer_cli_lib:weighted_widths([22, 111], [0, 1]).

render_runtime_limit_info(SysInfo) ->
    Widths = [Metric1W, Value1W, Metric2W, Value2W, Metric3W, Value3W] = runtime_limit_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("System Statistics / Limit", Metric1W),
        ?W("Value", Value1W),
        ?W("System Statistics / Limit", Metric2W),
        ?W("Value", Value2W),
        ?W("System Statistics / Limit", Metric3W),
        ?W("Value", Value3W)
    ]),
    Rows = [
        [
            {"Processes", format_count_limit(process_count, process_limit, SysInfo)},
            {"Dirty CPU schedulers", to_list(proplists:get_value(dirty_cpu_schedulers, SysInfo))},
            {"Dist busy limit (bytes)", to_list(proplists:get_value(dist_buf_busy_limit, SysInfo))}
        ],
        [
            {"Ports", format_count_limit(port_count, port_limit, SysInfo)},
            {"Online dirty CPU schedulers",
                to_list(proplists:get_value(dirty_cpu_schedulers_online, SysInfo))},
            {"Modules", to_list(proplists:get_value(module_count, SysInfo))}
        ],
        [
            {"Atoms", format_count_limit(atom_count, atom_limit, SysInfo)},
            {"Run Queue", to_list(proplists:get_value(run_queue, SysInfo))},
            {"ETS", format_count_limit(ets_count, ets_limit, SysInfo)}
        ]
    ],
    [Title | [render_runtime_limit_row(Row, Widths) || Row <- Rows]].

render_runtime_limit_row(
    [{Name1, Value1}, {Name2, Value2}, {Name3, Value3}],
    [Metric1W, Value1W, Metric2W, Value2W, Metric3W, Value3W]
) ->
    ?render([
        ?W(Name1, Metric1W),
        ?W(Value1, Value1W),
        ?W(Name2, Metric2W),
        ?W(Value2, Value2W),
        ?W(Name3, Metric3W),
        ?W(Value3, Value3W)
    ]).

runtime_limit_widths() ->
    observer_cli_lib:weighted_widths([30, 10, 30, 10, 30, 11], [0, 1, 0, 1, 0, 1]).

format_count_limit(CountKey, LimitKey, SysInfo) ->
    Count = proplists:get_value(CountKey, SysInfo),
    Limit = proplists:get_value(LimitKey, SysInfo),
    format_count_limit(Count, Limit).

format_count_limit(Count, Limit) when is_integer(Count), is_integer(Limit), Limit > 0 ->
    [
        integer_to_list(Count),
        " / ",
        integer_to_list(Limit),
        " (",
        observer_cli_lib:to_percent(Count / Limit),
        " used)"
    ];
format_count_limit(Count, Limit) ->
    [to_list(Count), " / ", to_list(Limit)].

collect_os_process_info(Cmd) ->
    [_, CmdValue | _] = string:split(os:cmd(Cmd), "\n", all),
    [CpuPsV, MemPsV, RssPsV, VszPsV] =
        case lists:filter(fun(Y) -> Y =/= [] end, string:split(CmdValue, " ", all)) of
            [] -> ["--", "--", "--", "--"];
            [V1, V2, V3, V4] -> [V1, V2, list_to_integer(V3) * 1024, list_to_integer(V4) * 1024]
        end,

    [
        {ps_cpu, CpuPsV ++ "%"},
        {ps_mem, MemPsV ++ "%"},
        {ps_rss, RssPsV},
        {ps_vsz, VszPsV}
    ].

collect_runtime_info() ->
    MemInfo =
        try erlang:memory() of
            Mem -> Mem
        catch
            _:_ -> []
        end,

    SchedulersOnline = erlang:system_info(schedulers_online),
    SchedulersAvailable =
        case erlang:system_info(multi_scheduling) of
            enabled -> SchedulersOnline;
            _ -> 1
        end,
    {{_, Input}, {_, Output}} = erlang:statistics(io),
    [
        {run_queue, erlang:statistics(run_queue)},
        {io_input, Input},
        {io_output, Output},

        {logical_processors, erlang:system_info(logical_processors)},
        {logical_processors_online, erlang:system_info(logical_processors_online)},
        {logical_processors_available, erlang:system_info(logical_processors_available)},
        {schedulers, erlang:system_info(schedulers)},
        {schedulers_online, SchedulersOnline},
        {schedulers_available, SchedulersAvailable},
        {dirty_cpu_schedulers, maybe_system_info(dirty_cpu_schedulers)},
        {dirty_cpu_schedulers_online, maybe_system_info(dirty_cpu_schedulers_online)},

        {otp_release, erlang:system_info(otp_release)},
        {version, erlang:system_info(version)},
        {system_architecture, erlang:system_info(system_architecture)},
        {kernel_poll, erlang:system_info(kernel_poll)},
        {smp_support, erlang:system_info(smp_support)},
        {threads, erlang:system_info(threads)},
        {thread_pool_size, erlang:system_info(thread_pool_size)},
        {wordsize_internal, erlang:system_info({wordsize, internal})},
        {wordsize_external, erlang:system_info({wordsize, external})},
        {alloc_info, alloc_info()},
        {module_count, erlang:length(code:all_loaded())},
        {process_count, erlang:system_info(process_count)},
        {process_limit, erlang:system_info(process_limit)},
        {port_count, erlang:system_info(port_count)},
        {port_limit, erlang:system_info(port_limit)},
        {atom_count, erlang:system_info(atom_count)},
        {atom_limit, maybe_system_info(atom_limit)},
        {ets_count, maybe_system_info(ets_count)},
        {ets_limit, maybe_system_info(ets_limit)},
        {dist_buf_busy_limit, maybe_system_info(dist_buf_busy_limit)}
        | MemInfo
    ].

alloc_info() ->
    Alloc = erlang:system_info(alloc_util_allocators),
    try erlang:system_info({allocator_sizes, Alloc}) of
        Allocators -> Allocators
    catch
        _:_ -> []
    end.

maybe_system_info(Key) ->
    try erlang:system_info(Key) of
        Value -> Value
    catch
        _:badarg -> undefined
    end.

fill_info([{dynamic, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        {Str, Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, Key} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case proplists:get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, SubStructure} | Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([{Str, Attrib, SubStructure} | Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([], _) ->
    [].

to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) -> observer_cli_lib:to_byte(Val);
to_list(Val) -> Val.

info_fields() ->
    Info = [
        {"System and Architecture", [
            {"System Version", otp_release},
            {"Erts Version", version},
            {"Compiled for", system_architecture},
            {"Emulator Wordsize", wordsize_external},
            {"Process Wordsize", wordsize_internal},
            {"Smp Support", smp_support},
            {"Thread Support", threads},
            {"Async thread pool size", thread_pool_size}
        ]},
        {"CPU's and Threads", [
            {"Logical CPU's", logical_processors},
            {"Online Logical CPU's", logical_processors_online},
            {"Available Logical CPU's", logical_processors_available},
            {"Schedulers", schedulers},
            {"Online schedulers", schedulers_online},
            {"Available schedulers", schedulers_available}
        ]}
    ],
    Stat = [
        {"Memory Usage", right, [
            {"Total", {bytes, total}},
            {"Processes", {bytes, processes}},
            {"Atoms", {bytes, atom}},
            {"Binaries", {bytes, binary}},
            {"Code", {bytes, code}},
            {"Ets", {bytes, ets}}
        ]},
        {"Statistics", right, [
            {"ps -o pcpu", ps_cpu},
            {"ps -o pmem", ps_mem},
            {"ps -o rss", {bytes, ps_rss}},
            {"ps -o vsz", {bytes, ps_vsz}},
            {"Total IOIn", {bytes, io_input}},
            {"Total IOOut", {bytes, io_output}}
        ]}
    ],
    {Info, Stat}.

get_cachehit_info(Seq, CacheHitInfo) ->
    [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value(
        {instance, Seq},
        CacheHitInfo
    ),
    SeqStr = lists:flatten(io_lib:format("\e[92m~2..0w| \e[0m", [Seq])),
    HitRateStr = observer_cli_lib:to_percent(HitRate),
    {SeqStr, integer_to_list(Hit), integer_to_list(Call), HitRateStr}.
