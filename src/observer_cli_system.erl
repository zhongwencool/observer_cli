%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").
%% API
-export([start/1]).

-define(UTIL_ALLOCATORS,
    [
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
-spec start(ViewOpts) -> no_return when
    ViewOpts :: view_opts().
start(#view_opts{sys = #system{interval = Interval}} = ViewOpts) ->
    Pid = spawn(fun() ->
        ?output(?CLEAR),
        render_worker(Interval, ?INIT_TIME_REF)
                end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, #view_opts{sys = AllocatorOpts} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, Pid) of
        quit -> erlang:send(Pid, quit);
        {new_interval, NewInterval} ->
            erlang:send(Pid, {new_interval, NewInterval}),
            NewAllocate = AllocatorOpts#system{interval = NewInterval},
            manager(Pid, ViewOpts#view_opts{sys = NewAllocate});
        _ -> manager(Pid, ViewOpts)
    end.

render_worker(Interval, LastTimeRef) ->
    CacheHitInfo = recon_alloc:cache_hit_rates(),
    AverageBlockCurs = recon_alloc:average_block_sizes(current),
    AverageBlockMaxes = recon_alloc:average_block_sizes(max),
    Sys = render_sys_info(),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(allocator, Text),
    BlockView = render_average_block_size_info(AverageBlockCurs, AverageBlockMaxes),
    HitView = render_cache_hit_rates(CacheHitInfo),
    LastLine = observer_cli_lib:render_last_line("q(quit)"),
    ?output([?CURSOR_TOP, Menu, Sys, BlockView, HitView, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> render_worker(NewInterval, NextTimeRef);
        redraw -> render_worker(Interval, NextTimeRef)
    end.

render_cache_hit_rates(CacheHitInfo) ->
    Title = ?render([?UNDERLINE, ?GRAY_BG, ?W("Instance", 8), ?W("Hits", 10), ?W("Calls", 11), ?W("Hit Rate", 99), ?RESET]),
    Len = erlang:length(CacheHitInfo),
    View = [begin
         [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value({instance, Seq}, CacheHitInfo),
         HitRateStr = observer_cli_lib:to_percent(HitRate),
         SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
         TrueHitRate = case Hit == 0 andalso Call == 0 of true -> 0; false -> HitRate end,
         Process = lists:duplicate(trunc(TrueHitRate * 91), "|"),
         ?render([?W(SeqStr, 8),
             ?W(observer_cli_lib:to_list(Hit), 10),
             ?W(observer_cli_lib:to_list(Call), 11),
             ?W(Process, 90),
             ?W(HitRateStr, 6)])
            end || Seq <- lists:seq(0, Len - 1)],
    [Title|View].

render_average_block_size_info(AverageBlockCurs, AverageBlockMaxes) ->
    Title = ?render([ ?UNDERLINE, ?GRAY_BG,
        ?W("Allocator Type", 16), ?W("Current Multiblock Carriers", 28),
        ?W("Max Multiblock Carriers", 28), ?W("Current SingleBlock Carriers", 27),
        ?W("Max Single Block Carriers", 26), ?RESET]),
    View =
        [begin
             [Type, CMC, MMC, CSC, MSBC] = get_alloc(AllocKey, AverageBlockCurs, AverageBlockMaxes),
             ?render([?W(Type, 16), ?W(CMC, 28), ?W(MMC, 28), ?W(CSC, 27), ?W(MSBC, 26)])
         end || AllocKey <- ?UTIL_ALLOCATORS],
    [Title|View].

get_alloc(Key, Curs, Maxes) ->
    CurRes = proplists:get_value(Key, Curs),
    MaxRes = proplists:get_value(Key, Maxes),
    CurMbcs = proplists:get_value(mbcs, CurRes),
    CurSbcs = proplists:get_value(sbcs, CurRes),
    MaxMbcs = proplists:get_value(mbcs, MaxRes),
    MaxSbcs = proplists:get_value(sbcs, MaxRes),
    [atom_to_list(Key),
        observer_cli_lib:to_byte(CurMbcs),
        observer_cli_lib:to_byte(MaxMbcs),
        observer_cli_lib:to_byte(CurSbcs),
        observer_cli_lib:to_byte(MaxSbcs)].

render_sys_info() ->
    SysInfo = sys_info(),
    {Info, Stat} = info_fields(),
    SystemAndCPU = fill_info(Info, SysInfo),
    MemAndStatistics = fill_info(Stat, SysInfo),
    System = proplists:get_value("System and Architecture", SystemAndCPU),
    CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
    {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
    {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
    render_sys_info(System, CPU, Memory, Statistics).

render_sys_info(System, CPU, Memory, Statistics) ->
    Title = ?render([?GRAY_BG, ?UNDERLINE,
        ?W("System/Architecture", 22),
        ?W("State", 8),
        ?W("CPU's and Threads", 23),
        ?W("State", 7),
        ?W("Memory Usage", 11),
        ?W("State", 22),
        ?W("Statistics", 11),
        ?W("State", 12),
        ?RESET]),
    NewSystem = [begin {Key, Value} end || {Key, Value} <- System,
        Key =/= "Compiled for" andalso Key =/= "smp Support"],
    [{_, TotalMem} | _R] = Memory,
    {bytes, TotalMemInt} = TotalMem,
    Row =
        [begin
             {SysKey, SysVal} = lists:nth(Pos, NewSystem),
             {CpuKey, CpuVal} = lists:nth(Pos, CPU),
             {MemKey, MemVal} = lists:nth(Pos, Memory),
             {StatisticsKey, StatisticsVal} =
                 case lists:nth(Pos, Statistics) of
                     {"Up time", _} -> {"Smp Support", to_list(proplists:get_value("Smp Support", System))};
                     Value -> Value
                 end,
             {bytes, MemValInt} = MemVal,
             Percent = observer_cli_lib:to_percent(MemValInt / TotalMemInt),
             ?render([
                 ?W(SysKey, 22), ?W(to_list(SysVal), 8),
                 ?W(CpuKey, 23), ?W(to_list(CpuVal), 7),
                 ?W(MemKey, 11), ?W(observer_cli_lib:to_byte(MemValInt), 13), ?W(Percent, 6),
                 ?W(StatisticsKey, 11), ?W(to_list(StatisticsVal), 12)
             ])
         end || Pos <- lists:seq(1, 6)],
    Compile = ?render([
        ?UNDERLINE,
        ?W("compiled for", 22), ?W(to_list(proplists:get_value("Compiled for", System)), 112),
        ?RESET]),
    [Title | (Row ++ [Compile])].

sys_info() ->
    MemInfo = try erlang:memory() of
                  Mem -> Mem
              catch _:_ -> []
              end,
    
    SchedulersOnline = erlang:system_info(schedulers_online),
    SchedulersAvailable = case erlang:system_info(multi_scheduling) of
                              enabled -> SchedulersOnline;
                              _ -> 1
                          end,
    
    {{_, Input}, {_, Output}} = erlang:statistics(io),
    [{process_count, erlang:system_info(process_count)},
        {process_limit, erlang:system_info(process_limit)},
        {uptime, element(1, erlang:statistics(wall_clock))},
        {run_queue, erlang:statistics(run_queue)},
        {io_input, Input},
        {io_output, Output},
        
        {logical_processors, erlang:system_info(logical_processors)},
        {logical_processors_online, erlang:system_info(logical_processors_online)},
        {logical_processors_available, erlang:system_info(logical_processors_available)},
        {schedulers, erlang:system_info(schedulers)},
        {schedulers_online, SchedulersOnline},
        {schedulers_available, SchedulersAvailable},
        
        {otp_release, erlang:system_info(otp_release)},
        {version, erlang:system_info(version)},
        {system_architecture, erlang:system_info(system_architecture)},
        {kernel_poll, erlang:system_info(kernel_poll)},
        {smp_support, erlang:system_info(smp_support)},
        {threads, erlang:system_info(threads)},
        {thread_pool_size, erlang:system_info(thread_pool_size)},
        {wordsize_internal, erlang:system_info({wordsize, internal})},
        {wordsize_external, erlang:system_info({wordsize, external})},
        {alloc_info, alloc_info()}
        | MemInfo].


alloc_info() ->
    Alloc = erlang:system_info(alloc_util_allocators),
    try erlang:system_info({allocator_sizes, Alloc}) of
        Allocators -> Allocators
    catch _:_ -> []
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
fill_info([], _) -> [].


to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) -> observer_cli_lib:to_byte(Val);
to_list(Val) -> Val.

info_fields() ->
    Info = [{"System and Architecture",
        [{"System Version", otp_release},
            {"Erts Version", version},
            {"Compiled for", system_architecture},
            {"Emulator Wordsize", wordsize_external},
            {"Process Wordsize", wordsize_internal},
            {"Smp Support", smp_support},
            {"Thread Support", threads},
            {"Async thread pool size", thread_pool_size}
        ]},
        {"CPU's and Threads",
            [{"Logical CPU's", logical_processors},
                {"Online Logical CPU's", logical_processors_online},
                {"Available Logical CPU's", logical_processors_available},
                {"Schedulers", schedulers},
                {"Online schedulers", schedulers_online},
                {"Available schedulers", schedulers_available}
            ]}
    ],
    Stat = [{"Memory Usage", right,
        [{"Total", {bytes, total}},
            {"Processes", {bytes, processes}},
            {"Atoms", {bytes, atom}},
            {"Binaries", {bytes, binary}},
            {"Code", {bytes, code}},
            {"Ets", {bytes, ets}}
        ]},
        {"Statistics", right,
            [{"Up time", {time_ms, uptime}},
                {"Max Process", process_limit},
                {"Processes", process_count},
                {"Run Queue", run_queue},
                {"Total IO In", {bytes, io_input}},
                {"Total IO Out", {bytes, io_output}}
            ]}
    ],
    {Info, Stat}.
