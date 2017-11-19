%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_allocator).

-include("observer_cli.hrl").
%% API
-export([start/1]).

-define(UTIL_ALLOCATORS,
    [binary_alloc,
        driver_alloc,
        eheap_alloc,
        ets_alloc,
        fix_alloc,
        ll_alloc,
        sl_alloc,
        std_alloc,
        temp_alloc
    ]).

%% @doc List Memory Allocators: std, ll, eheap, ets, fix, binary, driver.
-spec start(ViewOpts) -> no_return when
    ViewOpts :: view_opts().
start(#view_opts{allocate = #allocate{interval = Interval}} = ViewOpts) ->
    Pid = spawn(fun() ->
        ?output(?CLEAR),
        render_worker(Interval, undefined)
                end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, #view_opts{allocate = AllocatorOpts} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, Pid) of
        quit -> erlang:send(Pid, quit);
        {new_interval, NewInterval} ->
            erlang:send(Pid, {new_interval, NewInterval}),
            NewAllocate = AllocatorOpts#allocate{interval = NewInterval},
            manager(Pid, ViewOpts#view_opts{allocate = NewAllocate});
        _ -> manager(Pid, ViewOpts)
    end.

render_worker(Interval, LastTimeRef) ->
    CacheHitInfo = recon_alloc:cache_hit_rates(),
    AverageBlockCurs = recon_alloc:average_block_sizes(current),
    AverageBlockMaxes = recon_alloc:average_block_sizes(max),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(allocator, Text, 133),
    BlockView = render_average_block_size_info(AverageBlockCurs, AverageBlockMaxes),
    HitView = render_cache_hit_rates(CacheHitInfo),
    LastLine = render_last_line(Interval),
    ?output([?CURSOR_TOP, Menu, BlockView, HitView, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> render_worker(NewInterval, NextTimeRef);
        redraw -> render_worker(Interval, NextTimeRef)
    end.

render_cache_hit_rates(CacheHitInfo) ->
    Title = ?render([?BLUE_BG, ?W("Instance", 8), ?W("Hits", 10), ?W("Calls", 11), ?W("Hit Rate", 100), ?RESET_BG]),
    Len = erlang:length(CacheHitInfo),
    View = [begin
         [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value({instance, Seq}, CacheHitInfo),
         HitRateStr = observer_cli_lib:float_to_percent_with_two_digit(HitRate),
         SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
         TrueHitRate = case Hit == 0 andalso Call == 0 of true -> 0; false -> HitRate end,
         Process = lists:duplicate(trunc(TrueHitRate * 91), "|"),
         ?render([?W(SeqStr, 8),
             ?W(observer_cli_lib:to_list(Hit), 10),
             ?W(observer_cli_lib:to_list(Call), 11),
             ?W(Process, 91),
             ?W(HitRateStr, 6)])
            end || Seq <- lists:seq(0, Len - 1)],
    [Title|View].

render_average_block_size_info(AverageBlockCurs, AverageBlockMaxes) ->
    Title = ?render([ ?BLUE_BG,
        ?W("Allocator Type", 16), ?W("Current Multiblock Carriers", 28),
        ?W("Max Multiblock Carriers", 28), ?W("Current SingleBlock Carriers", 27),
        ?W("Max Single Block Carriers", 27),
        ?RESET_BG]),
    View =
        [begin
             [Type, CMC, MMC, CSC, MSBC] = get_alloc(AllocKey, AverageBlockCurs, AverageBlockMaxes),
             ?render([?W(Type, 16), ?W(CMC, 28), ?W(MMC, 28), ?W(CSC, 27), ?W(MSBC, 27)])
         end || AllocKey <- ?UTIL_ALLOCATORS],
    [Title|View].

render_last_line(Interval) ->
    Text = io_lib:format("i~w(Interval ~wms must >=1000ms) ", [Interval, Interval]),
    ?render([?UNDERLINE, ?RED, "INPUT:", ?RESET, ?BLUE_BG, "q(quit) ",
        ?W(Text, ?COLUMN - 11), ?RESET_BG]).

get_alloc(Key, Curs, Maxes) ->
    CurRes = proplists:get_value(Key, Curs),
    MaxRes = proplists:get_value(Key, Maxes),
    CurMbcs = proplists:get_value(mbcs, CurRes),
    CurSbcs = proplists:get_value(sbcs, CurRes),
    MaxMbcs = proplists:get_value(mbcs, MaxRes),
    MaxSbcs = proplists:get_value(sbcs, MaxRes),
    [atom_to_list(Key),
        observer_cli_lib:to_megabyte_str(CurMbcs),
        observer_cli_lib:to_megabyte_str(MaxMbcs),
        observer_cli_lib:to_megabyte_str(CurSbcs),
        observer_cli_lib:to_megabyte_str(MaxSbcs)].
