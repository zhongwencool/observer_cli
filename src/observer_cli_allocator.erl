%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_allocator).

%% API
-export([start/0]).

-define(TOP_MIN_REFLUSH_INTERVAL, 5000).
-define(BROAD, 102).

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
start() ->
  ParentPid = self(),
  Pid = spawn_link(fun() ->
    observer_cli_lib:move_cursor_to_top_line(),
    observer_cli_lib:clear_screen(),
    loop(?TOP_MIN_REFLUSH_INTERVAL, ParentPid) end),
  waiting(Pid, ?TOP_MIN_REFLUSH_INTERVAL).

waiting(Pid, Interval) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, quit);
    "o\n" ->
      erlang:send(Pid, go_to_home_view),
      waiting_last_draw_done_to_other_view(Interval);
    "e\n" ->
      erlang:send(Pid, go_to_ets_view),
      waiting_last_draw_done_to_other_view(Interval);
    "h\n" ->
      erlang:send(Pid, go_to_help_view),
      waiting_last_draw_done_to_other_view(Interval);
    _ -> waiting(Pid, Interval)
  end.

waiting_last_draw_done_to_other_view(Interval) ->
  receive
    draw_work_done_to_home_view ->
      observer_cli:start();
    draw_work_done_to_ets_view ->
      observer_cli_allocator:start();
    draw_work_done_to_help_view ->
      observer_cli_help:start()
  after Interval -> time_out
  end.

loop(Interval, ParentPid) ->
  CacheHitInfo = recon_alloc:cache_hit_rates(),
  AverageBlockCurs = recon_alloc:average_block_sizes(current),
  AverageBlockMaxs = recon_alloc:average_block_sizes(max),

  observer_cli_lib:move_cursor_to_top_line(),
  draw_menu(),
  draw_average_block_size_info(AverageBlockCurs, AverageBlockMaxs),
  draw_cache_hit_rates(CacheHitInfo),
  erlang:send_after(Interval, self(), refresh),
  receive
    quit -> quit;
    refresh -> loop(Interval, ParentPid);
    go_to_ets_view ->  erlang:send(ParentPid, draw_work_done_to_ets_view), quit;
    go_to_home_view -> erlang:send(ParentPid, draw_work_done_to_home_view), quit;
    go_to_help_view ->  erlang:send(ParentPid, draw_work_done_to_help_view), quit
  end.

draw_menu() ->
  [Home, Ets, Alloc, Help]  = observer_cli_lib:get_menu_title(allocator),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime()) ++ "|",
  RefreshStr = "Refresh: " ++ integer_to_list(?TOP_MIN_REFLUSH_INTERVAL) ++ "ms",
  Space = lists:duplicate(?BROAD - erlang:length(Title)  - erlang:length(RefreshStr)  - erlang:length(UpTime)+ 90, " "),
  io:format("~s~n", [Title ++ RefreshStr ++ Space ++ UpTime]).

draw_cache_hit_rates(CacheHitInfo) ->
  io:format("|\e[46m~8.8s|  ~6.6s  |  ~7.7s  |~68.68s\e[49m|~n",["Instance", "Hits", "Calls", "Hit Rate"]),
  Format = "| ~4.4s   |~10.10s|~-11.11s|~-61.61s ~6.6s|~n",
  Len = erlang:length(CacheHitInfo),
  [begin
     [{hit_rate, HitRate},{hits, Hit},{calls, Call}] = proplists:get_value({instance, Seq}, CacheHitInfo),
     HitRateStr = observer_cli_lib:float_to_percent_with_two_digit(HitRate),
     SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
     Process = lists:duplicate(trunc(HitRate * 61), "|"),
     io:format(Format, [SeqStr, observer_cli_lib:to_list(Hit), observer_cli_lib:to_list(Call), Process, HitRateStr])
   end|| Seq <- lists:seq(0, Len - 1)].

draw_average_block_size_info(AverageBlockCurs, AverageBlockMaxs) ->
  io:format("|\e[46m~-16.16s|~20.20s|~20.20s|~20.20s|~20.20s\e[49m|~n",
    ["Allocator Type", "current    mbcs", "max    mbcs", "current      sbcs", "max      sbcs"]),
  Format = "|~-16.16s|~20.20s|~20.20s|~20.20s|~20.20s|~n",
  [begin
     Content = get_alloc(AllocKey, AverageBlockCurs, AverageBlockMaxs),
     io:format(Format, Content)
   end||AllocKey <-?UTIL_ALLOCATORS],
  ok.

get_alloc(Key, Curs, Maxs) ->
  CurRes = proplists:get_value(Key, Curs),
  MaxRes = proplists:get_value(Key, Maxs),
  CurMbcs = proplists:get_value(mbcs, CurRes),
  CurSbcs = proplists:get_value(sbcs, CurRes),
  MaxMbcs = proplists:get_value(mbcs, MaxRes),
  MaxSbcs = proplists:get_value(sbcs, MaxRes),
  [atom_to_list(Key),
    observer_cli_lib:to_megabyte_list(CurMbcs),
    observer_cli_lib:to_megabyte_list(MaxMbcs),
    observer_cli_lib:to_megabyte_list(CurSbcs),
    observer_cli_lib:to_megabyte_list(MaxSbcs)].
