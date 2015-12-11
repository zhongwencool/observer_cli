%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_allocator).

-include("observer_cli.hrl").
%% API
-export([start/0]).
-export([start/2]).

%%for rpc
-export([get_average_block_sizes/1]).
-export([get_cache_hit_rates/1]).

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

-type allocator() :: temp_alloc | eheap_alloc | binary_alloc | ets_alloc
| driver_alloc | sl_alloc | ll_alloc | fix_alloc
| std_alloc.

%% @doc List Memory Allocators: std, ll, eheap, ets, fix, binary, driver.
-spec start() -> quit.
start() ->
  ParentPid = self(),
  Pid = spawn(fun() ->
    observer_cli_lib:move_cursor_to_top_line(),
    observer_cli_lib:clear_screen(),
    loop(local_node, ?ALLOCATOR_MIN_INTERVAL, ParentPid) end),
  waiting(local_node, Pid, ?ALLOCATOR_MIN_INTERVAL).

-spec start(Node, Interval) -> quit when
  Node:: atom(),
  Interval:: pos_integer().
start(Node, Interval) ->
  ParentPid = self(),
  Pid = spawn(fun() ->
    observer_cli_lib:move_cursor_to_top_line(),
    observer_cli_lib:clear_screen(),
    loop(Node, Interval, ParentPid) end),
  waiting(Node, Pid, Interval).

%%for fetching data from remote data by rpc:call/4
-spec get_cache_hit_rates(Node) -> [{{instance, non_neg_integer()}, [{Key, Val}]}] when
  Node:: atom(),
  Key:: hit_rate | hits | calls,
  Val:: term().
get_cache_hit_rates(local_node) -> recon_alloc:cache_hit_rates();
get_cache_hit_rates(Node) -> rpc:call(Node, ?MODULE, get_cache_hit_rates, [local_node]).

-spec get_average_block_sizes(Node) -> {CurrentSize, MaxSize} when
  Node:: atom(),
  CurrentSize:: [{allocator(), [{mbcs | sbcs, pos_integer()}]}],
  MaxSize:: [{allocator(), [{mbcs | sbcs, pos_integer()}]}].
get_average_block_sizes(local_node) ->
  {recon_alloc:average_block_sizes(current), recon_alloc:average_block_sizes(max)};
get_average_block_sizes(Node) ->
  rpc:call(Node, ?MODULE, get_average_block_sizes, [local_node]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, Pid, Interval) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, quit);
    "o\n" ->
      erlang:exit(Pid, stop),
      observer_cli:start(Node, ?HOME_MIN_INTERVAL);
    "e\n" ->
      erlang:exit(Pid, stop),
      observer_cli_system:start(Node, ?SYSTEM_MIN_INTERVAL);
    "h\n" ->
      erlang:exit(Pid, stop),
      observer_cli_help:start(Node, ?HELP_MIN_INTERVAL);
    "db\n" ->
      erlang:exit(Pid, stop),
      observer_cli_mnesia:start(Node, ?MNESIA_MIN_INTERVAL);
    [$r, $:| RefreshInterval] ->
      case string:to_integer(RefreshInterval) of
        {error, no_integer} -> waiting(Node, Pid, Interval);
        {NewInterval, _} when NewInterval >= ?ALLOCATOR_MIN_INTERVAL ->
          erlang:send(Pid, {new_interval, NewInterval}),
          waiting(Node, Pid, NewInterval)
      end;
    _ -> waiting(Node, Pid, Interval)
  end.

loop(Node, Interval, ParentPid) ->
  CacheHitInfo = get_cache_hit_rates(Node),
  {AverageBlockCurs, AverageBlockMaxs}  = get_average_block_sizes(Node),

  observer_cli_lib:move_cursor_to_top_line(),
  draw_menu(Node),
  draw_average_block_size_info(AverageBlockCurs, AverageBlockMaxs),
  draw_cache_hit_rates(CacheHitInfo),
  draw_last_line(),
  erlang:send_after(Interval, self(), refresh),
  receive
    quit -> quit;
    refresh -> loop(Node, Interval, ParentPid);
    {new_interval, NewInterval} -> loop(Node, NewInterval, ParentPid)
  end.

draw_menu(Node) ->
  [Home, Ets, Alloc, Mnesia, Help]  = observer_cli_lib:get_menu_title(allocator),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Mnesia, "|", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
  RefreshStr = "Refresh: " ++ integer_to_list(?ALLOCATOR_MIN_INTERVAL) ++ "ms",
  Space = lists:duplicate(?ALLOCATOR_BROAD - erlang:length(Title)  - erlang:length(RefreshStr)  - erlang:length(UpTime)+ 110, " "),
  io:format("~s~n", [Title ++ RefreshStr ++ Space ++ UpTime]).

draw_cache_hit_rates(CacheHitInfo) ->
  io:format("|\e[46m    ~8.8s    |    ~-6.6s | ~7.7s    |~89.89s\e[49m|~n", ["Instance", "Hits", "Calls", "Hit Rate"]),
  Format = "|     ~4.4s       |~10.10s | ~-11.11s|~-82.82s ~6.6s|~n",
  Len = erlang:length(CacheHitInfo),
  [begin
     [{hit_rate, HitRate}, {hits, Hit}, {calls, Call}] = proplists:get_value({instance, Seq}, CacheHitInfo),
     HitRateStr = observer_cli_lib:float_to_percent_with_two_digit(HitRate),
     SeqStr = lists:flatten(io_lib:format("~2..0w", [Seq])),
     RealyHitRate = case Hit == 0 andalso Call == 0 of true -> 0; false -> HitRate end,
     Process = lists:duplicate(trunc(RealyHitRate * 82), "|"),
     io:format(Format, [SeqStr, observer_cli_lib:to_list(Hit), observer_cli_lib:to_list(Call), Process, HitRateStr])
   end|| Seq <- lists:seq(0, Len - 1)].

draw_average_block_size_info(AverageBlockCurs, AverageBlockMaxs) ->
  io:format("|\e[46m~-16.16s| ~-26.26s | ~-26.26s |~-28.28s| ~-25.25s \e[49m|~n",
    ["Allocator Type", "Current Multiblock Carriers", "Max Multiblock Carriers",
      "Current SingleBlock Carriers", "Max Single Block Carriers"]),
  Format = "|~-16.16s|  ~24.24s  |  ~24.24s  |  ~24.24s  | ~24.24s  |~n",
  [begin
     Content = get_alloc(AllocKey, AverageBlockCurs, AverageBlockMaxs),
     io:format(Format, Content)
   end||AllocKey <-?UTIL_ALLOCATORS],
  ok.

draw_last_line()  ->
  io:format("|\e[31;1mINPUT: \e[0m\e[44mq(quit)      ~111.111s\e[49m|~n", ["r:5000(refresh every 5000ms)"]).

get_alloc(Key, Curs, Maxs) ->
  CurRes = proplists:get_value(Key, Curs),
  MaxRes = proplists:get_value(Key, Maxs),
  CurMbcs = proplists:get_value(mbcs, CurRes),
  CurSbcs = proplists:get_value(sbcs, CurRes),
  MaxMbcs = proplists:get_value(mbcs, MaxRes),
  MaxSbcs = proplists:get_value(sbcs, MaxRes),
  [atom_to_list(Key),
    observer_cli_lib:to_megabyte_str(CurMbcs),
    observer_cli_lib:to_megabyte_str(MaxMbcs),
    observer_cli_lib:to_megabyte_str(CurSbcs),
    observer_cli_lib:to_megabyte_str(MaxSbcs)].
