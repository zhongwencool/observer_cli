%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").

%% API
-export([start/0]).
-export([start/1]).
-export([start/2]).

%%for rpc call
-export([get_system_info/1]).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system
-spec start() -> quit.
start() -> start(local_node, ?SYSTEM_MIN_INTERVAL).
-spec start(pos_integer()) -> quit.
start(RefreshMillSecond)when RefreshMillSecond >= ?SYSTEM_MIN_INTERVAL ->
  start(local_node, RefreshMillSecond).

-spec start(Node, RefreshMillSecond) -> quit when
  Node:: atom(),
  RefreshMillSecond:: pos_integer().
start(Node, RefreshMillSecond)when RefreshMillSecond >= ?SYSTEM_MIN_INTERVAL ->
  ParentPid = self(),
  Pid = spawn_link(fun() ->
    observer_cli_lib:clear_screen(),
    loop(Node, RefreshMillSecond, erlang:make_ref(), ParentPid) end),
  waiting(Node, Pid, RefreshMillSecond).

%%for fetching data from remote data by rpc:call/4
-spec get_system_info(Node) -> [tuple()] when
  Node:: atom().
get_system_info(local_node) -> observer_backend:sys_info();
get_system_info(Node) -> rpc:call(Node, ?MODULE, get_system_info, [local_node]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, ChildPid, Interval) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(ChildPid, quit);
    "o\n" ->
      erlang:send(ChildPid, go_to_home_view),
      waiting_last_draw_done_to_other_view(Node, Interval);
    "a\n" ->
      erlang:send(ChildPid, go_to_allocator_view),
      waiting_last_draw_done_to_other_view(Node, Interval);
    "h\n" ->
      erlang:send(ChildPid, go_to_help_view),
      waiting_last_draw_done_to_other_view(Node, Interval);
    "db\n" ->
      erlang:send(ChildPid, go_to_mnesia_view),
      waiting_last_draw_done_to_other_view(Node, Interval);
    _ -> waiting(Node, ChildPid, Interval)
  end.

waiting_last_draw_done_to_other_view(Node, Interval) ->
  receive
    draw_work_done_to_home_view  -> observer_cli:start(Node, ?HOME_MIN_INTERVAL);
    draw_work_done_to_allocator_view  -> observer_cli_allocator:start(Node, ?ALLOCATOR_MIN_INTERVAL);
    draw_work_done_to_help_view  -> observer_cli_help:start(Node, ?HELP_MIN_INTERVAL);
    draw_work_done_to_mnesia_view -> observer_cli_mnesia:start(Node, ?MNESIA_MIN_INTERVAL)
  after Interval -> timeout
  end.

loop(Node, Interval, LastTimeRef, ParentPid) ->
  observer_cli_lib:move_cursor_to_top_line(),
  refresh(Node),
  observer_cli_ets:start(Node),
  erlang:cancel_timer(LastTimeRef),
  TimeRef = erlang:send_after(Interval, self(), refresh),
  receive
    quit -> quit;
    go_to_home_view -> erlang:send(ParentPid, draw_work_done_to_home_view), quit;
    go_to_allocator_view -> erlang:send(ParentPid, draw_work_done_to_allocator_view), quit;
    go_to_help_view -> erlang:send(ParentPid, draw_work_done_to_help_view), quit;
    go_to_mnesia_view -> erlang:send(ParentPid, draw_work_done_to_mnesia_view), quit;
    _ -> loop(Node, Interval, TimeRef, ParentPid)
  end.

refresh(Node) ->
  SysInfo = get_system_info(Node),
  {Info, Stat} = info_fields(),
  SystemAndCPU = observer_lib:fill_info(Info, SysInfo),
  MemAndStatistics = observer_lib:fill_info(Stat, SysInfo),
  System = proplists:get_value("System and Architecture", SystemAndCPU),
  CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
  {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
  {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
  draw_menu(Node),
  draw(System, CPU, Memory, Statistics).

draw(System, CPU, Memory, Statistics) ->
  io:format("|\e[46m~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s \e[49m|~n", %%cyan background
    ["System and Architecture", "State", "CPU's and Threads", "State", "Memory Usage", "State", "Statistics", "State"]),
  NewSystem = [begin {Key, Value} end|| {Key, Value}<- System, Key =/= "Compiled for" andalso Key =/= "smp Support"],
  Format = "|~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s |~n",
  [{_, TotalMem}|_R] = Memory,
  [begin
     {SysKey, SysVal} = lists:nth(Pos, NewSystem),
     {CpuKey, CpuVal} = lists:nth(Pos, CPU),
     {MemKey, MemVal} = lists:nth(Pos, Memory),
     {StatisKey, StatisVal} =
       case lists:nth(Pos, Statistics) of
         {"Up time", _} -> {"Smp Support", to_list(proplists:get_value("Smp Support", System))};
         Value -> Value
       end,
     io:format(Format, [SysKey, to_list(SysVal), CpuKey, to_list(CpuVal),
       MemKey, byte_to_megabyte(MemVal, TotalMem), StatisKey, to_list(StatisVal)])
   end|| Pos<- lists:seq(1, 6)],
  io:format("|~-22.22s| ~-106.106s |~n", ["Compiled for", to_list(proplists:get_value("Compiled for", System))]).

draw_menu(Node) ->
  [Home, Ets, Alloc, Mnesia, Help]  = observer_cli_lib:get_menu_title(ets),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Mnesia, "|", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
  RefreshStr = "Refresh: " ++ integer_to_list(?SYSTEM_MIN_INTERVAL) ++ "ms",
  Space = lists:duplicate(?SYSTEM_BROAD - erlang:length(Title)  - erlang:length(RefreshStr)  - erlang:length(UpTime)+ 110, " "),
  io:format("~s~n", [Title ++ RefreshStr ++ Space ++ UpTime]).

to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) ->
  M = trunc(Val/(1024*1024)*1000),
  Integer = M div 1000,
  Decmial = M - Integer * 1000,
  lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decmial]));
to_list(Val) -> Val.

byte_to_megabyte({bytes, Val}, {bytes, Total}) when is_integer(Val) ->
  M = trunc(Val/(1024*1024)*1000),
  Integer = M div 1000,
  Decmial = M - Integer * 1000,
  Perc = observer_cli_lib:float_to_percent_with_two_digit(Val/Total),
  lists:flatten(io_lib:format("~w.~4..0wM ~s", [Integer, Decmial, Perc])).

info_fields() ->
  Info = [{"System and Architecture",
    [{"System Version", otp_release},
      {"Erts Version", version},
      {"Compiled for", system_architecture},
      {"Emulator Wordsize", wordsize_external},
      {"Process Wordsize", wordsize_internal},
      {"Smp Support",  smp_support},
      {"Thread Support",  threads},
      {"Async thread pool size",  thread_pool_size}
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
        {"Max Processes", process_limit},
        {"Processes", process_count},
        {"Run Queue", run_queue},
        {"Total IO In",  {bytes, io_input}},
        {"Total IO Out", {bytes, io_output}}
      ]}
         ],
  {Info, Stat}.