%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

%% API
-export([start/0]).
-export([start/1]).

-define(TOP_MIN_REFLUSH_INTERAL, 2000).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system
-spec start() -> quit.
start() -> start(?TOP_MIN_REFLUSH_INTERAL).
-spec start(pos_integer()) -> quit.
start(ReflushMillSecond)when ReflushMillSecond >= ?TOP_MIN_REFLUSH_INTERAL ->
  Pid = spawn_link(fun() ->
    observer_cli_lib:clear_screen(),
    loop(ReflushMillSecond, erlang:make_ref()) end),
  top(Pid).

top(Pid) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, quit);
    _ -> top(Pid)
  end.

loop(Interal, LastTimeRef) ->
  observer_cli_lib:move_cursor_to_top_line(),
  refresh(),
  erlang:cancel_timer(LastTimeRef),
  TimeRef = erlang:send_after(Interal, self(), refresh),
  receive
    quit -> quit;
    _ -> loop(Interal, TimeRef)
  end.

refresh() ->
  SysInfo = observer_backend:sys_info(),
  {Info, Stat} = info_fields(),
  SystemAndCPU = observer_lib:fill_info(Info, SysInfo),
  MemAndStatistics = observer_lib:fill_info(Stat, SysInfo),
  System = proplists:get_value("System and Architecture", SystemAndCPU),
  CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
  {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
  {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
  draw(System, CPU, Memory, Statistics).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
draw(System, CPU, Memory, Statistics) ->
  io:format("|\e[46m~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s \e[49m|~n", %%cyan background
    ["System and Architecture", "State", "CPU's and Threads", "State", "Memory Usage", "State", "Statistics", "State"]),
  NewSystem = [begin {Key, Value} end
    || {Key, Value}<- System, Key =/= "Compiled for" andalso Key =/= "Smp Support"],
  FormatGreen = "|~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|\e[32;1m~15.15s\e[0m |~n",
  FormatNormal = "|~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s |~n",
  [{_, TotalMem}|_R] = Memory,
  [begin
     {SysKey, SysVal} = lists:nth(Pos, NewSystem),
     {CpuKey, CpuVal} = lists:nth(Pos, CPU),
     {MemKey, MemVal} = lists:nth(Pos, Memory),
     {StatisKey, StatisVal} = lists:nth(Pos, Statistics),
     Format = case Pos =:= 1 of true -> FormatGreen; false -> FormatNormal end,
     io:format(Format, [SysKey, to_list(SysVal), CpuKey, to_list(CpuVal),
       MemKey, byte_to_megabyte(MemVal, TotalMem), StatisKey, to_list(StatisVal)])
   end|| Pos<- lists:seq(1, 6)],
  io:format("|~-22.22s| ~-76.76s|~-13.13s|~15.15s |~n",
    ["Compiled for", to_list(proplists:get_value("Compiled for", System)),
      "Smp Support", to_list(proplists:get_value("Smp Support", System))]),
  observer_cli_table:start().

to_list(Val) when is_integer(Val) ->
  integer_to_list(Val);
to_list(Val) when is_atom(Val) ->
  atom_to_list(Val);
to_list({time_ms, Time}) ->
   observer_cli_lib:uptime(Time);
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
  Perc = observer_cli_lib:float_to_list_with_two_digit(Val/Total),
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