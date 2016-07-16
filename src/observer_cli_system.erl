%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").

%% API
-export([start/2]).

%%for rpc call
-export([get_system_info/1]).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system

-spec start(Node, ViewOpts) -> no_return() when
      Node:: atom(),
      ViewOpts:: view_opts().
start(Node, #view_opts{sys = #system{interval = RefreshMillSecond, rows = Rows}} = ViewOpts) ->
    ParentPid = self(),
    Pid = spawn(fun() ->
                        observer_cli_lib:clear_screen(),
                        loop(Node, RefreshMillSecond, Rows, erlang:make_ref(), ParentPid) 
                end),
    waiting(Node, Pid, ViewOpts).

%%for fetching data from remote data by rpc:call/4
-spec get_system_info(Node) -> [tuple()] when
      Node:: atom().
get_system_info(local_node) -> sys_info();
get_system_info(Node) -> rpc:call(Node, ?MODULE, get_system_info, [local_node]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, ChildPid, #view_opts{sys = SysOpts} = ViewOpts) ->
    Input = observer_cli_lib:get_line(""),
    case  Input of
        "q\n" -> erlang:send(ChildPid, quit);
        "o\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli:start_node(Node, ViewOpts);
        "a\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_allocator:start(Node, ViewOpts);
        "h\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_help:start(Node, ViewOpts);
        "n\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts);
        "db\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_mnesia:start(Node, ViewOpts);
        [$i| Interval] ->
            case string:to_integer(Interval) of
                {error, no_integer} -> waiting(Node, ChildPid, ViewOpts);
                {NewInterval, _} when NewInterval >= ?SYSTEM_MIN_INTERVAL ->
                    erlang:send(ChildPid, {new_interval, NewInterval}),
                    waiting(Node, ChildPid, ViewOpts#view_opts{sys = SysOpts#system{interval = NewInterval}});
                {_Interval, _} -> waiting(Node, ChildPid, ViewOpts)
            end;
        [$r, $o, $w| NewRows] ->
            case string:to_integer(NewRows) of
                {error, no_integer} -> waiting(Node, ChildPid, ViewOpts);
                {NewRows2, _} ->
                    erlang:send(ChildPid, {new_rows, NewRows2}),
                    waiting(Node, ChildPid, ViewOpts#view_opts{sys = SysOpts#system{rows = NewRows2}})
            end;
        _ -> waiting(Node, ChildPid, ViewOpts)
    end.

loop(Node, Interval, Rows, LastTimeRef, ParentPid) ->
    observer_cli_lib:move_cursor_to_top_line(),
    refresh(Node, Interval),
    observer_cli_ets:draw_ets_info(Node, Rows),
    draw_last_line(Interval),
    erlang:cancel_timer(LastTimeRef),
    TimeRef = erlang:send_after(Interval, self(), refresh),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> 
            loop(Node, NewInterval, Rows, TimeRef, ParentPid);
        {new_rows, NewRows} -> 
            observer_cli_lib:clear_screen(),
            loop(Node, Interval, NewRows, TimeRef, ParentPid);
        _ -> 
            loop(Node, Interval, Rows, TimeRef, ParentPid)
    end.

refresh(Node, Interval) ->
    SysInfo = get_system_info(Node),
    {Info, Stat} = info_fields(),
    SystemAndCPU = fill_info(Info, SysInfo),
    MemAndStatistics = fill_info(Stat, SysInfo),
    System = proplists:get_value("System and Architecture", SystemAndCPU),
    CPU = proplists:get_value("CPU's and Threads", SystemAndCPU),
    {_, _, Memory} = lists:keyfind("Memory Usage", 1, MemAndStatistics),
    {_, _, Statistics} = lists:keyfind("Statistics", 1, MemAndStatistics),
    draw_menu(Node, Interval),
    draw(System, CPU, Memory, Statistics).

draw(System, CPU, Memory, Statistics) ->
    io:format("|\e[0m\e[44m~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s \e[49m|~n", %%cyan background
              ["System and Architecture", "State", "CPU's and Threads", "State", "Memory Usage", "State", "Statistics", "State"]),
    NewSystem = [begin {Key, Value} end|| {Key, Value}<- System, Key =/= "Compiled for" andalso Key =/= "smp Support"],
    Format = "|~-22.22s| ~-8.8s|~-23.23s| ~-7.7s|~-15.15s|~18.18s|~-13.13s|~15.15s |~n",
    [{_, TotalMem}|_R] = Memory,
    [begin
         {SysKey, SysVal} = lists:nth(Pos, NewSystem),
         {CpuKey, CpuVal} = lists:nth(Pos, CPU),
         {MemKey, MemVal} = lists:nth(Pos, Memory),
         {StatisticsKey, StatisticsVal} =
             case lists:nth(Pos, Statistics) of
                 {"Up time", _} -> {"Smp Support", to_list(proplists:get_value("Smp Support", System))};
                 Value -> Value
             end,
         io:format(Format, [SysKey, to_list(SysVal), CpuKey, to_list(CpuVal),
                            MemKey, byte_to_megabyte(MemVal, TotalMem), StatisticsKey, to_list(StatisticsVal)])
     end|| Pos<- lists:seq(1, 6)],
    io:format("|~-22.22s| ~-106.106s |~n", ["Compiled for", to_list(proplists:get_value("Compiled for", System))]).

draw_menu(Node, Interval) ->
    Title  = observer_cli_lib:get_menu_title(ets),
    UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
    RefreshStr = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    SpaceLen = ?COLUMN_WIDTH - erlang:length(Title)  - erlang:length(RefreshStr)  - erlang:length(UpTime)+ 130,
    Space = case SpaceLen > 0 of  true -> lists:duplicate(SpaceLen, " "); false -> [] end,
    io:format("~s~n", [Title ++ RefreshStr ++ Space ++ UpTime]).

draw_last_line(Interval)  ->
    Text = io_lib:format("i~w(Interval ~wms must >=5000ms) row10(show 10 rows)", [Interval, Interval]),
    io:format("|\e[31;1mINPUT: \e[0m\e[44mq(quit)      ~-111.111s\e[49m|~n", [Text]).

to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) ->
    M = trunc(Val/(1024*1024)*1000),
    Integer = M div 1000,
    Decimal = M - Integer * 1000,
    lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decimal]));
to_list(Val) -> Val.

byte_to_megabyte({bytes, Val}, {bytes, Total}) when is_integer(Val) ->
    M = trunc(Val/(1024*1024)*1000),
    Integer = M div 1000,
    Decimal = M - Integer * 1000,
    Percent = observer_cli_lib:float_to_percent_with_two_digit(Val/Total),
    lists:flatten(io_lib:format("~w.~4..0wM ~s", [Integer, Decimal, Percent])).

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
     {io_output,  Output},

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

fill_info([{dynamic, Key}|Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        {Str, Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key}|Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, Key}|Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}}|Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}}|Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, SubStructure}|Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)}|fill_info(Rest, Data)];
fill_info([{Str, Attrib, SubStructure}|Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)}|fill_info(Rest, Data)];
fill_info([], _) -> [].

get_value(Key, Data) ->
    proplists:get_value(Key, Data).

