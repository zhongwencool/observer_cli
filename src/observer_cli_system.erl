%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_system).

-include("observer_cli.hrl").

%% API
-export([start/1]).

%% @doc List System and Architecture, CPU's and Threads metrics  in observer's system

-spec start(ViewOpts) -> no_return() when
    ViewOpts :: view_opts().
start(#view_opts{sys = #system{interval = Interval}, terminal_row = TerminalRow} = ViewOpts) ->
    Pid = spawn(fun() ->
        ?output(?CLEAR),
        render_worker(Interval, undefined, TerminalRow)
                end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, #view_opts{sys = SysOpts} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ChildPid) of
        quit -> erlang:send(ChildPid, quit);
        {new_interval, NewMs} = Msg ->
            erlang:send(ChildPid, Msg),
            NewSysOpts = SysOpts#system{interval = NewMs},
            manager(ChildPid, ViewOpts#view_opts{sys = NewSysOpts});
        _ -> manager(ChildPid, ViewOpts)
    end.

render_worker(Interval, LastTimeRef, TerminalRow0) ->
    {ok, TerminalRow} =
        case TerminalRow0 of
            undefined -> io:rows();
            _ -> {ok, TerminalRow0}
        end,
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(ets, Text, 133),
    Sys = render_sys_info(),
    Ets = observer_cli_ets:render_ets_info(erlang:max(0, TerminalRow - 12)),
    LastLine = render_last_line(Interval),
    ?output([?CURSOR_TOP, Menu, Sys, Ets, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewMs} -> render_worker(NewMs, NextTimeRef, TerminalRow);
        _ -> render_worker(Interval, NextTimeRef, TerminalRow)
    end.

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
    Title = ?render([?BLUE_BG,
        ?W("System/Architecture", 22),
        ?W("State", 8),
        ?W("CPU's and Threads", 23),
        ?W("State", 7),
        ?W("Memory Usage", 15),
        ?W("State", 18),
        ?W("Statistics", 13),
        ?W("State", 11),
        ?RESET_BG]),
    NewSystem = [begin {Key, Value} end || {Key, Value} <- System,
        Key =/= "Compiled for" andalso Key =/= "smp Support"],
    [{_, TotalMem} | _R] = Memory,
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
             ?render([?W(SysKey, 22), ?W(to_list(SysVal), 8), ?W(CpuKey, 23), ?W(to_list(CpuVal), 7),
                 ?W(MemKey, 15), ?W(byte_to_megabyte(MemVal, TotalMem), 18),
                 ?W(StatisticsKey, 13), ?W(to_list(StatisticsVal), 11)])
         end || Pos <- lists:seq(1, 6)],
    Compile = ?render([?UNDERLINE, ?W("compiled for", 22),
        ?W(to_list(proplists:get_value("Compiled for", System)), 113), ?RESET]),
    [Title|(Row ++ [Compile])].

render_last_line(Interval) ->
    Text = io_lib:format("i~w(Interval ~wms must >=1000ms) ", [Interval, Interval]),
    ?render([?UNDERLINE,?RED, "INPUT:", ?RESET, ?BLUE_BG, "q(quit) ",
        ?W(Text, ?COLUMN - 11), ?RESET_BG]).

to_list(Val) when is_integer(Val) -> integer_to_list(Val);
to_list(Val) when is_atom(Val) -> atom_to_list(Val);
to_list({bytes, Val}) ->
    M = trunc(Val / (1024 * 1024) * 1000),
    Integer = M div 1000,
    Decimal = M - Integer * 1000,
    lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decimal]));
to_list(Val) -> Val.

byte_to_megabyte({bytes, Val}, {bytes, Total}) when is_integer(Val) ->
    M = trunc(Val / (1024 * 1024) * 1000),
    Integer = M div 1000,
    Decimal = M - Integer * 1000,
    Percent = observer_cli_lib:float_to_percent_with_two_digit(Val / Total),
    io_lib:format("~w.~4..0wM ~s", [Integer, Decimal, Percent]).

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
                {"Max Processes", process_limit},
                {"Processes", process_count},
                {"Run Queue", run_queue},
                {"Total IO In", {bytes, io_input}},
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
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        {Str, Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key} | Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, Key} | Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}} | Rest], Data) when is_atom(Key) ->
    case get_value(Key, Data) of
        undefined -> [undefined | fill_info(Rest, Data)];
        Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, SubStructure} | Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([{Str, Attrib, SubStructure} | Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)} | fill_info(Rest, Data)];
fill_info([], _) -> [].

get_value(Key, Data) ->
    proplists:get_value(Key, Data).
