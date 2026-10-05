%% OS process counters for the TUI. No scheduler measurement is enabled here.
-module(observer_cli_runtime_metrics).

-export([
    init/1,
    sample/1,
    window/2,
    format_cpu/1,
    format_rss/1, format_rss/2,
    format_window/1,
    system_info/1
]).

-ifdef(TEST).
-export([parse_linux/3, parse_macos/1, command/3, parse_cpu_time/1]).
-endif.

-define(MAX_OUTPUT, 4096).

-spec init(pos_integer()) -> map().
init(Interval) ->
    Timeout = min(500, Interval),
    Base = #{identity => {node(), os:getpid()}, timeout_ms => Timeout},
    case os:type() of
        {unix, linux} ->
            Getconf = os:find_executable("getconf"),
            Base#{
                platform => linux,
                ticks => unit(Getconf, "CLK_TCK", Timeout),
                page_size => unit(Getconf, "PAGESIZE", Timeout)
            };
        {unix, darwin} ->
            Base#{platform => darwin};
        _ ->
            Base#{platform => unsupported}
    end.

unit(false, _Name, _Timeout) ->
    undefined;
unit(Path, Name, Timeout) ->
    case command(Path, [Name], Timeout) of
        {ok, Output} -> positive_integer(string:trim(Output));
        {error, _} -> undefined
    end.

-spec sample(map()) -> map().
sample(Context) ->
    Start = erlang:monotonic_time(microsecond),
    Data = read_sample(Context),
    End = erlang:monotonic_time(microsecond),
    Data#{
        identity => maps:get(identity, Context),
        monotonic_us => (Start + End) div 2,
        collection_us => End - Start
    }.

read_sample(#{platform := linux, ticks := Ticks, page_size := PageSize}) ->
    case file:read_file("/proc/self/stat") of
        {ok, Data} -> parse_linux(Data, Ticks, PageSize);
        {error, _} -> empty_sample()
    end;
read_sample(#{platform := darwin, identity := {_, Pid}, timeout_ms := Timeout}) ->
    case command("/bin/ps", ["-p", Pid, "-o", "time=", "-o", "rss=", "-o", "vsz="], Timeout) of
        {ok, Data} -> parse_macos(Data);
        {error, _} -> empty_sample()
    end;
read_sample(#{platform := unsupported}) ->
    empty_sample().

empty_sample() -> #{cpu_time_us => undefined, rss_bytes => undefined, vsz_bytes => undefined}.

%% comm can contain spaces and parentheses: all fields after its final ')' are numeric/state.
parse_linux(Data, Ticks, PageSize) ->
    case binary:matches(Data, <<")">>) of
        [] ->
            empty_sample();
        Matches ->
            {Pos, _} = lists:last(Matches),
            Fields = string:lexemes(binary:part(Data, Pos + 1, byte_size(Data) - Pos - 1), " \n\t"),
            User = numeric_field(Fields, 12),
            System = numeric_field(Fields, 13),
            Rss = numeric_field(Fields, 22),
            (empty_sample())#{
                cpu_time_us => cpu_ticks(User, System, Ticks),
                rss_bytes => rss_pages(Rss, PageSize),
                vsz_bytes => numeric_field(Fields, 21),
                start_time => numeric_field(Fields, 20)
            }
    end.

numeric_field(Fields, Index) when length(Fields) >= Index ->
    nonnegative_integer(lists:nth(Index, Fields));
numeric_field(_Fields, _Index) ->
    undefined.

cpu_ticks(User, System, Ticks) when
    is_integer(User), is_integer(System), is_integer(Ticks), Ticks > 0
->
    (User + System) * 1000000 div Ticks;
cpu_ticks(_, _, _) ->
    undefined.

rss_pages(Rss, PageSize) when is_integer(Rss), is_integer(PageSize), PageSize > 0 -> Rss * PageSize;
rss_pages(_, _) -> undefined.

parse_macos(Data) ->
    case string:lexemes(Data, " \n\t") of
        [Time, Rss] -> macos_fields(Time, Rss, undefined);
        [Time, Rss, Vsz] -> macos_fields(Time, Rss, rss_pages(nonnegative_integer(Vsz), 1024));
        _ -> empty_sample()
    end.

macos_fields(Time, Rss, Vsz) ->
    (empty_sample())#{
        cpu_time_us => parse_cpu_time(Time),
        rss_bytes => rss_pages(nonnegative_integer(Rss), 1024),
        vsz_bytes => Vsz
    }.

parse_cpu_time(Time) ->
    case string:split(Time, ":", all) of
        [Minutes, Seconds] ->
            case {nonnegative_integer(Minutes), string:split(Seconds, ".", all)} of
                {M, [Sec, Centi]} when is_integer(M), byte_size(Centi) =:= 2 ->
                    case {nonnegative_integer(Sec), nonnegative_integer(Centi)} of
                        {S, C} when is_integer(S), S < 60, is_integer(C), C < 100 ->
                            (M * 60 + S) * 1000000 + C * 10000;
                        _ ->
                            undefined
                    end;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

nonnegative_integer(Value) ->
    try binary_to_integer(Value) of
        N when N >= 0 -> N;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

positive_integer(Value) ->
    case nonnegative_integer(Value) of
        N when is_integer(N), N > 0 -> N;
        _ -> undefined
    end.

%% Only our saved cumulative counters are used; another tool's reads cannot reset them.
-spec window(undefined | map(), map()) -> {map(), map()}.
window(Previous, Current) ->
    Same = same_identity(Previous, Current),
    {Cpu, NextCpu} = cpu_window(Previous, Current, Same),
    Rss = maps:get(rss_bytes, Current),
    Delta = rss_delta(Previous, Current, Same),
    {
        #{
            cpu => Cpu,
            rss_bytes => Rss,
            rss_delta_bytes => Delta,
            vsz_bytes => maps:get(vsz_bytes, Current, undefined)
        },
        Current#{cpu_time_us := NextCpu}
    }.

same_identity(undefined, _Current) ->
    true;
same_identity(Previous, Current) ->
    maps:get(identity, Previous) =:= maps:get(identity, Current) andalso
        same_start_time(
            maps:get(start_time, Previous, undefined),
            maps:get(start_time, Current, undefined)
        ).

%% A failed proc read has no observed start time, not evidence of a new process.
%% Missing CPU counters still force warm-up before any subsequent delta is usable.
same_start_time(undefined, _) -> true;
same_start_time(_, undefined) -> true;
same_start_time(First, Second) -> First =:= Second.

cpu_window(_Previous, #{cpu_time_us := undefined}, _Same) ->
    {#{status => unavailable}, undefined};
cpu_window(_Previous, _Current, false) ->
    {#{status => unavailable}, undefined};
cpu_window(undefined, #{cpu_time_us := Cpu}, true) ->
    {#{status => warming_up}, Cpu};
cpu_window(#{cpu_time_us := undefined}, #{cpu_time_us := Cpu}, true) ->
    {#{status => warming_up}, Cpu};
cpu_window(Previous, Current, true) ->
    Cpu = maps:get(cpu_time_us, Current),
    DeltaCpu = Cpu - maps:get(cpu_time_us, Previous),
    DeltaTime = maps:get(monotonic_us, Current) - maps:get(monotonic_us, Previous),
    case DeltaCpu >= 0 andalso DeltaTime > 0 of
        true ->
            {
                #{
                    status => available,
                    percent => 100 * DeltaCpu / DeltaTime,
                    interval_us => DeltaTime
                },
                Cpu
            };
        false ->
            {#{status => unavailable}, undefined}
    end.

rss_delta(#{rss_bytes := Before}, #{rss_bytes := After}, true) when
    is_integer(Before), is_integer(After)
->
    After - Before;
rss_delta(_Previous, _Current, _Same) ->
    undefined.

-spec format_cpu(map()) -> iolist().
format_cpu(#{cpu := #{status := available, percent := Percent}}) ->
    io_lib:format("~.1f%", [Percent]);
format_cpu(#{cpu := #{status := warming_up}}) ->
    "warming up";
format_cpu(#{cpu := #{status := unavailable}}) ->
    "unavailable".

-spec format_rss(map()) -> iolist().
format_rss(#{rss_bytes := undefined}) ->
    "unavailable";
format_rss(#{rss_bytes := Bytes, rss_delta_bytes := undefined}) ->
    bytes(Bytes);
format_rss(#{rss_bytes := Bytes, rss_delta_bytes := Delta}) ->
    Sign =
        case Delta >= 0 of
            true -> "+";
            false -> "-"
        end,
    [bytes(Bytes), " (", Sign, bytes(abs(Delta)), ")"].

-spec format_rss(map(), pos_integer()) -> iolist().
format_rss(#{rss_bytes := undefined}, Width) when Width < 11 -> "n/a";
format_rss(Metrics, Width) ->
    Full = format_rss(Metrics),
    case iolist_size(Full) =< Width of
        true -> Full;
        false -> compact_rss(Metrics)
    end.

compact_rss(#{rss_bytes := Bytes, rss_delta_bytes := undefined}) ->
    compact_bytes(Bytes);
compact_rss(#{rss_bytes := Bytes, rss_delta_bytes := Delta}) ->
    Sign =
        case Delta >= 0 of
            true -> "+";
            false -> "-"
        end,
    [compact_bytes(Bytes), " ", Sign, compact_bytes(abs(Delta))].

compact_bytes(Bytes) ->
    [Number, Unit] = string:lexemes(lists:flatten(bytes(Bytes)), " "),
    [Number, hd(Unit)].

bytes(Bytes) when Bytes < 1024 -> [integer_to_list(Bytes), " B"];
bytes(Bytes) when Bytes < 1048576 -> scaled_bytes(Bytes / 1024, "KiB");
bytes(Bytes) when Bytes < 1073741824 -> scaled_bytes(Bytes / 1048576, "MiB");
bytes(Bytes) when Bytes < 1099511627776 -> scaled_bytes(Bytes / 1073741824, "GiB");
bytes(Bytes) -> scaled_bytes(Bytes / 1099511627776, "TiB").

scaled_bytes(Value, Unit) when Value < 10 -> io_lib:format("~.1f ~s", [Value, Unit]);
scaled_bytes(Value, Unit) -> io_lib:format("~B ~s", [round(Value), Unit]).

-spec format_window(map()) -> iolist().
format_window(#{cpu := #{status := unavailable}}) ->
    "CPU window:n/a";
format_window(Metrics) ->
    ["CPU window:", window_value(Metrics)].

-spec system_info(map()) -> list().
system_info(Metrics) ->
    Vsz =
        case maps:get(vsz_bytes, Metrics, undefined) of
            undefined -> "unavailable";
            V -> bytes(V)
        end,
    [
        {beam_cpu, format_cpu(Metrics)},
        {beam_rss, {rss, Metrics}},
        {cpu_window, window_value(Metrics)},
        {beam_vsz, Vsz}
    ].

window_value(#{cpu := #{status := available, interval_us := Us}}) ->
    io_lib:format("~.2fs", [Us / 1000000]);
window_value(Metrics) ->
    format_cpu(Metrics).

%% Trap termination only during the bounded command so view cleanup also kills its OS child.
command(Path, Args, Timeout) ->
    WasTrapping = process_flag(trap_exit, true),
    try
        run_command(Path, Args, Timeout, WasTrapping)
    after
        process_flag(trap_exit, WasTrapping),
        propagate_exit(WasTrapping)
    end.

run_command(Path, Args, Timeout, WasTrapping) ->
    try
        open_port({spawn_executable, Path}, [
            binary,
            exit_status,
            use_stdio,
            stderr_to_stdout,
            hide,
            {args, Args},
            {env, [{"LC_ALL", "C"}]}
        ])
    of
        Port ->
            unlink(Port),
            Monitor = erlang:monitor(port, Port),
            Deadline = erlang:monotonic_time(millisecond) + Timeout,
            try command_output(Port, Monitor, Deadline, <<>>, WasTrapping) of
                {ok, _} = Result ->
                    Result;
                {error, _} = Result ->
                    kill_child(Port),
                    Result
            catch
                Class:Reason:Stack ->
                    kill_child(Port),
                    erlang:raise(Class, Reason, Stack)
            after
                close_port(Port),
                erlang:demonitor(Monitor, [flush]),
                flush_port(Port)
            end
    catch
        error:_ -> {error, unavailable}
    end.

command_output(Port, Monitor, Deadline, Output, WasTrapping) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, Data}} when byte_size(Output) + byte_size(Data) =< ?MAX_OUTPUT ->
            command_output(Port, Monitor, Deadline, <<Output/binary, Data/binary>>, WasTrapping);
        {Port, {data, _}} ->
            {error, output_limit};
        {Port, {exit_status, 0}} ->
            case erlang:monotonic_time(millisecond) =< Deadline of
                true -> {ok, Output};
                false -> {error, timeout}
            end;
        {Port, {exit_status, _}} ->
            {error, command_failed};
        {'DOWN', Monitor, port, Port, _} ->
            {error, command_failed};
        {'EXIT', Port, _} ->
            command_output(Port, Monitor, Deadline, Output, WasTrapping);
        {'EXIT', _From, Reason} when not WasTrapping -> exit(Reason)
    after Remaining -> {error, timeout}
    end.

%% port_close only closes pipes; it does not terminate a command that ignores EOF.
kill_child(Port) ->
    case safe_os_pid(Port) of
        {os_pid, Pid} ->
            {KillPath, KillArgs} = kill_command(Pid),
            try
                open_port({spawn_executable, KillPath}, [
                    exit_status,
                    use_stdio,
                    stderr_to_stdout,
                    hide,
                    {args, KillArgs}
                ])
            of
                Killer ->
                    unlink(Killer),
                    receive
                        {Killer, {exit_status, _}} -> ok
                    after 50 -> ok
                    end,
                    close_port(Killer),
                    flush_port(Killer)
            catch
                error:_ -> ok
            end;
        undefined ->
            ok
    end.

kill_command(Pid) ->
    case os:find_executable("kill") of
        false ->
            %% Minimal Linux images may only have the POSIX shell builtin. PID stays an argument.
            {"/bin/sh", [
                "-c", "kill -KILL \"$1\"", "observer_cli_runtime_metrics", integer_to_list(Pid)
            ]};
        Path ->
            {Path, ["-KILL", integer_to_list(Pid)]}
    end.

safe_os_pid(Port) ->
    try
        erlang:port_info(Port, os_pid)
    catch
        error:badarg -> undefined
    end.

close_port(Port) ->
    try
        erlang:port_close(Port)
    catch
        error:badarg -> ok
    end.

flush_port(Port) ->
    receive
        {Port, _} -> flush_port(Port);
        {'EXIT', Port, _} -> flush_port(Port)
    after 0 -> ok
    end.

%% A stop arriving during bounded OS-child cleanup must not be left in the redraw mailbox.
propagate_exit(true) ->
    ok;
propagate_exit(false) ->
    receive
        {'EXIT', _, normal} -> propagate_exit(false);
        {'EXIT', _, Reason} -> exit(Reason)
    after 0 -> ok
    end.
