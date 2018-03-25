%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

-include("observer_cli.hrl").
-compile(inline).

%% API
-export([parse_cmd/2]).
-export([uptime/0]).
-export([to_percent/1]).
-export([to_list/1]).
-export([green/1]).
-export([to_byte/1]).
-export([mfa_to_list/1]).
-export([render/1]).
-export([next_redraw/2]).
-export([render_menu/3]).
-export([get_terminal_rows/1]).

-define(DEFAULT_ROW_SIZE, 46). %% the number from 13' mbp

-spec uptime() -> list().
uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    [?GREEN, ?W(io_lib:format("~pDays ~p:~p:~p", [D, H, M, S]), 16), ?RESET].

%% @doc 0.982342 -> 98.23%, 1 -> 100.0%
-spec to_percent(float()) -> string().
to_percent(Float)when Float < 0.1 -> io_lib:format("0~.2..f%", [Float*100]);
to_percent(Float)when Float < 1 -> io_lib:format("~.2..f%", [Float*100]);
to_percent(_) -> "100.0%".

-spec to_list(term()) -> list().
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list(Binary) when is_binary(Binary) -> erlang:binary_to_list(Binary);
to_list(Port)when is_port(Port) -> erlang:port_to_list(Port);
to_list(Ref) when is_reference(Ref) -> erlang:ref_to_list(Ref);
to_list(Val) -> Val.

parse_integer(Str, Min) ->
    case string:to_integer(Str) of
        {error, Reason} -> {error, Reason};
        {Integer, _} when Integer >= Min -> {ok, Integer};
        {_, _} -> {error, too_small}
    end.

-spec get_menu_title('allocator'|'ets'|'doc'|'home'|'inet'|'mnesia'|'app') -> list().
get_menu_title(Type) ->
    [Home, Ets, App, Inet, Alloc, Mnesia, Help] = get_menu_title2(Type),
    ["|", Home, "|", Ets, "|", App, "|", Inet, "|", Alloc, "|", Mnesia, "|", Help, "|"].

get_menu_title2(home) ->
    [select("Home(H)"), unselect("Network(N)"), unselect("System(S)"), unselect("Ets(E)"),
        unselect("Mnesia(M)"), unselect("App(A)"), unselect("Doc(D)")];
get_menu_title2(ets) ->
    [unselect("Home(H)"), unselect("Network(N)"), unselect("System(S)"), select("Ets(E)"),
        unselect("Mnesia(M)"), unselect("App(A)"), unselect("Doc(D)")];
get_menu_title2(allocator) ->
    [unselect("Home(H)"), unselect("Network(N)"), select("System(S)"), unselect("Ets(E)"),
        unselect("Mnesia(M)"), unselect("App(A)"), unselect("Doc(D)")];
get_menu_title2(doc) ->
    [unselect("Home(H)"), unselect("Network(N)"), unselect("System(S)"), unselect("Ets(E)"),
        unselect("Mnesia(M)"), unselect("App(A)"), select("Doc(D)")];
get_menu_title2(inet) ->
    [unselect("Home(H)"), select("Network(N)"), unselect("System(S)"), unselect("Ets(E)"),
         unselect("Mnesia(M)"), unselect("App(A)"), unselect("Doc(D)")];
get_menu_title2(mnesia) ->
    [unselect("Home(H)"), unselect("Network(N)"), unselect("System(S)"), unselect("Ets(E)"),
        select("Mnesia(M)"), unselect("App(A)"), unselect("Doc(D)")];
get_menu_title2(app) ->
    [unselect("Home(H)"), unselect("Network(N)"), unselect("System(S)"), unselect("Ets(E)"),
        unselect("Mnesia(M)"), select("App(A)"), unselect("Doc(D)")].

select(Title) -> [?RED_BG, Title, ?RESET_BG].
unselect(Title) -> [?L_GRAY_BG, Title, ?RESET_BG].

-spec green(list()) -> list().
green(String) -> "\e[32;1m" ++ String ++ "\e[0m".

-spec to_byte(pos_integer()) -> list().
to_byte(Byte) when Byte < 1024 -> %% byte
    io_lib:format("~w B", [Byte]);
to_byte(Byte) when Byte < 1024*1024 ->  %% kilobyte
    io_lib:format("~.4..f KB", [Byte/1024]);
to_byte(Byte) when Byte < 1024*1024*1024 -> %% megabyte
    io_lib:format("~.4..f MB", [Byte /(1024*1024)]);
to_byte(Byte) when is_integer(Byte) -> %% megabyte
    io_lib:format("~.4..f GB", [Byte /(1024*1024*1024)]);
to_byte(Byte) -> %% process died
    io_lib:format("~p", [Byte]).

-spec mfa_to_list({atom(), atom(), integer()}) -> nonempty_string().
mfa_to_list({Module, Fun, Arg}) ->
    atom_to_list(Module) ++ ":" ++
        atom_to_list(Fun) ++ "/" ++
        integer_to_list(Arg).

-spec render(list()) -> iolist().
render(FA) ->
    {F, A} = split_format_args(["|~n"|lists:reverse(["|"|FA])], true, [], []),
    io_lib:format(erlang:iolist_to_binary(F), A).

-spec render_menu(atom(), string(), integer()) -> iolist().
render_menu(Type, Text, Extend) ->
    Title = get_menu_title(Type),
    UpTime = uptime(),
    TitleWidth = ?COLUMN + Extend - erlang:length(UpTime),
    ?render([?W([Title| Text], TitleWidth)|UpTime]).

split_format_args([], _Flag, FAcc, AAcc) -> {FAcc, AAcc};
split_format_args([{extend, A, W}|Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts">>,
    split_format_args(Rest, false, [F|FAcc], [to_str(A)|AAcc]);
split_format_args([{extend, A, W}|Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts", ?I/binary>>,
    split_format_args(Rest, false, [F|FAcc], [to_str(A)|AAcc]);
split_format_args([{extend_color, C, A, W}|Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary>>,
    split_format_args(Rest, false, [F|FAcc], [to_str(A)|AAcc]);
split_format_args([{extend_color, C, A, W}|Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = << C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary, ?I/binary>>,
    split_format_args(Rest, false, [F|FAcc], [to_str(A)|AAcc]);
split_format_args([F|Rest], Flag, FAcc, AAcc) ->
    split_format_args(Rest, Flag, [F|FAcc], AAcc).

to_str({byte, Bytes}) -> to_byte(Bytes);
to_str(Int)when is_integer(Int) -> erlang:integer_to_list(Int);
to_str(Str) -> Str.

-spec parse_cmd(#view_opts{}, pid()) -> atom()|string().
parse_cmd(ViewOpts, Pid) ->
    case to_list(io:get_line("")) of
        %% inet
        "ic\n" -> inet_count;
        "iw\n" -> inet_window;
        "rc\n" -> recv_cnt;
        "ro\n" -> recv_oct;
        "sc\n" -> send_cnt;
        "so\n" -> send_oct;
        "cnt\n" -> cnt;
        "oct\n" -> oct;

        "q\n" -> quit;
        "Q\n" -> quit;
        "H\n" ->
            erlang:exit(Pid, stop),
            observer_cli:start(ViewOpts);
        "S\n" ->
            erlang:exit(Pid, stop),
            observer_cli_system:start(ViewOpts);
        "A\n" ->
            erlang:exit(Pid, stop),
            observer_cli_application:start(ViewOpts);
        "N\n" ->
            erlang:exit(Pid, stop),
            observer_cli_inet:start(ViewOpts);
        "M\n" ->
            erlang:exit(Pid, stop),
            observer_cli_mnesia:start(ViewOpts);
        "E\n" ->
            erlang:exit(Pid, stop),
            observer_cli_ets:start(ViewOpts);
        "D\n" ->
            erlang:exit(Pid, stop),
            observer_cli_help:start(ViewOpts);
        [$i | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {new_interval, NewInterval};
                {error, _} -> error_input
            end;

        %% home
        "p\n" -> pause_or_resume;
        "r\n" -> {proc_count, reductions, no_change};
        "b\n" -> {proc_count, binary_memory, no_change};
        "t\n" -> {proc_count, total_heap_size, no_change};
        "m\n" -> {proc_count, memory, no_change};
        "mq\n" -> {proc_count, message_queue_len, no_change};
        "rr\n" -> {proc_window, reductions, no_change};
        "bb\n" -> {proc_window, binary_memory, no_change};
        "tt\n" -> {proc_window, total_heap_size, no_change};
        "mm\n" -> {proc_window, memory, no_change};
        "mmq\n" -> {proc_window, message_queue_len, no_change};
        [$r, $r | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_window, reductions, NewInterval};
                {error, _} -> error_input
            end;
        [$b, $b | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_window, binary_memory, NewInterval};
                {error, _} -> error_input
            end;
        [$t, $t | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_window, total_heap_size, NewInterval};
                {error, _} -> error_input
            end;
        [$m, $m, $q | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_window, message_queue_len, NewInterval};
                {error, _} -> error_input
            end;
        [$m, $m | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_window, memory, NewInterval};
                {error, _} -> error_input
            end;
        [$r | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_count, reductions, NewInterval};
                {error, _} -> error_input
            end;
        [$b | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_count, binary_memory, NewInterval};
                {error, _} -> error_input
            end;
        [$t | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_count, total_heap_size, NewInterval};
                {error, _} -> error_input
            end;
        [$m, $q | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_count, message_queue_len, NewInterval};
                {error, _} -> error_input
            end;
        [$m | Interval] ->
            case parse_integer(Interval, ?MIN_INTERVAL) of
                {ok, NewInterval} -> {proc_count, memory, NewInterval};
                {error, _} -> error_input
            end;
        [$j | Pos] ->
            case parse_integer(Pos, 1) of
                {ok, NewPos} -> {jump_to_process, NewPos};
                {error, _} -> error_input
            end;
        "\n" -> jump_to_process;
        Input -> Input
    end.

-spec next_redraw(reference(), pos_integer()) -> reference().
next_redraw(LastTimeRef, Interval) ->
    LastTimeRef =/= undefined andalso erlang:cancel_timer(LastTimeRef),
    erlang:send_after(Interval, self(), redraw).

-spec get_terminal_rows(boolean()) -> integer().
get_terminal_rows(_AutoRow = false) -> ?DEFAULT_ROW_SIZE;
get_terminal_rows(_AutoRow = true) ->
    case io:rows() of
        {error, _} -> ?DEFAULT_ROW_SIZE;
        {ok, Rows} -> Rows
    end.
