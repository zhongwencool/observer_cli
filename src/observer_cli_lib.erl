%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

-include("observer_cli.hrl").
-compile(inline).

%% API
-export([parse_cmd/2]).
-export([uptime/0]).
-export([float_to_percent_with_two_digit/1]).
-export([to_list/1]).
-export([green/1]).
-export([to_megabyte_str/1]).
-export([mfa_to_list/1]).
-export([render/1]).
-export([next_redraw/2]).
-export([render_menu/3]).
-export([to_row/1]).

-spec uptime() -> list().
uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    [?GREEN, ?W(io_lib:format("~pDays ~p:~p:~p", [D, H, M, S]), 16), ?RESET].

%% @doc 0.98.2342 -> 98.23%, 1 -> 100.0%
-spec float_to_percent_with_two_digit(float()) -> string().
float_to_percent_with_two_digit(Float) ->
    Val = trunc(Float*10000),
    Integer = Val div 100,
    Decimal = Val - Integer * 100,
    case Integer of
        100 -> "100.0%";
        _ -> lists:flatten(io_lib:format("~2..0w.~2..0w%", [Integer, Decimal]))
    end.

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
    [select("h(HOME)"), unselect("e(ETS/SYS)"), unselect("a(APP)"), unselect("n(NET)"),
     unselect("c(CARRIER)"), unselect("db(MNESIA)"), unselect("d(DOC)")];
get_menu_title2(ets) ->
    [unselect("h(HOME)"), select("e(ETS/SYS)"), unselect("a(APP)"), unselect("n(NET)"),
     unselect("c(CARRIER)"), unselect("db(MNESIA)"), unselect("d(DOC)")];
get_menu_title2(allocator) ->
    [unselect("h(HOME)"), unselect("e(ETS/SYS)"), unselect("a(APP)"), unselect("n(NET)"),
     select("c(CARRIER)"), unselect("db(MNESIA)"), unselect("d(DOC)")];
get_menu_title2(doc) ->
    [unselect("h(HOME)"), unselect("e(ETS/SYS)"), unselect("a(APP)"), unselect("n(NET)"),
     unselect("c(CARRIER)"), unselect("db(MNESIA)"), select("d(DOC)")];
get_menu_title2(inet) ->
    [unselect("h(HOME)"), unselect("e(ETS/SYS)"), unselect("a(APP)"), select("n(NET)"),
     unselect("c(CARRIER)"), unselect("db(MNESIA)"), unselect("d(DOC)")];
get_menu_title2(mnesia) ->
    [unselect("h(HOME)"), unselect("e(ETS/SYS)"), unselect("a(APP)"), unselect("n(NET)"),
     unselect("c(CARRIER)"), select("db(MNESIA)"), unselect("d(DOC)")];
get_menu_title2(app) ->
    [unselect("h(HOME)"), unselect("e(ETS/SYS)"), select("a(APP)"), unselect("n(NET)"),
      unselect("c(CARRIER)"), unselect("db(MNESIA)"), unselect("d(DOC)")].

select(Title) -> [?RED_BG, Title, ?RESET_BG].
unselect(Title) -> [?L_GRAY_BG, Title, ?RESET_BG].

-spec green(list()) -> list().
green(String) -> "\e[32;1m" ++ String ++ "\e[0m".

-spec to_megabyte_str(pos_integer()) -> list().
to_megabyte_str(M) ->
    Val = trunc(M/(1024*1024)*1000),
    Integer = Val div 1000,
    Decimal = Val - Integer * 1000,
    lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decimal])).

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
    split_format_args(Rest, false, [F|FAcc], [A|AAcc]);
split_format_args([{extend, A, W}|Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts", ?I/binary>>,
    split_format_args(Rest, false, [F|FAcc], [A|AAcc]);
split_format_args([{extend_color, C, A, W}|Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary>>,
    split_format_args(Rest, false, [F|FAcc], [A|AAcc]);
split_format_args([{extend_color, C, A, W}|Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = << C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary, ?I/binary>>,
    split_format_args(Rest, false, [F|FAcc], [A|AAcc]);
split_format_args([F|Rest], Flag, FAcc, AAcc) ->
    split_format_args(Rest, Flag, [F|FAcc], AAcc).

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
        "h\n" ->
            erlang:exit(Pid, stop),
            observer_cli:start(ViewOpts);
        "e\n" ->
            erlang:exit(Pid, stop),
            observer_cli_system:start(ViewOpts);
        "a\n" ->
            erlang:exit(Pid, stop),
            observer_cli_application:start(ViewOpts);
        "n\n" ->
            erlang:exit(Pid, stop),
            observer_cli_inet:start(ViewOpts);
        "db\n" ->
            erlang:exit(Pid, stop),
            observer_cli_mnesia:start(ViewOpts);
        "c\n" ->
            erlang:exit(Pid, stop),
            observer_cli_allocator:start(ViewOpts);
        "d\n" ->
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

-spec to_row(undefined | integer()) -> integer().
to_row(TerminalRow) when is_integer(TerminalRow) ->
    TerminalRow;
to_row(_) ->
    {ok, TerminalRow} = io:rows(),
    TerminalRow.
