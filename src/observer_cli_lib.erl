%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

-include("observer_cli.hrl").

-compile(inline).

%% API
-export([parse_cmd/3]).
-export([uptime/0]).
-export([to_percent/1]).
-export([to_list/1]).
-export([green/1]).
-export([to_byte/1]).
-export([mfa_to_list/1]).
-export([render/1]).
-export([next_redraw/2]).
-export([flush_redraw_timer/1]).
-export([render_menu/2]).
-export([layout_width/0]).
-export([layout_extra_width/0]).
-export([weighted_widths/2]).
-export([get_terminal_rows/1]).
-export([select/1]).
-export([unselect/1]).
-export([parse_integer/1]).
-export([pad_rendered/1]).
-export([render_last_line/1]).
-export([exit_processes/1]).
-export([update_page_pos/3]).
-export([get_pos/4]).
-export([sublist/3]).
-export([sbcs_to_mbcs/2]).
-export([pipe/2]).

-ifdef(TEST).
-export([visible_length/1]).
-endif.

-spec uptime() -> list().
uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    Time = io_lib:format("~BDays ~2..0B:~2..0B:~2..0B", [D, H, M, S]),
    [?W(?GREEN, Time, 16)].

%% @doc 0.982342 -> 98.23%, 1 -> 100.0%
-spec to_percent(term()) -> string().
to_percent(Float) when is_float(Float), Float < 0.1 ->
    [$0, erlang:float_to_list(Float * 100, [{decimals, 2}]), $%];
to_percent(Float) when is_float(Float), Float < 1 ->
    [erlang:float_to_list(Float * 100, [{decimals, 2}]), $%];
to_percent(undefined) ->
    "******";
to_percent(_) ->
    "100.0%".

-spec to_list(term()) -> string() | {error, term()}.
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list(Binary) when is_binary(Binary) -> erlang:binary_to_list(Binary);
to_list(Port) when is_port(Port) -> erlang:port_to_list(Port);
to_list(Ref) when is_reference(Ref) -> erlang:ref_to_list(Ref);
to_list(Float) when is_float(Float) -> erlang:float_to_list(Float, [{decimals, 4}]);
to_list(Val) -> Val.

get_menu_title(Selection, MnesiaTitle) ->
    Options = [
        {home, "Home(H)"},
        {inet, "Network(N)"},
        {allocator, "System(S)"},
        {ets, "Ets(E)"},
        {mnesia, MnesiaTitle},
        {app, "App(A)"},
        {doc, "Doc(D)"},
        {plugin, "Plugin(P)"}
    ],
    lists:map(
        fun
            ({_Key, ""}) -> unselect("");
            ({Key, Value}) when Key =:= Selection -> select(Value) ++ "|";
            ({_Key, Value}) -> unselect(Value) ++ "|"
        end,
        Options
    ).

-spec select(string()) -> list().
select(Title) -> [?RED_BG, Title, ?RESET_BG].

-spec unselect(string()) -> list().
unselect(Title) -> [?L_GRAY_BG, Title, ?RESET_BG].

-spec green(list()) -> list().
green(String) -> "\e[32;1m" ++ String ++ "\e[0m".

-spec to_byte(pos_integer()) -> list().
%% byte
to_byte(Byte) when is_integer(Byte), Byte < 1024 ->
    [erlang:integer_to_list(Byte), $\s, $B];
%% kilobyte
to_byte(Byte) when Byte < 1024 * 1024 ->
    [erlang:float_to_list(Byte / 1024, [{decimals, 4}]), $\s, $K, $i, $B];
%% megabyte
to_byte(Byte) when Byte < 1024 * 1024 * 1024 ->
    [erlang:float_to_list(Byte / (1024 * 1024), [{decimals, 4}]), $\s, $M, $i, $B];
%% megabyte
to_byte(Byte) when is_integer(Byte) ->
    [erlang:float_to_list(Byte / (1024 * 1024 * 1024), [{decimals, 4}]), $\s, $G, $i, $B];
%% process died
to_byte(Byte) ->
    [to_list(Byte)].

-spec mfa_to_list({atom(), atom(), integer()} | term()) -> nonempty_string().
mfa_to_list({Module, Fun, Arg}) ->
    [atom_to_list(Module), ":", atom_to_list(Fun), "/", integer_to_list(Arg)];
mfa_to_list(Function) ->
    io_lib:format("~p", [Function]).

-spec render(list()) -> iolist().
render(FA) ->
    {F, A} = tidy_format_args([" \e[0m|~n" | lists:reverse(["|" | FA])], true, [], []),
    pad_rendered(io_lib:format(erlang:iolist_to_binary(F), A)).

%{erlang:iolist_to_binary(F), A}.

-spec render_menu(atom(), string()) -> iolist().
render_menu(Type, Text) ->
    MnesiaTitle =
        case ets:info(schema, owner) of
            undefined -> "";
            _ -> "Mnesia(M)"
        end,
    Title = get_menu_title(Type, MnesiaTitle),
    UpTime = uptime(),
    TitleWidth = ?COLUMN + 151 - erlang:length(UpTime) + layout_extra_width(),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

tidy_format_args([], _NeedLine, FAcc, AAcc) ->
    {FAcc, AAcc};
tidy_format_args([{width, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{width, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<"~-", WBin/binary, ".", WBin/binary, "ts", ?I/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{width_color, C, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{width_color, C, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?I2/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{width_color_2, C, A, W} | Rest], true, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts">>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([{width_color_2, C, A, W} | Rest], false, FAcc, AAcc) ->
    WBin = erlang:integer_to_binary(W),
    F = <<C/binary, "~-", WBin/binary, ".", WBin/binary, "ts", ?RESET/binary, ?I2/binary>>,
    tidy_format_args(Rest, false, [F | FAcc], [to_str(A) | AAcc]);
tidy_format_args([F | Rest], NeedLine, FAcc, AAcc) ->
    tidy_format_args(Rest, NeedLine, [F | FAcc], AAcc).

to_str({byte, Bytes}) -> to_byte(Bytes);
to_str(Term) -> to_list(Term).

-spec parse_cmd(view_opts(), atom(), list()) -> atom() | string() | tuple().
parse_cmd(ViewOpts, Module, Args) ->
    case observer_cli_command:parse_shared(to_list(io:get_line(""))) of
        home_view ->
            apply(Module, clean, [Args]),
            observer_cli:start(ViewOpts);
        system_view ->
            apply(Module, clean, [Args]),
            observer_cli_system:start(ViewOpts);
        app_view ->
            apply(Module, clean, [Args]),
            observer_cli_application:start(ViewOpts);
        inet_view ->
            apply(Module, clean, [Args]),
            observer_cli_inet:start(ViewOpts);
        mnesia_view ->
            apply(Module, clean, [Args]),
            observer_cli_mnesia:start(ViewOpts);
        ets_view ->
            apply(Module, clean, [Args]),
            observer_cli_ets:start(ViewOpts);
        help_view ->
            apply(Module, clean, [Args]),
            observer_cli_help:start(ViewOpts);
        plugin_view ->
            apply(Module, clean, [Args]),
            observer_cli_plugin:start(ViewOpts);
        Action ->
            Action
    end.

-spec next_redraw(reference() | ?INIT_TIME_REF, pos_integer()) -> reference().
next_redraw(LastTimeRef, Interval) ->
    flush_redraw_timer(LastTimeRef),
    erlang:send_after(Interval, self(), redraw).

-spec flush_redraw_timer(LastTimeRef :: reference() | ?INIT_TIME_REF) -> ok.
flush_redraw_timer(LastTimeRef) ->
    LastTimeRef =/= ?INIT_TIME_REF andalso erlang:cancel_timer(LastTimeRef),
    ok.

-spec layout_width() -> pos_integer().
layout_width() ->
    case io:columns() of
        {ok, Columns} when is_integer(Columns), Columns > ?COLUMN + 5 ->
            Columns - 1;
        {ok, Columns} when is_integer(Columns) ->
            erlang:max(?COLUMN + 5, Columns);
        _ ->
            ?COLUMN + 5
    end.

-spec layout_extra_width() -> non_neg_integer().
layout_extra_width() ->
    layout_width() - (?COLUMN + 5).

-spec weighted_widths([non_neg_integer()], [non_neg_integer()]) -> [non_neg_integer()].
weighted_widths(BaseWidths, Weights) when length(BaseWidths) =:= length(Weights) ->
    Extras = weighted_extras(layout_extra_width(), Weights),
    [Width + Extra || {Width, Extra} <- lists:zip(BaseWidths, Extras)].

weighted_extras(_Extra, []) ->
    [];
weighted_extras(Extra, Weights) ->
    case lists:sum(Weights) of
        0 ->
            [0 || _ <- Weights];
        Total ->
            Base = [(Extra * Weight) div Total || Weight <- Weights],
            add_extra_remainder(Base, Weights, Extra - lists:sum(Base))
    end.

add_extra_remainder(Widths, _Weights, 0) ->
    Widths;
add_extra_remainder([], [], _Rem) ->
    [];
add_extra_remainder([Width | Widths], [0 | Weights], Rem) ->
    [Width | add_extra_remainder(Widths, Weights, Rem)];
add_extra_remainder([Width | Widths], [_Weight | Weights], Rem) ->
    [Width + 1 | add_extra_remainder(Widths, Weights, Rem - 1)].

-spec pad_rendered(iodata()) -> list().
pad_rendered(IoData) ->
    Lines = binary:split(unicode:characters_to_binary(IoData), <<"\n">>, [global]),
    unicode:characters_to_list(iolist_to_binary(join_lines([pad_line(Line) || Line <- Lines]))).

join_lines([]) ->
    <<>>;
join_lines([Line]) ->
    Line;
join_lines([Line | Rest]) ->
    [Line, <<"\n">> | join_lines(Rest)].

pad_line(<<>>) ->
    <<>>;
pad_line(Line) ->
    NormalizedLine = trim_border_space(Line),
    case border_parts(NormalizedLine) of
        {Body, Suffix} -> pad_bordered_line(NormalizedLine, Body, Suffix);
        false -> NormalizedLine
    end.

trim_border_space(Line) ->
    Reset = ?RESET,
    case take_suffix(Line, <<Reset/binary, $\s, $|>>) of
        {Body, _} ->
            <<Body/binary, Reset/binary, $|>>;
        false ->
            Suffix = <<$|, $\s, Reset/binary>>,
            case take_suffix(Line, Suffix) of
                {Body, _} -> <<Body/binary, $|, Reset/binary>>;
                false -> Line
            end
    end.

border_parts(Line) ->
    Reset = ?RESET,
    ResetBeforeBorderSuffix = <<Reset/binary, $|>>,
    ResetSuffix = <<$|, Reset/binary>>,
    case take_suffix(Line, ResetBeforeBorderSuffix) of
        {Body, Suffix} ->
            {Body, Suffix};
        false ->
            border_parts(Line, ResetSuffix)
    end.

border_parts(Line, ResetSuffix) ->
    case take_suffix(Line, <<"|">>) of
        {Body, Suffix} ->
            {Body, Suffix};
        false ->
            take_suffix(Line, ResetSuffix)
    end.

take_suffix(Line, Suffix) when byte_size(Line) >= byte_size(Suffix) ->
    BodySize = byte_size(Line) - byte_size(Suffix),
    case Line of
        <<Body:BodySize/binary, Suffix/binary>> -> {Body, Suffix};
        _ -> false
    end;
take_suffix(_Line, _Suffix) ->
    false.

pad_bordered_line(Line, Body, Suffix) ->
    case layout_width() - visible_length(Line) of
        Padding when Padding > 0 ->
            [Body, lists:duplicate(Padding, $\s), Suffix];
        _ ->
            Line
    end.

visible_length(IoData) ->
    Bin = unicode:characters_to_binary(IoData),
    Clean0 = re:replace(Bin, <<"\e\\[[0-9;]*[A-Za-z]">>, <<>>, [global, {return, binary}]),
    Clean = binary:replace(Clean0, [<<"\n">>, <<"\r">>], <<>>, [global]),
    length(unicode:characters_to_list(Clean)).

-spec get_terminal_rows(boolean()) -> integer().
get_terminal_rows(_AutoRow = false) ->
    application:get_env(observer_cli, default_row_size, 30);
get_terminal_rows(_AutoRow = true) ->
    case io:rows() of
        {error, _} -> application:get_env(observer_cli, default_row_size, 30);
        {ok, Rows} -> Rows
    end.

-spec parse_integer(string()) -> {term(), term()}.
parse_integer(Number) ->
    observer_cli_command:parse_integer(Number).

-spec render_last_line(iodata()) -> list().
render_last_line(Text) ->
    ?render([?UNDERLINE, ?GRAY_BG, ?W(Text, layout_width() - 3)]).

-spec exit_processes(list()) -> ok.
exit_processes(List) ->
    [
        begin
            erlang:unlink(Pid),
            erlang:exit(Pid, stop)
        end
     || Pid <- List
    ],
    flush(),
    ok.

-spec update_page_pos(pid() | pos_integer(), pos_integer(), list()) -> list().
update_page_pos(StorePid, Page, Pages) when is_pid(StorePid) ->
    Pos =
        case lists:keyfind(Page, 1, Pages) of
            false ->
                Row = observer_cli_store:lookup_row(StorePid),
                (Page - 1) * Row + 1;
            {_, P} ->
                P
        end,
    update_page_pos(Page, Pos, Pages);
update_page_pos(Page, Pos, Pages) ->
    [{Page, Pos} | lists:keydelete(Page, 1, Pages)].

-spec get_pos(pos_integer(), pos_integer(), list(), pos_integer()) ->
    {pos_integer(), pos_integer()}.
get_pos(_Page, _PageRow, _Pages, 0) ->
    {1, 1};
get_pos(Page, PageRow, Pages, TopLen) ->
    Start = erlang:min((Page - 1) * PageRow + 1, TopLen),
    case lists:keyfind(Page, 1, Pages) of
        {_, P} when P >= Start andalso P =< Start + PageRow -> {Start, P};
        _ -> {Start, Start}
    end.

flush() ->
    receive
        _Msg ->
            flush()
    after 100 -> ok
    end.

-spec sublist(list(), integer(), integer()) -> {integer(), list()}.
sublist(AllEts, Rows, CurPage) ->
    SortEts = recon_lib:sublist_top_n_attrs(AllEts, Rows * CurPage),
    Start = Rows * (CurPage - 1) + 1,
    case erlang:length(SortEts) >= Start of
        true ->
            {Start, lists:sublist(SortEts, Start, Rows)};
        false ->
            {Start, []}
    end.

-spec sbcs_to_mbcs(list(), list()) -> list().
sbcs_to_mbcs(TypeList, STMList) ->
    FoldlFun = fun
        ({{Type, _}, New}, Acc) when is_number(New) ->
            case lists:member(Type, TypeList) of
                true -> maps:update_with(Type, fun(V) -> V + New end, 0, Acc);
                false -> Acc
            end;
        (_, Acc) ->
            Acc
    end,
    maps:to_list(lists:foldl(FoldlFun, #{}, STMList)).

-spec pipe(Acc, FunList :: [F]) -> Acc2 when
    Acc :: term(),
    F :: fun((X :: term()) -> X2 :: term()),
    Acc2 :: term().
pipe(Acc, FunList) ->
    lists:foldl(
        fun(Fun, Acc2) when is_function(Fun, 1) -> Fun(Acc2) end, Acc, FunList
    ).
