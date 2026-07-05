%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_inet).

-include("observer_cli.hrl").

%% API
-export([start/1]).

-ifdef(TEST).
-export([
    trans_type/1,
    trans_format/4,
    get_menu_str/4,
    title/3,
    add_choose_color/3,
    collect_io_info/1,
    collect_inet_info/5,
    collect_inet_render_info/3,
    render_inet_rows/3,
    render_io_info/1,
    render_io_rows/1,
    inet_info/5,
    getstat/2,
    get_remote_ip/1,
    start_port_view/4
]).
-endif.

-define(LAST_LINE,
    "q(quit) ic(inet_count) iw(inet_window) rc(recv_cnt) ro(recv_oct)"
    " sc(send_cnt) so(send_oct) cnt oct 9(port 9 info) pd/pu(page:down/up)"
).

-spec start(view_opts()) -> no_return().
start(#view_opts{inet = InetOpt, auto_row = AutoRow} = ViewOpts) ->
    StorePid = observer_cli_store:start(),
    RenderPid = spawn_link(
        fun() ->
            ?output(?CLEAR),
            {{input, In}, {output, Out}} = erlang:statistics(io),
            render_worker(StorePid, InetOpt, ?INIT_TIME_REF, 0, {In, Out}, AutoRow)
        end
    ),
    manager(StorePid, RenderPid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(StorePid, RenderPid, ViewOpts = #view_opts{inet = InetOpts}) ->
    #inet{cur_page = CurPage, pages = Pages} = InetOpts,
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [RenderPid, StorePid]) of
        quit ->
            observer_cli_lib:exit_processes([StorePid]),
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            NewInet = InetOpts#inet{interval = NewInterval},
            manager(StorePid, RenderPid, ViewOpts#view_opts{inet = NewInet});
        Func when Func =:= inet_count; Func =:= inet_window ->
            restart(StorePid, RenderPid, ViewOpts#view_opts{inet = InetOpts#inet{func = Func}});
        Type when
            Type =:= recv_cnt;
            Type =:= recv_oct;
            Type =:= send_cnt;
            Type =:= send_oct;
            Type =:= cnt;
            Type =:= oct
        ->
            restart(StorePid, RenderPid, ViewOpts#view_opts{inet = InetOpts#inet{type = Type}});
        {jump, NewPos} ->
            NewPages = observer_cli_lib:update_page_pos(CurPage, NewPos, Pages),
            NewInetOpts = InetOpts#inet{pages = NewPages},
            start_port_view(StorePid, RenderPid, ViewOpts#view_opts{inet = NewInetOpts}, false);
        jump ->
            start_port_view(StorePid, RenderPid, ViewOpts, true);
        page_down_top_n ->
            restart_page(StorePid, RenderPid, ViewOpts, CurPage, 1);
        page_up_top_n ->
            restart_page(StorePid, RenderPid, ViewOpts, CurPage, -1);
        _ ->
            manager(StorePid, RenderPid, ViewOpts)
    end.

restart_page(
    StorePid,
    RenderPid,
    ViewOpts = #view_opts{inet = InetOpts = #inet{pages = Pages}},
    CurPage,
    Delta
) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    NewPages = observer_cli_lib:update_page_pos(StorePid, NewPage, Pages),
    restart(StorePid, RenderPid, ViewOpts#view_opts{
        inet = InetOpts#inet{
            cur_page = NewPage, pages = NewPages
        }
    }).

restart(StorePid, RenderPid, ViewOpts) ->
    observer_cli_lib:exit_processes([StorePid, RenderPid]),
    start(ViewOpts).

render_worker(StorePid, InetOpt, LastTimeRef, Count, LastIO, AutoRow) ->
    #inet{func = Function, type = Type, interval = Interval, cur_page = CurPage} = InetOpt,
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Row = erlang:max(TerminalRow - 5, 0),
    Text = get_menu_str(Function, Type, Interval, Row),
    Menu = observer_cli_lib:render_top_menu(inet, Text),
    TopLen = Row * CurPage,
    InetList = collect_inet_info(Function, Type, TopLen, Interval, Count),
    {IOInfo, NewIO} = collect_io_info(LastIO),
    IORows = render_io_info(IOInfo),
    InetInfo = collect_inet_render_info(InetList, TopLen, InetOpt),
    {PortList, InetRows} = render_inet_rows(InetInfo, TopLen, InetOpt),
    LastLine = observer_cli_lib:render_footer(?LAST_LINE),
    ?output([?CURSOR_TOP, Menu, IORows, InetRows, LastLine]),
    observer_cli_store:update(StorePid, Row, PortList),
    NewInterval =
        case Function of
            inet_count -> Interval;
            inet_window -> 10
        end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, NewInterval),
    receive
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(
                StorePid,
                InetOpt#inet{interval = NewInterval},
                TimeRef,
                Count + 1,
                NewIO,
                AutoRow
            );
        quit ->
            quit;
        _ ->
            render_worker(StorePid, InetOpt, TimeRef, Count + 1, NewIO, AutoRow)
    end.

collect_io_info({LastIn, LastOut}) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {
        #{
            input_delta => In - LastIn,
            output_delta => Out - LastOut,
            total_input => In,
            total_output => Out
        },
        {In, Out}
    }.

render_io_info(#{
    input_delta := InputDelta,
    output_delta := OutputDelta,
    total_input := In,
    total_output := Out
}) ->
    [
        ByteInputW,
        InputDeltaW,
        ByteOutputW,
        OutputDeltaW,
        TotalInputW,
        TotalInW,
        TotalOutputW,
        TotalOutW
    ] = io_widths(),
    ?render([
        ?YELLOW,
        ?W("Byte Input", ByteInputW),
        ?W({byte, InputDelta}, InputDeltaW),
        ?W("Byte Output", ByteOutputW),
        ?W({byte, OutputDelta}, OutputDeltaW),
        ?W("Total Input", TotalInputW),
        ?W({byte, In}, TotalInW),
        ?W("Total Output", TotalOutputW),
        ?W({byte, Out}, TotalOutW)
    ]).

-ifdef(TEST).
render_io_rows(LastIO) ->
    {IOInfo, NewIO} = collect_io_info(LastIO),
    {render_io_info(IOInfo), NewIO}.
-endif.

collect_inet_render_info([], _Num, _InetOpt) ->
    [];
collect_inet_render_info(InetList, Num, #inet{
    type = Type,
    pages = Pages,
    cur_page = Page
}) ->
    {Start, ChoosePos} = observer_cli_lib:get_pos(Page, Num, Pages, erlang:length(InetList)),
    Rows = lists:sublist(InetList, Start, Num),
    collect_inet_rows(Type, Rows, Start, ChoosePos).

collect_inet_rows(Type, Rows, Start, ChoosePos) ->
    {_, Collected} =
        lists:foldl(
            fun(Item, {Pos, Acc}) ->
                {Pos + 1, [collect_inet_row(Type, Item, Pos, ChoosePos) | Acc]}
            end,
            {Start, []},
            Rows
        ),
    lists:reverse(Collected).

collect_inet_row(Type, Item, Pos, ChoosePos) when Type =:= cnt orelse Type =:= oct ->
    {Port, Value, [{_, Type1Value}, {_, Type2Value}]} = Item,
    collect_inet_row(Port, Value, Type1Value, Type2Value, Pos, ChoosePos);
collect_inet_row(Type, {Port, Value, _}, Pos, ChoosePos) ->
    {_, Type1, _} = trans_type(Type),
    Packet1 = getstat(Port, erlang:list_to_existing_atom(Type1)),
    AllPacket =
        case is_integer(Packet1) of
            true -> Value + Packet1;
            false -> Value
        end,
    collect_inet_row(Port, Value, Packet1, AllPacket, Pos, ChoosePos).

collect_inet_row(Port, Value, Type1Value, Type2Value, Pos, ChoosePos) ->
    {memory_used, MemoryUsed} = recon:port_info(Port, memory_used),
    {io, IO} = recon:port_info(Port, io),
    #{
        pos => Pos,
        choose_pos => ChoosePos,
        port => Port,
        value => Value,
        type1 => Type1Value,
        type2 => Type2Value,
        input => proplists:get_value(input, IO),
        output => proplists:get_value(output, IO),
        queue_size => proplists:get_value(queue_size, MemoryUsed),
        memory => proplists:get_value(memory, MemoryUsed),
        peer => get_remote_ip(Port)
    }.

render_inet_rows([], Rows, #inet{func = inet_count, type = Type}) ->
    {[], io_lib:format("\e[32;1mGet nothing for recon:inet_count(~p, ~p)\e[0m~n", [Type, Rows])};
render_inet_rows([], Rows, #inet{func = inet_window, type = Type, interval = Interval}) ->
    {
        [],
        io_lib:format("\e[32;1mGet nothing for recon:inet_window(~p, ~p, ~p)\e[0m~n", [
            Type,
            Rows,
            Interval
        ])
    };
render_inet_rows(InetList, _Num, #inet{type = Type}) when Type =:= cnt orelse Type =:= oct ->
    {Unit, RecvType, SendType} = trans_type(Type),
    Title = title(Type, RecvType, SendType),
    [NoW, PortW, ValueW, RecvW, SendW, OutputW, InputW, QueueW, MemoryW, PeerW] =
        inet_widths(),
    FormatFunc = fun(Item, {Acc, Acc1}) ->
        #{
            pos := Pos,
            choose_pos := ChoosePos,
            port := Port,
            value := Value,
            type1 := Recv,
            type2 := Send,
            input := Input,
            output := Output,
            queue_size := QueueSize,
            memory := Memory,
            peer := IP
        } = Item,
        {ValueFormat, RecvFormat, SendFormat} = trans_format(
            Unit, Value, Recv, Send, [ValueW, RecvW, SendW]
        ),
        R = [
            ?W(Pos, NoW),
            ?W(Port, PortW),
            ValueFormat,
            RecvFormat,
            SendFormat,
            ?W({byte, Output}, OutputW),
            ?W({byte, Input}, InputW),
            ?W(QueueSize, QueueW),
            ?W({byte, Memory}, MemoryW),
            ?W(IP, PeerW)
        ],
        Rows = add_choose_color(ChoosePos, Pos, R),
        {[?render(Rows) | Acc], [{Pos, Port} | Acc1]}
    end,
    {Rows, PortList} = lists:foldl(
        FormatFunc,
        {[], []},
        InetList
    ),
    {PortList, [Title | lists:reverse(Rows)]};
render_inet_rows(InetList, _Num, #inet{type = Type}) ->
    {Unit, Type1, Type2} = trans_type(Type),
    Title = title(Type, Type1, Type2),
    [NoW, PortW, ValueW, Type1W, Type2W, OutputW, InputW, QueueW, MemoryW, PeerW] =
        inet_widths(),
    FormatFunc = fun(Item, {Acc, Acc1}) ->
        #{
            pos := Pos,
            choose_pos := ChoosePos,
            port := Port,
            value := Value,
            type1 := Packet1,
            type2 := AllPacket,
            input := Input,
            output := Output,
            queue_size := QueueSize,
            memory := Memory,
            peer := IP
        } = Item,
        {ValueFormat, Packet1Format, AllFormat} = trans_format(
            Unit, Value, Packet1, AllPacket, [ValueW, Type1W, Type2W]
        ),
        R = [
            ?W(Pos, NoW),
            ?W(Port, PortW),
            ValueFormat,
            Packet1Format,
            AllFormat,
            ?W({byte, Input}, OutputW),
            ?W({byte, Output}, InputW),
            ?W(QueueSize, QueueW),
            ?W({byte, Memory}, MemoryW),
            ?W(IP, PeerW)
        ],
        Rows = add_choose_color(ChoosePos, Pos, R),
        {[?render(Rows) | Acc], [{Pos, Port} | Acc1]}
    end,
    {Rows, PortList} = lists:foldl(
        FormatFunc,
        {[], []},
        InetList
    ),
    {PortList, [Title | lists:reverse(Rows)]}.

get_menu_str(inet_count, Type, Interval, Rows) ->
    io_lib:format("recon:inet_count(~p, ~w) Interval:~wms", [Type, Rows, Interval]);
get_menu_str(inet_window, Type, Interval, Rows) ->
    io_lib:format("recon:inet_window(~p, ~w, ~w) Interval:~wms", [Type, Rows, Interval, Interval]).

collect_inet_info(Function, Type, Num, Ms, Count) ->
    inet_info(Function, Type, Num, Ms, Count).

inet_info(inet_count, Type, Num, _, _) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, _, 0) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, Ms, _) -> recon:inet_window(Type, Num, Ms).

getstat(Port, Attr) ->
    case inet:getstat(Port, [Attr]) of
        {ok, [{_, Value}]} -> Value;
        {error, Err} -> inet:format_error(Err)
    end.

start_port_view(StorePid, RenderPid, Opts = #view_opts{inet = InetOpt}, AutoJump) ->
    #inet{cur_page = CurPage, pages = Pages} = InetOpt,
    {_, CurPos} = lists:keyfind(CurPage, 1, Pages),
    case observer_cli_store:lookup_pos(StorePid, CurPos) of
        {CurPos, ChoosePort} ->
            observer_cli_lib:exit_processes([StorePid, RenderPid]),
            observer_cli_port:start(ChoosePort, Opts);
        {_, ChoosePort} when AutoJump ->
            observer_cli_lib:exit_processes([StorePid, RenderPid]),
            observer_cli_port:start(ChoosePort, Opts);
        _ ->
            manager(StorePid, RenderPid, Opts)
    end.

trans_type(cnt) ->
    {number, "recv_cnt", "send_cnt"};
trans_type(oct) ->
    {byte, "recv_oct", "send_oct"};
trans_type(send_cnt) ->
    {number, "recv_cnt", "cnt"};
trans_type(recv_cnt) ->
    {number, "send_cnt", "cnt"};
trans_type(send_oct) ->
    {byte, "recv_oct", "oct"};
trans_type(recv_oct) ->
    {byte, "send_oct", "oct"}.

-ifdef(TEST).
trans_format(byte, Val, Val1, Val2) ->
    {
        ?W({byte, Val}, 10),
        ?W({byte, Val1}, 10),
        ?W({byte, Val2}, 10)
    };
trans_format(number, Val, Val1, Val2) ->
    {
        ?W(Val, 10),
        ?W(Val1, 10),
        ?W(Val2, 10)
    }.
-endif.

trans_format(byte, Val, Val1, Val2, [ValW, Val1W, Val2W]) ->
    {
        ?W({byte, Val}, ValW),
        ?W({byte, Val1}, Val1W),
        ?W({byte, Val2}, Val2W)
    };
trans_format(number, Val, Val1, Val2, [ValW, Val1W, Val2W]) ->
    {
        ?W(Val, ValW),
        ?W(Val1, Val1W),
        ?W(Val2, Val2W)
    }.

title(Type, Type1, Type2) ->
    [NoW, PortW, TypeW, Type1W, Type2W, OutputW, InputW, QueueW, MemoryW, PeerW] =
        inet_widths(),
    ?render([
        ?UNDERLINE,
        ?GRAY_BG,
        ?W("NO", NoW),
        ?W("Port", PortW),
        ?W(erlang:atom_to_list(Type), TypeW),
        ?W(Type1, Type1W),
        ?W(Type2, Type2W),
        ?W("output", OutputW),
        ?W("input", InputW),
        ?W("queuesize", QueueW),
        ?W("memory", MemoryW),
        ?W("Peername(ip:port)", PeerW)
    ]).

io_widths() ->
    observer_cli_lib:weighted_widths(
        [14, 12, 13, 12, 15, 17, 15, 17],
        [0, 1, 0, 1, 0, 1, 0, 1]
    ).

inet_widths() ->
    observer_cli_lib:weighted_widths(
        [2, 16, 10, 10, 10, 12, 12, 6, 12, 19],
        [0, 3, 3, 3, 3, 1, 1, 0, 1, 4]
    ).

get_remote_ip(P) ->
    case inet:peername(P) of
        {ok, {Addr, Port}} ->
            AddrList = [
                begin
                    erlang:integer_to_list(A)
                end
             || A <- erlang:tuple_to_list(Addr)
            ],
            string:join(AddrList, ".") ++ ":" ++ erlang:integer_to_list(Port);
        {error, Err} ->
            inet:format_error(Err)
    end.

add_choose_color(ChoosePos, ChoosePos, R) -> [?CHOOSE_BG | R];
add_choose_color(_ChoosePos, _CurPos, R) -> R.
