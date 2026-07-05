-module(observer_cli_port).

-include("observer_cli.hrl").

-export([start/2]).

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    addr_to_str/1,
    collect_port_info/1,
    render_last_line/0,
    render_port_info/1,
    render_link_monitor/2,
    render_type_line/1,
    render_stats/1,
    render_opts/1,
    render_menu/2,
    output_die_view/2,
    get_menu_title/1,
    get_menu_title2/1
]).
-endif.

-spec start(pid(), view_opts()) -> no_return().
start(Port, Opts) ->
    #view_opts{port = RefreshMs} = Opts,
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(Port, RefreshMs, ?INIT_TIME_REF)
    end),
    manager(RenderPid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(RenderPid, Opts) ->
    case parse_cmd(Opts, RenderPid) of
        quit ->
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            manager(RenderPid, Opts#view_opts{port = NewInterval});
        ViewAction ->
            erlang:send(RenderPid, ViewAction),
            manager(RenderPid, Opts)
    end.

render_worker(Port, Interval, TimeRef) ->
    case collect_port_info(Port) of
        dead ->
            output_die_view(Port, Interval),
            next_draw_view(TimeRef, Interval, Port);
        #{port := PortView, links := Link, monitors := Monitors, type := Type} ->
            Menu = render_menu(info, Interval),
            Line1 = render_port_info(PortView),
            Line2 = render_link_monitor(Link, Monitors),
            Line3 = render_type_line(Type),
            LastLine = render_last_line(),

            ?output([?CURSOR_TOP, Menu, Line1, Line2, Line3, LastLine]),
            next_draw_view(TimeRef, Interval, Port)
    end.

collect_port_info(Port) ->
    PortInfo = recon:port_info(Port),
    Meta = proplists:get_value(meta, PortInfo),
    case lists:member(undefined, Meta) of
        true ->
            dead;
        false ->
            Signals = proplists:get_value(signals, PortInfo),
            IO = proplists:get_value(io, PortInfo),
            MemoryUsed = proplists:get_value(memory_used, PortInfo),
            #{
                port => #{
                    port => Port,
                    id => proplists:get_value(id, Meta),
                    name => proplists:get_value(name, Meta),
                    os_pid => proplists:get_value(os_pid, Meta),
                    input => proplists:get_value(input, IO),
                    output => proplists:get_value(output, IO),
                    memory => proplists:get_value(memory, MemoryUsed),
                    queue_size => proplists:get_value(queue_size, MemoryUsed),
                    connected => proplists:get_value(connected, Signals)
                },
                links => proplists:get_value(links, Signals),
                monitors => proplists:get_value(monitors, Signals),
                type => proplists:get_value(type, PortInfo)
            }
    end.

next_draw_view(TimeRef, Interval, Port) ->
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    next_draw_view_2(NewTimeRef, Interval, Port).

next_draw_view_2(TimeRef, Interval, Port) ->
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(Port, NewInterval, TimeRef);
        _ ->
            ?output(?CLEAR),
            render_worker(Port, Interval, TimeRef)
    end.

render_port_info(#{
    port := Port,
    id := Id,
    name := Name,
    os_pid := OsPid,
    input := Input,
    output := Output,
    memory := Memory,
    queue_size := QueueSize,
    connected := Connected
}) ->
    QueueSizeColor =
        case QueueSize > 0 of
            true -> ?RED;
            false -> ?GREEN
        end,
    [Attr1W, Value1W, Attr2W, Value2W, Attr3W, Value3W] =
        port_info_widths([18, 20, 18, 20, 19, 21]),
    Title =
        ?render([
            ?GRAY_BG,
            ?W("Attr", Attr1W),
            ?W("Value", Value1W),
            ?W("Attr", Attr2W),
            ?W("Value", Value2W),
            ?W("Attr", Attr3W),
            ?W("Value", Value3W)
        ]),
    Rows =
        ?render([
            ?W("port", Attr1W),
            ?W(Port, Value1W),
            ?W("id", Attr2W),
            ?W(Id, Value2W),
            ?W("name", Attr3W),
            ?W(Name, Value3W),
            ?NEW_LINE,
            ?W("queue_size", Attr1W),
            ?W2(QueueSizeColor, QueueSize, Value1W + 1),
            " ",
            ?W("input", Attr2W),
            ?W({byte, Input}, Value2W),
            ?W("output", Attr3W),
            ?W({byte, Output}, Value3W),
            ?NEW_LINE,
            ?W("connected", Attr1W),
            ?W(Connected, Value1W),
            ?W("memory", Attr2W),
            ?W({byte, Memory}, Value2W),
            ?W("os_pid", Attr3W),
            ?W(OsPid, Value3W)
        ]),
    [Title, Rows].

render_link_monitor(Link, Monitors) ->
    LinkStr = [
        begin
            observer_cli_lib:to_list(P)
        end
     || P <- lists:sublist(Link, 30)
    ],
    MonitorsStr = [
        begin
            case P of
                {process, Pid} ->
                    observer_cli_lib:to_list(Pid);
                {RegName, Node} ->
                    observer_cli_lib:to_list(RegName) ++ "/" ++ observer_cli_lib:to_list(Node)
            end
        end
     || P <- lists:sublist(Monitors, 30)
    ],
    LinkInfo = "Links(" ++ erlang:integer_to_list(erlang:length(Link)) ++ ")",
    MonitorInfo = "Monitors(" ++ erlang:integer_to_list(erlang:length(Monitors)) ++ ")",
    Extra = observer_cli_lib:layout_extra_width(),
    ValueW = 110 + Extra + wide_fill(5),
    ?render([
        ?W(LinkInfo, 18),
        ?W(LinkStr, ValueW),
        ?NEW_LINE,
        ?UNDERLINE,
        ?W(MonitorInfo, 18),
        ?W(MonitorsStr, ValueW)
    ]).

render_type_line(List) ->
    PeerName =
        case lists:keyfind(peername, 1, List) of
            {_, Peer} -> addr_to_str(Peer);
            false -> "undefined"
        end,
    SockName =
        case lists:keyfind(sockname, 1, List) of
            {_, Sock} -> addr_to_str(Sock);
            false -> "undefined"
        end,
    [SockW, ArrowW, PeerW] = type_line_widths(),
    Line1 =
        ?render([
            ?UNDERLINE,
            ?W("            " ++ SockName ++ "(sockname)", SockW),
            ?W("<=============>", ArrowW),
            ?W("            " ++ PeerName ++ "(peername)", PeerW)
        ]),
    Line2 =
        case lists:keyfind(statistics, 1, List) of
            {_, Stats} -> [Line1, render_stats(Stats)];
            false -> Line1
        end,
    case lists:keyfind(options, 1, List) of
        {_, Opts} -> Line2 ++ [render_opts(Opts)];
        false -> Line2
    end.

port_info_widths(Base) ->
    fill_last(observer_cli_lib:weighted_widths(Base, [0, 1, 0, 1, 0, 3]), wide_fill(5)).

type_line_widths() ->
    fill_last(observer_cli_lib:weighted_widths([55, 15, 55], [1, 0, 1]), wide_fill(5)).

render_stats(Stats) ->
    RecvOct = proplists:get_value(recv_oct, Stats),
    RecvCnt = proplists:get_value(recv_cnt, Stats),
    RecvMax = proplists:get_value(recv_max, Stats),
    RecvAvg = proplists:get_value(recv_avg, Stats),
    RecvDvi = proplists:get_value(recv_dvi, Stats),
    SendOct = proplists:get_value(send_oct, Stats),
    SendCnt = proplists:get_value(send_cnt, Stats),
    SendMax = proplists:get_value(send_max, Stats),
    SendAvg = proplists:get_value(send_avg, Stats),
    SendPend = proplists:get_value(send_pend, Stats),
    [
        CntLabelW,
        CntValueW,
        OctLabelW,
        OctValueW,
        MaxLabelW,
        MaxValueW,
        AvgLabelW,
        AvgValueW,
        LastLabelW,
        LastValueW
    ] =
        stats_widths(),
    ?render([
        ?W("recv_cnt", CntLabelW),
        ?W(RecvCnt, CntValueW),
        ?W("recv_oct", OctLabelW),
        ?W({byte, RecvOct}, OctValueW),
        ?W("recv_max", MaxLabelW),
        ?W({byte, RecvMax}, MaxValueW),
        ?W("recv_avg", AvgLabelW),
        ?W({byte, RecvAvg}, AvgValueW),
        ?W("recv_dvi", LastLabelW),
        ?W({byte, RecvDvi}, LastValueW),
        ?NEW_LINE,
        ?W("send_cnt", CntLabelW),
        ?W(SendCnt, CntValueW),
        ?W("send_oct", OctLabelW),
        ?W({byte, SendOct}, OctValueW),
        ?W("send_max", MaxLabelW),
        ?W({byte, SendMax}, MaxValueW),
        ?W("send_avg", AvgLabelW),
        ?W({byte, SendAvg}, AvgValueW),
        ?W("send_pend", LastLabelW),
        ?W(SendPend, LastValueW)
    ]).

render_opts(Opts) ->
    Active = proplists:get_value(active, Opts),
    Broadcast = proplists:get_value(broadcast, Opts),
    Buffer = proplists:get_value(buffer, Opts),
    DelaySend = proplists:get_value(delay_send, Opts),
    DontRoute = proplists:get_value(dontroute, Opts),

    ExitOnClose = proplists:get_value(exit_on_close, Opts),
    Header = proplists:get_value(header, Opts),
    HighWatermark = proplists:get_value(high_watermark, Opts),
    KeepAlive = proplists:get_value(keepalive, Opts),
    Linger = io_lib:format("~p", [proplists:get_value(linger, Opts)]),

    LowWatermark = proplists:get_value(low_watermark, Opts),
    Mode = proplists:get_value(mode, Opts),
    NoDelay = proplists:get_value(nodelay, Opts),
    Packet = proplists:get_value(packet, Opts),
    PacketSize = proplists:get_value(packet_size, Opts),

    Priority = proplists:get_value(priority, Opts),
    RecBuf = proplists:get_value(recbuf, Opts),
    ReuseAddr = proplists:get_value(reuseaddr, Opts),
    SendTimeout = proplists:get_value(send_timeout, Opts),
    SndBuf = proplists:get_value(sndbuf, Opts),
    [Option1W, Value1W, Option2W, Value2W, Option3W, Value3W, Option4W, Value4W, Option5W, Value5W] =
        opts_widths(),
    Title =
        ?render([
            ?GRAY_BG,
            ?W("Option", Option1W),
            ?W("Value", Value1W),
            ?W("Option", Option2W),
            ?W("Value", Value2W),
            ?W("Option", Option3W),
            ?W("Value", Value3W),
            ?W("Option", Option4W),
            ?W("Value", Value4W),
            ?W("Option", Option5W),
            ?W("Value", Value5W)
        ]),
    Rows =
        ?render([
            ?W("mode", Option1W),
            ?W(Mode, Value1W),
            ?W("recbuf", Option2W),
            ?W({byte, RecBuf}, Value2W),
            ?W("sndbuf", Option3W),
            ?W({byte, SndBuf}, Value3W),
            ?W("delay_send", Option4W),
            ?W(DelaySend, Value4W),
            ?W("dontroute", Option5W),
            ?W(DontRoute, Value5W),
            ?NEW_LINE,
            ?W("reuseaddr", Option1W),
            ?W(ReuseAddr, Value1W),
            ?W("packet_size", Option2W),
            ?W({byte, PacketSize}, Value2W),
            ?W("buffer", Option3W),
            ?W({byte, Buffer}, Value3W),
            ?W("exit_on_close", Option4W),
            ?W(ExitOnClose, Value4W),
            ?W("priority", Option5W),
            ?W(Priority, Value5W),
            ?NEW_LINE,
            ?W("active", Option1W),
            ?W(Active, Value1W),
            ?W("low_watermark", Option2W),
            ?W({byte, LowWatermark}, Value2W),
            ?W("header", Option3W),
            ?W(Header, Value3W),
            ?W("keepalive", Option4W),
            ?W(KeepAlive, Value4W),
            ?W("linger", Option5W),
            ?W(Linger, Value5W),
            ?NEW_LINE,
            ?W("nodelay", Option1W),
            ?W(NoDelay, Value1W),
            ?W("high_watermark", Option2W),
            ?W({byte, HighWatermark}, Value2W),
            ?W("broadcast", Option3W),
            ?W(Broadcast, Value3W),
            ?W("send_timeout", Option4W),
            ?W(SendTimeout, Value4W),
            ?W("packet", Option5W),
            ?W(Packet, Value5W)
        ]),
    [Title, Rows].

stats_widths() ->
    fill_last(
        observer_cli_lib:weighted_widths([9, 12, 8, 12, 9, 12, 9, 12, 9, 12], [
            0, 1, 0, 1, 0, 1, 0, 1, 0, 1
        ]),
        wide_fill(5)
    ).

opts_widths() ->
    fill_last(
        observer_cli_lib:weighted_widths([9, 6, 14, 12, 9, 12, 13, 8, 9, 12], [
            0, 1, 0, 2, 0, 2, 0, 1, 0, 2
        ]),
        wide_fill(5)
    ).

wide_fill(Amount) ->
    case observer_cli_lib:layout_extra_width() of
        0 -> 0;
        _ -> Amount
    end.

fill_last([Last], Amount) ->
    [Last + Amount];
fill_last([Width | Rest], Amount) ->
    [Width | fill_last(Rest, Amount)].

render_last_line() ->
    observer_cli_lib:render_last_line("q(quit)").

render_menu(Type, Interval) ->
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Title = get_menu_title(Type),
    UpTime = observer_cli_lib:uptime(),
    TitleWidth = ?COLUMN + 41 - erlang:length(UpTime) + observer_cli_lib:layout_extra_width(),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

get_menu_title(Type) ->
    [Home, Net, Port] = get_menu_title2(Type),
    [Home, "|", Net, "|", Port].

get_menu_title2(info) ->
    [?UNSELECT("Home(H)"), ?UNSELECT("Network(N)"), ?SELECT("Port Info(P)")].

parse_cmd(ViewOpts, Pid) ->
    case parse_cmd_str(observer_cli_lib:to_list(io:get_line(""))) of
        home_view ->
            erlang:exit(Pid, stop),
            observer_cli:start(ViewOpts);
        net_view ->
            erlang:exit(Pid, stop),
            observer_cli_inet:start(ViewOpts);
        Action ->
            Action
    end.

parse_cmd_str(Key) ->
    case Key of
        "q\n" -> quit;
        "Q\n" -> quit;
        "P\n" -> info_view;
        "H\n" -> home_view;
        "N\n" -> net_view;
        Number -> observer_cli_command:parse_integer(Number)
    end.

output_die_view(Port, Interval) ->
    Menu = render_menu(info, Interval),
    Line = io_lib:format("\e[31mPort(~p) has already died.\e[0m~n", [Port]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

addr_to_str({Addr, Port}) ->
    AddrList = [
        begin
            erlang:integer_to_list(A)
        end
     || A <- erlang:tuple_to_list(Addr)
    ],
    string:join(AddrList, ".") ++ ":" ++ erlang:integer_to_list(Port).
