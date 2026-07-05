-module(observer_cli_port).

-include("observer_cli.hrl").

-export([start/2]).

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    addr_to_str/1,
    collect_port_info/1,
    collect_port_type/1,
    render_footer/0,
    render_port_sections/1,
    render_port_info/1,
    render_link_monitor/2,
    render_type_line/1,
    render_socket_peer/1,
    render_stats/1,
    render_opts/1,
    render_menu/2,
    output_die_view/2,
    get_menu_title/1
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
    handle_action(parse_cmd(), RenderPid, Opts).

handle_action(quit, RenderPid, _Opts) ->
    erlang:send(RenderPid, quit);
handle_action({new_interval, NewInterval}, RenderPid, Opts) ->
    erlang:send(RenderPid, {new_interval, NewInterval}),
    manager(RenderPid, Opts#view_opts{port = NewInterval});
handle_action(home_view, RenderPid, Opts) ->
    open_home(RenderPid, Opts);
handle_action(net_view, RenderPid, Opts) ->
    open_network(RenderPid, Opts);
handle_action(ViewAction, RenderPid, Opts) ->
    erlang:send(RenderPid, ViewAction),
    manager(RenderPid, Opts).

open_home(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli:start(Opts).

open_network(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli_inet:start(Opts).

render_worker(Port, Interval, TimeRef) ->
    case collect_port_info(Port) of
        dead ->
            output_die_view(Port, Interval),
            next_draw_view(TimeRef, Interval, Port);
        PortDetail ->
            Menu = render_menu(info, Interval),
            Lines = render_port_sections(PortDetail),
            LastLine = render_footer(),

            ?output([?CURSOR_TOP, Menu, Lines, LastLine]),
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
                type => collect_port_type(proplists:get_value(type, PortInfo))
            }
    end.

collect_port_type(Type) ->
    #{
        peername => port_type_value(peername, Type),
        sockname => port_type_value(sockname, Type),
        statistics => port_type_value(statistics, Type),
        options => port_type_value(options, Type)
    }.

port_type_value(Key, Type) ->
    case lists:keyfind(Key, 1, Type) of
        {_, Value} -> Value;
        false -> undefined
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

render_port_sections(#{port := PortView, links := Link, monitors := Monitors, type := Type}) ->
    [
        render_port_info(PortView),
        render_link_monitor(Link, Monitors),
        render_type_line(Type)
    ].

render_port_info(PortView) ->
    Fields = port_attr_value_fields(PortView),
    Widths = port_info_widths([18, 20, 18, 20, 19, 21]),
    [
        render_port_info_title(Widths),
        render_port_info_rows(Fields, Widths)
    ].

port_attr_value_fields(#{
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
    #{
        port => Port,
        id => Id,
        name => Name,
        os_pid => OsPid,
        input => Input,
        output => Output,
        memory => Memory,
        queue_size => {QueueSize, QueueSizeColor},
        connected => Connected
    }.

render_port_info_title([Attr1W, Value1W, Attr2W, Value2W, Attr3W, Value3W]) ->
    ?render([
        ?GRAY_BG,
        ?W("Attr", Attr1W),
        ?W("Value", Value1W),
        ?W("Attr", Attr2W),
        ?W("Value", Value2W),
        ?W("Attr", Attr3W),
        ?W("Value", Value3W)
    ]).

render_port_info_rows(
    #{
        port := Port,
        id := Id,
        name := Name,
        os_pid := OsPid,
        input := Input,
        output := Output,
        memory := Memory,
        queue_size := {QueueSize, QueueSizeColor},
        connected := Connected
    },
    [Attr1W, Value1W, Attr2W, Value2W, Attr3W, Value3W]
) ->
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
    ]).

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

render_type_line(TypeDetail) ->
    [
        render_socket_peer(TypeDetail),
        render_stats_section(maps:get(statistics, TypeDetail)),
        render_options_section(maps:get(options, TypeDetail))
    ].

render_socket_peer(#{peername := Peer, sockname := Sock}) ->
    PeerName = port_addr_to_str(Peer),
    SockName = port_addr_to_str(Sock),
    [SockW, ArrowW, PeerW] = type_line_widths(),
    ?render([
        ?UNDERLINE,
        ?W("            " ++ SockName ++ "(sockname)", SockW),
        ?W("<=============>", ArrowW),
        ?W("            " ++ PeerName ++ "(peername)", PeerW)
    ]).

port_addr_to_str(undefined) ->
    "undefined";
port_addr_to_str(Addr) ->
    addr_to_str(Addr).

render_stats_section(undefined) ->
    [];
render_stats_section(Stats) ->
    render_stats(Stats).

render_options_section(undefined) ->
    [];
render_options_section(Opts) ->
    render_opts(Opts).

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

render_footer() ->
    observer_cli_lib:render_footer("q(quit)").

render_menu(Type, Interval) ->
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Title = get_menu_title(Type),
    UpTime = observer_cli_lib:uptime(),
    TitleWidth =
        observer_cli_lib:layout_base_width() + 36 - erlang:length(UpTime) +
            observer_cli_lib:layout_extra_width(),
    observer_cli_lib:render_menu_header(Title, Text, TitleWidth).

get_menu_title(Type) ->
    [
        observer_cli_lib:menu_item(Type, home, "Home(H)"),
        "|",
        observer_cli_lib:menu_item(Type, network, "Network(N)"),
        "|",
        observer_cli_lib:menu_item(Type, info, "Port Info(P)")
    ].

parse_cmd() ->
    parse_cmd_str(observer_cli_lib:to_list(io:get_line(""))).

parse_cmd_str(Key) ->
    case Key of
        Cmd when Cmd =:= "q\n"; Cmd =:= "Q\n" -> quit;
        "P\n" -> info_view;
        "H\n" -> home_view;
        "N\n" -> net_view;
        {error, _Reason} -> quit;
        Number -> observer_cli_command:parse_integer(Number)
    end.

output_die_view(Port, Interval) ->
    Menu = render_menu(info, Interval),
    Line = [observer_cli_lib:ansi_red(io_lib:format("Port(~p) has already died.", [Port])), "\n"],
    LastLine = render_footer(),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

addr_to_str({Addr, Port}) ->
    AddrList = [
        begin
            erlang:integer_to_list(A)
        end
     || A <- erlang:tuple_to_list(Addr)
    ],
    string:join(AddrList, ".") ++ ":" ++ erlang:integer_to_list(Port).
