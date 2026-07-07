-module(observer_cli_port).

-include("observer_cli.hrl").

-export([start/1, start/2]).

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    addr_to_str/1,
    collect_ports_info/1,
    collect_ports_render_info/3,
    collect_port_info/1,
    collect_port_type/1,
    port_info_value/2,
    render_ports_info/2,
    render_footer/0,
    render_port_sections/1,
    render_port_info/1,
    render_link_monitor/2,
    render_link_monitor/3,
    render_type_line/1,
    render_socket_peer/1,
    render_stats/1,
    render_opts/1,
    select_port/2,
    sock_opts/0,
    render_menu/2,
    output_die_view/2,
    get_menu_title/1
]).
-endif.

-define(PORTS_LAST_LINE,
    "q(quit) qs(sort by queue_size) m(sort by memory) 9(port 9 info) "
    "pd/pu(page:down/up) F/B(forward/back) Current page is ~w"
).

-spec start(view_opts()) -> no_return().
start(#view_opts{ports = Ports, auto_row = AutoRow} = ViewOpts) ->
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_ports_worker(Ports, AutoRow)
    end),
    ports_manager(RenderPid, ViewOpts).

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
ports_manager(RenderPid, #view_opts{ports = Ports = #ports{cur_page = CurPage}} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [RenderPid]) of
        quit ->
            erlang:send(RenderPid, quit);
        {new_interval, NewInterval} ->
            restart_ports(RenderPid, ViewOpts#view_opts{
                ports = Ports#ports{interval = NewInterval}
            });
        queue_size ->
            restart_ports(RenderPid, ViewOpts#view_opts{
                ports = Ports#ports{attr = queue_size}
            });
        {func, proc_count, memory} ->
            restart_ports(RenderPid, ViewOpts#view_opts{
                ports = Ports#ports{attr = memory}
            });
        {jump, Pos} ->
            open_port_from_list(RenderPid, ViewOpts, Pos);
        page_down_top_n ->
            restart_ports_page(RenderPid, ViewOpts, CurPage, 1);
        page_up_top_n ->
            restart_ports_page(RenderPid, ViewOpts, CurPage, -1);
        _ ->
            ports_manager(RenderPid, ViewOpts)
    end.

restart_ports_page(RenderPid, ViewOpts = #view_opts{ports = Ports}, CurPage, Delta) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    restart_ports(RenderPid, ViewOpts#view_opts{ports = Ports#ports{cur_page = NewPage}}).

restart_ports(RenderPid, ViewOpts) ->
    observer_cli_lib:exit_processes([RenderPid]),
    start(ViewOpts).

open_port_from_list(RenderPid, ViewOpts = #view_opts{ports = Ports}, Pos) ->
    case select_port(Pos, Ports) of
        {ok, Port} ->
            observer_cli_lib:exit_processes([RenderPid]),
            start(Port, ViewOpts);
        error ->
            ports_manager(RenderPid, ViewOpts)
    end.

render_ports_worker(
    Ports = #ports{interval = Interval, attr = Attr, cur_page = CurPage},
    AutoRow
) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 4, 0),
    Text = io_lib:format("sort:~p Interval:~wms", [Attr, Interval]),
    Menu = observer_cli_lib:render_top_menu(ports, Text),
    PortInfo = collect_ports_render_info(Rows, CurPage, Attr),
    PortRows = render_ports_info(PortInfo, Attr),
    LastLine = observer_cli_lib:render_footer(io_lib:format(?PORTS_LAST_LINE, [CurPage])),
    ?output([?CURSOR_TOP, Menu, PortRows, LastLine]),
    erlang:send_after(Interval, self(), redraw),
    receive
        quit -> quit;
        redraw -> render_ports_worker(Ports, AutoRow)
    end.

select_port(Pos, #ports{attr = Attr}) when is_integer(Pos), Pos > 0 ->
    {_, SortPorts} = collect_ports_render_info(Pos, 1, Attr),
    case erlang:length(SortPorts) >= Pos of
        true ->
            {_, _, #{port := Port}} = lists:nth(Pos, SortPorts),
            {ok, Port};
        false ->
            error
    end;
select_port(_Pos, _Ports) ->
    error.

collect_ports_render_info(Rows, CurPage, Attr) ->
    observer_cli_lib:sublist(collect_ports_info(Attr), Rows, CurPage).

collect_ports_info(Attr) ->
    [Info || Port <- erlang:ports(), Info <- [port_overview(Port, Attr)], keep_port(Info)].

keep_port(dead) ->
    false;
keep_port({_, _, #{controls := Controls, name := Name}}) ->
    not (is_inet_port_name(Controls) orelse is_inet_port_name(Name)).

is_inet_port_name(Name) ->
    lists:member(Name, ["tcp_inet", "udp_inet", "sctp_inet"]).

port_overview(Port, Attr) ->
    case erlang:port_info(Port) of
        undefined ->
            dead;
        Info ->
            PortInfo = #{
                port => Port,
                id => proplists:get_value(id, Info),
                connected => proplists:get_value(connected, Info),
                name => proplists:get_value(name, Info),
                controls => port_controls(Port, Info),
                slot => port_info_value(Port, slot),
                parallelism => port_info_value(Port, parallelism),
                locking => port_info_value(Port, locking),
                queue_size => port_info_value(Port, queue_size, 0),
                memory => port_info_value(Port, memory, 0),
                monitors => port_info_value(Port, monitors, []),
                monitored_by => port_info_value(Port, monitored_by, [])
            },
            {0, sort_value(Attr, PortInfo), PortInfo}
    end.

sort_value(Attr, PortInfo) ->
    case maps:get(Attr, PortInfo, 0) of
        Value when is_integer(Value) -> Value;
        _ -> 0
    end.

port_controls(Port, Info) ->
    case proplists:get_value(controls, Info, port_info_value(Port, controls)) of
        undefined -> proplists:get_value(name, Info);
        Controls -> Controls
    end.

port_info_value(Port, Key) ->
    port_info_value(Port, Key, undefined).

port_info_value(Port, Key, Default) ->
    try erlang:port_info(Port, Key) of
        {Key, Value} -> Value;
        undefined -> Default
    catch
        error:badarg -> Default
    end.

render_ports_info({StartPos, SortPorts}, Attr) ->
    {QueueColor, MemoryColor} =
        case Attr of
            memory -> {?GRAY_BG, ?RED_BG};
            _ -> {?RED_BG, ?GRAY_BG}
        end,
    [
        NoW,
        IdW,
        ConnectedW,
        NameW,
        ControlsW,
        SlotW,
        QueueW,
        MemoryW,
        ParallelW,
        LockingW,
        MonitorsW
    ] = ports_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "NO", NoW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Id", IdW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Connected", ConnectedW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Name", NameW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Controls", ControlsW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Slot", SlotW),
        ?UNDERLINE,
        ?W2(QueueColor, "Queue", QueueW),
        ?UNDERLINE,
        ?W2(MemoryColor, "Memory", MemoryW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Parallel", ParallelW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Locking", LockingW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Mon/By", MonitorsW)
    ]),
    {_, Rows} = lists:foldl(
        fun({_, _, PortInfo}, {Pos, Acc}) ->
            #{
                id := Id,
                connected := Connected,
                name := Name,
                controls := Controls,
                slot := Slot,
                parallelism := Parallelism,
                locking := Locking,
                queue_size := QueueSize,
                memory := Memory,
                monitors := Monitors,
                monitored_by := MonitoredBy
            } = PortInfo,
            MonitorsText = monitor_count(Monitors) ++ "/" ++ monitor_count(MonitoredBy),
            {Pos + 1, [
                ?render([
                    ?W2(?RESET, Pos, NoW),
                    ?W2(?RESET, Id, IdW),
                    ?W2(?RESET, Connected, ConnectedW),
                    ?W2(?RESET, Name, NameW),
                    ?W2(?RESET, Controls, ControlsW),
                    ?W2(?RESET, Slot, SlotW),
                    ?W2(?RESET, QueueSize, QueueW),
                    ?W2(?RESET, {byte, Memory}, MemoryW),
                    ?W2(?RESET, Parallelism, ParallelW),
                    ?W2(?RESET, Locking, LockingW),
                    ?W2(?RESET, MonitorsText, MonitorsW)
                ])
                | Acc
            ]}
        end,
        {StartPos, []},
        SortPorts
    ),
    [Title | lists:reverse(Rows)].

ports_widths() ->
    observer_cli_lib:weighted_widths(
        [3, 6, 13, 22, 22, 5, 7, 12, 8, 11, 6],
        [0, 0, 0, 4, 4, 0, 0, 1, 0, 0, 0]
    ).

monitor_count(Values) when is_list(Values) ->
    integer_to_list(erlang:length(Values));
monitor_count(_Values) ->
    "0".

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
handle_action(ports_view, RenderPid, Opts) ->
    open_ports(RenderPid, Opts);
handle_action(ViewAction, RenderPid, Opts) ->
    erlang:send(RenderPid, ViewAction),
    manager(RenderPid, Opts).

open_home(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli:start(Opts).

open_network(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli_inet:start(Opts).

open_ports(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    start(Opts).

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
                    controls => port_controls(Port, [{name, proplists:get_value(name, Meta)}]),
                    slot => port_info_value(Port, slot),
                    parallelism => port_info_value(Port, parallelism),
                    locking => port_info_value(Port, locking),
                    os_pid => proplists:get_value(os_pid, Meta),
                    input => proplists:get_value(input, IO),
                    output => proplists:get_value(output, IO),
                    memory => proplists:get_value(memory, MemoryUsed),
                    queue_size => proplists:get_value(queue_size, MemoryUsed),
                    connected => proplists:get_value(connected, Signals)
                },
                links => proplists:get_value(links, Signals),
                monitors => proplists:get_value(monitors, Signals),
                monitored_by => port_info_value(Port, monitored_by, []),
                type => collect_port_type(Port, proplists:get_value(type, PortInfo))
            }
    end.

-ifdef(TEST).
collect_port_type(Type) ->
    collect_port_type(undefined, Type).
-endif.

collect_port_type(Port, Type) ->
    Options0 = port_type_value(options, Type),
    Options =
        case {Port, Options0} of
            {undefined, _} -> Options0;
            {_, undefined} -> undefined;
            _ -> collect_sock_opts(Port)
        end,
    #{
        peername => port_type_value(peername, Type),
        sockname => port_type_value(sockname, Type),
        statistics => port_type_value(statistics, Type),
        options => Options
    }.

port_type_value(Key, Type) ->
    case lists:keyfind(Key, 1, Type) of
        {_, Value} -> Value;
        false -> undefined
    end.

sock_opts() ->
    [
        active,
        broadcast,
        buffer,
        bind_to_device,
        delay_send,
        deliver,
        dontroute,
        exit_on_close,
        header,
        high_msgq_watermark,
        high_watermark,
        ipv6_v6only,
        keepalive,
        linger,
        low_msgq_watermark,
        low_watermark,
        mode,
        netns,
        nodelay,
        packet,
        packet_size,
        priority,
        read_packets,
        recbuf,
        reuseaddr,
        send_timeout,
        send_timeout_close,
        show_econnreset,
        sndbuf,
        tos,
        tclass
    ].

collect_sock_opts(Port) ->
    collect_sock_opts(Port, sock_opts(), []).

collect_sock_opts(_Port, [], Acc) ->
    lists:reverse(Acc);
collect_sock_opts(Port, [Opt | Opts], Acc) ->
    Value =
        try inet:getopts(Port, [Opt]) of
            {ok, [Res]} -> Res;
            {ok, []} -> {Opt, "-"};
            {error, einval} -> {Opt, "Not Supported"};
            {error, Reason} -> {Opt, io_lib:format("error:~p", [Reason])}
        catch
            _:_ -> {Opt, "Not Supported"}
        end,
    collect_sock_opts(Port, Opts, [Value | Acc]).

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

render_port_sections(#{
    port := PortView,
    links := Link,
    monitors := Monitors,
    monitored_by := MonitoredBy,
    type := Type
}) ->
    [
        render_port_info(PortView),
        render_link_monitor(Link, Monitors, MonitoredBy),
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
    controls := Controls,
    slot := Slot,
    parallelism := Parallelism,
    locking := Locking,
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
        controls => Controls,
        slot => Slot,
        parallelism => Parallelism,
        locking => Locking,
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
        controls := Controls,
        slot := Slot,
        parallelism := Parallelism,
        locking := Locking,
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
        ?W(OsPid, Value3W),
        ?NEW_LINE,
        ?W("slot", Attr1W),
        ?W(Slot, Value1W),
        ?W("controls", Attr2W),
        ?W(Controls, Value2W),
        ?W("parallelism", Attr3W),
        ?W(Parallelism, Value3W),
        ?NEW_LINE,
        ?W("locking", Attr1W),
        ?W(Locking, Value1W),
        ?W("", Attr2W),
        ?W("", Value2W),
        ?W("", Attr3W),
        ?W("", Value3W)
    ]).

-ifdef(TEST).
render_link_monitor(Link, Monitors) ->
    render_link_monitor(Link, Monitors, []).
-endif.

render_link_monitor(Link, Monitors, MonitoredBy) ->
    LinkStr = [
        begin
            observer_cli_lib:to_list(P)
        end
     || P <- lists:sublist(Link, 30)
    ],
    MonitorsStr = [monitor_to_list(P) || P <- lists:sublist(Monitors, 30)],
    MonitoredByStr = [monitor_to_list(P) || P <- lists:sublist(MonitoredBy, 30)],
    LinkInfo = "Links(" ++ erlang:integer_to_list(erlang:length(Link)) ++ ")",
    MonitorInfo = "Monitors(" ++ erlang:integer_to_list(erlang:length(Monitors)) ++ ")",
    MonitoredByInfo = "Monitored by(" ++ erlang:integer_to_list(erlang:length(MonitoredBy)) ++ ")",
    Extra = observer_cli_lib:layout_extra_width(),
    ValueW = 110 + Extra + wide_fill(5),
    ?render([
        ?W(LinkInfo, 18),
        ?W(LinkStr, ValueW),
        ?NEW_LINE,
        ?UNDERLINE,
        ?W(MonitorInfo, 18),
        ?W(MonitorsStr, ValueW),
        ?NEW_LINE,
        ?UNDERLINE,
        ?W(MonitoredByInfo, 18),
        ?W(MonitoredByStr, ValueW)
    ]).

monitor_to_list({process, Pid}) ->
    observer_cli_lib:to_list(Pid);
monitor_to_list({RegName, Node}) when is_atom(RegName), is_atom(Node) ->
    observer_cli_lib:to_list(RegName) ++ "/" ++ observer_cli_lib:to_list(Node);
monitor_to_list(Pid) when is_pid(Pid) ->
    observer_cli_lib:to_list(Pid);
monitor_to_list(Other) ->
    observer_cli_lib:to_list(Other).

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
    [Option1W, Value1W, Option2W, Value2W, Option3W, Value3W, Option4W, Value4W] =
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
            ?W("Value", Value4W)
        ]),
    Rows = render_option_rows(Opts, opts_widths()),
    [Title, Rows].

render_option_rows([], _Widths) ->
    [];
render_option_rows(Opts, Widths) ->
    Rows = [lists:sublist(Opts, Pos, 4) || Pos <- lists:seq(1, erlang:length(Opts), 4)],
    ?render(lists:append(lists:join(?NEW_LINE, [render_option_row(Row, Widths) || Row <- Rows]))).

render_option_row(Opts, Widths) ->
    Pairs = Opts ++ lists:duplicate(4 - erlang:length(Opts), {"", ""}),
    lists:append([
        render_option_pair(Pair, OptionW, ValueW)
     || {Pair, OptionW, ValueW} <- option_columns(Pairs, Widths)
    ]).

option_columns(
    [Pair1, Pair2, Pair3, Pair4],
    [Option1W, Value1W, Option2W, Value2W, Option3W, Value3W, Option4W, Value4W]
) ->
    [
        {Pair1, Option1W, Value1W},
        {Pair2, Option2W, Value2W},
        {Pair3, Option3W, Value3W},
        {Pair4, Option4W, Value4W}
    ].

render_option_pair({"", ""}, OptionW, ValueW) ->
    [?W("", OptionW), ?W("", ValueW)];
render_option_pair({Key, Value}, OptionW, ValueW) ->
    [?W(Key, OptionW), ?W(format_sock_opt(Key, Value), ValueW)].

format_sock_opt(Key, Value) when
    Key =:= buffer;
    Key =:= high_msgq_watermark;
    Key =:= high_watermark;
    Key =:= low_msgq_watermark;
    Key =:= low_watermark;
    Key =:= packet_size;
    Key =:= recbuf;
    Key =:= sndbuf
->
    {byte, Value};
format_sock_opt(linger, Value) ->
    io_lib:format("~p", [Value]);
format_sock_opt(_Key, Value) ->
    Value.

stats_widths() ->
    fill_last(
        observer_cli_lib:weighted_widths([9, 12, 8, 12, 9, 12, 9, 12, 9, 12], [
            0, 1, 0, 1, 0, 1, 0, 1, 0, 1
        ]),
        wide_fill(5)
    ).

opts_widths() ->
    fill_last(
        observer_cli_lib:weighted_widths([19, 8, 19, 8, 19, 8, 19, 8], [
            2, 1, 2, 1, 2, 1, 2, 1
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
        observer_cli_lib:menu_item(Type, ports, "Ports(O)"),
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
        "O\n" -> ports_view;
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
