%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_socket).

-include("observer_cli.hrl").

-export([start/1, start/2]).

-ifdef(TEST).
-export([
    collect_general_info/0,
    add_delta_counters/2,
    current_counters/1,
    counter_delta/2,
    sort_value/2,
    collect_socket_detail/1,
    collect_socket_overviews/0,
    render_general_info/1,
    render_socket_detail/1,
    render_socket_rows/2,
    select_socket/2,
    socket_overview/1,
    socket_options/2,
    protocol_option_specs/3,
    safe_getopt/2,
    safe_getopt/3,
    sockaddr_to_list/1,
    format_value/1,
    value_or_zero/1,
    collect_socket_render_info/4,
    socket_available/0,
    safe_which_sockets/0,
    safe_socket_info/1,
    socket_id/1,
    socket_addr/2,
    safe_monitored_by/1,
    level_option_specs/1,
    domain_option_specs/1,
    getopt_value/2,
    render_kv_rows/2,
    collect_socket_info/2,
    render_socket_worker/3,
    output_die_view/2,
    next_draw_view/3,
    getopt_result/1
]).
-endif.

-define(LAST_LINE,
    "q(quit) io/rb/wb/pk/wt/fl/mx/ac/ow(sort) 9(socket info) pd/pu(page:down/up) "
    "F/B(forward/back) Current page is ~w"
).

-spec start(view_opts()) -> quit.
start(#view_opts{sockets = Sockets, auto_row = AutoRow} = ViewOpts) ->
    StorePid = observer_cli_store:start(),
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_sockets_worker(StorePid, Sockets, AutoRow, #{})
    end),
    manager(StorePid, RenderPid, ViewOpts).

-spec start(term(), view_opts()) -> quit.
start(Socket, #view_opts{sockets = #sockets{interval = Interval}} = Opts) ->
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_socket_worker(Socket, Interval, ?INIT_TIME_REF)
    end),
    detail_manager(RenderPid, Opts).

manager(
    StorePid, RenderPid, #view_opts{sockets = Sockets = #sockets{cur_page = CurPage}} = ViewOpts
) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [RenderPid, StorePid]) of
        quit ->
            observer_cli_lib:exit_processes([StorePid]),
            erlang:send(RenderPid, quit),
            quit;
        {new_interval, NewInterval} ->
            restart(StorePid, RenderPid, ViewOpts#view_opts{
                sockets = Sockets#sockets{interval = NewInterval}
            });
        Action when
            Action =:= socket_io;
            Action =:= socket_read_byte;
            Action =:= socket_write_byte;
            Action =:= socket_packets;
            Action =:= socket_waits;
            Action =:= socket_fails;
            Action =:= socket_max_packet;
            Action =:= socket_accepts;
            Action =:= socket_id;
            Action =:= socket_fd;
            Action =:= socket_owner;
            Action =:= socket_domain;
            Action =:= socket_type;
            Action =:= socket_protocol
        ->
            restart(StorePid, RenderPid, ViewOpts#view_opts{
                sockets = Sockets#sockets{sort = socket_sort(Action)}
            });
        {jump, Pos} ->
            open_socket_from_list(StorePid, RenderPid, ViewOpts, Pos);
        page_down_top_n ->
            restart_page(StorePid, RenderPid, ViewOpts, CurPage, 1);
        page_up_top_n ->
            restart_page(StorePid, RenderPid, ViewOpts, CurPage, -1);
        _ ->
            manager(StorePid, RenderPid, ViewOpts)
    end.

restart_page(StorePid, RenderPid, ViewOpts = #view_opts{sockets = Sockets}, CurPage, Delta) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    restart(StorePid, RenderPid, ViewOpts#view_opts{sockets = Sockets#sockets{cur_page = NewPage}}).

restart(StorePid, RenderPid, ViewOpts = #view_opts{sockets = Sockets}) ->
    %% Sorting, paging and refresh changes do not invalidate the sample.
    Ref = make_ref(),
    RenderPid ! {configure, Sockets, self(), Ref},
    receive
        {Ref, ready} -> manager(StorePid, RenderPid, ViewOpts)
    end.

socket_sort(socket_io) -> io;
socket_sort(socket_read_byte) -> rb;
socket_sort(socket_write_byte) -> wb;
socket_sort(socket_packets) -> pk;
socket_sort(socket_waits) -> wt;
socket_sort(socket_fails) -> fl;
socket_sort(socket_max_packet) -> mx;
socket_sort(socket_accepts) -> ac;
socket_sort(socket_id) -> id;
socket_sort(socket_fd) -> fd;
socket_sort(socket_owner) -> owner;
socket_sort(socket_domain) -> domain;
socket_sort(socket_type) -> type;
socket_sort(socket_protocol) -> protocol.

open_socket_from_list(StorePid, RenderPid, ViewOpts, Pos) ->
    case observer_cli_store:lookup_pos(StorePid, Pos) of
        {FoundPos, Socket} when is_integer(FoundPos) ->
            observer_cli_lib:exit_processes([StorePid, RenderPid]),
            start(Socket, ViewOpts);
        _ ->
            manager(StorePid, RenderPid, ViewOpts)
    end.

render_sockets_worker(StorePid, Sockets, AutoRow, LastCounters) ->
    render_sockets_worker(StorePid, Sockets, AutoRow, LastCounters, undefined).

render_sockets_worker(
    StorePid,
    Sockets = #sockets{interval = Interval, sort = Sort, cur_page = CurPage},
    AutoRow,
    LastCounters,
    Notify
) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    BaseRows = erlang:max(TerminalRow - 10, 0),
    Text = io_lib:format("Sort: ~p | Refresh:~wms", [Sort, Interval]),
    GeneralRows = render_general_info(collect_general_info()),
    {SocketInfo, NewCounters} =
        case collect_socket_info(Sort, LastCounters) of
            {error, _} = Error -> {Error, #{}};
            Sample -> Sample
        end,
    {ExtraRows, Menu} = observer_cli_lib:render_sampling_menu(
        sockets,
        [Text, socket_sample_status(LastCounters, NewCounters)]
    ),
    Rows = max(BaseRows - ExtraRows, 0),
    SocketRenderInfo =
        case SocketInfo of
            {error, _} -> SocketInfo;
            _ -> observer_cli_lib:sublist(SocketInfo, Rows, CurPage)
        end,
    {SocketList, SocketRows} = render_socket_rows(SocketRenderInfo, Sort),
    LastLine = observer_cli_lib:render_footer(io_lib:format(?LAST_LINE, [CurPage])),
    ?output([?CURSOR_TOP, Menu, GeneralRows, SocketRows, LastLine, "\e[J"]),
    observer_cli_store:update(StorePid, Rows, SocketList),
    case Notify of
        undefined ->
            ok;
        {Manager, Ref} ->
            %% Fence the row mapping before accepting a jump on the new page.
            _ = observer_cli_store:lookup_row(StorePid),
            Manager ! {Ref, ready}
    end,
    Timer = erlang:send_after(Interval, self(), redraw),
    receive
        quit ->
            quit;
        {configure, NewSockets, From, ConfigRef} ->
            erlang:cancel_timer(Timer),
            receive
                redraw -> ok
            after 0 -> ok
            end,
            render_sockets_worker(StorePid, NewSockets, AutoRow, NewCounters, {From, ConfigRef});
        redraw ->
            render_sockets_worker(StorePid, Sockets, AutoRow, NewCounters)
    end.

socket_sample_status(Previous, Current) ->
    case {maps:find(sample_time, Previous), maps:find(sample_time, Current)} of
        {_, error} ->
            " | missing sample; baseline cleared";
        {error, _} ->
            " | warming up";
        {{ok, Before}, {ok, After}} ->
            io_lib:format(" | Sample:~wms", [round((After - Before) / 1000)])
    end.

-ifdef(TEST).
collect_socket_render_info(Rows, CurPage, Sort, LastCounters) ->
    case collect_socket_info(Sort, LastCounters) of
        {error, _Reason} = Error ->
            {Error, #{}};
        {SocketInfo, NewCounters} ->
            {observer_cli_lib:sublist(SocketInfo, Rows, CurPage), NewCounters}
    end.

-endif.

sort_value(Sort, Info) when
    Sort =:= io;
    Sort =:= rb;
    Sort =:= wb;
    Sort =:= pk;
    Sort =:= wt;
    Sort =:= fl;
    Sort =:= ac
->
    Value =
        case Sort of
            io -> sum_metrics([read_bytes(Info), write_bytes(Info)]);
            rb -> read_bytes(Info);
            wb -> write_bytes(Info);
            pk -> packets(Info);
            wt -> waits(Info);
            fl -> fails(Info);
            ac -> accepts(Info)
        end,
    case is_number(Value) of
        true -> Value;
        false -> -1
    end;
sort_value(mx, Info) ->
    case max_packet(Info) of
        Value when is_number(Value) -> Value;
        _ -> -1
    end;
sort_value(fd, Info) ->
    maps:get(fd, Info, -1);
sort_value(Sort, Info) ->
    format_value(maps:get(Sort, Info, "")).

-ifdef(TEST).
select_socket(Pos, #sockets{sort = Sort}) when is_integer(Pos), Pos > 0 ->
    case collect_socket_info(Sort, #{}) of
        {error, _Reason} ->
            error;
        {SocketInfo, _Counters} ->
            SortSockets = recon_lib:sublist_top_n_attrs(SocketInfo, Pos),
            case erlang:length(SortSockets) >= Pos of
                true ->
                    {_, _, Fields} = lists:nth(Pos, SortSockets),
                    {ok, maps:get(id, maps:from_list(Fields))};
                false ->
                    error
            end
    end;
select_socket(_Pos, _Sockets) ->
    error.
-endif.

collect_general_info() ->
    case socket_available() of
        false ->
            {error, "socket API is not available"};
        true ->
            try
                Info0 = socket:info(),
                {Counters, Info1} = maps:take(counters, Info0),
                Counters#{
                    iov_max => maps:get(iov_max, Info1, undefined),
                    num_sockets => socket:number_of(),
                    num_monitors => socket:number_of_monitors()
                }
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end
    end.

collect_socket_overviews() ->
    case socket_available() of
        false ->
            {error, "socket API is not available"};
        true ->
            case safe_which_sockets() of
                {error, _} = Error ->
                    Error;
                Sockets ->
                    [
                        case socket_overview(Socket) of
                            dead -> (socket_overview(Socket, #{}))#{sample_state => missing};
                            Info -> Info
                        end
                     || Socket <- Sockets
                    ]
            end
    end.

collect_socket_info(Sort, LastCounters) ->
    case collect_socket_overviews() of
        {error, _Reason} = Error ->
            Error;
        SocketInfos0 ->
            SocketInfos = add_delta_counters(SocketInfos0, LastCounters),
            {
                [{0, sort_value(Sort, Info), maps:to_list(Info)} || Info <- SocketInfos],
                current_counters(SocketInfos0)
            }
    end.

add_delta_counters(SocketInfos, LastCounters) ->
    [
        Info#{
            delta_counters =>
                case maps:get(sample_state, Info, available) of
                    missing ->
                        #{};
                    available ->
                        counter_delta(
                            maps:get(counters, Info, #{}), maps:get(Id, LastCounters, undefined)
                        )
                end
        }
     || #{id := Id} = Info <- SocketInfos
    ].

current_counters(SocketInfos) ->
    (maps:from_list([{Id, maps:get(counters, Info, #{})} || #{id := Id} = Info <- SocketInfos]))#{
        sample_time => erlang:monotonic_time(microsecond)
    }.

counter_delta(_Current, undefined) ->
    warming_up;
counter_delta(Current, Previous) ->
    Keys = lists:usort(maps:keys(Current) ++ maps:keys(Previous)),
    maps:from_list([
        {Key,
            case {maps:find(Key, Current), maps:find(Key, Previous)} of
                {{ok, Now}, {ok, Before}} when is_integer(Now), is_integer(Before), Now >= Before ->
                    Now - Before;
                {{ok, Now}, {ok, Before}} when is_integer(Now), is_integer(Before) -> "reset";
                _ ->
                    "miss"
            end}
     || Key <- Keys
    ]).

socket_available() ->
    case code:ensure_loaded(socket) of
        {module, socket} ->
            erlang:function_exported(socket, info, 0) andalso
                erlang:function_exported(socket, which_sockets, 0);
        _ ->
            false
    end.

safe_which_sockets() ->
    try socket:which_sockets() of
        Sockets -> Sockets
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

socket_overview(Socket) ->
    case safe_socket_info(Socket) of
        {ok, Info} ->
            socket_overview(Socket, Info);
        error ->
            dead
    end.

socket_overview(Socket, Info) ->
    #{
        id => Socket,
        id_str => socket_id(Socket),
        fd => socket_fd(Socket),
        owner => maps:get(owner, Info, undefined),
        domain => maps:get(domain, Info, undefined),
        type => maps:get(type, Info, undefined),
        protocol => maps:get(protocol, Info, undefined),
        counters => maps:get(counters, Info, #{}),
        rstate => maps:get(rstates, Info, maps:get(rstate, Info, [])),
        wstate => maps:get(wstates, Info, maps:get(wstate, Info, []))
    }.

safe_socket_info(Socket) ->
    try socket:info(Socket) of
        Info -> {ok, Info}
    catch
        _:_ -> error
    end.

socket_id(Socket) ->
    try socket:to_list(Socket) of
        Id -> Id
    catch
        _:_ -> format_value(Socket)
    end.

socket_fd(Socket) ->
    case safe_getopt(Socket, otp, fd) of
        {ok, Fd} -> Fd;
        _ -> -1
    end.

render_general_info({error, Reason}) ->
    [observer_cli_lib:ansi_red(["Socket API unavailable: ", format_value(Reason)]), "\n"];
render_general_info(Info) ->
    render_kv_section("General", general_pairs(Info), general_widths()).

general_pairs(Info) ->
    keys_to_pairs(
        [
            iov_max,
            num_cnt_bits,
            num_sockets,
            num_monitors,
            num_dinet,
            num_dinet6,
            num_dlocal,
            num_tstreams,
            num_tdgrams,
            num_tseqpkgs,
            num_pip,
            num_psctp,
            num_ptcp,
            num_pudp
        ],
        Info
    ).

render_socket_rows({error, Reason}, _Sort) ->
    {[], [observer_cli_lib:ansi_red(["Socket API unavailable: ", format_value(Reason)]), "\n"]};
render_socket_rows({_StartPos, []}, _Sort) ->
    {[], [observer_cli_lib:ansi_green("No socket API sockets found."), "\n"]};
render_socket_rows({StartPos, SocketInfos}, Sort) ->
    Legend = render_socket_legend(),
    Title = render_socket_title(Sort),
    {_, SocketList, Rows} = lists:foldl(
        fun({_, _, Fields}, {Pos, PosAcc, RowAcc}) ->
            Info = maps:from_list(Fields),
            {
                Pos + 1,
                [{Pos, maps:get(id, Info)} | PosAcc],
                [render_socket_row(Pos, Info) | RowAcc]
            }
        end,
        {StartPos, [], []},
        SocketInfos
    ),
    {lists:reverse(SocketList), [Legend, Title | lists:reverse(Rows)]}.

render_socket_legend() ->
    observer_cli_lib:render_footer(
        "Legend: chg=window change; rb/wb=bytes pk/ac=packets/accepts wt/fl=waits/fails; "
        "mx=lifetime max; warm=warming up, miss=missing"
    ).

render_socket_title(Sort) ->
    [NoW, IdW, OwnerW, EndpointW, KindW, StateW, ReadW, WriteW, PacketW, WaitW, FailW, MaxW] =
        socket_widths(),
    ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "NO", NoW),
        ?UNDERLINE,
        ?W2(sort_color(socket, Sort), "Socket", IdW),
        ?UNDERLINE,
        ?W2(sort_color(owner, Sort), "Owner(ow)", OwnerW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Endpoint", EndpointW),
        ?UNDERLINE,
        ?W2(sort_color(kind, Sort), "Kind", KindW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "State", StateW),
        ?UNDERLINE,
        ?W2(sort_color(read, Sort), "Read chg", ReadW),
        ?UNDERLINE,
        ?W2(sort_color(write, Sort), "Write chg", WriteW),
        ?UNDERLINE,
        ?W2(sort_color(packet, Sort), "Pkt/Acc chg", PacketW),
        ?UNDERLINE,
        ?W2(sort_color(wait, Sort), "Wait chg", WaitW),
        ?UNDERLINE,
        ?W2(sort_color(fail, Sort), "Fail chg", FailW),
        ?UNDERLINE,
        ?W2(sort_color(max_packet, Sort), "MaxPkt", MaxW)
    ]).

sort_color(socket, Sort) when Sort =:= id; Sort =:= fd -> ?RED_BG;
sort_color(kind, Sort) when Sort =:= domain; Sort =:= type; Sort =:= protocol -> ?RED_BG;
sort_color(read, Sort) when Sort =:= io; Sort =:= rb -> ?RED_BG;
sort_color(write, Sort) when Sort =:= io; Sort =:= wb -> ?RED_BG;
sort_color(packet, Sort) when Sort =:= pk; Sort =:= ac -> ?RED_BG;
sort_color(wait, wt) -> ?RED_BG;
sort_color(fail, fl) -> ?RED_BG;
sort_color(max_packet, mx) -> ?RED_BG;
sort_color(owner, owner) -> ?RED_BG;
sort_color(_Key, _Sort) -> ?GRAY_BG.

render_socket_row(
    Pos,
    #{
        id_str := Id,
        owner := Owner,
        id := Socket
    } = Info
) ->
    [NoW, IdW, OwnerW, EndpointW, KindW, StateW, ReadW, WriteW, PacketW, WaitW, FailW, MaxW] =
        socket_widths(),
    ?render([
        ?W2(?RESET, Pos, NoW),
        ?W2(?RESET, Id, IdW),
        ?W2(?RESET, format_value(Owner), OwnerW),
        ?W2(?RESET, endpoint(Socket), EndpointW),
        ?W2(?RESET, kind(Info), KindW),
        ?W2(?RESET, state(Info), StateW),
        ?W2(?RESET, {byte, read_bytes(Info)}, ReadW),
        ?W2(?RESET, {byte, write_bytes(Info)}, WriteW),
        ?W2(?RESET, packets_accepts(Info), PacketW),
        ?W2(?RESET, waits(Info), WaitW),
        ?W2(?RESET, fails(Info), FailW),
        ?W2(?RESET, {byte, max_packet(Info)}, MaxW)
    ]).

socket_widths() ->
    observer_cli_lib:weighted_widths(
        [4, 16, 12, 20, 11, 11, 8, 9, 11, 8, 8, 7],
        [0, 3, 2, 5, 1, 2, 1, 1, 1, 0, 0, 1]
    ).

endpoint(Socket) ->
    [socket_addr(Socket, sockname), " -> ", socket_addr(Socket, peername)].

kind(#{domain := Domain, type := Type, protocol := Protocol}) ->
    [format_value(Domain), "/", format_value(Protocol), "/", format_value(Type)].

state(#{sample_state := missing}) ->
    "missing";
state(#{delta_counters := warming_up}) ->
    "warming up";
state(#{rstate := ReadState, wstate := WriteState}) ->
    ["R:", short_state(ReadState), " W:", short_state(WriteState)].

short_state([]) ->
    "-";
short_state(State) ->
    format_value(State).

read_bytes(Info) ->
    delta_counter(Info, read_byte).

write_bytes(Info) ->
    sum_metrics([delta_counter(Info, write_byte), optional_delta(Info, sendfile_byte)]).

packets(Info) ->
    sum_metrics([
        delta_counter(Info, read_pkg),
        delta_counter(Info, write_pkg),
        optional_delta(Info, sendfile_pkg)
    ]).

packets_accepts(Info) ->
    [format_value(packets(Info)), "/", format_value(delta_counter(Info, acc_success))].

waits(Info) ->
    sum_metrics([
        delta_counter(Info, acc_waits),
        delta_counter(Info, read_waits),
        delta_counter(Info, write_waits),
        optional_delta(Info, sendfile_waits)
    ]).

fails(Info) ->
    sum_metrics([
        delta_counter(Info, acc_fails),
        delta_counter(Info, read_fails),
        delta_counter(Info, write_fails),
        optional_delta(Info, sendfile_fails)
    ]).

max_packet(Info) ->
    Counters = maps:get(counters, Info, #{}),
    Values = [
        maps:get(read_pkg_max, Counters, "miss"),
        maps:get(write_pkg_max, Counters, "miss"),
        maps:get(sendfile_pkg_max, Counters, 0)
    ],
    case lists:all(fun is_integer/1, Values) of
        true -> lists:max(Values);
        false -> "miss"
    end.

accepts(Info) ->
    sum_metrics([delta_counter(Info, acc_success), delta_counter(Info, acc_tries)]).

sum_metrics(Values) ->
    case [Value || Value <- Values, not is_integer(Value)] of
        [] -> lists:sum(Values);
        [Status | _] -> Status
    end.

optional_delta(Info, Key) -> delta_counter(Info, Key, 0).

delta_counter(Info, Key) -> delta_counter(Info, Key, "miss").

delta_counter(Info, Key, Default) ->
    case maps:get(delta_counters, Info, warming_up) of
        warming_up -> "warm";
        Counters -> maps:get(Key, Counters, Default)
    end.

render_socket_worker(Socket, Interval, TimeRef) ->
    case collect_socket_detail(Socket) of
        dead ->
            output_die_view(Socket, Interval),
            next_draw_view(TimeRef, Interval, Socket);
        Detail ->
            Menu = render_detail_menu(Interval),
            Lines = render_socket_detail(Detail),
            LastLine = observer_cli_lib:render_footer(
                "q(quit) K(sockets) | Counters: lifetime totals; *_max: lifetime maximum; miss: unavailable"
            ),
            ?output([?CURSOR_TOP, Menu, Lines, LastLine]),
            next_draw_view(TimeRef, Interval, Socket)
    end.

detail_manager(RenderPid, Opts) ->
    case observer_cli_lib:parse_cmd(Opts, ?MODULE, [RenderPid]) of
        quit ->
            erlang:send(RenderPid, quit),
            quit;
        {new_interval, NewInterval} ->
            erlang:send(RenderPid, {new_interval, NewInterval}),
            detail_manager(RenderPid, Opts#view_opts{
                sockets = (Opts#view_opts.sockets)#sockets{interval = NewInterval}
            });
        _ ->
            detail_manager(RenderPid, Opts)
    end.

render_detail_menu(Interval) ->
    observer_cli_lib:render_top_menu(
        sockets, "Socket Info Refresh: " ++ integer_to_list(Interval) ++ "ms (configured)"
    ).

collect_socket_detail(Socket) ->
    case safe_socket_info(Socket) of
        error ->
            dead;
        {ok, Info} ->
            Overview = socket_overview(Socket, Info),
            Counters = maps:get(counters, Info, #{}),
            Overview#{
                laddress => socket_addr(Socket, sockname),
                raddress => socket_addr(Socket, peername),
                monitored_by => safe_monitored_by(Socket),
                counters => maps:to_list(Counters),
                options => socket_options(Socket, Info)
            }
    end.

socket_addr(Socket, sockname) ->
    case safe_sockname(Socket) of
        {ok, Addr} -> sockaddr_to_list(Addr);
        _ -> "-"
    end;
socket_addr(Socket, peername) ->
    case safe_peername(Socket) of
        {ok, Addr} -> sockaddr_to_list(Addr);
        _ -> "-"
    end.

safe_sockname(Socket) ->
    try socket:sockname(Socket) of
        Result -> Result
    catch
        _:_ -> {error, badarg}
    end.

safe_peername(Socket) ->
    try socket:peername(Socket) of
        Result -> Result
    catch
        _:_ -> {error, badarg}
    end.

safe_monitored_by(Socket) ->
    try socket:monitored_by(Socket) of
        MonitoredBy -> MonitoredBy
    catch
        _:_ -> []
    end.

socket_options(Socket, Info) ->
    lists:sort([socket_option(Socket, Spec) || Spec <- socket_option_specs(Info)]).

socket_option_specs(Info) ->
    Domain = maps:get(domain, Info, undefined),
    Type = maps:get(type, Info, undefined),
    Protocol = maps:get(protocol, Info, undefined),
    level_option_specs(socket) ++
        domain_option_specs(Domain) ++ protocol_option_specs(Domain, Type, Protocol).

level_option_specs(Level) ->
    try
        [{{Level, Opt}, Supported} || {Opt, Supported} <- socket:supports(options, Level)]
    catch
        _:_ -> []
    end.

domain_option_specs(inet6) ->
    level_option_specs(ipv6);
domain_option_specs(_Domain) ->
    level_option_specs(ip).

protocol_option_specs(Domain, stream, tcp) when Domain =:= inet; Domain =:= inet6 ->
    level_option_specs(tcp);
protocol_option_specs(Domain, dgram, udp) when Domain =:= inet; Domain =:= inet6 ->
    level_option_specs(udp);
protocol_option_specs(Domain, seqpacket, sctp) when Domain =:= inet; Domain =:= inet6 ->
    level_option_specs(sctp);
protocol_option_specs(_Domain, _Type, _Protocol) ->
    [].

socket_option(_Socket, {Key, false}) ->
    {option_key(Key), "Not Supported"};
socket_option(Socket, {Key, true}) ->
    {option_key(Key), getopt_value(Socket, Key)}.

getopt_value(Socket, Key) ->
    getopt_result(safe_getopt(Socket, Key)).

getopt_result({ok, []}) -> "-";
getopt_result({ok, Value}) -> Value;
getopt_result({error, enotsup}) -> "Not Supported";
getopt_result({error, enoprotoopt}) -> "Not Supported";
getopt_result({error, enotconn}) -> "Not Connected";
getopt_result({error, {invalid, _}}) -> "Not Implemented";
getopt_result({error, Reason}) -> io_lib:format("error:~p", [Reason]).

safe_getopt(Socket, Key) ->
    try socket:getopt(Socket, Key) of
        Result -> Result
    catch
        _:_ -> {error, badarg}
    end.

safe_getopt(Socket, Level, Key) ->
    try socket:getopt(Socket, Level, Key) of
        Result -> Result
    catch
        _:_ -> {error, badarg}
    end.

option_key({Level, Opt}) ->
    [format_value(Level), ":", format_value(Opt)].

render_socket_detail(#{
    id_str := Id,
    owner := Owner,
    fd := Fd,
    domain := Domain,
    type := Type,
    protocol := Protocol,
    rstate := ReadState,
    wstate := WriteState,
    laddress := LocalAddress,
    raddress := RemoteAddress,
    monitored_by := MonitoredBy,
    counters := Counters,
    options := Options
}) ->
    [
        render_kv_section(
            "Overview",
            [
                {"id", Id},
                {"owner", Owner},
                {"fd", Fd},
                {"domain", Domain},
                {"type", Type},
                {"protocol", Protocol},
                {"read_state", ReadState},
                {"write_state", WriteState},
                {"monitored_by", MonitoredBy}
            ],
            detail_widths()
        ),
        render_kv_section(
            "Net",
            [{"local_address", LocalAddress}, {"remote_address", RemoteAddress}],
            detail_widths()
        ),
        render_kv_section("Totals / maxima", counter_pairs(Counters), counter_widths()),
        render_kv_section("Options", Options, option_widths())
    ].

counter_pairs(Counters) ->
    Preferred = [
        acc_tries,
        acc_waits,
        acc_fails,
        acc_success,
        read_tries,
        read_waits,
        read_fails,
        read_byte,
        read_pkg,
        read_pkg_max,
        write_tries,
        write_waits,
        write_fails,
        write_byte,
        write_pkg,
        write_pkg_max
    ],
    Ordered = keys_to_pairs(Preferred, maps:from_list(Counters)),
    RestKeys = lists:sort([Key || {Key, _Value} <- Counters, not lists:member(Key, Preferred)]),
    Ordered ++ keys_to_pairs(RestKeys, maps:from_list(Counters)).

keys_to_pairs(Keys, Map) ->
    [{Key, format_counter(Key, maps:get(Key, Map, undefined))} || Key <- Keys].

format_counter(_Key, undefined) ->
    "miss";
format_counter(Key, Value) when
    Key =:= read_byte;
    Key =:= read_pkg_max;
    Key =:= write_byte;
    Key =:= write_pkg_max
->
    {byte, value_or_zero(Value)};
format_counter(_Key, Value) ->
    Value.

value_or_zero(undefined) -> 0;
value_or_zero(Value) -> Value.

render_kv_section(Header, Pairs, Widths) ->
    [render_kv_title(Header, Widths), render_kv_rows(Pairs, Widths)].

render_kv_title(Header, Widths) ->
    [Key1W, Value1W | Rest] = Widths,
    ?render([
        ?GRAY_BG,
        ?W(Header, Key1W),
        ?W("Value", Value1W)
        | render_kv_title_rest(Rest)
    ]).

render_kv_title_rest([]) ->
    [];
render_kv_title_rest([KeyW, ValueW | Rest]) ->
    [?W("Attr", KeyW), ?W("Value", ValueW) | render_kv_title_rest(Rest)].

render_kv_rows([], _Widths) ->
    [];
render_kv_rows(Pairs, Widths) ->
    PairCount = erlang:length(Widths) div 2,
    Rows = [
        lists:sublist(Pairs, Pos, PairCount)
     || Pos <- lists:seq(1, erlang:length(Pairs), PairCount)
    ],
    ?render(lists:append(lists:join(?NEW_LINE, [render_kv_row(Row, Widths) || Row <- Rows]))).

render_kv_row(Pairs, Widths) ->
    PairCount = erlang:length(Widths) div 2,
    FilledPairs = Pairs ++ lists:duplicate(PairCount - erlang:length(Pairs), {"", ""}),
    lists:append([
        [?W(format_key(Key), KeyW), ?W(format_cell_value(Value), ValueW)]
     || {{Key, Value}, KeyW, ValueW} <- kv_columns(FilledPairs, Widths)
    ]).

kv_columns(Pairs, Widths) ->
    lists:zip3(Pairs, take_odd(Widths), take_even(Widths)).

take_odd([]) -> [];
take_odd([Odd | Rest]) -> [Odd | take_even(Rest)].

take_even([]) -> [];
take_even([_Odd | Rest]) -> take_odd(Rest).

format_key(Key) ->
    format_value(Key).

format_cell_value({byte, _Value} = Byte) ->
    Byte;
format_cell_value(Value) ->
    format_value(Value).

general_widths() ->
    observer_cli_lib:weighted_widths([17, 10, 17, 10, 17, 10, 17, 10], [2, 1, 2, 1, 2, 1, 2, 1]).

detail_widths() ->
    observer_cli_lib:weighted_widths([15, 25, 15, 25, 15, 25], [1, 3, 1, 3, 1, 3]).

counter_widths() ->
    observer_cli_lib:weighted_widths([15, 12, 15, 12, 15, 12, 15, 12], [1, 1, 1, 1, 1, 1, 1, 1]).

option_widths() ->
    observer_cli_lib:weighted_widths([22, 18, 22, 18, 22, 18], [2, 2, 2, 2, 2, 2]).

next_draw_view(TimeRef, Interval, Socket) ->
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_socket_worker(Socket, NewInterval, NewTimeRef);
        _ ->
            ?output(?CLEAR),
            render_socket_worker(Socket, Interval, NewTimeRef)
    end.

output_die_view(Socket, Interval) ->
    Menu = render_detail_menu(Interval),
    Line = [
        observer_cli_lib:ansi_red(
            io_lib:format("Socket(~s) has already died.", [socket_id(Socket)])
        ),
        "\n"
    ],
    LastLine = observer_cli_lib:render_footer(
        "q(quit) K(sockets) | Counters: lifetime totals; *_max: lifetime maximum; miss: unavailable"
    ),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

sockaddr_to_list(#{family := local, path := Path}) ->
    format_value(Path);
sockaddr_to_list(#{family := inet, addr := Addr, port := Port}) ->
    inet_parse:ntoa(Addr) ++ ":" ++ integer_to_list(Port);
sockaddr_to_list(#{family := inet6, addr := Addr, port := Port, flowinfo := FI, scope_id := SID}) ->
    inet_parse:ntoa(Addr) ++ ":" ++ integer_to_list(Port) ++ "," ++ integer_to_list(FI) ++ "," ++
        integer_to_list(SID);
sockaddr_to_list(Addr) ->
    format_value(Addr).

format_value(undefined) ->
    "-";
format_value([]) ->
    "-";
format_value(Value) when is_atom(Value) ->
    atom_to_list(Value);
format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_pid(Value) ->
    pid_to_list(Value);
format_value(Value) when is_port(Value) ->
    port_to_list(Value);
format_value(Value) when is_reference(Value) ->
    ref_to_list(Value);
format_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
format_value(Value) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true -> Value;
        false -> lists:flatten(io_lib:format("~p", [Value]))
    end;
format_value(Value) ->
    lists:flatten(io_lib:format("~p", [Value])).
