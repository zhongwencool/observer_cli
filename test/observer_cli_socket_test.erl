-module(observer_cli_socket_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

diagnostic_socket_formulas_and_missing_core_test() ->
    Counters = (counter_fixture())#{
        sendfile_byte => 10,
        sendfile_pkg => 2,
        sendfile_waits => 3,
        sendfile_fails => 4
    },
    Metrics = observer_cli_snapshot:socket_metrics(Counters),
    ?assertEqual(#{status => available, value => 2048}, maps:get(read_bytes, Metrics)),
    ?assertEqual(#{status => available, value => 1034}, maps:get(write_bytes, Metrics)),
    ?assertEqual(#{status => available, value => 3082}, maps:get(io, Metrics)),
    ?assertEqual(#{status => available, value => 5}, maps:get(packets, Metrics)),
    ?assertEqual(#{status => available, value => 18}, maps:get(waits, Metrics)),
    ?assertEqual(#{status => available, value => 22}, maps:get(fails, Metrics)),
    Missing = observer_cli_snapshot:socket_metrics(maps:remove(read_byte, Counters)),
    ?assertEqual(#{status => missing_core}, maps:get(read_bytes, Missing)),
    ?assertEqual(#{status => missing_core}, maps:get(io, Missing)),
    OptionalAbsent = observer_cli_snapshot:socket_metrics(counter_fixture()),
    ?assertEqual(#{status => available, value => 1024}, maps:get(write_bytes, OptionalAbsent)).

diagnostic_socket_identity_reset_and_shape_test() ->
    SocketA = {'$socket', make_ref()},
    SocketB = {'$socket', make_ref()},
    First = #{SocketA => diagnostic_socket_item(SocketA, counter_fixture())},
    ReusedDescriptor = #{
        SocketB => diagnostic_socket_item(SocketB, (counter_fixture())#{read_byte := 4096})
    },
    ReuseWindow = observer_cli_snapshot:counter_window(sockets, First, ReusedDescriptor),
    [Born] = maps:get(items, ReuseWindow),
    ?assertEqual(baseline_missing, maps:get(state, Born)),
    ?assertEqual([SocketA], maps:get(gone, ReuseWindow)),
    ResetCounters = (counter_fixture())#{read_byte := 1},
    [Reset] = maps:get(
        items,
        observer_cli_snapshot:counter_window(
            sockets, First, #{SocketA => diagnostic_socket_item(SocketA, ResetCounters)}
        )
    ),
    ?assertEqual(counter_reset, maps:get(state, Reset)),
    ShapeCounters = (counter_fixture())#{sendfile_byte => 1},
    [Shape] = maps:get(
        items,
        observer_cli_snapshot:counter_window(
            sockets, First, #{SocketA => diagnostic_socket_item(SocketA, ShapeCounters)}
        )
    ),
    ?assertEqual(shape_change, maps:get(state, Shape)).

diagnostic_socket_registry_coverage_and_duration_fixture_test() ->
    {ok, Registered} = socket:open(inet, stream, tcp),
    {ok, Unregistered} = socket:open(inet, stream, tcp, #{use_registry => false}),
    try
        Known = socket:which_sockets(),
        ?assert(lists:member(Registered, Known)),
        ?assertNot(lists:member(Unregistered, Known)),
        Total = diagnostic_socket_data(#{sort => io, limit => 20}),
        ?assertEqual(<<"total">>, maps:get(<<"sort_semantics">>, Total)),
        ?assertEqual(<<"no_registry_known_sockets">>, maps:get(<<"empty_meaning">>, Total)),
        Delta = diagnostic_socket_data(#{sort => io, limit => 20, duration_ms => 250}),
        ?assertEqual(<<"delta">>, maps:get(<<"sort_semantics">>, Delta)),
        ?assert(maps:get(<<"interval_ms">>, Delta) >= 250),
        [Probe] = maps:get(<<"probes">>, diagnostic_socket_capture(#{sort => io, limit => 20})),
        ?assert(lists:member(<<"registry_known_sockets">>, maps:get(<<"coverage">>, Probe))),
        ?assert(
            lists:member(<<"optional_sendfile_counter_absent">>, maps:get(<<"coverage">>, Probe))
        )
    after
        socket:close(Registered),
        socket:close(Unregistered)
    end.

diagnostic_socket_enumeration_error_test() ->
    Source = #{
        available_fun => fun() -> true end,
        count_fun => fun() -> 0 end,
        global_fun => fun() -> #{use_registry => false} end,
        all_fun => fun() -> {error, registry_failed} end,
        info_fun => fun(_Socket) -> error(unexpected_info) end,
        sleep_fun => fun timer:sleep/1,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = observer_cli_snapshot:dispatch(
        self(),
        sockets,
        #{test_socket_source => Source},
        #{timeout_ms => 5000, identifier_policy => include}
    ),
    ?assertEqual(<<"partial">>, maps:get(<<"outcome">>, Result)),
    ?assertEqual(
        <<"enumeration_error">>, maps:get(<<"reason_code">>, maps:get(<<"data">>, Result))
    ).

diagnostic_socket_excludes_endpoints_and_descriptors_test() ->
    Socket = {'$socket', make_ref()},
    Source = #{
        available_fun => fun() -> true end,
        count_fun => fun() -> 1 end,
        global_fun => fun() -> #{use_registry => true} end,
        all_fun => fun() -> {ok, [Socket]} end,
        info_fun => fun(_Socket) ->
            #{
                counters => counter_fixture(),
                domain => local,
                type => stream,
                protocol => default,
                fd => 42,
                laddress => <<"/tmp/diagnostic-secret.sock">>,
                raddress => {{192, 0, 2, 1}, 1883}
            }
        end,
        sleep_fun => fun timer:sleep/1,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = observer_cli_snapshot:dispatch(
        self(),
        sockets,
        #{test_socket_source => Source},
        #{timeout_ms => 5000, identifier_policy => include}
    ),
    Encoded = term_to_binary(Result),
    ?assertEqual(nomatch, binary:match(Encoded, <<"diagnostic-secret.sock">>)),
    ?assertEqual(nomatch, binary:match(Encoded, <<"192.0.2.1">>)),
    ?assertEqual(nomatch, binary:match(Encoded, <<"fd">>)).

collect_general_info_test() ->
    Info = observer_cli_socket:collect_general_info(),
    ?assert(is_map(Info)),
    ?assert(maps:is_key(iov_max, Info)),
    ?assert(maps:is_key(num_cnt_bits, Info)),
    ?assert(maps:is_key(num_sockets, Info)),
    ?assert(maps:is_key(num_monitors, Info)).

collect_socket_detail_test() ->
    with_socket(fun(Socket) ->
        Detail = observer_cli_socket:collect_socket_detail(Socket),
        ?assertMatch(#{owner := _, domain := inet, type := stream, protocol := tcp}, Detail),
        ?assert(maps:is_key(counters, Detail)),
        ?assert(maps:is_key(options, Detail)),
        ?assert(lists:keymember(read_byte, 1, maps:get(counters, Detail)))
    end).

socket_runtime_helper_contract_test() ->
    ?assert(observer_cli_socket:socket_available()),
    ?assert(is_list(observer_cli_socket:safe_which_sockets())),
    Invalid = make_ref(),
    ?assertEqual(error, observer_cli_socket:safe_socket_info(Invalid)),
    ?assert(is_list(observer_cli_socket:socket_id(Invalid))),
    ?assertEqual(1, observer_cli_socket:counter_value(#{read => 1}, read)),
    ?assertEqual(0, observer_cli_socket:counter_value(#{read => invalid}, read)),
    ?assertEqual("-", observer_cli_socket:socket_addr(Invalid, sockname)),
    ?assertEqual("-", observer_cli_socket:socket_addr(Invalid, peername)),
    ?assertEqual([], observer_cli_socket:safe_monitored_by(Invalid)),
    ?assertEqual(dead, observer_cli_socket:socket_overview(Invalid)),
    ?assertEqual(dead, observer_cli_socket:collect_socket_detail(Invalid)),
    ?assert(is_list(observer_cli_socket:level_option_specs(socket))),
    ?assert(is_list(observer_cli_socket:domain_option_specs(inet6))),
    ?assert(is_list(observer_cli_socket:domain_option_specs(inet))),
    ?assertMatch(
        [$e, $r, $r, $o, $r, $: | _],
        observer_cli_socket:getopt_value(
            Invalid, {socket, sndbuf}
        )
    ),
    ?assertEqual("-", observer_cli_socket:getopt_result({ok, []})),
    ?assertEqual(value, observer_cli_socket:getopt_result({ok, value})),
    ?assertEqual("Not Supported", observer_cli_socket:getopt_result({error, enotsup})),
    ?assertEqual("Not Supported", observer_cli_socket:getopt_result({error, enoprotoopt})),
    ?assertEqual("Not Connected", observer_cli_socket:getopt_result({error, enotconn})),
    ?assertEqual(
        "Not Implemented", observer_cli_socket:getopt_result({error, {invalid, option}})
    ),
    ?assertMatch(
        [$e, $r, $r, $o, $r, $: | _],
        observer_cli_socket:getopt_result({error, failed})
    ),
    ?assertEqual([], observer_cli_socket:render_kv_rows([], [10, 10])),
    ?assertNotEqual(
        [],
        observer_cli_socket:render_kv_rows(
            [{one, 1}, {two, 2}, {three, 3}], [10, 10, 10, 10]
        )
    ),
    with_socket(fun(Socket) ->
        ?assertMatch({ok, _}, observer_cli_socket:safe_socket_info(Socket)),
        ?assert(is_list(observer_cli_socket:socket_id(Socket))),
        ?assertMatch({_, _}, observer_cli_socket:collect_socket_info(id, #{})),
        ?assertMatch(
            {_, _},
            observer_cli_socket:collect_socket_render_info(
                10, 1, id, #{}
            )
        )
    end),
    observer_cli_test_io:with_geometry(24, 160, [], fun() ->
        self() ! quit,
        ?assertEqual(quit, observer_cli_socket:render_socket_worker(Invalid, 10, undefined)),
        self() ! quit,
        ?assertEqual(quit, observer_cli_socket:next_draw_view(undefined, 10, Invalid)),
        ?assertNotEqual([], observer_cli_socket:output_die_view(Invalid, 10))
    end),
    with_socket(fun(Socket) ->
        observer_cli_test_io:with_geometry(24, 160, [], fun() ->
            self() ! quit,
            ?assertEqual(quit, observer_cli_socket:render_socket_worker(Socket, 10, undefined))
        end)
    end).

render_general_info_test() ->
    Text = plain(observer_cli_socket:render_general_info(general_fixture())),
    ?assert(string:find(Text, "General") =/= nomatch),
    ?assert(string:find(Text, "num_sockets") =/= nomatch),
    ?assert(string:find(Text, "num_ptcp") =/= nomatch).

render_socket_rows_test() ->
    {_SocketList, Rows} = observer_cli_socket:render_socket_rows(
        {1, [ranked_overview_fixture()]}, io
    ),
    Text = plain(Rows),
    ?assert(string:find(Text, "Legend: io=read+write") =/= nomatch),
    ?assert(string:find(Text, "Read rb") =/= nomatch),
    ?assert(string:find(Text, "Write wb") =/= nomatch),
    ?assert(string:find(Text, "Pkt/Acc") =/= nomatch),
    ?assert(string:find(Text, "#Socket<0.1.2>") =/= nomatch),
    ?assert(string:find(Text, "inet/tcp") =/= nomatch),
    ?assert(string:find(Text, "3/1") =/= nomatch).

render_socket_empty_and_error_states_test() ->
    ?assertNotEqual(
        nomatch,
        string:find(
            plain(observer_cli_socket:render_general_info({error, unavailable})), "unavailable"
        )
    ),
    {[], Empty} = observer_cli_socket:render_socket_rows({1, []}, io),
    ?assertNotEqual(nomatch, string:find(plain(Empty), "No socket")),
    {[], Error} = observer_cli_socket:render_socket_rows({error, unavailable}, io),
    ?assertNotEqual(nomatch, string:find(plain(Error), "unavailable")),
    lists:foreach(
        fun(Sort) ->
            {_Sockets, Rows} = observer_cli_socket:render_socket_rows(
                {1, [ranked_overview_fixture()]}, Sort
            ),
            observer_cli_test_io:assert_ansi_boundaries(Rows)
        end,
        [id, fd, domain, type, protocol, pk, ac, wt, fl, mx]
    ).

render_socket_detail_test() ->
    Text = plain(observer_cli_socket:render_socket_detail(detail_fixture())),
    ?assert(string:find(Text, "Overview") =/= nomatch),
    ?assert(string:find(Text, "local_address") =/= nomatch),
    ?assert(string:find(Text, "read_byte") =/= nomatch),
    ?assert(string:find(Text, "socket:sndbuf") =/= nomatch).

render_socket_layout_width_test() ->
    observer_cli_test_io:with_geometry(
        24,
        205,
        [],
        fun() ->
            {_SocketList, SocketRows} = observer_cli_socket:render_socket_rows(
                {1, [ranked_overview_fixture()]}, owner
            ),
            Output = [
                observer_cli_socket:render_general_info(general_fixture()),
                SocketRows,
                observer_cli_socket:render_socket_detail(detail_fixture())
            ],
            observer_cli_test_io:assert_ansi_boundaries(Output),
            LayoutWidth = observer_cli_lib:layout_width(),
            ?assert(
                lists:all(
                    fun(Width) -> Width =< LayoutWidth end,
                    observer_cli_test_io:line_widths(Output)
                )
            )
        end
    ).

select_socket_test() ->
    with_socket(fun(Socket) ->
        SocketCount = length(observer_cli_socket:collect_socket_overviews()),
        ?assert(
            lists:any(
                fun(Pos) ->
                    observer_cli_socket:select_socket(Pos, #sockets{sort = id}) =:= {ok, Socket}
                end,
                lists:seq(1, SocketCount)
            )
        ),
        lists:foreach(
            fun(Sort) ->
                ?assertMatch({ok, _}, observer_cli_socket:select_socket(1, #sockets{sort = Sort}))
            end,
            [io, rb, wb, pk, wt, fl, mx, ac, fd]
        ),
        ?assertEqual(error, observer_cli_socket:select_socket(0, #sockets{})),
        ?assertEqual(error, observer_cli_socket:select_socket(SocketCount + 1, #sockets{}))
    end).

start_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_socket:start(Opts))
        end
    ).

start_manager_branches_test() ->
    Inputs = [
        "io\n",
        "rb\n",
        "wb\n",
        "pk\n",
        "wt\n",
        "fl\n",
        "mx\n",
        "ac\n",
        "id\n",
        "fd\n",
        "ow\n",
        "dm\n",
        "tp\n",
        "pt\n",
        "1000\n",
        "pd\n",
        "pu\n",
        "x\n",
        "q\n"
    ],
    observer_cli_test_io:with_input(
        Inputs,
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_socket:start(Opts))
        end
    ).

start_invalid_selection_test() ->
    observer_cli_test_io:with_input(
        ["99999\n", "q\n"],
        fun() ->
            ?assertEqual(quit, observer_cli_socket:start(#view_opts{auto_row = false}))
        end
    ).

start_detail_quit_test() ->
    with_socket(fun(Socket) ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                Opts = #view_opts{auto_row = false},
                ?assertEqual(quit, observer_cli_socket:start(Socket, Opts))
            end
        )
    end).

start_detail_interval_test() ->
    with_socket(fun(Socket) ->
        observer_cli_test_io:with_input(
            ["1000\n", "q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_socket:start(Socket, #view_opts{auto_row = false}))
            end
        )
    end).

start_detail_unknown_input_test() ->
    with_socket(fun(Socket) ->
        observer_cli_test_io:with_input(
            ["x\n", "q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_socket:start(Socket, #view_opts{auto_row = false}))
            end
        )
    end).

start_socket_selection_test() ->
    with_socket(fun(_Socket) ->
        {quit, Output} = observer_cli_test_io:capture_with_geometry(
            24,
            80,
            [{sleep, 250, "1\n"}, {sleep, 100, "q\n"}],
            fun() ->
                observer_cli_socket:start(#view_opts{auto_row = false})
            end
        ),
        observer_cli_test_io:assert_stable_fragments(Output, ["Overview", "local_address"])
    end).

start_socket_redraw_test() ->
    {quit, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        80,
        [{sleep, 200, "q\n"}],
        fun() ->
            Opts = #view_opts{auto_row = false, sockets = #sockets{interval = 20}},
            observer_cli_socket:start(Opts)
        end
    ),
    ?assert(length(binary:matches(iolist_to_binary(Output), <<"Current page is 1">>)) >= 2).

-if(?OTP_RELEASE >= 27).
socket_capability_absence_fails_closed_test() ->
    {module, socket} = code:ensure_loaded(socket),
    SocketPath = filename:dirname(code:which(socket)),
    CodePath = code:get_path(),
    true = code:del_path(SocketPath),
    try
        true = code:delete(socket),
        ?assertNot(observer_cli_socket:socket_available()),
        ?assertMatch({error, _}, observer_cli_socket:collect_general_info()),
        ?assertMatch({error, _}, observer_cli_socket:collect_socket_overviews()),
        ?assertMatch({error, _}, observer_cli_socket:collect_socket_info(io, #{})),
        ?assertMatch(
            {{error, _}, #{}},
            observer_cli_socket:collect_socket_render_info(10, 1, io, #{})
        ),
        ?assertEqual(error, observer_cli_socket:select_socket(1, #sockets{}))
    after
        true = code:set_path(CodePath),
        {module, socket} = code:ensure_loaded(socket)
    end.
-endif.

connected_socket_peer_address_test() ->
    {ok, Listen} = socket:open(inet, stream, tcp),
    ok = socket:bind(Listen, #{family => inet, addr => loopback, port => 0}),
    ok = socket:listen(Listen),
    {ok, #{port := Port}} = socket:sockname(Listen),
    {ok, Client} = socket:open(inet, stream, tcp),
    ok = socket:connect(Client, #{family => inet, addr => loopback, port => Port}),
    {ok, Server} = socket:accept(Listen),
    try
        ?assertNotEqual("-", observer_cli_socket:socket_addr(Client, peername))
    after
        socket:close(Server),
        socket:close(Client),
        socket:close(Listen)
    end.

socket_options_dynamic_test() ->
    with_socket(fun(Socket) ->
        #{domain := Domain, type := Type, protocol := Protocol} = socket:info(Socket),
        Options = observer_cli_socket:socket_options(Socket, #{
            domain => Domain,
            type => Type,
            protocol => Protocol
        }),
        ?assert(
            lists:any(fun({Key, _Value}) -> string:find(Key, "socket:") =/= nomatch end, Options)
        )
    end),
    {ok, Datagram} = socket:open(inet, dgram, udp),
    try
        Info = socket:info(Datagram),
        ?assert(is_list(observer_cli_socket:socket_options(Datagram, Info)))
    after
        socket:close(Datagram)
    end.

socket_boundary_helpers_test() ->
    ?assert(is_list(observer_cli_socket:protocol_option_specs(inet6, stream, tcp))),
    ?assert(is_list(observer_cli_socket:protocol_option_specs(inet, dgram, udp))),
    ?assert(is_list(observer_cli_socket:protocol_option_specs(inet, seqpacket, sctp))),
    ?assertEqual([], observer_cli_socket:protocol_option_specs(local, stream, default)),
    ?assertEqual({error, badarg}, observer_cli_socket:safe_getopt(not_a_socket, {socket, type})),
    ?assertEqual(
        {error, badarg}, observer_cli_socket:safe_getopt(not_a_socket, socket, type)
    ),
    ?assertEqual(
        "/tmp/observer-cli.sock",
        observer_cli_socket:sockaddr_to_list(#{family => local, path => "/tmp/observer-cli.sock"})
    ),
    ?assertEqual(
        "127.0.0.1:1883",
        observer_cli_socket:sockaddr_to_list(#{
            family => inet, addr => {127, 0, 0, 1}, port => 1883
        })
    ),
    ?assertNotEqual(
        nomatch,
        string:find(
            observer_cli_socket:sockaddr_to_list(#{
                family => inet6,
                addr => {0, 0, 0, 0, 0, 0, 0, 1},
                port => 1883,
                flowinfo => 1,
                scope_id => 2
            }),
            ",1,2"
        )
    ),
    ?assertEqual("other", observer_cli_socket:sockaddr_to_list(other)),
    ?assertEqual(0, observer_cli_socket:value_or_zero(undefined)),
    ?assertEqual(42, observer_cli_socket:value_or_zero(42)),
    Reference = make_ref(),
    ?assertEqual(ref_to_list(Reference), observer_cli_socket:format_value(Reference)),
    ?assertEqual("binary", observer_cli_socket:format_value(<<"binary">>)),
    ?assertEqual("[0]", observer_cli_socket:format_value([0])),
    ?assertEqual("{tuple}", observer_cli_socket:format_value({tuple})),
    Port = open_port({spawn, "cat"}, []),
    try
        ?assertEqual(port_to_list(Port), observer_cli_socket:format_value(Port))
    after
        port_close(Port)
    end.

with_socket(Fun) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    try
        Fun(Socket)
    after
        socket:close(Socket)
    end.

diagnostic_socket_item(Socket, Counters) ->
    #{
        raw_id => Socket,
        resource => {identifier, socket, Socket},
        domain => inet,
        type => stream,
        protocol => tcp,
        counters => Counters,
        counter_shape => lists:sort(maps:keys(Counters))
    }.

diagnostic_socket_data(Request) ->
    #{<<"data">> := Data} = diagnostic_socket_result(Request),
    Data.

diagnostic_socket_capture(Request) ->
    #{<<"meta">> := #{<<"capture">> := Capture}} = diagnostic_socket_result(Request),
    Capture.

diagnostic_socket_result(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = observer_cli_snapshot:dispatch(
        self(), sockets, Request, #{timeout_ms => 7000, identifier_policy => include}
    ),
    Result.

plain(IoData) ->
    observer_cli_test_io:plain(IoData).

general_fixture() ->
    #{
        iov_max => 1024,
        num_cnt_bits => 64,
        num_sockets => 1,
        num_monitors => 0,
        num_dinet => 1,
        num_dinet6 => 0,
        num_dlocal => 0,
        num_tstreams => 1,
        num_tdgrams => 0,
        num_tseqpkgs => 0,
        num_pip => 0,
        num_psctp => 0,
        num_ptcp => 1,
        num_pudp => 0
    }.

overview_fixture() ->
    #{
        id => fake_socket,
        id_str => "#Socket<0.1.2>",
        owner => self(),
        fd => 42,
        domain => inet,
        type => stream,
        protocol => tcp,
        counters => counter_fixture(),
        delta_counters => counter_fixture(),
        rstate => [],
        wstate => [select]
    }.

ranked_overview_fixture() ->
    {0, "fixture", maps:to_list(overview_fixture())}.

counter_fixture() ->
    #{
        acc_success => 1,
        acc_tries => 2,
        acc_waits => 3,
        acc_fails => 4,
        read_byte => 2048,
        read_pkg => 2,
        read_pkg_max => 512,
        read_waits => 5,
        read_fails => 6,
        write_byte => 1024,
        write_pkg => 1,
        write_pkg_max => 256,
        write_waits => 7,
        write_fails => 8
    }.

detail_fixture() ->
    (overview_fixture())#{
        laddress => "127.0.0.1:1234",
        raddress => "-",
        monitored_by => [self()],
        counters => maps:to_list(counter_fixture()),
        options => [{"socket:sndbuf", 131072}, {"tcp:nodelay", false}]
    }.

-endif.
