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
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, Result))),
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
        )
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
        "ow\n",
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
    end).

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
    #{<<"capture">> := Capture} = diagnostic_socket_result(Request),
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
