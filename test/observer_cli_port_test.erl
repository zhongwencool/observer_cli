-module(observer_cli_port_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

diagnostic_ports_use_explicit_keys_and_raw_identity_test() ->
    PortA = open_port({spawn, "cat"}, []),
    PortB = open_port({spawn, "cat"}, []),
    Parent = self(),
    Info = fun(Port, Key) ->
        Parent ! {port_info_key, Port, Key},
        diagnostic_port_field(Key)
    end,
    Source = #{
        count_fun => fun() -> 2 end,
        all_fun => fun() -> {ok, [PortA, PortB]} end,
        info_fun => Info
    },
    try
        Data = diagnostic_port_data(#{sort => io, limit => 2, test_port_source => Source}),
        Items = maps:get(<<"items">>, Data),
        ?assertEqual(2, length(Items)),
        ?assertEqual([12, 12], [maps:get(<<"io">>, Item) || Item <- Items]),
        ?assertEqual(1, length(lists:usort([maps:get(<<"display_id">>, Item) || Item <- Items]))),
        Calls = collect_port_info_calls(18, []),
        ?assertEqual(
            [connected, id, input, locking, memory, name, output, parallelism, queue_size],
            lists:usort([Key || {_Port, Key} <- Calls])
        )
    after
        port_close(PortA),
        port_close(PortB)
    end.

diagnostic_port_field(name) -> {ok, "efile"};
diagnostic_port_field(connected) -> {ok, self()};
diagnostic_port_field(queue_size) -> {ok, 3};
diagnostic_port_field(memory) -> {ok, 4};
diagnostic_port_field(id) -> {ok, 7};
diagnostic_port_field(input) -> {ok, 5};
diagnostic_port_field(output) -> {ok, 7};
diagnostic_port_field(parallelism) -> missing;
diagnostic_port_field(locking) -> missing.

collect_port_info_calls(0, Acc) ->
    Acc;
collect_port_info_calls(Count, Acc) ->
    receive
        {port_info_key, Port, Key} -> collect_port_info_calls(Count - 1, [{Port, Key} | Acc])
    after 1000 ->
        error({missing_port_info_calls, Count})
    end.

diagnostic_port_data(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := #{<<"data">> := Data}} =
        observer_cli_snapshot:dispatch(
            self(), ports, Request, #{timeout_ms => 5000, identifier_policy => include}
        ),
    Data.

start_quit_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

start_ports_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_port:start(Opts))
        end
    ).

start_ports_sort_and_page_test() ->
    observer_cli_test_io:with_input(
        ["m\n", "qs\n", "pd\n", "pu\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_port:start(Opts))
        end
    ).

start_ports_restarts_and_ignores_invalid_selection_test() ->
    observer_cli_test_io:with_input(
        ["1500\n", "x\n", "999\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_port:start(Opts))
        end
    ).

start_ports_open_detail_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        observer_cli_test_io:with_input(
            ["1\n", "q\n"],
            fun() ->
                Opts = #view_opts{auto_row = false},
                ?assertEqual(quit, observer_cli_port:start(Opts))
            end
        )
    after
        gen_tcp:close(Listen)
    end.

start_net_view_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["N\n", "q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

start_info_view_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["P\n", "q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

start_ports_view_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["O\n", "q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

start_home_view_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["H\n", "q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

start_interval_change_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["1500\n", "q\n"], Listen))
    after
        gen_tcp:close(Listen)
    end.

run_port_start(Inputs, Listen) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        process_flag(trap_exit, true),
        Result = observer_cli_test_io:with_input(
            Inputs,
            fun() ->
                Opts = #view_opts{auto_row = false},
                observer_cli_port:start(Listen, Opts)
            end
        ),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> Result
    after 5000 ->
        timeout
    end.

parse_cmd_str_test() ->
    ?assertEqual(quit, observer_cli_port:parse_cmd_str("q\n")),
    ?assertEqual(quit, observer_cli_port:parse_cmd_str("Q\n")),
    ?assertEqual(info_view, observer_cli_port:parse_cmd_str("P\n")),
    ?assertEqual(home_view, observer_cli_port:parse_cmd_str("H\n")),
    ?assertEqual(net_view, observer_cli_port:parse_cmd_str("N\n")),
    ?assertEqual(ports_view, observer_cli_port:parse_cmd_str("O\n")),
    ?assertEqual({jump, 10}, observer_cli_port:parse_cmd_str("10")),
    ?assertEqual({new_interval, 1500}, observer_cli_port:parse_cmd_str("1500")),
    ?assertEqual({input_str, "oops"}, observer_cli_port:parse_cmd_str("oops\n")),
    ?assertEqual(quit, observer_cli_port:parse_cmd_str({error, terminated})).

addr_to_str_test() ->
    ?assertEqual("127.0.0.1:4000", observer_cli_port:addr_to_str({{127, 0, 0, 1}, 4000})).

render_footer_test() ->
    Line = lists:flatten(observer_cli_port:render_footer()),
    ?assert(lists:member($q, Line)).

collect_port_info_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        Info = observer_cli_port:collect_port_info(Listen),
        ?assertEqual(
            lists:sort([links, monitored_by, monitors, port, type]),
            lists:sort(maps:keys(Info))
        ),
        ?assertMatch(
            #{
                port := #{
                    port := Listen,
                    controls := _,
                    id := _,
                    locking := _,
                    name := _,
                    os_pid := _,
                    input := _,
                    output := _,
                    memory := _,
                    parallelism := _,
                    queue_size := _,
                    slot := _,
                    connected := _
                },
                links := _,
                monitors := _,
                monitored_by := _,
                type := #{peername := _, sockname := _, statistics := _, options := _}
            },
            Info
        ),
        Port = maps:get(port, Info),
        ?assertEqual(
            lists:sort([
                connected,
                controls,
                id,
                input,
                locking,
                memory,
                name,
                os_pid,
                output,
                parallelism,
                port,
                queue_size,
                slot
            ]),
            lists:sort(maps:keys(Port))
        ),
        ?assertEqual(
            lists:sort([options, peername, sockname, statistics]),
            lists:sort(maps:keys(maps:get(type, Info)))
        ),
        ?assert(is_integer(maps:get(input, Port))),
        ?assert(is_integer(maps:get(output, Port))),
        ?assert(is_integer(maps:get(memory, Port))),
        ?assert(is_integer(maps:get(queue_size, Port))),
        ?assert(is_list(maps:get(monitored_by, Info)))
    after
        gen_tcp:close(Listen)
    end.

collect_port_type_test() ->
    ?assertEqual(
        #{
            peername => undefined,
            sockname => {{127, 0, 0, 1}, 4000},
            statistics => undefined,
            options => undefined
        },
        observer_cli_port:collect_port_type([{sockname, {{127, 0, 0, 1}, 4000}}])
    ).

collect_ports_info_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    Port = open_port({spawn, "cat"}, [binary]),
    try
        Infos = observer_cli_port:collect_ports_info(queue_size),
        ?assertNot(lists:any(fun({_, _, #{port := P}}) -> P =:= Listen end, Infos)),
        ?assert(lists:any(fun({_, _, #{port := P}}) -> P =:= Port end, Infos)),
        {_, _, PortInfo} = hd(Infos),
        ?assert(maps:is_key(controls, PortInfo)),
        ?assert(maps:is_key(slot, PortInfo)),
        ?assert(maps:is_key(parallelism, PortInfo)),
        ?assert(maps:is_key(locking, PortInfo)),
        ?assert(maps:is_key(monitored_by, PortInfo))
    after
        gen_tcp:close(Listen),
        port_close(Port)
    end.

port_runtime_helper_contract_test() ->
    ?assertNot(observer_cli_port:keep_port(dead)),
    ?assertNot(observer_cli_port:keep_port({0, 0, #{controls => "tcp_inet", name => "name"}})),
    ?assert(observer_cli_port:keep_port({0, 0, #{controls => "efile", name => "efile"}})),
    ?assertEqual(2, observer_cli_port:sort_value(memory, #{memory => 2})),
    ?assertEqual(0, observer_cli_port:sort_value(memory, #{memory => invalid})),
    ?assertEqual("controls", observer_cli_port:port_controls(invalid, [{controls, "controls"}])),
    ?assertEqual("name", observer_cli_port:port_controls(invalid, [{name, "name"}])),
    ?assertEqual("2", observer_cli_port:monitor_count([one, two])),
    ?assertEqual("0", observer_cli_port:monitor_count(invalid)),
    ?assert(is_list(observer_cli_port:monitor_to_list({process, self()}))),
    ?assert(is_list(observer_cli_port:monitor_to_list({registered, node()}))),
    ?assert(is_list(observer_cli_port:monitor_to_list(self()))),
    ?assert(is_list(observer_cli_port:monitor_to_list(other))),
    ?assertEqual([], observer_cli_port:render_option_rows([], [10, 10, 10, 10, 10, 10, 10, 10])),
    ?assertNotEqual(
        [],
        observer_cli_port:render_option_rows(
            [{active, false}], [10, 10, 10, 10, 10, 10, 10, 10]
        )
    ),
    Port = open_port({spawn, "cat"}, [binary]),
    try
        ?assertMatch({0, _, #{port := Port}}, observer_cli_port:port_overview(Port, memory))
    after
        port_close(Port)
    end,
    ?assertEqual(dead, observer_cli_port:port_overview(Port, memory)),
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assert(is_list(observer_cli_port:collect_sock_opts(Listen)))
    after
        gen_tcp:close(Listen)
    end,
    ?assert(is_list(observer_cli_port:collect_sock_opts(invalid_port))),
    ?assertEqual(
        {active, true},
        observer_cli_port:collect_sock_opt_result(
            active, {ok, [{active, true}]}
        )
    ),
    ?assertEqual({active, "-"}, observer_cli_port:collect_sock_opt_result(active, {ok, []})),
    ?assertEqual(
        {active, "Not Supported"},
        observer_cli_port:collect_sock_opt_result(active, {error, einval})
    ),
    ?assertMatch(
        {active, [$e, $r, $r, $o, $r, $: | _]},
        observer_cli_port:collect_sock_opt_result(active, {error, failed})
    ).

select_port_test() ->
    Port0 = open_port({spawn, "cat"}, [binary]),
    try
        {ok, Port} = observer_cli_port:select_port(1, #ports{}),
        ?assert(is_port(Port)),
        ?assertEqual(error, observer_cli_port:select_port(0, #ports{}))
    after
        port_close(Port0)
    end.

render_ports_info_test() ->
    PortInfo = #{
        port => self(),
        id => 1,
        connected => self(),
        name => "tcp_inet",
        controls => "tcp_inet",
        slot => undefined,
        parallelism => false,
        locking => port_level,
        queue_size => 0,
        memory => 48,
        monitors => [],
        monitored_by => [self()]
    },
    Rows = observer_cli_port:render_ports_info({1, [{0, 0, PortInfo}]}, queue_size),
    MemoryRows = observer_cli_port:render_ports_info({1, [{0, 0, PortInfo}]}, memory),
    Text = lists:flatten(Rows),
    ?assert(string:find(lists:flatten(MemoryRows), "Memory") =/= nomatch),
    ?assert(string:find(Text, "Controls") =/= nomatch),
    ?assert(string:find(Text, "Mon/By") =/= nomatch).

render_ports_info_wide_layout_test() ->
    PortInfo = #{
        port => self(),
        id => 1,
        connected => self(),
        name => "/Users/zw/.local/share/mise/install/long-driver-name",
        controls => "/Users/zw/.local/share/mise/install/long-driver-controls",
        slot => undefined,
        parallelism => false,
        locking => port_level,
        queue_size => 0,
        memory => 48,
        monitors => [],
        monitored_by => []
    },
    observer_cli_test_io:with_geometry(
        24,
        205,
        [],
        fun() ->
            Rows = observer_cli_port:render_ports_info({1, [{0, 0, PortInfo}]}, queue_size),
            LayoutWidth = observer_cli_lib:layout_width(),
            ?assert(
                lists:all(
                    fun(Width) -> Width =< LayoutWidth end, observer_cli_test_io:line_widths(Rows)
                )
            ),
            [TitleColumns | RowColumns] = observer_cli_test_io:line_column_widths(Rows),
            ?assert(lists:all(fun(Columns) -> Columns =:= TitleColumns end, RowColumns))
        end
    ).

render_port_info_test() ->
    PortView = #{
        port => self(),
        id => "id",
        name => "name",
        controls => "tcp_inet",
        slot => undefined,
        parallelism => false,
        locking => port_level,
        os_pid => 123,
        input => 10,
        output => 20,
        memory => 30,
        queue_size => 0,
        connected => connected
    },
    [Title, Rows] = observer_cli_port:render_port_info(PortView),
    ?assert(string:find(lists:flatten(Title), "Attr") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "port") =/= nomatch).

render_port_info_queue_size_test() ->
    PortView = #{
        port => self(),
        id => "id",
        name => "name",
        controls => "tcp_inet",
        slot => undefined,
        parallelism => false,
        locking => port_level,
        os_pid => 123,
        input => 10,
        output => 20,
        memory => 30,
        queue_size => 1,
        connected => connected
    },
    [Title, Rows] = observer_cli_port:render_port_info(PortView),
    ?assert(string:find(lists:flatten(Title), "Attr") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "queue_size") =/= nomatch).

render_port_sections_test() ->
    Type = type_fixture(),
    Detail = #{
        port => port_view(),
        links => [self()],
        monitors => [{process, self()}],
        monitored_by => [self()],
        type => Type
    },
    Expected = [
        observer_cli_port:render_port_info(port_view()),
        observer_cli_port:render_link_monitor([self()], [{process, self()}], [self()]),
        observer_cli_port:render_socket_peer(Type),
        observer_cli_port:render_stats(stats_fixture()),
        observer_cli_port:render_opts(opts_fixture())
    ],
    ?assertEqual(
        lists:flatten(Expected), lists:flatten(observer_cli_port:render_port_sections(Detail))
    ).

render_port_info_wide_layout_test() ->
    Base = port_info_widths(80),
    Wide = port_info_widths(180),
    ?assertEqual([1, 3, 5], unchanged_columns(Base, Wide, [1, 3, 5])),
    ?assertEqual([2, 4, 6], wider_columns(Base, Wide, [2, 4, 6])).

render_port_info_wide_alignment_test() ->
    {Title, Rows} = port_info_columns(205),
    ?assert(lists:all(fun(Row) -> Row =:= Title end, Rows)).

render_link_monitor_test() ->
    Line = observer_cli_port:render_link_monitor([self()], [{process, self()}], [self()]),
    Text = lists:flatten(Line),
    ?assert(string:find(Text, "Links(") =/= nomatch),
    ?assert(string:find(Text, "Monitored by(") =/= nomatch).

render_link_monitor_named_test() ->
    Line = observer_cli_port:render_link_monitor([self()], [{reg_name, node()}]),
    ?assert(string:find(lists:flatten(Line), "/") =/= nomatch).

render_link_monitor_wide_layout_test() ->
    Base = port_link_widths(80),
    Wide = port_link_widths(180),
    ?assert(
        lists:all(
            fun({BaseLine, WideLine}) ->
                lists:nth(1, BaseLine) =:= lists:nth(1, WideLine) andalso
                    lists:nth(2, WideLine) > lists:nth(2, BaseLine)
            end,
            lists:zip(Base, Wide)
        )
    ).

render_link_monitor_wide_alignment_test() ->
    [First | Rest] = port_link_widths(205),
    ?assert(lists:all(fun(Row) -> Row =:= First end, Rest)).

render_type_line_test() ->
    Line = observer_cli_port:render_type_line(type_fixture()),
    ?assert(string:find(lists:flatten(Line), "peername") =/= nomatch).

render_type_line_missing_fields_test() ->
    Line = observer_cli_port:render_type_line(type_missing_fields_fixture()),
    ?assert(string:find(lists:flatten(Line), "sockname") =/= nomatch).

sock_opts_observer_options_test() ->
    Opts = observer_cli_port:sock_opts(),
    [
        ?assert(lists:member(Opt, Opts))
     || Opt <- [
            bind_to_device,
            deliver,
            high_msgq_watermark,
            ipv6_v6only,
            low_msgq_watermark,
            netns,
            read_packets,
            send_timeout_close,
            show_econnreset,
            tos,
            tclass
        ]
    ].

render_type_line_wide_layout_test() ->
    Base = type_line_widths(80),
    Wide = type_line_widths(180),
    ?assert(lists:nth(1, Wide) > lists:nth(1, Base)),
    ?assertEqual(lists:nth(2, Base), lists:nth(2, Wide)),
    ?assert(lists:nth(3, Wide) > lists:nth(3, Base)).

render_stats_wide_layout_test() ->
    Base = stats_widths(80),
    Wide = stats_widths(180),
    ?assertEqual([1, 3, 5, 7, 9], same_columns(Base, Wide, [1, 3, 5, 7, 9])),
    ?assertEqual([2, 4, 6, 8], wider_columns(Base, Wide, [2, 4, 6, 8])).

render_stats_wide_alignment_test() ->
    [First | Rest] = stats_columns(205),
    ?assert(lists:all(fun(Row) -> Row =:= First end, Rest)).

render_opts_wide_layout_test() ->
    {BaseTitle, BaseRow} = opts_widths(80),
    {WideTitle, WideRow} = opts_widths(180),
    ?assertEqual(erlang:length(BaseTitle), erlang:length(WideTitle)),
    ?assert(lists:all(fun({Base, Wide}) -> Wide >= Base end, lists:zip(BaseTitle, WideTitle))),
    ?assert(lists:all(fun({Base, Wide}) -> Wide >= Base end, lists:zip(BaseRow, WideRow))).

render_opts_dynamic_options_test() ->
    [_Title, Rows] = observer_cli_port:render_opts(opts_fixture()),
    Text = lists:flatten(Rows),
    ?assert(string:find(Text, "bind_to_device") =/= nomatch),
    ?assert(string:find(Text, "send_timeout_close") =/= nomatch),
    ?assert(string:find(Text, "show_econnreset") =/= nomatch).

render_opts_wide_alignment_test() ->
    {Title, Rows} = opts_columns(205),
    ?assert(lists:all(fun(Row) -> Row =:= Title end, Rows)).

render_info_page_wide_border_alignment_test() ->
    {LayoutWidth, LineWidths} = port_info_page_line_widths(205),
    ?assert(lists:all(fun(Width) -> Width =:= LayoutWidth end, LineWidths)).

port_detail_golden_output_fragments_test() ->
    observer_cli_test_io:with_geometry(
        24,
        205,
        [],
        fun() ->
            Output = [
                observer_cli_port:render_menu(info, 1500),
                observer_cli_port:render_port_info(port_view()),
                observer_cli_port:render_link_monitor([self()], [{process, self()}]),
                observer_cli_port:render_type_line(type_fixture()),
                observer_cli_port:render_footer()
            ],
            observer_cli_test_io:assert_stable_fragments(Output, [
                "Home(H)",
                "Network(N)",
                "Ports(O)",
                "Port Info(P)",
                "Interval: 1500ms",
                "Attr",
                "Value",
                "queue_size",
                "controls",
                "connected",
                "Links(1)",
                "Monitors(1)",
                "Monitored by(",
                "sockname",
                "peername",
                "recv_cnt",
                "send_pend",
                "Option",
                "bind_to_device",
                "mode",
                "packet",
                "q(quit)"
            ]),
            observer_cli_test_io:assert_ansi_boundaries(Output)
        end
    ).

port_dead_golden_output_fragments_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    ok = gen_tcp:close(Listen),
    {_Result, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        205,
        [],
        fun() -> observer_cli_port:output_die_view(Listen, 1500) end
    ),
    observer_cli_test_io:assert_stable_fragments(Output, [
        "Port Info(P)",
        "Port(",
        "has already died.",
        "q(quit)"
    ]),
    observer_cli_test_io:assert_ansi_boundaries(Output).

render_menu_test() ->
    Line = observer_cli_port:render_menu(info, 1000),
    ?assert(string:find(lists:flatten(Line), "Interval: 1000ms") =/= nomatch).

port_info_widths(Columns) ->
    {Title, [FirstRow | _]} = port_info_columns(Columns),
    {Title, FirstRow}.

port_info_columns(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Rows] = observer_cli_port:render_port_info(port_view()),
            {
                observer_cli_test_io:column_widths(Title),
                observer_cli_test_io:line_column_widths(Rows)
            }
        end
    ).

port_link_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Line = observer_cli_port:render_link_monitor([self()], [{process, self()}]),
            observer_cli_test_io:line_column_widths(Line)
        end
    ).

type_line_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Line = observer_cli_port:render_type_line(type_width_fixture()),
            observer_cli_test_io:column_widths(Line)
        end
    ).

stats_widths(Columns) ->
    [FirstRow | _] = stats_columns(Columns),
    FirstRow.

stats_columns(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Line = observer_cli_port:render_stats(stats_fixture()),
            observer_cli_test_io:line_column_widths(Line)
        end
    ).

opts_widths(Columns) ->
    {Title, [FirstRow | _]} = opts_columns(Columns),
    {Title, FirstRow}.

opts_columns(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Rows] = observer_cli_port:render_opts(opts_fixture()),
            {
                observer_cli_test_io:column_widths(Title),
                observer_cli_test_io:line_column_widths(Rows)
            }
        end
    ).

port_info_page_line_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            IoData = [
                observer_cli_port:render_menu(info, 1500),
                observer_cli_port:render_port_info(port_view()),
                observer_cli_port:render_link_monitor([self()], [{process, self()}]),
                observer_cli_port:render_type_line(type_fixture()),
                observer_cli_port:render_footer()
            ],
            {observer_cli_lib:layout_width(), observer_cli_test_io:line_widths(IoData)}
        end
    ).

port_view() ->
    #{
        port => self(),
        id => "id",
        name => "very_long_port_name_for_layout",
        controls => "tcp_inet",
        slot => undefined,
        parallelism => false,
        locking => port_level,
        os_pid => 123,
        input => 10,
        output => 20,
        memory => 30,
        queue_size => 0,
        connected => connected
    }.

stats_fixture() ->
    [
        {recv_oct, 1},
        {recv_cnt, 2},
        {recv_max, 3},
        {recv_avg, 4},
        {recv_dvi, 5},
        {send_oct, 6},
        {send_cnt, 7},
        {send_max, 8},
        {send_avg, 9},
        {send_pend, 10}
    ].

opts_fixture() ->
    [
        {active, false},
        {broadcast, false},
        {buffer, 0},
        {bind_to_device, "Not Supported"},
        {delay_send, false},
        {deliver, term},
        {dontroute, false},
        {exit_on_close, true},
        {header, 0},
        {high_msgq_watermark, 10},
        {high_watermark, 10},
        {ipv6_v6only, "-"},
        {keepalive, false},
        {linger, {false, 0}},
        {low_msgq_watermark, 0},
        {low_watermark, 0},
        {mode, binary},
        {netns, "Not Supported"},
        {nodelay, false},
        {packet, 0},
        {packet_size, 0},
        {priority, 0},
        {read_packets, "-"},
        {recbuf, 0},
        {reuseaddr, false},
        {send_timeout, 0},
        {send_timeout_close, false},
        {show_econnreset, false},
        {sndbuf, 0},
        {tos, 0},
        {tclass, "-"}
    ].

type_fixture() ->
    observer_cli_port:collect_port_type([
        {peername, {{127, 0, 0, 1}, 4369}},
        {sockname, {{127, 0, 0, 1}, 58521}},
        {statistics, stats_fixture()},
        {options, opts_fixture()}
    ]).

type_missing_fields_fixture() ->
    observer_cli_port:collect_port_type([{sockname, {{127, 0, 0, 1}, 4000}}]).

type_width_fixture() ->
    observer_cli_port:collect_port_type([
        {peername, {{127, 0, 0, 1}, 4000}},
        {sockname, {{0, 0, 0, 0}, 0}}
    ]).

unchanged_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, BaseTitle) =:= lists:nth(Pos, WideTitle),
        lists:nth(Pos, BaseRow) =:= lists:nth(Pos, WideRow)
    ].

wider_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, WideTitle) > lists:nth(Pos, BaseTitle),
        lists:nth(Pos, WideRow) > lists:nth(Pos, BaseRow)
    ];
wider_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Wide) > lists:nth(Pos, Base)].

same_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Base) =:= lists:nth(Pos, Wide)].

-endif.
