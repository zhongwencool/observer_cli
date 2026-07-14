-module(observer_cli_inet_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

network_raw_port_generation_test() ->
    FirstPort = open_port({spawn, "cat"}, []),
    SecondPort = open_port({spawn, "cat"}, []),
    try
        First = #{FirstPort => network_counter_item(FirstPort, 10, 20)},
        Second = #{SecondPort => network_counter_item(SecondPort, 30, 40)},
        Window = observer_cli_snapshot:counter_window(network, First, Second),
        [Born] = maps:get(items, Window),
        ?assertEqual(baseline_missing, maps:get(state, Born)),
        ?assertEqual(null, maps:get(oct, Born)),
        ?assertEqual([FirstPort], maps:get(gone, Window))
    after
        port_close(FirstPort),
        port_close(SecondPort)
    end.

network_counter_states_test() ->
    Port = open_port({spawn, "cat"}, []),
    try
        First = #{Port => network_counter_item(Port, 10, 20)},
        Reset = #{Port => network_counter_item(Port, 9, 21)},
        [ResetItem] = maps:get(items, observer_cli_snapshot:counter_window(network, First, Reset)),
        ?assertEqual(counter_reset, maps:get(state, ResetItem)),
        ?assertEqual(0, maps:get(cnt, ResetItem)),
        CountReset = (network_counter_item(Port, 11, 21))#{
            counters := #{recv_oct => 11, recv_cnt => 0, send_oct => 21, send_cnt => 3}
        },
        [CountResetItem] = maps:get(
            items, observer_cli_snapshot:counter_window(network, First, #{Port => CountReset})
        ),
        ?assertEqual(counter_reset, maps:get(state, CountResetItem)),
        ?assertEqual(null, maps:get(cnt, CountResetItem)),
        Shape = (network_counter_item(Port, 11, 22))#{
            counters := #{recv_oct => 11}, counter_shape := [recv_oct]
        },
        [ShapeItem] = maps:get(
            items, observer_cli_snapshot:counter_window(network, First, #{Port => Shape})
        ),
        ?assertEqual(shape_change, maps:get(state, ShapeItem))
    after
        port_close(Port)
    end.

network_total_and_bounded_delta_fixture_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        Total = diagnostic_data(network, #{sort => oct, limit => 20}),
        ?assertEqual(<<"total">>, maps:get(<<"sort_semantics">>, Total)),
        ?assertMatch(
            #{
                <<"input_bytes_total">> := _,
                <<"output_bytes_total">> := _,
                <<"io_bytes_total">> := _
            },
            maps:get(<<"vm_port_driver_io">>, Total)
        ),
        [Listener | _] = [
            Item
         || Item <- maps:get(<<"items">>, Total),
            maps:get(<<"resource">>, Item) =:= list_to_binary(port_to_list(Listen))
        ],
        lists:foreach(
            fun(Key) -> ?assert(maps:is_key(Key, Listener)) end,
            [
                <<"recv_cnt">>,
                <<"send_cnt">>,
                <<"cnt">>,
                <<"queue_size">>,
                <<"memory">>,
                <<"input">>,
                <<"output">>,
                <<"peername">>
            ]
        ),
        ?assertEqual(null, maps:get(<<"peername">>, Listener)),
        ?assertNot(lists:member(<<"peername">>, maps:get(<<"field_errors">>, Listener))),
        Delta = diagnostic_data(network, #{sort => oct, limit => 20, duration_ms => 250}),
        ?assertEqual(<<"delta">>, maps:get(<<"sort_semantics">>, Delta)),
        ?assertMatch(
            #{
                <<"input_bytes_delta">> := _,
                <<"output_bytes_delta">> := _,
                <<"io_bytes_delta">> := _
            },
            maps:get(<<"vm_port_driver_io">>, Delta)
        ),
        ?assert(maps:get(<<"interval_ms">>, Delta) >= 250)
    after
        gen_tcp:close(Listen)
    end.

network_port_disappearing_during_stat_read_test() ->
    Port = open_port({spawn, "cat"}, []),
    Source = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() -> {ok, [Port]} end,
        name_fun => fun(_Port) -> {ok, "tcp_inet"} end,
        stat_fun => fun(_Port) -> erlang:error(badarg) end,
        info_fun => fun(_Port, _Key) -> missing end,
        peername_fun => fun(_Port) -> {error, enotconn} end,
        io_fun => fun() -> {{input, 0}, {output, 0}} end,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> 0 end
    },
    try
        Data = diagnostic_data(network, #{sort => oct, limit => 20, test_network_source => Source}),
        ?assertEqual(1, maps:get(<<"disappeared_count">>, Data)),
        ?assertEqual([], maps:get(<<"items">>, Data))
    after
        port_close(Port)
    end.

network_resource_bad_stat_shape_is_treated_as_disappeared_test() ->
    Port = open_port({spawn, "cat"}, []),
    Source = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() -> {ok, [Port]} end,
        name_fun => fun(_Port) -> {ok, "tcp_inet"} end,
        stat_fun => fun(_Port) -> {ok, [{recv_oct, 10}, {send_oct}]} end,
        info_fun => fun(_Port, _Key) -> missing end,
        peername_fun => fun(_Port) -> {error, enotconn} end,
        io_fun => fun() -> {{input, 0}, {output, 0}} end,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> 0 end
    },
    try
        Data = diagnostic_data(network, #{sort => oct, limit => 20, test_network_source => Source}),
        ?assertEqual(1, maps:get(<<"disappeared_count">>, Data)),
        ?assertEqual([], maps:get(<<"items">>, Data))
    after
        port_close(Port)
    end.

network_count_context_and_peer_fixture_test() ->
    FirstPort = open_port({spawn, "cat"}, []),
    SecondPort = open_port({spawn, "cat"}, []),
    try
        Source = network_fixture_source(FirstPort, SecondPort),
        erase(network_fixture_second),
        Total = diagnostic_data(network, #{
            sort => recv_cnt, limit => 1, test_network_source => Source
        }),
        [Top] = maps:get(<<"items">>, Total),
        ?assertEqual(list_to_binary(port_to_list(SecondPort)), maps:get(<<"resource">>, Top)),
        ?assertEqual(20, maps:get(<<"recv_cnt">>, Top)),
        ?assertEqual(21, maps:get(<<"cnt">>, Top)),
        ?assertEqual(<<"10.0.0.1:2883">>, maps:get(<<"peername">>, Top)),
        erase(network_fixture_second),
        SendTop = diagnostic_data(network, #{
            sort => send_cnt, limit => 1, test_network_source => Source
        }),
        [SendTopItem] = maps:get(<<"items">>, SendTop),
        ?assertEqual(
            list_to_binary(port_to_list(FirstPort)), maps:get(<<"resource">>, SendTopItem)
        ),
        erase(network_fixture_second),
        PeerErrorSource = Source#{
            peername_fun := fun
                (Port) when Port =:= FirstPort -> {error, eio};
                (_Port) -> {ok, {{10, 0, 0, 1}, 2883}}
            end
        },
        All = diagnostic_data(network, #{
            sort => cnt, limit => 2, test_network_source => PeerErrorSource
        }),
        [First] = [
            Item
         || Item <- maps:get(<<"items">>, All),
            maps:get(<<"resource">>, Item) =:= list_to_binary(port_to_list(FirstPort))
        ],
        ?assertEqual(null, maps:get(<<"memory">>, First)),
        ?assertEqual([<<"memory">>, <<"peername">>], maps:get(<<"field_errors">>, First)),
        erase(network_fixture_second),
        RedactedResponse = diagnostic_response_policy(
            network, #{sort => cnt, limit => 2, test_network_source => Source}, redact
        ),
        Redacted = maps:get(<<"data">>, RedactedResponse),
        [RedactedTop, RedactedSecond] = maps:get(<<"items">>, Redacted),
        Endpoint = maps:get(<<"peername">>, RedactedTop),
        ?assertMatch(<<"endpoint-", _/binary>>, Endpoint),
        ?assertEqual(Endpoint, maps:get(<<"peername">>, RedactedSecond)),
        LeakedItem = RedactedTop#{
            <<"peername">> := #{<<"data">> => <<"10.0.0.1:2883">>}
        },
        LeakedResponse = RedactedResponse#{
            <<"data">> := Redacted#{<<"items">> := [LeakedItem, RedactedSecond]}
        },
        ?assertMatch(
            {error, invalid_command_response},
            observer_cli_escriptize:validate_response(network, redact, node(), LeakedResponse)
        ),
        erase(network_fixture_second),
        Delta = diagnostic_data(network, #{
            sort => cnt, limit => 2, duration_ms => 250, test_network_source => Source
        }),
        [DeltaTop | _] = maps:get(<<"items">>, Delta),
        ?assertEqual(list_to_binary(port_to_list(FirstPort)), maps:get(<<"resource">>, DeltaTop)),
        ?assertEqual(8, maps:get(<<"cnt">>, DeltaTop)),
        ?assertEqual(9, maps:get(<<"queue_size">>, DeltaTop)),
        ?assertEqual(7, maps:get(<<"memory">>, DeltaTop)),
        ?assertEqual(4, maps:get(<<"input">>, DeltaTop)),
        ?assertEqual(5, maps:get(<<"output">>, DeltaTop)),
        ?assertEqual(<<"10.0.0.2:3883">>, maps:get(<<"peername">>, DeltaTop)),
        EqualStats = fun(_Port) ->
            {ok, [{recv_oct, 1}, {recv_cnt, 1}, {send_oct, 1}, {send_cnt, 1}]}
        end,
        Tie = diagnostic_data(network, #{
            sort => cnt,
            limit => 2,
            test_network_source => Source#{stat_fun := EqualStats}
        }),
        ?assertEqual(
            [list_to_binary(port_to_list(FirstPort)), list_to_binary(port_to_list(SecondPort))],
            [maps:get(<<"resource">>, Item) || Item <- maps:get(<<"items">>, Tie)]
        )
    after
        erase(network_fixture_second),
        port_close(FirstPort),
        port_close(SecondPort)
    end.

network_fixture_source(FirstPort, SecondPort) ->
    #{
        count_fun => fun() -> 2 end,
        all_fun => fun() -> {ok, [FirstPort, SecondPort]} end,
        name_fun => fun(_Port) -> {ok, "tcp_inet"} end,
        stat_fun => fun(Port) ->
            Second = get(network_fixture_second) =:= true,
            case {Port, Second} of
                {FirstPort, false} ->
                    {ok, [{recv_oct, 100}, {recv_cnt, 10}, {send_oct, 20}, {send_cnt, 2}]};
                {FirstPort, true} ->
                    {ok, [{recv_oct, 130}, {recv_cnt, 15}, {send_oct, 27}, {send_cnt, 5}]};
                {SecondPort, false} ->
                    {ok, [{recv_oct, 200}, {recv_cnt, 20}, {send_oct, 10}, {send_cnt, 1}]};
                {SecondPort, true} ->
                    {ok, [{recv_oct, 201}, {recv_cnt, 21}, {send_oct, 11}, {send_cnt, 2}]}
            end
        end,
        info_fun => fun
            (Port, memory) when Port =:= FirstPort ->
                case get(network_fixture_second) of
                    true -> {ok, 7};
                    _ -> missing
                end;
            (_Port, queue_size) ->
                {ok,
                    case get(network_fixture_second) of
                        true -> 9;
                        _ -> 1
                    end};
            (_Port, input) ->
                {ok,
                    case get(network_fixture_second) of
                        true -> 4;
                        _ -> 2
                    end};
            (_Port, output) ->
                {ok,
                    case get(network_fixture_second) of
                        true -> 5;
                        _ -> 2
                    end};
            (_Port, _Key) ->
                {ok, 2}
        end,
        peername_fun => fun(_Port) ->
            case get(network_fixture_second) of
                true -> {ok, {{10, 0, 0, 2}, 3883}};
                _ -> {ok, {{10, 0, 0, 1}, 2883}}
            end
        end,
        io_fun => fun() -> {{input, 0}, {output, 0}} end,
        sleep_fun => fun(_Duration) ->
            put(network_fixture_second, true),
            ok
        end,
        monotonic_fun => fun() ->
            case get(network_fixture_second) of
                true -> 250;
                _ -> 0
            end
        end
    }.

start_manager_branches_test() ->
    Inputs = [
        "ic\n",
        "iw\n",
        "rc\n",
        "ro\n",
        "sc\n",
        "so\n",
        "cnt\n",
        "oct\n",
        "1\n",
        "\n",
        "pd\n",
        "pu\n",
        "x\n",
        "q\n"
    ],
    observer_cli_test_io:with_input(
        Inputs,
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_inet:start(Opts))
        end
    ).

start_manager_paging_branches_test() ->
    observer_cli_test_io:with_input(
        ["pd\n", "pu\n", "x\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_inet:start(Opts))
        end
    ).

trans_type_test() ->
    ?assertEqual({number, "recv_cnt", "send_cnt"}, observer_cli_inet:trans_type(cnt)),
    ?assertEqual({byte, "recv_oct", "send_oct"}, observer_cli_inet:trans_type(oct)),
    ?assertEqual({number, "recv_cnt", "cnt"}, observer_cli_inet:trans_type(send_cnt)),
    ?assertEqual({byte, "recv_oct", "oct"}, observer_cli_inet:trans_type(send_oct)),
    ?assertEqual({byte, "send_oct", "oct"}, observer_cli_inet:trans_type(recv_oct)).

trans_format_test() ->
    ?assertEqual(
        {
            {width, {byte, 1}, 10},
            {width, {byte, 2}, 10},
            {width, {byte, 3}, 10}
        },
        observer_cli_inet:trans_format(byte, 1, 2, 3)
    ),
    ?assertEqual(
        {
            {width, 1, 10},
            {width, 2, 10},
            {width, 3, 10}
        },
        observer_cli_inet:trans_format(number, 1, 2, 3)
    ).

inet_window_uses_matching_deltas_test() ->
    First = [
        {first, 110, [{recv_cnt, 10}, {send_cnt, 100}]},
        {second, 220, [{recv_cnt, 20}, {send_cnt, 200}]}
    ],
    Last = [
        {first, 215, [{recv_cnt, 15}, {send_cnt, 200}]},
        {second, 241, [{recv_cnt, 30}, {send_cnt, 211}]}
    ],
    ?assertEqual(
        [{second, 10, [{recv_cnt, 10}, {send_cnt, 11}]}],
        observer_cli_inet:inet_window(recv_cnt, 1, First, Last)
    ),
    ?assertEqual(
        [{first, 105, [{recv_cnt, 5}, {send_cnt, 100}]}],
        observer_cli_inet:inet_window(cnt, 1, First, Last)
    ).

get_menu_str_test() ->
    Count = lists:flatten(observer_cli_inet:get_menu_str(inet_count, cnt, 2000, 5)),
    Window = lists:flatten(observer_cli_inet:get_menu_str(inet_window, cnt, 2000, 5)),
    ?assert(lists:prefix("recon:inet_count", Count)),
    ?assert(lists:prefix("recon:inet_window", Window)).

title_test() ->
    Line = lists:flatten(observer_cli_inet:title(cnt, "recv", "send")),
    ?assert(string:find(Line, "Port") =/= nomatch).

add_choose_color_test() ->
    Row = [a, b],
    Chosen = observer_cli_inet:add_choose_color(1, 1, Row),
    ?assertEqual(?CHOOSE_BG, hd(Chosen)),
    ?assertEqual(Row, observer_cli_inet:add_choose_color(1, 2, Row)).

network_counter_item(Port, Recv, Send) ->
    #{
        raw_id => Port,
        resource => {identifier, port, Port},
        protocol => tcp,
        counters => #{recv_oct => Recv, recv_cnt => 1, send_oct => Send, send_cnt => 2},
        counter_shape => [recv_cnt, recv_oct, send_cnt, send_oct]
    }.

diagnostic_data(Command, Request) ->
    diagnostic_data_policy(Command, Request, include).

diagnostic_data_policy(Command, Request, Policy) ->
    maps:get(<<"data">>, diagnostic_response_policy(Command, Request, Policy)).

diagnostic_response_policy(Command, Request, Policy) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), Command, Request, #{timeout_ms => 7000, identifier_policy => Policy}
        ),
    Response.

collect_inet_info_test() ->
    ?assert(is_list(observer_cli_inet:collect_inet_info(inet_count, recv_cnt, 0, 1500, 0))),
    ?assert(is_list(observer_cli_inet:collect_inet_info(inet_window, recv_cnt, 0, 1500, 0))),
    ?assert(is_list(observer_cli_inet:collect_inet_info(inet_window, recv_cnt, 1, 1, 1))).

collect_inet_render_info_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    Opts = #inet{type = cnt, cur_page = 1, pages = [{1, 1}]},
    try
        [Row] = observer_cli_inet:collect_inet_render_info(
            [{Listen, 1, [{recv_cnt, 2}, {send_cnt, 3}]}], 1, Opts
        ),
        ?assertMatch(
            #{
                pos := 1,
                choose_pos := 1,
                port := Listen,
                value := 1,
                type1 := 2,
                type2 := 3,
                input := _,
                output := _,
                queue_size := _,
                memory := _,
                peer := _
            },
            Row
        ),
        ?assertEqual(
            lists:sort([
                choose_pos,
                input,
                memory,
                output,
                peer,
                port,
                pos,
                queue_size,
                type1,
                type2,
                value
            ]),
            lists:sort(maps:keys(Row))
        )
    after
        gen_tcp:close(Listen)
    end.

render_inet_rows_empty_test() ->
    Opts = #inet{func = inet_count, type = cnt, cur_page = 1, pages = [{1, 1}]},
    {PortList, Rows} = observer_cli_inet:render_inet_rows([], 5, Opts),
    ?assertEqual([], PortList),
    ?assert(string:find(lists:flatten(Rows), "recon:inet_count") =/= nomatch).

render_inet_rows_empty_window_test() ->
    Opts = #inet{func = inet_window, type = cnt, interval = 10, cur_page = 1, pages = [{1, 1}]},
    {PortList, Rows} = observer_cli_inet:render_inet_rows([], 5, Opts),
    ?assertEqual([], PortList),
    ?assert(string:find(lists:flatten(Rows), "recon:inet_window") =/= nomatch).

render_inet_rows_cnt_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    Opts = #inet{type = cnt, cur_page = 1, pages = [{1, 1}]},
    try
        InetInfo = observer_cli_inet:collect_inet_render_info(
            [{Listen, 1, [{recv_cnt, 2}, {send_cnt, 3}]}], 1, Opts
        ),
        {PortList, Rows} =
            observer_cli_inet:render_inet_rows(InetInfo, 1, Opts),
        ?assertEqual(1, length(PortList)),
        ?assertEqual(2, length(Rows))
    after
        gen_tcp:close(Listen)
    end.

render_inet_rows_non_cnt_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    Opts = #inet{type = recv_cnt, cur_page = 1, pages = [{1, 1}]},
    try
        InetInfo = observer_cli_inet:collect_inet_render_info([{Server, 1, []}], 1, Opts),
        {PortList, Rows} = observer_cli_inet:render_inet_rows(InetInfo, 1, Opts),
        ?assertEqual(1, length(PortList)),
        ?assertEqual(2, length(Rows))
    after
        gen_tcp:close(Client),
        gen_tcp:close(Server),
        gen_tcp:close(Listen)
    end.

render_inet_rows_directional_io_columns_test() ->
    Opts = #inet{type = recv_oct, cur_page = 1, pages = [{1, 1}]},
    Item = #{
        pos => 1,
        choose_pos => 1,
        port => test_port,
        value => 1,
        type1 => 2,
        type2 => 3,
        input => 1024,
        output => 2048,
        queue_size => 0,
        memory => 0,
        peer => "-"
    },
    {_, [_Title, Row]} = observer_cli_inet:render_inet_rows([Item], 1, Opts),
    Plain = observer_cli_test_io:plain(Row),
    ?assert(string:str(Plain, "2.0000 KiB") < string:str(Plain, "1.0000 KiB")).

render_inet_rows_octet_stat_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    Opts = #inet{type = recv_oct, cur_page = 1, pages = [{1, 1}]},
    try
        InetInfo = observer_cli_inet:collect_inet_render_info(
            [{Server, 1, [{send_oct, 2}]}], 1, Opts
        ),
        ?assertMatch([#{type1 := 2, type2 := 3}], InetInfo),
        {PortList, Rows} = observer_cli_inet:render_inet_rows(InetInfo, 1, Opts),
        ?assertEqual(1, length(PortList)),
        ?assert(string:find(lists:flatten(Rows), "recv_oct") =/= nomatch)
    after
        gen_tcp:close(Client),
        gen_tcp:close(Server),
        gen_tcp:close(Listen)
    end.

render_inet_rows_non_integer_packet_test() ->
    Port = open_port({spawn, "cat"}, [binary]),
    Opts = #inet{type = recv_cnt, cur_page = 1, pages = [{1, 1}]},
    try
        InetInfo = observer_cli_inet:collect_inet_render_info([{Port, 1, []}], 1, Opts),
        {PortList, Rows} = observer_cli_inet:render_inet_rows(InetInfo, 1, Opts),
        ?assertEqual(1, length(PortList)),
        ?assertEqual(2, length(Rows))
    after
        port_close(Port)
    end.

getstat_error_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assert(is_list(observer_cli_inet:getstat(Listen, unknown_stat)))
    after
        gen_tcp:close(Listen)
    end.

get_remote_ip_error_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        Ip = observer_cli_inet:get_remote_ip(Listen),
        ?assert(is_list(Ip))
    after
        gen_tcp:close(Listen)
    end.

get_remote_ip_success_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    try
        Ip = observer_cli_inet:get_remote_ip(Server),
        ?assert(string:find(Ip, ":") =/= nomatch)
    after
        gen_tcp:close(Client),
        gen_tcp:close(Server),
        gen_tcp:close(Listen)
    end.

render_io_rows_test() ->
    {Row, _} = observer_cli_inet:render_io_rows({0, 0}),
    ?assert(string:find(lists:flatten(Row), "Byte Input") =/= nomatch).

collect_io_info_test() ->
    {Info, {In, Out}} = observer_cli_inet:collect_io_info({0, 0}),
    ?assertMatch(
        #{
            input_delta := In,
            output_delta := Out,
            total_input := In,
            total_output := Out
        },
        Info
    ).

render_io_rows_wide_layout_test() ->
    Base = io_row_widths(80),
    Wide = io_row_widths(180),
    ?assertEqual([1, 3, 5, 7], unchanged_columns(Base, Wide, [1, 3, 5, 7])),
    ?assertEqual([2, 4, 6, 8], wider_columns(Base, Wide, [2, 4, 6, 8])).

render_inet_rows_wide_layout_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    try
        Base = inet_row_widths(Server, 80),
        Wide = inet_row_widths(Server, 180),
        ?assertEqual([1, 8], unchanged_columns(Base, Wide, [1, 8])),
        ?assertEqual(
            [2, 3, 4, 5, 6, 7, 9, 10], wider_columns(Base, Wide, [2, 3, 4, 5, 6, 7, 9, 10])
        )
    after
        gen_tcp:close(Client),
        gen_tcp:close(Server),
        gen_tcp:close(Listen)
    end.

start_new_interval_test() ->
    observer_cli_test_io:with_input(
        ["1600\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_inet:start(Opts))
        end
    ).

start_render_worker_same_interval_test() ->
    observer_cli_test_io:with_input(
        ["1500\n", {sleep, 30, "q\n"}],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_inet:start(Opts))
        end
    ).

start_inet_window_worker_test() ->
    observer_cli_test_io:with_input(
        [{sleep, 30, "q\n"}],
        fun() ->
            Opts = #view_opts{auto_row = false, inet = #inet{func = inet_window, interval = 1}},
            ?assertEqual(quit, observer_cli_inet:start(Opts))
        end
    ).

start_port_view_jump_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    StorePid = observer_cli_store:start(),
    RenderPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    observer_cli_store:update(StorePid, 1, [{1, Listen}]),
    try
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                Inet = #inet{cur_page = 1, pages = [{1, 1}]},
                Opts = #view_opts{inet = Inet, auto_row = false},
                ?assertEqual(
                    quit, observer_cli_inet:start_port_view(StorePid, RenderPid, Opts, false)
                )
            end
        )
    after
        gen_tcp:close(Listen)
    end.

start_port_view_auto_jump_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    StorePid = observer_cli_store:start(),
    RenderPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    observer_cli_store:update(StorePid, 1, [{2, Listen}]),
    try
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                Inet = #inet{cur_page = 1, pages = [{1, 1}]},
                Opts = #view_opts{inet = Inet, auto_row = false},
                ?assertEqual(
                    quit, observer_cli_inet:start_port_view(StorePid, RenderPid, Opts, true)
                )
            end
        )
    after
        gen_tcp:close(Listen)
    end.

start_port_view_missing_row_test() ->
    StorePid = observer_cli_store:start(),
    RenderPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                Inet = #inet{cur_page = 1, pages = [{1, 1}]},
                Opts = #view_opts{inet = Inet, auto_row = false},
                ?assertEqual(
                    quit, observer_cli_inet:start_port_view(StorePid, RenderPid, Opts, false)
                )
            end
        )
    after
        observer_cli_lib:exit_processes([StorePid, RenderPid])
    end.

io_row_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            {Row, _} = observer_cli_inet:render_io_rows({0, 0}),
            observer_cli_test_io:column_widths(Row)
        end
    ).

inet_row_widths(Server, Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Opts = #inet{type = recv_cnt, cur_page = 1, pages = [{1, 1}]},
            InetInfo = observer_cli_inet:collect_inet_render_info([{Server, 1, []}], 1, Opts),
            {_, [Title, Row]} = observer_cli_inet:render_inet_rows(InetInfo, 1, Opts),
            {observer_cli_test_io:column_widths(Title), observer_cli_test_io:column_widths(Row)}
        end
    ).

unchanged_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, BaseTitle) =:= lists:nth(Pos, WideTitle),
        lists:nth(Pos, BaseRow) =:= lists:nth(Pos, WideRow)
    ];
unchanged_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Base) =:= lists:nth(Pos, Wide)].

wider_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, WideTitle) > lists:nth(Pos, BaseTitle),
        lists:nth(Pos, WideRow) > lists:nth(Pos, BaseRow)
    ];
wider_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Wide) > lists:nth(Pos, Base)].

-endif.
