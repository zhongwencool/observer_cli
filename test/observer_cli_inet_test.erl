-module(observer_cli_inet_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

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

render_inet_rows_octet_stat_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
    {ok, Server} = gen_tcp:accept(Listen),
    Opts = #inet{type = recv_oct, cur_page = 1, pages = [{1, 1}]},
    try
        InetInfo = observer_cli_inet:collect_inet_render_info([{Server, 1, []}], 1, Opts),
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
