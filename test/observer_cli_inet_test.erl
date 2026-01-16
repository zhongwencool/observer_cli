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

render_inet_rows_empty_test() ->
    Opts = #inet{func = inet_count, type = cnt, cur_page = 1, pages = [{1, 1}]},
    {PortList, Rows} = observer_cli_inet:render_inet_rows([], 5, Opts),
    ?assertEqual([], PortList),
    ?assert(string:find(lists:flatten(Rows), "recon:inet_count") =/= nomatch).

render_inet_rows_cnt_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    Opts = #inet{type = cnt, cur_page = 1, pages = [{1, 1}]},
    try
        {PortList, Rows} =
            observer_cli_inet:render_inet_rows(
                [{Listen, 1, [{recv_cnt, 2}, {send_cnt, 3}]}], 1, Opts
            ),
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
        {PortList, Rows} = observer_cli_inet:render_inet_rows([{Server, 1, []}], 1, Opts),
        ?assertEqual(1, length(PortList)),
        ?assertEqual(2, length(Rows))
    after
        gen_tcp:close(Client),
        gen_tcp:close(Server),
        gen_tcp:close(Listen)
    end.

render_inet_rows_non_integer_packet_test() ->
    Port = open_port({spawn, "cat"}, [binary]),
    Opts = #inet{type = recv_cnt, cur_page = 1, pages = [{1, 1}]},
    try
        {PortList, Rows} = observer_cli_inet:render_inet_rows([{Port, 1, []}], 1, Opts),
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

start_new_interval_test() ->
    observer_cli_test_io:with_input(
        ["1600\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
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

-endif.
