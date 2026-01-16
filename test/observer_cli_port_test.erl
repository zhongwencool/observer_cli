-module(observer_cli_port_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_test() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
    try
        ?assertEqual(quit, run_port_start(["q\n"], Listen))
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
    ?assertEqual({new_interval, 1500}, observer_cli_port:parse_cmd_str("1500")).

addr_to_str_test() ->
    ?assertEqual("127.0.0.1:4000", observer_cli_port:addr_to_str({{127, 0, 0, 1}, 4000})).

render_last_line_test() ->
    Line = lists:flatten(observer_cli_port:render_last_line()),
    ?assert(lists:member($q, Line)).

render_port_info_test() ->
    [Title, Rows] = observer_cli_port:render_port_info(
        self(),
        "id",
        "name",
        123,
        10,
        20,
        30,
        0,
        connected
    ),
    ?assert(string:find(lists:flatten(Title), "Attr") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "port") =/= nomatch).

render_port_info_queue_size_test() ->
    [Title, Rows] = observer_cli_port:render_port_info(
        self(),
        "id",
        "name",
        123,
        10,
        20,
        30,
        1,
        connected
    ),
    ?assert(string:find(lists:flatten(Title), "Attr") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "queue_size") =/= nomatch).

render_link_monitor_test() ->
    Line = observer_cli_port:render_link_monitor([self()], [{process, self()}]),
    ?assert(string:find(lists:flatten(Line), "Links(") =/= nomatch).

render_link_monitor_named_test() ->
    Line = observer_cli_port:render_link_monitor([self()], [{reg_name, node()}]),
    ?assert(string:find(lists:flatten(Line), "/") =/= nomatch).

render_type_line_test() ->
    Stats = [
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
    ],
    Opts = [
        {active, false},
        {broadcast, false},
        {buffer, 0},
        {delay_send, false},
        {dontroute, false},
        {exit_on_close, true},
        {header, 0},
        {high_watermark, 10},
        {keepalive, false},
        {linger, {false, 0}},
        {low_watermark, 0},
        {mode, binary},
        {nodelay, false},
        {packet, 0},
        {packet_size, 0},
        {priority, 0},
        {recbuf, 0},
        {reuseaddr, false},
        {send_timeout, 0},
        {sndbuf, 0}
    ],
    Line = observer_cli_port:render_type_line([
        {peername, {{127, 0, 0, 1}, 4000}},
        {sockname, {{0, 0, 0, 0}, 0}},
        {statistics, Stats},
        {options, Opts}
    ]),
    ?assert(string:find(lists:flatten(Line), "peername") =/= nomatch).

render_type_line_missing_fields_test() ->
    Line = observer_cli_port:render_type_line([
        {sockname, {{127, 0, 0, 1}, 4000}}
    ]),
    ?assert(string:find(lists:flatten(Line), "sockname") =/= nomatch).

render_menu_test() ->
    Line = observer_cli_port:render_menu(info, 1000),
    ?assert(string:find(lists:flatten(Line), "Interval: 1000ms") =/= nomatch).

-endif.
