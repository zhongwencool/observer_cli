-module(observer_cli_lib_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

select_unselect_test() ->
    Selected = observer_cli_lib:selected_menu_item("Home"),
    Unselected = observer_cli_lib:unselected_menu_item("Home"),
    ?assert(string:find(lists:flatten(Selected), "Home") =/= nomatch),
    ?assert(string:find(lists:flatten(Unselected), "Home") =/= nomatch),
    ?assertEqual(Selected, observer_cli_lib:select("Home")),
    ?assertEqual(Unselected, observer_cli_lib:unselect("Home")).

render_menu_test() ->
    application:set_env(observer_cli, default_row_size, 20),
    Line = observer_cli_lib:render_top_menu(home, "Test"),
    ?assert(string:find(lists:flatten(Line), "Home") =/= nomatch),
    ?assertEqual(Line, observer_cli_lib:render_menu(home, "Test")).

render_footer_test() ->
    Line = observer_cli_lib:render_footer("q(quit)"),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch),
    ?assertEqual(Line, observer_cli_lib:render_last_line("q(quit)")).

render_keeps_unicode_text_test() ->
    Line = observer_cli_lib:render([?W("中文", 10)]),
    ?assert(string:find(Line, "中文") =/= nomatch).

layout_width_keeps_base_width_test() ->
    BaseWidth = observer_cli_lib:layout_base_width(),
    [
        assert_layout_width(Columns, BaseWidth, 0)
     || Columns <- [20, 80, BaseWidth, BaseWidth + 1]
    ].

layout_width_uses_wide_terminal_test() ->
    BaseWidth = observer_cli_lib:layout_base_width(),
    observer_cli_test_io:with_geometry(
        24,
        160,
        [],
        fun() ->
            ?assertEqual(159, observer_cli_lib:layout_width()),
            ?assertEqual(20, observer_cli_lib:layout_extra_width()),
            ?assertEqual(20, observer_cli_lib:layout_extra_width(159, BaseWidth)),
            ?assertEqual(
                159, observer_cli_lib:visible_length(observer_cli_lib:render_footer("q"))
            )
        end
    ).

assert_layout_width(Columns, ExpectedWidth, ExpectedExtra) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            ?assertEqual(ExpectedWidth, observer_cli_lib:layout_width()),
            ?assertEqual(ExpectedExtra, observer_cli_lib:layout_extra_width())
        end
    ).

get_terminal_rows_test() ->
    application:set_env(observer_cli, default_row_size, 20),
    ?assertEqual(20, observer_cli_lib:get_terminal_rows(false)),
    observer_cli_test_io:with_input(
        [],
        fun() ->
            ?assertEqual(24, observer_cli_lib:get_terminal_rows(true))
        end
    ).

parse_cmd_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_lib:parse_cmd(Opts, ?MODULE, []))
        end
    ).

parse_cmd_top_menu_routes_test() ->
    [
        route_shared_command(Cmd)
     || Cmd <- ["H\n", "S\n", "A\n", "N\n", "O\n", "M\n", "E\n", "D\n", "P\n"]
    ].

parse_cmd_top_menu_exits_plain_pids_test() ->
    Pid = spawn(fun wait_forever/0),
    route_shared_command("D\n", ?MODULE, [Pid]),
    assert_dead(Pid).

parse_cmd_top_menu_keeps_home_cleanup_test() ->
    RenderPid = spawn(fun wait_forever/0),
    StorePid = spawn(fun wait_forever/0),
    route_shared_command("D\n", observer_cli, [RenderPid, StorePid, false, ?DISABLE]),
    assert_dead(RenderPid),
    assert_dead(StorePid).

route_shared_command(Cmd) ->
    route_shared_command(Cmd, observer_cli_help, []).

route_shared_command(Cmd, Module, Args) ->
    PrevTrap = process_flag(trap_exit, true),
    try
        observer_cli_test_io:with_input(
            [Cmd, "q\n"],
            fun() ->
                Opts = #view_opts{auto_row = false},
                ?assertEqual(quit, observer_cli_lib:parse_cmd(Opts, Module, Args))
            end
        )
    after
        process_flag(trap_exit, PrevTrap)
    end.

wait_forever() ->
    receive
    after infinity -> ok
    end.

assert_dead(Pid) ->
    ?assertEqual(false, erlang:is_process_alive(Pid)).

to_percent_test() ->
    ?assertEqual("05.00%", lists:flatten(observer_cli_lib:to_percent(0.05))),
    ?assertEqual("50.00%", lists:flatten(observer_cli_lib:to_percent(0.5))),
    ?assertEqual("******", lists:flatten(observer_cli_lib:to_percent(undefined))),
    ?assertEqual("100.0%", lists:flatten(observer_cli_lib:to_percent(2))).

to_list_test() ->
    ?assertEqual("ok", observer_cli_lib:to_list(ok)),
    ?assertEqual("1", observer_cli_lib:to_list(1)),
    PidStr = observer_cli_lib:to_list(self()),
    ?assert(lists:prefix("<", PidStr)),
    RefStr = observer_cli_lib:to_list(make_ref()),
    ?assert(lists:prefix("#Ref<", RefStr)),
    ?assertEqual("abc", observer_cli_lib:to_list(<<"abc">>)),
    ?assertEqual("1.2345", observer_cli_lib:to_list(1.2345)),
    ?assertEqual([1, 2], observer_cli_lib:to_list([1, 2])).

green_test() ->
    ?assertEqual(<<"\e[32;1mok\e[0m">>, iolist_to_binary(observer_cli_lib:ansi_green("ok"))),
    ?assertEqual(<<"\e[31mfail\e[0m">>, iolist_to_binary(observer_cli_lib:ansi_red("fail"))),
    ?assertEqual("\e[32;1mok\e[0m", observer_cli_lib:green("ok")).

to_byte_test() ->
    ?assertEqual("10 B", lists:flatten(observer_cli_lib:to_byte(10))),
    ?assertEqual("2.0000 KiB", lists:flatten(observer_cli_lib:to_byte(2048))),
    ?assertEqual("1.0000 MiB", lists:flatten(observer_cli_lib:to_byte(1024 * 1024))).

to_byte_fallback_test() ->
    ?assertEqual("oops", lists:flatten(observer_cli_lib:to_byte("oops"))).

mfa_to_list_test() ->
    ?assertEqual(
        "mod:fun_name/2",
        lists:flatten(observer_cli_lib:mfa_to_list({mod, fun_name, 2}))
    ),
    ?assertEqual("123", lists:flatten(observer_cli_lib:mfa_to_list(123))).

command_parse_integer_test() ->
    ?assertEqual({new_interval, 1500}, observer_cli_command:parse_integer("1500")),
    ?assertEqual({jump, 10}, observer_cli_command:parse_integer("10")),
    ?assertEqual({input_str, "abc"}, observer_cli_command:parse_integer("abc")),
    ?assertEqual({input_str, "-1"}, observer_cli_command:parse_integer("-1")),
    ?assertEqual({jump, 10}, observer_cli_lib:parse_integer("10")).

pad_rendered_trims_reset_before_border_test() ->
    Line = observer_cli_lib:pad_rendered([<<"value">>, ?RESET, <<" |">>]),
    ?assertNotEqual(nomatch, string:find(Line, "value")),
    ?assertEqual(nomatch, binary:match(unicode:characters_to_binary(Line), <<"\e[0m |">>)).

shared_parse_cmd_str_test() ->
    ?assertEqual(home_view, observer_cli_command:parse_shared("H\n")),
    ?assertEqual(system_view, observer_cli_command:parse_shared("S\n")),
    ?assertEqual(app_view, observer_cli_command:parse_shared("A\n")),
    ?assertEqual(inet_view, observer_cli_command:parse_shared("N\n")),
    ?assertEqual(ports_view, observer_cli_command:parse_shared("O\n")),
    ?assertEqual(sockets_view, observer_cli_command:parse_shared("K\n")),
    ?assertEqual(mnesia_view, observer_cli_command:parse_shared("M\n")),
    ?assertEqual(ets_view, observer_cli_command:parse_shared("E\n")),
    ?assertEqual(help_view, observer_cli_command:parse_shared("D\n")),
    ?assertEqual(plugin_view, observer_cli_command:parse_shared("P\n")),
    ?assertEqual(inet_count, observer_cli_command:parse_shared("ic\n")),
    ?assertEqual(inet_window, observer_cli_command:parse_shared("iw\n")),
    ?assertEqual(recv_cnt, observer_cli_command:parse_shared("rc\n")),
    ?assertEqual(recv_oct, observer_cli_command:parse_shared("ro\n")),
    ?assertEqual(send_cnt, observer_cli_command:parse_shared("sc\n")),
    ?assertEqual(send_oct, observer_cli_command:parse_shared("so\n")),
    ?assertEqual(cnt, observer_cli_command:parse_shared("cnt\n")),
    ?assertEqual(oct, observer_cli_command:parse_shared("oct\n")),
    ?assertEqual(queue_size, observer_cli_command:parse_shared("qs\n")),
    ?assertEqual(socket_io, observer_cli_command:parse_shared("io\n")),
    ?assertEqual(socket_read_byte, observer_cli_command:parse_shared("rb\n")),
    ?assertEqual(socket_write_byte, observer_cli_command:parse_shared("wb\n")),
    ?assertEqual(socket_packets, observer_cli_command:parse_shared("pk\n")),
    ?assertEqual(socket_waits, observer_cli_command:parse_shared("wt\n")),
    ?assertEqual(socket_fails, observer_cli_command:parse_shared("fl\n")),
    ?assertEqual(socket_max_packet, observer_cli_command:parse_shared("mx\n")),
    ?assertEqual(socket_accepts, observer_cli_command:parse_shared("ac\n")),
    ?assertEqual(socket_id, observer_cli_command:parse_shared("id\n")),
    ?assertEqual(socket_fd, observer_cli_command:parse_shared("fd\n")),
    ?assertEqual(socket_owner, observer_cli_command:parse_shared("ow\n")),
    ?assertEqual(socket_domain, observer_cli_command:parse_shared("dm\n")),
    ?assertEqual(socket_type, observer_cli_command:parse_shared("tp\n")),
    ?assertEqual(socket_protocol, observer_cli_command:parse_shared("pt\n")),
    ?assertEqual(page_up_top_n, observer_cli_command:parse_shared("pu\n")),
    ?assertEqual(page_down_top_n, observer_cli_command:parse_shared("pd\n")),
    ?assertEqual(page_up_top_n, observer_cli_command:parse_shared("PU\n")),
    ?assertEqual(page_down_top_n, observer_cli_command:parse_shared("PD\n")),
    ?assertEqual(page_up_top_n, observer_cli_command:parse_shared("B\n")),
    ?assertEqual(page_down_top_n, observer_cli_command:parse_shared("F\n")),
    ?assertEqual(size, observer_cli_command:parse_shared("s\n")),
    ?assertEqual(hide, observer_cli_command:parse_shared("hide\n")),
    ?assertEqual(quit, observer_cli_command:parse_shared({error, estale})).

home_parse_cmd_str_test() ->
    assert_shared_parse([
        {"m\n", {func, proc_count, memory}},
        {"r\n", {func, proc_count, reductions}},
        {"b\n", {func, proc_count, binary_memory}},
        {"t\n", {func, proc_count, total_heap_size}},
        {"mq\n", {func, proc_count, message_queue_len}},
        {"rr\n", {func, proc_window, reductions}},
        {"bb\n", {func, proc_window, binary_memory}},
        {"tt\n", {func, proc_window, total_heap_size}},
        {"mm\n", {func, proc_window, memory}},
        {"mmq\n", {func, proc_window, message_queue_len}},
        {"\n", jump},
        {"10", {jump, 10}},
        {"oops\n", {input_str, "oops"}},
        {"`\n", scheduler_usage},
        {"<0.0.0>\n", {go_to_pid, list_to_pid("<0.0.0>")}},
        {">12\n", {go_to_pid, list_to_pid("<0.12.0>")}},
        {">\n", quit}
    ]).

assert_shared_parse(Cases) ->
    lists:foreach(
        fun({Command, Expected}) ->
            ?assertEqual(Expected, observer_cli_command:parse_shared(Command))
        end,
        Cases
    ).

shared_parse_interval_test() ->
    ?assertEqual({new_interval, 1500}, observer_cli_command:parse_shared("1500")).

network_command_parse_test() ->
    assert_shared_parse([
        {"ic\n", inet_count},
        {"iw\n", inet_window},
        {"rc\n", recv_cnt},
        {"ro\n", recv_oct},
        {"sc\n", send_cnt},
        {"so\n", send_oct},
        {"cnt\n", cnt},
        {"oct\n", oct},
        {"1500", {new_interval, 1500}},
        {"q\n", quit},
        {"Q\n", quit},
        {{error, terminated}, quit}
        | pagination_command_cases()
    ]).

ets_mnesia_command_parse_test() ->
    assert_shared_parse([
        {"s\n", size},
        {"m\n", {func, proc_count, memory}},
        {"hide\n", hide},
        {"1500", {new_interval, 1500}},
        {"q\n", quit},
        {"Q\n", quit}
        | pagination_command_cases()
    ]).

application_command_parse_test() ->
    assert_shared_parse([
        {"p\n", pause_or_resume},
        {"r\n", {func, proc_count, reductions}},
        {"m\n", {func, proc_count, memory}},
        {"mq\n", {func, proc_count, message_queue_len}},
        {"1500", {new_interval, 1500}},
        {"q\n", quit},
        {"Q\n", quit}
        | pagination_command_cases()
    ]).

pagination_command_cases() ->
    [
        {"pd\n", page_down_top_n},
        {"pu\n", page_up_top_n},
        {"PD\n", page_down_top_n},
        {"PU\n", page_up_top_n},
        {"F\n", page_down_top_n},
        {"B\n", page_up_top_n}
    ].

weighted_widths_edge_test() ->
    ?assertEqual([], observer_cli_lib:weighted_widths([], [])),
    ?assertEqual([10, 20], observer_cli_lib:weighted_widths([10, 20], [0, 0])),
    observer_cli_test_io:with_geometry(
        24,
        150,
        [],
        fun() ->
            ?assertEqual([14, 26, 30], observer_cli_lib:weighted_widths([10, 20, 30], [1, 2, 0]))
        end
    ).

pad_rendered_plain_text_test() ->
    ?assertEqual([], observer_cli_lib:pad_rendered("")),
    ?assertEqual("plain", observer_cli_lib:pad_rendered("plain")).

pad_rendered_boundary_lines_test() ->
    observer_cli_test_io:with_geometry(
        24,
        160,
        [],
        fun() ->
            LayoutWidth = observer_cli_lib:layout_width(),
            FullLine = ["|", lists:duplicate(LayoutWidth - 2, $x), "|"],
            ?assertEqual(
                unicode:characters_to_binary(FullLine),
                unicode:characters_to_binary(observer_cli_lib:pad_rendered(FullLine))
            ),

            UnicodeLine = <<"|中文|"/utf8>>,
            UnicodePadded = observer_cli_lib:pad_rendered(UnicodeLine),
            ?assertNotEqual(nomatch, binary:match(to_binary(UnicodePadded), <<"中文"/utf8>>)),
            ?assertEqual(LayoutWidth, observer_cli_lib:visible_length(UnicodePadded)),
            ?assertEqual(2, border_count(UnicodePadded)),

            AnsiLine = ["|", observer_cli_lib:ansi_green("ok"), "|"],
            AnsiPadded = observer_cli_lib:pad_rendered(AnsiLine),
            observer_cli_test_io:assert_ansi_boundaries(AnsiPadded),
            ?assertEqual(4, observer_cli_lib:visible_length(AnsiLine)),
            ?assertEqual(LayoutWidth, observer_cli_lib:visible_length(AnsiPadded)),
            ?assertEqual(2, border_count(AnsiPadded))
        end
    ).

to_binary(IoData) ->
    unicode:characters_to_binary(IoData).

border_count(IoData) ->
    length(binary:matches(to_binary(IoData), <<"|">>)).

update_page_pos_test() ->
    Pages = [{1, 1}],
    NewPages = observer_cli_lib:update_page_pos(2, 5, Pages),
    ?assertEqual({2, 5}, lists:keyfind(2, 1, NewPages)),

    StorePid = observer_cli_store:start(),
    observer_cli_store:update(StorePid, 2, [{1, self()}]),
    StorePages = observer_cli_lib:update_page_pos(StorePid, 3, []),
    ?assertEqual({3, 5}, lists:keyfind(3, 1, StorePages)),
    erlang:unlink(StorePid),
    erlang:exit(StorePid, kill).

next_page_test() ->
    ?assertEqual(2, observer_cli_lib:next_page(1, 1)),
    ?assertEqual(1, observer_cli_lib:next_page(1, -1)),
    ?assertEqual(3, observer_cli_lib:next_page(5, -2)).

get_pos_test() ->
    ?assertEqual({1, 1}, observer_cli_lib:get_pos(1, 10, [], 0)),
    ?assertEqual({3, 3}, observer_cli_lib:get_pos(2, 2, [], 5)).

sublist_test() ->
    Items = [{0, 2, a}, {0, 5, b}, {0, 1, c}],
    {Start, List} = observer_cli_lib:sublist(Items, 2, 1),
    ?assertEqual(1, Start),
    ?assertEqual(2, length(List)),
    ?assertEqual({7, []}, observer_cli_lib:sublist(Items, 2, 4)).

sbcs_to_mbcs_test() ->
    TypeList = [binary_alloc, driver_alloc],
    Input = [
        {{binary_alloc, foo}, 10},
        {{driver_alloc, bar}, 5},
        {{other, baz}, 7},
        {ignore, no_number}
    ],
    ResultMap = maps:from_list(observer_cli_lib:sbcs_to_mbcs(TypeList, Input)),
    ?assert(maps:is_key(binary_alloc, ResultMap)),
    ?assert(maps:is_key(driver_alloc, ResultMap)),
    BinVal = maps:get(binary_alloc, ResultMap),
    DrvVal = maps:get(driver_alloc, ResultMap),
    ?assert(is_number(BinVal)),
    ?assert(is_number(DrvVal)),
    ?assert(BinVal >= 0),
    ?assert(DrvVal >= 0),
    ?assertEqual(false, maps:is_key(other, ResultMap)).

pipe_test() ->
    Result = observer_cli_lib:pipe(1, [fun(X) -> X + 1 end, fun(X) -> X * 2 end]),
    ?assertEqual(4, Result).

exit_processes_test() ->
    Pid1 = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Pid2 = spawn(fun() -> receive
        after infinity -> ok
        end end),
    observer_cli_lib:exit_processes([Pid1, Pid2]),
    ?assertEqual(false, is_process_alive(Pid1)),
    ?assertEqual(false, is_process_alive(Pid2)).

-endif.
