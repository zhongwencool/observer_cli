-module(observer_cli_lib_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

select_unselect_test() ->
    Selected = observer_cli_lib:select("Home"),
    Unselected = observer_cli_lib:unselect("Home"),
    ?assert(string:find(lists:flatten(Selected), "Home") =/= nomatch),
    ?assert(string:find(lists:flatten(Unselected), "Home") =/= nomatch).

render_menu_test() ->
    application:set_env(observer_cli, default_row_size, 20),
    Line = observer_cli_lib:render_menu(home, "Test"),
    ?assert(string:find(lists:flatten(Line), "Home") =/= nomatch).

render_last_line_test() ->
    Line = observer_cli_lib:render_last_line("q(quit)"),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

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

parse_integer_test() ->
    ?assertEqual({new_interval, 1500}, observer_cli_lib:parse_integer("1500")),
    ?assertEqual({jump, 10}, observer_cli_lib:parse_integer("10")),
    ?assertEqual({input_str, "abc"}, observer_cli_lib:parse_integer("abc")),
    ?assertEqual({input_str, "-1"}, observer_cli_lib:parse_integer("-1")).

parse_cmd_str_test() ->
    ?assertEqual(home_view, observer_cli_lib:parse_cmd_str("H\n")),
    ?assertEqual(system_view, observer_cli_lib:parse_cmd_str("S\n")),
    ?assertEqual(app_view, observer_cli_lib:parse_cmd_str("A\n")),
    ?assertEqual(inet_view, observer_cli_lib:parse_cmd_str("N\n")),
    ?assertEqual(mnesia_view, observer_cli_lib:parse_cmd_str("M\n")),
    ?assertEqual(ets_view, observer_cli_lib:parse_cmd_str("E\n")),
    ?assertEqual(help_view, observer_cli_lib:parse_cmd_str("D\n")),
    ?assertEqual(plugin_view, observer_cli_lib:parse_cmd_str("P\n")),
    ?assertEqual(inet_count, observer_cli_lib:parse_cmd_str("ic\n")),
    ?assertEqual(inet_window, observer_cli_lib:parse_cmd_str("iw\n")),
    ?assertEqual(recv_cnt, observer_cli_lib:parse_cmd_str("rc\n")),
    ?assertEqual(recv_oct, observer_cli_lib:parse_cmd_str("ro\n")),
    ?assertEqual(send_cnt, observer_cli_lib:parse_cmd_str("sc\n")),
    ?assertEqual(send_oct, observer_cli_lib:parse_cmd_str("so\n")),
    ?assertEqual(cnt, observer_cli_lib:parse_cmd_str("cnt\n")),
    ?assertEqual(oct, observer_cli_lib:parse_cmd_str("oct\n")),
    ?assertEqual(page_up_top_n, observer_cli_lib:parse_cmd_str("pu\n")),
    ?assertEqual(page_down_top_n, observer_cli_lib:parse_cmd_str("pd\n")),
    ?assertEqual(page_up_top_n, observer_cli_lib:parse_cmd_str("PU\n")),
    ?assertEqual(page_down_top_n, observer_cli_lib:parse_cmd_str("PD\n")),
    ?assertEqual(page_up_top_n, observer_cli_lib:parse_cmd_str("B\n")),
    ?assertEqual(page_down_top_n, observer_cli_lib:parse_cmd_str("F\n")),
    ?assertEqual({func, proc_count, memory}, observer_cli_lib:parse_cmd_str("m\n")),
    ?assertEqual({func, proc_count, reductions}, observer_cli_lib:parse_cmd_str("r\n")),
    ?assertEqual({func, proc_count, binary_memory}, observer_cli_lib:parse_cmd_str("b\n")),
    ?assertEqual({func, proc_count, total_heap_size}, observer_cli_lib:parse_cmd_str("t\n")),
    ?assertEqual({func, proc_count, message_queue_len}, observer_cli_lib:parse_cmd_str("mq\n")),
    ?assertEqual({func, proc_window, reductions}, observer_cli_lib:parse_cmd_str("rr\n")),
    ?assertEqual({func, proc_window, binary_memory}, observer_cli_lib:parse_cmd_str("bb\n")),
    ?assertEqual({func, proc_window, total_heap_size}, observer_cli_lib:parse_cmd_str("tt\n")),
    ?assertEqual({func, proc_window, memory}, observer_cli_lib:parse_cmd_str("mm\n")),
    ?assertEqual({func, proc_window, message_queue_len}, observer_cli_lib:parse_cmd_str("mmq\n")),
    ?assertEqual(jump, observer_cli_lib:parse_cmd_str("\n")),
    ?assertEqual(size, observer_cli_lib:parse_cmd_str("s\n")),
    ?assertEqual(hide, observer_cli_lib:parse_cmd_str("hide\n")),
    ?assertEqual(scheduler_usage, observer_cli_lib:parse_cmd_str("`\n")),
    ?assertEqual({new_interval, 1500}, observer_cli_lib:parse_cmd_str("1500")),
    ?assertEqual({jump, 10}, observer_cli_lib:parse_cmd_str("10")),
    ?assertEqual(quit, observer_cli_lib:parse_cmd_str({error, estale})),
    ?assertEqual(
        {go_to_pid, list_to_pid("<0.0.0>")},
        observer_cli_lib:parse_cmd_str("<0.0.0>\n")
    ).

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

get_pos_test() ->
    ?assertEqual({1, 1}, observer_cli_lib:get_pos(1, 10, [], 0)),
    ?assertEqual({3, 3}, observer_cli_lib:get_pos(2, 2, [], 5)).

sublist_test() ->
    Items = [{0, 2, a}, {0, 5, b}, {0, 1, c}],
    {Start, List} = observer_cli_lib:sublist(Items, 2, 1),
    ?assertEqual(1, Start),
    ?assertEqual(2, length(List)).

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
