-module(observer_cli_process_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(true, run_start(["q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_state_view_quit_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(true, run_start(["S\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_view_switch_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(true, run_start(["M\n", "D\n", "C\n", "P\n", "1500\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_home_action_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(quit, run_start(["H\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_back_home_action_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(quit, run_start(["B\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_back_plugin_action_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    try
        ?assertEqual(quit, run_start_type(plugin, ["B\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

render_state_success_test() ->
    Pid = whereis(application_controller),
    ?assert(is_pid(Pid)),
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Result = observer_cli_process:render_state(Pid, home, 1500),
            ?assertMatch({ok, _}, Result)
        end
    ).

state_nav_test() ->
    NavHome = observer_cli_process:state_nav(home),
    NavPlugin = observer_cli_process:state_nav(plugin),
    ?assertEqual(false, maps:is_key("B\n", NavHome)),
    ?assertEqual(true, maps:is_key("B\n", NavPlugin)).

truncate_str_formatter_fallback_test() ->
    Prev = application:get_env(observer_cli, formatter),
    application:set_env(observer_cli, formatter, #{mod => missing_formatter}),
    try
        Value = observer_cli_process:truncate_str(self(), #{test => ok}),
        ?assert(is_list(Value))
    after
        restore_formatter_env(Prev)
    end.

restore_formatter_env({ok, Formatter}) ->
    application:set_env(observer_cli, formatter, Formatter);
restore_formatter_env(undefined) ->
    application:unset_env(observer_cli, formatter).

run_start(Inputs, Pid) ->
    run_start_type(home, Inputs, Pid).

run_start_type(Type, Inputs, Pid) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        process_flag(trap_exit, true),
        Result = observer_cli_test_io:with_input(
            Inputs,
            fun() ->
                Opts = #view_opts{auto_row = false},
                observer_cli_process:start(Type, Pid, Opts)
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
    ?assertEqual(quit, observer_cli_process:parse_cmd_str("q\n")),
    ?assertEqual(quit, observer_cli_process:parse_cmd_str("Q\n")),
    ?assertEqual(info_view, observer_cli_process:parse_cmd_str("P\n")),
    ?assertEqual(message_view, observer_cli_process:parse_cmd_str("M\n")),
    ?assertEqual(dict_view, observer_cli_process:parse_cmd_str("D\n")),
    ?assertEqual(stack_view, observer_cli_process:parse_cmd_str("C\n")),
    ?assertEqual(state_view, observer_cli_process:parse_cmd_str("S\n")),
    ?assertEqual(home, observer_cli_process:parse_cmd_str("H\n")),
    ?assertEqual(back, observer_cli_process:parse_cmd_str("B\n")),
    ?assertEqual({jump, 10}, observer_cli_process:parse_cmd_str("10")),
    ?assertEqual({new_interval, 1500}, observer_cli_process:parse_cmd_str("1500")),
    ?assertEqual(quit, observer_cli_process:parse_cmd_str({error, estale})).

chart_format_test() ->
    ?assertEqual("3->", observer_cli_process:chart_format([3, 3], "")).

replace_first_line_test() ->
    ?assertEqual("new\nrest", observer_cli_process:replace_first_line("old\nrest", "new")),
    ?assertEqual("new\n", observer_cli_process:replace_first_line("only", "new")).

render_process_info_test() ->
    GC = [
        {min_bin_vheap_size, 1},
        {min_heap_size, 2},
        {fullsweep_after, 3},
        {minor_gcs, 4}
    ],
    [Title, Rows] = observer_cli_process:render_process_info(
        self(),
        "",
        self(),
        running,
        false,
        {mod, init, 1},
        0,
        10,
        20,
        GC
    ),
    ?assert(string:find(lists:flatten(Title), "Meta") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "registered_name") =/= nomatch).

render_process_info_registered_test() ->
    GC = [
        {min_bin_vheap_size, 1},
        {min_heap_size, 2},
        {fullsweep_after, 3},
        {minor_gcs, 4}
    ],
    [Title, Rows] = observer_cli_process:render_process_info(
        self(),
        test_reg,
        self(),
        running,
        true,
        {mod, init, 1},
        5,
        10,
        20,
        GC
    ),
    ?assert(string:find(lists:flatten(Title), "Meta") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "test_reg") =/= nomatch).

render_link_monitor_test() ->
    Line = observer_cli_process:render_link_monitor([self()], [{process, self()}], [self()]),
    ?assert(string:find(lists:flatten(Line), "Links(") =/= nomatch).

render_link_monitor_variants_test() ->
    Port = open_port({spawn, "cat"}, []),
    try
        Monitors = [
            {process, {reg_proc, node()}},
            {process, self()},
            {port, {reg_port, node()}},
            {port, Port}
        ],
        Line = observer_cli_process:render_link_monitor([self()], Monitors, [self()]),
        ?assert(string:find(lists:flatten(Line), "Monitors(") =/= nomatch)
    after
        port_close(Port)
    end.

render_reduction_memory_test() ->
    Q = queue:new(),
    {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(10, 20, Q, Q),
    ?assert(string:find(lists:flatten(Lines), "Reductions") =/= nomatch).

render_reduction_memory_queue_trim_test() ->
    Q = lists:foldl(fun(_, Acc) -> queue:in(1, Acc) end, queue:new(), lists:seq(1, 20)),
    {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(10, 20, Q, Q),
    ?assert(string:find(lists:flatten(Lines), "Reductions") =/= nomatch).

render_menu_test() ->
    Line = observer_cli_process:render_menu(info, home, 1500),
    ?assert(string:find(lists:flatten(Line), "Interval: 1500ms") =/= nomatch).

render_menu_views_test() ->
    Line1 = observer_cli_process:render_menu(message, home, 1500),
    Line2 = observer_cli_process:render_menu(dict, home, 1500),
    Line3 = observer_cli_process:render_menu(stack, home, 1500),
    Line4 = observer_cli_process:render_menu(state, home, 1500),
    ?assert(string:find(lists:flatten(Line1), "Messages") =/= nomatch),
    ?assert(string:find(lists:flatten(Line2), "Dictionary") =/= nomatch),
    ?assert(string:find(lists:flatten(Line3), "Stack") =/= nomatch),
    ?assert(string:find(lists:flatten(Line4), "State") =/= nomatch).

render_menu_plugin_test() ->
    Line = observer_cli_process:render_menu(info, plugin, 1500),
    ?assert(string:find(lists:flatten(Line), "Back(B)") =/= nomatch).

render_last_line_test() ->
    Line = observer_cli_process:render_last_line(),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

state_title_test() ->
    Title = observer_cli_process:state_title(self()),
    ?assert(string:find(lists:flatten(Title), "recon:get_state") =/= nomatch).

state_footer_text_test() ->
    Text = observer_cli_process:state_footer_text(#{}),
    ?assertEqual("q(quit)    F/B(page forward/back)", Text),
    Line = observer_cli_process:render_footer_line(Text, 140),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

state_footer_test() ->
    Line = observer_cli_process:state_footer("menu", #{}),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

chart_format_branch_test() ->
    ?assertEqual("3->", observer_cli_process:chart_format([3, 2], "")),
    ?assertEqual("1->", observer_cli_process:chart_format([1, 2], "")).

truncate_str_default_formatter_test() ->
    Value = observer_cli_process:truncate_str(self(), #{test => ok}),
    ?assert(is_list(Value)).

render_worker_message_empty_test() ->
    Target = spawn(fun() -> receive after infinity -> ok end end),
    Worker = spawn_worker(message, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_message_with_messages_test() ->
    Target = spawn(fun() -> receive after infinity -> ok end end),
    Target ! hello,
    Target ! world,
    Worker = spawn_worker(message, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_message_too_many_test() ->
    Target = spawn(fun() -> receive after infinity -> ok end end),
    lists:foreach(fun(_) -> Target ! msg end, lists:seq(1, 10001)),
    Worker = spawn_worker(message, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_message_undefined_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    Worker = spawn_worker(message, Target),
    stop_worker(Worker).

render_worker_dict_empty_test() ->
    Target = spawn(fun() -> receive after infinity -> ok end end),
    Worker = spawn_worker(dict, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_dict_with_entries_test() ->
    Target = spawn(fun() ->
        put(test_key, test_value),
        receive after infinity -> ok end
    end),
    Worker = spawn_worker(dict, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_dict_undefined_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    Worker = spawn_worker(dict, Target),
    stop_worker(Worker).

render_worker_stack_test() ->
    Target = spawn(fun() -> deep_stack_wait() end),
    Worker = spawn_worker(stack, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_stack_undefined_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    Worker = spawn_worker(stack, Target),
    stop_worker(Worker).

render_worker_info_dead_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    Worker = spawn_worker(info, Target),
    stop_worker(Worker).

render_worker_next_draw_actions_test() ->
    Target = spawn(fun() -> receive after infinity -> ok end end),
    Worker = spawn_worker(info, Target),
    Worker ! {new_interval, 2000},
    Worker ! info_view,
    Worker ! message_view,
    Worker ! dict_view,
    Worker ! stack_view,
    Worker ! redraw,
    stop_worker(Worker),
    exit(Target, kill).

spawn_worker(Type, TargetPid) ->
    Parent = self(),
    spawn(fun() ->
        observer_cli_process:render_worker(
            Type,
            home,
            1500,
            TargetPid,
            ?INIT_TIME_REF,
            queue:new(),
            queue:new(),
            Parent
        )
    end).

stop_worker(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 2000 ->
        exit(timeout)
    end.

deep_stack_wait() ->
    deep_stack_level1().

deep_stack_level1() ->
    deep_stack_level2(),
    ok.

deep_stack_level2() ->
    deep_stack_level3(),
    ok.

deep_stack_level3() ->
    receive after infinity -> ok end.

-endif.
