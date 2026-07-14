-module(observer_cli_process_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(true, run_start(["q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_state_view_quit_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(true, run_start(["S\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_state_view_success_action_test() ->
    Pid = whereis(application_controller),
    ?assert(is_pid(Pid)),
    ?assertEqual(true, run_start(["S\n", "P\n", "q\n"], Pid)).

state_view_none_returns_to_manager_test() ->
    RenderPid = spawn(fun() ->
        receive
            quit -> ok
        end
    end),
    self() ! {state_view_done, {ok, none}},
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(
                true,
                observer_cli_process:wait_for_state_view(RenderPid, home, self(), Opts)
            )
        end
    ).

start_view_switch_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(true, run_start(["M\n", "D\n", "C\n", "P\n", "1500\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_home_action_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(quit, run_start(["H\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_back_home_action_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(quit, run_start(["B\n", "q\n"], Pid))
    after
        exit(Pid, kill)
    end.

start_back_plugin_action_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
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
    ?assertEqual(quit, maps:get("q\n", NavHome)),
    ?assertEqual(quit, maps:get("Q\n", NavHome)),
    ?assertEqual(home, maps:get("H\n", NavHome)),
    ?assertEqual(info_view, maps:get("P\n", NavHome)),
    ?assertEqual(message_view, maps:get("M\n", NavHome)),
    ?assertEqual(dict_view, maps:get("D\n", NavHome)),
    ?assertEqual(stack_view, maps:get("C\n", NavHome)),
    ?assertEqual(false, maps:is_key("B\n", NavHome)),
    ?assertEqual(back, maps:get("B\n", NavPlugin)).

truncate_str_formatter_fallback_test() ->
    Prev = application:get_env(observer_cli, formatter),
    application:set_env(observer_cli, formatter, #{mod => missing_formatter}),
    try
        Value = observer_cli_process:truncate_str(self(), #{test => ok}),
        ?assert(lists:prefix("Process: ", Value))
    after
        restore_formatter_env(Prev)
    end.

restore_formatter_env({ok, Formatter}) ->
    application:set_env(observer_cli, formatter, Formatter);
restore_formatter_env(undefined) ->
    application:unset_env(observer_cli, formatter).

collect_process_info_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        Info = observer_cli_process:collect_process_info(Target),
        ?assertEqual(
            lists:sort([links, memory, monitored_by, monitors, process, reductions]),
            lists:sort(maps:keys(Info))
        ),
        ?assertMatch(
            #{
                process := #{
                    pid := Target,
                    registered_name := _,
                    group_leader := _,
                    status := _,
                    trap_exit := _,
                    initial_call := _,
                    message_queue_len := _,
                    heap_size := _,
                    total_heap_size := _,
                    stack_size := _,
                    binary_refs := _,
                    priority := _,
                    catchlevel := _,
                    suspending := _,
                    error_handler := _,
                    garbage_collection := #{
                        min_bin_vheap_size := _,
                        min_heap_size := _,
                        fullsweep_after := _,
                        minor_gcs := _
                    }
                },
                links := _,
                monitors := _,
                monitored_by := _,
                reductions := _,
                memory := _
            },
            Info
        ),
        Process = maps:get(process, Info),
        ?assert(is_pid(maps:get(group_leader, Process))),
        ?assert(is_integer(maps:get(message_queue_len, Process))),
        ?assert(is_integer(maps:get(heap_size, Process))),
        ?assert(is_integer(maps:get(total_heap_size, Process))),
        ?assert(is_integer(maps:get(stack_size, Process))),
        {garbage_collection, RawGC} = erlang:process_info(Target, garbage_collection),
        GC = maps:get(garbage_collection, Process),
        WordSize = erlang:system_info(wordsize),
        ?assertEqual(
            proplists:get_value(min_bin_vheap_size, RawGC) * WordSize,
            maps:get(min_bin_vheap_size, GC)
        ),
        ?assertEqual(
            proplists:get_value(min_heap_size, RawGC) * WordSize,
            maps:get(min_heap_size, GC)
        ),
        ?assertMatch({_, _}, maps:get(binary_refs, Process)),
        ?assert(is_atom(maps:get(priority, Process))),
        ?assert(is_integer(maps:get(catchlevel, Process))),
        ?assert(is_list(maps:get(suspending, Process))),
        ?assert(is_atom(maps:get(error_handler, Process))),
        ?assert(is_integer(maps:get(memory, Info))),
        ?assert(is_integer(maps:get(reductions, Info))),
        ?assert(is_list(maps:get(links, Info))),
        ?assert(is_list(maps:get(monitors, Info))),
        ?assert(is_list(maps:get(monitored_by, Info)))
    after
        exit(Target, kill)
    end.

collect_process_info_dead_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    ?assertEqual(dead, observer_cli_process:collect_process_info(Target)).

collect_process_messages_empty_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        {ok, Info} = observer_cli_process:collect_process_messages(Target),
        ?assertMatch(#{pid := Target, message_queue_len := 0, messages := []}, Info),
        Line = observer_cli_process:render_process_messages(Info),
        ?assert(string:find(lists:flatten(Line), "No messages") =/= nomatch)
    after
        exit(Target, kill)
    end.

collect_process_messages_with_messages_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        Target ! hello,
        {ok, Info} = observer_cli_process:collect_process_messages(Target),
        ?assertMatch(#{pid := Target, message_queue_len := 1, messages := [hello]}, Info),
        Line = observer_cli_process:render_process_messages(Info),
        ?assert(string:find(lists:flatten(Line), "Message Len:1") =/= nomatch),
        ?assert(string:find(lists:flatten(Line), "hello") =/= nomatch)
    after
        exit(Target, kill)
    end.

collect_process_dictionary_test() ->
    Parent = self(),
    Target = spawn(fun() ->
        put(observer_cli_test_key, observer_cli_test_value),
        Parent ! ready,
        receive
        after infinity -> ok
        end
    end),
    receive
        ready -> ok
    after 1000 ->
        exit(timeout)
    end,
    try
        {ok, Info} = observer_cli_process:collect_process_dictionary(Target),
        ?assertMatch(#{pid := Target, dictionary := _}, Info),
        ?assertEqual(
            observer_cli_test_value,
            proplists:get_value(observer_cli_test_key, maps:get(dictionary, Info))
        ),
        Line = observer_cli_process:render_process_dictionary(Info),
        ?assert(string:find(lists:flatten(Line), "dictionary_len") =/= nomatch),
        ?assert(string:find(lists:flatten(Line), "observer_cli_test_key") =/= nomatch)
    after
        exit(Target, kill)
    end.

collect_process_stack_test() ->
    {ok, Info} = observer_cli_process:collect_process_stack(self()),
    ?assertMatch(#{pid := _, stack := [_ | _]}, Info).

render_process_stack_test() ->
    Line = observer_cli_process:render_process_stack([
        {observer_cli_process_test, render_process_stack_test, 0, [
            {file, "observer_cli_process_test.erl"}, {line, 1}
        ]}
    ]),
    ?assert(string:find(lists:flatten(Line), "observer_cli_process_test") =/= nomatch),
    ?assert(string:find(lists:flatten(Line), "observer_cli_process_test.erl:1") =/= nomatch).

render_process_stack_multiple_entries_test() ->
    Line = observer_cli_process:render_process_stack([
        {mod_a, fun_a, 0, [{file, "a.erl"}, {line, 1}]},
        {mod_b, fun_b, 1, [{file, "b.erl"}, {line, 2}]}
    ]),
    Text = lists:flatten(Line),
    ?assert(string:find(Text, "mod_a:fun_a/0") =/= nomatch),
    ?assert(string:find(Text, "mod_b:fun_b/1") =/= nomatch).

collect_process_state_test() ->
    Pid = whereis(application_controller),
    ?assert(is_pid(Pid)),
    _State = observer_cli_process:collect_process_state(Pid),
    ok.

render_process_state_test() ->
    Line = observer_cli_process:render_process_state(self(), #{state => ok}),
    Text = lists:flatten(Line),
    ?assert(string:find(Text, "recon:get_state") =/= nomatch),
    ?assert(string:find(Text, "state") =/= nomatch).

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
    ?assertEqual({input_str, "oops"}, observer_cli_process:parse_cmd_str("oops\n")),
    ?assertEqual(quit, observer_cli_process:parse_cmd_str({error, estale})).

chart_format_test() ->
    ?assertEqual("3->", observer_cli_process:chart_format([3, 3], "")).

replace_first_line_test() ->
    ?assertEqual("new\nrest", observer_cli_process:replace_first_line("old\nrest", "new")),
    ?assertEqual("new\n", observer_cli_process:replace_first_line("only", "new")).

render_process_info_test() ->
    GC = gc_view(),
    ProcessView = #{
        pid => self(),
        registered_name => "",
        group_leader => self(),
        status => running,
        trap_exit => false,
        initial_call => {mod, init, 1},
        message_queue_len => 0,
        heap_size => 10,
        total_heap_size => 20,
        stack_size => 30,
        binary_refs => {2, 2048},
        priority => normal,
        catchlevel => 1,
        suspending => [],
        error_handler => error_handler,
        garbage_collection => GC
    },
    [Title, Rows] = observer_cli_process:render_process_info(ProcessView),
    ?assert(string:find(lists:flatten(Title), "Meta") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "registered_name") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "binary_refs") =/= nomatch).

render_process_info_registered_test() ->
    GC = gc_view(),
    ProcessView = #{
        pid => self(),
        registered_name => test_reg,
        group_leader => self(),
        status => running,
        trap_exit => true,
        initial_call => {mod, init, 1},
        message_queue_len => 5,
        heap_size => 10,
        total_heap_size => 20,
        stack_size => 30,
        binary_refs => {2, 2048},
        priority => normal,
        catchlevel => 1,
        suspending => [],
        error_handler => error_handler,
        garbage_collection => GC
    },
    [Title, Rows] = observer_cli_process:render_process_info(ProcessView),
    ?assert(string:find(lists:flatten(Title), "Meta") =/= nomatch),
    ?assert(string:find(lists:flatten(Rows), "test_reg") =/= nomatch).

render_process_info_wide_layout_test() ->
    Base = process_info_widths(80),
    Wide = process_info_widths(180),
    ?assertEqual([1, 3, 5], unchanged_columns(Base, Wide, [1, 3, 5])),
    ?assertEqual([2, 4, 6], wider_columns(Base, Wide, [2, 4, 6])).

render_process_info_wide_alignment_test() ->
    {Title, Rows} = process_info_columns(205),
    ?assert(lists:all(fun(Row) -> Row =:= Title end, Rows)).

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

render_link_monitor_wide_layout_test() ->
    Base = process_link_widths(80),
    Wide = process_link_widths(180),
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
    [First | Rest] = process_link_widths(205),
    ?assert(lists:all(fun(Row) -> Row =:= First end, Rest)).

render_reduction_memory_test() ->
    Q = queue:new(),
    {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(10, 20, Q, Q),
    ?assert(string:find(lists:flatten(Lines), "Reductions") =/= nomatch).

render_reduction_memory_queue_trim_test() ->
    Q = lists:foldl(fun(_, Acc) -> queue:in(1, Acc) end, queue:new(), lists:seq(1, 20)),
    {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(10, 20, Q, Q),
    ?assert(string:find(lists:flatten(Lines), "Reductions") =/= nomatch).

render_reduction_memory_wide_layout_test() ->
    Base = reduction_memory_widths(80),
    Wide = reduction_memory_widths(220),
    ?assert(lists:nth(1, Wide) > lists:nth(1, Base)),
    ?assert(lists:nth(2, Wide) > lists:nth(2, Base)),
    observer_cli_test_io:with_geometry(
        24,
        220,
        [],
        fun() ->
            Q = queue:from_list(['NaN', 'NaN', 'NaN']),
            {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(
                'NaN', 'NaN', Q, Q
            ),
            ?assert(string:find(lists:flatten(Lines), "NaN") =/= nomatch)
        end
    ).

render_info_page_wide_border_alignment_test() ->
    {LayoutWidth, LineWidths} = process_info_page_line_widths(205),
    ?assert(lists:all(fun(Width) -> Width =:= LayoutWidth end, LineWidths)).

render_process_sections_test() ->
    Q = queue:from_list([nan, nan, nan]),
    Info = #{
        process => process_view(),
        links => [self()],
        monitors => [{process, self()}],
        monitored_by => [self()],
        reductions => 10,
        memory => 20
    },
    {NewRedQ, NewMemQ, [Process, Links, RedMem]} =
        observer_cli_process:render_process_sections(Info, Q, Q),
    {ExpectedRedQ, ExpectedMemQ, ExpectedRedMem} =
        observer_cli_process:render_reduction_memory(10, 20, Q, Q),
    ?assertEqual(observer_cli_process:render_process_info(process_view()), Process),
    ?assertEqual(
        observer_cli_process:render_link_monitor([self()], [{process, self()}], [self()]),
        Links
    ),
    ?assertEqual(queue:to_list(ExpectedRedQ), queue:to_list(NewRedQ)),
    ?assertEqual(queue:to_list(ExpectedMemQ), queue:to_list(NewMemQ)),
    ?assertEqual(ExpectedRedMem, RedMem).

render_stateless_view_test() ->
    Output = observer_cli_process:render_stateless_view(message, home, 1500, "body\n"),
    observer_cli_test_io:assert_stable_fragments(Output, [
        "Home(H)",
        "Messages(M)",
        "Interval: 1500ms",
        "body",
        "q(quit)"
    ]).

process_detail_golden_output_fragments_test() ->
    observer_cli_test_io:with_geometry(
        24,
        205,
        [],
        fun() ->
            Q = queue:from_list([nan, nan, nan]),
            {_RedQ, _MemQ, RedMem} = observer_cli_process:render_reduction_memory(
                10, 20, Q, Q
            ),
            Output = [
                observer_cli_process:render_menu(info, home, 1500),
                observer_cli_process:render_process_info(process_view()),
                observer_cli_process:render_link_monitor([self()], [{process, self()}], [self()]),
                RedMem,
                observer_cli_process:render_footer()
            ],
            observer_cli_test_io:assert_stable_fragments(Output, [
                "Home(H)",
                "Process Info(P)",
                "Messages(M)",
                "Dictionary(D)",
                "Current Stack(C)",
                "State(S)",
                "Interval: 1500ms",
                "Meta",
                "Memory Used",
                "Garbage Collection",
                "registered_name",
                "msg_queue_len",
                "priority",
                "stack_size",
                "binary_refs",
                "catchlevel",
                "suspending",
                "error_handler",
                "minor_gcs",
                "Links(1)",
                "Monitors(1)",
                "MonitoredBy(1)",
                "Reductions:",
                "Memory:",
                "q(quit)"
            ]),
            observer_cli_test_io:assert_ansi_boundaries(Output)
        end
    ).

process_dead_golden_output_fragments_test() ->
    Target = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Target),
    receive
        {'DOWN', Ref, process, Target, _} -> ok
    after 1000 ->
        ok
    end,
    {_Result, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        205,
        [],
        fun() -> observer_cli_process:output_die_view(Target, home, 1500) end
    ),
    observer_cli_test_io:assert_stable_fragments(Output, [
        "Process Info(P)",
        "Process(",
        "has already died.",
        "q(quit)"
    ]),
    observer_cli_test_io:assert_ansi_boundaries(Output).

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

render_footer_test() ->
    Line = observer_cli_process:render_footer(),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

state_title_test() ->
    Title = observer_cli_process:state_title(self()),
    ?assert(string:find(lists:flatten(Title), "recon:get_state") =/= nomatch).

state_footer_text_test() ->
    Text = observer_cli_process:state_footer_text(#{}),
    ?assertEqual("q(quit)    F/B(page forward/back)", Text),
    Line = observer_cli_process:state_footer("menu", #{}),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

state_footer_test() ->
    Line = observer_cli_process:state_footer("menu", #{}),
    ?assert(string:find(lists:flatten(Line), "q(quit)") =/= nomatch).

chart_format_branch_test() ->
    ?assertEqual("3->", observer_cli_process:chart_format([3, 2], "")),
    ?assertEqual("1->", observer_cli_process:chart_format([1, 2], "")).

truncate_str_default_formatter_test() ->
    Value = observer_cli_process:truncate_str(self(), #{test => ok}),
    ?assert(lists:prefix("Process: ", Value)).

render_worker_message_empty_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Worker = spawn_worker(message, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_message_with_messages_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Target ! hello,
    Target ! world,
    Worker = spawn_worker(message, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_message_too_many_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
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
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Worker = spawn_worker(dict, Target),
    stop_worker(Worker),
    exit(Target, kill).

render_worker_dict_with_entries_test() ->
    Target = spawn(fun() ->
        put(test_key, test_value),
        receive
        after infinity -> ok
        end
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

render_worker_state_success_test() ->
    Target = whereis(application_controller),
    ?assert(is_pid(Target)),
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Worker = spawn_worker(state, Target),
            receive
                {state_view_done, {ok, quit}} -> ok
            after 2000 ->
                erlang:error(state_view_done_missing)
            end,
            stop_worker(Worker)
        end
    ).

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
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Worker = spawn_worker(info, Target),
    Worker ! {new_interval, 2000},
    Worker ! info_view,
    Worker ! message_view,
    Worker ! dict_view,
    Worker ! stack_view,
    Worker ! redraw,
    stop_worker(Worker),
    exit(Target, kill).

render_worker_shrinking_redraw_clears_test() ->
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        MessageOutput = capture_redraw(
            message,
            Target,
            observer_cli_process:render_process_messages(#{
                pid => Target, message_queue_len => 1, messages => [message_before_shrink]
            })
        ),
        assert_clear_between(MessageOutput, "message_before_shrink", "No messages"),

        DictionaryOutput = capture_redraw(
            dict,
            Target,
            observer_cli_process:render_process_dictionary(#{
                pid => Target, dictionary => [{observer_cli_redraw_key, value}]
            })
        ),
        assert_clear_between(
            DictionaryOutput, "observer_cli_redraw_key", "No dictionary"
        ),

        StackOutput = capture_redraw(
            stack,
            Target,
            observer_cli_process:render_process_stack([
                {old_stack, frame, 0, [{file, "old.erl"}, {line, 1}]},
                {old_stack, caller, 0, [{file, "old.erl"}, {line, 2}]}
            ])
        ),
        assert_clear_between(StackOutput, "old_stack:caller/0", "current_stacktrace"),

        Ref = erlang:monitor(process, Target),
        exit(Target, kill),
        receive
            {'DOWN', Ref, process, Target, _} -> ok
        after 1000 ->
            exit(fixture_exit_timeout)
        end,
        DeadOutput = capture_redraw(info, Target, "old process detail\nstale tail\n"),
        assert_clear_between(DeadOutput, "stale tail", "has already died.")
    after
        erlang:is_process_alive(Target) andalso exit(Target, kill)
    end.

capture_redraw(Status, Target, PreviousBody) ->
    {ok, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        205,
        [],
        fun() ->
            Worker = spawn(fun() ->
                io:put_chars(PreviousBody),
                observer_cli_process:render_worker(
                    Status,
                    home,
                    1500,
                    Target,
                    ?INIT_TIME_REF,
                    queue:new(),
                    queue:new(),
                    self()
                )
            end),
            Worker ! redraw,
            stop_worker(Worker)
        end
    ),
    unicode:characters_to_binary(Output).

assert_clear_between(Output, Before, After) ->
    {BeforePos, _} = binary:match(Output, list_to_binary(Before)),
    {ClearPos, _} = binary:match(Output, ?CLEAR),
    [{AfterPos, _} | _] = lists:reverse(binary:matches(Output, list_to_binary(After))),
    ?assert(BeforePos < ClearPos),
    ?assert(ClearPos < AfterPos).

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
    receive
    after infinity -> ok
    end.

process_info_widths(Columns) ->
    {Title, [FirstRow | _]} = process_info_columns(Columns),
    {Title, FirstRow}.

process_info_columns(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Rows] = observer_cli_process:render_process_info(process_view()),
            {
                observer_cli_test_io:column_widths(Title),
                observer_cli_test_io:line_column_widths(Rows)
            }
        end
    ).

process_link_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Line = observer_cli_process:render_link_monitor(
                [self()], [{process, self()}], [self()]
            ),
            observer_cli_test_io:line_column_widths(Line)
        end
    ).

reduction_memory_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Q = queue:from_list(['NaN', 'NaN', 'NaN']),
            {_RedQ, _MemQ, Lines} = observer_cli_process:render_reduction_memory(
                'NaN', 'NaN', Q, Q
            ),
            observer_cli_test_io:line_widths(Lines)
        end
    ).

process_info_page_line_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            Q = queue:from_list([nan, nan, nan]),
            {_RedQ, _MemQ, RedMem} = observer_cli_process:render_reduction_memory(
                10, 20, Q, Q
            ),
            IoData = [
                observer_cli_process:render_menu(info, home, 1500),
                observer_cli_process:render_process_info(process_view()),
                observer_cli_process:render_link_monitor([self()], [{process, self()}], [self()]),
                RedMem,
                observer_cli_process:render_footer()
            ],
            {observer_cli_lib:layout_width(), observer_cli_test_io:line_widths(IoData)}
        end
    ).

process_view() ->
    GC = gc_view(),
    #{
        pid => self(),
        registered_name => test_reg,
        group_leader => self(),
        status => running,
        trap_exit => true,
        initial_call => {very_long_module_for_layout, very_long_function_for_layout, 1},
        message_queue_len => 5,
        heap_size => 10,
        total_heap_size => 20,
        stack_size => 30,
        binary_refs => {2, 2048},
        priority => normal,
        catchlevel => 1,
        suspending => [],
        error_handler => error_handler,
        garbage_collection => GC
    }.

gc_view() ->
    #{
        min_bin_vheap_size => 1,
        min_heap_size => 2,
        fullsweep_after => 3,
        minor_gcs => 4
    }.

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
    ].

safe_diagnostics_process_info_and_name_resolution_test() ->
    Parent = self(),
    Name = observer_cli_goal08_fixture,
    _ExistingUnregistered = observer_cli_goal08_unregistered,
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    true = register(Name, Pid),
    Source = #{
        count_fun => fun() -> 1 end,
        fold => {fixture_list, fun(_Fun, Acc) -> Acc end},
        info_fun => fun(Target, Keys) ->
            Parent ! {detail_keys, Target, Keys},
            detail_info(Keys)
        end,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> 0 end,
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    },
    try
        #{<<"status">> := <<"ok">>, <<"result">> := Response} =
            observer_cli_snapshot:dispatch(
                self(),
                process,
                #{target => atom_to_binary(Name), test_process_source => Source},
                #{timeout_ms => 3000, identifier_policy => include}
            ),
        receive
            {detail_keys, Pid, Keys} ->
                ?assertEqual(
                    [
                        registered_name,
                        status,
                        current_function,
                        initial_call,
                        memory,
                        message_queue_len,
                        reductions,
                        heap_size,
                        total_heap_size,
                        stack_size,
                        group_leader,
                        binary,
                        garbage_collection,
                        garbage_collection_info,
                        priority,
                        links,
                        monitors,
                        monitored_by,
                        catchlevel,
                        suspending,
                        error_handler,
                        trap_exit,
                        current_stacktrace
                    ],
                    Keys
                )
        end,
        Data = maps:get(<<"data">>, Response),
        Forbidden = [
            <<"messages">>,
            <<"dictionary">>,
            <<"state">>
        ],
        ?assertEqual([], [Key || Key <- Forbidden, maps:is_key(Key, Data)]),
        ?assertEqual(80, maps:get(<<"heap_size_bytes">>, Data)),
        ?assertEqual(160, maps:get(<<"total_heap_size_bytes">>, Data)),
        ?assertEqual(<<"running">>, maps:get(<<"status">>, Data)),
        ?assert(is_binary(maps:get(<<"group_leader">>, Data))),
        AtomCount = erlang:system_info(atom_count),
        Missing = <<"observer_cli_goal08_atom_that_must_not_exist_987654321">>,
        ?assertEqual(not_found, observer_cli_snapshot:resolve_process_target(Missing, Source)),
        ?assertEqual(
            not_found,
            observer_cli_snapshot:resolve_process_target(
                <<"observer_cli_goal08_unregistered">>, Source
            )
        ),
        ?assertEqual(AtomCount, erlang:system_info(atom_count)),
        ?assertEqual(
            {ok, Pid},
            observer_cli_snapshot:resolve_process_target(list_to_binary(pid_to_list(Pid)), Source)
        ),
        ?assertEqual(
            not_found, observer_cli_snapshot:resolve_process_target(<<"<0.bad.0>">>, Source)
        )
    after
        unregister(Name),
        exit(Pid, kill)
    end.

detail_info(Keys) ->
    Values = #{
        registered_name => observer_cli_goal08_fixture,
        status => running,
        current_function => {?MODULE, detail_info, 1},
        initial_call => {?MODULE, detail_info, 1},
        memory => 256,
        message_queue_len => 0,
        reductions => 7,
        heap_size => 10,
        total_heap_size => 20,
        stack_size => 3,
        group_leader => self(),
        garbage_collection => [
            {min_bin_vheap_size, 2},
            {min_heap_size, 3},
            {fullsweep_after, 11},
            {minor_gcs, 99}
        ],
        garbage_collection_info => [{heap_size, 10}, {minor_gcs, 99}, {secret, true}]
    },
    [{Key, maps:get(Key, Values, undefined)} || Key <- Keys].

process_private_helper_contract_test() ->
    ?assertEqual(
        {2, 10},
        observer_cli_process:binary_refs_summary([
            {ref, 10, 1}, invalid
        ])
    ),
    ?assertEqual("0", observer_cli_process:format_suspending([])),
    ?assert(is_list(observer_cli_process:format_suspending([self(), self(), self(), self()]))),
    ?assertMatch(
        #{binary_refs := _},
        observer_cli_process:collect_process_extra(
            self(), erlang:system_info(wordsize)
        )
    ),
    Dead = spawn(fun() -> ok end),
    Mon = erlang:monitor(process, Dead),
    receive
        {'DOWN', Mon, process, Dead, normal} -> ok
    end,
    ?assertEqual(
        dead,
        observer_cli_process:collect_process_extra(
            Dead, erlang:system_info(wordsize)
        )
    ).

-endif.
