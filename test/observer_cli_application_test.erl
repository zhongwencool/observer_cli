-module(observer_cli_application_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

app_status_test() ->
    Info = [
        {loaded, [{app1, "desc", "1.0"}]},
        {loading, [{app2, self()}]},
        {started, [{app1, permanent}]},
        {start_p_false, [{app4, temporary, worker, self()}]},
        {starting, [{app3, temporary, worker, self()}]}
    ],
    Map = observer_cli_application:app_status(Info),
    ?assertEqual({0, 0, 0, 0, "Started", "1.0"}, maps:get(app1, Map)),
    ?assertEqual({0, 0, 0, 0, "Loading", "unknown"}, maps:get(app2, Map)),
    ?assertEqual({0, 0, 0, 0, "Starting", "unknown"}, maps:get(app3, Map)),
    ?assertEqual({0, 0, 0, 0, "StartPFalse", "unknown"}, maps:get(app4, Map)).

update_app_stats_test() ->
    AllApps = #{
        app1 => {0, 0, 0, 0, "Started", "1.0"},
        no_group => {0, 0, 0, 0, "Unknown", "unknown"}
    },
    Leaders = #{self() => app1},
    Updated = observer_cli_application:update_app_stats(self(), 10, 5, 2, AllApps, Leaders),
    ?assertEqual({1, 10, 5, 2, "Started", "1.0"}, maps:get(app1, Updated)).

update_app_stats_unknown_test() ->
    AllApps = #{no_group => {0, 0, 0, 0, "Unknown", "unknown"}},
    Updated = observer_cli_application:update_app_stats(self(), 3, 2, 1, AllApps, #{}),
    ?assertEqual({1, 3, 2, 1, "Unknown", "unknown"}, maps:get(no_group, Updated)).

update_app_stats_group_leader_chain_test() ->
    GroupLeader = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Child = spawn(fun() -> receive
        after infinity -> ok
        end end),
    group_leader(GroupLeader, GroupLeader),
    group_leader(GroupLeader, Child),
    AllApps = #{
        app1 => {0, 0, 0, 0, "Started", "1.0"},
        no_group => {0, 0, 0, 0, "Unknown", "unknown"}
    },
    Leaders = #{GroupLeader => app1},
    Updated = observer_cli_application:update_app_stats(Child, 1, 2, 3, AllApps, Leaders),
    ?assertEqual({1, 1, 2, 3, "Started", "1.0"}, maps:get(app1, Updated)),
    exit(Child, kill),
    exit(GroupLeader, kill).

collect_app_info_legacy_list_test() ->
    Child = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Dead = spawn(fun() -> ok end),
    DeadRef = erlang:monitor(process, Dead),
    receive
        {'DOWN', DeadRef, process, Dead, _} -> ok
    after 1000 ->
        erlang:error(dead_process_still_alive)
    end,
    {group_leader, Leader} = erlang:process_info(Child, group_leader),
    AllApps = #{
        app1 => {0, 0, 0, 0, "Started", "1.0"},
        no_group => {0, 0, 0, 0, "Unknown", "unknown"}
    },
    try
        Found = observer_cli_application:collect_app_info(
            AllApps, #{Leader => app1}, [self(), Dead, Child], self()
        ),
        {AppCount, AppMemory, AppReds, _AppMsgQ, "Started", "1.0"} = maps:get(app1, Found),
        ?assertEqual(1, AppCount),
        ?assert(AppMemory > 0),
        ?assert(AppReds >= 0),

        Unknown = observer_cli_application:collect_app_info(AllApps, #{}, [Child], self()),
        {UnknownCount, UnknownMemory, UnknownReds, _UnknownMsgQ, "Unknown", "unknown"} =
            maps:get(no_group, Unknown),
        ?assertEqual(1, UnknownCount),
        ?assert(UnknownMemory > 0),
        ?assert(UnknownReds >= 0)
    after
        exit(Child, kill)
    end.

start_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_application:start(Opts))
        end
    ).

start_manager_branches_test() ->
    Inputs = ["r\n", "m\n", "mq\n", "p\n", "2000\n", "pd\n", "pu\n", "x\n", "q\n"],
    observer_cli_test_io:with_input(
        Inputs,
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_application:start(Opts))
        end
    ).

find_group_leader_test() ->
    ?assert(is_pid(observer_cli_application:find_group_leader(self()))).

render_app_info_wide_layout_test() ->
    Base = app_row_widths(80),
    Wide = app_row_widths(180),
    ?assertEqual([1, 3, 4, 6, 7], unchanged_columns(Base, Wide, [1, 3, 4, 6, 7])),
    ?assertEqual([2, 5, 8], wider_columns(Base, Wide, [2, 5, 8])).

app_row_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Row | _] = observer_cli_application:render_app_info(
                20, 1, {proc_count, 1}
            ),
            {observer_cli_test_io:column_widths(Title), observer_cli_test_io:column_widths(Row)}
        end
    ).

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

-endif.
