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

collect_app_info_structure_test() ->
    Info = observer_cli_application:collect_app_info(),
    ?assert(is_map(Info)),
    ?assert(maps:is_key(no_group, Info)),
    ?assert(lists:all(fun app_info_entry/1, maps:to_list(Info))).

app_info_entry({_App, {Count, Memory, Reductions, MsgQueueLen, Status, Version}}) ->
    is_integer(Count) andalso Count >= 0 andalso
        is_integer(Memory) andalso Memory >= 0 andalso
        is_integer(Reductions) andalso Reductions >= 0 andalso
        is_integer(MsgQueueLen) andalso MsgQueueLen >= 0 andalso
        is_list(Status) andalso
        is_list(Version);
app_info_entry(_) ->
    false.

app_render_info_sorting_test() ->
    AppInfo = #{
        high_app => {3, 30, 300, 2, "Started", "1.0"},
        low_app => {1, 10, 100, 0, "Started", "1.0"},
        mid_app => {2, 20, 200, 1, "Loaded", "1.0"}
    },
    ?assertEqual(
        {1, [
            {0, {3, "Started"}, [high_app, 3, 30, 300, 2, "Started", "1.0"]},
            {0, {2, "Loaded"}, [mid_app, 2, 20, 200, 1, "Loaded", "1.0"]}
        ]},
        observer_cli_application:app_render_info(AppInfo, 2, 1, {proc_count, 1})
    ),
    ?assertEqual(
        {3, [{0, {1, "Started"}, [low_app, 1, 10, 100, 0, "Started", "1.0"]}]},
        observer_cli_application:app_render_info(AppInfo, 2, 2, {proc_count, 1})
    ).

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

start_redraw_test() ->
    observer_cli_test_io:with_input(
        [{sleep, 30, "q\n"}],
        fun() ->
            Opts = #view_opts{auto_row = false, app = #app{interval = 1}},
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

diagnostics_application_attribution_uses_one_public_inventory_test() ->
    Parent = self(),
    App = observer_cli_goal08_app,
    Leader = spawn(fun application_fixture/0),
    Root = spawn(fun application_fixture/0),
    Processes = [spawn(fun application_fixture/0), spawn(fun application_fixture/0)],
    Values = maps:from_list(lists:zip(Processes, [100, 200])),
    ProcessSource = diagnostics_process_source(Processes, fun(Pid, Keys) ->
        Parent ! {app_process_info, Keys},
        [
            {memory, maps:get(Pid, Values)},
            {message_queue_len, 1},
            {reductions, maps:get(Pid, Values) * 2},
            {group_leader, Leader}
        ]
    end),
    AppSource = #{
        loaded_fun => fun() -> [{App, "fixture", "1"}] end,
        running_fun => fun(_Timeout) -> [{App, "fixture", "1"}] end,
        supervisor_fun => fun(Requested) ->
            Parent ! {supervisor, Requested},
            {ok, Root}
        end,
        root_info_fun => fun(RequestedRoot, Key) ->
            Parent ! {root_info, RequestedRoot, Key},
            {group_leader, Leader}
        end
    },
    try
        #{<<"status">> := <<"ok">>, <<"result">> := Response} =
            observer_cli_snapshot:dispatch(
                self(),
                applications,
                #{
                    sort => memory,
                    limit => 20,
                    test_process_source => ProcessSource,
                    test_application_source => AppSource
                },
                #{timeout_ms => 3000, identifier_policy => include}
            ),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(<<"group_leader_application">>, maps:get(<<"attribution">>, Data)),
        ?assertEqual(<<"approximation">>, maps:get(<<"attribution_semantics">>, Data)),
        ?assertEqual(<<"fixture_list">>, maps:get(<<"inventory_path">>, Data)),
        ?assertEqual(2, maps:get(<<"scanned_count">>, Data)),
        ?assertEqual(0, maps:get(<<"unattributed_process_count">>, Data)),
        [Item] = maps:get(<<"items">>, Data),
        ?assertEqual(<<"observer_cli_goal08_app">>, maps:get(<<"application">>, Item)),
        ?assertEqual(2, maps:get(<<"process_count">>, Item)),
        ?assertEqual(300, maps:get(<<"memory_bytes">>, Item)),
        ?assertEqual(true, maps:get(<<"loaded">>, Item)),
        ?assertEqual(true, maps:get(<<"running">>, Item)),
        receive
            {process_fold, Processes} -> ok
        end,
        InfoKeys = [
            receive
                {app_process_info, Keys} -> Keys
            end
         || _ <- Processes
        ],
        ?assert(
            lists:all(
                fun(Keys) ->
                    Keys =:= [memory, message_queue_len, reductions, group_leader]
                end,
                InfoKeys
            )
        ),
        receive
            {supervisor, App} -> ok
        end,
        receive
            {root_info, Root, group_leader} -> ok
        end,
        receive
            {process_fold, _} -> ?assert(false)
        after 50 -> ok
        end
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, [Leader, Root | Processes])
    end.

application_post_enumeration_refusal_skips_attribution_test() ->
    Parent = self(),
    Apps = [{Number, "fixture", "1"} || Number <- lists:seq(1, 5001)],
    AppSource = #{
        loaded_fun => fun() -> Apps end,
        running_fun => fun(_Timeout) -> [] end,
        supervisor_fun => fun(_App) ->
            Parent ! supervisor_called,
            undefined
        end,
        root_info_fun => fun(_Root, _Key) ->
            Parent ! root_info_called,
            undefined
        end
    },
    ProcessSource = diagnostics_process_source([], fun(_Pid, _Keys) -> undefined end),
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            applications,
            #{test_process_source => ProcessSource, test_application_source => AppSource},
            #{timeout_ms => 3000, identifier_policy => include}
        ),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)),
    ?assertEqual(<<"post_enumeration">>, maps:get(<<"admission_stage">>, Data)),
    receive
        supervisor_called -> ?assert(false)
    after 50 -> ok
    end,
    receive
        root_info_called -> ?assert(false)
    after 50 -> ok
    end,
    receive
        {process_fold, _} -> ?assert(false)
    after 50 -> ok
    end.

diagnostics_process_source(Pids, InfoFun) ->
    Parent = self(),
    #{
        count_fun => fun() -> length(Pids) end,
        fold =>
            {fixture_list, fun(Fun, Acc) ->
                Parent ! {process_fold, Pids},
                lists:foldl(Fun, Acc, Pids)
            end},
        info_fun => InfoFun,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> 0 end,
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    }.

application_fixture() ->
    receive
        stop -> ok
    end.

-endif.
