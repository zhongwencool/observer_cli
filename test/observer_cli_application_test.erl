-module(observer_cli_application_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

-export([start/2, stop/1, init/1, start_fixture_child/0]).

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

find_remote_group_leader_test() ->
    RemotePid = binary_to_term(
        <<131, 103, 100, 0, 11, "remote@host", 0, 0, 0, 1, 0, 0, 0, 0, 0>>
    ),
    ?assertEqual(no_group, observer_cli_application:find_group_leader(RemotePid)).

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
        ?assertEqual(<<"group_leader_chain">>, maps:get(<<"attribution_semantics">>, Data)),
        ?assertEqual(<<"fixture_list">>, maps:get(<<"inventory_path">>, Data)),
        ?assertEqual(2, maps:get(<<"scanned_count">>, Data)),
        ?assertEqual(0, maps:get(<<"unattributed_process_count">>, Data)),
        Items = maps:get(<<"items">>, Data),
        Item = hd([I || #{<<"application">> := <<"observer_cli_goal08_app">>} = I <- Items]),
        ?assertEqual(<<"observer_cli_goal08_app">>, maps:get(<<"application">>, Item)),
        ?assertEqual(2, maps:get(<<"process_count">>, Item)),
        ?assertEqual(300, maps:get(<<"memory_bytes">>, Item)),
        ?assertEqual(true, maps:get(<<"loaded">>, Item)),
        ?assertEqual(true, maps:get(<<"running">>, Item)),
        ?assertEqual(<<"1">>, maps:get(<<"version">>, Item)),
        NoGroup = hd([I || #{<<"application">> := <<"no_group">>} = I <- Items]),
        ?assertEqual(0, maps:get(<<"process_count">>, NoGroup)),
        ?assertEqual(null, maps:get(<<"version">>, NoGroup)),
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

application_counts_are_separate_from_process_scan_test() ->
    Apps = [{app_a, "fixture", "1"}, {app_b, "fixture", "1"}],
    ProcessSource = diagnostics_process_source([], fun(_Pid, _Keys) -> undefined end),
    AppSource = #{
        loaded_fun => fun() -> Apps end,
        running_fun => fun(_Timeout) -> [] end,
        supervisor_fun => fun(_App) -> undefined end,
        root_info_fun => fun(_Root, _Key) -> undefined end
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} = observer_cli_snapshot:dispatch(
        self(),
        applications,
        #{test_process_source => ProcessSource, test_application_source => AppSource},
        #{timeout_ms => 3000, identifier_policy => include}
    ),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(3, maps:get(<<"scanned_count">>, Data)),
    ?assertEqual(3, maps:get(<<"eligible_count">>, Data)),
    ?assertEqual(3, maps:get(<<"returned_count">>, Data)),
    ?assertEqual(0, maps:get(<<"process_scanned_count">>, Data)),
    ?assertEqual(0, maps:get(<<"process_eligible_count">>, Data)),
    ?assertEqual(
        ok, observer_cli_escriptize:validate_response(applications, include, node(), Response)
    ),
    receive
        {process_fold, []} -> ok
    end.

applications_follow_group_leader_chain_and_aggregate_no_group_test() ->
    App = observer_cli_goal08_chain_app,
    Leader = spawn(fun application_fixture/0),
    Intermediate = spawn(fun application_fixture/0),
    Unknown = spawn(fun application_fixture/0),
    Root = spawn(fun application_fixture/0),
    [Attributed, Unattributed] =
        Processes = [
            spawn(fun application_fixture/0), spawn(fun application_fixture/0)
        ],
    ProcessSource = diagnostics_process_source(Processes, fun
        (Pid, Keys) when Pid =:= Attributed, is_list(Keys) ->
            [
                {memory, 100},
                {message_queue_len, 2},
                {reductions, 300},
                {group_leader, Intermediate}
            ];
        (Pid, Keys) when Pid =:= Unattributed, is_list(Keys) ->
            [
                {memory, 40},
                {message_queue_len, 4},
                {reductions, 80},
                {group_leader, Unknown}
            ];
        (Pid, group_leader) when Pid =:= Intermediate ->
            {group_leader, Leader};
        (Pid, group_leader) when Pid =:= Unknown ->
            {group_leader, Unknown}
    end),
    AppSource = #{
        loaded_fun => fun() -> [{App, "fixture", "1.2.3"}] end,
        running_fun => fun(_Timeout) -> [{App, "fixture", "1.2.3"}] end,
        supervisor_fun => fun(Requested) when Requested =:= App -> {ok, Root} end,
        root_info_fun => fun(Requested, group_leader) when Requested =:= Root ->
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
        ?assertEqual(1, maps:get(<<"unattributed_process_count">>, Data)),
        Items = maps:get(<<"items">>, Data),
        AppItem = hd([I || #{<<"application">> := <<"observer_cli_goal08_chain_app">>} = I <- Items]),
        ?assertEqual(100, maps:get(<<"memory_bytes">>, AppItem)),
        ?assertEqual(<<"1.2.3">>, maps:get(<<"version">>, AppItem)),
        NoGroup = hd([I || #{<<"application">> := <<"no_group">>} = I <- Items]),
        ?assertEqual(1, maps:get(<<"process_count">>, NoGroup)),
        ?assertEqual(40, maps:get(<<"memory_bytes">>, NoGroup)),
        ?assertEqual(80, maps:get(<<"reductions">>, NoGroup)),
        ?assertEqual(4, maps:get(<<"message_queue_len">>, NoGroup)),
        ?assertEqual(false, maps:get(<<"loaded">>, NoGroup)),
        ?assertEqual(false, maps:get(<<"running">>, NoGroup)),
        ?assertEqual(null, maps:get(<<"version">>, NoGroup)),
        receive
            {process_fold, Processes} -> ok
        end
    after
        lists:foreach(
            fun(Pid) -> exit(Pid, kill) end,
            [Leader, Intermediate, Unknown, Root | Processes]
        )
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

supervision_tree_public_static_and_dynamic_fixtures_test_() ->
    {timeout, 10, fun supervision_tree_public_static_and_dynamic_fixtures/0}.

supervision_tree_public_static_and_dynamic_fixtures() ->
    StaticApp = observer_cli_goal13_static,
    DynamicApp = observer_cli_goal13_dynamic,
    try
        ok = load_fixture_application(StaticApp, static),
        ok = application:start(StaticApp),
        {ok, StaticRoot} = application:get_supervisor(StaticApp),
        Static = supervision_tree(StaticApp, #{}),
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, Static)),
        ?assertEqual(list_to_binary(pid_to_list(StaticRoot)), maps:get(<<"root">>, Static)),
        [StaticChild] = maps:get(<<"children">>, Static),
        ?assertEqual(<<"available">>, maps:get(<<"identity">>, StaticChild)),
        ?assertEqual(<<"atom:static_child">>, maps:get(<<"id">>, StaticChild)),
        ?assertEqual(true, maps:get(<<"leaf">>, StaticChild)),

        ok = load_fixture_application(DynamicApp, dynamic),
        ok = application:start(DynamicApp),
        {ok, DynamicRoot} = application:get_supervisor(DynamicApp),
        {ok, _DynamicChild} = supervisor:start_child(DynamicRoot, []),
        Dynamic = supervision_tree(DynamicApp, #{}),
        [DynamicItem] = maps:get(<<"children">>, Dynamic),
        ?assertEqual(<<"unavailable">>, maps:get(<<"identity">>, DynamicItem)),
        ?assertEqual(<<"dynamic_id">>, maps:get(<<"identity_reason">>, DynamicItem)),
        ?assertEqual(<<"aggregate_only">>, maps:get(<<"churn_semantics">>, DynamicItem))
    after
        stop_fixture_application(DynamicApp),
        stop_fixture_application(StaticApp)
    end.

supervision_tree_bounds_and_normalizes_child_identity_test() ->
    Parent = self(),
    Root = spawn(fun application_fixture/0),
    Child = spawn(fun application_fixture/0),
    Remote = remote_pid_fixture(),
    HugeInteger = 1 bsl 2048,
    Children = [
        {safe_atom, Child, worker, [secret_module]},
        {42, restarting, supervisor, [secret_module]},
        {<<"safe-binary">>, Remote, worker, [secret_module]},
        {undefined, undefined, worker, [secret_module]},
        {duplicate, Child, worker, [secret_module]},
        {duplicate, Child, worker, [secret_module]},
        {{complex, <<"fixture-child-secret">>}, Child, worker, [secret_module]},
        {binary:copy(<<"x">>, 129), Child, worker, [secret_module]},
        {HugeInteger, Child, worker, [secret_module]}
    ],
    Source = supervision_source(Root, Children, Parent),
    try
        Data = supervision_tree(observer_cli_goal13_fixture, Source),
        ?assertEqual(9, maps:get(<<"observed_child_count">>, Data)),
        ?assertEqual(6, maps:get(<<"identity_unavailable_count">>, Data)),
        Items = maps:get(<<"children">>, Data),
        ?assertEqual(
            [<<"available">>, <<"available">>, <<"available">>],
            [maps:get(<<"identity">>, Item) || Item <- lists:sublist(Items, 3)]
        ),
        ?assertEqual(
            [
                <<"dynamic_id">>,
                <<"duplicate_id">>,
                <<"duplicate_id">>,
                <<"complex_id">>,
                <<"oversized_id">>,
                <<"oversized_id">>
            ],
            [
                maps:get(<<"identity_reason">>, Item)
             || Item <- lists:nthtail(3, Items)
            ]
        ),
        RemoteItem = lists:nth(3, Items),
        ?assertEqual(<<"remote">>, maps:get(<<"location">>, maps:get(<<"child">>, RemoteItem))),
        ?assertEqual(
            nomatch, binary:match(term_to_binary(Data), <<"fixture-child-secret">>)
        ),
        ?assertEqual(nomatch, binary:match(term_to_binary(Data), <<"secret_module">>)),
        Redacted = supervision_tree_response(
            observer_cli_goal13_fixture,
            supervision_source(
                Root, [{<<"fixture-scalar-secret">>, Child, worker, []}], Parent
            ),
            redact
        ),
        ?assertEqual(
            nomatch, binary:match(term_to_binary(Redacted), <<"fixture-scalar-secret">>)
        ),
        [RedactedItem] = maps:get(<<"children">>, maps:get(<<"data">>, Redacted)),
        ?assertEqual(<<"child-1">>, maps:get(<<"id">>, RedactedItem)),
        [
            receive
                {count_children, Root} -> ok
            end
         || _ <- [1, 2]
        ],
        [
            receive
                {which_children, Root} -> ok
            end
         || _ <- [1, 2]
        ],
        receive
            {which_children, Root} -> ?assert(false)
        after 50 -> ok
        end
    after
        flush_supervision_messages(),
        exit(Child, kill),
        exit(Root, kill)
    end.

supervision_tree_preflight_and_soft_output_cap_test() ->
    Parent = self(),
    Root = spawn(fun application_fixture/0),
    RefusedSource = (supervision_source(Root, [], Parent))#{
        count_children_fun => fun(RequestedRoot) ->
            Parent ! {count_children, RequestedRoot},
            [{specs, 301}, {active, 1}, {supervisors, 0}, {workers, 1}]
        end
    },
    try
        Refused = supervision_tree(observer_cli_goal13_fixture, RefusedSource),
        ?assertEqual(<<"unavailable">>, maps:get(<<"status">>, Refused)),
        ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Refused)),
        receive
            {which_children, Root} -> ?assert(false)
        after 50 -> ok
        end,
        Children = [{N, Root, worker, []} || N <- lists:seq(1, 101)],
        CappedResponse = supervision_tree_response(
            observer_cli_goal13_fixture, supervision_source(Root, Children, Parent)
        ),
        Capped = maps:get(<<"data">>, CappedResponse),
        ?assertEqual(101, maps:get(<<"observed_child_count">>, Capped)),
        ?assertEqual(100, maps:get(<<"returned_count">>, Capped)),
        ?assertEqual(1, maps:get(<<"dropped_count">>, Capped)),
        ?assertEqual(100, length(maps:get(<<"children">>, Capped))),
        ?assertEqual(
            #{
                <<"child_id_canonical_bytes">> => 128,
                <<"output_child_count">> => 100,
                <<"scan_budget_count">> => 300
            },
            maps:get(<<"limits">>, Capped)
        ),
        Acquisition = maps:get(<<"acquisition">>, Capped),
        ?assertEqual(<<"o_children">>, maps:get(<<"count_children_complexity">>, Acquisition)),
        ?assertEqual(false, maps:get(<<"snapshot_atomic">>, Acquisition)),
        ?assertEqual(
            false, maps:get(<<"deadline_retracts_delivered_request">>, Acquisition)
        ),
        WarningReasons = [
            maps:get(<<"reason_code">>, Warning)
         || Warning <-
                maps:get(<<"issues">>, CappedResponse)
        ],
        ?assert(lists:member(<<"supervisor_snapshot_is_non_atomic">>, WarningReasons)),
        ?assert(lists:member(<<"deadline_does_not_retract_infinity_calls">>, WarningReasons))
    after
        flush_supervision_messages(),
        exit(Root, kill)
    end.

supervision_tree_post_preflight_growth_is_refused_test() ->
    Parent = self(),
    Root = spawn(fun application_fixture/0),
    Child = spawn(fun application_fixture/0),
    AliveFun = fun(Pid) ->
        Parent ! {alive, Pid},
        true
    end,
    Children = [{N, Child, worker, []} || N <- lists:seq(1, 301)],
    Source = (supervision_source(Root, Children, Parent))#{
        alive_fun := AliveFun,
        count_children_fun := fun(RequestedRoot) ->
            Parent ! {count_children, RequestedRoot},
            [{specs, 300}, {active, 300}, {supervisors, 0}, {workers, 300}]
        end
    },
    try
        Refused = supervision_tree(observer_cli_goal13_fixture, Source),
        ?assertEqual(<<"unavailable">>, maps:get(<<"status">>, Refused)),
        ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Refused)),
        ?assertEqual(301, maps:get(<<"observed_child_count">>, Refused)),
        ?assertEqual([], maps:get(<<"children">>, Refused)),
        receive
            {alive, Root} -> ok
        end,
        receive
            {count_children, Root} -> ok
        end,
        receive
            {which_children, Root} -> ok
        end,
        receive
            {alive, Child} -> ?assert(false)
        after 50 -> ok
        end,

        ImproperSource = (supervision_source(Root, [], Parent))#{
            alive_fun := AliveFun,
            count_children_fun := fun(RequestedRoot) ->
                Parent ! {count_children, RequestedRoot},
                [{specs, 1}, {active, 1}, {supervisors, 0}, {workers, 1}]
            end,
            which_children_fun := fun(RequestedRoot) ->
                Parent ! {which_children, RequestedRoot},
                [{malformed, Child, worker, []} | improper]
            end
        },
        Malformed = supervision_tree(observer_cli_goal13_fixture, ImproperSource),
        ?assertEqual(<<"error">>, maps:get(<<"status">>, Malformed)),
        ?assertEqual(<<"supervisor_children_failed">>, maps:get(<<"reason_code">>, Malformed)),
        receive
            {alive, Root} -> ok
        end,
        receive
            {count_children, Root} -> ok
        end,
        receive
            {which_children, Root} -> ok
        end,
        receive
            {alive, Child} -> ?assert(false)
        after 50 -> ok
        end
    after
        flush_supervision_messages(),
        exit(Child, kill),
        exit(Root, kill)
    end.

supervision_tree_real_child_boundaries_test_() ->
    {timeout, 30, fun supervision_tree_real_child_boundaries/0}.

supervision_tree_real_child_boundaries() ->
    Fixtures = [
        {observer_cli_goal13_100, 100, <<"ok">>, 100, 0},
        {observer_cli_goal13_101, 101, <<"ok">>, 100, 1},
        {observer_cli_goal13_300, 300, <<"ok">>, 100, 200},
        {observer_cli_goal13_301, 301, <<"unavailable">>, 0, 301}
    ],
    lists:foreach(
        fun({App, Count, Status, Returned, Dropped}) ->
            try
                ok = load_fixture_application(App, {bounded, Count}),
                ok = application:start(App),
                Data = supervision_tree(App, #{}),
                ?assertEqual(Status, maps:get(<<"status">>, Data)),
                ?assertEqual(Count, maps:get(<<"observed_child_count">>, Data)),
                ?assertEqual(Returned, length(maps:get(<<"children">>, Data))),
                case Status of
                    <<"ok">> ->
                        ?assertEqual(Dropped, maps:get(<<"dropped_count">>, Data));
                    <<"unavailable">> ->
                        ?assertEqual(
                            <<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)
                        )
                end
            after
                stop_fixture_application(App)
            end
        end,
        Fixtures
    ).

supervision_tree_public_outcome_normalization_test() ->
    Root = spawn(fun application_fixture/0),
    Base = supervision_source(Root, [], self()),
    try
        NotFound = supervision_tree(unknown_goal13_application, Base),
        ?assertEqual(<<"not_found">>, maps:get(<<"status">>, NotFound)),
        NotRunning = supervision_tree(
            observer_cli_goal13_fixture, Base#{supervisor_fun => fun(_App) -> undefined end}
        ),
        ?assertEqual(<<"not_running">>, maps:get(<<"status">>, NotRunning)),
        Failed = supervision_tree_response(
            observer_cli_goal13_fixture,
            Base#{supervisor_fun => fun(_App) -> {error, <<"fixture-supervisor-secret">>} end}
        ),
        ?assertEqual(<<"partial">>, maps:get(<<"outcome">>, Failed)),
        ?assertEqual(
            nomatch, binary:match(term_to_binary(Failed), <<"fixture-supervisor-secret">>)
        ),
        Exited = supervision_tree_response(
            observer_cli_goal13_fixture,
            Base#{supervisor_fun => fun(_App) -> exit(fixture_supervisor_exit) end}
        ),
        ?assertEqual(
            <<"supervisor_resolution_failed">>, response_probe_reason(Exited)
        ),
        Dead = spawn(fun() -> ok end),
        DeadRef = monitor(process, Dead),
        receive
            {'DOWN', DeadRef, process, Dead, _} -> ok
        end,
        DeadRoot = supervision_tree(
            observer_cli_goal13_fixture, Base#{supervisor_fun => fun(_App) -> {ok, Dead} end}
        ),
        ?assertEqual(<<"not_running">>, maps:get(<<"status">>, DeadRoot)),
        RemoteRoot = supervision_tree_response(
            observer_cli_goal13_fixture,
            Base#{supervisor_fun => fun(_App) -> {ok, remote_pid_fixture()} end}
        ),
        ?assertEqual(<<"remote_supervisor_root">>, response_probe_reason(RemoteRoot))
    after
        exit(Root, kill)
    end.

load_fixture_application(App, Mode) ->
    application:load(
        {application, App, [
            {description, "observer_cli supervision fixture"},
            {vsn, "1"},
            {modules, [?MODULE]},
            {registered, []},
            {applications, [kernel, stdlib]},
            {mod, {?MODULE, Mode}}
        ]}
    ).

stop_fixture_application(App) ->
    _ = application:stop(App),
    _ = application:unload(App),
    ok.

start(_StartType, Mode) ->
    supervisor:start_link(?MODULE, Mode).

stop(_State) ->
    ok.

init(static) ->
    {ok,
        {{one_for_one, 1, 5}, [
            {static_child, {?MODULE, start_fixture_child, []}, permanent, 5000, worker, [?MODULE]}
        ]}};
init(dynamic) ->
    {ok,
        {{simple_one_for_one, 1, 5}, [
            {dynamic_child, {?MODULE, start_fixture_child, []}, temporary, 5000, worker, [?MODULE]}
        ]}};
init({bounded, Count}) ->
    Children = [
        {N, {?MODULE, start_fixture_child, []}, temporary, 5000, worker, [?MODULE]}
     || N <- lists:seq(1, Count)
    ],
    {ok, {{one_for_one, 1, 5}, Children}}.

start_fixture_child() ->
    {ok, spawn_link(fun application_fixture/0)}.

supervision_tree(App, Source) ->
    maps:get(<<"data">>, supervision_tree_response(App, Source)).

supervision_tree_response(App, Source) ->
    supervision_tree_response(App, Source, include).

supervision_tree_response(App, Source, Policy) ->
    Request =
        case Source of
            #{} when map_size(Source) =:= 0 -> #{app => atom_to_binary(App)};
            _ -> #{app => atom_to_binary(App), test_application_source => Source}
        end,
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            supervision_tree,
            Request,
            #{timeout_ms => 3000, identifier_policy => Policy}
        ),
    Response.

response_probe_reason(Response) ->
    #{<<"meta">> := #{<<"capture">> := #{<<"probes">> := [Probe]}}} = Response,
    maps:get(<<"reason_code">>, Probe).

supervision_source(Root, Children, Parent) ->
    #{
        loaded_fun => fun() -> [{observer_cli_goal13_fixture, "fixture", "1"}] end,
        running_fun => fun(_Timeout) -> [] end,
        supervisor_fun => fun(_App) -> {ok, Root} end,
        root_info_fun => fun erlang:process_info/2,
        alive_fun => fun erlang:is_process_alive/1,
        count_children_fun => fun(RequestedRoot) ->
            Parent ! {count_children, RequestedRoot},
            [
                {specs, length(Children)},
                {active, length(Children)},
                {supervisors, 0},
                {workers, length(Children)}
            ]
        end,
        which_children_fun => fun(RequestedRoot) ->
            Parent ! {which_children, RequestedRoot},
            Children
        end
    }.

remote_pid_fixture() ->
    binary_to_term(<<131, 103, 100, 0, 9, "undefined", 0, 0, 0, 1, 0, 0, 0, 0, 0>>, [safe]).

flush_supervision_messages() ->
    receive
        {count_children, _Root} -> flush_supervision_messages();
        {which_children, _Root} -> flush_supervision_messages()
    after 0 ->
        ok
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

dead_application_leader_test() ->
    Dead = spawn(fun() -> ok end),
    Mon = erlang:monitor(process, Dead),
    receive
        {'DOWN', Mon, process, Dead, normal} -> ok
    end,
    ?assertEqual(
        #{},
        observer_cli_application:leader_info([
            {running, [{app, Dead}, invalid]}
        ])
    ).

-endif.
