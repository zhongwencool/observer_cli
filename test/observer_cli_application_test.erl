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

-endif.
