-module(observer_cli_scheduler_summary_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

pool_weighting_and_online_membership_test() ->
    {First, Last} = samples(),
    Summary = observer_cli_snapshot:scheduler_busy_window(First, Last),
    ?assertEqual(valid, maps:get(status, Summary)),
    ?assert(abs(maps:get(utilization_ratio, maps:get(normal, Summary)) - 140 / 300) < 0.000001),
    ?assert(abs(maps:get(utilization_ratio, maps:get(dirty_cpu, Summary)) - 60 / 200) < 0.000001),
    ?assertEqual("N 47% / D 30%", lists:flatten(observer_cli:render_scheduler_summary(Summary))),
    {Usage, Summary, Last} = observer_cli:scheduler_stats(stats(First), stats(Last)),
    ?assertEqual(6, length(Usage)).

missing_pool_and_invalid_window_test() ->
    {First, Last} = samples(),
    Topology = (maps:get(topology, First))#{dirty_cpu_schedulers_online := 0},
    Summary = observer_cli_snapshot:scheduler_busy_window(First#{topology := Topology}, Last#{
        topology := Topology
    }),
    ?assertEqual("N 47% / D n/a", lists:flatten(observer_cli:render_scheduler_summary(Summary))),
    Changed = Last#{topology := Topology},
    ?assertEqual(
        topology_changed,
        maps:get(
            reason_code,
            observer_cli_snapshot:scheduler_busy_window(First, Changed)
        )
    ),
    {undefined, #{status := unavailable}, Baseline} = observer_cli:scheduler_stats(
        stats(First), stats(Changed)
    ),
    ?assertEqual(undefined, maps:get(wall_time, Baseline)),
    ?assertMatch(
        {undefined, #{status := warming_up}, Changed},
        observer_cli:scheduler_stats(stats(Baseline), stats(Changed))
    ),
    Zero = First,
    ?assertEqual(
        zero_denominator,
        maps:get(
            reason_code,
            observer_cli_snapshot:scheduler_busy_window(First, Zero)
        )
    ),
    ?assertMatch(
        {undefined, #{status := unavailable}, _},
        observer_cli:scheduler_stats(stats(First), stats(Zero))
    ),
    ?assertEqual(
        invalid_sample, maps:get(reason_code, observer_cli_snapshot:scheduler_busy_window(#{}, #{}))
    ),
    Reset = Last#{
        wall_time := [{1, 0, 0}, {2, 150, 300}, {3, 0, 0}, {4, 0, 0}, {5, 110, 200}, {6, 150, 200}]
    },
    ?assertEqual(
        counter_reset,
        maps:get(reason_code, observer_cli_snapshot:scheduler_busy_window(First, Reset))
    ),
    Duplicate = Last#{wall_time := [{1, 0, 0}, {1, 1, 1}]},
    ?assertEqual(
        duplicate_scheduler_id,
        maps:get(
            reason_code,
            observer_cli_snapshot:scheduler_busy_window(First, Duplicate)
        )
    ).

summary_does_not_add_sampling_or_registration_test_() ->
    {spawn, fun() ->
        _ = erlang:system_flag(scheduler_wall_time, true),
        try
            ?assertEqual(
                #{wall => 0, flag => 0},
                trace_measurement_reads(fun() ->
                    observer_cli:get_incremental_stats(?DISABLE)
                end)
            ),
            ?assertEqual(
                #{wall => 2, flag => 0},
                trace_measurement_reads(fun() ->
                    A = observer_cli:get_incremental_stats(?ENABLE),
                    timer:sleep(10),
                    B = observer_cli:get_incremental_stats(?ENABLE),
                    observer_cli:scheduler_stats(A, B)
                end)
            )
        after
            erlang:system_flag(scheduler_wall_time, false)
        end
    end}.

summary_layout_and_switch_test() ->
    {First, Last} = samples(),
    Summary = observer_cli_snapshot:scheduler_busy_window(First, Last),
    Metrics = #{
        cpu => #{status => available, percent => 236.0, interval_us => 1510000},
        rss_bytes => 182 * 1048576,
        rss_delta_bytes => 3 * 1048576
    },
    lists:foreach(
        fun(Columns) ->
            observer_cli_test_io:with_geometry(24, Columns, [], fun() ->
                Stable = observer_cli:get_stable_system_info(),
                Off = observer_cli:render_system_line(Metrics, Stable),
                On = observer_cli:render_system_line(
                    Metrics#{scheduler_summary => Summary}, Stable
                ),
                ?assertEqual(
                    length(observer_cli_test_io:line_widths(Off)),
                    length(observer_cli_test_io:line_widths(On))
                ),
                observer_cli_test_io:assert_stable_fragments(Off, [
                    "Version", "BEAM CPU", "BEAM RSS"
                ]),
                observer_cli_test_io:assert_stable_fragments(On, [
                    "Sched busy", "N 47% / D 30%", "BEAM CPU", "BEAM RSS"
                ]),
                ?assertEqual(nomatch, string:find(observer_cli_test_io:plain(On), " Version")),
                ?assert(
                    lists:all(
                        fun(W) -> W =< observer_cli_lib:layout_width() end,
                        observer_cli_test_io:line_widths(On)
                    )
                )
            end)
        end,
        [139, 160, 201]
    ),
    ?assertEqual("warming up", observer_cli:render_scheduler_summary(#{status => warming_up})),
    ?assertEqual("unavailable", observer_cli:render_scheduler_summary(#{status => unavailable})),
    ?assertEqual(scheduler_usage, observer_cli_command:parse_shared("`\n")),
    ?assertEqual(undefined, element(5, observer_cli:get_incremental_stats(?DISABLE))).

first_resume_and_toggle_test_() ->
    {spawn, fun() ->
        {quit, Output} = observer_cli_test_io:capture_with_geometry(
            24,
            201,
            [{sleep, 120, "p\n"}, {sleep, 80, "p\n"}, {sleep, 120, "`\n"}, {sleep, 80, "q\n"}],
            fun() ->
                observer_cli:start(#view_opts{
                    home = #home{scheduler_usage = ?ENABLE, interval = 20}
                })
            end
        ),
        Lines = string:split(observer_cli_test_io:plain(Output), "\n", all),
        Warming = [
            L
         || L <- Lines,
            string:find(L, "Sched busy") =/= nomatch,
            string:find(L, "warming up") =/= nomatch
        ],
        ?assertEqual(2, length(Warming)),
        observer_cli_test_io:assert_stable_fragments(Output, [
            "N ", " / D ", "PAUSE", "Version", "BEAM CPU", "BEAM RSS"
        ]),
        ?assertEqual(undefined, erlang:statistics(scheduler_wall_time))
    end}.

render_worker_crash_releases_its_registration_test_() ->
    {spawn, fun() ->
        ?assertEqual(undefined, erlang:statistics(scheduler_wall_time)),
        Parent = self(),
        observer_cli_test_io:with_geometry(24, 139, [], fun() ->
            {Worker, Monitor} = spawn_monitor(fun() ->
                observer_cli:run_home_worker(
                    #{platform => unsupported, identity => {node(), os:getpid()}, timeout_ms => 20},
                    Parent,
                    #home{scheduler_usage = ?ENABLE, interval = 20},
                    false
                )
            end),
            wait_for_registration(100),
            exit(Worker, simulated_render_failure),
            receive
                {'DOWN', Monitor, process, Worker, simulated_render_failure} -> ok
            after 1000 -> ?assert(false)
            end,
            wait_until_disabled(100)
        end)
    end}.

stats(Scheduler) -> {0, 0, 0, 0, Scheduler}.

samples() ->
    Topology = #{
        schedulers_configured => 4, schedulers_online => 2, dirty_cpu_schedulers_online => 2
    },
    First = #{
        topology => Topology,
        wall_time => [
            {1, 100, 100}, {2, 100, 100}, {3, 0, 0}, {4, 0, 0}, {5, 100, 100}, {6, 100, 100}
        ]
    },
    Last = #{
        topology => Topology,
        wall_time => [
            {6, 150, 200}, {5, 110, 200}, {4, 0, 0}, {3, 0, 0}, {2, 150, 300}, {1, 190, 200}
        ]
    },
    {First, Last}.

trace_measurement_reads(Fun) ->
    Parent = self(),
    Worker = spawn(fun() ->
        receive
            run ->
                Fun(),
                Parent ! measured,
                receive
                    stop -> ok
                end
        end
    end),
    erlang:trace_pattern({erlang, statistics, 1}, [{[scheduler_wall_time], [], []}], [local]),
    erlang:trace_pattern({erlang, system_flag, 2}, [{[scheduler_wall_time, '_'], [], []}], [local]),
    try
        erlang:trace(Worker, true, [call, {tracer, Parent}]),
        Worker ! run,
        receive
            measured -> ok
        after 1000 -> ?assert(false)
        end,
        Ref = erlang:trace_delivered(Worker),
        receive
            {trace_delivered, Worker, Ref} -> ok
        after 1000 -> ?assert(false)
        end,
        count_reads(Worker, #{wall => 0, flag => 0})
    after
        exit(Worker, kill),
        erlang:trace_pattern({erlang, statistics, 1}, false, [local]),
        erlang:trace_pattern({erlang, system_flag, 2}, false, [local])
    end.

count_reads(Worker, Counts) ->
    receive
        {trace, Worker, call, {erlang, statistics, [scheduler_wall_time]}} ->
            count_reads(Worker, Counts#{wall := maps:get(wall, Counts) + 1});
        {trace, Worker, call, {erlang, system_flag, [scheduler_wall_time, _]}} ->
            count_reads(Worker, Counts#{flag := maps:get(flag, Counts) + 1})
    after 0 -> Counts
    end.

wait_until_disabled(0) ->
    ?assertEqual(undefined, erlang:statistics(scheduler_wall_time));
wait_until_disabled(N) ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            ok;
        _ ->
            timer:sleep(5),
            wait_until_disabled(N - 1)
    end.

wait_for_registration(0) ->
    ?assert(false);
wait_for_registration(N) ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            timer:sleep(5),
            wait_for_registration(N - 1);
        _ ->
            ok
    end.
-endif.
