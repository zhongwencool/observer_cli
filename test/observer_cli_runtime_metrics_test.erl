-module(observer_cli_runtime_metrics_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

linux_stat_test() ->
    Data = linux_stat(<<"beam (nested) name)">>, 125, 75, 9000, 3),
    Sample = observer_cli_runtime_metrics:parse_linux(Data, 250, 16384),
    ?assertEqual(800000, maps:get(cpu_time_us, Sample)),
    ?assertEqual(49152, maps:get(rss_bytes, Sample)),
    ?assertEqual(9000, maps:get(start_time, Sample)),
    DifferentUnits = observer_cli_runtime_metrics:parse_linux(Data, 100, 4096),
    ?assertEqual(2000000, maps:get(cpu_time_us, DifferentUnits)),
    ?assertEqual(12288, maps:get(rss_bytes, DifferentUnits)).

linux_missing_rss_retains_cpu_test() ->
    Full = linux_stat(<<"beam">>, 100, 200, 9, 3),
    Tokens = string:lexemes(Full, " "),
    WithoutRss = iolist_to_binary(lists:join(" ", lists:sublist(Tokens, length(Tokens) - 1))),
    S = observer_cli_runtime_metrics:parse_linux(WithoutRss, 100, 4096),
    ?assertEqual(3000000, maps:get(cpu_time_us, S)),
    ?assertEqual(undefined, maps:get(rss_bytes, S)).

linux_invalid_and_independent_fields_test() ->
    lists:foreach(
        fun(Data) ->
            S = observer_cli_runtime_metrics:parse_linux(Data, 100, 4096),
            ?assertEqual(undefined, maps:get(cpu_time_us, S)),
            ?assertEqual(undefined, maps:get(rss_bytes, S))
        end,
        [<<>>, <<"123 no parentheses">>, <<"123 (beam) R 0 1">>]
    ),
    Data = linux_stat(<<"beam">>, 100, 200, 9, 3),
    ?assertEqual(
        undefined,
        maps:get(
            cpu_time_us,
            observer_cli_runtime_metrics:parse_linux(Data, undefined, 4096)
        )
    ),
    ?assertEqual(
        12288,
        maps:get(
            rss_bytes,
            observer_cli_runtime_metrics:parse_linux(Data, undefined, 4096)
        )
    ),
    ?assertEqual(
        3000000,
        maps:get(
            cpu_time_us,
            observer_cli_runtime_metrics:parse_linux(Data, 100, undefined)
        )
    ),
    ?assertEqual(
        undefined,
        maps:get(
            rss_bytes,
            observer_cli_runtime_metrics:parse_linux(Data, 100, undefined)
        )
    ),
    ?assertEqual(
        undefined,
        maps:get(
            cpu_time_us,
            observer_cli_runtime_metrics:parse_linux(
                linux_stat(<<"beam">>, -1, 200, 9, 3), 100, 4096
            )
        )
    ).

macos_time_and_rss_test() ->
    S = observer_cli_runtime_metrics:parse_macos(<<"  123456:59.99   2048\n">>),
    ?assertEqual((123456 * 60 + 59) * 1000000 + 990000, maps:get(cpu_time_us, S)),
    ?assertEqual(2097152, maps:get(rss_bytes, S)),
    lists:foreach(
        fun(Time) ->
            ?assertEqual(undefined, observer_cli_runtime_metrics:parse_cpu_time(Time))
        end,
        [<<>>, <<"1:60.00">>, <<"1:00.001">>, <<"1:00">>, <<"-1:00.00">>, <<"1:x.00">>]
    ),
    WithVsz = observer_cli_runtime_metrics:parse_macos(<<"1:00.00 2048 4096">>),
    ?assertEqual(4194304, maps:get(vsz_bytes, WithVsz)),
    S2 = observer_cli_runtime_metrics:parse_macos(<<"bad 2048">>),
    ?assertEqual(undefined, maps:get(cpu_time_us, S2)),
    ?assertEqual(2097152, maps:get(rss_bytes, S2)),
    S3 = observer_cli_runtime_metrics:parse_macos(<<"1:00.01 bad">>),
    ?assertEqual(60010000, maps:get(cpu_time_us, S3)),
    ?assertEqual(undefined, maps:get(rss_bytes, S3)),
    lists:foreach(
        fun(Output) ->
            ?assertEqual(
                undefined,
                maps:get(
                    cpu_time_us,
                    observer_cli_runtime_metrics:parse_macos(Output)
                )
            )
        end,
        [<<>>, <<"1:00.00">>, <<"unexpected command output here">>]
    ).

cpu_actual_window_test() ->
    First = sample(0, 1000000, 1048576),
    Last = sample(2500000, 8000000, 2097152),
    {Metrics, Last} = observer_cli_runtime_metrics:window(First, Last),
    ?assertEqual(280.0, maps:get(percent, maps:get(cpu, Metrics))),
    ?assertEqual(2500000, maps:get(interval_us, maps:get(cpu, Metrics))),
    ?assertEqual(1048576, maps:get(rss_delta_bytes, Metrics)),
    ?assertEqual("280.0%", lists:flatten(observer_cli_runtime_metrics:format_cpu(Metrics))),
    ?assertEqual(
        "CPU window:2.50s", lists:flatten(observer_cli_runtime_metrics:format_window(Metrics))
    ).

warming_zero_and_rss_delta_test() ->
    First = sample(0, 100, 1048576),
    {Warm, First} = observer_cli_runtime_metrics:window(undefined, First),
    ?assertEqual("warming up", observer_cli_runtime_metrics:format_cpu(Warm)),
    ?assertEqual("1.0 MiB", lists:flatten(observer_cli_runtime_metrics:format_rss(Warm))),
    {Zero, _} = observer_cli_runtime_metrics:window(First, sample(1000000, 100, 1048576)),
    ?assertEqual("0.0%", lists:flatten(observer_cli_runtime_metrics:format_cpu(Zero))),
    ?assertEqual("1.0 MiB (+0 B)", lists:flatten(observer_cli_runtime_metrics:format_rss(Zero))),
    {Negative, _} = observer_cli_runtime_metrics:window(First, sample(1000000, 200, 0)),
    ?assertEqual(
        "0 B (-1.0 MiB)", lists:flatten(observer_cli_runtime_metrics:format_rss(Negative))
    ).

reset_invalid_time_and_identity_test() ->
    First = sample(1000000, 1000, 1000),
    lists:foreach(
        fun(Last) ->
            {Invalid, Baseline} = observer_cli_runtime_metrics:window(First, Last),
            ?assertEqual(unavailable, maps:get(status, maps:get(cpu, Invalid))),
            ?assertEqual(undefined, maps:get(cpu_time_us, Baseline)),
            {Warm, _} = observer_cli_runtime_metrics:window(
                Baseline,
                Last#{monotonic_us := 3000000, cpu_time_us := 3000}
            ),
            ?assertEqual(warming_up, maps:get(status, maps:get(cpu, Warm)))
        end,
        [
            sample(2000000, 1, 1000),
            sample(1000000, 2000, 1000),
            sample(0, 2000, 1000),
            (sample(2000000, 2000, 1000))#{identity := {other, "2"}},
            (sample(2000000, 2000, 1000))#{start_time => 2}
        ]
    ).

missing_fields_do_not_poison_other_metric_test() ->
    First = sample(0, 100, 1000),
    {NoCpu, Next} = observer_cli_runtime_metrics:window(First, sample(1000000, undefined, 1200)),
    ?assertEqual("unavailable", observer_cli_runtime_metrics:format_cpu(NoCpu)),
    ?assertEqual(200, maps:get(rss_delta_bytes, NoCpu)),
    {Warm, _} = observer_cli_runtime_metrics:window(Next, sample(2000000, 200, 1300)),
    ?assertEqual(warming_up, maps:get(status, maps:get(cpu, Warm))),
    {NoRss, NextRss} = observer_cli_runtime_metrics:window(First, sample(1000000, 200, undefined)),
    ?assertEqual(available, maps:get(status, maps:get(cpu, NoRss))),
    ?assertEqual("unavailable", observer_cli_runtime_metrics:format_rss(NoRss)),
    {RssBack, _} = observer_cli_runtime_metrics:window(NextRss, sample(2000000, 300, 1200)),
    ?assertEqual(undefined, maps:get(rss_delta_bytes, RssBack)).

command_success_failure_and_budget_test() ->
    ?assertEqual(
        {ok, <<"hello\n">>}, observer_cli_runtime_metrics:command("/bin/echo", ["hello"], 500)
    ),
    ?assertEqual(
        {error, command_failed},
        observer_cli_runtime_metrics:command("/bin/sh", ["-c", "exit 7"], 500)
    ),
    ?assertEqual(
        {error, unavailable}, observer_cli_runtime_metrics:command("/not/a/program", [], 500)
    ),
    ?assertEqual(
        {error, output_limit},
        observer_cli_runtime_metrics:command("/bin/echo", [lists:duplicate(4096, $x)], 500)
    ),
    ?assertEqual({error, timeout}, observer_cli_runtime_metrics:command("/bin/sleep", ["10"], 20)).

command_timeout_kills_os_child_test() ->
    Path = pid_file(),
    try
        Before = erlang:monotonic_time(millisecond),
        ?assertEqual(
            {error, timeout},
            observer_cli_runtime_metrics:command(
                "/bin/sh",
                ["-c", "echo $$ > " ++ Path ++ "; exec sleep 30"],
                100
            )
        ),
        ?assert(erlang:monotonic_time(millisecond) - Before < 500),
        assert_child_gone(Path)
    after
        file:delete(Path)
    end.

command_owner_exit_kills_os_child_test() ->
    Path = pid_file(),
    try
        {Worker, Monitor} = spawn_monitor(fun() ->
            observer_cli_runtime_metrics:command(
                "/bin/sh",
                ["-c", "echo $$ > " ++ Path ++ "; exec sleep 30"],
                500
            )
        end),
        wait_for_pid(Path, 100),
        exit(Worker, stop),
        receive
            {'DOWN', Monitor, process, Worker, stop} -> ok
        after 1000 -> ?assert(false)
        end,
        assert_child_gone(Path)
    after
        file:delete(Path)
    end.

compact_rss_format_test() ->
    M = #{rss_bytes => 999 * 1048576, rss_delta_bytes => 999 * 1048576},
    ?assert(length(lists:flatten(observer_cli_runtime_metrics:format_rss(M))) =< 21).

unsupported_sample_test() ->
    Context = #{platform => unsupported, identity => {node(), os:getpid()}, timeout_ms => 20},
    S = observer_cli_runtime_metrics:sample(Context),
    ?assert(maps:get(collection_us, S) >= 0),
    {Metrics, _} = observer_cli_runtime_metrics:window(undefined, S),
    ?assertEqual("unavailable", observer_cli_runtime_metrics:format_cpu(Metrics)),
    ?assertEqual("unavailable", observer_cli_runtime_metrics:format_rss(Metrics)).

live_backend_smoke_test() ->
    Context = observer_cli_runtime_metrics:init(500),
    S = observer_cli_runtime_metrics:sample(Context),
    case maps:get(platform, Context) of
        unsupported ->
            ok;
        _ ->
            ?assert(is_integer(maps:get(cpu_time_us, S))),
            ?assert(is_integer(maps:get(rss_bytes, S)))
    end,
    ?assertEqual({node(), os:getpid()}, maps:get(identity, S)).

sample(Time, Cpu, Rss) ->
    #{
        identity => {node(), "1"},
        monotonic_us => Time,
        cpu_time_us => Cpu,
        rss_bytes => Rss
    }.

linux_stat(Name, User, System, Start, Rss) ->
    %% Fields 3..24; vsize is deliberately not the RSS.
    Fields =
        ["R"] ++ lists:duplicate(10, "0") ++
            [integer_to_list(User), integer_to_list(System)] ++ lists:duplicate(6, "0") ++
            [integer_to_list(Start), "123456", integer_to_list(Rss)],
    iolist_to_binary(["42 (", Name, ") ", lists:join(" ", Fields)]).

pid_file() ->
    filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer-runtime-child-" ++ integer_to_list(erlang:unique_integer([positive]))
    ).

wait_for_pid(_Path, 0) ->
    ?assert(false);
wait_for_pid(Path, Tries) ->
    case file:read_file(Path) of
        {ok, Data} when byte_size(Data) > 0 -> ok;
        _ ->
            timer:sleep(5),
            wait_for_pid(Path, Tries - 1)
    end.

assert_child_gone(Path) ->
    {ok, Data} = file:read_file(Path),
    Pid = binary_to_list(string:trim(Data)),
    timer:sleep(20),
    ?assertMatch(
        {error, command_failed},
        observer_cli_runtime_metrics:command("/bin/ps", ["-p", Pid, "-o", "pid="], 500)
    ).
-endif.
