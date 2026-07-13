-module(observer_cli_start_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

phase0_smoke_behaviors_test() ->
    ?assertEqual(quit, run_start(["q\n"])),
    lists:foreach(
        fun(Cmd) -> ?assertEqual(quit, run_start([Cmd, "q\n"])) end,
        ["H\n", "S\n", "A\n", "N\n", "M\n", "E\n", "D\n", "P\n"]
    ),
    ?assertEqual(quit, run_start(["2000\n", "q\n"])),
    ?assertEqual(quit, run_start(["F\n", "B\n", "q\n"])),
    Target = spawn(fun() -> receive
        after infinity -> ok
        end end),
    try
        ?assertEqual(true, run_start([pid_to_list(Target) ++ "\n", "q\n"]))
    after
        exit(Target, kill)
    end.

start_manager_branches_test() ->
    Inputs = ["p\n", "r\n", "bb\n", "tt\n", "mmq\n", "2000\n", "pd\n", "pu\n", "`\n", "x\n", "q\n"],
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            Inputs,
            fun() ->
                Opts = #view_opts{},
                ?assertEqual(quit, observer_cli:start(Opts))
            end
        )
    end).

start_defaults_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() -> ?assertEqual(quit, observer_cli:start()) end
        )
    end).

start_interval_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() -> ?assertEqual(quit, observer_cli:start(2000)) end
        )
    end).

start_same_node_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() -> ?assertEqual(quit, observer_cli:start(node())) end
        )
    end).

start_same_node_cookie_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() -> ?assertEqual(quit, observer_cli:start(node(), test_cookie)) end
        )
    end).

start_remote_node_test() ->
    Remote = remote_node(),
    observer_cli_test_io:with_input(
        [],
        fun() -> ?assertMatch({badrpc, nodedown}, observer_cli:start(Remote)) end
    ).

start_remote_cookie_test() ->
    Remote = remote_node(),
    observer_cli_test_io:with_input(
        [],
        fun() ->
            try observer_cli:start(Remote, test_cookie) of
                _ -> ok
            catch
                error:distribution_not_started -> ok
            end
        end
    ).

start_remote_options_no_cookie_test() ->
    Remote = remote_node(),
    observer_cli_test_io:with_input(
        [],
        fun() ->
            ?assertMatch({badrpc, nodedown}, observer_cli:start(Remote, [{interval, 1234}]))
        end
    ).

start_plugin_quit_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() -> ?assertEqual(quit, observer_cli:start_plugin()) end
        )
    end).

start_scheduler_usage_toggle_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["`\n", "q\n"],
            fun() ->
                Opts = #view_opts{home = #home{scheduler_usage = ?ENABLE}},
                ?assertEqual(quit, observer_cli:start(Opts))
            end
        )
    end).

scheduler_wall_time_cleanup_is_strictly_paired_test_() ->
    {spawn, fun() ->
        ?assertEqual(undefined, erlang:statistics(scheduler_wall_time)),
        ?assertEqual(false, erlang:system_flag(scheduler_wall_time, true)),
        with_trap_exit(fun() ->
            observer_cli_test_io:with_input(
                ["H\n", "q\n"],
                fun() ->
                    Opts = #view_opts{home = #home{scheduler_usage = ?ENABLE}},
                    ?assertEqual(quit, observer_cli:start(Opts))
                end
            )
        end),
        ?assert(is_list(erlang:statistics(scheduler_wall_time))),
        ?assertEqual(true, erlang:system_flag(scheduler_wall_time, false)),
        ?assertEqual(undefined, erlang:statistics(scheduler_wall_time))
    end}.

start_numeric_jump_without_row_test() ->
    with_trap_exit(fun() ->
        observer_cli_test_io:with_input(
            ["1\n", "q\n"],
            fun() ->
                Opts = #view_opts{},
                ?assertEqual(quit, observer_cli:start(Opts))
            end
        )
    end).

start_blank_jump_without_row_test() ->
    ?assertEqual(true, run_start(["\n", "q\n"])).

start_pause_quit_test() ->
    ?assertEqual(quit, run_start(["p\n", {sleep, 30, "q\n"}])).

start_pause_resume_test() ->
    ?assertEqual(quit, run_start(["p\n", {sleep, 30, "p\n"}, {sleep, 30, "q\n"}])).

start_timer_redraw_test() ->
    ?assertEqual(quit, run_start([{sleep, 30, "q\n"}], #view_opts{home = #home{interval = 1}})).

start_pause_timer_redraw_test() ->
    ?assertEqual(
        quit,
        run_start(["p\n", {sleep, 30, "q\n"}], #view_opts{
            home = #home{interval = 1}
        })
    ).

with_trap_exit(Fun) ->
    PrevTrap = process_flag(trap_exit, true),
    try
        Fun()
    after
        process_flag(trap_exit, PrevTrap)
    end.

remote_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom("observer_cli_missing@" ++ Host).

run_start(Inputs) ->
    run_start(Inputs, #view_opts{}).

run_start(Inputs, Opts) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        process_flag(trap_exit, true),
        Result = observer_cli_test_io:with_input(
            Inputs,
            fun() -> observer_cli:start(Opts) end
        ),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> Result
    after 5000 ->
        exit(Pid, kill),
        timeout
    end.

-endif.
