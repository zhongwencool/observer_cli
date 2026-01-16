-module(observer_cli_start_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

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

-endif.
