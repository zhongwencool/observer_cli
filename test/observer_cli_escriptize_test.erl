-module(observer_cli_escriptize_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

required_modules_test_() ->
    [
        {"simple application without deps", fun simple_app/0},
        {"application with dependency", fun app_with_dependency/0},
        {"unloaded dependency metadata", fun unloaded_dependency_metadata/0},
        {"application with included application", fun app_with_included/0},
        {"resolve target name", fun resolve_target_name_test/0},
        {"random local node name", fun random_local_node_name_test/0},
        {"ensure set env", fun ensure_set_env_test/0},
        {"ensure set env stop remote fun", fun ensure_set_env_stop_remote_fun_test/0},
        {"application helpers", fun application_helpers_test/0},
        {"application helpers fallback", fun application_helpers_fallback_test/0},
        {"parse args", fun parse_args_test/0},
        {"run args", fun run_args_test/0},
        {"run args usage", fun run_args_usage/0},
        {"main usage", fun main_usage_test/0},
        {"remote load local", fun remote_load_local_test/0},
        {"remote load peer node", fun remote_load_peer_node_test/0},
        {"run starts distribution", fun run_starts_distribution_test/0},
        {"run waits for missing node", fun run_waits_for_missing_node_test/0},
        {"run waits for stopped peer", fun run_waits_for_stopped_peer_test/0},
        {"run name mode mismatch", fun run_name_mode_mismatch_test/0},
        {"run unreachable node", {timeout, 20000, fun run_unreachable_node_test/0}}
    ].

simple_app() ->
    TestApp = application_spec(#{
        application => some_app,
        applications => [kernel, stdlib],
        included_applications => [],
        modules => [some_app]
    }),
    ok = application:load(TestApp),

    ?assertEqual([some_app], observer_cli_escriptize:required_modules([some_app])),

    application:unload(some_app).

app_with_dependency() ->
    ok = application:load(
        application_spec(#{
            application => dependency_a,
            applications => [kernel, stdlib],
            included_applications => [],
            modules => [dependency_a_1, dependency_a_2]
        })
    ),

    ok = application:load(
        application_spec(#{
            application => some_app,
            applications => [kernel, stdlib, dependency_a],
            included_applications => [],
            modules => [some_app]
        })
    ),

    ?assertEqual(
        lists:sort([some_app, dependency_a_1, dependency_a_2]),
        lists:sort(observer_cli_escriptize:required_modules([some_app]))
    ),

    application:unload(some_app),
    application:unload(dependency_a).

unloaded_dependency_metadata() ->
    ReconWasLoaded = application:get_key(recon, modules) =/= undefined,
    _ = application:unload(recon),
    _ = application:load(observer_cli),
    try
        Mods = observer_cli_escriptize:required_modules([observer_cli]),
        ?assert(lists:member(recon_lib, Mods))
    after
        case ReconWasLoaded of
            true -> ok;
            false -> application:unload(recon)
        end
    end.

app_with_included() ->
    ok = application:load(
        application_spec(#{
            application => included_a,
            applications => [kernel, stdlib],
            included_applications => [],
            modules => [included_a_1, included_a_2]
        })
    ),

    ok = application:load(
        application_spec(#{
            application => some_app,
            applications => [kernel, stdlib],
            included_applications => [included_a],
            modules => [some_app]
        })
    ),

    ?assertEqual(
        lists:sort([some_app, included_a_1, included_a_2]),
        lists:sort(observer_cli_escriptize:required_modules([some_app]))
    ),

    application:unload(some_app).

application_spec(#{
    application := Application,
    applications := Applications,
    included_applications := IncludedApplications,
    modules := Modules
}) ->
    {application, Application, [
        {modules, Modules},
        {included_applications, IncludedApplications},
        {applications, Applications}
    ]}.

resolve_target_name_test() ->
    {Node1, shortnames} = observer_cli_escriptize:resolve_target_name("target@host"),
    ?assertEqual(list_to_atom("target@host"), Node1),
    {Node2, longnames} = observer_cli_escriptize:resolve_target_name("target@host.example"),
    ?assertEqual(list_to_atom("target@host.example"), Node2),
    {ok, Host} = inet:gethostname(),
    {Node3, shortnames} = observer_cli_escriptize:resolve_target_name("target"),
    ?assertEqual(list_to_atom("target@" ++ Host), Node3).

random_local_node_name_test() ->
    Name = observer_cli_escriptize:random_local_node_name(),
    ?assert(lists:prefix("observer_cli_", Name)).

ensure_set_env_test() ->
    application:unset_env(test_env_app, sample),
    observer_cli_escriptize:ensure_set_env(test_env_app, [{sample, 1}]),
    ?assertEqual(1, application:get_env(test_env_app, sample, undefined)),
    observer_cli_escriptize:ensure_set_env(test_env_app, [{sample, 2}]),
    ?assertEqual(1, application:get_env(test_env_app, sample, undefined)),
    application:unset_env(test_env_app, sample).

ensure_set_env_stop_remote_fun_test() ->
    Parent = self(),
    StopFun = fun() -> Parent ! stop_remote_called end,
    application:set_env(test_stop_app, test_stop_remote, true),
    application:set_env(test_stop_app, test_stop_remote_fun, StopFun),
    try
        ?assertEqual(ok, observer_cli_escriptize:ensure_set_env(test_stop_app, [{sample, 1}])),
        receive
            stop_remote_called -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        application:unset_env(test_stop_app, test_stop_remote),
        application:unset_env(test_stop_app, test_stop_remote_fun)
    end.

application_helpers_test() ->
    _ = application:unload(helper_inc),
    _ = application:unload(helper_app),
    ok = application:load(
        application_spec(#{
            application => helper_inc,
            applications => [kernel, stdlib],
            included_applications => [],
            modules => [helper_inc_mod]
        })
    ),
    _LoadRes = application:load(
        application_spec(#{
            application => helper_app,
            applications => [kernel, stdlib, helper_dep],
            included_applications => [helper_inc],
            modules => [helper_mod]
        })
    ),
    LoadRes2 = application:load(
        application_spec(#{
            application => helper_app,
            applications => [kernel, stdlib, helper_dep],
            included_applications => [helper_inc],
            modules => [helper_mod]
        })
    ),
    case LoadRes2 of
        ok -> ok;
        {error, {already_loaded, _}} -> ok
    end,
    ?assertEqual([helper_inc], observer_cli_escriptize:application_included(helper_app)),
    ?assertEqual([helper_mod], observer_cli_escriptize:application_modules(helper_app)),
    ?assertEqual(
        [kernel, stdlib, helper_dep], observer_cli_escriptize:applications([], helper_app)
    ),
    ?assertEqual(
        [helper_dep, helper_inc],
        lists:sort(observer_cli_escriptize:all_applications(helper_app))
    ),
    application:unload(helper_app),
    application:unload(helper_inc).

application_helpers_fallback_test() ->
    ?assertEqual([], observer_cli_escriptize:application_included(unknown_app)),
    ?assertEqual([], observer_cli_escriptize:application_modules(unknown_app)),
    ?assertEqual([], observer_cli_escriptize:applications([], unknown_app)).

parse_args_test() ->
    ?assertEqual(
        {ok, "target@host", undefined, 1500},
        observer_cli_escriptize:parse_args(["target@host"])
    ),
    ?assertEqual(
        {ok, "target@host", test_cookie, 2000},
        observer_cli_escriptize:parse_args(["target@host", "test_cookie", "2000"])
    ),
    ?assertEqual(usage, observer_cli_escriptize:parse_args([])),
    ?assertEqual(usage, observer_cli_escriptize:parse_args(["target@host", "cookie"])),
    ?assertEqual(
        usage,
        observer_cli_escriptize:parse_args(["target@host", "cookie", "2000", "extra"])
    ),
    ?assertError(
        badarg,
        observer_cli_escriptize:parse_args(["target@host", "cookie", "not-an-integer"])
    ).

run_args_test() ->
    ?assertEqual(
        {ok, "target@host", test_cookie, 2000},
        observer_cli_escriptize:run_args(
            ["target@host", "test_cookie", "2000"],
            fun(TargetNode, Cookie, Interval) -> {ok, TargetNode, Cookie, Interval} end
        )
    ).

run_args_usage() ->
    Parent = self(),
    ?assertEqual(
        ok,
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_args(
                    ["target@host", "cookie"],
                    fun(_TargetNode, _Cookie, _Interval) -> Parent ! run_called end
                )
            end
        )
    ),
    receive
        run_called -> ?assert(false)
    after 0 ->
        ok
    end.

main_usage_test() ->
    observer_cli_test_io:with_input(
        [],
        fun() ->
            ?assertEqual(ok, observer_cli_escriptize:main([]))
        end
    ).

run_unreachable_node_test() ->
    Cookie = "observer_cli_test_cookie",
    CookieAtom = list_to_atom(Cookie),
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true -> ok;
        false -> {ok, _} = net_kernel:start([observer_cli_test, shortnames])
    end,
    erlang:set_cookie(node(), CookieAtom),
    try
        try
            observer_cli_escriptize:run(
                "missing@invalid-host",
                CookieAtom,
                1000,
                fun(_Node) -> ok end
            )
        catch
            _:_ -> ok
        end,
        ok
    after
        erlang:set_cookie(node(), PrevCookie),
        case WasAlive of
            true -> ok;
            false -> _ = net_kernel:stop()
        end
    end.

run_name_mode_mismatch_test() ->
    Cookie = "observer_cli_test_cookie",
    CookieAtom = list_to_atom(Cookie),
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true -> ok;
        false -> {ok, _} = net_kernel:start([observer_cli_test, shortnames])
    end,
    ActualMode =
        case net_kernel:longnames() of
            true -> longnames;
            false -> shortnames
        end,
    {TargetNode, ExpectedMode} =
        case ActualMode of
            longnames -> {"target@host", shortnames};
            shortnames -> {"target@host.example", longnames}
        end,
    erlang:set_cookie(node(), CookieAtom),
    try
        ?assertError(
            {net_kernel_start_failed, {name_mode_mismatch, ExpectedMode, ActualMode, _}},
            observer_cli_escriptize:run(
                TargetNode,
                CookieAtom,
                1000
            )
        )
    after
        erlang:set_cookie(node(), PrevCookie),
        case WasAlive of
            true -> ok;
            false -> _ = net_kernel:stop()
        end
    end.

remote_load_local_test() ->
    ?assertEqual(ok, observer_cli_escriptize:remote_load(node())).

remote_load_peer_node_test() ->
    with_distribution(fun(_Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_remote")}),
        Key = test_remote_load_env,
        PrevEnv = application:get_env(observer_cli, Key),
        ok = application:set_env(observer_cli, Key, copied_to_peer),
        try
            erpc:call(Node, application, unset_env, [observer_cli, Key]),
            ?assertEqual(ok, observer_cli_escriptize:remote_load(Node)),
            ?assertEqual(
                {ok, copied_to_peer},
                erpc:call(Node, application, get_env, [observer_cli, Key])
            )
        after
            restore_env(observer_cli, Key, PrevEnv),
            peer:stop(Peer)
        end
    end).

run_starts_distribution_test() ->
    WasAlive = erlang:is_alive(),
    case WasAlive of
        true ->
            ok;
        false ->
            Cookie = observer_cli_run_start_cookie,
            PrevCookie = erlang:get_cookie(),
            try
                ?assertEqual(
                    ok,
                    observer_cli_test_io:with_input(
                        [],
                        fun() ->
                            observer_cli_escriptize:run(
                                "missing@invalid-host",
                                Cookie,
                                1000,
                                fun(_Node) -> ok end
                            )
                        end
                    )
                )
            after
                erlang:set_cookie(node(), PrevCookie),
                net_kernel:stop()
            end
    end.

run_waits_for_missing_node_test() ->
    with_distribution(fun(Cookie) ->
        PrevStopEnv = application:get_env(observer_cli, test_stop_remote),
        ok = application:set_env(observer_cli, test_stop_remote, true),
        try
            ?assertEqual(
                ok,
                observer_cli_test_io:with_input(
                    [],
                    fun() ->
                        observer_cli_escriptize:run(
                            "missing@invalid-host",
                            Cookie,
                            1000,
                            fun(_Node) -> ok end
                        )
                    end
                )
            )
        after
            restore_env(observer_cli, test_stop_remote, PrevStopEnv)
        end
    end).

run_waits_for_stopped_peer_test() ->
    with_distribution(fun(Cookie) ->
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_run")}),
        PrevStopEnv = application:get_env(observer_cli, test_stop_remote),
        ok = application:set_env(observer_cli, test_stop_remote, true),
        try
            ?assertEqual(
                ok,
                observer_cli_test_io:with_input(
                    [],
                    fun() ->
                        observer_cli_escriptize:run(
                            atom_to_list(Node),
                            Cookie,
                            1000,
                            fun(_Node) ->
                                spawn(fun() ->
                                    timer:sleep(50),
                                    peer:stop(Peer)
                                end),
                                ok
                            end
                        )
                    end
                )
            )
        after
            restore_env(observer_cli, test_stop_remote, PrevStopEnv),
            try peer:stop(Peer) of
                _ -> ok
            catch
                _:_ -> ok
            end
        end
    end).

with_distribution(Fun) ->
    WasAlive = erlang:is_alive(),
    PrevCookie = erlang:get_cookie(),
    case WasAlive of
        true ->
            Fun(PrevCookie);
        false ->
            Name = list_to_atom(peer:random_name("observer_cli_origin")),
            {ok, _} = net_kernel:start([Name, shortnames]),
            try
                Fun(erlang:get_cookie())
            after
                erlang:set_cookie(node(), PrevCookie),
                net_kernel:stop()
            end
    end.

restore_env(App, Key, {ok, Value}) ->
    application:set_env(App, Key, Value);
restore_env(App, Key, undefined) ->
    application:unset_env(App, Key).

-endif.
