-module(observer_cli_escriptize_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

required_modules_test_() ->
    [
        {"simple application without deps", fun simple_app/0},
        {"application with dependency", fun app_with_dependency/0},
        {"application with included application", fun app_with_included/0},
        {"resolve target name", fun resolve_target_name_test/0},
        {"random local node name", fun random_local_node_name_test/0},
        {"ensure set env", fun ensure_set_env_test/0},
        {"application helpers", fun application_helpers_test/0},
        {"application helpers fallback", fun application_helpers_fallback_test/0},
        {"main usage", fun main_usage_test/0},
        {"remote load local", fun remote_load_local_test/0},
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
        _ =
            catch observer_cli_escriptize:run(
                "missing@invalid-host",
                CookieAtom,
                1000,
                fun(_Node) -> ok end
            ),
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
        Result =
            catch observer_cli_escriptize:run(
                TargetNode,
                CookieAtom,
                1000,
                fun(_Node) -> ok end
            ),
        ?assertMatch(
            {'EXIT', {
                {net_kernel_start_failed, {name_mode_mismatch, ExpectedMode, ActualMode, _}}, _
            }},
            Result
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

-endif.
