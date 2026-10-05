-module(observer_cli_target_test).
-include_lib("eunit/include/eunit.hrl").

environment_selector_test() ->
    with_environment(fun() ->
        ?assertEqual({error, missing_target}, observer_cli_target:resolve(#{})),
        os:putenv("OBSERVER_CLI_NODE", "app@127.0.0.1"),
        ?assertEqual({error, missing_cookie_source}, observer_cli_target:resolve(#{})),
        os:putenv("OBSERVER_CLI_COOKIE", "secret-never-returned"),
        {ok, Target} = observer_cli_target:resolve(#{json => true}),
        ?assertMatch(
            #{node := "app@127.0.0.1", cookie_env := "OBSERVER_CLI_COOKIE", name_mode := "long"},
            Target
        ),
        ?assertNot(lists:member("secret-never-returned", maps:values(Target))),
        os:putenv("OBSERVER_CLI_COOKIE_FILE", "/tmp/cookie"),
        ?assertEqual({error, conflicting_cookie_sources}, observer_cli_target:resolve(#{})),
        os:unsetenv("OBSERVER_CLI_COOKIE"),
        {ok, File} = observer_cli_target:resolve(#{}),
        ?assertEqual("/tmp/cookie", maps:get(cookie_file, File))
    end).

explicit_selector_is_atomic_test() ->
    with_environment(fun() ->
        os:putenv("OBSERVER_CLI_NODE", "wrong@host"),
        os:putenv("OBSERVER_CLI_COOKIE", "wrong-secret"),
        ?assertEqual(
            {error, missing_cookie_source}, observer_cli_target:resolve(#{node => "right@host"})
        ),
        ?assertEqual(
            {error, target_option_requires_node},
            observer_cli_target:resolve(#{cookie_env => "RIGHT"})
        ),
        {ok, Explicit} = observer_cli_target:resolve(#{node => "right@host", cookie_env => "RIGHT"}),
        ?assertEqual("right@host", maps:get(node, Explicit)),
        ?assertEqual("RIGHT", maps:get(cookie_env, Explicit))
    end).

environment_name_mode_test() ->
    with_environment(fun() ->
        os:putenv("OBSERVER_CLI_NODE", "app@127.0.0.1"),
        os:putenv("OBSERVER_CLI_COOKIE", "secret"),
        os:putenv("OBSERVER_CLI_NAME_MODE", "short"),
        {ok, Target} = observer_cli_target:resolve(#{}),
        ?assertEqual("short", maps:get(name_mode, Target)),
        os:putenv("OBSERVER_CLI_NAME_MODE", "invalid"),
        ?assertMatch({error, _}, observer_cli_target:resolve(#{}))
    end).

with_environment(Fun) ->
    Names = [
        "OBSERVER_CLI_NODE",
        "OBSERVER_CLI_COOKIE",
        "OBSERVER_CLI_COOKIE_FILE",
        "OBSERVER_CLI_NAME_MODE"
    ],
    Previous = [{N, os:getenv(N)} || N <- Names],
    lists:foreach(fun os:unsetenv/1, Names),
    try
        Fun()
    after
        lists:foreach(
            fun
                ({N, false}) -> os:unsetenv(N);
                ({N, V}) -> os:putenv(N, V)
            end,
            Previous
        )
    end.
