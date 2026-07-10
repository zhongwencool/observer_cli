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
        {"run command args", fun run_command_args_test/0},
        {"run args usage", fun run_args_usage/0},
        {"main usage", fun main_usage_test/0},
        {"escript command exits", fun escript_command_exits/0},
        {"remote load local", fun remote_load_local_test/0},
        {"remote load peer node", fun remote_load_peer_node_test/0},
        {"run starts distribution", fun run_starts_distribution_test/0},
        {"run waits for missing node", fun run_waits_for_missing_node_test/0},
        {"run waits for stopped peer", fun run_waits_for_stopped_peer_test/0},
        {"run preinstalled TUI once", fun run_preinstalled_tui_once_test/0},
        {"run legacy-loaded TUI once", fun run_legacy_loaded_tui_once_test/0},
        {"run name mode mismatch", fun run_name_mode_mismatch_test/0},
        {"run unreachable node", {timeout, 20000, fun run_unreachable_node_test/0}},
        {"refuse pre-distributed controller", fun refuse_pre_distributed_controller/0},
        {"stop before connect on random failure", fun random_failure_stops_before_connect/0},
        {"dynamic controller handshake", {timeout, 20000, fun dynamic_controller_handshake/0}},
        {"missing capability", {timeout, 20000, fun missing_capability/0}},
        {"incompatible capability", {timeout, 20000, fun incompatible_capability/0}}
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
        {ok, #{route => tui, target => "target@host", cookie => undefined, interval => 1500}},
        observer_cli_escriptize:parse_args(["target@host"])
    ),
    ?assertEqual(
        {ok, #{route => tui, target => "target@host", cookie => "test_cookie", interval => 2000}},
        observer_cli_escriptize:parse_args(["target@host", "test_cookie", "2000"])
    ),
    ?assertMatch({error, #{exit_code := 2}}, observer_cli_escriptize:parse_args([])),
    ?assertMatch(
        {error, #{exit_code := 2}}, observer_cli_escriptize:parse_args(["target@host", "cookie"])
    ),
    ?assertMatch(
        {error, #{exit_code := 2}},
        observer_cli_escriptize:parse_args(["target@host", "cookie", "2000", "extra"])
    ),
    ?assertMatch(
        {error, #{exit_code := 2}},
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

run_command_args_test() ->
    Parent = self(),
    ?assertMatch(
        {ok, #{route := command, command := memory}},
        observer_cli_escriptize:run_args(
            ["memory", "--json"],
            fun(_TargetNode, _Cookie, _Interval) -> Parent ! run_called end
        )
    ),
    receive
        run_called -> ?assert(false)
    after 0 ->
        ok
    end.

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

escript_command_exits() ->
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    CliBeamDir = filename:join(AppDir, "test"),
    EscriptizeBeamDir = filename:join(AppDir, "ebin"),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_exit_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".escript"
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts -pa ~ts~n"
        "main([\"command\"]) -> observer_cli_escriptize:main([\"memory\", \"--format\", \"term\"]);~n"
        "main([Category]) -> erlang:halt(observer_cli_cli:exit_code(list_to_atom(Category))).~n",
        [CliBeamDir, EscriptizeBeamDir]
    ),
    ok = file:write_file(Script, Contents),
    try
        {2, CommandOutput} = run_escript(Escript, [Script, "command"]),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"observer_cli.cli/v1">>)),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"capture">>)),
        ?assertNotEqual(nomatch, binary:match(CommandOutput, <<"null">>)),
        ?assertEqual({0, <<>>}, run_escript(Escript, [Script, "success"])),
        ?assertEqual({1, <<>>}, run_escript(Escript, [Script, "diagnose_findings"])),
        ?assertEqual({3, <<>>}, run_escript(Escript, [Script, "connection"])),
        ?assertEqual({4, <<>>}, run_escript(Escript, [Script, "internal"]))
    after
        file:delete(Script)
    end.

run_escript(Escript, Args) ->
    Port = open_port(
        {spawn_executable, Escript},
        [binary, exit_status, stderr_to_stdout, {args, Args}]
    ),
    collect_escript(Port, <<>>).

collect_escript(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_escript(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 10000 ->
        erlang:error(escript_timeout)
    end.

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
            spawn(fun() ->
                timer:sleep(50),
                peer:stop(Peer)
            end),
            ?assertEqual(
                ok,
                observer_cli_test_io:with_input(
                    [],
                    fun() ->
                        observer_cli_escriptize:run(
                            atom_to_list(Node),
                            Cookie,
                            1000,
                            fun(_Node) -> ok end
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

run_preinstalled_tui_once_test() ->
    Parent = self(),
    ProbeFun = fun(_Node) -> true end,
    RemoteLoadFun = fun(_Node) -> Parent ! remote_load_called end,
    StartFun = fun() ->
        Parent ! tui_started,
        quit
    end,
    ?assertEqual(
        ok,
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_remote(
                    preinstalled@target, ProbeFun, RemoteLoadFun, StartFun
                )
            end
        )
    ),
    ?assertEqual([tui_started], drain_run_messages([])).

run_legacy_loaded_tui_once_test() ->
    Parent = self(),
    ProbeFun = fun(_Node) -> false end,
    RemoteLoadFun = fun(_Node) -> Parent ! remote_load_called end,
    StartFun = fun() ->
        Parent ! tui_started,
        quit
    end,
    ?assertEqual(
        ok,
        observer_cli_test_io:with_input(
            [],
            fun() ->
                observer_cli_escriptize:run_remote(
                    missing_module@target, ProbeFun, RemoteLoadFun, StartFun
                )
            end
        )
    ),
    ?assertEqual([remote_load_called, tui_started], drain_run_messages([])).

refuse_pre_distributed_controller() ->
    with_distribution(fun(_Cookie) ->
        ?assertEqual(
            {error, controller, controller_already_distributed},
            observer_cli_escriptize:with_target(#{}, fun(_Target, _Capabilities) -> ok end)
        )
    end).

random_failure_stops_before_connect() ->
    ?assertEqual(nonode@nohost, node()),
    Parent = self(),
    ?assertEqual(
        {error, controller, random_cookie_unavailable},
        observer_cli_escriptize:connect_target(
            missing@host,
            shortnames,
            observer_cli_target_cookie,
            1000,
            fun() -> erlang:error(no_random_source) end,
            fun(_Target) -> Parent ! connect_called end,
            fun(_Target, _Capabilities) -> Parent ! callback_called end
        )
    ),
    receive
        connect_called -> ?assert(false);
        callback_called -> ?assert(false)
    after 0 ->
        ok
    end,
    ?assertEqual(nonode@nohost, node()).

dynamic_controller_handshake() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_dynamic_target_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [snapshot_beam_dir()]),
    RandomBytes = binary:copy(<<16#aa>>, 24),
    RandomCookie = binary_to_atom(binary:encode_hex(RandomBytes)),
    try
        ?assertEqual(
            {ok, #{protocol_version => 1}},
            observer_cli_escriptize:connect_target(
                Target,
                shortnames,
                Cookie,
                10000,
                fun() -> RandomBytes end,
                fun net_kernel:connect_node/1,
                fun(ConnectedTarget, Capabilities) ->
                    Controller = node(self()),
                    assert_not_equal(nonode@nohost, Controller),
                    assert_equal(RandomCookie, erlang:get_cookie()),
                    assert_equal(Cookie, erlang:get_cookie(ConnectedTarget)),
                    ?assert(
                        lists:member(
                            Controller, erpc:call(ConnectedTarget, erlang, nodes, [hidden])
                        )
                    ),
                    ?assertNot(
                        lists:member(
                            Controller, erpc:call(ConnectedTarget, erlang, nodes, [visible])
                        )
                    ),
                    {ok, EpmdNames} = net_adm:names(),
                    ControllerName = hd(string:split(atom_to_list(Controller), "@")),
                    ?assertNot(lists:keymember(ControllerName, 1, EpmdNames)),
                    {ok, Capabilities}
                end
            )
        )
    after
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

snapshot_escript_envelopes_test_() ->
    {timeout, 30, fun snapshot_escript_envelopes/0}.

snapshot_escript_envelopes() ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_snapshot_escript_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [snapshot_beam_dir()]),
    Escript = os:find_executable("escript"),
    AppDir = code:lib_dir(observer_cli),
    Script = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        "observer_cli_snapshot_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".escript"
    ),
    Contents = io_lib:format(
        "#!/usr/bin/env escript~n%%! -pa ~ts/ebin~n"
        "main(Args) -> observer_cli_escriptize:main(Args).~n",
        [AppDir]
    ),
    CookieEnv = "OBSERVER_CLI_SNAPSHOT_ESCRIPT_COOKIE",
    ok = file:write_file(Script, Contents),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    Args = [
        Script,
        "snapshot",
        "--node",
        atom_to_list(Target),
        "--cookie-env",
        CookieEnv
    ],
    try
        {0, Text} = run_escript(Escript, Args),
        ?assertNotEqual(nomatch, binary:match(Text, <<"observer_cli.cli/v1">>)),
        ?assertNotEqual(nomatch, binary:match(Text, <<"complete">>)),
        ?assertEqual(nomatch, binary:match(Text, atom_to_binary(Target))),
        {0, Term} = run_escript(Escript, Args ++ ["--format", "term"]),
        {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
        {ok, Response} = erl_parse:parse_term(Tokens),
        ?assertMatch(
            #{
                <<"schema">> := <<"observer_cli.cli/v1">>,
                <<"command">> := <<"snapshot">>,
                <<"capture">> := #{<<"status">> := <<"complete">>}
            },
            Response
        ),
        assert_partial_snapshot_exit(Escript, Script, CookieEnv)
    after
        true = os:unsetenv(CookieEnv),
        file:delete(Script),
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

assert_partial_snapshot_exit(Escript, Script, CookieEnv) ->
    Dir = temporary_directory("observer_cli_partial_snapshot"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        <<
            "-module(observer_cli_snapshot).\n"
            "-export([capabilities/0,dispatch/4]).\n"
            "capabilities() -> #{protocol_version => 1}.\n"
            "dispatch(_,snapshot,_,_) -> #{<<\"status\">> => <<\"ok\">>, "
            "<<\"result\">> => #{<<\"schema\">> => <<\"observer_cli.cli/v1\">>, "
            "<<\"command\">> => <<\"snapshot\">>, <<\"target\">> => null, "
            "<<\"capture\">> => #{<<\"status\">> => <<\"partial\">>}, "
            "<<\"data\">> => #{}, <<\"warnings\">> => [], <<\"errors\">> => []}}.\n"
        >>
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    Cookie = observer_cli_partial_snapshot_cookie,
    {Port, Target} = start_target(shortnames, Cookie, [Dir]),
    true = os:putenv(CookieEnv, atom_to_list(Cookie)),
    try
        {3, Term} = run_escript(Escript, [
            Script,
            "snapshot",
            "--node",
            atom_to_list(Target),
            "--cookie-env",
            CookieEnv,
            "--format",
            "term"
        ]),
        {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
        {ok, Response} = erl_parse:parse_term(Tokens),
        ?assertMatch(
            #{<<"capture">> := #{<<"status">> := <<"partial">>}},
            Response
        )
    after
        stop_target(Port),
        file:del_dir_r(Dir)
    end.

missing_capability() ->
    capability_error_test([]).

incompatible_capability() ->
    Dir = temporary_directory("observer_cli_incompatible"),
    Source = filename:join(Dir, "observer_cli_snapshot.erl"),
    ok = file:write_file(
        Source,
        <<"-module(observer_cli_snapshot).\n-export([capabilities/0]).\ncapabilities() -> #{protocol_version => 2}.\n">>
    ),
    {ok, observer_cli_snapshot} = compile:file(Source, [{outdir, Dir}]),
    try
        capability_error_test([Dir])
    after
        file:del_dir_r(Dir)
    end.

capability_error_test(CodePaths) ->
    ?assertEqual(nonode@nohost, node()),
    Cookie = observer_cli_capability_target_cookie,
    {Port, Target} = start_target(shortnames, Cookie, CodePaths),
    try
        ?assertEqual(
            {error, capability, capability_unavailable},
            observer_cli_escriptize:connect_target(
                Target,
                shortnames,
                Cookie,
                10000,
                fun() -> binary:copy(<<16#55>>, 24) end,
                fun net_kernel:connect_node/1,
                fun(_ConnectedTarget, _Capabilities) -> capability_accepted end
            )
        )
    after
        stop_target(Port)
    end,
    ?assertEqual(nonode@nohost, node()).

snapshot_beam_dir() ->
    filename:join(code:lib_dir(observer_cli), "ebin").

start_target(NameMode, Cookie, CodePaths) ->
    Erl = filename:join([
        code:root_dir(), "erts-" ++ erlang:system_info(version), "bin", "erl"
    ]),
    Name = peer:random_name("observer_cli_diagnostic_target"),
    NameArguments =
        case NameMode of
            shortnames -> ["-sname", Name];
            longnames -> ["-name", Name ++ "@127.0.0.1"]
        end,
    PathArguments = lists:append([["-pa", Path] || Path <- CodePaths]),
    Arguments =
        ["-noshell", "-noinput"] ++
            NameArguments ++
            ["-setcookie", atom_to_list(Cookie)] ++
            PathArguments ++
            ["-eval", "io:put_chars(\"READY\\n\"), receive after infinity -> ok end."],
    Port = open_port(
        {spawn_executable, Erl},
        [binary, exit_status, stderr_to_stdout, {args, Arguments}]
    ),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    wait_target_ready(Port, <<>>),
    Host =
        case NameMode of
            shortnames ->
                {ok, Hostname} = inet:gethostname(),
                Hostname;
            longnames ->
                "127.0.0.1"
        end,
    {{Port, OsPid}, list_to_atom(Name ++ "@" ++ Host)}.

wait_target_ready(Port, Output) ->
    receive
        {Port, {data, Data}} ->
            Combined = <<Output/binary, Data/binary>>,
            case binary:match(Combined, <<"READY\n">>) of
                nomatch -> wait_target_ready(Port, Combined);
                _ -> ok
            end;
        {Port, {exit_status, Status}} ->
            erlang:error({target_start_failed, Status, Output})
    after 10000 ->
        erlang:error(target_start_timeout)
    end.

stop_target({Port, OsPid}) ->
    _ = os:cmd("kill -TERM " ++ integer_to_list(OsPid)),
    try port_close(Port) of
        true -> ok
    catch
        error:badarg -> ok
    end.

temporary_directory(Prefix) ->
    Dir = filename:join(
        os:getenv("TMPDIR", "/tmp"),
        Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Dir.

assert_equal(Expected, Actual) ->
    ?assertEqual(Expected, Actual).

assert_not_equal(Unexpected, Actual) ->
    ?assertNotEqual(Unexpected, Actual).

drain_run_messages(Messages) ->
    receive
        Message when Message =:= remote_load_called; Message =:= tui_started ->
            drain_run_messages(Messages ++ [Message])
    after 0 ->
        Messages
    end.

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
