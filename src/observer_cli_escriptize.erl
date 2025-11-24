-module(observer_cli_escriptize).

-export([main/1]).

%% for rpc
-export([ensure_set_env/2]).

%% @doc escript main
-spec main(list()) -> 'ok'.

main([TargetNode]) ->
    run(TargetNode, undefined, 1500);
main([TargetNode, Cookie, Interval]) ->
    CookieAtom = list_to_atom(Cookie),
    IntervalInt = list_to_integer(Interval),
    run(TargetNode, CookieAtom, IntervalInt);
main(_Options) ->
    io:format("Usage: observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]~n").

run(TargetNode, Cookie, Interval) ->
    {TargetNodeAtom, NameOpt} = resolve_target_name(TargetNode),
    LocalNode = random_local_node_name(),
    MyName =
        case NameOpt of
            shortnames -> list_to_atom(LocalNode);
            longnames -> list_to_atom(LocalNode ++ "@127.0.0.1")
        end,
    {ok, _} = net_kernel:start([MyName, NameOpt]),
    Start = fun() ->
        Options = [{cookie, Cookie}, {interval, Interval}],
        observer_cli:start(TargetNodeAtom, Options)
    end,
    {badrpc, _} = Start(),
    remote_load(TargetNodeAtom),
    io:format("~p~n", [Start()]).

remote_load(Node) ->
    application:load(observer_cli),
    {ok, Formatter} = application:get_env(observer_cli, formatter),
    FormatterApp = maps:get(application, Formatter),
    Apps = lists:usort([observer_cli, FormatterApp]),
    [recon:remote_load([Node], Mod) || Mod <- required_modules(Apps)],
    erpc:call(Node, ?MODULE, ensure_set_env, [
        observer_cli, application:get_all_env(observer_cli)
    ]),
    ok.

random_local_node_name() ->
    {_, {H, M, S}} = calendar:local_time(),
    lists:flatten(io_lib:format("observer_cli_~2.2.0p_~2.2.0p_~2.2.0p", [H, M, S])).

resolve_target_name(TargetNode) ->
    case string:tokens(TargetNode, "@") of
        [_Name, Host] ->
            Node = list_to_atom(TargetNode),
            case string:tokens(Host, ".") of
                [Host] -> {Node, shortnames};
                [_ | _] -> {Node, longnames}
            end;
        [Name] ->
            %% only a name without host given, assume shortname
            {ok, Host} = inet:gethostname(),
            {list_to_atom(Name ++ "@" ++ Host), shortnames}
    end.

%%%===================================================================
%%% applicaiton
%%%===================================================================

required_modules(AppList) ->
    required_modules(AppList, sets:new()).

required_modules([], Res) ->
    sets:to_list(Res);
required_modules(AppList, Res) ->
    [H | T] = AppList,
    required_modules(
        T ++ all_applications(H),
        lists:foldl(fun sets:add_element/2, Res, application_modules(H))
    ).

all_applications(App) ->
    observer_cli_lib:pipe([], [
        fun(ApplicationsAcc) ->
            applications(ApplicationsAcc, App)
        end,
        fun(ApplicationsAcc) ->
            ApplicationsAcc ++ application_included(App)
        end,
        fun(ApplicationsAcc) -> ApplicationsAcc -- [kernel, stdlib] end
    ]).

ensure_set_env(App, Env) ->
    case application:get_all_env(App) of
        [] -> application:set_env([{App, Env}]);
        _EnvLoaded -> ok
    end.

application_included(Application) ->
    case application:get_key(Application, included_applications) of
        {ok, Apps} -> Apps;
        _ -> []
    end.

application_modules(Application) ->
    case application:get_key(Application, modules) of
        {ok, Modules} -> Modules;
        _ -> []
    end.

applications(ApplicationsAcc, App) ->
    case application:get_key(App, applications) of
        {ok, Applications} -> ApplicationsAcc ++ Applications;
        undefined -> ApplicationsAcc
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

required_modules_test_() ->
    [
        {"simple application without deps", fun simple_app/0},
        {"application with dependency", fun app_with_dependency/0},
        {"application with included application", fun app_with_included/0}
    ].

simple_app() ->
    TestApp = application_spec(#{
        application => some_app,
        applications => [kernel, stdlib],
        included_applications => [],
        modules => [some_app]
    }),
    ok = application:load(TestApp),

    ?assertEqual([some_app], required_modules([some_app])),

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
        lists:sort(required_modules([some_app]))
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
        lists:sort(required_modules([some_app]))
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

-endif.
