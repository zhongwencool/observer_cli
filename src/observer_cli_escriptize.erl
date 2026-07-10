-module(observer_cli_escriptize).

-include("observer_cli.hrl").

-export([main/1]).

%% for rpc
-export([ensure_set_env/2]).

-ifdef(TEST).
-export([
    required_modules/1,
    parse_args/1,
    run_args/2,
    resolve_target_name/1,
    random_local_node_name/0,
    application_included/1,
    application_modules/1,
    applications/2,
    all_applications/1,
    run/3,
    run/4,
    run_remote/4,
    remote_load/1
]).
-endif.

%% @doc escript main
-spec main([string()]) -> ok | no_return().

main(Options) ->
    case parse_args(Options) of
        {ok, #{route := tui, target := TargetNode, cookie := Cookie, interval := Interval}} ->
            run(TargetNode, cookie_atom(Cookie), Interval);
        {ok, #{route := command, command := Command, options := CommandOptions}} ->
            command_error(Command, CommandOptions, capability, command_unavailable);
        {error, Error} ->
            case command_from_args(Options) of
                undefined ->
                    io:format("Usage: observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]~n");
                Command ->
                    command_error(
                        Command,
                        requested_format(Options),
                        maps:get(category, Error),
                        maps:get(reason, Error)
                    )
            end
    end.

-ifdef(TEST).
run_args(Options, RunFun) ->
    case parse_args(Options) of
        {ok, #{route := tui, target := TargetNode, cookie := Cookie, interval := Interval}} ->
            RunFun(TargetNode, cookie_atom(Cookie), Interval);
        {ok, #{route := command} = Command} ->
            {ok, Command};
        {error, _Reason} ->
            io:format("Usage: observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]~n")
    end.
-endif.

parse_args(Options) ->
    observer_cli_cli:parse(Options).

command_error(Command, Options, Category, Reason) when is_map(Options) ->
    Format =
        case Options of
            #{json := true} -> json;
            #{format := "json"} -> json;
            #{format := "term"} -> term;
            _ -> text
        end,
    command_error(Command, Format, Category, Reason);
command_error(Command, Format, Category, Reason) ->
    Error = observer_cli_cli:error(Category, Reason),
    Response = observer_cli_cli:envelope(Command, null, null, null, [], [Error]),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            Device =
                case Format of
                    text -> standard_error;
                    _ -> standard_io
                end,
            io:put_chars(Device, Output),
            erlang:halt(observer_cli_cli:exit_code(Category));
        {error, EncodeError} ->
            EncodeReason = maps:get(reason, EncodeError),
            Message = maps:get(
                <<"message">>,
                observer_cli_cli:error(
                    maps:get(category, EncodeError), EncodeReason
                )
            ),
            io:format(standard_error, "observer_cli: ~ts~n", [
                observer_cli_cli:escape_text(Message)
            ]),
            erlang:halt(observer_cli_cli:exit_code(EncodeError))
    end.

command_from_args([[$-, $- | _] | _] = Arguments) ->
    command_from_arguments(Arguments);
command_from_args([First | _]) ->
    case observer_cli_cli:parse([First]) of
        {ok, #{route := command, command := Command}} -> Command;
        _ -> undefined
    end;
command_from_args([]) ->
    undefined.

command_from_arguments([Argument | Rest]) ->
    case observer_cli_cli:parse([Argument]) of
        {ok, #{route := command, command := Command}} -> Command;
        _ -> command_from_arguments(Rest)
    end;
command_from_arguments([]) ->
    undefined.

requested_format(Arguments) ->
    case lists:member("--json", Arguments) of
        true -> json;
        false -> requested_format_value(Arguments)
    end.

requested_format_value(["--format", "json" | _]) -> json;
requested_format_value(["--format", "term" | _]) -> term;
requested_format_value([_ | Rest]) -> requested_format_value(Rest);
requested_format_value([]) -> text.

run(TargetNode, Cookie, Interval) ->
    run(TargetNode, Cookie, Interval, fun remote_load/1).

run(TargetNode, Cookie, Interval, RemoteLoadFun) ->
    {TargetNodeAtom, NameOpt} = resolve_target_name(TargetNode),
    LocalNode = random_local_node_name(),
    MyName =
        case NameOpt of
            shortnames -> list_to_atom(LocalNode);
            longnames -> list_to_atom(LocalNode ++ "@127.0.0.1")
        end,
    case net_kernel:start([MyName, NameOpt]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ensure_net_kernel_name_mode(NameOpt);
        {error, Reason} -> erlang:error({net_kernel_start_failed, Reason})
    end,
    Start = fun() ->
        Options = [{cookie, Cookie}, {interval, Interval}],
        observer_cli:start(TargetNodeAtom, Options)
    end,
    maybe_set_target_cookie(TargetNodeAtom, Cookie),
    run_remote(TargetNodeAtom, fun remote_module_available/1, RemoteLoadFun, Start).

run_remote(TargetNode, ProbeFun, RemoteLoadFun, StartFun) ->
    case ProbeFun(TargetNode) of
        true -> ok;
        false -> RemoteLoadFun(TargetNode)
    end,
    maybe_wait_remote_stop(TargetNode),
    io:format("~p~n", [StartFun()]).

remote_module_available(Node) ->
    net_kernel:hidden_connect_node(Node) =:= true andalso
        rpc:call(Node, code, ensure_loaded, [observer_cli]) =:= {module, observer_cli}.

maybe_set_target_cookie(_Node, undefined) ->
    ok;
maybe_set_target_cookie(Node, Cookie) ->
    erlang:set_cookie(Node, Cookie).

cookie_atom(undefined) ->
    undefined;
cookie_atom(Cookie) ->
    list_to_atom(Cookie).

remote_load(Node) when Node =:= node() ->
    ok;
remote_load(Node) ->
    do_remote_load(Node).

do_remote_load(Node) ->
    application:load(observer_cli),
    Formatter = application:get_env(observer_cli, formatter, ?DEFAULT_FORMATTER),
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

ensure_net_kernel_name_mode(ExpectedMode) ->
    ActualMode =
        case net_kernel:longnames() of
            true -> longnames;
            false -> shortnames
        end,
    case ExpectedMode =:= ActualMode of
        true ->
            ok;
        false ->
            Hint = "use -name for longnames, -sname for shortnames",
            erlang:error(
                {net_kernel_start_failed, {name_mode_mismatch, ExpectedMode, ActualMode, Hint}}
            )
    end.

%%%===================================================================
%%% application
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

-spec ensure_set_env(App :: atom(), Env :: [{atom(), term()}]) -> ok | {error, term()}.
ensure_set_env(App, Env) ->
    Result =
        case application:get_all_env(App) of
            [] -> application:set_env([{App, Env}]);
            _EnvLoaded -> ok
        end,
    maybe_stop_remote(App),
    Result.

-ifdef(TEST).
maybe_stop_remote(App) ->
    case application:get_env(App, test_stop_remote, false) of
        true ->
            StopFun = application:get_env(App, test_stop_remote_fun, fun init:stop/0),
            spawn(StopFun),
            ok;
        false ->
            ok
    end.

maybe_wait_remote_stop(Node) ->
    case application:get_env(observer_cli, test_stop_remote, false) of
        true -> wait_for_nodedown(Node);
        false -> ok
    end.

wait_for_nodedown(Node) ->
    net_kernel:monitor_nodes(true),
    try
        case lists:member(Node, nodes()) of
            false ->
                ok;
            true ->
                receive
                    {nodedown, Node} -> ok;
                    {nodedown, Node, _Reason} -> ok
                after 5000 ->
                    erlang:error({remote_node_stop_timeout, Node})
                end
        end
    after
        net_kernel:monitor_nodes(false)
    end.
-else.
maybe_stop_remote(_App) ->
    ok.

maybe_wait_remote_stop(_Node) ->
    ok.
-endif.

application_included(Application) ->
    ensure_application_loaded(Application),
    case application:get_key(Application, included_applications) of
        {ok, Apps} -> Apps;
        _ -> []
    end.

application_modules(Application) ->
    ensure_application_loaded(Application),
    case application:get_key(Application, modules) of
        {ok, Modules} -> Modules;
        _ -> []
    end.

applications(ApplicationsAcc, App) ->
    ensure_application_loaded(App),
    case application:get_key(App, applications) of
        {ok, Applications} -> ApplicationsAcc ++ Applications;
        undefined -> ApplicationsAcc
    end.

ensure_application_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, _Reason} -> ok
    end.
