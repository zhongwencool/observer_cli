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
    remote_load/1,
    run_command/2,
    with_target/2,
    connect_target/7,
    probe_target/7
]).
-endif.

%% @doc escript main
-spec main([string()]) -> ok | no_return().

main(Options) ->
    case parse_args(Options) of
        {ok, #{route := tui, target := TargetNode, cookie := Cookie, interval := Interval}} ->
            run(TargetNode, cookie_atom(Cookie), Interval);
        {ok, #{route := command, command := Command, options := CommandOptions}} ->
            case run_command(Command, CommandOptions) of
                {ok, Response, ExitCode} ->
                    command_output(CommandOptions, Response, ExitCode);
                {error, Category, Reason} ->
                    command_error(Command, CommandOptions, Category, Reason)
            end;
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

run_command(snapshot, #{deep := true} = Options) ->
    with_target(Options, fun(_Target, _Capabilities) ->
        {error, capability, command_unavailable}
    end);
run_command(snapshot, Options) ->
    with_target(Options, fun(Target, _Capabilities) ->
        run_snapshot(Target, Options)
    end);
run_command(diagnose, Options) ->
    with_target(Options, fun(Target, _Capabilities) ->
        run_diagnose(Target, Options)
    end);
run_command(connect, Options) ->
    run_connect(Options);
run_command(status, Options) ->
    run_status(Options);
run_command(disconnect, _Options) ->
    run_disconnect();
run_command(_Command, Options) ->
    with_target(Options, fun(_Target, _Capabilities) ->
        {error, capability, command_unavailable}
    end).

run_connect(Options) ->
    case observer_cli_cli:context_options(Options) of
        {ok, ContextOptions} ->
            probe_options(ContextOptions, fun(_Target, CapabilityResult) ->
                case observer_cli_cli:save_context(ContextOptions) of
                    ok -> probe_response(connect, ContextOptions, CapabilityResult);
                    {error, Reason} -> {error, internal, Reason}
                end
            end);
        {error, Reason} ->
            {error, argument, Reason}
    end.

run_status(Options) ->
    case observer_cli_cli:load_context() of
        {ok, ContextOptions} ->
            ProbeOptions = maps:merge(ContextOptions, maps:with([timeout], Options)),
            probe_options(ProbeOptions, fun(_Target, CapabilityResult) ->
                probe_response(status, ContextOptions, CapabilityResult)
            end);
        {error, no_active_context} ->
            {error, capability, no_active_context};
        {error, Reason} ->
            {error, internal, Reason}
    end.

run_disconnect() ->
    case observer_cli_cli:load_context() of
        {ok, #{node := Node}} ->
            case observer_cli_cli:delete_context() of
                ok -> disconnect_response(list_to_binary(Node));
                {error, Reason} -> {error, internal, Reason}
            end;
        {error, no_active_context} ->
            disconnect_response(null);
        {error, Reason} ->
            {error, internal, Reason}
    end.

probe_response(Command, #{node := Node}, CapabilityResult) ->
    {DiagnosticsModule, Warnings} =
        case CapabilityResult of
            {ok, _Capabilities} ->
                {<<"available">>, []};
            {error, capability, capability_unavailable} ->
                {<<"missing">>, [observer_cli_cli:error(capability, capability_unavailable)]}
        end,
    NodeBinary = list_to_binary(Node),
    Response = observer_cli_cli:envelope(
        Command,
        #{<<"node">> => NodeBinary},
        #{<<"status">> => <<"complete">>},
        #{
            <<"node">> => NodeBinary,
            <<"probe">> => <<"succeeded">>,
            <<"diagnostics_module">> => DiagnosticsModule,
            <<"persistent_connection">> => false
        },
        Warnings,
        []
    ),
    {ok, Response, observer_cli_cli:exit_code(success)}.

disconnect_response(Node) ->
    Response = observer_cli_cli:envelope(
        disconnect,
        null,
        null,
        #{<<"node">> => Node, <<"disconnected">> => true},
        [],
        []
    ),
    {ok, Response, observer_cli_cli:exit_code(success)}.

run_snapshot(Target, Options) ->
    {ok, Timeout} = observer_cli_cli:timeout(Options),
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    DispatchOptions = #{timeout_ms => Timeout, identifier_policy => Policy},
    try
        erpc:call(
            Target,
            observer_cli_snapshot,
            dispatch,
            [self(), snapshot, #{}, DispatchOptions],
            Timeout
        )
    of
        #{<<"status">> := <<"ok">>, <<"result">> := Response} ->
            snapshot_response(Response);
        #{<<"status">> := <<"error">>, <<"reason_code">> := Reason} ->
            {error, required_probe, Reason};
        _Invalid ->
            {error, schema, invalid_snapshot_response}
    catch
        _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
    end.

snapshot_response(#{<<"capture">> := #{<<"status">> := <<"complete">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(success)};
snapshot_response(#{<<"capture">> := #{<<"status">> := <<"partial">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(partial)};
snapshot_response(_Invalid) ->
    {error, schema, invalid_snapshot_response}.

run_diagnose(Target, Options) ->
    {ok, Timeout} = observer_cli_cli:timeout(Options),
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    DispatchOptions = #{timeout_ms => Timeout, identifier_policy => Policy},
    Request = maps:with([observe, deep, app], Options),
    try
        erpc:call(
            Target,
            observer_cli_snapshot,
            dispatch,
            [self(), diagnose, Request, DispatchOptions],
            Timeout
        )
    of
        #{<<"status">> := <<"ok">>, <<"result">> := Response} ->
            diagnose_response(Response);
        #{<<"status">> := <<"error">>, <<"reason_code">> := Reason} ->
            {error, required_probe, Reason};
        _Invalid ->
            {error, schema, invalid_diagnose_response}
    catch
        _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
    end.

diagnose_response(
    #{
        <<"capture">> := #{<<"status">> := <<"complete">>},
        <<"data">> := #{
            <<"findings">> := []
        }
    } = Response
) ->
    {ok, Response, observer_cli_cli:exit_code(success)};
diagnose_response(
    #{
        <<"capture">> := #{<<"status">> := <<"complete">>},
        <<"data">> := #{
            <<"findings">> := [_ | _]
        }
    } = Response
) ->
    {ok, Response, observer_cli_cli:exit_code(diagnose_findings)};
diagnose_response(#{<<"capture">> := #{<<"status">> := <<"partial">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(partial)};
diagnose_response(_Invalid) ->
    {error, schema, invalid_diagnose_response}.

with_target(Options, Fun) ->
    case node() of
        nonode@nohost -> with_active_target(Options, Fun);
        _Distributed -> {error, controller, controller_already_distributed}
    end.

with_active_target(Options, Fun) ->
    case active_options(Options) of
        {ok, TargetOptions} ->
            probe_options(TargetOptions, fun(Target, CapabilityResult) ->
                case CapabilityResult of
                    {ok, Capabilities} -> Fun(Target, Capabilities);
                    Error -> Error
                end
            end);
        {error, no_active_context} ->
            {error, capability, no_active_context};
        {error, Reason} ->
            {error, internal, Reason}
    end.

active_options(#{node := _Node} = Options) ->
    {ok, Options};
active_options(Options) ->
    case observer_cli_cli:load_context() of
        {ok, ContextOptions} ->
            CommandOptions = maps:without([cookie_env, cookie_file, name_mode], Options),
            {ok, maps:merge(ContextOptions, CommandOptions)};
        Error ->
            Error
    end.

probe_options(Options, Fun) ->
    case node() of
        nonode@nohost ->
            case {observer_cli_cli:target(Options), observer_cli_cli:cookie_source(Options)} of
                {{ok, {TargetText, NameMode}}, {ok, CookieBinary}} ->
                    case observer_cli_cli:timeout(Options) of
                        {ok, Timeout} ->
                            Target = list_to_atom(TargetText),
                            Cookie = binary_to_atom(CookieBinary),
                            probe_target(Target, NameMode, Cookie, Timeout, Fun);
                        {error, Reason} ->
                            {error, argument, Reason}
                    end;
                {{error, no_active_context}, _Cookie} ->
                    {error, capability, no_active_context};
                {{error, Reason}, _Cookie} ->
                    {error, argument, Reason};
                {_Target, {error, missing_cookie_source}} ->
                    {error, argument, missing_cookie_source};
                {_Target, {error, Reason}} ->
                    {error, connection, Reason}
            end;
        _Distributed ->
            {error, controller, controller_already_distributed}
    end.

-ifdef(TEST).
connect_target(Target, NameMode, Cookie, Timeout, RandomFun, ConnectFun, Fun) ->
    probe_target(
        Target,
        NameMode,
        Cookie,
        Timeout,
        RandomFun,
        ConnectFun,
        fun(ConnectedTarget, CapabilityResult) ->
            case CapabilityResult of
                {ok, Capabilities} -> Fun(ConnectedTarget, Capabilities);
                Error -> Error
            end
        end
    ).
-endif.

probe_target(Target, NameMode, Cookie, Timeout, Fun) ->
    probe_target(
        Target,
        NameMode,
        Cookie,
        Timeout,
        fun() -> crypto:strong_rand_bytes(24) end,
        fun net_kernel:connect_node/1,
        Fun
    ).

probe_target(Target, NameMode, Cookie, Timeout, RandomFun, ConnectFun, Fun) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    case node() of
        nonode@nohost ->
            case
                net_kernel:start(undefined, #{
                    name_domain => NameMode, dist_listen => false, hidden => true
                })
            of
                {ok, _Pid} ->
                    try
                        connect_started(
                            Target, Cookie, Deadline, RandomFun, ConnectFun, Fun
                        )
                    after
                        stop_controller(Target)
                    end;
                {error, {already_started, _Pid}} ->
                    {error, controller, controller_already_distributed};
                {error, _Reason} ->
                    {error, controller, controller_start_failed}
            end;
        _Distributed ->
            {error, controller, controller_already_distributed}
    end.

connect_started(Target, Cookie, Deadline, RandomFun, ConnectFun, Fun) ->
    case random_cookie(RandomFun) of
        {ok, RandomCookie} ->
            true = erlang:set_cookie(RandomCookie),
            true = erlang:set_cookie(Target, Cookie),
            case connect_before(Target, ConnectFun, remaining(Deadline)) of
                ok ->
                    Fun(Target, capabilities(Target, remaining(Deadline)));
                Error ->
                    Error
            end;
        error ->
            {error, controller, random_cookie_unavailable}
    end.

random_cookie(RandomFun) ->
    try RandomFun() of
        Bytes when is_binary(Bytes), byte_size(Bytes) =:= 24 ->
            {ok, binary_to_atom(binary:encode_hex(Bytes))};
        _Other ->
            error
    catch
        _:_ -> error
    end.

connect_before(_Target, _ConnectFun, Timeout) when Timeout =< 0 ->
    {error, connection, connection_failed};
connect_before(Target, ConnectFun, Timeout) ->
    Parent = self(),
    {Pid, Monitor} = spawn_monitor(fun() -> Parent ! {self(), ConnectFun(Target)} end),
    receive
        {Pid, true} ->
            erlang:demonitor(Monitor, [flush]),
            ok;
        {Pid, _Other} ->
            erlang:demonitor(Monitor, [flush]),
            {error, connection, connection_failed};
        {'DOWN', Monitor, process, Pid, _Reason} ->
            {error, connection, connection_failed}
    after Timeout ->
        exit(Pid, kill),
        receive
            {'DOWN', Monitor, process, Pid, _Reason} -> ok
        end,
        {error, connection, connection_failed}
    end.

capabilities(_Target, Timeout) when Timeout =< 0 ->
    {error, capability, capability_unavailable};
capabilities(Target, Timeout) ->
    try erpc:call(Target, observer_cli_snapshot, capabilities, [], Timeout) of
        #{protocol_version := 1} = Capabilities -> {ok, Capabilities};
        _Incompatible -> {error, capability, capability_unavailable}
    catch
        _:_ -> {error, capability, capability_unavailable}
    end.

remaining(Deadline) ->
    erlang:max(0, Deadline - erlang:monotonic_time(millisecond)).

stop_controller(_Target) ->
    case whereis(net_kernel) of
        undefined ->
            ok;
        Pid ->
            Monitor = erlang:monitor(process, Pid),
            _ = net_kernel:stop(),
            receive
                {'DOWN', Monitor, process, Pid, _Reason} -> ok
            after 5000 ->
                erlang:demonitor(Monitor, [flush])
            end
    end,
    wait_not_alive(500).

wait_not_alive(0) ->
    ok;
wait_not_alive(Attempts) ->
    case erlang:is_alive() of
        false ->
            ok;
        true ->
            timer:sleep(10),
            wait_not_alive(Attempts - 1)
    end.

command_output(Options, Response, ExitCode) ->
    Format = command_format(Options),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            io:put_chars(standard_io, Output),
            erlang:halt(ExitCode);
        {error, EncodeError} ->
            output_encode_error(EncodeError)
    end.

command_error(Command, Options, Category, Reason) when is_map(Options) ->
    Format = command_format(Options),
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
            output_encode_error(EncodeError)
    end.

command_format(#{json := true}) -> json;
command_format(#{format := "json"}) -> json;
command_format(#{format := "term"}) -> term;
command_format(_Options) -> text.

output_encode_error(EncodeError) ->
    EncodeReason = maps:get(reason, EncodeError),
    Message = maps:get(
        <<"message">>,
        observer_cli_cli:error(maps:get(category, EncodeError), EncodeReason)
    ),
    io:format(standard_error, "observer_cli: ~ts~n", [
        observer_cli_cli:escape_text(Message)
    ]),
    erlang:halt(observer_cli_cli:exit_code(EncodeError)).

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
