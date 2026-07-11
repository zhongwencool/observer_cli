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
    command_request/3,
    with_target/2,
    connect_target/7,
    probe_target/7
]).
-endif.

%% @doc escript main
-spec main([string()]) -> ok | no_return().

main(["--help"]) ->
    usage();
main([Command, "--help"]) ->
    command_help(Command);
main(Options) ->
    case parse_args(Options) of
        {ok, #{route := tui, target := TargetNode, cookie := Cookie, interval := Interval}} ->
            run(TargetNode, cookie_atom(Cookie), Interval);
        {ok, #{
            route := command,
            command := Command,
            arguments := Arguments,
            options := CommandOptions
        }} ->
            case run_command(Command, CommandOptions#{arguments => Arguments}) of
                {ok, Response, ExitCode} ->
                    command_output(CommandOptions, Response, ExitCode);
                {error, Category, Reason} ->
                    command_error(Command, CommandOptions, Category, Reason)
            end;
        {error, Error} ->
            case command_from_args(Options) of
                undefined ->
                    usage();
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
            usage()
    end.
-endif.

usage() ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli tui NODE [COOKIE REFRESH_MS]\n"
        "  observer_cli NODE [COOKIE REFRESH_MS]\n"
        "  observer_cli connect --node NODE (--cookie-env NAME | --cookie-file PATH)\n"
        "  observer_cli COMMAND [ARGUMENTS] [OPTIONS]\n"
        "\n"
        "Context:\n"
        "  connect             Verify and save a target context\n"
        "  status              Check the saved target context\n"
        "  disconnect          Remove the saved target context\n"
        "\n"
        "Diagnostics:\n"
        "  diagnose            Detect likely VM problems and report evidence\n"
        "  snapshot            Collect a point-in-time VM fact bundle\n"
        "\n"
        "Inspection:\n"
        "  memory              Show VM memory usage\n"
        "  schedulers          Measure scheduler utilization and run queues\n"
        "  distribution        Show connected Erlang nodes\n"
        "  processes           List top processes\n"
        "  process TARGET      Inspect one process\n"
        "  applications        Group process resources by application\n"
        "  ets                 List ETS tables\n"
        "  mnesia              List local Mnesia tables\n"
        "  network             Show VM network I/O\n"
        "  ports               List Erlang ports\n"
        "  sockets             List OTP sockets\n"
        "  gen-server-state TARGET\n"
        "                      Inspect a bounded gen_server state shape\n"
        "  supervision-tree --app APP\n"
        "                      Show an application supervision tree\n"
        "\n"
        "Tracing:\n"
        "  trace call MFA      Run a bounded function trace\n"
        "  trace stop --all    Stop observer_cli traces\n"
        "\n"
        "Target options:\n"
        "  --node NODE         Use an explicit target instead of saved context\n"
        "  --cookie-env NAME   Read the target cookie from an environment variable\n"
        "  --cookie-file PATH  Read the target cookie from a file\n"
        "  --name-mode MODE    short or long; inferred from NODE by default\n"
        "\n"
        "Output options:\n"
        "  --format FORMAT     text, term, or json; text by default\n"
        "  --json              Alias for --format json (OTP 27+ controller)\n"
        "  --redact            Hide target identifiers\n"
        "  --include-identifiers\n"
        "                      Include identifiers in snapshot or diagnose output\n"
        "  --timeout DURATION  Set the command deadline, up to 120s\n"
        "\n"
        "DURATION accepts milliseconds (1500 or 1500ms) or seconds (2s).\n"
        "\n"
        "Run 'observer_cli COMMAND --help' for command options and examples.\n"
    ).

command_help("connect") ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli connect --node NODE (--cookie-env NAME | --cookie-file PATH) [OPTIONS]\n"
        "\n"
        "Verify the target and save its node and cookie-source metadata. No cookie or\n"
        "persistent connection is stored. Later commands use this context by default.\n"
        "\n"
        "Options:\n"
        "  --name-mode short|long\n"
        "  --timeout DURATION      Command deadline, up to 120s\n"
        "  --format text|term|json\n"
        "  --json\n"
        "\n"
        "DURATION accepts milliseconds (1500 or 1500ms) or seconds (2s).\n"
        "\n"
        "Example:\n"
        "  observer_cli connect --node app@host --cookie-env ERL_COOKIE\n"
    );
command_help("status") ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli status [--timeout DURATION] [--format text|term|json] [--json]\n"
        "\n"
        "Probe the target saved by connect. This starts a fresh connection; connect\n"
        "does not run a daemon.\n"
        "\n"
        "Example:\n"
        "  observer_cli status\n"
    );
command_help("disconnect") ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli disconnect [--format text|term|json] [--json]\n"
        "\n"
        "Remove the saved target context. This is not a network disconnect operation.\n"
        "\n"
        "Example:\n"
        "  observer_cli disconnect\n"
    );
command_help("snapshot") ->
    remote_help(
        "snapshot [--deep] [--include-identifiers]",
        "Collect bounded runtime facts. The default avoids resource inventories; --deep adds admitted Top-N scans.",
        "  --deep                 Add process, table, network, port, and socket inventories\n"
        "  --include-identifiers  Include real node, PID, name, and MFA identifiers\n",
        "  observer_cli snapshot --deep --format term\n"
    );
command_help("diagnose") ->
    remote_help(
        "diagnose [--observe DURATION [--deep | --app APP]] [--include-identifiers]",
        "Run evidence-backed diagnostics. With no mode option, perform a quick point-in-time diagnosis.",
        "  --observe DURATION     Sample for 5s..60s\n"
        "  --deep                 Add deep resource observation; requires --observe\n"
        "  --app APP              Observe one application; requires --observe\n"
        "  --include-identifiers  Include real node, PID, name, and MFA identifiers\n",
        "  observer_cli diagnose\n"
        "  observer_cli diagnose --observe 30s --deep --json\n"
    );
command_help("memory") ->
    remote_help(
        "memory",
        "Show point-in-time BEAM memory and runtime facts. This is not host RSS.",
        "",
        "  observer_cli memory\n"
    );
command_help("schedulers") ->
    remote_help(
        "schedulers [--duration DURATION]",
        "Measure normal and dirty scheduler utilization and run queues.",
        "  --duration DURATION  250ms..10s; 1500ms by default\n",
        "  observer_cli schedulers --duration 2s\n"
    );
command_help("distribution") ->
    remote_help(
        "distribution [--limit N]",
        "Show connected visible and hidden Erlang nodes and available distribution context.",
        limit_help(),
        "  observer_cli distribution --limit 50\n"
    );
command_help("processes") ->
    remote_help(
        "processes [--sort KEY] [--limit N] [--duration DURATION]",
        "List top processes using bounded explicit-key inspection.",
        "  --sort KEY           memory (default), message_queue_len, reductions,\n"
        "                       binary_memory, or total_heap_size\n"
        "  --limit N            1..200; 20 by default\n"
        "  --duration DURATION  250ms..10s; only with --sort reductions\n",
        "  observer_cli processes --sort memory --limit 20\n"
        "  observer_cli processes --sort reductions --duration 1500ms\n"
    );
command_help("process") ->
    remote_help(
        "process PID_OR_NAME [--info]",
        "Inspect safe metadata for one local PID or registered process name. --info is the default mode.",
        "  --info  Show explicit-key process metadata without messages, dictionary, or state\n",
        "  observer_cli process \"<0.123.0>\"\n"
    );
command_help("applications") ->
    list_help(
        "applications",
        "Group process count, memory, reductions, and message queues by application.",
        "memory (default), process_count, reductions, message_queue_len"
    );
command_help("ets") ->
    list_help(
        "ets",
        "List ETS table metadata without reading table contents.",
        "memory (default), size"
    );
command_help("mnesia") ->
    list_help(
        "mnesia",
        "List local Mnesia table metadata. A stopped Mnesia application is reported as not_running.",
        "memory (default), size"
    );
command_help("network") ->
    counter_help(
        "network",
        "Show VM port-driver and legacy inet counters, not all host network traffic.",
        "oct (default), recv_oct, send_oct"
    );
command_help("ports") ->
    list_help(
        "ports",
        "List non-inet Erlang Port metadata and counters, not TCP or UDP port numbers.",
        "queue_size (default), memory, input, output, io"
    );
command_help("sockets") ->
    counter_help(
        "sockets",
        "List sockets visible through the OTP socket registry.",
        "io (default), read_bytes, write_bytes, packets, waits, fails"
    );
command_help("gen-server-state") ->
    remote_help(
        "gen-server-state PID_OR_NAME [--redact]",
        "Inspect the bounded shape of one gen_server state. Full state values are never returned.",
        "  --redact  Hide identifiers found in the state shape\n",
        "  observer_cli gen-server-state my_server --redact\n"
    );
command_help("supervision-tree") ->
    remote_help(
        "supervision-tree --app APP",
        "Show the bounded supervision tree rooted in one running application.",
        "  --app APP  Application name; required\n",
        "  observer_cli supervision-tree --app my_app\n"
    );
command_help("trace") ->
    remote_help(
        "trace (call MFA --pid PID --replace-existing-trace [OPTIONS] | stop --all)",
        "Run or stop observer_cli's bounded node-global call trace. Trace operations can clear unrelated static traces.",
        "  --pid PID                 Local tracee PID; required for trace call\n"
        "  --duration DURATION       100ms..60s; 10s by default\n"
        "  --limit N                 Maximum 1..1000 events; 100 by default\n"
        "  --rate N/s                Maximum 1..200 events per second; conflicts with --limit\n"
        "  --replace-existing-trace  Acknowledge node-global trace replacement; required\n"
        "  --all                     Required for trace stop\n",
        "  observer_cli trace call my_mod:my_fun/2 --pid \"<0.123.0>\" \\\n"
        "    --duration 30s --limit 200 --replace-existing-trace\n"
        "  observer_cli trace stop --all\n"
    );
command_help(_Command) ->
    usage().

remote_help(Usage, Description, Options, Examples) ->
    io:put_chars([
        "Usage:\n  observer_cli ",
        Usage,
        " [TARGET OPTIONS] [OUTPUT OPTIONS]\n\n",
        Description,
        "\n\nCommand options:\n",
        case Options of
            "" -> "  None\n";
            _ -> Options
        end,
        "\nTarget options:\n",
        "  Use the context saved by connect, or pass --node NODE and exactly one of\n",
        "  --cookie-env NAME or --cookie-file PATH. --name-mode accepts short or long.\n",
        "\nOutput options:\n",
        "  --format text|term|json, --json, --redact\n",
        "  --timeout DURATION sets the command deadline, up to 120s.\n",
        "  DURATION accepts milliseconds (1500 or 1500ms) or seconds (2s).\n",
        "\nExamples:\n",
        Examples
    ]).

list_help(Command, Description, Sorts) ->
    remote_help(
        Command ++ " [--sort KEY] [--limit N]",
        Description,
        "  --sort KEY  " ++ Sorts ++ "\n" ++ limit_help(),
        "  observer_cli " ++ Command ++ " --sort " ++ hd(string:split(Sorts, " ")) ++
            " --limit 20\n"
    ).

counter_help(Command, Description, Sorts) ->
    remote_help(
        Command ++ " [--sort KEY] [--limit N] [--duration DURATION]",
        Description,
        "  --sort KEY           " ++ Sorts ++ "\n" ++
            "  --limit N            1..200; 20 by default\n" ++
            "  --duration DURATION  250ms..10s; show interval deltas instead of totals\n",
        "  observer_cli " ++ Command ++ " --duration 1500ms --limit 20\n"
    ).

limit_help() ->
    "  --limit N  1..200; 20 by default\n".

parse_args(Options) ->
    observer_cli_cli:parse(Options).

run_command(snapshot, Options) ->
    with_target(Options, fun(Target, _Capabilities) ->
        run_snapshot(Target, Options, command_request(snapshot, arguments(Options), Options))
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
run_command(Command, Options) ->
    with_target(Options, fun(Target, _Capabilities) ->
        run_dispatch(
            Target, Command, command_request(Command, arguments(Options), Options), Options
        )
    end).

arguments(Options) -> maps:get(arguments, Options, []).

command_request(trace, ["call", MFA], Options) ->
    (trace_request(call, Options))#{mfa => MFA, pid => maps:get(pid, Options)};
command_request(trace, ["stop"], _Options) ->
    #{action => stop_all};
command_request(process, [Target], Options) ->
    (request_options(Options))#{target => Target};
command_request(gen_server_state, [Target], Options) ->
    (request_options(Options))#{target => Target};
command_request(supervision_tree, [], Options) ->
    (request_options(Options))#{app => maps:get(app, Options)};
command_request(Command, _Arguments, Options) when Command =:= snapshot; Command =:= diagnose ->
    maps:with([deep, observe, app], Options);
command_request(_Command, _Arguments, Options) ->
    request_options(Options).

request_options(Options) ->
    lists:foldl(
        fun
            ({sort, Value}, Acc) ->
                Acc#{sort => list_to_existing_atom(Value)};
            ({limit, Value}, Acc) ->
                Acc#{limit => list_to_integer(Value)};
            ({duration, _Value}, Acc) ->
                {ok, Duration} = observer_cli_cli:duration(Options),
                Acc#{duration_ms => Duration};
            (_, Acc) ->
                Acc
        end,
        #{},
        maps:to_list(maps:with([sort, limit, duration], Options))
    ).

trace_request(Action, Options) ->
    {ok, Duration} = observer_cli_cli:trace_duration(Options),
    {ok, Max} = observer_cli_cli:trace_limit(Options),
    #{
        action => Action,
        duration_ms => Duration,
        max => Max,
        replace_existing_trace => maps:get(replace_existing_trace, Options, false)
    }.

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

run_snapshot(Target, Options, Request) ->
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
            [self(), snapshot, Request, DispatchOptions],
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

run_dispatch(Target, Command, Request, Options) ->
    {ok, Timeout} = observer_cli_cli:timeout(Options),
    Policy =
        case maps:is_key(redact, Options) of
            true -> redact;
            false -> include
        end,
    try
        erpc:call(
            Target,
            observer_cli_snapshot,
            dispatch,
            [self(), Command, Request, #{timeout_ms => Timeout, identifier_policy => Policy}],
            Timeout
        )
    of
        #{<<"status">> := <<"ok">>, <<"result">> := Response} ->
            dispatch_response(Response);
        #{<<"status">> := <<"error">>, <<"reason_code">> := Reason} ->
            {error, required_probe, Reason};
        _Invalid ->
            {error, schema, invalid_command_response}
    catch
        _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
    end.

dispatch_response(
    #{<<"capture">> := null, <<"errors">> := [#{<<"class">> := Class} | _]} = Response
) ->
    {ok, Response, observer_cli_cli:exit_code(binary_to_existing_atom(Class))};
dispatch_response(#{<<"capture">> := #{<<"status">> := <<"partial">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(partial)};
dispatch_response(#{<<"capture">> := #{<<"probes">> := Probes}} = Response) ->
    case lists:any(fun is_unavailable_probe/1, Probes) of
        true -> {ok, Response, observer_cli_cli:exit_code(scan_budget_exceeded)};
        false -> {ok, Response, observer_cli_cli:exit_code(success)}
    end;
dispatch_response(_Invalid) ->
    {error, schema, invalid_command_response}.

is_unavailable_probe(#{<<"status">> := <<"unavailable">>}) -> true;
is_unavailable_probe(_Probe) -> false.

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

-spec command_output(map(), map(), non_neg_integer()) -> no_return().
command_output(Options, Response, ExitCode) ->
    Format = command_format(Options),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            io:put_chars(standard_io, Output),
            erlang:halt(ExitCode);
        {error, EncodeError} ->
            output_encode_error(EncodeError)
    end.

-spec command_error(atom(), map() | atom(), atom(), term()) -> no_return().
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
    net_kernel:hidden_connect_node(Node) andalso
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
