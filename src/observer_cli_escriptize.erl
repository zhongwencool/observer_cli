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
    probe_target/7,
    command_timeout/2,
    dispatch_response/1,
    target_dispatch_error/1,
    validate_response/4,
    cleanup_outcome/2,
    capability_error/2,
    probe_response/3,
    command_output/3
]).
-endif.

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_RESPONSE_DEPTH, 32).
-define(CONTROLLER_CLEANUP_MARGIN_MS, 1000).

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
    with_target(Options, fun(Target, _Capabilities, Remaining) ->
        run_snapshot(
            Target,
            Options,
            command_request(snapshot, arguments(Options), Options),
            Remaining
        )
    end);
run_command(diagnose, Options) ->
    with_target(Options, fun(Target, _Capabilities, Remaining) ->
        run_diagnose(Target, Options, Remaining)
    end);
run_command(connect, Options) ->
    run_connect(Options);
run_command(status, Options) ->
    run_status(Options);
run_command(disconnect, _Options) ->
    run_disconnect();
run_command(Command, Options) ->
    with_target(Options, fun(Target, _Capabilities, Remaining) ->
        run_dispatch(
            Target,
            Command,
            command_request(Command, arguments(Options), Options),
            Options,
            Remaining
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
            probe_options(ContextOptions, fun(_Target, CapabilityResult, _Remaining) ->
                case CapabilityResult of
                    {ok, _Capabilities} ->
                        save_connected_context(ContextOptions, CapabilityResult);
                    {error, capability, capability_unavailable} ->
                        save_connected_context(ContextOptions, CapabilityResult);
                    Error ->
                        Error
                end
            end);
        {error, Reason} ->
            {error, argument, Reason}
    end.

save_connected_context(ContextOptions, CapabilityResult) ->
    case observer_cli_cli:save_context(ContextOptions) of
        ok -> probe_response(connect, ContextOptions, CapabilityResult);
        {error, Reason} -> {error, internal, Reason}
    end.

run_status(Options) ->
    case observer_cli_cli:load_context() of
        {ok, ContextOptions} ->
            ProbeOptions = maps:merge(ContextOptions, maps:with([timeout], Options)),
            probe_options(ProbeOptions, fun(_Target, CapabilityResult, _Remaining) ->
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

probe_response(Command, ContextOptions, {ok, _Capabilities}) ->
    probe_response(Command, ContextOptions, <<"available">>, []);
probe_response(Command, ContextOptions, {error, capability, capability_unavailable}) ->
    probe_response(Command, ContextOptions, <<"missing">>, [
        observer_cli_cli:error(capability, capability_unavailable)
    ]);
probe_response(_Command, _ContextOptions, {error, _Category, _Reason} = Error) ->
    Error.

probe_response(Command, #{node := Node}, DiagnosticsModule, Warnings) ->
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

run_snapshot(Target, Options, Request, Remaining) ->
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    case command_timeout(Options, Remaining) of
        {ok, Timeout} ->
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
                #{
                    <<"status">> := <<"ok">>,
                    <<"result">> := Response,
                    <<"cleanup_confirmed">> := true
                } ->
                    validated_response(
                        snapshot, Policy, Target, Response, fun snapshot_response/1
                    );
                #{
                    <<"status">> := <<"error">>,
                    <<"reason_code">> := Reason,
                    <<"cleanup_confirmed">> := true
                } ->
                    target_dispatch_error(Reason);
                _Invalid ->
                    {error, schema, invalid_snapshot_response}
            catch
                _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
            end;
        {error, Reason} ->
            {error, required_probe, Reason}
    end.

snapshot_response(#{<<"capture">> := #{<<"status">> := <<"complete">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(success)};
snapshot_response(#{<<"capture">> := #{<<"status">> := <<"partial">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(partial)};
snapshot_response(_Invalid) ->
    {error, schema, invalid_snapshot_response}.

run_dispatch(Target, Command, Request, Options, Remaining) ->
    Policy =
        case maps:is_key(redact, Options) of
            true -> redact;
            false -> include
        end,
    case command_timeout(Options, Remaining) of
        {ok, Timeout} ->
            try
                erpc:call(
                    Target,
                    observer_cli_snapshot,
                    dispatch,
                    [
                        self(),
                        Command,
                        Request,
                        #{timeout_ms => Timeout, identifier_policy => Policy}
                    ],
                    Timeout
                )
            of
                #{
                    <<"status">> := <<"ok">>,
                    <<"result">> := Response,
                    <<"cleanup_confirmed">> := true
                } ->
                    validated_response(
                        response_command(Command, Request),
                        Policy,
                        Target,
                        Response,
                        fun dispatch_response/1
                    );
                #{
                    <<"status">> := <<"error">>,
                    <<"reason_code">> := Reason,
                    <<"cleanup_confirmed">> := true
                } ->
                    target_dispatch_error(Reason);
                _Invalid ->
                    {error, schema, invalid_command_response}
            catch
                _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
            end;
        {error, Reason} ->
            {error, required_probe, Reason}
    end.

dispatch_response(
    #{<<"errors">> := Errors} = Response
) when Errors =/= [] ->
    {ok, Response, response_errors_exit_code(Response, Errors)};
dispatch_response(#{<<"capture">> := #{<<"status">> := <<"partial">>}} = Response) ->
    {ok, Response, observer_cli_cli:exit_code(partial)};
dispatch_response(#{<<"capture">> := #{<<"probes">> := Probes}} = Response) ->
    case unavailable_exit_code(Probes) of
        none -> {ok, Response, observer_cli_cli:exit_code(success)};
        ExitCode -> {ok, Response, ExitCode}
    end;
dispatch_response(_Invalid) ->
    {error, schema, invalid_command_response}.

unavailable_exit_code(Probes) ->
    Reasons = [
        Reason
     || #{<<"status">> := <<"unavailable">>, <<"reason_code">> := Reason} <- Probes
    ],
    case lists:member(<<"scan_budget_exceeded">>, Reasons) of
        true ->
            observer_cli_cli:exit_code(scan_budget_exceeded);
        false ->
            case lists:member(<<"capability_unavailable">>, Reasons) of
                true -> observer_cli_cli:exit_code(capability);
                false when Reasons =/= [] -> observer_cli_cli:exit_code(required_probe);
                false -> none
            end
    end.

run_diagnose(Target, Options, Remaining) ->
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    Request = maps:with([observe, deep, app], Options),
    case command_timeout(Options, Remaining) of
        {ok, Timeout} ->
            DispatchOptions = #{timeout_ms => Timeout, identifier_policy => Policy},
            try
                erpc:call(
                    Target,
                    observer_cli_snapshot,
                    dispatch,
                    [self(), diagnose, Request, DispatchOptions],
                    Timeout
                )
            of
                #{
                    <<"status">> := <<"ok">>,
                    <<"result">> := Response,
                    <<"cleanup_confirmed">> := true
                } ->
                    validated_response(
                        diagnose, Policy, Target, Response, fun diagnose_response/1
                    );
                #{
                    <<"status">> := <<"error">>,
                    <<"reason_code">> := Reason,
                    <<"cleanup_confirmed">> := true
                } ->
                    target_dispatch_error(Reason);
                _Invalid ->
                    {error, schema, invalid_diagnose_response}
            catch
                _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
            end;
        {error, Reason} ->
            {error, required_probe, Reason}
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
            probe_options(TargetOptions, fun(Target, CapabilityResult, Remaining) ->
                case CapabilityResult of
                    {ok, Capabilities} -> Fun(Target, Capabilities, Remaining);
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
        fun(ConnectedTarget, CapabilityResult, _Remaining) ->
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
    WorkDeadline = Deadline - ?CONTROLLER_CLEANUP_MARGIN_MS,
    case node() of
        nonode@nohost ->
            case
                net_kernel:start(undefined, #{
                    name_domain => NameMode, dist_listen => false, hidden => true
                })
            of
                {ok, _Pid} ->
                    Outcome =
                        try
                            connect_started(
                                Target, Cookie, WorkDeadline, RandomFun, ConnectFun, Fun
                            )
                        catch
                            _Class:_Reason:_Stacktrace -> {error, internal, controller_failed}
                        end,
                    cleanup_outcome(Outcome, stop_controller(remaining(Deadline)));
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
                    CapabilityResult = capabilities(Target, remaining(Deadline)),
                    Fun(Target, CapabilityResult, remaining(Deadline));
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
    {error, required_probe, target_timeout};
capabilities(Target, Timeout) ->
    try erpc:call(Target, observer_cli_snapshot, capabilities, [], Timeout) of
        #{protocol_version := 1} = Capabilities -> {ok, Capabilities};
        _Incompatible -> {error, capability, capability_unavailable}
    catch
        Class:Reason -> capability_error(Class, Reason)
    end.

capability_error(error, undef) ->
    {error, capability, capability_unavailable};
capability_error(error, {exception, undef, _Stacktrace}) ->
    {error, capability, capability_unavailable};
capability_error(error, {erpc, timeout}) ->
    {error, required_probe, target_timeout};
capability_error(error, {erpc, noconnection}) ->
    {error, connection, connection_failed};
capability_error(_Class, _Reason) ->
    {error, required_probe, capability_probe_failed}.

remaining(Deadline) ->
    erlang:max(0, Deadline - erlang:monotonic_time(millisecond)).

command_timeout(Options, Remaining) when Remaining > 0 ->
    case observer_cli_cli:timeout(Options) of
        {ok, Timeout} -> {ok, min(Timeout, Remaining)};
        Error -> Error
    end;
command_timeout(_Options, _Remaining) ->
    {error, target_timeout}.

validated_response(Command, Policy, Target, Response, Fun) ->
    case validate_response(Command, Policy, Target, Response) of
        ok -> apply_error_priority(Response, Fun(Response));
        {error, Reason} -> {error, schema, Reason}
    end.

apply_error_priority(#{<<"errors">> := []}, Outcome) ->
    Outcome;
apply_error_priority(
    #{<<"errors">> := Errors} = Response, {ok, Response, ExitCode}
) ->
    {ok, Response, max(ExitCode, response_errors_exit_code(Response, Errors))};
apply_error_priority(_Response, Outcome) ->
    Outcome.

response_command(trace, #{action := call}) -> trace_call;
response_command(trace, #{action := stop_all}) -> trace_stop_all;
response_command(Command, _Request) -> Command.

validate_response(Command, redact, Target, Response) when is_map(Response) ->
    validate_response_map(Command, redact, Target, Response);
validate_response(Command, include, Target, Response) when is_map(Response) ->
    validate_response_map(Command, include, Target, Response);
validate_response(_Command, _Policy, _Target, _Response) ->
    {error, invalid_command_response}.

validate_response_map(Command, Policy, Target, Response) ->
    ExpectedKeys = [
        <<"capture">>,
        <<"command">>,
        <<"data">>,
        <<"errors">>,
        <<"schema">>,
        <<"target">>,
        <<"warnings">>
    ],
    Checks = [
        lists:sort(maps:keys(Response)) =:= ExpectedKeys,
        maps:get(<<"schema">>, Response) =:= <<"observer_cli.cli/v1">>,
        maps:get(<<"command">>, Response) =:= atom_to_binary(Command),
        public_value(Response, 0),
        erlang:external_size(Response) =< ?MAX_RESPONSE_BYTES,
        valid_envelope(Response, Policy, atom_to_binary(Target)),
        valid_command_data(Command, Response),
        valid_policy_response(Policy, Response)
    ],
    case lists:all(fun(Check) -> Check end, Checks) of
        true -> ok;
        false -> {error, invalid_command_response}
    end.

valid_policy_response(include, _Response) -> true;
valid_policy_response(redact, Response) -> valid_redaction(Response).

valid_envelope(
    #{
        <<"capture">> := null,
        <<"target">> := Target,
        <<"data">> := null,
        <<"warnings">> := Warnings,
        <<"errors">> := Errors
    },
    Policy,
    ExpectedTarget
) ->
    (Target =:= null orelse valid_target(Target, Policy, ExpectedTarget)) andalso
        is_list(Warnings) andalso valid_errors(Errors) andalso Errors =/= [];
valid_envelope(
    #{
        <<"capture">> := Capture,
        <<"target">> := Target,
        <<"data">> := Data,
        <<"warnings">> := Warnings,
        <<"errors">> := Errors
    },
    Policy,
    ExpectedTarget
) ->
    valid_target(Target, Policy, ExpectedTarget) andalso is_map(Data) andalso
        is_list(Warnings) andalso
        valid_errors(Errors) andalso valid_capture(Capture).

valid_target(
    #{<<"node">> := Node, <<"otp_release">> := OtpRelease} = Target,
    include,
    ExpectedTarget
) ->
    map_size(Target) =:= 2 andalso Node =:= ExpectedTarget andalso valid_otp_release(OtpRelease);
valid_target(
    #{<<"node">> := Node, <<"otp_release">> := OtpRelease} = Target, redact, _ExpectedTarget
) ->
    map_size(Target) =:= 2 andalso valid_redacted_node(Node) andalso
        valid_otp_release(OtpRelease);
valid_target(_Target, _Policy, _ExpectedTarget) ->
    false.

valid_redacted_node(<<"node-", Digits/binary>>) ->
    try binary_to_integer(Digits) of
        Number -> Number > 0 andalso integer_to_binary(Number) =:= Digits
    catch
        error:badarg -> false
    end;
valid_redacted_node(_Node) ->
    false.

valid_otp_release(OtpRelease) when is_binary(OtpRelease) ->
    try binary_to_integer(OtpRelease) of
        Release -> Release > 0 andalso integer_to_binary(Release) =:= OtpRelease
    catch
        error:badarg -> false
    end;
valid_otp_release(_OtpRelease) ->
    false.

valid_capture(
    #{
        <<"status">> := Status,
        <<"started_at">> := Started,
        <<"finished_at">> := Finished,
        <<"duration_ms">> := Duration,
        <<"probes">> := Probes,
        <<"observer_effects">> := Effects
    } = Capture
) ->
    map_size(Capture) =:= 6 andalso
        (Status =:= <<"complete">> orelse Status =:= <<"partial">>) andalso
        valid_rfc3339(Started) andalso valid_rfc3339(Finished) andalso
        is_integer(Duration) andalso Duration >= 0 andalso
        is_list(Probes) andalso lists:all(fun valid_probe/1, Probes) andalso is_list(Effects);
valid_capture(_Capture) ->
    false.

valid_rfc3339(Time) when is_binary(Time) ->
    case byte_size(Time) > 0 andalso binary:last(Time) =:= $Z of
        true ->
            try calendar:rfc3339_to_system_time(binary_to_list(Time), [{unit, millisecond}]) of
                Value -> is_integer(Value)
            catch
                _:_ -> false
            end;
        false ->
            false
    end;
valid_rfc3339(_Time) ->
    false.

valid_probe(
    #{
        <<"id">> := Id,
        <<"required">> := Required,
        <<"status">> := Status,
        <<"reason_code">> := Reason,
        <<"duration_ms">> := Duration,
        <<"samples">> := Samples,
        <<"coverage">> := Coverage
    } = Probe
) ->
    map_size(Probe) =:= 7 andalso is_binary(Id) andalso is_boolean(Required) andalso
        lists:member(Status, [<<"ok">>, <<"unavailable">>, <<"timeout">>, <<"error">>]) andalso
        valid_probe_reason(Status, Reason) andalso
        is_integer(Duration) andalso Duration >= 0 andalso
        is_integer(Samples) andalso Samples >= 0 andalso is_list(Coverage);
valid_probe(_Probe) ->
    false.

valid_probe_reason(<<"ok">>, null) -> true;
valid_probe_reason(Status, Reason) when Status =/= <<"ok">> -> is_binary(Reason);
valid_probe_reason(_Status, _Reason) -> false.

valid_errors(Errors) when is_list(Errors) ->
    lists:all(
        fun
            (#{<<"class">> := Class, <<"reason_code">> := Reason} = Error) ->
                valid_error_keys(Error) andalso valid_error_class(Class, Reason);
            (_) ->
                false
        end,
        Errors
    );
valid_errors(_Errors) ->
    false.

valid_error_keys(Error) ->
    Keys = lists:sort(maps:keys(Error)),
    lists:member(Keys, [
        [<<"class">>, <<"reason_code">>],
        [<<"class">>, <<"probe">>, <<"reason_code">>]
    ]).

valid_error_class(<<"cleanup">>, <<"cleanup_unconfirmed">>) ->
    true;
valid_error_class(<<"internal">>, Reason) ->
    lists:member(Reason, [<<"internal_error">>, <<"capture_internal_error">>]);
valid_error_class(<<"schema">>, Reason) ->
    lists:member(Reason, [
        <<"field_too_large">>,
        <<"invalid_evidence_pointer">>,
        <<"invalid_identifier">>,
        <<"invalid_identifier_policy">>,
        <<"invalid_request">>,
        <<"invalid_schema">>,
        <<"response_too_deep">>,
        <<"response_too_large">>
    ]);
valid_error_class(<<"capability">>, <<"capability_unavailable">>) ->
    true;
valid_error_class(Class, Reason) when is_binary(Reason) ->
    not lists:member(Reason, [
        <<"capability_unavailable">>,
        <<"capture_internal_error">>,
        <<"cleanup_unconfirmed">>,
        <<"field_too_large">>,
        <<"internal_error">>,
        <<"invalid_evidence_pointer">>,
        <<"invalid_identifier">>,
        <<"invalid_identifier_policy">>,
        <<"invalid_request">>,
        <<"invalid_schema">>,
        <<"response_too_deep">>,
        <<"response_too_large">>
    ]) andalso
        lists:member(Class, [
            <<"argument">>,
            <<"connection">>,
            <<"partial">>,
            <<"required_probe">>,
            <<"safety_refusal">>
        ]);
valid_error_class(_Class, _Reason) ->
    false.

valid_command_data(Command, #{<<"capture">> := null}) ->
    lists:member(Command, [trace_call, trace_stop_all]);
valid_command_data(
    Command,
    #{<<"capture">> := #{<<"status">> := Status, <<"probes">> := Probes}} = Response
) ->
    valid_required_probes(Command, Status, Probes) andalso
        valid_resource_wrappers(Response) andalso valid_command_payload(Command, Response, Probes);
valid_command_data(_Command, _Response) ->
    false.

valid_command_payload(Command, #{<<"data">> := Data} = Response, Probes) ->
    case Command of
        diagnose ->
            valid_diagnose_data(Response);
        snapshot ->
            is_map(Data);
        memory ->
            valid_map_fields(Data, [<<"runtime">>, <<"memory">>]);
        schedulers ->
            valid_map_fields(Data, [<<"status">>]);
        distribution ->
            valid_map_fields(Data, [
                <<"connected_peers">>, <<"returned_peer_count">>, <<"truncated">>
            ]);
        process ->
            valid_map_fields(Data, [<<"status">>]);
        gen_server_state ->
            valid_map_fields(Data, [<<"status">>, <<"risk_level">>]);
        supervision_tree ->
            valid_map_fields(Data, [<<"status">>, <<"risk_level">>]);
        trace_call ->
            valid_map_fields(Data, [<<"reason">>, <<"trace">>]);
        trace_stop_all ->
            valid_map_fields(Data, [<<"reason">>, <<"trace">>]);
        _ ->
            valid_list_command_payload(Command, Data, Probes)
    end.

valid_map_fields(Data, Fields) when is_map(Data) ->
    lists:all(fun(Field) -> maps:is_key(Field, Data) end, Fields);
valid_map_fields(_Data, _Fields) ->
    false.

valid_list_command_payload(Command, Data, [Probe]) when
    Command =:= processes;
    Command =:= applications;
    Command =:= ets;
    Command =:= mnesia;
    Command =:= network;
    Command =:= ports;
    Command =:= sockets
->
    case maps:get(<<"status">>, Probe) of
        <<"ok">> -> valid_complete_resource_wrapper(Data);
        _ -> is_map(Data)
    end;
valid_list_command_payload(_Command, _Data, _Probes) ->
    false.

valid_required_probes(snapshot, Status, Probes) ->
    Required = [Probe || #{<<"required">> := true} = Probe <- Probes],
    lists:sort([maps:get(<<"id">>, Probe) || Probe <- Required]) =:=
        [<<"memory">>, <<"resources">>, <<"runtime">>] andalso
        probe_statuses_match_capture(Status, Required, Probes);
valid_required_probes(diagnose, Status, Probes) ->
    Required = [Probe || #{<<"required">> := true} = Probe <- Probes],
    [maps:get(<<"id">>, Probe) || Probe <- Required] =:= [<<"core_limits">>] andalso
        probe_statuses_match_capture(Status, Required, Probes);
valid_required_probes(Command, Status, Probes) ->
    RequiredId = required_probe_id(Command),
    case Probes of
        [#{<<"required">> := true, <<"id">> := RequiredId} = Probe] ->
            probe_statuses_match_capture(Status, [Probe], Probes);
        _ ->
            false
    end.

probe_statuses_match_capture(<<"complete">>, Required, Probes) ->
    lists:all(
        fun(Probe) ->
            lists:member(maps:get(<<"status">>, Probe), [<<"ok">>, <<"unavailable">>])
        end,
        Required
    ) andalso
        not lists:any(fun probe_started_failure/1, Probes);
probe_statuses_match_capture(<<"partial">>, _Required, Probes) ->
    lists:any(fun probe_started_failure/1, Probes).

probe_started_failure(#{<<"status">> := Status}) ->
    lists:member(Status, [<<"timeout">>, <<"error">>]).

required_probe_id(memory) -> <<"memory">>;
required_probe_id(schedulers) -> <<"scheduler_wall_time">>;
required_probe_id(distribution) -> <<"distribution">>;
required_probe_id(processes) -> <<"process_inventory">>;
required_probe_id(process) -> <<"process_info">>;
required_probe_id(applications) -> <<"application_inventory">>;
required_probe_id(ets) -> <<"ets_inventory">>;
required_probe_id(mnesia) -> <<"mnesia_inventory">>;
required_probe_id(network) -> <<"network_inventory">>;
required_probe_id(ports) -> <<"port_inventory">>;
required_probe_id(sockets) -> <<"socket_inventory">>;
required_probe_id(gen_server_state) -> <<"gen_server_state">>;
required_probe_id(supervision_tree) -> <<"supervision_tree">>;
required_probe_id(trace_call) -> <<"trace">>;
required_probe_id(trace_stop_all) -> <<"trace">>;
required_probe_id(_Command) -> undefined.

valid_resource_wrappers(Value) when is_map(Value) ->
    valid_resource_wrapper(Value) andalso
        lists:all(fun valid_resource_wrappers/1, maps:values(Value));
valid_resource_wrappers(Value) when is_list(Value) ->
    lists:all(fun valid_resource_wrappers/1, Value);
valid_resource_wrappers(_Value) ->
    true.

valid_resource_wrapper(
    #{
        <<"items">> := _,
        <<"scanned_count">> := _,
        <<"eligible_count">> := _,
        <<"returned_count">> := _
    } = Wrapper
) ->
    valid_complete_resource_wrapper(Wrapper);
valid_resource_wrapper(_Value) ->
    true.

valid_complete_resource_wrapper(Wrapper) ->
    case Wrapper of
        #{
            <<"items">> := Items,
            <<"scanned_count">> := Scanned,
            <<"eligible_count">> := Eligible,
            <<"returned_count">> := Returned,
            <<"dropped_count">> := Dropped,
            <<"complete">> := Complete,
            <<"truncated">> := Truncated
        } ->
            is_list(Items) andalso is_integer(Scanned) andalso Scanned >= 0 andalso
                is_integer(Eligible) andalso Eligible >= 0 andalso Eligible =< Scanned andalso
                is_integer(Returned) andalso Returned =:= length(Items) andalso
                Returned =< Eligible andalso is_integer(Dropped) andalso Dropped >= 0 andalso
                Dropped =:= Eligible - Returned andalso
                is_boolean(Complete) andalso is_boolean(Truncated);
        _ ->
            false
    end.

valid_diagnose_data(#{<<"data">> := Data} = Response) ->
    lists:sort(maps:keys(Data)) =:=
        [
            <<"context">>,
            <<"findings">>,
            <<"ruleset">>,
            <<"ruleset_version">>,
            <<"sampling_plan">>,
            <<"skipped">>,
            <<"summary">>,
            <<"suspects">>
        ] andalso
        is_binary(maps:get(<<"ruleset">>, Data)) andalso
        is_integer(maps:get(<<"ruleset_version">>, Data)) andalso
        is_map(maps:get(<<"sampling_plan">>, Data)) andalso
        is_map(maps:get(<<"context">>, Data)) andalso
        is_list(maps:get(<<"findings">>, Data)) andalso
        is_list(maps:get(<<"suspects">>, Data)) andalso
        is_list(maps:get(<<"skipped">>, Data)) andalso
        is_binary(maps:get(<<"summary">>, Data)) andalso valid_findings(Response).

valid_findings(#{<<"data">> := #{<<"findings">> := Findings}} = Response) ->
    lists:all(
        fun
            (
                #{
                    <<"id">> := Id,
                    <<"severity">> := Severity,
                    <<"entity">> := #{<<"type">> := Type, <<"id">> := EntityId},
                    <<"summary">> := Summary,
                    <<"ruleset_version">> := RulesetVersion,
                    <<"evidence">> := Evidence,
                    <<"recommendations">> := Recommendations
                }
            ) ->
                is_binary(Id) andalso lists:member(Severity, [<<"warning">>, <<"critical">>]) andalso
                    is_binary(Type) andalso is_binary(EntityId) andalso is_binary(Summary) andalso
                    is_integer(RulesetVersion) andalso Evidence =/= [] andalso
                    lists:all(fun(Item) -> valid_evidence(Response, Item) end, Evidence) andalso
                    is_list(Recommendations) andalso
                    lists:all(fun is_binary/1, Recommendations);
            (_) ->
                false
        end,
        Findings
    ).

valid_evidence(
    Response,
    #{
        <<"path">> := Path,
        <<"sample_index">> := SampleIndex,
        <<"monotonic_midpoint_ms">> := Midpoint,
        <<"observed">> := Observed,
        <<"operator">> := Operator,
        <<"threshold">> := Threshold
    }
) ->
    is_binary(Path) andalso pointer_exists(Response, Path) andalso
        is_integer(SampleIndex) andalso SampleIndex >= 0 andalso is_integer(Midpoint) andalso
        is_number(Observed) andalso is_binary(Operator) andalso is_number(Threshold);
valid_evidence(_Response, _Evidence) ->
    false.

valid_redaction(Value) when is_map(Value) ->
    lists:all(
        fun({Key, Item}) ->
            valid_redacted_identifier_field(Key, Item) andalso valid_redaction(Item)
        end,
        maps:to_list(Value)
    );
valid_redaction(Value) when is_list(Value) ->
    lists:all(fun valid_redaction/1, Value);
valid_redaction(Value) when is_binary(Value) ->
    not raw_identifier_text(Value);
valid_redaction(_Value) ->
    true.

valid_redacted_identifier_field(_Key, null) ->
    true;
valid_redacted_identifier_field(Key, Value) when is_binary(Value) ->
    case identifier_field_prefix(Key) of
        undefined -> true;
        Prefix -> valid_stable_identifier(Prefix, Value)
    end;
valid_redacted_identifier_field(_Key, _Value) ->
    true.

identifier_field_prefix(<<"pid">>) -> <<"pid-">>;
identifier_field_prefix(<<"owner">>) -> <<"pid-">>;
identifier_field_prefix(<<"group_leader">>) -> <<"pid-">>;
identifier_field_prefix(<<"controller">>) -> <<"pid-">>;
identifier_field_prefix(<<"controller_peer">>) -> <<"peer-">>;
identifier_field_prefix(<<"peer">>) -> <<"peer-">>;
identifier_field_prefix(<<"node">>) -> <<"node-">>;
identifier_field_prefix(<<"module">>) -> <<"module-">>;
identifier_field_prefix(<<"function">>) -> <<"function-">>;
identifier_field_prefix(<<"application">>) -> <<"application-">>;
identifier_field_prefix(<<"table">>) -> <<"table-">>;
identifier_field_prefix(<<"socket">>) -> <<"socket-">>;
identifier_field_prefix(<<"port">>) -> <<"port-">>;
identifier_field_prefix(<<"registered_name">>) -> <<"name-">>;
identifier_field_prefix(_Key) -> undefined.

valid_stable_identifier(Prefix, Value) ->
    PrefixSize = byte_size(Prefix),
    case Value of
        <<Prefix:PrefixSize/binary, Digits/binary>> ->
            try binary_to_integer(Digits) of
                Number -> Number > 0 andalso integer_to_binary(Number) =:= Digits
            catch
                error:badarg -> false
            end;
        _ ->
            false
    end.

raw_identifier_text(Value) ->
    lists:any(
        fun(Pattern) -> re:run(Value, Pattern, [{capture, none}]) =:= match end,
        [
            <<"^<[0-9]+\\.[0-9]+\\.[0-9]+>$">>,
            <<"^#Port<[0-9]+\\.[0-9]+>$">>,
            <<"^#Ref<[0-9]+(\\.[0-9]+)+>$">>
        ]
    ).

pointer_exists(Response, <<"/", Pointer/binary>>) ->
    case decode_pointer_segments(binary:split(Pointer, <<"/">>, [global]), []) of
        {ok, Segments} -> resolve_pointer(Response, Segments);
        error -> false
    end;
pointer_exists(_Response, _Pointer) ->
    false.

resolve_pointer(_Value, []) ->
    true;
resolve_pointer(Map, [Key | Rest]) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> resolve_pointer(Value, Rest);
        error -> false
    end;
resolve_pointer(List, [Index | Rest]) when is_list(List) ->
    try binary_to_integer(Index) of
        Number when Number >= 0, Number < length(List) ->
            case integer_to_binary(Number) =:= Index of
                true -> resolve_pointer(lists:nth(Number + 1, List), Rest);
                false -> false
            end;
        _ ->
            false
    catch
        error:badarg -> false
    end;
resolve_pointer(_Value, _Segments) ->
    false.

decode_pointer_segments([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_pointer_segments([Segment | Rest], Acc) ->
    case decode_pointer_segment(Segment, <<>>) of
        {ok, Decoded} -> decode_pointer_segments(Rest, [Decoded | Acc]);
        error -> error
    end.

decode_pointer_segment(<<>>, Acc) ->
    {ok, Acc};
decode_pointer_segment(<<"~0", Rest/binary>>, Acc) ->
    decode_pointer_segment(Rest, <<Acc/binary, "~">>);
decode_pointer_segment(<<"~1", Rest/binary>>, Acc) ->
    decode_pointer_segment(Rest, <<Acc/binary, "/">>);
decode_pointer_segment(<<"~", _/binary>>, _Acc) ->
    error;
decode_pointer_segment(<<Byte, Rest/binary>>, Acc) ->
    decode_pointer_segment(Rest, <<Acc/binary, Byte>>).

public_value(_Value, Depth) when Depth > ?MAX_RESPONSE_DEPTH ->
    false;
public_value(null, _Depth) ->
    true;
public_value(true, _Depth) ->
    true;
public_value(false, _Depth) ->
    true;
public_value(Value, _Depth) when is_integer(Value); is_float(Value) ->
    true;
public_value(Value, _Depth) when is_binary(Value) ->
    is_binary(unicode:characters_to_binary(Value));
public_value(Value, Depth) when is_list(Value) ->
    lists:all(fun(Item) -> public_value(Item, Depth + 1) end, Value);
public_value(Value, Depth) when is_map(Value) ->
    lists:all(
        fun({Key, Item}) -> is_binary(Key) andalso public_value(Item, Depth + 1) end,
        maps:to_list(Value)
    );
public_value(_Value, _Depth) ->
    false.

target_dispatch_error(<<"invalid_schema">>) ->
    {error, schema, invalid_schema};
target_dispatch_error(<<"invalid_request">>) ->
    {error, schema, invalid_request};
target_dispatch_error(<<"field_too_large">>) ->
    {error, schema, field_too_large};
target_dispatch_error(<<"response_too_deep">>) ->
    {error, schema, response_too_deep};
target_dispatch_error(<<"response_too_large">>) ->
    {error, schema, response_too_large};
target_dispatch_error(<<"invalid_evidence_pointer">>) ->
    {error, schema, invalid_evidence_pointer};
target_dispatch_error(<<"invalid_identifier">>) ->
    {error, schema, invalid_identifier};
target_dispatch_error(<<"invalid_identifier_policy">>) ->
    {error, schema, invalid_identifier_policy};
target_dispatch_error(<<"cleanup_unconfirmed">>) ->
    {error, cleanup, cleanup_unconfirmed};
target_dispatch_error(<<"internal_error">>) ->
    {error, internal, internal_error};
target_dispatch_error(<<"capability_unavailable">>) ->
    {error, capability, capability_unavailable};
target_dispatch_error(<<"worker_heap_limit_exceeded">>) ->
    {error, safety_refusal, worker_heap_limit_exceeded};
target_dispatch_error(Reason) ->
    {error, required_probe, Reason}.

response_errors_exit_code(Response, Errors) ->
    CaptureCode =
        case Response of
            #{<<"capture">> := #{<<"status">> := <<"partial">>}} -> 3;
            _ -> 0
        end,
    lists:max([
        CaptureCode
        | [
            response_class_priority(maps:get(<<"class">>, Error, <<"schema">>))
         || Error <- Errors, is_map(Error)
        ]
    ]).

response_class_priority(<<"cleanup">>) -> 4;
response_class_priority(<<"schema">>) -> 4;
response_class_priority(<<"internal">>) -> 4;
response_class_priority(<<"safety_refusal">>) -> 3;
response_class_priority(<<"connection">>) -> 3;
response_class_priority(<<"required_probe">>) -> 3;
response_class_priority(<<"partial">>) -> 3;
response_class_priority(<<"capability">>) -> 2;
response_class_priority(<<"argument">>) -> 2;
response_class_priority(_Class) -> 4.

cleanup_outcome(_Outcome, {error, cleanup_unconfirmed}) ->
    {error, cleanup, cleanup_unconfirmed};
cleanup_outcome(Outcome, ok) ->
    Outcome.

stop_controller(Timeout) when Timeout =< 0 ->
    {error, cleanup_unconfirmed};
stop_controller(Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    case whereis(net_kernel) of
        undefined ->
            controller_stopped(Deadline);
        _Pid ->
            Parent = self(),
            {Stopper, Monitor} = spawn_monitor(fun() ->
                Result = net_kernel:stop(),
                Parent ! {self(), Result}
            end),
            receive
                {Stopper, _Result} ->
                    erlang:demonitor(Monitor, [flush]),
                    controller_stopped(Deadline);
                {'DOWN', Monitor, process, Stopper, _Reason} ->
                    {error, cleanup_unconfirmed}
            after remaining(Deadline) ->
                exit(Stopper, kill),
                erlang:demonitor(Monitor, [flush]),
                {error, cleanup_unconfirmed}
            end
    end.

controller_stopped(Deadline) ->
    case erlang:is_alive() of
        false ->
            ok;
        true ->
            case remaining(Deadline) of
                0 ->
                    {error, cleanup_unconfirmed};
                Timeout ->
                    receive
                    after min(10, Timeout) -> controller_stopped(Deadline)
                    end
            end
    end.

-spec command_output(map(), map(), non_neg_integer()) -> no_return().
command_output(Options, Response, ExitCode) ->
    Format = command_format(Options),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            io:put_chars(standard_io, Output),
            erlang:halt(ExitCode);
        {error, EncodeError} ->
            output_command_encode_error(
                maps:get(<<"command">>, Response, <<"unknown">>), Format, EncodeError
            )
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

-spec output_encode_error(map()) -> no_return().
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

-spec output_command_encode_error(binary() | atom(), text | term | json, map()) -> no_return().
output_command_encode_error(_Command, json, #{reason := json_unavailable} = EncodeError) ->
    output_encode_error(EncodeError);
output_command_encode_error(Command, Format, EncodeError) when Format =:= term; Format =:= json ->
    Category = maps:get(category, EncodeError),
    Reason = maps:get(reason, EncodeError),
    Error = observer_cli_cli:error(Category, Reason),
    Response = observer_cli_cli:envelope(Command, null, null, null, [], [Error]),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            io:put_chars(standard_io, Output),
            erlang:halt(observer_cli_cli:exit_code(EncodeError));
        {error, _RetryError} ->
            output_encode_error(EncodeError)
    end;
output_command_encode_error(_Command, _Format, EncodeError) ->
    output_encode_error(EncodeError).

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
