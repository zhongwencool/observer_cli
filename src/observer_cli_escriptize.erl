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
    application_modules/1,
    run/3,
    run/4,
    run_remote/4,
    remote_module_available/1,
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
    probe_response/5,
    command_output/3,
    command_output_device/2,
    response_command/2,
    valid_probe_reason/2,
    required_probe_id/1,
    pointer_exists/2,
    public_value/2,
    public_text/1,
    command_format/1,
    command_identity/2,
    command_display/1,
    command_help_command/1,
    command_from_args/1,
    requested_format/1,
    command_error/4,
    output_encode_error/1,
    output_command_encode_error/3,
    valid_target/3,
    valid_otp_release/1,
    valid_rfc3339/1,
    valid_probe/1,
    valid_issues/1,
    valid_issue_class/2,
    response_exit_code/1,
    valid_command_payload/3,
    valid_list_command_payload/3,
    valid_required_probes/3,
    valid_findings/1,
    valid_evidence/2,
    valid_redaction/1,
    valid_redacted_identifier_field/2,
    identifier_field_prefix/1,
    valid_stable_identifier/2,
    command_from_arguments/1,
    maybe_set_target_cookie/2,
    cookie_atom/1,
    stop_controller/1,
    request_options/1,
    snapshot_response/1,
    diagnose_response/1,
    active_options/1,
    random_cookie/1,
    connect_before/3,
    capabilities/2,
    compatible_capabilities/1,
    validated_response/5,
    validate_logs_response/3,
    run_snapshot/4,
    run_dispatch/5,
    run_diagnose/3,
    target_dispatch/6,
    probe_options/2,
    connect_started/6,
    ensure_net_kernel_name_mode/1,
    run_connect/1,
    ensure_output_format/2,
    save_connected_context/2,
    run_status/1,
    run_disconnect/0,
    with_active_target/2,
    controller_stopped/1
]).
-endif.

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_RESPONSE_DEPTH, 32).
-define(CONTROLLER_CLEANUP_MARGIN_MS, 1000).

%% @doc escript main
-spec main([string()]) -> ok | no_return().

main([]) ->
    usage();
main(["--help"]) ->
    usage();
main(["-h"]) ->
    usage();
main(["help"]) ->
    usage();
main(["help", Command]) ->
    help_target([Command]);
main(["--version"]) ->
    version();
main(["version"]) ->
    root_error({unknown_command, "version"});
main(Options) ->
    case requested_help(Options) of
        true -> help_target(Options);
        false -> main_options(Options)
    end.

main_options(Options) ->
    case parse_args(Options) of
        {ok, #{
            route := tui,
            target := TargetNode,
            cookie := Cookie,
            interval := Interval
        }} ->
            run_tui(TargetNode, Cookie, Interval);
        {ok, #{
            route := command,
            command := Command,
            arguments := Arguments,
            options := CommandOptions
        }} ->
            CommandIdentity = command_identity(Command, Arguments),
            case run_command(Command, CommandOptions#{arguments => Arguments}) of
                {ok, Response, ExitCode} ->
                    command_output(CommandOptions, Response, ExitCode);
                {error, Category, Reason} ->
                    command_error(CommandIdentity, CommandOptions, Category, Reason)
            end;
        {error, Error} ->
            case command_from_args(Options) of
                undefined ->
                    command_error(
                        unknown,
                        requested_format(Options),
                        maps:get(category, Error),
                        maps:get(reason, Error)
                    );
                Command ->
                    command_error(
                        Command,
                        requested_format(Options),
                        maps:get(category, Error),
                        maps:get(reason, Error)
                    )
            end
    end.

command_identity(trace, ["call" | _]) -> trace_call;
command_identity(trace, ["stop" | _]) -> trace_stop_all;
command_identity(Command, _Arguments) -> Command.

requested_help(Options) ->
    lists:member("--help", Options) orelse lists:member("-h", Options).

help_target(["tui" | _]) ->
    tui_help();
help_target(["trace", "call" | _]) ->
    trace_call_help();
help_target(["trace", "stop" | _]) ->
    trace_stop_help();
help_target([[$- | _] | _]) ->
    usage();
help_target([Command | _]) ->
    case observer_cli_cli:command(Command) of
        undefined -> root_error({unknown_command, Command});
        _ -> command_help(Command)
    end;
help_target([]) ->
    usage().

version() ->
    #{bundle_version := Version, protocol_version := Protocol} =
        observer_cli_snapshot:capabilities(),
    io:put_chars(
        io_lib:format(
            "observer_cli ~ts~nschema ~ts~nprotocol ~B~ncontroller OTP ~ts~n",
            [
                Version,
                observer_cli_cli:schema(),
                Protocol,
                erlang:system_info(otp_release)
            ]
        )
    ).

-spec root_error(term()) -> no_return().
root_error(Reason) ->
    command_error(unknown, text, argument, Reason).

run_tui(TargetNode, Cookie, Interval) ->
    try run(TargetNode, cookie_atom(Cookie), Interval) of
        Result -> Result
    catch
        error:{remote_otp_mismatch, ControllerOtp, TargetOtp}:_Stacktrace ->
            command_error(
                tui,
                text,
                connection,
                {remote_otp_mismatch, ControllerOtp, TargetOtp}
            );
        _Class:_Reason:_Stacktrace ->
            command_error(tui, text, connection, tui_start_failed)
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
        "  observer_cli COMMAND [ARGUMENTS] [OPTIONS]\n"
        "  observer_cli tui NODE [COOKIE REFRESH_MS]\n"
        "  observer_cli connect --node NODE\n"
        "    (--cookie-env NAME | --cookie-file PATH)\n"
        "\n"
        "Target context:\n"
        "  connect             Verify and save a target context\n"
        "  status              Check the saved target context\n"
        "  disconnect          Remove the saved target context\n"
        "\n"
        "Diagnostics:\n"
        "  diagnose            Detect likely VM problems and report evidence\n"
        "  snapshot            Collect a point-in-time VM fact bundle\n"
        "  logs                Read bounded retained logs from one configured file\n"
        "\n"
        "VM health:\n"
        "  memory              Show VM memory and allocator usage\n"
        "  schedulers          Measure scheduler utilization and run queues\n"
        "  distribution        Show connected Erlang nodes\n"
        "  network             Show VM network I/O\n"
        "\n"
        "Runtime resources:\n"
        "  processes           List top processes\n"
        "  process PID_OR_NAME Inspect one process\n"
        "  applications        Group process resources by application\n"
        "  ets                 List ETS tables\n"
        "  mnesia              List local Mnesia tables\n"
        "  ports               List Erlang ports\n"
        "  port PORT_ID        Inspect one Erlang port\n"
        "  sockets             List OTP sockets\n"
        "\n"
        "OTP structures:\n"
        "  otp-state PID_OR_NAME\n"
        "                      Inspect an asserted OTP behavior state shape\n"
        "  supervision-tree --app APP\n"
        "                      Show an application supervision tree\n"
        "\n"
        "Tracing:\n"
        "  trace call MFA      Run a bounded function trace\n"
        "  trace stop --all    Clear all node-static tracing\n"
        "\n"
        "Interactive:\n"
        "  tui NODE            Open the terminal UI; see 'tui --help'\n"
        "\n"
        "Help and version:\n"
        "  --help, -h          Show this overview\n"
        "  help COMMAND        Show command help (also COMMAND --help)\n"
        "  --version           Show local bundle, protocol, schema, and OTP\n"
        "\n"
        "Target options:\n"
        "  --node NODE         Use an explicit target instead of saved context\n"
        "  --cookie-env NAME   Read the target cookie from an environment variable\n"
        "  --cookie-file PATH  Read the target cookie from a file\n"
        "  --name-mode MODE    short or long; inferred from NODE by default\n"
        "\n"
        "Identifier policy:\n"
        "  snapshot and diagnose redact identifiers by default.\n"
        "  Inspection and tracing include identifiers by default.\n"
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
        "Exit status: 0 success, 1 diagnose findings, 2 usage/capability,\n"
        "3 runtime/refusal/partial, 4 internal/schema/cleanup.\n"
        "\n"
        "Run 'observer_cli COMMAND --help' for options and examples.\n"
    ).

command_help("connect") ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli connect --node NODE\n"
        "    (--cookie-env NAME | --cookie-file PATH) [OPTIONS]\n"
        "\n"
        "Verify the target and save its node and cookie-source metadata.\n"
        "No cookie or persistent connection is stored. Later commands use this\n"
        "context by default. The target must already have a compatible\n"
        "observer_cli diagnostics bundle installed.\n"
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
        "  observer_cli status [--timeout DURATION]\n"
        "    [--format text|term|json] [--json]\n"
        "\n"
        "Probe the target saved by connect. This starts a fresh connection; connect\n"
        "does not run a daemon. The result reports target OTP, diagnostics bundle,\n"
        "name mode, and cookie-source metadata without exposing the cookie.\n"
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
        "Collect bounded runtime facts. The default avoids resource inventories;\n"
        "--deep adds admitted Top-N scans.",
        "  --deep                 Add process, table, network, port, and socket\n"
        "                         inventories\n"
        "  --include-identifiers  Include real node, PID, name, and MFA identifiers\n",
        "  observer_cli snapshot --deep --format term\n"
    );
command_help("diagnose") ->
    remote_help(
        "diagnose [DIAGNOSTIC OPTIONS]",
        "Run evidence-backed diagnostics. With no mode option, perform a quick\n"
        "point-in-time diagnosis. Exit 1 means a complete report found warnings\n"
        "or critical findings.",
        "  --observe DURATION     Sample for 5s..60s\n"
        "  --deep                 Add deep resource observation; requires --observe\n"
        "  --app APP              Observe one application; requires --observe\n"
        "  --include-identifiers  Include real node, PID, name, and MFA identifiers\n",
        "  observer_cli diagnose\n"
        "  observer_cli diagnose --observe 30s --deep --json\n"
        "  observer_cli diagnose --observe 30s --app kernel\n"
    );
command_help("memory") ->
    remote_help(
        "memory",
        "Show point-in-time BEAM memory, allocator, and runtime facts.\n"
        "This is not host RSS.",
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
        "Show connected visible and hidden Erlang nodes and available\n"
        "distribution context.",
        limit_help(),
        "  observer_cli distribution --limit 50\n"
    );
command_help("processes") ->
    remote_help(
        "processes [--sort KEY] [--limit N] [--duration DURATION]",
        "List top processes with TUI-style context using bounded explicit-key\n"
        "inspection. --duration reports interval deltas instead of totals.",
        "  --sort KEY           memory (default), message_queue_len, reductions,\n"
        "                       binary_memory, or total_heap_size\n"
        "  --limit N            1..200; 20 by default\n"
        "  --duration DURATION  250ms..10s\n",
        "  observer_cli processes --sort memory --limit 20\n"
        "  observer_cli processes --sort reductions --duration 1500ms\n"
    );
command_help("process") ->
    remote_help(
        "process PID_OR_NAME",
        "Inspect safe metadata for one local PID or registered process name.\n"
        "A bounded, normalized current stacktrace is included. Messages, the\n"
        "dictionary, and arbitrary state are not returned.",
        "",
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
        "List local Mnesia table metadata. A stopped Mnesia application is\n"
        "reported as not_running.",
        "memory (default), size"
    );
command_help("network") ->
    counter_help(
        "network",
        "Show VM port-driver and legacy inet counters, not all host network traffic.",
        "oct (default), recv_oct, send_oct,\n"
        "                       cnt, recv_cnt, send_cnt"
    );
command_help("ports") ->
    list_help(
        "ports",
        "List non-inet Erlang Port metadata and counters, not TCP or UDP port numbers.",
        "queue_size (default), memory, input, output, io"
    );
command_help("port") ->
    remote_help(
        "port '#Port<0.N>' [--redact]",
        "Inspect one target-local Erlang port by its raw Port text.",
        "  --redact  Hide port, process, endpoint, interface, and netns identifiers\n",
        "  observer_cli port '#Port<0.12>'\n"
    );
command_help("sockets") ->
    counter_help(
        "sockets",
        "List sockets visible through the OTP socket registry.",
        "io (default), read_bytes, write_bytes, packets,\n"
        "                       waits, fails"
    );
command_help("otp-state") ->
    remote_help(
        "otp-state PID_OR_NAME --behavior BEHAVIOR [--limit N] [--redact]",
        "Inspect bounded, value-free shapes after a full OTP behavior state copy\n"
        "on the target. The full-copy acquisition can wait up to 5s.",
        "  --behavior BEHAVIOR  Required operator assertion: gen_server,\n"
        "                       gen_statem, or gen_event\n"
        "  --limit N            gen_event handlers only; 1..200, 20 by default\n"
        "                       This is an output soft cap, not an acquisition cap\n"
        "  --timeout DURATION   Overall command deadline; minimum 10s\n"
        "  --redact             Hide identifiers found in returned shapes\n",
        "  observer_cli otp-state my_server --behavior gen_server --redact\n"
        "  observer_cli otp-state alarm_handler --behavior gen_event --limit 50\n"
    );
command_help("supervision-tree") ->
    remote_help(
        "supervision-tree --app APP",
        "Show the bounded supervision tree rooted in one running application.",
        "  --app APP  Application name; required\n",
        "  observer_cli supervision-tree --app my_app\n"
    );
command_help("logs") ->
    logs_help();
command_help("trace") ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli trace call MFA --pid PID --replace-existing-trace\n"
        "    [OPTIONS] [TARGET OPTIONS] [OUTPUT OPTIONS]\n"
        "  observer_cli trace stop --all\n"
        "    [TARGET OPTIONS] [OUTPUT OPTIONS]\n"
        "\n"
        "Run or stop bounded node-global call tracing. Calls cover external/global\n"
        "calls only for one live target-local PID and one loaded, exported MFA;\n"
        "local intra-module calls are excluded. Events contain identity and\n"
        "relative timing, never arguments, returns, exceptions, or stacks.\n"
        "Trace setup and cleanup clear node-wide legacy process/port trace flags\n"
        "and tracers plus static call patterns without restoring prior state.\n"
        "Recon 2.5.6 may terminate processes or ports occupying its fixed\n"
        "tracer/formatter names. Dynamic trace sessions are not directly cleared,\n"
        "but killing such a fixed-name occupant can disable one.\n"
        "\n"
        "Run 'observer_cli trace call --help' or\n"
        "'observer_cli trace stop --help' for the exact safety contract.\n"
    );
command_help(_Command) ->
    root_error(unknown_command).

logs_help() ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli logs [--handler HANDLER_ID] [--tail LINES]\n"
        "    [TARGET OPTIONS] [OUTPUT OPTIONS]\n"
        "\n"
        "Read retained bytes once from one trusted logger_std_h file handler's\n"
        "configured path. It does not wait for new lines.\n"
        "This does not flush Logger buffers, prove a match with the handler's active\n"
        "file descriptor, or read rotation archives.\n"
        "Log text is sensitive and untrusted; --redact and --include-identifiers\n"
        "are rejected rather than implying that arbitrary text can be sanitized.\n"
        "\n"
        "Options:\n"
        "  --handler HANDLER_ID  Select one handler; required when several qualify\n"
        "  --tail LINES          1..2000 physical lines; 200 by default\n"
        "  --timeout DURATION    Remote-operation deadline; 10s default, max 120s\n"
        "  --format text|term|json\n"
        "  --json\n"
        "\n"
        "Only plain regular files on Linux and macOS targets are supported.\n"
        "The command reads at most 64 KiB and retains at most 32 KiB per line.\n"
        "\n"
        "Examples:\n"
        "  observer_cli logs\n"
        "  observer_cli logs --handler app_file --tail 500\n"
    ).

tui_help() ->
    io:put_chars(
        "Usage:\n"
        "  observer_cli tui NODE [COOKIE REFRESH_MS]\n"
        "\n"
        "Open the interactive terminal UI. REFRESH_MS defaults to 1500 and must\n"
        "be at least 1000. The TUI retains automatic remote bundle loading.\n"
        "\n"
        "Warning:\n"
        "  A positional COOKIE is visible in process arguments and shell history.\n"
        "  Prefer a protected Erlang cookie file or an already matching cookie.\n"
    ).

trace_call_help() ->
    remote_help(
        "trace call MFA --pid PID --replace-existing-trace [OPTIONS]",
        "Run a bounded call-only trace for one loaded, exported MFA and one live\n"
        "target-local PID. MFA syntax is module:function/arity. Only external/global\n"
        "calls are captured; local intra-module calls are excluded.\n"
        "Setup and teardown clear node-wide legacy process/port trace flags,\n"
        "tracers, and all static call patterns, including on-load and call-memory.\n"
        "Prior state is not restored. Recon 2.5.6 may terminate a process or port\n"
        "occupying a fixed tracer or formatter name.\n"
        "Dynamic trace sessions are not directly cleared, but killing such an\n"
        "occupant can disable one.\n"
        "The acknowledgement flag is therefore required. An explicit overall\n"
        "timeout must cover the duration plus seven seconds.\n"
        "Events contain only tracee, MFA, and session-relative offset; arguments,\n"
        "returns, exceptions, and stacks are never collected. Identifiers are\n"
        "included by default; use --redact before exporting output.\n"
        "data.trace.trace_complete=true means normal lifecycle completion with no\n"
        "detected loss, module change, or interference.\n"
        "At most 1000 events are returned; inspect status, truncated, and\n"
        "dropped_count. Duration expiry, manual stop, response-cap loss, a detected\n"
        "module change or rejected interference can still return\n"
        "outcome=complete and exit 0 with data.trace.status=partial.\n"
        "cleanup_unconfirmed is outcome=error/exit 4 and may omit trace data;\n"
        "target tracing may remain, so retry trace stop --all and verify it.",
        "  --pid PID                 Live target-local tracee PID; required\n"
        "  --duration DURATION       100ms..60s; 10s by default\n"
        "  --limit N                 1..1000 events; 100 by default\n"
        "  --rate N/s                Recon burst breaker, not a pacer; 1..200/s;\n"
        "                            trip event included; the first event after an\n"
        "                            expired window is forwarded and resets the\n"
        "                            counter to zero; total capture may exceed N\n"
        "                            across windows; conflicts with --limit\n"
        "  --replace-existing-trace  Acknowledge node-global replacement\n",
        "  observer_cli trace call my_mod:my_fun/2 --pid \"<0.123.0>\" \\\n"
        "    --duration 30s --limit 200 --replace-existing-trace\n"
    ).

trace_stop_help() ->
    remote_help(
        "trace stop --all",
        "Stop an active observer_cli trace or perform emergency cleanup. This\n"
        "clears node-wide legacy process/port trace flags, tracers, and all static\n"
        "call patterns, including on-load and call-memory, without restoring prior\n"
        "state. With recon 2.5.6, a process or port occupying a fixed tracer or\n"
        "formatter name may be terminated. Dynamic trace sessions are not directly\n"
        "cleared, but killing such an occupant can disable one. --all acknowledges\n"
        "that scope. Explicit timeout minimum: 5s. When trace data is present, the\n"
        "public stop response has events=[]; captured events are returned only by\n"
        "the original waiting trace call invocation. With no owned observer_cli\n"
        "trace, cleanup still runs but reports cleanup_unconfirmed. This is\n"
        "outcome=error, exit 4. Retry stop and verify target trace state whenever\n"
        "cleanup is unconfirmed.",
        "  --all  Acknowledge node-global trace cleanup; required\n",
        "  observer_cli trace stop --all\n"
    ).

remote_help(Usage, Description, Options, Examples) ->
    io:put_chars([
        "Usage:\n  observer_cli ",
        Usage,
        "\n    [TARGET OPTIONS] [OUTPUT OPTIONS]\n\n",
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
        "  --format text|term|json, --json\n",
        "  --redact hides identifiers for inspection and trace commands.\n",
        "  --include-identifiers reveals them for snapshot and diagnose.\n",
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
run_command(disconnect, Options) ->
    run_disconnect(Options);
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
command_request(trace, ["stop"], Options) ->
    #{action => stop_all, all => maps:get(all, Options, false)};
command_request(process, [Target], Options) ->
    (request_options(Options))#{target => Target};
command_request(port, [Target], Options) ->
    (request_options(Options))#{target => Target};
command_request(otp_state, [Target], Options) ->
    Behavior = list_to_existing_atom(maps:get(behavior, Options)),
    Request = #{target => unicode:characters_to_binary(Target), behavior => Behavior},
    case Behavior of
        gen_event -> Request#{limit => list_to_integer(maps:get(limit, Options, "20"))};
        _ -> Request
    end;
command_request(supervision_tree, [], Options) ->
    (request_options(Options))#{app => maps:get(app, Options)};
command_request(logs, [], Options) ->
    #{
        handler =>
            case maps:find(handler, Options) of
                {ok, Handler} -> unicode:characters_to_binary(Handler);
                error -> null
            end,
        tail => list_to_integer(maps:get(tail, Options, "200"))
    };
command_request(schedulers, _Arguments, Options) ->
    {ok, Duration} = observer_cli_cli:duration(Options),
    #{duration_ms => Duration};
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
    case ensure_output_format(connect, Options) of
        ok -> run_connect_ready(Options);
        Error -> Error
    end.

run_connect_ready(Options) ->
    case observer_cli_cli:context_options(Options) of
        {ok, ContextOptions} ->
            Outcome = probe_options(ContextOptions, fun(Target, CapabilityResult, Remaining) ->
                probe_response(connect, ContextOptions, Target, CapabilityResult, Remaining)
            end),
            save_connected_context(ContextOptions, Outcome);
        {error, Reason} ->
            {error, argument, Reason}
    end.

save_connected_context(ContextOptions, {ok, _Response, _ExitCode} = Outcome) ->
    case observer_cli_cli:save_context(ContextOptions) of
        ok -> Outcome;
        {error, Reason} -> {error, internal, Reason}
    end;
save_connected_context(_ContextOptions, Error) ->
    Error.

run_status(Options) ->
    case observer_cli_cli:load_context() of
        {ok, ContextOptions} ->
            ProbeOptions = maps:merge(ContextOptions, maps:with([timeout], Options)),
            probe_options(ProbeOptions, fun(Target, CapabilityResult, Remaining) ->
                probe_response(status, ContextOptions, Target, CapabilityResult, Remaining)
            end);
        {error, no_active_context} ->
            {error, capability, no_active_context};
        {error, Reason} ->
            {error, internal, Reason}
    end.

-ifdef(TEST).
run_disconnect() ->
    run_disconnect(#{}).
-endif.

run_disconnect(Options) ->
    case ensure_output_format(disconnect, Options) of
        ok -> run_disconnect_ready();
        Error -> Error
    end.

run_disconnect_ready() ->
    case observer_cli_cli:load_context() of
        {ok, #{node := Node}} ->
            case observer_cli_cli:delete_context() of
                ok -> disconnect_response(public_text(Node));
                {error, Reason} -> {error, internal, Reason}
            end;
        {error, no_active_context} ->
            disconnect_response(null);
        {error, Reason} when Reason =:= invalid_context; Reason =:= context_too_large ->
            case observer_cli_cli:delete_context() of
                ok -> recovered_disconnect_response();
                {error, DeleteReason} -> {error, internal, DeleteReason}
            end;
        {error, Reason} ->
            {error, internal, Reason}
    end.

ensure_output_format(Command, Options) ->
    Response = observer_cli_cli:response(Command, complete, null, null, #{}, []),
    case observer_cli_cli:encode(command_format(Options), Response) of
        {ok, _Output} ->
            ok;
        {error, EncodeError} ->
            {error, maps:get(category, EncodeError), maps:get(reason, EncodeError)}
    end.

probe_response(Command, ContextOptions, Target, CapabilityResult, Remaining) when
    element(1, CapabilityResult) =:= ok;
    element(1, CapabilityResult) =:= error,
    element(2, CapabilityResult) =:= capability
->
    case target_otp_release(Target, Remaining) of
        {ok, OtpRelease} ->
            probe_response(Command, ContextOptions, CapabilityResult, OtpRelease);
        Error ->
            Error
    end;
probe_response(_Command, _ContextOptions, _Target, {error, _Category, _Reason} = Error, _Remaining) ->
    Error.

probe_response(Command, ContextOptions, CapabilityResult, OtpRelease) ->
    #{node := Node, name_mode := NameMode} = ContextOptions,
    {DiagnosticsModule, Observed, Warnings} = capability_status(CapabilityResult),
    Expected = public_capabilities(observer_cli_snapshot:capabilities()),
    NodeBinary = public_text(Node),
    Response = observer_cli_cli:response(
        Command,
        complete,
        #{<<"node">> => NodeBinary, <<"otp_release">> => OtpRelease},
        null,
        #{
            <<"node">> => NodeBinary,
            <<"name_mode">> => public_text(NameMode),
            <<"cookie_source">> => public_cookie_source(ContextOptions),
            <<"probe">> => <<"succeeded">>,
            <<"diagnostics_module">> => DiagnosticsModule,
            <<"expected_capabilities">> => Expected,
            <<"observed_capabilities">> => Observed,
            <<"persistent_connection">> => false
        },
        Warnings
    ),
    {ok, Response, response_exit_code(Response)}.

capability_status({ok, Capabilities}) ->
    {<<"compatible">>, public_capabilities(Capabilities), []};
capability_status({error, capability, diagnostics_missing}) ->
    {<<"missing">>, null, [warning(capability, diagnostics_missing)]};
capability_status({error, capability, {diagnostics_incompatible, Observed}}) ->
    {
        <<"incompatible">>,
        public_capabilities(Observed),
        [warning(capability, diagnostics_incompatible)]
    }.

warning(Category, Reason) ->
    (observer_cli_cli:error(Category, Reason))#{<<"severity">> => <<"warning">>}.

public_capabilities(Capabilities) when is_map(Capabilities) ->
    #{
        <<"protocol_version">> => maps:get(protocol_version, Capabilities, null),
        <<"bundle_version">> => maps:get(bundle_version, Capabilities, null)
    };
public_capabilities(_Capabilities) ->
    null.

public_cookie_source(#{cookie_env := Name}) ->
    #{<<"type">> => <<"env">>, <<"name">> => public_text(Name)};
public_cookie_source(#{cookie_file := Path}) ->
    #{<<"type">> => <<"file">>, <<"path">> => public_text(Path)}.

public_text(Text) ->
    try list_to_binary(Text) of
        Raw -> public_binary_text(Raw)
    catch
        error:badarg ->
            case unicode:characters_to_binary(Text) of
                Unicode when is_binary(Unicode) -> Unicode;
                _Invalid -> <<"invalid-text">>
            end
    end.

public_binary_text(Binary) ->
    case unicode:characters_to_binary(Binary) of
        Binary -> Binary;
        _Invalid -> <<"base64:", (base64:encode(Binary))/binary>>
    end.

disconnect_response(Node) ->
    Response = observer_cli_cli:response(
        disconnect,
        complete,
        null,
        null,
        #{<<"node">> => Node, <<"disconnected">> => true},
        []
    ),
    {ok, Response, response_exit_code(Response)}.

recovered_disconnect_response() ->
    Response = observer_cli_cli:response(
        disconnect,
        complete,
        null,
        null,
        #{
            <<"node">> => null,
            <<"disconnected">> => true,
            <<"recovered_invalid_context">> => true
        },
        []
    ),
    {ok, Response, response_exit_code(Response)}.

run_snapshot(Target, Options, Request, Remaining) ->
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    case target_dispatch(Target, snapshot, Request, Options, Policy, Remaining) of
        {ok, Response} ->
            validated_response(snapshot, Policy, Target, Response, fun snapshot_response/1);
        invalid ->
            {error, schema, invalid_snapshot_response};
        Error ->
            Error
    end.

snapshot_response(Response) ->
    dispatch_response(Response).

run_dispatch(Target, Command, Request, Options, Remaining) ->
    Policy =
        case maps:is_key(redact, Options) of
            true -> redact;
            false -> include
        end,
    case target_dispatch(Target, Command, Request, Options, Policy, Remaining) of
        {ok, Response} ->
            validate_dispatched_response(Command, Request, Policy, Target, Response);
        invalid ->
            {error, schema, invalid_command_response};
        Error ->
            Error
    end.

validate_dispatched_response(logs, Request, Policy, Target, Response) ->
    case validate_response(logs, Policy, Target, Response) of
        ok ->
            case validate_logs_response(Request, Response, atom_to_binary(Target)) of
                true -> dispatch_response(Response);
                false -> {error, schema, invalid_command_response}
            end;
        {error, Reason} ->
            {error, schema, Reason}
    end;
validate_dispatched_response(Command, Request, Policy, Target, Response) ->
    validated_response(
        response_command(Command, Request),
        Policy,
        Target,
        Response,
        fun dispatch_response/1
    ).

dispatch_response(#{<<"outcome">> := Outcome} = Response) when
    Outcome =:= <<"complete">>; Outcome =:= <<"partial">>; Outcome =:= <<"error">>
->
    {ok, Response, response_exit_code(Response)};
dispatch_response(_Invalid) ->
    {error, schema, invalid_command_response}.

response_exit_code(#{
    <<"outcome">> := <<"complete">>, <<"command">> := <<"diagnose">>, <<"data">> := Data
}) ->
    case maps:get(<<"findings">>, Data, []) of
        [] -> 0;
        [_ | _] -> 1
    end;
response_exit_code(#{<<"outcome">> := <<"complete">>}) ->
    0;
response_exit_code(#{<<"outcome">> := <<"partial">>}) ->
    3;
response_exit_code(#{<<"outcome">> := <<"error">>, <<"issues">> := Issues} = Response) ->
    ProbeCodes =
        case Response of
            #{<<"meta">> := #{<<"capture">> := #{<<"probes">> := Probes}}} ->
                [probe_exit_code(Probe) || Probe <- Probes, probe_failed(Probe)];
            _ ->
                []
        end,
    ErrorCodes = [
        issue_exit_code(Issue)
     || #{<<"severity">> := <<"error">>} = Issue <- Issues
    ],
    case ErrorCodes ++ ProbeCodes of
        [] -> 4;
        Codes -> lists:max(Codes)
    end.

issue_exit_code(#{<<"class">> := Class}) ->
    case Class of
        <<"argument">> -> 2;
        <<"format">> -> 2;
        <<"capability">> -> 2;
        <<"safety_refusal">> -> 3;
        <<"controller">> -> 3;
        <<"distribution">> -> 3;
        <<"connection">> -> 3;
        <<"required_probe">> -> 3;
        <<"partial">> -> 3;
        _ -> 4
    end;
issue_exit_code(_Issue) ->
    4.

probe_failed(#{<<"status">> := Status}) ->
    Status =/= <<"ok">>.

probe_exit_code(#{<<"reason_code">> := Reason}) ->
    case Reason of
        <<"capability_unavailable">> -> 2;
        <<"mfa_unavailable">> -> 2;
        <<"mfa_not_traceable">> -> 2;
        <<"log_source_unavailable">> -> 2;
        <<"log_handler_required">> -> 2;
        <<"log_handler_not_found">> -> 2;
        <<"unsupported_log_handler">> -> 2;
        <<"unsupported_file_modes">> -> 2;
        <<"unsupported_log_file_type">> -> 2;
        <<"log_path_unrepresentable">> -> 2;
        <<"invalid_log_handler_config">> -> 2;
        <<"unsupported_target_platform">> -> 2;
        <<"cleanup_unconfirmed">> -> 4;
        <<"invalid_schema">> -> 4;
        <<"internal_error">> -> 4;
        <<"capture_internal_error">> -> 4;
        <<"dispatcher_disconnected">> -> 4;
        <<"helper_setup_failed">> -> 4;
        _ -> 3
    end.

run_diagnose(Target, Options, Remaining) ->
    Policy =
        case maps:is_key(include_identifiers, Options) of
            true -> include;
            false -> redact
        end,
    Request = maps:with([observe, deep, app], Options),
    case target_dispatch(Target, diagnose, Request, Options, Policy, Remaining) of
        {ok, Response} ->
            validated_response(diagnose, Policy, Target, Response, fun diagnose_response/1);
        invalid ->
            {error, schema, invalid_diagnose_response};
        Error ->
            Error
    end.

target_dispatch(Target, Command, Request, Options, Policy, Remaining) ->
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
                    {ok, Response};
                #{
                    <<"status">> := <<"error">>,
                    <<"reason_code">> := Reason,
                    <<"cleanup_confirmed">> := true
                } ->
                    target_dispatch_error(Reason);
                #{<<"cleanup_confirmed">> := false} ->
                    {error, cleanup, cleanup_unconfirmed};
                _Invalid ->
                    invalid
            catch
                _Class:_Reason:_Stacktrace -> {error, required_probe, target_dispatch_failed}
            end;
        {error, Reason} ->
            {error, required_probe, Reason}
    end.

diagnose_response(Response) ->
    dispatch_response(Response).

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
                    {ok, Capabilities} ->
                        Fun(Target, Capabilities, Remaining);
                    {error, capability, diagnostics_missing} ->
                        {error, capability, capability_unavailable};
                    {error, capability, {diagnostics_incompatible, _Observed}} ->
                        {error, capability, capability_unavailable};
                    Error ->
                        Error
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
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    try erpc:call(Target, code, which, [observer_cli_snapshot], Timeout) of
        non_existing ->
            {error, capability, diagnostics_missing};
        _Loaded ->
            fetch_capabilities(Target, remaining(Deadline))
    catch
        Class:Reason -> capability_probe_error(Class, Reason)
    end.

fetch_capabilities(_Target, Timeout) when Timeout =< 0 ->
    {error, required_probe, target_timeout};
fetch_capabilities(Target, Timeout) ->
    try erpc:call(Target, observer_cli_snapshot, capabilities, [], Timeout) of
        Capabilities ->
            Observed = observed_capabilities(Capabilities),
            case compatible_capabilities(Observed) of
                true -> {ok, Observed};
                false -> {error, capability, {diagnostics_incompatible, Observed}}
            end
    catch
        error:undef ->
            {error, capability, {diagnostics_incompatible, null}};
        error:{exception, undef, _Stacktrace} ->
            {error, capability, {diagnostics_incompatible, null}};
        Class:Reason ->
            capability_probe_error(Class, Reason)
    end.

observed_capabilities(Capabilities) when is_map(Capabilities) ->
    Protocol = maps:get(protocol_version, Capabilities, null),
    Bundle = maps:get(bundle_version, Capabilities, null),
    #{
        protocol_version => public_protocol_version(Protocol),
        bundle_version => public_bundle_version(Bundle)
    };
observed_capabilities(_Capabilities) ->
    #{}.

public_protocol_version(Protocol) when
    is_integer(Protocol), Protocol > 0, Protocol =< 16#7FFFFFFF
->
    Protocol;
public_protocol_version(_Protocol) ->
    null.

public_bundle_version(Bundle) when
    is_binary(Bundle), byte_size(Bundle) > 0, byte_size(Bundle) =< 64
->
    case lists:all(fun(Byte) -> Byte >= 32 andalso Byte =< 126 end, binary:bin_to_list(Bundle)) of
        true -> Bundle;
        false -> null
    end;
public_bundle_version(_Bundle) ->
    null.

compatible_capabilities(Capabilities) when is_map(Capabilities) ->
    Expected = observer_cli_snapshot:capabilities(),
    maps:with(maps:keys(Expected), Capabilities) =:= Expected;
compatible_capabilities(_Capabilities) ->
    false.

-ifdef(TEST).
capability_error(error, undef) ->
    {error, capability, {diagnostics_incompatible, null}};
capability_error(error, {exception, undef, _Stacktrace}) ->
    {error, capability, {diagnostics_incompatible, null}};
capability_error(Class, Reason) ->
    capability_probe_error(Class, Reason).
-endif.

capability_probe_error(error, {erpc, timeout}) ->
    {error, required_probe, target_timeout};
capability_probe_error(error, {erpc, noconnection}) ->
    {error, connection, connection_failed};
capability_probe_error(_Class, _Reason) ->
    {error, required_probe, capability_probe_failed}.

target_otp_release(_Target, Timeout) when Timeout =< 0 ->
    {error, required_probe, target_timeout};
target_otp_release(Target, Timeout) ->
    try erpc:call(Target, erlang, system_info, [otp_release], Timeout) of
        OtpRelease when is_list(OtpRelease) ->
            target_otp_release_value(unicode:characters_to_binary(OtpRelease));
        OtpRelease when is_binary(OtpRelease) ->
            target_otp_release_value(OtpRelease);
        _Invalid ->
            {error, required_probe, capability_probe_failed}
    catch
        Class:Reason -> capability_probe_error(Class, Reason)
    end.

target_otp_release_value(OtpRelease) ->
    case valid_otp_release(OtpRelease) of
        true -> {ok, OtpRelease};
        false -> {error, required_probe, capability_probe_failed}
    end.

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
        ok -> Fun(Response);
        {error, Reason} -> {error, schema, Reason}
    end.

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
        <<"command">>,
        <<"data">>,
        <<"issues">>,
        <<"meta">>,
        <<"outcome">>,
        <<"schema">>
    ],
    Checks = [
        lists:sort(maps:keys(Response)) =:= ExpectedKeys,
        maps:get(<<"schema">>, Response, undefined) =:= <<"observer_cli.cli/v1">>,
        maps:get(<<"command">>, Response, undefined) =:= atom_to_binary(Command),
        public_value(Response, 0),
        erlang:external_size(Response) =< ?MAX_RESPONSE_BYTES,
        valid_envelope(Response, Policy, atom_to_binary(Target)),
        valid_command_data(Command, Response),
        valid_policy_response(Command, Policy, Response)
    ],
    case lists:all(fun(Check) -> Check end, Checks) of
        true -> ok;
        false -> {error, invalid_command_response}
    end.

validate_logs_response(
    Request,
    #{
        <<"command">> := <<"logs">>,
        <<"outcome">> := Outcome,
        <<"data">> := Data,
        <<"meta">> := #{<<"target">> := Target, <<"capture">> := Capture},
        <<"issues">> := []
    },
    ExpectedTarget
) ->
    valid_log_request(Request) andalso is_map(Data) andalso
        valid_log_target(Target, ExpectedTarget) andalso
        valid_log_data(Request, Data) andalso
        valid_log_capture(Request, Outcome, Data, Capture);
validate_logs_response(_Request, _Response, _ExpectedTarget) ->
    false.

valid_log_request(#{handler := Handler, tail := Tail} = Request) ->
    map_size(Request) =:= 2 andalso
        (Handler =:= null orelse
            (is_binary(Handler) andalso observer_cli_log:addressable_handler_id(Handler))) andalso
        is_integer(Tail) andalso Tail >= 1 andalso Tail =< 2000;
valid_log_request(_Request) ->
    false.

valid_log_target(
    #{<<"node">> := ExpectedTarget, <<"otp_release">> := OtpRelease} = Target,
    ExpectedTarget
) when is_binary(OtpRelease) ->
    map_size(Target) =:= 2 andalso byte_size(OtpRelease) =< 16;
valid_log_target(_Target, _ExpectedTarget) ->
    false.

valid_log_data(
    #{handler := Handler, tail := Requested},
    #{<<"sources">> := Sources, <<"selected_source">> := Selected, <<"tail">> := Tail} = Data
) ->
    map_size(Data) =:= 3 andalso valid_log_sources(Sources) andalso
        valid_log_selected(Selected, Sources) andalso
        valid_log_request_binding(Handler, Sources, Selected) andalso
        (Tail =:= null orelse valid_log_tail(Tail, Requested));
valid_log_data(_Request, _Data) ->
    false.

valid_log_sources(Sources) when is_list(Sources), length(Sources) =< 64 ->
    Ids = [maps:get(<<"id">>, Source, undefined) || Source <- Sources],
    lists:all(fun valid_log_source/1, Sources) andalso
        length(Ids) =:= length(lists:usort(Ids)) andalso
        lists:sum([byte_size(Id) || Id <- Ids, is_binary(Id)]) =< 64 * 1024;
valid_log_sources(_Sources) ->
    false.

valid_log_source(
    #{
        <<"id">> := Id,
        <<"addressable">> := Addressable,
        <<"handler_kind">> := Kind,
        <<"supported">> := Supported,
        <<"reason_code">> := Reason
    } = Source
) ->
    map_size(Source) =:= 5 andalso valid_log_id(Id) andalso is_boolean(Addressable) andalso
        Addressable =:= observer_cli_log:addressable_handler_id(Id) andalso
        lists:member(Kind, [<<"logger_std_h_file">>, <<"other">>]) andalso
        is_boolean(Supported) andalso
        valid_log_source_relation(
            Addressable, Kind, Supported, Reason
        );
valid_log_source(_Source) ->
    false.

valid_log_id(Id) when is_binary(Id), byte_size(Id) >= 1, byte_size(Id) =< 1024 ->
    case unicode:characters_to_list(Id) of
        Codepoints when is_list(Codepoints) -> length(Codepoints) =< 255;
        _ -> false
    end;
valid_log_id(_Id) ->
    false.

valid_log_source_relation(true, <<"logger_std_h_file">>, true, null) ->
    true;
valid_log_source_relation(
    false, <<"logger_std_h_file">>, true, <<"unaddressable_handler_id">>
) ->
    true;
valid_log_source_relation(_Addressable, <<"other">>, false, <<"unsupported_log_handler">>) ->
    true;
valid_log_source_relation(
    _Addressable, <<"logger_std_h_file">>, false, <<"unsupported_file_modes">>
) ->
    true;
valid_log_source_relation(
    _Addressable, <<"logger_std_h_file">>, false, <<"log_path_unrepresentable">>
) ->
    true;
valid_log_source_relation(
    _Addressable, Kind, false, <<"invalid_log_handler_config">>
) ->
    Kind =:= <<"logger_std_h_file">> orelse Kind =:= <<"other">>;
valid_log_source_relation(_Addressable, _Kind, _Supported, _Reason) ->
    false.

log_config_reasons() ->
    [
        <<"unsupported_log_handler">>,
        <<"unsupported_file_modes">>,
        <<"log_path_unrepresentable">>,
        <<"invalid_log_handler_config">>
    ].

valid_log_selected(null, _Sources) ->
    true;
valid_log_selected(
    #{
        <<"configured_path">> := Path,
        <<"active_handler_fd_match">> := <<"unknown">>
    } = Selected,
    Sources
) ->
    map_size(Selected) =:= 7 andalso valid_log_path(Path) andalso
        maps:get(<<"supported">>, Selected, false) andalso
        maps:get(<<"handler_kind">>, Selected, undefined) =:= <<"logger_std_h_file">> andalso
        lists:member(
            maps:without([<<"configured_path">>, <<"active_handler_fd_match">>], Selected),
            Sources
        );
valid_log_selected(_Selected, _Sources) ->
    false.

valid_log_path(<<"/", _/binary>> = Path) when byte_size(Path) =< 4096 ->
    unicode:characters_to_binary(Path) =:= Path;
valid_log_path(_Path) ->
    false.

valid_log_request_binding(null, _Sources, _Selected) ->
    true;
valid_log_request_binding(Handler, Sources, Selected) ->
    lists:all(fun(Source) -> maps:get(<<"id">>, Source) =:= Handler end, Sources) andalso
        case Selected of
            null -> true;
            _ -> maps:get(<<"id">>, Selected) =:= Handler
        end.

valid_log_tail(
    #{
        <<"scope">> := <<"configured_path">>,
        <<"active_handler_fd_match">> := <<"unknown">>,
        <<"visibility">> := <<"reader_visible">>,
        <<"command_filesync_requested">> := false,
        <<"consistency">> := <<"non_atomic">>,
        <<"content_trust">> := <<"untrusted">>,
        <<"requested_lines">> := Requested,
        <<"returned_lines">> := Returned,
        <<"captured_eof_bytes">> := CapturedEof,
        <<"bytes_read">> := BytesRead,
        <<"has_more">> := HasMore,
        <<"content_truncated">> := ContentTruncated,
        <<"truncation_reasons">> := Reasons,
        <<"truncated_line_indexes">> := Indexes,
        <<"lines">> := Lines
    } = Tail,
    Requested
) ->
    map_size(Tail) =:= 15 andalso is_integer(Requested) andalso Requested >= 1 andalso
        Requested =< 2000 andalso is_integer(Returned) andalso Returned >= 0 andalso
        Returned =< Requested andalso is_integer(CapturedEof) andalso CapturedEof >= 0 andalso
        CapturedEof =< 16#7FFFFFFFFFFFFFFF andalso is_integer(BytesRead) andalso
        BytesRead =:= min(CapturedEof, 64 * 1024) andalso is_boolean(HasMore) andalso
        is_boolean(ContentTruncated) andalso
        valid_log_lines(
            Lines,
            Returned,
            BytesRead,
            CapturedEof,
            Requested,
            HasMore,
            ContentTruncated,
            Reasons,
            Indexes
        );
valid_log_tail(_Tail, _Requested) ->
    false.

valid_log_lines(
    Lines,
    Returned,
    BytesRead,
    CapturedEof,
    Requested,
    HasMore,
    ContentTruncated,
    Reasons,
    Indexes
) when is_list(Lines), length(Lines) =:= Returned ->
    case decoded_log_lines(Lines, []) of
        {ok, Decoded} ->
            Sizes = [byte_size(Line) || Line <- Decoded],
            Total = lists:sum(Sizes),
            valid_log_reason_order(Reasons) andalso valid_log_indexes(Indexes, Returned) andalso
                lists:all(fun(Size) -> Size =< 32 * 1024 end, Sizes) andalso
                Total =< BytesRead andalso
                max(Returned, Total + max(0, Returned - 1)) =< BytesRead andalso
                ((Returned =:= 0) =:= (CapturedEof =:= 0)) andalso
                valid_log_truncation(
                    ContentTruncated, Reasons, Indexes, Sizes, CapturedEof, BytesRead, HasMore
                ) andalso
                (CapturedEof =< BytesRead orelse HasMore) andalso
                valid_log_empty(CapturedEof, Returned, HasMore, ContentTruncated) andalso
                valid_log_exhausted_tail(
                    CapturedEof,
                    BytesRead,
                    Returned,
                    Requested,
                    Reasons,
                    HasMore
                );
        error ->
            false
    end;
valid_log_lines(
    _Lines,
    _Returned,
    _BytesRead,
    _CapturedEof,
    _Requested,
    _HasMore,
    _ContentTruncated,
    _Reasons,
    _Indexes
) ->
    false.

decoded_log_lines([], Acc) ->
    {ok, lists:reverse(Acc)};
decoded_log_lines([Line | Rest], Acc) when is_binary(Line) ->
    case unicode:characters_to_binary(Line) of
        Line -> decoded_log_lines(Rest, [Line | Acc]);
        _ -> error
    end;
decoded_log_lines(
    [
        #{<<"encoding">> := <<"base64">>, <<"data">> := Encoded} = Line | Rest
    ],
    Acc
) when map_size(Line) =:= 2, is_binary(Encoded) ->
    try base64:decode(Encoded) of
        Decoded ->
            case
                base64:encode(Decoded) =:= Encoded andalso
                    unicode:characters_to_binary(Decoded) =/= Decoded
            of
                true -> decoded_log_lines(Rest, [Decoded | Acc]);
                false -> error
            end
    catch
        _:_ -> error
    end;
decoded_log_lines(_Lines, _Acc) ->
    error.

valid_log_reason_order([]) -> true;
valid_log_reason_order([<<"byte_cap">>]) -> true;
valid_log_reason_order([<<"line_cap">>]) -> true;
valid_log_reason_order([<<"byte_cap">>, <<"line_cap">>]) -> true;
valid_log_reason_order(_Reasons) -> false.

valid_log_indexes(Indexes, Returned) when is_list(Indexes) ->
    valid_log_indexes(Indexes, Returned, -1);
valid_log_indexes(_Indexes, _Returned) ->
    false.

valid_log_indexes([], _Returned, _Previous) ->
    true;
valid_log_indexes([Index | Rest], Returned, Previous) when
    is_integer(Index), Index > Previous, Index >= 0, Index < Returned
->
    valid_log_indexes(Rest, Returned, Index);
valid_log_indexes(_Indexes, _Returned, _Previous) ->
    false.

valid_log_truncation(false, [], [], _Sizes, _CapturedEof, _BytesRead, _HasMore) ->
    true;
valid_log_truncation(true, [<<"byte_cap">>], [0], _Sizes, CapturedEof, 65536, true) ->
    CapturedEof > 65536;
valid_log_truncation(true, [<<"line_cap">>], Indexes, Sizes, _CapturedEof, BytesRead, true) ->
    Indexes =/= [] andalso BytesRead > 32768 andalso
        lists:all(fun(Index) -> line_cap_size(lists:nth(Index + 1, Sizes)) end, Indexes);
valid_log_truncation(
    true,
    [<<"byte_cap">>, <<"line_cap">>],
    Indexes,
    Sizes,
    CapturedEof,
    65536,
    true
) ->
    CapturedEof > 65536 andalso lists:member(0, Indexes) andalso
        lists:all(
            fun
                (0) -> true;
                (Index) -> line_cap_size(lists:nth(Index + 1, Sizes))
            end,
            Indexes
        ) andalso
        lists:any(fun(Index) -> line_cap_size(lists:nth(Index + 1, Sizes)) end, Indexes);
valid_log_truncation(
    _ContentTruncated, _Reasons, _Indexes, _Sizes, _CapturedEof, _BytesRead, _HasMore
) ->
    false.

line_cap_size(Size) -> Size >= 32765 andalso Size =< 32768.

valid_log_empty(0, 0, false, false) ->
    true;
valid_log_empty(CapturedEof, Returned, _HasMore, _ContentTruncated) ->
    CapturedEof > 0 andalso Returned > 0.

valid_log_exhausted_tail(
    CapturedEof, BytesRead, Returned, Requested, [], HasMore
) when CapturedEof =:= BytesRead, Returned < Requested ->
    not HasMore;
valid_log_exhausted_tail(
    _CapturedEof, _BytesRead, _Returned, _Requested, _Reasons, _HasMore
) ->
    true.

valid_log_capture(
    Request,
    Outcome,
    #{<<"sources">> := Sources, <<"selected_source">> := Selected, <<"tail">> := Tail},
    #{
        <<"started_at">> := Started,
        <<"finished_at">> := Finished,
        <<"duration_ms">> := Duration,
        <<"probes">> := [Probe],
        <<"observer_effects">> := Effects
    } = Capture
) ->
    map_size(Capture) =:= 5 andalso is_binary(Started) andalso is_binary(Finished) andalso
        byte_size(Started) =< 64 andalso
        byte_size(Finished) =< 64 andalso is_integer(Duration) andalso Duration >= 0 andalso
        Duration =< 16#7FFFFFFFFFFFFFFF andalso valid_log_probe(Probe, Duration) andalso
        valid_log_effects(Effects) andalso
        valid_log_matrix(Request, Outcome, Sources, Selected, Tail, Probe, lists:last(Effects));
valid_log_capture(_Request, _Outcome, _Data, _Capture) ->
    false.

valid_log_probe(
    #{
        <<"id">> := <<"log_file_tail">>,
        <<"required">> := true,
        <<"status">> := Status,
        <<"reason_code">> := Reason,
        <<"duration_ms">> := Duration,
        <<"samples">> := 1,
        <<"coverage">> := Coverage
    } = Probe,
    Duration
) ->
    map_size(Probe) =:= 7 andalso
        lists:member(Status, [<<"ok">>, <<"unavailable">>, <<"error">>]) andalso
        valid_probe_reason(Status, Reason) andalso valid_log_coverage(Coverage);
valid_log_probe(_Probe, _Duration) ->
    false.

valid_log_coverage(Coverage) when is_list(Coverage) ->
    Stages = [
        <<"source_classification_complete">>,
        <<"source_selected">>,
        <<"path_prechecked">>,
        <<"fd_identity_verified">>,
        <<"bytes_captured">>,
        <<"post_read_verified">>
    ],
    lists:prefix(Coverage, Stages);
valid_log_coverage(_Coverage) ->
    false.

valid_log_effects([Diagnostics, ModuleLoad, LogRead]) ->
    valid_log_diagnostics_effect(Diagnostics) andalso valid_log_module_effect(ModuleLoad) andalso
        valid_configured_log_effect(LogRead);
valid_log_effects([Diagnostics, ModuleLoad, Distribution, LogRead]) ->
    valid_log_diagnostics_effect(Diagnostics) andalso valid_log_module_effect(ModuleLoad) andalso
        valid_log_distribution_effect(Distribution) andalso valid_configured_log_effect(LogRead);
valid_log_effects(_Effects) ->
    false.

valid_log_diagnostics_effect(
    #{
        <<"id">> := <<"diagnostics_worker">>,
        <<"affected_facts">> := [
            <<"process_count">>,
            <<"port_count">>,
            <<"memory">>,
            <<"io">>,
            <<"garbage_collection">>
        ]
    } = Effect
) ->
    map_size(Effect) =:= 2;
valid_log_diagnostics_effect(_Effect) ->
    false.

valid_log_module_effect(
    #{<<"id">> := <<"module_load">>, <<"module_loaded_before_sample">> := Loaded} = Effect
) ->
    map_size(Effect) =:= 2 andalso is_boolean(Loaded);
valid_log_module_effect(_Effect) ->
    false.

valid_log_distribution_effect(
    #{
        <<"id">> := <<"distribution_controller">>,
        <<"controller_peer">> := Peer,
        <<"dynamic_controller_name_atom">> := true
    } = Effect
) ->
    map_size(Effect) =:= 3 andalso is_binary(Peer) andalso byte_size(Peer) > 0;
valid_log_distribution_effect(_Effect) ->
    false.

valid_configured_log_effect(
    #{
        <<"id">> := <<"configured_log_read">>,
        <<"handler_ids_enumerated">> := Enumerated,
        <<"handler_config_lookups">> := Lookups,
        <<"read_attempts">> := Attempts,
        <<"raw_read_cap_bytes">> := 65536,
        <<"atime_may_change">> := true,
        <<"consistency">> := <<"non_atomic">>,
        <<"command_filesync_attempted">> := false
    } = Effect
) ->
    map_size(Effect) =:= 8 andalso is_boolean(Enumerated) andalso is_integer(Lookups) andalso
        Lookups >= 0 andalso Lookups =< 66 andalso is_integer(Attempts) andalso Attempts >= 0 andalso
        Attempts =< 2;
valid_configured_log_effect(_Effect) ->
    false.

valid_log_matrix(
    #{handler := Handler},
    Outcome,
    Sources,
    Selected,
    Tail,
    #{<<"status">> := Status, <<"reason_code">> := Reason, <<"coverage">> := Coverage},
    #{
        <<"handler_ids_enumerated">> := Enumerated,
        <<"handler_config_lookups">> := Lookups,
        <<"read_attempts">> := Attempts
    }
) ->
    Mode =
        case Handler of
            null -> auto;
            _ -> explicit
        end,
    valid_log_effect_relations(
        Mode, Reason, Sources, Selected, Coverage, Enumerated, Lookups, Attempts
    ) andalso
        valid_log_count_matrix(Mode, Outcome, Reason, Lookups, Attempts) andalso
        valid_log_outcome_matrix(Mode, Handler, Outcome, Status, Reason, Sources, Selected, Tail) andalso
        valid_log_outcome_coverage(Outcome, Reason, Selected, Coverage);
valid_log_matrix(
    _Request, _Outcome, _Sources, _Selected, _Tail, _Probe, _Effect
) ->
    false.

valid_log_effect_relations(
    Mode, Reason, Sources, Selected, Coverage, Enumerated, Lookups, Attempts
) ->
    Enumerated =:= (Mode =:= auto andalso Reason =/= <<"unsupported_target_platform">>) andalso
        Lookups >= length(Sources) andalso
        (Mode =:= auto orelse Lookups =< 3) andalso
        ((Selected =:= null) =:= (Attempts =:= 0)) andalso
        lists:member(<<"source_selected">>, Coverage) =:= (Selected =/= null) andalso
        (not lists:member(<<"path_prechecked">>, Coverage) orelse Attempts >= 1) andalso
        (Selected =:= null orelse Lookups >= 1) andalso
        valid_log_zero_work(Reason, Lookups, Attempts).

valid_log_zero_work(Reason, Lookups, Attempts) when
    Reason =:= <<"unsupported_target_platform">>; Reason =:= <<"scan_budget_exceeded">>
->
    Lookups =:= 0 andalso Attempts =:= 0;
valid_log_zero_work(_Reason, _Lookups, _Attempts) ->
    true.

valid_log_count_matrix(explicit, Outcome, _Reason, Lookups, Attempts) when
    Outcome =:= <<"complete">>; Outcome =:= <<"partial">>
->
    lists:member(Lookups, [2, 3]) andalso lists:member(Attempts, [1, 2]);
valid_log_count_matrix(explicit, <<"error">>, <<"log_handler_not_found">>, Lookups, 0) ->
    lists:member(Lookups, [0, 1]);
valid_log_count_matrix(explicit, <<"error">>, Reason, 1, 0) ->
    lists:member(Reason, log_config_reasons());
valid_log_count_matrix(_Mode, _Outcome, _Reason, _Lookups, _Attempts) ->
    true.

valid_log_outcome_matrix(
    Mode, Handler, <<"complete">>, <<"ok">>, null, Sources, Selected, Tail
) ->
    Selected =/= null andalso Tail =/= null andalso
        not maps:get(<<"content_truncated">>, Tail) andalso
        valid_log_selected_sources(Mode, Handler, Sources, Selected) andalso
        valid_log_success_counts(Mode, Handler, Sources, Selected, Tail);
valid_log_outcome_matrix(
    Mode, Handler, <<"partial">>, <<"error">>, Reason, Sources, Selected, Tail
) ->
    Selected =/= null andalso Tail =/= null andalso
        maps:get(<<"content_truncated">>, Tail) andalso
        Reason =:= log_tail_reason(Tail) andalso
        valid_log_selected_sources(Mode, Handler, Sources, Selected) andalso
        valid_log_success_counts(Mode, Handler, Sources, Selected, Tail);
valid_log_outcome_matrix(
    Mode, Handler, <<"error">>, <<"unavailable">>, Reason, Sources, Selected, null
) ->
    valid_log_error_matrix(Mode, Handler, Reason, Sources, Selected);
valid_log_outcome_matrix(
    _Mode, _Handler, _Outcome, _Status, _Reason, _Sources, _Selected, _Tail
) ->
    false.

valid_log_success_counts(auto, _Handler, Sources, Selected, _Tail) ->
    log_supported_count(Sources) =:= 1 andalso
        selected_projection(Selected) =:=
            hd([Source || #{<<"supported">> := true} = Source <- Sources]);
valid_log_success_counts(explicit, Handler, [Source], Selected, _Tail) ->
    maps:get(<<"id">>, Source) =:= Handler andalso
        maps:get(<<"supported">>, Source) andalso
        maps:get(<<"addressable">>, Source) andalso
        selected_projection(Selected) =:= Source;
valid_log_success_counts(_Mode, _Handler, _Sources, _Selected, _Tail) ->
    false.

valid_log_selected_sources(auto, _Handler, Sources, Selected) ->
    log_supported_count(Sources) =:= 1 andalso lists:member(selected_projection(Selected), Sources);
valid_log_selected_sources(explicit, Handler, [Source], Selected) ->
    maps:get(<<"id">>, Source) =:= Handler andalso
        maps:get(<<"supported">>, Source) andalso
        maps:get(<<"addressable">>, Source) andalso
        selected_projection(Selected) =:= Source;
valid_log_selected_sources(_Mode, _Handler, _Sources, _Selected) ->
    false.

selected_projection(Selected) ->
    maps:without([<<"configured_path">>, <<"active_handler_fd_match">>], Selected).

log_supported_count(Sources) ->
    length([ok || #{<<"supported">> := true} <- Sources]).

log_tail_reason(#{<<"truncation_reasons">> := [<<"byte_cap">> | _]}) ->
    <<"log_byte_cap_reached">>;
log_tail_reason(#{<<"truncation_reasons">> := [<<"line_cap">>]}) ->
    <<"log_line_cap_reached">>;
log_tail_reason(_Tail) ->
    invalid.

valid_log_error_matrix(auto, _Handler, <<"unsupported_target_platform">>, [], null) ->
    true;
valid_log_error_matrix(explicit, _Handler, <<"unsupported_target_platform">>, [], null) ->
    true;
valid_log_error_matrix(auto, _Handler, <<"scan_budget_exceeded">>, [], null) ->
    true;
valid_log_error_matrix(auto, _Handler, <<"log_handler_required">>, Sources, null) ->
    length(Sources) >= 2 andalso log_supported_count(Sources) >= 2;
valid_log_error_matrix(auto, _Handler, <<"log_source_unavailable">>, Sources, null) ->
    log_supported_count(Sources) =:= 0;
valid_log_error_matrix(explicit, _Handler, <<"log_handler_not_found">>, [], null) ->
    true;
valid_log_error_matrix(explicit, Handler, Reason, [Source], null) ->
    case lists:member(Reason, log_config_reasons()) of
        true ->
            maps:get(<<"id">>, Source) =:= Handler andalso
                not maps:get(<<"supported">>, Source) andalso
                maps:get(<<"reason_code">>, Source) =:= Reason;
        false ->
            false
    end;
valid_log_error_matrix(Mode, Handler, Reason, Sources, Selected) ->
    lists:member(Reason, log_selected_file_reasons()) andalso Selected =/= null andalso
        valid_log_selected_sources(Mode, Handler, Sources, Selected).

log_selected_file_reasons() ->
    [
        <<"unsupported_log_file_type">>,
        <<"log_file_identity_unavailable">>,
        <<"log_source_changed">>,
        <<"log_file_unavailable">>,
        <<"log_file_read_failed">>
    ].

valid_log_outcome_coverage(Outcome, _Reason, _Selected, Coverage) when
    Outcome =:= <<"complete">>; Outcome =:= <<"partial">>
->
    Coverage =:=
        [
            <<"source_classification_complete">>,
            <<"source_selected">>,
            <<"path_prechecked">>,
            <<"fd_identity_verified">>,
            <<"bytes_captured">>,
            <<"post_read_verified">>
        ];
valid_log_outcome_coverage(
    <<"error">>, Reason, null, Coverage
) when Reason =:= <<"unsupported_target_platform">>; Reason =:= <<"scan_budget_exceeded">> ->
    Coverage =:= [];
valid_log_outcome_coverage(<<"error">>, _Reason, null, Coverage) ->
    Coverage =:= [<<"source_classification_complete">>];
valid_log_outcome_coverage(<<"error">>, _Reason, _Selected, Coverage) ->
    length(Coverage) >= 2 andalso
        not lists:member(<<"post_read_verified">>, Coverage);
valid_log_outcome_coverage(_Outcome, _Reason, _Selected, _Coverage) ->
    false.

valid_policy_response(_Command, include, _Response) ->
    true;
valid_policy_response(otp_state, redact, #{<<"data">> := Data} = Response) ->
    valid_redaction(Response) andalso valid_otp_state_redaction(Data);
valid_policy_response(_Command, redact, Response) ->
    valid_redaction(Response).

valid_envelope(
    #{
        <<"command">> := Command,
        <<"outcome">> := <<"error">>,
        <<"meta">> := #{<<"target">> := Target, <<"capture">> := Capture} = Meta,
        <<"data">> := null,
        <<"issues">> := Issues
    },
    Policy,
    ExpectedTarget
) ->
    map_size(Meta) =:= 2 andalso
        (Target =:= null orelse valid_target(Target, Policy, ExpectedTarget)) andalso
        (Capture =:= null orelse valid_capture(Capture)) andalso
        valid_issues(Issues) andalso valid_outcome(Command, <<"error">>, Capture, Issues) andalso
        unique_probe_facts(null, Capture, Issues);
valid_envelope(
    #{
        <<"command">> := Command,
        <<"outcome">> := Outcome,
        <<"meta">> := #{<<"target">> := Target, <<"capture">> := Capture} = Meta,
        <<"data">> := Data,
        <<"issues">> := Issues
    },
    Policy,
    ExpectedTarget
) ->
    map_size(Meta) =:= 2 andalso valid_target(Target, Policy, ExpectedTarget) andalso
        is_map(Data) andalso valid_issues(Issues) andalso valid_capture(Capture) andalso
        valid_outcome(Command, Outcome, Capture, Issues) andalso
        unique_probe_facts(Data, Capture, Issues);
valid_envelope(_Response, _Policy, _ExpectedTarget) ->
    false.

valid_outcome(_Command, <<"complete">>, Capture, Issues) ->
    not has_error_issue(Issues) andalso capture_matches_outcome(complete, Capture);
valid_outcome(_Command, <<"partial">>, Capture, Issues) ->
    not has_error_issue(Issues) andalso capture_matches_outcome(partial, Capture);
valid_outcome(_Command, <<"error">>, null, Issues) ->
    has_error_issue(Issues);
valid_outcome(Command, <<"error">>, Capture, Issues) ->
    has_error_issue(Issues) orelse probe_only_error(Command, Capture);
valid_outcome(_Command, _Outcome, _Capture, _Issues) ->
    false.

has_error_issue(Issues) ->
    lists:any(
        fun
            (#{<<"severity">> := <<"error">>}) -> true;
            (_) -> false
        end,
        Issues
    ).

capture_matches_outcome(complete, #{<<"probes">> := Probes}) ->
    lists:all(
        fun
            (#{<<"required">> := true, <<"status">> := Status}) ->
                Status =:= <<"ok">>;
            (#{<<"required">> := false, <<"status">> := Status}) ->
                Status =:= <<"ok">> orelse Status =:= <<"unavailable">>
        end,
        Probes
    );
capture_matches_outcome(partial, #{<<"probes">> := Probes}) ->
    lists:any(
        fun
            (#{<<"required">> := true, <<"status">> := Status}) ->
                Status =/= <<"ok">>;
            (#{<<"required">> := false, <<"status">> := Status}) ->
                Status =:= <<"timeout">> orelse Status =:= <<"error">>
        end,
        Probes
    ).

probe_only_error(Command, #{<<"probes">> := Probes}) ->
    AllowedStatus =
        case Command of
            <<"trace_call">> -> [<<"unavailable">>, <<"timeout">>, <<"error">>];
            <<"trace_stop_all">> -> [<<"unavailable">>, <<"timeout">>, <<"error">>];
            _ -> [<<"unavailable">>]
        end,
    direct_probe_command(Command) andalso
        lists:any(
            fun
                (#{<<"required">> := true, <<"status">> := Status}) ->
                    lists:member(Status, AllowedStatus);
                (_) ->
                    false
            end,
            Probes
        ).

direct_probe_command(Command) ->
    lists:member(Command, [
        <<"schedulers">>,
        <<"distribution">>,
        <<"processes">>,
        <<"process">>,
        <<"applications">>,
        <<"ets">>,
        <<"mnesia">>,
        <<"network">>,
        <<"ports">>,
        <<"port">>,
        <<"sockets">>,
        <<"otp_state">>,
        <<"supervision_tree">>,
        <<"logs">>,
        <<"trace_call">>,
        <<"trace_stop_all">>
    ]).

unique_probe_facts(Data, #{<<"probes">> := Probes}, Issues) ->
    ProbeReasons = [
        Reason
     || #{<<"status">> := Status, <<"reason_code">> := Reason} <- Probes,
        Status =/= <<"ok">>
    ],
    IssueReasons = [maps:get(<<"reason_code">>, Issue) || Issue <- Issues],
    SkippedReasons =
        case Data of
            #{<<"skipped">> := Skipped} when is_list(Skipped) ->
                [
                    Reason
                 || #{<<"reason_code">> := Reason} <- Skipped
                ];
            _ ->
                []
        end,
    not lists:any(
        fun(Reason) -> lists:member(Reason, IssueReasons ++ SkippedReasons) end,
        ProbeReasons
    );
unique_probe_facts(_Data, null, _Issues) ->
    true.

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
        <<"started_at">> := Started,
        <<"finished_at">> := Finished,
        <<"duration_ms">> := Duration,
        <<"probes">> := Probes,
        <<"observer_effects">> := Effects
    } = Capture
) ->
    map_size(Capture) =:= 5 andalso
        valid_rfc3339(Started) andalso valid_rfc3339(Finished) andalso
        is_integer(Duration) andalso Duration >= 0 andalso
        is_list(Probes) andalso lists:all(fun valid_probe/1, Probes) andalso is_list(Effects) andalso
        lists:all(fun erlang:is_map/1, Effects);
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
    map_size(Probe) =:= 7 andalso is_binary(Id) andalso byte_size(Id) > 0 andalso
        is_boolean(Required) andalso
        lists:member(Status, [<<"ok">>, <<"unavailable">>, <<"timeout">>, <<"error">>]) andalso
        valid_probe_reason(Status, Reason) andalso
        is_integer(Duration) andalso Duration >= 0 andalso
        is_integer(Samples) andalso Samples >= 0 andalso is_list(Coverage) andalso
        lists:all(fun erlang:is_binary/1, Coverage);
valid_probe(_Probe) ->
    false.

valid_probe_reason(<<"ok">>, null) ->
    true;
valid_probe_reason(Status, Reason) when Status =/= <<"ok">> ->
    is_binary(Reason) andalso byte_size(Reason) > 0;
valid_probe_reason(_Status, _Reason) ->
    false.

valid_issues(Issues) when is_list(Issues) ->
    lists:all(
        fun
            (
                #{
                    <<"severity">> := Severity,
                    <<"class">> := Class,
                    <<"reason_code">> := Reason,
                    <<"message">> := Message
                } = Issue
            ) ->
                map_size(Issue) =:= 4 andalso
                    (Severity =:= <<"warning">> orelse Severity =:= <<"error">>) andalso
                    valid_issue_class(Class, Reason) andalso byte_size(Reason) > 0 andalso
                    (Message =:= null orelse is_binary(Message));
            (_) ->
                false
        end,
        Issues
    );
valid_issues(_Issues) ->
    false.

valid_issue_class(Class, Reason) when is_binary(Reason) ->
    lists:member(Class, [
        <<"argument">>,
        <<"format">>,
        <<"capability">>,
        <<"safety_refusal">>,
        <<"controller">>,
        <<"distribution">>,
        <<"connection">>,
        <<"cleanup">>,
        <<"schema">>,
        <<"internal">>,
        <<"required_probe">>,
        <<"partial">>
    ]);
valid_issue_class(_Class, _Reason) ->
    false.

valid_command_data(
    Command,
    #{<<"meta">> := #{<<"capture">> := null}} = Response
) when Command =:= trace_call; Command =:= trace_stop_all ->
    valid_null_trace_response(Command, Response);
valid_command_data(
    Command,
    #{
        <<"outcome">> := Outcome,
        <<"meta">> := #{<<"capture">> := #{<<"probes">> := Probes}}
    } = Response
) ->
    valid_required_probes(Command, Outcome, Probes) andalso
        valid_resource_wrappers(Response) andalso valid_command_payload(Command, Response, Probes);
valid_command_data(_Command, _Response) ->
    false.

valid_null_trace_response(
    Command,
    #{<<"outcome">> := <<"error">>, <<"data">> := null, <<"issues">> := Issues}
) when is_list(Issues) ->
    Errors = [
        {Class, Reason}
     || #{
            <<"severity">> := <<"error">>,
            <<"class">> := Class,
            <<"reason_code">> := Reason
        } <- Issues
    ],
    Warnings = [
        {Class, Reason}
     || #{
            <<"severity">> := <<"warning">>,
            <<"class">> := Class,
            <<"reason_code">> := Reason
        } <- Issues
    ],
    case Errors of
        [{Class, Reason}] ->
            valid_null_trace_error(Command, Class, Reason) andalso
                lists:member(Warnings, [
                    [], [{<<"safety_refusal">>, <<"global_trace_replacement">>}]
                ]);
        _ ->
            false
    end;
valid_null_trace_response(_Command, _Response) ->
    false.

valid_null_trace_error(trace_call, Class, Reason) ->
    lists:member({Class, Reason}, [
        {<<"argument">>, <<"invalid_mfa">>},
        {<<"capability">>, <<"capability_unavailable">>},
        {<<"capability">>, <<"mfa_unavailable">>},
        {<<"capability">>, <<"mfa_not_traceable">>},
        {<<"safety_refusal">>, <<"invalid_trace_pid">>},
        {<<"safety_refusal">>, <<"trace_busy">>},
        {<<"internal">>, <<"helper_setup_failed">>},
        {<<"cleanup">>, <<"cleanup_unconfirmed">>}
    ]);
valid_null_trace_error(trace_stop_all, Class, Reason) ->
    lists:member({Class, Reason}, [
        {<<"capability">>, <<"capability_unavailable">>},
        {<<"cleanup">>, <<"cleanup_unconfirmed">>}
    ]).

valid_command_payload(Command, #{<<"data">> := Data} = Response, Probes) ->
    case Command of
        diagnose ->
            valid_diagnose_data(Response);
        snapshot ->
            is_map(Data);
        memory ->
            valid_memory_data(Data);
        schedulers ->
            valid_status_data(Data);
        distribution ->
            valid_distribution_data(Data);
        process ->
            valid_status_data(Data);
        port ->
            valid_status_data(Data);
        otp_state ->
            valid_otp_state_payload(Response, Probes);
        supervision_tree ->
            valid_supervision_tree_data(Data);
        logs ->
            is_map(Data);
        trace_call ->
            valid_trace_response(trace_call, Response, Probes);
        trace_stop_all ->
            valid_trace_response(trace_stop_all, Response, Probes);
        _ ->
            valid_list_command_payload(Command, Data, Probes)
    end;
valid_command_payload(_Command, _Response, _Probes) ->
    false.

valid_status_data(#{<<"status">> := Status}) ->
    is_binary(Status) andalso byte_size(Status) > 0;
valid_status_data(_Data) ->
    false.

valid_distribution_data(#{
    <<"connected_peers">> := Peers,
    <<"returned_peer_count">> := Count,
    <<"truncated">> := Truncated
}) ->
    is_list(Peers) andalso nonnegative_integer(Count) andalso is_boolean(Truncated);
valid_distribution_data(_Data) ->
    false.

valid_supervision_tree_data(#{<<"status">> := Status, <<"risk_level">> := Risk}) ->
    is_binary(Status) andalso byte_size(Status) > 0 andalso
        is_binary(Risk) andalso byte_size(Risk) > 0;
valid_supervision_tree_data(_Data) ->
    false.

valid_trace_data(Command, #{<<"reason">> := Reason, <<"trace">> := Trace}) ->
    is_binary(Reason) andalso byte_size(Reason) > 0 andalso is_map(Trace) andalso
        maps:get(<<"reason">>, Trace, undefined) =:= Reason andalso
        valid_trace_capture(Command, Trace);
valid_trace_data(_Command, _Data) ->
    false.

valid_trace_response(
    Command,
    #{<<"outcome">> := Outcome, <<"data">> := Data} = Response,
    [Probe]
) ->
    valid_trace_contract(Response, Probe) andalso valid_trace_data(Command, Data) andalso
        valid_trace_outcome(Command, Outcome, Data, Probe);
valid_trace_response(_Command, _Response, _Probes) ->
    false.

valid_trace_contract(
    #{
        <<"issues">> := [
            #{
                <<"severity">> := <<"warning">>,
                <<"class">> := <<"safety_refusal">>,
                <<"reason_code">> := <<"global_trace_replacement">>,
                <<"message">> := Message
            } = Issue
        ],
        <<"meta">> := #{
            <<"capture">> := #{
                <<"duration_ms">> := Duration,
                <<"observer_effects">> := [
                    #{
                        <<"id">> := <<"global_trace_replacement">>,
                        <<"controller">> := Controller
                    } = Effect
                ]
            }
        }
    },
    #{
        <<"id">> := <<"trace">>,
        <<"duration_ms">> := Duration,
        <<"samples">> := 1,
        <<"coverage">> := [<<"recon_2_5_6">>, <<"external_global_calls_only">>]
    }
) ->
    map_size(Issue) =:= 4 andalso is_binary(Message) andalso byte_size(Message) > 0 andalso
        map_size(Effect) =:= 2 andalso is_binary(Controller) andalso byte_size(Controller) > 0;
valid_trace_contract(_Response, _Probe) ->
    false.

valid_trace_outcome(
    Command,
    <<"complete">>,
    #{<<"reason">> := Reason, <<"trace">> := Trace},
    #{<<"status">> := <<"ok">>, <<"reason_code">> := null}
) ->
    maps:get(<<"cleanup_confirmed">>, Trace) andalso
        valid_trace_reason(Command, complete, Reason) andalso
        valid_trace_reason_shape(Command, complete, Reason, Trace);
valid_trace_outcome(
    Command,
    <<"error">>,
    #{<<"reason">> := Reason, <<"trace">> := Trace},
    #{<<"status">> := <<"error">>, <<"reason_code">> := Reason}
) ->
    Cleanup = maps:get(<<"cleanup_confirmed">>, Trace),
    valid_trace_reason(Command, error, Reason) andalso
        valid_trace_reason_shape(Command, error, Reason, Trace) andalso
        (Reason =:= <<"cleanup_unconfirmed">>) =/= Cleanup;
valid_trace_outcome(_Command, _Outcome, _Data, _Probe) ->
    false.

valid_trace_reason(trace_call, complete, Reason) ->
    lists:member(Reason, [
        <<"limit_reached">>, <<"rate_exceeded">>, <<"duration_elapsed">>, <<"stopped">>
    ]);
valid_trace_reason(trace_stop_all, complete, <<"stopped">>) ->
    true;
valid_trace_reason(trace_call, error, Reason) ->
    lists:member(Reason, [
        <<"capture_internal_error">>,
        <<"dispatcher_disconnected">>,
        <<"controller_disconnected">>,
        <<"tracee_exited">>,
        <<"cleanup_unconfirmed">>
    ]);
valid_trace_reason(trace_stop_all, error, Reason) ->
    lists:member(Reason, [<<"capture_internal_error">>, <<"cleanup_unconfirmed">>]);
valid_trace_reason(_Command, _Outcome, _Reason) ->
    false.

valid_trace_reason_shape(trace_call, complete, Reason, Trace) when
    Reason =:= <<"limit_reached">>; Reason =:= <<"rate_exceeded">>
->
    valid_natural_trace_shape(Trace);
valid_trace_reason_shape(_Command, complete, _Reason, Trace) ->
    valid_forced_trace_shape(Trace);
valid_trace_reason_shape(_Command, error, _Reason, Trace) ->
    valid_forced_trace_shape(Trace).

valid_natural_trace_shape(#{
    <<"trace_complete">> := Complete,
    <<"dropped_count">> := Dropped,
    <<"module_reloaded">> := Reloaded,
    <<"interference_detected">> := Interference
}) ->
    case Complete of
        true ->
            not Reloaded andalso not Interference;
        false ->
            (Reloaded orelse Interference orelse
                (is_integer(Dropped) andalso Dropped > 0)) andalso
                (Dropped =/= null orelse Interference)
    end;
valid_natural_trace_shape(_Trace) ->
    false.

valid_forced_trace_shape(
    #{
        <<"status">> := <<"partial">>,
        <<"trace_complete">> := false,
        <<"truncated">> := true,
        <<"dropped_count">> := null
    } = Trace
) ->
    not is_map_key(<<"module_reloaded">>, Trace);
valid_forced_trace_shape(_Trace) ->
    false.

valid_trace_capture(
    Command,
    #{
        <<"status">> := Status,
        <<"reason">> := CaptureReason,
        <<"trace_complete">> := Complete,
        <<"truncated">> := Truncated,
        <<"dropped_count">> := Dropped,
        <<"events">> := Events,
        <<"coverage">> := <<"external_global_calls_only">>,
        <<"cleanup_confirmed">> := Cleanup,
        <<"interference_detected">> := Interference
    } = Trace
) ->
    valid_trace_keys(Trace) andalso is_binary(CaptureReason) andalso
        byte_size(CaptureReason) > 0 andalso valid_trace_status(Status, Complete) andalso
        is_boolean(Truncated) andalso valid_trace_loss(Complete, Truncated, Dropped) andalso
        is_list(Events) andalso length(Events) =< 1000 andalso
        lists:all(fun valid_trace_event/1, Events) andalso is_boolean(Cleanup) andalso
        valid_trace_cleanup(Cleanup, Complete) andalso valid_trace_reload(Trace) andalso
        is_boolean(Interference) andalso valid_trace_interference(Interference, Trace) andalso
        valid_trace_selector(Command, Trace) andalso valid_trace_events(Command, Trace, Events);
valid_trace_capture(_Command, _Trace) ->
    false.

valid_trace_keys(Trace) ->
    Allowed = [
        <<"status">>,
        <<"reason">>,
        <<"trace_complete">>,
        <<"truncated">>,
        <<"dropped_count">>,
        <<"events">>,
        <<"module_reloaded">>,
        <<"interference_detected">>,
        <<"coverage">>,
        <<"cleanup_confirmed">>,
        <<"tracee">>,
        <<"mfa">>
    ],
    lists:all(fun(Key) -> lists:member(Key, Allowed) end, maps:keys(Trace)).

valid_trace_status(<<"complete">>, true) -> true;
valid_trace_status(<<"partial">>, false) -> true;
valid_trace_status(_Status, _Complete) -> false.

valid_trace_loss(true, false, 0) -> true;
valid_trace_loss(false, false, 0) -> true;
valid_trace_loss(false, true, null) -> true;
valid_trace_loss(false, true, Dropped) -> nonnegative_integer(Dropped) andalso Dropped > 0;
valid_trace_loss(_Complete, _Truncated, _Dropped) -> false.

valid_trace_interference(false, _Trace) ->
    true;
valid_trace_interference(true, #{
    <<"trace_complete">> := false,
    <<"truncated">> := true,
    <<"dropped_count">> := null
}) ->
    true;
valid_trace_interference(true, _Trace) ->
    false.

valid_trace_cleanup(true, _Complete) -> true;
valid_trace_cleanup(false, false) -> true;
valid_trace_cleanup(false, true) -> false.

valid_trace_reload(#{<<"module_reloaded">> := true, <<"trace_complete">> := false}) -> true;
valid_trace_reload(#{<<"module_reloaded">> := false}) -> true;
valid_trace_reload(Trace) -> not is_map_key(<<"module_reloaded">>, Trace).

valid_trace_event(
    #{
        <<"tracee">> := Tracee,
        <<"mfa">> := MFA,
        <<"offset_ms">> := Offset
    } = Event
) ->
    map_size(Event) =:= 3 andalso is_binary(Tracee) andalso byte_size(Tracee) > 0 andalso
        valid_trace_mfa(MFA) andalso nonnegative_integer(Offset);
valid_trace_event(_Event) ->
    false.

valid_trace_selector(_Command, #{<<"tracee">> := Tracee, <<"mfa">> := MFA}) ->
    is_binary(Tracee) andalso byte_size(Tracee) > 0 andalso valid_trace_mfa(MFA);
valid_trace_selector(trace_stop_all, Trace) ->
    not is_map_key(<<"tracee">>, Trace) andalso not is_map_key(<<"mfa">>, Trace);
valid_trace_selector(_Command, _Trace) ->
    false.

valid_trace_event_selectors(#{<<"tracee">> := Tracee, <<"mfa">> := MFA}, Events) ->
    lists:all(
        fun(Event) ->
            maps:get(<<"tracee">>, Event) =:= Tracee andalso maps:get(<<"mfa">>, Event) =:= MFA
        end,
        Events
    );
valid_trace_event_selectors(_Trace, Events) ->
    Events =:= [].

valid_trace_events(trace_call, Trace, Events) ->
    valid_trace_event_selectors(Trace, Events);
valid_trace_events(trace_stop_all, _Trace, Events) ->
    Events =:= [].

valid_trace_mfa(
    #{
        <<"module">> := Module,
        <<"function">> := Function,
        <<"arity">> := Arity
    } = MFA
) ->
    map_size(MFA) =:= 3 andalso is_binary(Module) andalso byte_size(Module) > 0 andalso
        is_binary(Function) andalso byte_size(Function) > 0 andalso
        is_integer(Arity) andalso Arity >= 0 andalso Arity =< 255;
valid_trace_mfa(_MFA) ->
    false.

valid_otp_state_payload(#{<<"data">> := Data} = Response, Probes) ->
    valid_otp_state_data(Data) andalso valid_otp_state_outcome(Data, Response, Probes).

valid_otp_state_outcome(
    #{<<"status">> := Status},
    #{<<"outcome">> := <<"complete">>},
    [
        #{
            <<"id">> := <<"otp_state">>,
            <<"required">> := true,
            <<"status">> := <<"ok">>,
            <<"reason_code">> := null
        }
    ]
) when Status =:= <<"ok">>; Status =:= <<"not_found">> ->
    true;
valid_otp_state_outcome(
    #{<<"status">> := <<"error">>, <<"reason_code">> := Reason},
    #{<<"outcome">> := <<"partial">>},
    [
        #{
            <<"id">> := <<"otp_state">>,
            <<"required">> := true,
            <<"status">> := <<"error">>,
            <<"reason_code">> := Reason
        }
    ]
) ->
    true;
valid_otp_state_outcome(_Data, _Response, _Probes) ->
    false.

valid_otp_state_data(
    #{
        <<"status">> := Status,
        <<"risk_level">> := <<"high">>,
        <<"behavior">> := Behavior,
        <<"behavior_source">> := <<"operator_asserted">>,
        <<"structural_validation">> := Validation,
        <<"acquisition">> := Acquisition,
        <<"limits">> := Limits,
        <<"truncated">> := Truncated,
        <<"truncation_reason">> := TruncationReason,
        <<"visited_node_count">> := Visited
    } = Data
) ->
    valid_otp_state_data_size(Behavior, Status, Data) andalso
        valid_otp_state_acquisition(Acquisition) andalso
        valid_otp_state_limits(Behavior, Limits) andalso
        valid_otp_state_truncation(Truncated, TruncationReason) andalso
        nonnegative_integer(Visited) andalso Visited =< 10000 andalso
        valid_otp_state_behavior(Behavior, Data) andalso
        valid_otp_state_shape_budget(Behavior, Data) andalso
        valid_otp_state_status(Status, Behavior, Validation, Data);
valid_otp_state_data(_Data) ->
    false.

valid_otp_state_data_size(Behavior, Status, Data) ->
    BehaviorFields =
        case Behavior of
            <<"gen_server">> -> 1;
            <<"gen_statem">> -> 4;
            <<"gen_event">> -> 5;
            _ -> invalid
        end,
    ReasonFields =
        case Status of
            <<"ok">> -> 0;
            <<"not_found">> -> 0;
            <<"error">> -> 1;
            _ -> invalid
        end,
    is_integer(BehaviorFields) andalso is_integer(ReasonFields) andalso
        map_size(Data) =:= 10 + BehaviorFields + ReasonFields.

valid_otp_state_status(<<"ok">>, <<"gen_server">>, <<"not_applicable">>, Data) ->
    not maps:is_key(<<"reason_code">>, Data);
valid_otp_state_status(<<"not_found">>, <<"gen_server">>, <<"not_applicable">>, Data) ->
    not maps:is_key(<<"reason_code">>, Data);
valid_otp_state_status(<<"error">>, <<"gen_server">>, <<"not_applicable">>, Data) ->
    lists:member(maps:get(<<"reason_code">>, Data, undefined), [
        <<"state_timeout">>, <<"state_probe_failed">>, <<"state_shape_failed">>
    ]);
valid_otp_state_status(<<"ok">>, Behavior, <<"passed">>, Data) when
    Behavior =:= <<"gen_statem">>; Behavior =:= <<"gen_event">>
->
    not maps:is_key(<<"reason_code">>, Data);
valid_otp_state_status(<<"not_found">>, Behavior, <<"not_performed">>, Data) when
    Behavior =:= <<"gen_statem">>; Behavior =:= <<"gen_event">>
->
    not maps:is_key(<<"reason_code">>, Data);
valid_otp_state_status(<<"error">>, Behavior, Validation, Data) when
    Behavior =:= <<"gen_statem">>; Behavior =:= <<"gen_event">>
->
    case maps:get(<<"reason_code">>, Data, undefined) of
        <<"behavior_shape_mismatch">> ->
            Validation =:= <<"failed">>;
        Reason when Reason =:= <<"state_timeout">>; Reason =:= <<"state_probe_failed">> ->
            Validation =:= <<"not_performed">>;
        <<"state_shape_failed">> ->
            Validation =:= <<"passed">>;
        _ ->
            false
    end;
valid_otp_state_status(_Status, _Behavior, _Validation, _Data) ->
    false.

valid_otp_state_acquisition(
    #{
        <<"full_state_copy_risk">> := true,
        <<"timeout_ms">> := 5000,
        <<"timeout_retracts_delivered_request">> := false
    } = Acquisition
) ->
    map_size(Acquisition) =:= 3;
valid_otp_state_acquisition(_Acquisition) ->
    false.

valid_otp_state_limits(
    Behavior,
    #{
        <<"output_bytes">> := 65536,
        <<"depth">> := 6,
        <<"nodes">> := 10000,
        <<"container_prefix">> := 2,
        <<"semantic_identifier_bytes">> := 128
    } = Limits
) ->
    valid_otp_state_event_limits(Behavior, Limits);
valid_otp_state_limits(_Behavior, _Limits) ->
    false.

valid_otp_state_event_limits(
    <<"gen_event">>,
    #{
        <<"handler_output_count">> := Count, <<"max_handler_output_count">> := Max
    } = Limits
) ->
    map_size(Limits) =:= 7 andalso positive_integer(Count) andalso Max =:= 200 andalso
        Count =< Max;
valid_otp_state_event_limits(Behavior, Limits) when
    Behavior =:= <<"gen_server">>; Behavior =:= <<"gen_statem">>
->
    map_size(Limits) =:= 5;
valid_otp_state_event_limits(_Behavior, _Limits) ->
    false.

valid_otp_state_truncation(false, null) ->
    true;
valid_otp_state_truncation(true, Reason) ->
    lists:member(Reason, [<<"depth_cap">>, <<"node_cap">>, <<"output_cap">>]);
valid_otp_state_truncation(_Truncated, _Reason) ->
    false.

valid_otp_state_behavior(<<"gen_server">>, #{
    <<"status">> := Status,
    <<"truncated">> := Truncated,
    <<"truncation_reason">> := TruncationReason,
    <<"visited_node_count">> := Visited,
    <<"state_shape">> := Shape
}) ->
    valid_gen_server_shape(Status, Shape, Truncated, TruncationReason, Visited);
valid_otp_state_behavior(<<"gen_statem">>, #{
    <<"status">> := Status,
    <<"truncated">> := Truncated,
    <<"truncation_reason">> := TruncationReason,
    <<"visited_node_count">> := Visited,
    <<"current_state">> := CurrentState,
    <<"current_state_identity">> := Identity,
    <<"current_state_shape">> := CurrentShape,
    <<"data_shape">> := DataShape
}) ->
    valid_gen_statem_state(
        Status,
        CurrentState,
        Identity,
        CurrentShape,
        DataShape,
        Truncated,
        TruncationReason,
        Visited
    );
valid_otp_state_behavior(<<"gen_event">>, #{
    <<"status">> := Status,
    <<"limits">> := #{<<"handler_output_count">> := Limit},
    <<"visited_node_count">> := Visited,
    <<"observed_handler_count">> := Observed,
    <<"returned_count">> := Returned,
    <<"dropped_count">> := Dropped,
    <<"shape_budget_exhausted_count">> := Exhausted,
    <<"handlers">> := Handlers,
    <<"truncated">> := Truncated,
    <<"truncation_reason">> := TruncationReason
}) ->
    valid_gen_event_state(
        Status,
        Observed,
        Returned,
        Dropped,
        Exhausted,
        Handlers,
        Limit,
        Truncated,
        TruncationReason,
        Visited
    );
valid_otp_state_behavior(_Behavior, _Data) ->
    false.

valid_otp_state_shape_budget(<<"gen_server">>, #{<<"state_shape">> := Shape}) ->
    state_shape_bytes(Shape) =< 65536;
valid_otp_state_shape_budget(<<"gen_statem">>, #{
    <<"current_state_shape">> := CurrentShape, <<"data_shape">> := DataShape
}) ->
    state_shape_bytes(CurrentShape) + state_shape_bytes(DataShape) =< 65536;
valid_otp_state_shape_budget(<<"gen_event">>, #{<<"handlers">> := Handlers}) ->
    lists:sum([
        state_shape_bytes(maps:get(<<"state_shape">>, Handler))
     || Handler <- Handlers
    ]) =< 65536.

state_shape_bytes(null) -> 0;
state_shape_bytes(Shape) -> erlang:external_size(Shape).

valid_gen_server_shape(<<"ok">>, null, true, Reason, Visited) ->
    positive_integer(Visited) andalso lists:member(Reason, [<<"node_cap">>, <<"output_cap">>]);
valid_gen_server_shape(<<"ok">>, Shape, _Truncated, Reason, Visited) when is_map(Shape) ->
    positive_integer(Visited) andalso Reason =/= <<"output_cap">> andalso
        valid_state_shape(Shape);
valid_gen_server_shape(Status, null, false, null, 0) ->
    Status =:= <<"not_found">> orelse Status =:= <<"error">>;
valid_gen_server_shape(_Status, _Shape, _Truncated, _Reason, _Visited) ->
    false.

valid_gen_statem_state(
    <<"ok">>,
    CurrentState,
    Identity,
    CurrentShape,
    DataShape,
    Truncated,
    Reason,
    Visited
) ->
    positive_integer(Visited) andalso valid_semantic_identity(Identity, CurrentState) andalso
        valid_gen_statem_shapes(CurrentShape, DataShape, Truncated, Reason);
valid_gen_statem_state(
    Status, null, <<"unavailable">>, null, null, false, null, 0
) ->
    Status =:= <<"not_found">> orelse Status =:= <<"error">>;
valid_gen_statem_state(
    _Status,
    _CurrentState,
    _Identity,
    _CurrentShape,
    _DataShape,
    _Truncated,
    _Reason,
    _Visited
) ->
    false.

valid_gen_statem_shapes(CurrentShape, DataShape, false, null) ->
    valid_state_shape(CurrentShape) andalso valid_state_shape(DataShape);
valid_gen_statem_shapes(CurrentShape, DataShape, true, <<"depth_cap">>) ->
    valid_state_shape(CurrentShape) andalso valid_state_shape(DataShape);
valid_gen_statem_shapes(CurrentShape, null, true, <<"output_cap">>) ->
    CurrentShape =:= null orelse valid_state_shape(CurrentShape);
valid_gen_statem_shapes(null, null, true, <<"node_cap">>) ->
    true;
valid_gen_statem_shapes(CurrentShape, DataShape, true, <<"node_cap">>) ->
    valid_state_shape(CurrentShape) andalso
        (DataShape =:= null orelse valid_state_shape(DataShape));
valid_gen_statem_shapes(_CurrentShape, _DataShape, _Truncated, _Reason) ->
    false.

valid_gen_event_state(
    <<"ok">>,
    Observed,
    Returned,
    Dropped,
    Exhausted,
    Handlers,
    Limit,
    Truncated,
    TruncationReason,
    Visited
) ->
    nonnegative_integer(Observed) andalso nonnegative_integer(Returned) andalso
        nonnegative_integer(Dropped) andalso nonnegative_integer(Exhausted) andalso
        valid_otp_state_event_visited(Observed, Returned, Visited) andalso Returned =< Limit andalso
        valid_otp_state_handlers(Handlers) andalso
        Returned =:= length(Handlers) andalso Observed =:= Returned + Dropped andalso
        valid_otp_state_event_exhaustion(
            Observed, Returned, Limit, Exhausted, Truncated, TruncationReason
        );
valid_gen_event_state(
    Status, 0, 0, 0, 0, [], _Limit, false, null, 0
) ->
    Status =:= <<"not_found">> orelse Status =:= <<"error">>;
valid_gen_event_state(
    _Status,
    _Observed,
    _Returned,
    _Dropped,
    _Exhausted,
    _Handlers,
    _Limit,
    _Truncated,
    _TruncationReason,
    _Visited
) ->
    false.

valid_otp_state_event_visited(0, 0, 0) ->
    true;
valid_otp_state_event_visited(Observed, Returned, Visited) ->
    positive_integer(Observed) andalso positive_integer(Visited) andalso Visited >= Returned.

valid_otp_state_handler(
    #{
        <<"index">> := Index,
        <<"module">> := Module,
        <<"id">> := Id,
        <<"id_identity">> := Identity,
        <<"state_shape">> := Shape
    } = Handler
) ->
    map_size(Handler) =:= 5 andalso positive_integer(Index) andalso is_binary(Module) andalso
        valid_semantic_identity(Identity, Id) andalso valid_state_shape(Shape);
valid_otp_state_handler(_Handler) ->
    false.

valid_otp_state_handlers(Handlers) ->
    valid_otp_state_handlers(Handlers, 1).

valid_otp_state_handlers([], _Index) ->
    true;
valid_otp_state_handlers([#{<<"index">> := Index} = Handler | Rest], Index) ->
    valid_otp_state_handler(Handler) andalso valid_otp_state_handlers(Rest, Index + 1);
valid_otp_state_handlers(_Handlers, _Index) ->
    false.

valid_otp_state_event_exhaustion(Observed, Returned, _Limit, 0, false, null) ->
    Observed =:= Returned;
valid_otp_state_event_exhaustion(Observed, Returned, _Limit, 0, true, <<"depth_cap">>) ->
    Returned > 0 andalso Observed =:= Returned;
valid_otp_state_event_exhaustion(Observed, Returned, Limit, 0, true, <<"output_cap">>) ->
    Returned =:= Limit andalso Observed > Returned;
valid_otp_state_event_exhaustion(
    Observed, Returned, Limit, Exhausted, true, <<"output_cap">>
) when Exhausted > 0 ->
    Exhausted =:= min(Observed, Limit) - Returned;
valid_otp_state_event_exhaustion(
    Observed, Returned, Limit, Exhausted, true, <<"node_cap">>
) when Exhausted > 0 ->
    Remaining = min(Observed, Limit) - Returned,
    Exhausted =:= Remaining orelse Exhausted =:= Remaining + 1;
valid_otp_state_event_exhaustion(
    _Observed, _Returned, _Limit, _Exhausted, _Truncated, _Reason
) ->
    false.

valid_semantic_identity(<<"available">>, Value) ->
    is_binary(Value) andalso byte_size(Value) =< 128;
valid_semantic_identity(<<"unavailable">>, null) ->
    true;
valid_semantic_identity(_Identity, _Value) ->
    false.

valid_state_shape(Shape) ->
    valid_state_shape(Shape, 0).

valid_state_shape(#{<<"type">> := Type} = Shape, Depth) when
    Depth < 6, Type =:= <<"atom">>;
    Depth < 6, Type =:= <<"number">>;
    Depth < 6, Type =:= <<"other">>
->
    map_size(Shape) =:= 1;
valid_state_shape(
    #{<<"type">> := <<"binary">>, <<"size_bytes">> := Size} = Shape, Depth
) ->
    Depth < 6 andalso map_size(Shape) =:= 2 andalso nonnegative_integer(Size);
valid_state_shape(
    #{<<"type">> := <<"bitstring">>, <<"size_bits">> := Size} = Shape, Depth
) ->
    Depth < 6 andalso map_size(Shape) =:= 2 andalso nonnegative_integer(Size);
valid_state_shape(
    #{<<"type">> := <<"truncated">>, <<"truncation_reason">> := <<"node_cap">>} = Shape,
    Depth
) ->
    Depth =< 6 andalso map_size(Shape) =:= 2;
valid_state_shape(
    #{
        <<"type">> := Type,
        <<"truncated">> := true,
        <<"truncation_reason">> := <<"depth_cap">>
    } = Shape,
    6
) ->
    map_size(Shape) =:= 3 andalso valid_state_shape_type(Type);
valid_state_shape(
    #{
        <<"type">> := Type,
        <<"size">> := Size,
        <<"children">> := Children,
        <<"returned_count">> := Returned,
        <<"truncated">> := Truncated
    } = Shape,
    Depth
) ->
    Depth < 6 andalso
        valid_state_container(
            Type, Size, Children, Returned, Truncated, Shape, Depth
        );
valid_state_shape(_Shape, _Depth) ->
    false.

valid_state_shape_type(Type) ->
    lists:member(Type, [
        <<"atom">>,
        <<"number">>,
        <<"binary">>,
        <<"bitstring">>,
        <<"map">>,
        <<"tuple">>,
        <<"list">>,
        <<"other">>
    ]).

valid_state_container(Type, Size, Children, Returned, Truncated, Shape, Depth) ->
    lists:member(Type, [<<"map">>, <<"tuple">>, <<"list">>]) andalso
        is_list(Children) andalso nonnegative_integer(Returned) andalso
        Returned =:= length(Children) andalso Returned =< 2 andalso is_boolean(Truncated) andalso
        lists:all(fun(Child) -> valid_state_shape(Child, Depth + 1) end, Children) andalso
        valid_state_container_size(Type, Size, Returned, Truncated, Shape).

valid_state_container_size(_Type, Size, Returned, Truncated, Shape) when
    is_integer(Size), Size >= 0, Returned =< Size
->
    case maps:find(<<"truncation_reason">>, Shape) of
        error -> map_size(Shape) =:= 5 andalso Truncated =:= (Size > Returned);
        {ok, <<"node_cap">>} -> map_size(Shape) =:= 6 andalso Truncated;
        _ -> false
    end;
valid_state_container_size(<<"list">>, null, _Returned, true, Shape) ->
    case maps:find(<<"truncation_reason">>, Shape) of
        error -> map_size(Shape) =:= 5;
        {ok, <<"node_cap">>} -> map_size(Shape) =:= 6;
        _ -> false
    end;
valid_state_container_size(_Type, _Size, _Returned, _Truncated, _Shape) ->
    false.

nonnegative_integer(Value) -> is_integer(Value) andalso Value >= 0.

positive_integer(Value) -> is_integer(Value) andalso Value > 0.

valid_otp_state_redaction(#{
    <<"behavior">> := <<"gen_statem">>, <<"current_state">> := CurrentState
}) ->
    valid_redacted_label(CurrentState);
valid_otp_state_redaction(#{<<"behavior">> := <<"gen_event">>, <<"handlers">> := Handlers}) when
    is_list(Handlers)
->
    lists:all(fun valid_otp_state_handler_redaction/1, Handlers);
valid_otp_state_redaction(#{<<"behavior">> := <<"gen_server">>}) ->
    true;
valid_otp_state_redaction(_Data) ->
    false.

valid_otp_state_handler_redaction(#{<<"module">> := Module, <<"id">> := Id}) ->
    is_binary(Module) andalso valid_stable_identifier(<<"module-">>, Module) andalso
        valid_redacted_label(Id);
valid_otp_state_handler_redaction(_Handler) ->
    false.

valid_redacted_label(null) ->
    true;
valid_redacted_label(Value) when is_binary(Value) ->
    valid_stable_identifier(<<"label-">>, Value);
valid_redacted_label(_Value) ->
    false.

valid_memory_data(#{<<"runtime">> := Runtime, <<"memory">> := Memory}) ->
    is_map(Runtime) andalso is_map(Memory) andalso maps:is_key(<<"allocator">>, Memory);
valid_memory_data(_Data) ->
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
        _ -> valid_status_data(Data)
    end;
valid_list_command_payload(_Command, _Data, _Probes) ->
    false.

valid_required_probes(snapshot, _Outcome, Probes) ->
    Required = [Probe || #{<<"required">> := true} = Probe <- Probes],
    lists:sort([maps:get(<<"id">>, Probe) || Probe <- Required]) =:=
        [<<"memory">>, <<"resources">>, <<"runtime">>];
valid_required_probes(diagnose, _Outcome, Probes) ->
    Required = [Probe || #{<<"required">> := true} = Probe <- Probes],
    lists:member([maps:get(<<"id">>, Probe) || Probe <- Required], [
        [<<"core_limits">>], [<<"core_limits_and_memory">>]
    ]);
valid_required_probes(memory, _Outcome, Probes) ->
    Required = [Probe || #{<<"required">> := true} = Probe <- Probes],
    lists:sort([maps:get(<<"id">>, Probe) || Probe <- Required]) =:=
        [<<"allocator">>, <<"memory">>];
valid_required_probes(Command, _Outcome, Probes) ->
    RequiredId = required_probe_id(Command),
    case Probes of
        [#{<<"required">> := true, <<"id">> := RequiredId}] ->
            true;
        _ ->
            false
    end.

required_probe_id(memory) -> <<"memory">>;
required_probe_id(schedulers) -> <<"scheduler_wall_time">>;
required_probe_id(distribution) -> <<"distribution">>;
required_probe_id(processes) -> <<"process_inventory">>;
required_probe_id(process) -> <<"process_info">>;
required_probe_id(port) -> <<"port_info">>;
required_probe_id(applications) -> <<"application_inventory">>;
required_probe_id(ets) -> <<"ets_inventory">>;
required_probe_id(mnesia) -> <<"mnesia_inventory">>;
required_probe_id(network) -> <<"network_inventory">>;
required_probe_id(ports) -> <<"port_inventory">>;
required_probe_id(sockets) -> <<"socket_inventory">>;
required_probe_id(otp_state) -> <<"otp_state">>;
required_probe_id(supervision_tree) -> <<"supervision_tree">>;
required_probe_id(logs) -> <<"log_file_tail">>;
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
valid_redacted_identifier_field(Key, _Value) when
    Key =:= <<"sockname">>; Key =:= <<"peername">>
->
    false;
valid_redacted_identifier_field(_Key, _Value) ->
    true.

identifier_field_prefix(<<"pid">>) -> <<"pid-">>;
identifier_field_prefix(<<"tracee">>) -> <<"pid-">>;
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
identifier_field_prefix(<<"sockname">>) -> <<"endpoint-">>;
identifier_field_prefix(<<"peername">>) -> <<"endpoint-">>;
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
            output_put_chars(command_output_device(Format, Response), Output),
            exit_with_code(ExitCode);
        {error, EncodeError} ->
            output_command_encode_error(
                maps:get(<<"command">>, Response, <<"unknown">>), Format, EncodeError
            )
    end.

command_output_device(text, #{<<"outcome">> := <<"error">>}) -> standard_error;
command_output_device(_Format, _Response) -> standard_io.

-spec command_error(atom(), map() | atom(), atom(), term()) -> no_return().
command_error(Command, Options, Category, Reason) when is_map(Options) ->
    Format = command_format(Options),
    command_error(Command, Format, Category, Reason);
command_error(unknown, text, Category, Reason) ->
    Message = maps:get(<<"message">>, observer_cli_cli:error(Category, Reason)),
    output_format(standard_error, "observer_cli: ~ts~n", [
        observer_cli_cli:escape_text(Message)
    ]),
    output_format(standard_error, "Run 'observer_cli --help' for usage.~n", []),
    exit_with_code(error_exit_code(unknown, Category, Reason));
command_error(Command, text, Category, Reason) ->
    Message = maps:get(<<"message">>, observer_cli_cli:error(Category, Reason)),
    output_format(standard_error, "observer_cli ~ts: ~ts~n", [
        command_display(Command), observer_cli_cli:escape_text(Message)
    ]),
    case Category of
        argument ->
            output_format(standard_error, "Run '~ts' for usage.~n", [command_help_command(Command)]);
        _ ->
            ok
    end,
    exit_with_code(error_exit_code(Command, Category, Reason));
command_error(Command, Format, Category, Reason) ->
    Response = error_response(Command, Category, Reason),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            output_put_chars(standard_io, Output),
            exit_with_code(response_exit_code(Response));
        {error, EncodeError} ->
            output_encode_error(EncodeError)
    end.

command_display(trace_call) -> <<"trace call">>;
command_display(trace_stop_all) -> <<"trace stop">>;
command_display(otp_state) -> <<"otp-state">>;
command_display(Command) -> atom_to_binary(Command).

response_command_name(unknown) -> null;
response_command_name(Command) -> Command.

error_exit_code(Command, Category, Reason) ->
    response_exit_code(error_response(Command, Category, Reason)).

error_response(Command, Category, Reason) ->
    observer_cli_cli:response(response_command_name(Command), error, null, null, null, [
        observer_cli_cli:error(Category, Reason)
    ]).

command_help_command(unknown) -> <<"observer_cli --help">>;
command_help_command(tui) -> <<"observer_cli tui --help">>;
command_help_command(trace_call) -> <<"observer_cli trace call --help">>;
command_help_command(trace_stop_all) -> <<"observer_cli trace stop --help">>;
command_help_command(otp_state) -> <<"observer_cli otp-state --help">>;
command_help_command(Command) -> <<"observer_cli ", (atom_to_binary(Command))/binary, " --help">>.

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
    output_format(standard_error, "observer_cli: ~ts~n", [
        observer_cli_cli:escape_text(Message)
    ]),
    exit_with_code(
        error_exit_code(null, maps:get(category, EncodeError), EncodeReason)
    ).

-spec output_command_encode_error(binary() | atom(), text | term | json, map()) -> no_return().
output_command_encode_error(_Command, json, #{reason := json_unavailable} = EncodeError) ->
    output_encode_error(EncodeError);
output_command_encode_error(Command, Format, EncodeError) when Format =:= term; Format =:= json ->
    Category = maps:get(category, EncodeError),
    Reason = maps:get(reason, EncodeError),
    Response = error_response(Command, Category, Reason),
    case observer_cli_cli:encode(Format, Response) of
        {ok, Output} ->
            output_put_chars(standard_io, Output),
            exit_with_code(response_exit_code(Response));
        {error, _RetryError} ->
            output_encode_error(EncodeError)
    end;
output_command_encode_error(_Command, _Format, EncodeError) ->
    output_encode_error(EncodeError).

-ifdef(TEST).
exit_with_code(Code) ->
    case get(observer_cli_test_halt_fun) of
        Fun when is_function(Fun, 1) -> Fun(Code);
        undefined -> erlang:halt(Code)
    end.
output_put_chars(Device, Output) ->
    case get(observer_cli_test_output) of
        capture -> io:put_chars(Output);
        undefined -> io:put_chars(Device, Output)
    end.
output_format(Device, Format, Args) ->
    case get(observer_cli_test_output) of
        capture -> io:put_chars(io_lib:format(Format, Args));
        undefined -> io:format(Device, Format, Args)
    end.
-else.
exit_with_code(Code) ->
    erlang:halt(Code).
output_put_chars(Device, Output) ->
    io:put_chars(Device, Output).
output_format(Device, Format, Args) ->
    io:format(Device, Format, Args).
-endif.

command_from_args(["trace", "call" | _]) ->
    trace_call;
command_from_args(["trace", "stop" | _]) ->
    trace_stop_all;
command_from_args(["tui" | _]) ->
    tui;
command_from_args([[$- | _] | _] = Arguments) ->
    command_from_arguments(Arguments);
command_from_args([First | _]) ->
    observer_cli_cli:command(First);
command_from_args([]) ->
    undefined.

command_from_arguments(["trace", "call" | _]) ->
    trace_call;
command_from_arguments(["trace", "stop" | _]) ->
    trace_stop_all;
command_from_arguments([Argument | Rest]) ->
    case observer_cli_cli:command(Argument) of
        undefined -> command_from_arguments(Rest);
        Command -> Command
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
        true ->
            ok;
        false ->
            RemoteLoadFun(TargetNode),
            case ProbeFun(TargetNode) of
                true -> ok;
                false -> erlang:error({remote_load_failed, TargetNode})
            end
    end,
    maybe_wait_remote_stop(TargetNode),
    io:format("~p~n", [StartFun()]).

remote_module_available(Node) ->
    net_kernel:hidden_connect_node(Node) andalso
        rpc:call(Node, code, ensure_loaded, [observer_cli]) =:= {module, observer_cli} andalso
        compatible_capabilities(rpc:call(Node, observer_cli_snapshot, capabilities, [])).

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
    ControllerOtp = integer_to_list(?OTP_RELEASE),
    TargetOtp = erpc:call(Node, erlang, system_info, [otp_release]),
    case TargetOtp of
        ControllerOtp -> ok;
        _ -> erlang:error({remote_otp_mismatch, ControllerOtp, TargetOtp})
    end,
    application:load(observer_cli),
    Formatter = application:get_env(observer_cli, formatter, ?DEFAULT_FORMATTER),
    FormatterApp = maps:get(application, Formatter),
    Apps = lists:usort([observer_cli, recon, FormatterApp]),
    lists:foreach(fun(Mod) -> remote_load_module(Node, Mod) end, required_modules(Apps)),
    erpc:call(Node, ?MODULE, ensure_set_env, [
        observer_cli, application:get_all_env(observer_cli)
    ]),
    ok.

remote_load_module(Node, Mod) ->
    Result =
        try
            recon:remote_load([Node], Mod)
        catch
            Class:Reason -> {exception, Class, Reason}
        end,
    case Result of
        {[{module, Mod}], []} ->
            ok;
        _ ->
            erlang:error({remote_load_failed, Node, Mod, Result})
    end.

random_local_node_name() ->
    {_, {H, M, S}} = calendar:local_time(),
    lists:flatten(
        io_lib:format("observer_cli_~2.2.0p_~2.2.0p_~2.2.0p_~s", [H, M, S, os:getpid()])
    ).

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
    lists:usort(lists:append([application_modules(App) || App <- AppList])).

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

application_modules(Application) ->
    ensure_application_loaded(Application),
    case application:get_key(Application, modules) of
        {ok, Modules} -> Modules;
        _ -> []
    end.

ensure_application_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, _Reason} -> ok
    end.
