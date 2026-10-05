%% Public, task-first command definitions. No target or credential access.
-module(observer_cli_catalog).

-export([
    entries/0,
    commands/0,
    describe/2,
    resolve/1,
    descriptor/1,
    option/1,
    help/1,
    schema/0,
    legacy_hint/1
]).

-define(SCHEMA_PATH, "schema/observer_cli.cli.v2.schema.json").

-spec entries() -> [map()].
entries() ->
    [
        #{name => <<"check">>, summary => <<"Find evidence and the next bounded observation">>},
        #{name => <<"inspect">>, summary => <<"Inspect runtime facts or one selected resource">>},
        #{name => <<"trace">>, summary => <<"Explicitly authorize node-global tracing">>},
        #{name => <<"tui">>, summary => <<"Explore interactively; code loading is opt-in">>},
        #{
            name => <<"describe">>,
            summary => <<"Discover commands offline; add a path for details">>
        }
    ].

-spec commands() -> [map()].
commands() -> [descriptor(Id) || {Id, _Path, _Capture, _Summary} <- definitions()].

-spec resolve([string()]) -> {ok, atom(), [string()]} | {error, binary()}.
resolve([]) -> {ok, help, []};
resolve(["help" | Rest]) -> {ok, help, Rest};
resolve(["describe" | Rest]) -> {ok, describe, Rest};
resolve([Family]) when Family =:= "inspect"; Family =:= "trace" -> {ok, help, [Family]};
resolve(["check"]) -> {ok, check, []};
resolve(["check", Focus]) -> resolve_exact(["check", Focus]);
resolve(["trace", "call", MFA]) -> {ok, trace_call, [MFA]};
resolve(Tokens) -> resolve_exact(Tokens).

resolve_exact(Tokens) ->
    case [Id || {Id, Path, _, _} <- definitions(), Path =:= Tokens] of
        [Id] -> {ok, Id, []};
        [] -> {error, unknown_hint(Tokens)}
    end.

unknown_hint([First | _]) ->
    case legacy_hint(First) of
        undefined -> <<"Unknown command path. Run observer_cli --help or inspect --help.">>;
        Hint -> Hint
    end;
unknown_hint([]) ->
    <<"Run observer_cli --help to choose a task.">>.

-spec describe([string()], boolean()) -> {ok, map()} | {error, binary()}.
describe([], false) ->
    {ok, #{
        <<"entries">> => [binary_keys(E) || E <- entries()],
        <<"schema">> => <<"observer_cli.cli/v2">>
    }};
describe([], true) ->
    {ok, #{<<"commands">> => commands(), <<"schema">> => <<"observer_cli.cli/v2">>}};
describe([Family], _Full) when Family =:= "inspect"; Family =:= "trace" ->
    {ok, #{
        <<"commands">> => [
            D
         || D <- commands(), hd(maps:get(<<"argv">>, D)) =:= list_to_binary(Family)
        ]
    }};
describe(Path, _Full) ->
    case resolve_exact(Path) of
        {ok, Id, []} -> {ok, descriptor(Id)};
        Error -> Error
    end.

-spec descriptor(atom()) -> map().
descriptor(Id) ->
    [{Id, Path, Capture, Summary}] = [D || D = {I, _, _, _} <- definitions(), I =:= Id],
    Base = capture_descriptor(Capture),
    Options = [binary_keys(O) || O <- options(Id)],
    maps:merge(Base, #{
        <<"id">> => atom_to_binary(Id),
        <<"name">> => unicode:characters_to_binary(string:join(Path, " ")),
        <<"argv">> => [unicode:characters_to_binary(P) || P <- Path],
        <<"summary">> => Summary,
        <<"positionals">> => positionals(Id),
        <<"options">> => Options,
        <<"mutually_exclusive">> => exclusions(Id),
        <<"constraints">> => constraints(Id),
        <<"prerequisites">> => prerequisites(Id),
        <<"identifiers">> => identifiers(Id),
        <<"authorization">> => authorization(Id),
        <<"output">> => #{
            <<"formats">> => output_formats(Id),
            <<"default">> =>
                case Id of
                    tui -> <<"interactive">>;
                    _ -> <<"text">>
                end,
            <<"verbose_format">> =>
                case Id of
                    tui -> null;
                    _ -> <<"text">>
                end,
            <<"json_minimum_controller_otp">> => 27,
            <<"schema_ref">> =>
                <<"https://raw.githubusercontent.com/zhongwencool/observer_cli/v3.0.0/priv/schema/observer_cli.cli.v2.schema.json">>,
            <<"response_command">> => unicode:characters_to_binary(string:join(Path, " ")),
            <<"data_schemas">> => data_schemas(Id, Capture)
        },
        <<"examples">> => examples(Id, Path)
    }).

data_schemas(Id, _Capture) when
    Id =:= check;
    Id =:= check_cpu;
    Id =:= check_memory;
    Id =:= check_mailbox;
    Id =:= check_connections
->
    [<<"checkData">>];
data_schemas(inspect_process, _) ->
    [<<"processesData">>, <<"processData">>];
data_schemas(inspect_port, _) ->
    [<<"portsData">>, <<"portData">>];
data_schemas(tui, _) ->
    [];
data_schemas(_, Capture) ->
    Names = #{
        snapshot => <<"snapshotData">>,
        memory => <<"memoryData">>,
        schedulers => <<"schedulersData">>,
        distribution => <<"distributionData">>,
        network => <<"networkData">>,
        applications => <<"applicationsData">>,
        ets => <<"etsData">>,
        mnesia => <<"mnesiaData">>,
        sockets => <<"socketsData">>,
        otp_state => <<"otpStateData">>,
        supervision_tree => <<"supervisionTreeData">>,
        logs => <<"logsData">>,
        trace_call => <<"traceData">>,
        trace_stop_all => <<"traceData">>,
        describe => <<"describeData">>
    },
    [maps:get(Capture, Names)].

capture_descriptor(tui) ->
    #{<<"risk_level">> => <<"high">>, <<"side_effects">> => [<<"repeated_sampling">>]};
capture_descriptor(Capture) ->
    Path = binary:split(observer_cli_capture_catalog:public_name(Capture), <<" ">>, [global]),
    {ok, D} = observer_cli_capture_catalog:describe([binary_to_list(P) || P <- Path]),
    D.

definitions() ->
    [
        {check, ["check"], diagnose,
            <<"Observe limits, scheduler pressure, memory and queues for 15s">>},
        {check_cpu, ["check", "cpu"], diagnose,
            <<"Measure scheduler pressure and process activity; not process CPU time">>},
        {check_memory, ["check", "memory"], diagnose,
            <<"Measure BEAM memory and resource changes; not proof of a leak">>},
        {check_mailbox, ["check", "mailbox"], diagnose,
            <<"Inspect mailbox lengths and changes; not a root-cause rule">>},
        {check_connections, ["check", "connections"], diagnose,
            <<"Inspect Erlang peers and VM network context; not host network health">>},
        {inspect_vm, ["inspect", "vm"], snapshot,
            <<"Collect current bounded VM facts; inventories require --deep">>},
        {inspect_memory, ["inspect", "memory"], memory,
            <<"Inspect BEAM memory and allocators, not host RSS">>},
        {inspect_scheduler, ["inspect", "scheduler"], schedulers,
            <<"Measure scheduler utilization and observed run queues">>},
        {inspect_distribution, ["inspect", "distribution"], distribution,
            <<"Inspect Erlang peers and distribution context">>},
        {inspect_network, ["inspect", "network"], network,
            <<"Inspect legacy inet counters, not all host traffic">>},
        {inspect_process, ["inspect", "process"], processes,
            <<"List processes; --pid or --name selects one safe metadata detail">>},
        {inspect_application, ["inspect", "application"], applications,
            <<"Attribute process counts and resources to applications">>},
        {inspect_ets, ["inspect", "ets"], ets, <<"Inspect ETS metadata, never table contents">>},
        {inspect_mnesia, ["inspect", "mnesia"], mnesia,
            <<"Inspect local Mnesia metadata, never table contents">>},
        {inspect_port, ["inspect", "port"], ports,
            <<"List Erlang ports; --id selects one detail, not a TCP port number">>},
        {inspect_socket, ["inspect", "socket"], sockets,
            <<"Inspect the OTP socket registry and its counters">>},
        {inspect_state, ["inspect", "state"], otp_state,
            <<"Explicitly acquire full OTP state and return value-free shapes">>},
        {inspect_supervision, ["inspect", "supervision"], supervision_tree,
            <<"Inspect one application root and direct children only">>},
        {inspect_logs, ["inspect", "logs"], logs,
            <<"Read bounded retained sensitive text from one configured handler">>},
        {trace_call, ["trace", "call"], trace_call,
            <<"Capture an exact MFA for one PID with node-global replacement consent">>},
        {trace_stop_all, ["trace", "stop"], trace_stop_all,
            <<"Explicitly clear node-global legacy tracing; prior state is not restored">>},
        {tui, ["tui"], tui, <<"Open the terminal UI with protected target options">>},
        {describe, ["describe"], describe,
            <<"Discover paths, constraints and risk without contacting a target">>}
    ].

-spec option(string()) -> {flag | value, atom()} | unknown | positional.
option("-h") ->
    {flag, help};
option("--help") ->
    {flag, help};
option("--version") ->
    {flag, version};
option("--" ++ Name) ->
    All = lists:usort(lists:append([options(Id) || {Id, _, _, _} <- definitions()])),
    case [O || O <- All, maps:get(name, O) =:= unicode:characters_to_binary(Name)] of
        [#{key := Key, kind := Kind} | _] -> {binary_to_existing_atom(Kind), Key};
        [] -> unknown
    end;
option(_) ->
    positional.

options(describe) ->
    output_options() ++
        [
            flag(full, <<"Return every full command descriptor">>),
            flag(schema, <<"Export the v2 JSON Schema; requires --json and no path">>)
        ];
options(tui) ->
    target_options() ++
        [
            flag(load_code, <<"Authorize remote code loading on the same OTP major">>),
            duration_option(interval, 1000, 120000, 1500, <<"Requested TUI refresh interval">>)
        ];
options(Id) ->
    target_options() ++ output_options() ++ privacy_options(Id) ++ specific_options(Id).

target_options() ->
    [
        value(node, <<"Explicit node; requires exactly one explicit cookie source">>),
        value(
            cookie_env,
            <<"Cookie environment variable NAME, never a cookie value; requires --node">>
        ),
        value(cookie_file, <<"Owner-only cookie file PATH; requires --node">>),
        (value(name_mode, <<"Requires --node; otherwise inferred from its host">>))#{
            enum => [<<"short">>, <<"long">>]
        }
    ].

output_options() ->
    [
        (value(format, <<"Output representation">>))#{
            enum => [<<"text">>, <<"term">>, <<"json">>], default => <<"text">>
        },
        flag(json, <<"Alias for --format json; controller OTP 27+">>),
        flag(verbose, <<"Expand text evidence; incompatible with JSON or term">>)
    ].

privacy_options(inspect_logs) ->
    [deadline_option()];
privacy_options(_) ->
    [
        flag(redact, <<"Hide identifiers for sharing; aliases cannot address resources">>),
        deadline_option()
    ].

deadline_option() ->
    (duration_option(
        timeout,
        1,
        120000,
        10000,
        <<"Overall deadline including cleanup; must cover the observation window">>
    ))#{
        default_rule =>
            <<"check window + 5s; sampled inspect max(10s, window + 5s); trace duration + 7s">>
    }.

specific_options(Id) when
    Id =:= check;
    Id =:= check_cpu;
    Id =:= check_memory;
    Id =:= check_mailbox;
    Id =:= check_connections
->
    [
        duration_option(window, 5000, 60000, 15000, <<"Shared observation window; default 15s">>),
        (value(fail_on, <<"Exit 1 only for a complete check meeting this finding severity">>))#{
            enum => [<<"warning">>, <<"critical">>]
        },
        value(app, <<"Add existing application-scoped observation; not application CPU time">>)
    ] ++
        case Id of
            check_memory ->
                [flag(deep, <<"Add admitted deep evidence; seven samples; conflicts with --app">>)];
            _ ->
                []
        end;
specific_options(inspect_vm) ->
    [flag(deep, <<"Add admitted inventories; never reads state, logs or payloads">>)];
specific_options(inspect_scheduler) ->
    [duration_option(window, 250, 10000, 1500, <<"Scheduler measurement window">>)];
specific_options(inspect_distribution) ->
    [limit_option(200, 20)];
specific_options(inspect_process) ->
    list_options(processes, true) ++
        [
            value(pid, <<"Canonical target-local <0.N.N> PID; selects detail">>),
            value(name, <<"Registered name; selects detail, never an alias lookup">>)
        ];
specific_options(inspect_port) ->
    list_options(ports, false) ++ [value(id, <<"Canonical #Port<0.N> ID; selects detail">>)];
specific_options(inspect_network) ->
    list_options(network, true);
specific_options(inspect_socket) ->
    list_options(sockets, true);
specific_options(inspect_application) ->
    list_options(applications, false);
specific_options(inspect_ets) ->
    list_options(ets, false);
specific_options(inspect_mnesia) ->
    list_options(mnesia, false);
specific_options(inspect_state) ->
    [
        value(pid, <<"Canonical target-local PID">>),
        value(name, <<"Registered name">>),
        (value(behavior, <<"Required operator assertion of behavior">>))#{
            required => true, enum => [<<"gen_server">>, <<"gen_statem">>, <<"gen_event">>]
        },
        (flag(
            allow_state_read, <<"Authorize full state acquisition before bounded shape reduction">>
        ))#{
            required => true
        },
        limit_option(200, 20)
    ];
specific_options(inspect_supervision) ->
    [(value(app, <<"Running application; root and direct children only">>))#{required => true}];
specific_options(inspect_logs) ->
    [
        value(handler, <<"Handler ID; required when several sources qualify">>),
        (value(tail, <<"Physical lines; capped at 64 KiB retained bytes">>))#{
            minimum => 1, maximum => 2000, default => 200
        }
    ];
specific_options(trace_call) ->
    [
        (value(pid, <<"One live canonical target-local PID">>))#{required => true},
        (flag(
            replace_existing_trace, <<"Authorize node-global trace replacement; no restoration">>
        ))#{
            required => true
        },
        duration_option(duration, 100, 60000, 10000, <<"Bounded call capture duration">>),
        limit_option(1000, 100),
        (value(rate, <<"Recon burst breaker, not a pacer; the trip event is retained">>))#{
            syntax => <<"N/s">>, minimum => 1, maximum => 200
        }
    ];
specific_options(trace_stop_all) ->
    [(flag(all, <<"Authorize clearing node-global legacy tracing">>))#{required => true}];
specific_options(_) ->
    [].

list_options(Capture, Window) ->
    Values = sort_values(Capture),
    [
        (value(sort, <<"Metric meaning is fixed; change/rate sorts require --window">>))#{
            enum => Values, default => hd(Values)
        },
        limit_option(200, 20)
    ] ++
        case Window of
            true ->
                [
                    duration_option(
                        window,
                        250,
                        10000,
                        undefined,
                        <<"Add sampling; does not change the selected metric">>
                    )
                ];
            false ->
                []
        end.

sort_values(processes) ->
    [
        <<"memory">>,
        <<"message_queue_len">>,
        <<"reductions">>,
        <<"binary_memory">>,
        <<"total_heap_size">>,
        <<"memory-change">>,
        <<"mailbox-change">>,
        <<"reductions-rate">>,
        <<"binary-memory-change">>,
        <<"heap-change">>
    ];
sort_values(Capture) when Capture =:= network; Capture =:= sockets ->
    Base = [list_to_binary(K) || K <- observer_cli_capture_catalog:sort_keys(Capture)],
    Base ++ [<<K/binary, "-change">> || K <- Base] ++ [<<K/binary, "-rate">> || K <- Base];
sort_values(Capture) ->
    [list_to_binary(K) || K <- observer_cli_capture_catalog:sort_keys(Capture)].

value(Key, Summary) ->
    #{key => Key, name => option_name(Key), kind => <<"value">>, summary => Summary}.
flag(Key, Summary) ->
    #{
        key => Key,
        name => option_name(Key),
        kind => <<"flag">>,
        default => false,
        summary => Summary
    }.
option_name(Key) -> binary:replace(atom_to_binary(Key), <<"_">>, <<"-">>, [global]).
limit_option(Max, Default) ->
    (value(limit, <<"Returned rows, not a scan-admission bypass">>))#{
        minimum => 1, maximum => Max, default => Default
    }.
duration_option(Key, Min, Max, Default, Summary) ->
    O = (value(Key, Summary))#{
        minimum_ms => Min, maximum_ms => Max, syntax => <<"integer milliseconds, Nms, or Ns">>
    },
    case Default of
        undefined -> O;
        _ -> O#{default => Default}
    end.

positionals(trace_call) ->
    [
        #{
            <<"name">> => <<"MFA">>,
            <<"required">> => true,
            <<"policy">> => <<"Exact exported module:function/arity; no wildcards">>
        }
    ];
positionals(describe) ->
    [#{<<"name">> => <<"COMMAND_PATH">>, <<"required">> => false}];
positionals(_) ->
    [].

exclusions(Id) ->
    Names = [maps:get(name, O) || O <- options(Id)],
    [
        Pair
     || Pair <- [
            [<<"cookie-env">>, <<"cookie-file">>],
            [<<"pid">>, <<"name">>],
            [<<"deep">>, <<"app">>],
            [<<"limit">>, <<"rate">>]
        ],
        lists:all(fun(N) -> lists:member(N, Names) end, Pair)
    ] ++
        case Id of
            tui ->
                [];
            _ ->
                [
                    [<<"verbose">>, <<"json">>],
                    [<<"verbose">>, <<"format=json|term">>],
                    [<<"json">>, <<"format=text|term">>]
                ]
        end.

constraints(Id) ->
    Required = [maps:get(name, O) || O <- options(Id), maps:get(required, O, false)],
    Exclusive = [
        #{<<"kind">> => <<"mutually_exclusive">>, <<"options">> => Pair}
     || Pair <- exclusions(Id)
    ],
    Target =
        case Id of
            describe ->
                [];
            _ ->
                [
                    #{
                        <<"kind">> => <<"exactly_one_option">>,
                        <<"when_present">> => [<<"node">>],
                        <<"options">> => [<<"cookie-env">>, <<"cookie-file">>]
                    }
                ] ++
                    [
                        #{
                            <<"kind">> => <<"requires_options">>,
                            <<"when_present">> => [N],
                            <<"options">> => [<<"node">>]
                        }
                     || N <- [<<"cookie-env">>, <<"cookie-file">>, <<"name-mode">>]
                    ]
        end,
    RequiredRules =
        case Required of
            [] -> [];
            _ -> [#{<<"kind">> => <<"required_options">>, <<"options">> => Required}]
        end,
    Exclusive ++ Target ++ RequiredRules ++ selector_constraints(Id) ++ window_constraints(Id) ++
        runtime_constraints(Id).

runtime_constraints(Id) when
    Id =:= check;
    Id =:= check_cpu;
    Id =:= check_memory;
    Id =:= check_mailbox;
    Id =:= check_connections
->
    [
        timeout_constraint(<<"window">>, 15000, 5000),
        #{
            <<"kind">> => <<"risk_when_present">>,
            <<"when_present">> => [<<"app">>],
            <<"risk_level">> => <<"high">>
        }
    ];
runtime_constraints(inspect_scheduler) ->
    [timeout_constraint(<<"window">>, 1500, 5000)];
runtime_constraints(Id) when
    Id =:= inspect_process; Id =:= inspect_network; Id =:= inspect_socket
->
    [
        (timeout_constraint(<<"window">>, 0, 5000))#{
            <<"when_present">> := [<<"timeout">>, <<"window">>]
        }
    ];
runtime_constraints(trace_call) ->
    [timeout_constraint(<<"duration">>, 10000, 7000)];
runtime_constraints(trace_stop_all) ->
    [
        #{
            <<"kind">> => <<"minimum_duration">>,
            <<"when_present">> => [<<"timeout">>],
            <<"option">> => <<"timeout">>,
            <<"minimum_ms">> => 5000
        }
    ];
runtime_constraints(inspect_state) ->
    [
        #{
            <<"kind">> => <<"minimum_duration">>,
            <<"when_present">> => [<<"timeout">>],
            <<"option">> => <<"timeout">>,
            <<"minimum_ms">> => 10000
        }
    ];
runtime_constraints(describe) ->
    [
        #{
            <<"kind">> => <<"effective_format">>,
            <<"when_present">> => [<<"schema">>],
            <<"format">> => <<"json">>
        },
        #{
            <<"kind">> => <<"positional_count">>,
            <<"when_present">> => [<<"schema">>],
            <<"count">> => 0
        }
    ];
runtime_constraints(_) ->
    [].

timeout_constraint(Sampling, Default, Margin) ->
    #{
        <<"kind">> => <<"timeout_margin">>,
        <<"when_present">> => [<<"timeout">>],
        <<"option">> => <<"timeout">>,
        <<"sampling_option">> => Sampling,
        <<"default_sampling_ms">> => Default,
        <<"margin_ms">> => Margin
    }.

selector_constraints(inspect_state) ->
    [
        #{<<"kind">> => <<"exactly_one_option">>, <<"options">> => [<<"pid">>, <<"name">>]},
        #{
            <<"kind">> => <<"option_value">>,
            <<"when_present">> => [<<"limit">>],
            <<"option">> => <<"behavior">>,
            <<"value">> => <<"gen_event">>
        }
    ];
selector_constraints(inspect_process) ->
    [
        #{
            <<"kind">> => <<"list_options_without_selector">>,
            <<"options">> => [<<"sort">>, <<"limit">>, <<"window">>],
            <<"selectors">> => [<<"pid">>, <<"name">>]
        }
    ];
selector_constraints(inspect_port) ->
    [
        #{
            <<"kind">> => <<"list_options_without_selector">>,
            <<"options">> => [<<"sort">>, <<"limit">>],
            <<"selectors">> => [<<"id">>]
        }
    ];
selector_constraints(_) ->
    [].

window_constraints(Id) when Id =:= inspect_process; Id =:= inspect_network; Id =:= inspect_socket ->
    [
        #{
            <<"kind">> => <<"sampled_sort_requires_window">>,
            <<"suffixes">> => [<<"-change">>, <<"-rate">>]
        }
    ];
window_constraints(_) ->
    [].

prerequisites(describe) ->
    [];
prerequisites(_) ->
    [
        <<"Explicit atomic target or current process environment; never a saved context">>,
        <<"Matching observer_cli 3.0.0 bundle and protocol 2; TUI code loading requires --load-code">>
    ].

identifiers(inspect_logs) ->
    #{<<"policy">> => <<"untrusted_sensitive_text">>, <<"redaction_supported">> => false};
identifiers(describe) ->
    #{<<"policy">> => <<"offline_metadata">>, <<"cookie_values_returned">> => false};
identifiers(_) ->
    #{
        <<"default">> => <<"included">>,
        <<"alias_scope">> => <<"single_response">>,
        <<"aliases_executable">> => false,
        <<"follow_up">> =>
            <<"Use typed selectors and preserve the originating explicit target; --redact disables selectors">>
    }.

authorization(inspect_state) -> [<<"--allow-state-read">>];
authorization(trace_call) -> [<<"--replace-existing-trace">>];
authorization(trace_stop_all) -> [<<"--all">>];
authorization(tui) -> [<<"--load-code only when remote loading is needed">>];
authorization(_) -> [].
output_formats(tui) -> [<<"interactive">>];
output_formats(_) -> [<<"text">>, <<"term">>, <<"json">>].

examples(trace_call, Path) ->
    [
        [
            list_to_binary(P)
         || P <-
                Path ++
                    [
                        "timer:sleep/1",
                        "--pid",
                        "<0.123.0>",
                        "--duration",
                        "1s",
                        "--replace-existing-trace"
                    ]
        ]
    ];
examples(trace_stop_all, Path) ->
    [[list_to_binary(P) || P <- Path ++ ["--all"]]];
examples(inspect_state, Path) ->
    [
        [
            list_to_binary(P)
         || P <- Path ++ ["--name", "my_server", "--behavior", "gen_server", "--allow-state-read"]
        ]
    ];
examples(inspect_supervision, Path) ->
    [[list_to_binary(P) || P <- Path ++ ["--app", "kernel"]]];
examples(_Id, Path) ->
    [[list_to_binary(P) || P <- Path]].

binary_keys(Map) ->
    maps:from_list([{atom_to_binary(K), V} || {K, V} <- maps:to_list(maps:remove(key, Map))]).

-spec help([string()]) -> binary().
help([]) ->
    iolist_to_binary([
        "observer_cli - bounded BEAM investigations\n",
        "Usage: observer_cli [TARGET OPTIONS] COMMAND [OPTIONS]\n\n",
        [["  ", maps:get(name, E), "  ", maps:get(summary, E), "\n"] || E <- entries()],
        "\nStart: observer_cli check                  (15-second overview)\n",
        "Focus: check cpu | memory | mailbox | connections\n",
        "Next:  inspect process --pid '<0.123.0>'\n",
        "\nTarget: --node NODE --cookie-env NAME (or --cookie-file PATH)\n",
        "Shell:  OBSERVER_CLI_NODE + OBSERVER_CLI_COOKIE\n",
        "        Or OBSERVER_CLI_COOKIE_FILE; choose only one cookie source\n",
        "Output: --json | --format term | --verbose (text)\n",
        "Share:  --redact; identifiers are included by default\n",
        "\nRequires a matching 3.0.0 target; cookies grant trusted-peer authority.\n",
        "No persistent connection or saved target is used.\n",
        "Help: COMMAND --help; describe COMMAND --json; --version\n"
    ]);
help([Family]) when Family =:= "inspect"; Family =:= "trace" ->
    {ok, #{<<"commands">> := Commands}} = describe([Family], false),
    iolist_to_binary([
        "Choose a path:\n",
        [["  ", maps:get(<<"name">>, D), " - ", maps:get(<<"summary">>, D), "\n"] || D <- Commands],
        "Run observer_cli PATH --help for options and examples.\n"
    ]);
help(Path) ->
    case describe(Path, false) of
        {ok, #{<<"name">> := Name, <<"summary">> := Summary, <<"options">> := Options} = D} ->
            iolist_to_binary([
                "Usage: observer_cli ",
                Name,
                " [OPTIONS]\n",
                Summary,
                "\n\n",
                [
                    [
                        "  --",
                        maps:get(<<"name">>, O),
                        option_hint(O),
                        "\n    ",
                        maps:get(<<"summary">>, O),
                        "\n"
                    ]
                 || O <- Options
                ],
                "\nExamples:\n",
                [
                    ["  observer_cli ", lists:join(" ", Args), "\n"]
                 || Args <- maps:get(<<"examples">>, D)
                ],
                "Detailed constraints and risk: describe ",
                Name,
                " --json\n"
            ]);
        {error, Hint} ->
            <<Hint/binary, "\n">>
    end.

option_hint(#{<<"minimum_ms">> := Min, <<"maximum_ms">> := Max} = D) ->
    Default =
        case maps:find(<<"default">>, D) of
            {ok, V} -> io_lib:format("; default ~Bms", [V]);
            error -> []
        end,
    io_lib:format(" VALUE (~Bms..~Bms~ts)", [Min, Max, Default]);
option_hint(#{<<"enum">> := Values}) ->
    [" ", lists:join("|", Values)];
option_hint(#{<<"kind">> := <<"value">>}) ->
    " VALUE";
option_hint(_) ->
    "".

-spec schema() -> {ok, binary()} | {error, schema_unavailable}.
schema() ->
    ModulePriv = filename:join([filename:dirname(filename:dirname(code:which(?MODULE))), "priv"]),
    Dirs =
        case code:priv_dir(observer_cli) of
            {error, _} -> [ModulePriv];
            Priv -> [Priv, ModulePriv]
        end,
    read_schema(Dirs).
read_schema([]) ->
    {error, schema_unavailable};
read_schema([Dir | Rest]) ->
    case erl_prim_loader:get_file(filename:join(Dir, ?SCHEMA_PATH)) of
        {ok, B, _} -> {ok, B};
        error -> read_schema(Rest)
    end.

-spec legacy_hint(string()) -> undefined | binary().
legacy_hint("connect") ->
    <<"connect was removed. Pass --node and a cookie source, or set OBSERVER_CLI_NODE and OBSERVER_CLI_COOKIE; then run check.">>;
legacy_hint("status") ->
    <<"status was removed. Run check with an explicit target or current shell environment.">>;
legacy_hint("disconnect") ->
    <<"disconnect was removed. v3 has no persistent connection; legacy context files are left untouched.">>;
legacy_hint("diagnose") ->
    <<"Use check [cpu|memory|mailbox|connections] [--window DURATION]. The default window is 15s.">>;
legacy_hint("snapshot") ->
    <<"Use inspect vm [--deep].">>;
legacy_hint("processes") ->
    <<"Use inspect process [--sort METRIC] [--window DURATION].">>;
legacy_hint("process") ->
    <<"Use inspect process --pid PID or --name NAME.">>;
legacy_hint("ports") ->
    <<"Use inspect port.">>;
legacy_hint("port") ->
    <<"Use inspect port --id '#Port<0.N>'.">>;
legacy_hint("applications") ->
    <<"Use inspect application.">>;
legacy_hint("sockets") ->
    <<"Use inspect socket.">>;
legacy_hint("otp-state") ->
    <<"Use inspect state (--pid PID | --name NAME) --behavior BEHAVIOR --allow-state-read.">>;
legacy_hint("supervision-tree") ->
    <<"Use inspect supervision --app APP.">>;
legacy_hint("schedulers") ->
    <<"Use inspect scheduler [--window DURATION].">>;
legacy_hint(Name) when
    Name =:= "memory";
    Name =:= "distribution";
    Name =:= "network";
    Name =:= "ets";
    Name =:= "mnesia";
    Name =:= "logs"
->
    <<"Use inspect ", (list_to_binary(Name))/binary, ".">>;
legacy_hint(_) ->
    undefined.
