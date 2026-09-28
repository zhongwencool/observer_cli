%% Offline command metadata. Never reads target context or resolves credentials.
-module(observer_cli_catalog).

-export([
    commands/0,
    describe/1,
    public_name/1,
    option/1,
    allowed_options/1,
    schema/0,
    command/1,
    common_options/1,
    sort_keys/1
]).

-define(SCHEMA_PATH, "schema/observer_cli.cli.v1.schema.json").

-spec commands() -> [map()].
commands() ->
    [public(descriptor(Id)) || Id <- identities()].

-spec describe([string()]) -> {ok, map()} | {error, atom()}.
describe([]) ->
    {ok, #{
        <<"commands">> => commands(),
        <<"schema">> => <<"observer_cli.cli/v1">>,
        <<"target_protocol">> => 1
    }};
describe(["trace"]) ->
    {ok, #{<<"commands">> => [public(descriptor(trace_call)), public(descriptor(trace_stop_all))]}};
describe(Tokens) when is_list(Tokens) ->
    case
        [
            D
         || D <- commands(),
            maps:get(<<"argv">>, D) =:= [unicode:characters_to_binary(T) || T <- Tokens]
        ]
    of
        [Descriptor] -> {ok, Descriptor};
        [] -> {error, unknown_command}
    end.

-spec command(string()) -> atom() | undefined.
command("trace") ->
    trace;
command(Text) ->
    case
        [
            Id
         || Id <- identities(),
            Id =/= trace_call,
            Id =/= trace_stop_all,
            public_name(Id) =:= unicode:characters_to_binary(Text)
        ]
    of
        [Id] -> Id;
        [] -> undefined
    end.

-spec sort_keys(atom()) -> [string()].
sort_keys(Id) -> [binary_to_list(Key) || Key <- sorts(Id)].

-spec public_name(atom()) -> binary().
public_name(otp_state) -> <<"otp-state">>;
public_name(supervision_tree) -> <<"supervision-tree">>;
public_name(trace_call) -> <<"trace call">>;
public_name(trace_stop_all) -> <<"trace stop">>;
public_name(Id) -> atom_to_binary(Id, utf8).

-spec option(string()) -> {flag | value, atom()} | unknown | positional.
option("--" ++ Name) ->
    case
        [O || O <- option_definitions(), maps:get(name, O) =:= unicode:characters_to_binary(Name)]
    of
        [#{key := Key, kind := <<"flag">>}] -> {flag, Key};
        [#{key := Key, kind := <<"value">>}] -> {value, Key};
        [] -> unknown
    end;
option(_) ->
    positional.

-spec allowed_options(atom()) -> [atom()].
allowed_options(Id) -> common_options(Id) ++ specific_options(Id).

%% erl_prim_loader understands both ordinary files and archived escript paths.
%% Reading bytes does not require the OTP 27+ json module.
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
        {ok, Binary, _Filename} -> {ok, Binary};
        error -> read_schema(Rest)
    end.

identities() ->
    [
        connect,
        status,
        disconnect,
        snapshot,
        memory,
        schedulers,
        distribution,
        processes,
        process,
        applications,
        ets,
        mnesia,
        network,
        ports,
        port,
        sockets,
        otp_state,
        supervision_tree,
        logs,
        trace_call,
        trace_stop_all,
        diagnose,
        describe
    ].

descriptor(Id) ->
    #{
        id => atom_to_binary(Id, utf8),
        name => public_name(Id),
        argv => binary:split(public_name(Id), <<" ">>, [global]),
        summary => summary(Id),
        positionals => positionals(Id),
        options => [command_option(Id, Key) || Key <- allowed_options(Id)],
        mutually_exclusive => exclusions(Id),
        constraints => constraints(Id),
        prerequisites => prerequisites(Id),
        identifiers => identifiers(Id),
        risk_level => risk_level(Id),
        side_effects => effects(Id),
        authorization => authorization(Id),
        output => #{
            formats => [<<"text">>, <<"term">>, <<"json">>],
            default => <<"text">>,
            json_minimum_controller_otp => 27,
            verbose_format => <<"text">>,
            schema_ref =>
                <<"https://raw.githubusercontent.com/zhongwencool/observer_cli/v2.0.0/priv/schema/observer_cli.cli.v1.schema.json">>,
            response_command => atom_to_binary(Id, utf8)
        },
        examples => examples(Id)
    }.

-spec common_options(atom()) -> [atom()].
common_options(describe) -> [format, json, verbose];
common_options(disconnect) -> [format, json, verbose];
common_options(status) -> [format, json, verbose, timeout];
common_options(Id) when Id =:= connect; Id =:= logs -> remote_options();
common_options(_) -> remote_options() ++ [redact, include_identifiers].

remote_options() -> [node, cookie_env, cookie_file, name_mode, format, json, verbose, timeout].

specific_options(snapshot) ->
    [deep];
specific_options(diagnose) ->
    [observe, deep, app];
specific_options(schedulers) ->
    [duration];
specific_options(distribution) ->
    [limit];
specific_options(processes) ->
    [sort, limit, duration];
specific_options(Id) when Id =:= applications; Id =:= ets; Id =:= mnesia; Id =:= ports ->
    [sort, limit];
specific_options(Id) when Id =:= network; Id =:= sockets -> [sort, limit, duration];
specific_options(process) ->
    [info];
specific_options(otp_state) ->
    [behavior, limit];
specific_options(supervision_tree) ->
    [app];
specific_options(logs) ->
    [handler, tail];
specific_options(trace_call) ->
    [pid, duration, limit, rate, replace_existing_trace];
specific_options(trace_stop_all) ->
    [all];
specific_options(trace) ->
    [pid, duration, limit, rate, replace_existing_trace, all];
specific_options(describe) ->
    [schema];
specific_options(_) ->
    [].

command_option(Id, Key) ->
    [Base] = [O || #{key := K} = O <- option_definitions(), K =:= Key],
    maps:merge(maps:remove(key, Base), option_override(Id, Key)).

option_override(Id, sort) ->
    [Default | _] = Values = sorts(Id),
    #{enum => Values, default => Default};
option_override(trace_call, duration) ->
    #{minimum_ms => 100, maximum_ms => 60000, default => 10000};
option_override(trace_call, limit) ->
    #{maximum => 1000, default => 100};
option_override(schedulers, duration) ->
    #{default => 1500};
option_override(otp_state, limit) ->
    #{requires => <<"--behavior gen_event; output cap only, not an acquisition cap">>};
option_override(diagnose, deep) ->
    #{requires => <<"--observe">>};
option_override(diagnose, app) ->
    #{requires => <<"--observe">>};
option_override(supervision_tree, app) ->
    #{required => true};
option_override(trace_call, pid) ->
    #{required => true};
option_override(trace_call, replace_existing_trace) ->
    #{required => true};
option_override(trace_stop_all, all) ->
    #{required => true};
option_override(_, _) ->
    #{}.

option_definitions() ->
    [
        value(node, <<"Target node; explicit --node requires exactly one cookie source">>),
        value(cookie_env, <<"Environment variable name, never the cookie value; requires --node">>),
        value(cookie_file, <<"Protected cookie file path; requires --node">>),
        (value(name_mode, <<"Distribution naming mode; requires --node">>))#{
            enum => [<<"short">>, <<"long">>],
            default_rule => <<"long when host contains a dot or colon; short otherwise">>
        },
        (value(format, <<"Output format">>))#{
            enum => [<<"text">>, <<"term">>, <<"json">>], default => <<"text">>
        },
        flag(json, <<"Alias for --format json; requires controller OTP 27 or later">>),
        flag(verbose, <<"Detailed text evidence only; incompatible with JSON or term">>),
        (value(
            timeout,
            <<
                "Overall deadline; sampling requires duration + 5s, trace call duration + 7s; "
                "trace stop minimum 5s and otp-state minimum 10s"
            >>
        ))#{
            minimum_ms => 1,
            maximum_ms => 120000,
            default => 10000,
            default_rule =>
                <<"max(10s, duration+5s) for sampled commands; max(10s, duration+7s) for trace call; observe+5s for diagnosis">>,
            syntax => <<"integer milliseconds, Nms, or Ns">>
        },
        flag(
            redact, <<"Replace supported identifiers with response-local non-executable aliases">>
        ),
        flag(include_identifiers, <<"Retain real identifiers for trusted follow-up actions">>),
        flag(deep, <<"Add admitted resource inventories; increases observation cost">>),
        value(sort, <<"Ranking field; values and default depend on command">>),
        (value(limit, <<"Maximum returned entries, not a global scan admission limit">>))#{
            minimum => 1, maximum => 200, default => 20
        },
        (value(duration, <<"Sampling window; counters use the actual measured interval">>))#{
            minimum_ms => 250, maximum_ms => 10000, syntax => <<"integer milliseconds, Nms, or Ns">>
        },
        flag(info, <<"Compatibility option for process metadata">>),
        (value(app, <<"Running application name">>))#{minimum_length => 1, maximum_length => 255},
        (value(observe, <<"Diagnostic observation window">>))#{
            minimum_ms => 5000,
            maximum_ms => 60000,
            syntax => <<"integer milliseconds, Nms, or Ns">>
        },
        value(pid, <<"One live target-local raw PID, not a redacted alias">>),
        (value(
            rate,
            <<"Recon burst-breaker threshold, not a pacer; the trip event is retained and capture can exceed N events">>
        ))#{
            syntax => <<"N/s">>, minimum => 1, maximum => 200
        },
        flag(
            replace_existing_trace,
            <<"Authorize node-global trace replacement; prior trace state is not restored">>
        ),
        flag(all, <<"Authorize stopping node-global legacy tracing">>),
        (value(behavior, <<"Required operator assertion of OTP behavior">>))#{
            required => true, enum => [<<"gen_server">>, <<"gen_statem">>, <<"gen_event">>]
        },
        (value(handler, <<"Logger handler ID; needed when multiple sources qualify">>))#{
            minimum_length => 1, maximum_length => 255
        },
        (value(tail, <<"Physical log lines; read cap 64 KiB and per-line cap 32 KiB">>))#{
            minimum => 1, maximum => 2000, default => 200
        },
        (flag(schema, <<"Export the bundled response JSON Schema without connecting">>))#{
            requires => <<"--json or --format json; no command arguments">>
        }
    ].

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
option_name(Key) -> binary:replace(atom_to_binary(Key, utf8), <<"_">>, <<"-">>, [global]).

sorts(processes) ->
    [
        <<"memory">>,
        <<"message_queue_len">>,
        <<"reductions">>,
        <<"binary_memory">>,
        <<"total_heap_size">>
    ];
sorts(applications) ->
    [<<"memory">>, <<"process_count">>, <<"reductions">>, <<"message_queue_len">>];
sorts(Id) when Id =:= ets; Id =:= mnesia -> [<<"memory">>, <<"size">>];
sorts(network) ->
    [<<"oct">>, <<"recv_oct">>, <<"send_oct">>, <<"cnt">>, <<"recv_cnt">>, <<"send_cnt">>];
sorts(ports) ->
    [<<"queue_size">>, <<"memory">>, <<"input">>, <<"output">>, <<"io">>];
sorts(sockets) ->
    [<<"io">>, <<"read_bytes">>, <<"write_bytes">>, <<"packets">>, <<"waits">>, <<"fails">>].

positionals(Id) when Id =:= process; Id =:= otp_state ->
    [
        #{
            name => <<"PID_OR_NAME">>,
            required => true,
            policy => <<"Raw local PID or registered process name; never a response-local alias">>
        }
    ];
positionals(port) ->
    [#{name => <<"PORT">>, required => true, policy => <<"Raw target-local #Port<...> text">>}];
positionals(trace_call) ->
    [
        #{
            name => <<"MFA">>,
            required => true,
            policy => <<"Exact loaded exported module:function/arity, arity 0..255; no wildcards">>
        }
    ];
positionals(describe) ->
    [
        #{name => <<"COMMAND">>, required => false},
        #{name => <<"SUBCOMMAND">>, required => false, requires => <<"COMMAND=trace">>}
    ];
positionals(_) ->
    [].

exclusions(Id) ->
    Keys = allowed_options(Id),
    [
        [option_name(A), option_name(B)]
     || {A, B} <- [{cookie_env, cookie_file}, {redact, include_identifiers}, {limit, rate}],
        lists:member(A, Keys),
        lists:member(B, Keys)
    ] ++
        case Id of
            diagnose -> [[<<"deep">>, <<"app">>]];
            _ -> []
        end ++
        [
            [<<"verbose">>, <<"json">>],
            [<<"verbose">>, <<"format=term">>],
            [<<"verbose">>, <<"format=json">>],
            [<<"json">>, <<"format=text|term">>]
        ].

%% Data-only dependency rules. when_present means ALL listed options were
%% explicitly supplied; absent defaulted options never create a required timeout.
constraints(Id) ->
    Format = [
        #{kind => <<"effective_format">>, when_present => [<<"verbose">>], format => <<"text">>},
        #{kind => <<"effective_format">>, when_present => [<<"json">>], format => <<"json">>}
    ],
    Keys = allowed_options(Id),
    Target =
        case lists:member(node, Keys) of
            true ->
                [
                    requires([<<"cookie-env">>], [<<"node">>]),
                    requires([<<"cookie-file">>], [<<"node">>]),
                    requires([<<"name-mode">>], [<<"node">>]),
                    #{
                        kind => <<"exactly_one_option">>,
                        when_present => [<<"node">>],
                        options => [<<"cookie-env">>, <<"cookie-file">>]
                    }
                ];
            false ->
                []
        end,
    Exclusive = [
        #{kind => <<"mutually_exclusive">>, options => [option_name(A), option_name(B)]}
     || {A, B} <- [{cookie_env, cookie_file}, {redact, include_identifiers}, {limit, rate}],
        lists:member(A, Keys),
        lists:member(B, Keys)
    ],
    Format ++ Target ++ Exclusive ++ command_constraints(Id).

requires(When, Options) ->
    #{kind => <<"requires_options">>, when_present => When, options => Options}.

required(Options) -> #{kind => <<"required_options">>, options => Options}.

timeout_margin(When, Sampling, Margin) ->
    #{
        kind => <<"timeout_margin">>,
        when_present => When,
        option => <<"timeout">>,
        sampling_option => Sampling,
        margin_ms => Margin
    }.

minimum_timeout(Minimum) ->
    #{
        kind => <<"minimum_duration">>,
        when_present => [<<"timeout">>],
        option => <<"timeout">>,
        minimum_ms => Minimum
    }.

command_constraints(connect) ->
    [required([<<"node">>])];
command_constraints(diagnose) ->
    [
        requires([<<"deep">>], [<<"observe">>]),
        requires([<<"app">>], [<<"observe">>]),
        #{kind => <<"mutually_exclusive">>, options => [<<"deep">>, <<"app">>]},
        timeout_margin([<<"timeout">>, <<"observe">>], <<"observe">>, 5000)
    ];
command_constraints(otp_state) ->
    [
        required([<<"behavior">>]),
        #{
            kind => <<"option_value">>,
            when_present => [<<"limit">>],
            option => <<"behavior">>,
            value => <<"gen_event">>
        },
        minimum_timeout(10000)
    ];
command_constraints(supervision_tree) ->
    [required([<<"app">>])];
command_constraints(trace_call) ->
    [
        required([<<"pid">>, <<"replace-existing-trace">>]),
        (timeout_margin([<<"timeout">>], <<"duration">>, 7000))#{default_sampling_ms => 10000}
    ];
command_constraints(trace_stop_all) ->
    [required([<<"all">>]), minimum_timeout(5000)];
command_constraints(schedulers) ->
    [(timeout_margin([<<"timeout">>], <<"duration">>, 5000))#{default_sampling_ms => 1500}];
command_constraints(Id) when Id =:= processes; Id =:= network; Id =:= sockets ->
    [timeout_margin([<<"timeout">>, <<"duration">>], <<"duration">>, 5000)];
command_constraints(describe) ->
    [
        #{kind => <<"effective_format">>, when_present => [<<"schema">>], format => <<"json">>},
        #{kind => <<"positional_count">>, when_present => [<<"schema">>], count => 0}
    ];
command_constraints(_) ->
    [].

prerequisites(describe) ->
    [];
prerequisites(disconnect) ->
    [];
prerequisites(status) ->
    [<<"Saved context from connect; fresh target connection per invocation">>];
prerequisites(connect) ->
    [
        <<"Explicit --node and one cookie source; probe reports whether the diagnostics bundle is available">>
    ];
prerequisites(_) ->
    [
        <<"Explicit --node and one cookie source, or an existing saved context">>,
        <<"Compatible diagnostics bundle already available on target; no automatic remote code injection">>
    ].

identifiers(logs) ->
    #{policy => <<"untrusted_sensitive_text">>, redaction_supported => false};
identifiers(Id) when Id =:= connect; Id =:= status; Id =:= disconnect; Id =:= describe ->
    #{policy => <<"context_or_catalog_metadata">>, cookie_values_returned => false};
identifiers(Id) ->
    #{
        default => identifier_default(Id),
        alias_scope => <<"single_response">>,
        aliases_executable => false,
        follow_up =>
            <<"Use --include-identifiers only in a trusted workflow; preserve explicit target and cookie source">>
    }.

identifier_default(Id) when Id =:= snapshot; Id =:= diagnose -> <<"redacted">>;
identifier_default(_) -> <<"included">>.

risk_level(Id) when Id =:= describe; Id =:= disconnect -> <<"local">>;
risk_level(Id) when
    Id =:= trace_call;
    Id =:= trace_stop_all;
    Id =:= otp_state;
    Id =:= supervision_tree;
    Id =:= logs
->
    <<"high">>;
risk_level(_) ->
    <<"bounded_observation">>.

effects(describe) ->
    [];
effects(disconnect) ->
    [<<"Remove user-global saved context; not a network disconnect">>];
effects(connect) ->
    [
        <<"Create temporary distribution connection and save user-global target selector without cookie value">>
    ];
effects(schedulers) ->
    remote_effects() ++
        [
            <<"Enable scheduler wall-time measurement for the worker during sampling and release its registration afterward">>
        ];
effects(diagnose) ->
    remote_effects() ++
        [
            <<"Observation samples counters and toggles scheduler wall-time measurement; deep mode adds admitted inventories">>
        ];
effects(otp_state) ->
    remote_effects() ++
        [
            <<"Acquire full OTP state before value-free shape reduction; output limit does not bound state acquisition">>
        ];
effects(Id) when Id =:= trace_call; Id =:= trace_stop_all ->
    remote_effects() ++
        [
            <<"Clear node-wide legacy process/port trace flags, tracers, and static call patterns; prior state not restored">>,
            <<"Recon fixed-name tracer/formatter occupants may be terminated; dynamic trace sessions may be affected indirectly">>
        ];
effects(_) ->
    remote_effects().

remote_effects() ->
    [
        <<"Temporary distribution controller and bounded diagnostics worker; observation consumes target resources">>
    ].
authorization(trace_call) -> [<<"--replace-existing-trace">>];
authorization(trace_stop_all) -> [<<"--all">>];
authorization(otp_state) -> [<<"--behavior">>];
authorization(_) -> [].

summary(connect) ->
    <<"Probe and save a target selector; does not start a daemon">>;
summary(status) ->
    <<"Probe the saved target with a fresh connection">>;
summary(disconnect) ->
    <<"Remove the saved target selector">>;
summary(snapshot) ->
    <<"Capture bounded runtime facts; deep inventories are opt-in">>;
summary(memory) ->
    <<"BEAM memory composition and allocator evidence, not host RSS">>;
summary(schedulers) ->
    <<"Sample normal/dirty scheduler utilization and run queues">>;
summary(distribution) ->
    <<"Connected visible and hidden Erlang node context">>;
summary(processes) ->
    <<"Rank processes by explicit counters or interval observations">>;
summary(process) ->
    <<"Safe process metadata and bounded current stacktrace; no mailbox, dictionary, or arbitrary state">>;
summary(applications) ->
    <<"Group process metrics by application">>;
summary(ets) ->
    <<"ETS metadata only; no table contents">>;
summary(mnesia) ->
    <<"Local Mnesia metadata; stopped Mnesia is not_running">>;
summary(network) ->
    <<"Legacy inet and VM port-driver counters, not host network traffic">>;
summary(ports) ->
    <<"Non-inet Erlang Port metadata, not TCP/UDP port numbers">>;
summary(port) ->
    <<"Inspect one raw target-local Erlang Port">>;
summary(sockets) ->
    <<"Sockets visible through the OTP socket registry">>;
summary(otp_state) ->
    <<"Bounded value-free shapes after full OTP state acquisition">>;
summary(supervision_tree) ->
    <<"One application's root and direct children only; not recursive">>;
summary(logs) ->
    <<"One-shot retained bytes from a trusted logger_std_h regular file on Linux/macOS; no flush, follow, or rotation archive">>;
summary(trace_call) ->
    <<"External/global calls for one PID and exact MFA; identity and relative timing only">>;
summary(trace_stop_all) ->
    <<"Stop node-global legacy tracing with explicit authorization">>;
summary(diagnose) ->
    <<"Evidence-backed calibrated findings; no findings is not proof of node health">>;
summary(describe) ->
    <<"Offline command discovery and bundled schema export; no context or credential reads">>.

examples(connect) ->
    [[<<"connect">>, <<"--node">>, <<"app@host">>, <<"--cookie-env">>, <<"ERL_COOKIE">>]];
examples(process) ->
    [[<<"process">>, <<"my_server">>]];
examples(port) ->
    [[<<"port">>, <<"#Port<0.12>">>]];
examples(otp_state) ->
    [[<<"otp-state">>, <<"my_server">>, <<"--behavior">>, <<"gen_server">>]];
examples(supervision_tree) ->
    [[<<"supervision-tree">>, <<"--app">>, <<"my_app">>]];
examples(trace_call) ->
    [
        [
            <<"trace">>,
            <<"call">>,
            <<"lists:reverse/1">>,
            <<"--pid">>,
            <<"<0.123.0>">>,
            <<"--replace-existing-trace">>
        ]
    ];
examples(trace_stop_all) ->
    [[<<"trace">>, <<"stop">>, <<"--all">>]];
examples(describe) ->
    [
        [<<"describe">>, <<"trace">>, <<"call">>, <<"--json">>],
        [<<"describe">>, <<"--schema">>, <<"--json">>]
    ];
examples(Id) ->
    [[public_name(Id)]].

public(Value) when is_map(Value) ->
    maps:from_list([{atom_to_binary(Key, utf8), public(Item)} || {Key, Item} <- maps:to_list(Value)]);
public(Value) when is_list(Value) -> [public(Item) || Item <- Value];
public(Value) ->
    Value.
