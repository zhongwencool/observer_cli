%% Stateless public argument parsing. Never reads files, cookies or context.
-module(observer_cli_input).

-export([parse/1, duration_ms/1, metric/1, requested_format/1]).

-spec parse([string()]) -> {ok, map()} | {error, map()}.
parse(Arguments) ->
    case collect(Arguments, [], #{}) of
        {ok, Tokens, #{help := true} = Options} ->
            parse_help(help_path(Tokens), Options);
        {ok, [], #{version := true} = Options} ->
            case validate_offline_options(version, Options) of
                ok -> {ok, #{route => version}};
                {error, Message} -> failure(null, Message)
            end;
        {ok, Tokens, Options} ->
            parse_path(Tokens, Options);
        {error, Message} ->
            failure(null, Message)
    end.

collect([], Positionals, Options) ->
    {ok, lists:reverse(Positionals), Options};
collect([Token | Rest], Positionals, Options) ->
    case observer_cli_catalog:option(Token) of
        positional ->
            collect(Rest, [Token | Positionals], Options);
        unknown ->
            {error, <<"Unknown option. Run the command with --help.">>};
        {_Kind, Key} when is_map_key(Key, Options) ->
            {error, <<"Duplicate option --", (option_name(Key))/binary, ".">>};
        {flag, Key} ->
            collect(Rest, Positionals, Options#{Key => true});
        {value, Key} ->
            collect_value(Rest, Positionals, Options, Key)
    end.

collect_value([], _Positionals, _Options, Key) ->
    missing_value(Key);
collect_value([[$-, $- | _] | _], _Positionals, _Options, Key) ->
    missing_value(Key);
collect_value([Value | Rest], Positionals, Options, Key) ->
    collect(Rest, Positionals, Options#{Key => Value}).

missing_value(Key) -> {error, <<"Missing value for --", (option_name(Key))/binary, ".">>}.

help_path(["help" | Rest]) -> Rest;
help_path(Tokens) -> Tokens.

parse_help(Path, Options) ->
    case validate_offline_options(help, Options) of
        ok -> parse_help(Path);
        {error, Message} -> failure(null, Message)
    end.

validate_offline_options(Kind, Options) ->
    Unsupported = maps:without([Kind, format], Options),
    case {map_size(Unsupported), maps:get(format, Options, "text")} of
        {0, "text"} ->
            ok;
        _ ->
            {error,
                <<"Help and version accept only text output and no execution options. ",
                    "Use describe [COMMAND PATH] --json for machine-readable discovery.">>}
    end.

parse_help([]) ->
    {ok, #{route => help, path => []}};
parse_help([Family]) when Family =:= "inspect"; Family =:= "trace" ->
    {ok, #{route => help, path => [Family]}};
parse_help(Path) ->
    case observer_cli_catalog:describe(Path, false) of
        {ok, _} -> {ok, #{route => help, path => Path}};
        {error, Message} -> failure(null, Message)
    end.

parse_path([], Options) when map_size(Options) =:= 0 -> {ok, #{route => help, path => []}};
parse_path(Tokens, Options) ->
    case observer_cli_catalog:resolve(Tokens) of
        {ok, help, Path} ->
            parse_help(Path, Options);
        {ok, Id, Arguments} ->
            Descriptor = observer_cli_catalog:descriptor(Id),
            Command = maps:get(<<"name">>, Descriptor),
            case validate_options(Options, Descriptor) of
                ok -> finish_parse(Id, Command, Arguments, Options);
                {error, Message} -> failure(Command, Message)
            end;
        {error, Message} ->
            failure(null, Message)
    end.

validate_options(Options, Descriptor) ->
    Definitions = maps:get(<<"options">>, Descriptor),
    Allowed = [maps:get(<<"name">>, O) || O <- Definitions],
    Unknown = [K || K <- maps:keys(Options), not lists:member(option_name(K), Allowed)],
    case Unknown of
        [Key | _] ->
            {error,
                <<"--", (option_name(Key))/binary,
                    " is not supported by this command. See --help.">>};
        [] ->
            validate_definitions(Definitions, Options)
    end.

validate_definitions([], Options) ->
    validate_formats(Options);
validate_definitions([D | Rest], Options) ->
    Key = option_key(maps:get(<<"name">>, D)),
    case maps:find(Key, Options) of
        error ->
            case maps:get(<<"required">>, D, false) of
                true ->
                    {error,
                        <<"Required option --", (option_name(Key))/binary,
                            " is missing; see --help.">>};
                false ->
                    validate_definitions(Rest, Options)
            end;
        {ok, Value} ->
            case valid_value(D, Value) of
                true -> validate_definitions(Rest, Options);
                false -> invalid_value(D)
            end
    end.

valid_value(#{<<"kind">> := <<"flag">>}, true) ->
    true;
valid_value(#{<<"enum">> := Values}, Value) ->
    lists:member(unicode:characters_to_binary(Value), Values);
valid_value(#{<<"minimum_ms">> := Min, <<"maximum_ms">> := Max}, Value) ->
    in_range(duration_ms(Value), Min, Max);
valid_value(#{<<"name">> := <<"rate">>, <<"minimum">> := Min, <<"maximum">> := Max}, Value) ->
    case string:split(Value, "/", all) of
        [Count, "s"] -> in_range(integer_value(Count), Min, Max);
        _ -> false
    end;
valid_value(#{<<"minimum">> := Min, <<"maximum">> := Max}, Value) ->
    in_range(integer_value(Value), Min, Max);
valid_value(_Definition, Value) ->
    is_list(Value) andalso Value =/= [] andalso length(Value) =< 4096 andalso printable(Value).

invalid_value(D) ->
    Name = maps:get(<<"name">>, D),
    Constraint =
        case D of
            #{<<"enum">> := Values} ->
                iolist_to_binary(lists:join(", ", Values));
            #{<<"minimum_ms">> := Min, <<"maximum_ms">> := Max} ->
                iolist_to_binary(io_lib:format("~Bms..~Bms", [Min, Max]));
            #{<<"minimum">> := Min, <<"maximum">> := Max} ->
                iolist_to_binary(io_lib:format("~B..~B", [Min, Max]));
            _ ->
                <<"a non-empty, bounded printable value">>
        end,
    {error, <<"Invalid --", Name/binary, "; expected ", Constraint/binary, ".">>}.

validate_formats(#{json := true, format := Format}) when Format =/= "json" ->
    {error, <<"--json conflicts with --format text or term.">>};
validate_formats(#{verbose := true, json := true}) ->
    {error, <<"--verbose is text-only; remove it when using --json.">>};
validate_formats(#{verbose := true, format := Format}) when Format =/= "text" ->
    {error, <<"--verbose is text-only.">>};
validate_formats(_) ->
    ok.

finish_parse(describe, Command, Path, Options) ->
    Schema = maps:get(schema, Options, false),
    Full = maps:get(full, Options, false),
    case {Schema, Full, Path, requested_format(Options)} of
        {true, false, [], json} ->
            {ok, route(describe, Command, Path, Options, describe, #{})};
        {true, _, _, _} ->
            failure(Command, <<"--schema requires JSON output, no path, and no --full.">>);
        {false, true, [_ | _], _} ->
            failure(Command, <<"--full requires no command path.">>);
        {false, _, _, _} ->
            case observer_cli_catalog:describe(Path, Full) of
                {ok, _} -> {ok, route(describe, Command, Path, Options, describe, #{})};
                {error, Message} -> failure(Command, Message)
            end
    end;
finish_parse(tui, Command, [], Options) ->
    case validate_target(Options) of
        ok -> {ok, (route(tui, Command, [], Options, tui, Options))#{route := tui}};
        {error, Message} -> failure(Command, Message)
    end;
finish_parse(Id, Command, Arguments, Options) ->
    case validate_domain(Id, Arguments, Options) of
        ok -> translate_and_validate(Id, Command, Arguments, Options);
        {error, Message} -> failure(Command, Message)
    end.

validate_domain(trace_call, [], _Options) ->
    {error, <<"trace call requires one exact module:function/arity argument.">>};
validate_domain(trace_call, _Arguments, Options) ->
    case validate_process_selector(Options) of
        ok -> validate_target(Options);
        Error -> Error
    end;
validate_domain(Id, _Arguments, Options) when Id =:= inspect_process; Id =:= inspect_state ->
    case validate_process_selector(Options) of
        ok -> validate_selector_mode(Id, Options);
        Error -> Error
    end;
validate_domain(inspect_port, _Arguments, #{id := Port} = Options) ->
    case re:run(Port, "^#Port<0\\.[0-9]+>$", [{capture, none}]) of
        match ->
            no_list_options(Options, [sort, limit]);
        nomatch ->
            {error,
                <<"--id requires a canonical target-local #Port<0.N> value, not a redacted alias.">>}
    end;
validate_domain(Id, _Arguments, Options) when Id =:= inspect_network; Id =:= inspect_socket ->
    validate_metric_window(Options);
validate_domain(check_memory, _Arguments, #{deep := true, app := _}) ->
    {error, <<"--deep conflicts with --app.">>};
validate_domain(_Id, _Arguments, Options) ->
    validate_target(Options).

validate_process_selector(#{pid := _, name := _}) ->
    {error, <<"--pid and --name are mutually exclusive.">>};
validate_process_selector(#{pid := Pid}) ->
    case re:run(Pid, "^<0\\.[0-9]+\\.[0-9]+>$", [{capture, none}]) of
        match ->
            ok;
        nomatch ->
            {error,
                <<"--pid requires a canonical target-local <0.N.N> PID. A redacted pid-N alias cannot be inspected.">>}
    end;
validate_process_selector(#{name := Name}) ->
    case length(Name) =< 255 andalso printable(Name) of
        true -> ok;
        false -> {error, <<"--name requires a bounded printable registered name.">>}
    end;
validate_process_selector(_) ->
    ok.

validate_selector_mode(inspect_state, Options) ->
    case maps:is_key(pid, Options) orelse maps:is_key(name, Options) of
        true -> validate_target(Options);
        false -> {error, <<"inspect state requires exactly one of --pid or --name.">>}
    end;
validate_selector_mode(inspect_process, Options) ->
    case maps:is_key(pid, Options) orelse maps:is_key(name, Options) of
        true -> no_list_options(Options, [sort, limit, window]);
        false -> validate_metric_window(Options)
    end.

no_list_options(Options, Keys) ->
    case lists:any(fun(K) -> maps:is_key(K, Options) end, Keys) of
        true ->
            {error, <<"List sorting, limits and windows cannot be used with a detail selector.">>};
        false ->
            validate_target(Options)
    end.

validate_metric_window(Options) ->
    {_Base, Semantics} = metric(maps:get(sort, Options, "memory")),
    case Semantics =/= current andalso not maps:is_key(window, Options) of
        true ->
            {error,
                <<"Change/rate sorting requires --window. For example: inspect process --sort reductions-rate --window 5s.">>};
        false ->
            validate_target(Options)
    end.

validate_target(#{node := _} = Options) ->
    case {maps:is_key(cookie_env, Options), maps:is_key(cookie_file, Options)} of
        {true, true} ->
            {error, <<"Use exactly one cookie source, not both --cookie-env and --cookie-file.">>};
        {false, false} ->
            {error,
                <<"Explicit --node requires --cookie-env NAME or --cookie-file PATH; environment selectors are not mixed in.">>};
        _ ->
            case observer_cli_capture:target(Options) of
                {ok, _} -> ok;
                _ -> {error, <<"Invalid node or name mode. Use NODE@HOST and short|long.">>}
            end
    end;
validate_target(Options) ->
    case lists:any(fun(K) -> maps:is_key(K, Options) end, [cookie_env, cookie_file, name_mode]) of
        true -> {error, <<"Explicit cookie/name-mode options require an explicit --node.">>};
        false -> ok
    end.

translate_and_validate(Id, Command, Arguments, Options) ->
    {Capture, CaptureArgs, CaptureOptions0} = translate(Id, Arguments, Options),
    Tokens = capture_tokens(Capture, CaptureArgs, CaptureOptions0),
    case observer_cli_capture:parse(Tokens) of
        {ok, #{route := command, options := Validated}} ->
            CaptureOptions = typed_selection(Id, Options, metric_options(Id, Options, Validated)),
            {ok,
                route(Id, Command, Arguments, Options, Capture, CaptureOptions#{
                    arguments => CaptureArgs
                })};
        {error, Error} ->
            Issue = observer_cli_capture:error(
                argument, maps:get(message_reason, Error, maps:get(reason, Error))
            ),
            failure(Command, maps:get(<<"message">>, Issue))
    end.

translate(Id, _Arguments, Options) when
    Id =:= check;
    Id =:= check_cpu;
    Id =:= check_memory;
    Id =:= check_mailbox;
    Id =:= check_connections
->
    C0 = forwarded(Options),
    C = C0#{observe => maps:get(window, Options, "15s")},
    Policy =
        case maps:get(redact, Options, false) of
            true -> C;
            false -> C#{include_identifiers => true}
        end,
    {diagnose, [], Policy};
translate(inspect_process, _Arguments, #{pid := Pid} = Options) ->
    {process, [Pid], maps:without([pid, sort, limit], forwarded(Options))};
translate(inspect_process, _Arguments, #{name := Name} = Options) ->
    {process, [Name], forwarded(Options)};
translate(inspect_port, _Arguments, #{id := Port} = Options) ->
    {port, [Port], maps:without([sort, limit], forwarded(Options))};
translate(inspect_state, _Arguments, Options) ->
    Target = maps:get(pid, Options, maps:get(name, Options, "")),
    {otp_state, [Target], maps:remove(pid, forwarded(Options))};
translate(trace_call, [MFA], Options) ->
    {trace, ["call", MFA], forwarded(Options)};
translate(trace_stop_all, [], Options) ->
    {trace, ["stop"], forwarded(Options)};
translate(Id, [], Options) ->
    Mapping = #{
        inspect_vm => snapshot,
        inspect_memory => memory,
        inspect_scheduler => schedulers,
        inspect_distribution => distribution,
        inspect_process => processes,
        inspect_application => applications,
        inspect_ets => ets,
        inspect_mnesia => mnesia,
        inspect_port => ports,
        inspect_socket => sockets,
        inspect_network => network,
        inspect_supervision => supervision_tree,
        inspect_logs => logs
    },
    Capture = maps:get(Id, Mapping),
    C0 = forwarded(Options),
    C1 =
        case maps:find(window, Options) of
            {ok, Window} -> C0#{duration => Window};
            error -> C0
        end,
    C2 =
        case maps:find(sort, C1) of
            {ok, Sort} ->
                {Base, _} = metric(Sort),
                C1#{sort := Base};
            error ->
                C1
        end,
    C =
        case Capture =:= snapshot andalso not maps:get(redact, Options, false) of
            true -> C2#{include_identifiers => true};
            false -> C2
        end,
    {Capture, [], C}.

forwarded(Options) ->
    maps:with(
        [
            node,
            cookie_env,
            cookie_file,
            name_mode,
            format,
            json,
            verbose,
            redact,
            timeout,
            deep,
            app,
            sort,
            limit,
            behavior,
            handler,
            tail,
            duration,
            pid,
            replace_existing_trace,
            all,
            rate
        ],
        Options
    ).

capture_tokens(Command, Arguments, Options) ->
    Name = binary_to_list(observer_cli_capture_catalog:public_name(Command)),
    [Name | Arguments] ++
        lists:append([
            case V of
                true -> ["--" ++ binary_to_list(option_name(K))];
                _ -> ["--" ++ binary_to_list(option_name(K)), V]
            end
         || {K, V} <- lists:sort(maps:to_list(Options))
        ]).

metric_options(Id, Options, Validated) when
    Id =:= inspect_process; Id =:= inspect_network; Id =:= inspect_socket
->
    {_Base, Semantics} = metric(maps:get(sort, Options, default_sort(Id))),
    Validated#{rank_semantics => Semantics};
metric_options(Id, _Options, Validated) when
    Id =:= check;
    Id =:= check_cpu;
    Id =:= check_memory;
    Id =:= check_mailbox;
    Id =:= check_connections
->
    Focus = maps:get(Id, #{
        check => overview,
        check_cpu => cpu,
        check_memory => memory,
        check_mailbox => mailbox,
        check_connections => connections
    }),
    Validated#{focus => Focus};
metric_options(_Id, _Options, Validated) ->
    Validated.

typed_selection(Id, Options, CaptureOptions) when Id =:= inspect_process; Id =:= inspect_state ->
    case {maps:is_key(pid, Options), maps:is_key(name, Options)} of
        {true, false} -> CaptureOptions#{selector_kind => pid};
        {false, true} -> CaptureOptions#{selector_kind => name};
        _ -> CaptureOptions
    end;
typed_selection(_, _, CaptureOptions) ->
    CaptureOptions.

default_sort(inspect_network) -> "oct";
default_sort(inspect_socket) -> "io";
default_sort(_) -> "memory".

-spec metric(string()) -> {string(), current | delta | rate}.
metric("memory-change") ->
    {"memory", delta};
metric("mailbox-change") ->
    {"message_queue_len", delta};
metric("binary-memory-change") ->
    {"binary_memory", delta};
metric("heap-change") ->
    {"total_heap_size", delta};
metric("reductions-rate") ->
    {"reductions", rate};
metric(Text) ->
    case lists:suffix("-change", Text) of
        true ->
            {lists:sublist(Text, length(Text) - 7), delta};
        false ->
            case lists:suffix("-rate", Text) of
                true -> {lists:sublist(Text, length(Text) - 5), rate};
                false -> {Text, current}
            end
    end.

-spec duration_ms(string()) -> pos_integer() | error.
duration_ms(Text) ->
    case re:run(Text, "^([0-9]+)(ms|s)?$", [{capture, [1, 2], list}]) of
        {match, [Digits, "s"]} -> positive_duration(integer_value(Digits), 1000);
        {match, [Digits, _]} -> positive_duration(integer_value(Digits), 1);
        nomatch -> error
    end.
positive_duration(N, Factor) when is_integer(N), N > 0 -> N * Factor;
positive_duration(_, _) -> error.
integer_value(Text) ->
    try
        list_to_integer(Text)
    catch
        error:badarg -> error
    end.
in_range(N, Min, Max) -> is_integer(N) andalso N >= Min andalso N =< Max.
printable(Text) -> lists:all(fun(C) -> C >= 32 andalso not (C >= 127 andalso C =< 159) end, Text).

route(Id, Command, Arguments, Options, Capture, CaptureOptions) ->
    #{
        route => command,
        id => Id,
        command => Command,
        arguments => Arguments,
        options => Options,
        capture => Capture,
        capture_options => CaptureOptions
    }.
failure(Command, Message) ->
    {error, #{command => Command, message => Message, reason_code => <<"invalid_arguments">>}}.
option_name(Key) -> binary:replace(atom_to_binary(Key), <<"_">>, <<"-">>, [global]).
option_key(Name) -> binary_to_existing_atom(binary:replace(Name, <<"-">>, <<"_">>, [global])).

-spec requested_format(map() | [string()]) -> text | term | json.
requested_format(#{json := true}) ->
    json;
requested_format(#{format := "json"}) ->
    json;
requested_format(#{format := "term"}) ->
    term;
requested_format(Options) when is_map(Options) -> text;
requested_format(Arguments) ->
    case lists:member("--json", Arguments) orelse contains_pair("--format", "json", Arguments) of
        true ->
            json;
        false ->
            case contains_pair("--format", "term", Arguments) of
                true -> term;
                false -> text
            end
    end.
contains_pair(Key, Value, [Key, Value | _]) -> true;
contains_pair(Key, Value, [_ | Rest]) -> contains_pair(Key, Value, Rest);
contains_pair(_, _, []) -> false.
