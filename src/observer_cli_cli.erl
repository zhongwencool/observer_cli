-module(observer_cli_cli).

-ignore_xref({json, encode, 1}).

-include_lib("kernel/include/file.hrl").

-export([
    parse/1,
    command/1,
    schema/0,
    target/1,
    cookie_source/1,
    duration/1,
    trace_duration/1,
    trace_limit/1,
    timeout/1,
    context_options/1,
    save_context/1,
    load_context/0,
    delete_context/0,
    response/6,
    error/2,
    encode/2,
    exit_code/1,
    escape_text/1
]).

-ifdef(TEST).
-export([
    context_path/0,
    write_context/2,
    read_context/1,
    decode_context/1,
    delete_context/1,
    validate_list_values/2,
    validate_scheduler_timeout/2,
    context_term/1,
    decode_context_fields/3,
    decode_context_source/1,
    finish_target/3,
    valid_env_name/1,
    safe_cookie_file_mode/1,
    strip_cookie_lf/1,
    valid_application_name/1,
    valid_mfa_text/1,
    duration_ms/1,
    multiply_duration/2,
    reason_code/1,
    ensure_context_dir/1,
    safe_context_destination/1,
    atomic_write_context/2,
    readable_context_dir/1,
    read_context_file/1,
    read_context_bytes/1,
    decode_context_binary/1,
    delete_context_file/1,
    read_cookie_bytes/1,
    read_cookie_file/1,
    finish_atomic_write/3
]).
-endif.

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_NODE_LENGTH, 255).
-define(MAX_COOKIE_LENGTH, 255).
-define(MAX_CONTEXT_BYTES, 8192).
-define(SCHEMA, <<"observer_cli.cli/v1">>).
-define(TRACE_TIMEOUT_MARGIN_MS, 7000).
-define(TRACE_STOP_TIMEOUT_MS, 5000).

-type route() ::
    #{
        route := tui,
        target := string(),
        cookie := undefined | string(),
        interval := integer()
    }
    | #{
        route := command,
        command := atom(),
        arguments := [string()],
        options := map()
    }.
-type parse_error() ::
    #{category := argument, exit_code := 2, reason := term()}.

-spec parse([string()]) -> {ok, route()} | {error, parse_error()}.
parse([]) ->
    argument_error(invalid_arguments);
parse([[$- | _] | _]) ->
    argument_error(global_option_before_command);
parse(["tui" | Arguments]) ->
    parse_tui(Arguments);
parse([First | _] = Arguments) ->
    case command(First) of
        undefined -> argument_error({unknown_command, First});
        Command -> parse_command(Command, tl(Arguments))
    end.

parse_tui([Target]) ->
    {ok, #{route => tui, target => Target, cookie => undefined, interval => 1500}};
parse_tui([Target, Cookie, IntervalText]) ->
    try list_to_integer(IntervalText) of
        Interval when Interval >= 1000 ->
            {ok, #{
                route => tui,
                target => Target,
                cookie => Cookie,
                interval => Interval
            }};
        _ ->
            argument_error(invalid_refresh_interval)
    catch
        error:badarg -> argument_error(invalid_refresh_interval)
    end;
parse_tui(_Arguments) ->
    argument_error(invalid_arguments).

parse_command(Command, Arguments) ->
    parse_command(Command, Arguments, [], #{}).

parse_command(Command, [], Positionals, Options) ->
    Arguments = lists:reverse(Positionals),
    case validate_command(Command, Arguments, Options) of
        ok ->
            {ok, #{
                route => command,
                command => Command,
                arguments => Arguments,
                options => Options
            }};
        {error, Reason} ->
            argument_error(Reason)
    end;
parse_command(Command, [Argument | Rest], Positionals, Options) ->
    case option(Argument) of
        positional ->
            parse_command(Command, Rest, [Argument | Positionals], Options);
        unknown ->
            argument_error({unknown_option, Argument});
        {flag, Key} ->
            add_option(Command, Rest, Positionals, Options, Key, true);
        {value, Key} when is_map_key(Key, Options) ->
            argument_error({duplicate_option, Key});
        {value, Key} ->
            parse_option_value(Command, Rest, Positionals, Options, Key, Argument)
    end.

parse_option_value(_Command, [], _Positionals, _Options, _Key, Option) ->
    argument_error({missing_option_value, Option});
parse_option_value(_Command, [[$-, $- | _] | _], _Positionals, _Options, _Key, Option) ->
    argument_error({missing_option_value, Option});
parse_option_value(Command, [Value | Rest], Positionals, Options, Key, _Option) ->
    add_option(Command, Rest, Positionals, Options, Key, Value).

add_option(_Command, _Rest, _Positionals, Options, Key, _Value) when is_map_key(Key, Options) ->
    argument_error({duplicate_option, Key});
add_option(Command, Rest, Positionals, Options, Key, Value) ->
    parse_command(Command, Rest, Positionals, Options#{Key => Value}).

validate_command(trace, Arguments, Options) ->
    case validate_options(trace, Options) of
        ok -> validate_trace_command(Arguments, Options);
        Error -> Error
    end;
validate_command(Command, Arguments, Options) ->
    case validate_options(Command, Options) of
        ok -> validate_arguments(Command, Arguments);
        Error -> Error
    end.

validate_trace_command(["call", _MFA] = Arguments, Options) ->
    case
        {
            validate_arguments(trace, Arguments),
            maps:get(replace_existing_trace, Options, false),
            maps:is_key(pid, Options),
            lists:all(
                fun(Key) ->
                    lists:member(Key, [pid, replace_existing_trace, limit, rate, duration])
                end,
                trace_mode_keys(Options)
            )
        }
    of
        {ok, true, true, true} -> ok;
        {{error, Reason}, _, _, _} -> {error, Reason};
        {ok, false, _, _} -> {error, replace_existing_trace_required};
        {ok, _, false, _} -> {error, trace_pid_required};
        {ok, _, _, false} -> {error, unsupported_command_option}
    end;
validate_trace_command(["stop"] = Arguments, Options) ->
    case
        {
            validate_arguments(trace, Arguments),
            maps:get(all, Options, false),
            trace_mode_keys(Options)
        }
    of
        {ok, true, [all]} -> validate_trace_stop_timeout(Options);
        {ok, false, _} -> {error, trace_all_required};
        {ok, true, _} -> {error, unsupported_command_option}
    end;
validate_trace_command(Arguments, _Options) ->
    validate_arguments(trace, Arguments).

trace_mode_keys(Options) ->
    Global = [
        node,
        cookie_env,
        cookie_file,
        name_mode,
        format,
        json,
        timeout,
        redact,
        include_identifiers
    ],
    lists:sort(maps:keys(maps:without(Global, Options))).

validate_options(Command, Options) ->
    validate_exclusive_options(
        Command,
        Options,
        [
            {cookie_env, cookie_file},
            {redact, include_identifiers},
            {limit, rate}
        ]
    ).

validate_exclusive_options(Command, Options, [{Left, Right} | Rest]) ->
    case maps:is_key(Left, Options) andalso maps:is_key(Right, Options) of
        true ->
            {error, {mutually_exclusive_options, Left, Right}};
        false ->
            validate_exclusive_options(Command, Options, Rest)
    end;
validate_exclusive_options(Command, Options, []) ->
    validate_format_options(Command, Options).

validate_format_options(_Command, #{json := true, format := Format}) when Format =/= "json" ->
    {error, {mutually_exclusive_options, json, format}};
validate_format_options(_Command, #{format := Format}) when
    Format =/= "text", Format =/= "json", Format =/= "term"
->
    {error, {unsupported_format, Format}};
validate_format_options(Command, Options) ->
    validate_runtime_options(Command, Options).

validate_runtime_options(schedulers, Options) ->
    case only_options(schedulers, Options, [duration]) of
        true ->
            case duration(Options) of
                {ok, Duration} -> validate_scheduler_timeout(Options, Duration);
                {error, Reason} -> {error, Reason}
            end;
        false ->
            {error, unsupported_command_option}
    end;
validate_runtime_options(snapshot, Options) ->
    validate_target_options(snapshot, Options, [deep]);
validate_runtime_options(diagnose, Options) ->
    case only_options(diagnose, Options, [observe, deep, app]) of
        true -> validate_diagnose_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(distribution, #{limit := Text} = Options) ->
    case only_options(distribution, Options, [limit]) of
        true ->
            case positive_integer(Text) of
                Limit when is_integer(Limit), Limit =< 200 -> validate_target_options(Options);
                _ -> {error, invalid_limit}
            end;
        false ->
            {error, unsupported_command_option}
    end;
validate_runtime_options(distribution, Options) ->
    case only_options(distribution, Options, [limit]) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(processes, Options) ->
    case only_options(processes, Options, [sort, limit, duration]) of
        true -> validate_processes_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(applications, Options) ->
    case only_options(applications, Options, [sort, limit]) of
        true ->
            validate_list_options(
                Options, ["memory", "process_count", "reductions", "message_queue_len"]
            );
        false ->
            {error, unsupported_command_option}
    end;
validate_runtime_options(Command, Options) when Command =:= ets; Command =:= mnesia ->
    case only_options(Command, Options, [sort, limit]) of
        true -> validate_list_options(Options, ["memory", "size"]);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(network, Options) ->
    validate_counter_list_options(
        network, Options, ["oct", "recv_oct", "send_oct", "cnt", "recv_cnt", "send_cnt"]
    );
validate_runtime_options(ports, Options) ->
    case only_options(ports, Options, [sort, limit]) of
        true -> validate_list_options(Options, ["queue_size", "memory", "input", "output", "io"]);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(sockets, Options) ->
    validate_counter_list_options(
        sockets, Options, ["io", "read_bytes", "write_bytes", "packets", "waits", "fails"]
    );
validate_runtime_options(process, Options) ->
    validate_target_options(process, Options, [info]);
validate_runtime_options(port, Options) ->
    validate_target_options(port, Options, []);
validate_runtime_options(otp_state, #{behavior := "gen_event"} = Options) ->
    case only_options(otp_state, Options, [behavior, limit]) of
        true ->
            case validate_limit_value(Options) of
                ok -> validate_otp_state_timeout(Options);
                Error -> Error
            end;
        false ->
            {error, unsupported_command_option}
    end;
validate_runtime_options(otp_state, #{behavior := Behavior} = Options) when
    Behavior =:= "gen_server"; Behavior =:= "gen_statem"
->
    case only_options(otp_state, Options, [behavior]) of
        true -> validate_otp_state_timeout(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(otp_state, #{behavior := _Behavior}) ->
    {error, invalid_behavior};
validate_runtime_options(otp_state, _Options) ->
    {error, behavior_required};
validate_runtime_options(supervision_tree, #{app := App} = Options) ->
    case valid_application_name(App) of
        true -> validate_target_options(supervision_tree, Options, [app]);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(supervision_tree, _Options) ->
    {error, application_required};
validate_runtime_options(logs, Options) ->
    case only_options(logs, Options, [handler, tail]) of
        true -> validate_logs_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(trace, #{all := true} = Options) ->
    case only_options(trace, Options, [all]) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(trace, Options) ->
    validate_trace_options(Options);
validate_runtime_options(Command, Options) ->
    validate_target_options(Command, Options, []).

validate_target_options(Command, Options, CommandOptions) ->
    case only_options(Command, Options, CommandOptions) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end.

validate_logs_options(Options) ->
    case maps:find(handler, Options) of
        {ok, Handler} ->
            case observer_cli_log:addressable_handler_id(Handler) of
                true -> validate_logs_tail(Options);
                false -> {error, unaddressable_handler_id}
            end;
        error ->
            validate_logs_tail(Options)
    end.

validate_logs_tail(#{tail := Text} = Options) ->
    case positive_integer(Text) of
        Tail when is_integer(Tail), Tail =< 2000 -> validate_target_options(Options);
        _ -> {error, invalid_tail}
    end;
validate_logs_tail(Options) ->
    validate_target_options(Options).

validate_trace_options(Options) ->
    case only_options(trace, Options, [pid, limit, rate, duration, replace_existing_trace, all]) of
        true ->
            case {trace_duration(Options), trace_limit(Options)} of
                {{ok, Duration}, {ok, _Max}} -> validate_trace_timeout(Options, Duration);
                {{error, Reason}, _} -> {error, Reason};
                {_, {error, Reason}} -> {error, Reason}
            end;
        false ->
            {error, unsupported_command_option}
    end.

validate_trace_timeout(#{timeout := _} = Options, Duration) ->
    case timeout_value(Options) of
        {ok, Timeout} when Timeout >= Duration + ?TRACE_TIMEOUT_MARGIN_MS ->
            validate_target_options(Options);
        {ok, _} ->
            {error, trace_timeout_too_short};
        Error ->
            Error
    end;
validate_trace_timeout(Options, _Duration) ->
    validate_target_options(Options).

validate_trace_stop_timeout(#{timeout := _} = Options) ->
    case timeout_value(Options) of
        {ok, Timeout} when Timeout >= ?TRACE_STOP_TIMEOUT_MS -> ok;
        {ok, _} -> {error, trace_stop_timeout_too_short};
        Error -> Error
    end;
validate_trace_stop_timeout(_Options) ->
    ok.

validate_counter_list_options(Command, Options, Sorts) ->
    case only_options(Command, Options, [sort, limit, duration]) of
        true -> validate_counter_list_values(Options, Sorts);
        false -> {error, unsupported_command_option}
    end.

validate_counter_list_values(Options, Sorts) ->
    case validate_list_values(Options, Sorts) of
        ok -> validate_optional_duration(Options);
        Error -> Error
    end.

validate_optional_duration(#{duration := _} = Options) ->
    case duration(Options) of
        {ok, Duration} -> validate_scheduler_timeout(Options, Duration);
        {error, Reason} -> {error, Reason}
    end;
validate_optional_duration(Options) ->
    validate_target_options(Options).

only_options(Command, Options, CommandOptions) ->
    lists:all(
        fun(Key) -> lists:member(Key, global_options(Command) ++ CommandOptions) end,
        maps:keys(Options)
    ).

global_options(connect) ->
    remote_options();
global_options(status) ->
    [format, json, timeout];
global_options(disconnect) ->
    [format, json];
global_options(logs) ->
    remote_options();
global_options(_Command) ->
    remote_options() ++ [redact, include_identifiers].

remote_options() ->
    [
        node,
        cookie_env,
        cookie_file,
        name_mode,
        format,
        json,
        timeout
    ].

validate_diagnose_options(Options) ->
    Observe = maps:find(observe, Options),
    Deep = maps:is_key(deep, Options),
    App = maps:find(app, Options),
    case {Observe, Deep, App} of
        {error, false, error} ->
            validate_target_options(Options);
        {error, true, _} ->
            {error, observe_required};
        {error, _, {ok, _}} ->
            {error, observe_required};
        {{ok, _}, true, {ok, _}} ->
            {error, {mutually_exclusive_options, deep, app}};
        {{ok, Text}, _, _} ->
            case duration_ms(Text) of
                Duration when is_integer(Duration), Duration >= 5000, Duration =< 60000 ->
                    case App of
                        {ok, Name} -> validate_observation_app(Name, Options, Duration);
                        error -> validate_observation_timeout(Options, Duration)
                    end;
                _ ->
                    {error, invalid_observation_duration}
            end
    end.

validate_observation_app(Name, Options, Duration) ->
    case valid_application_name(Name) of
        true -> validate_observation_timeout(Options, Duration);
        false -> {error, invalid_application}
    end.

validate_observation_timeout(#{timeout := _} = Options, Duration) ->
    case timeout_value(Options) of
        {ok, Timeout} when Timeout >= Duration + 5000 -> validate_target_options(Options);
        {ok, _} -> {error, timeout_too_short};
        Error -> Error
    end;
validate_observation_timeout(Options, _Duration) ->
    validate_target_options(Options).

validate_processes_options(Options) ->
    case
        validate_list_values(
            Options, [
                "memory", "message_queue_len", "reductions", "binary_memory", "total_heap_size"
            ]
        )
    of
        ok ->
            case maps:find(duration, Options) of
                {ok, _} ->
                    case duration(Options) of
                        {ok, Duration} -> validate_scheduler_timeout(Options, Duration);
                        {error, Reason} -> {error, Reason}
                    end;
                error ->
                    validate_target_options(Options)
            end;
        Error ->
            Error
    end.

validate_list_options(Options, Sorts) ->
    case validate_list_values(Options, Sorts) of
        ok -> validate_target_options(Options);
        Error -> Error
    end.

validate_list_values(Options, Sorts) ->
    case maps:get(sort, Options, hd(Sorts)) of
        Sort when is_list(Sort) ->
            case lists:member(Sort, Sorts) of
                true -> validate_limit_value(Options);
                false -> {error, invalid_sort}
            end;
        _ ->
            {error, invalid_sort}
    end.

validate_limit_value(#{limit := Text}) ->
    case positive_integer(Text) of
        Limit when is_integer(Limit), Limit =< 200 -> ok;
        _ -> {error, invalid_limit}
    end;
validate_limit_value(_Options) ->
    ok.

validate_otp_state_timeout(#{timeout := _} = Options) ->
    case timeout_value(Options) of
        {ok, Timeout} when Timeout >= 10000 -> validate_target_options(Options);
        {ok, _} -> {error, otp_state_timeout_too_short};
        Error -> Error
    end;
validate_otp_state_timeout(Options) ->
    validate_target_options(Options).

validate_arguments(process, [_Target]) ->
    ok;
validate_arguments(process, _Arguments) ->
    {error, process_target_required};
validate_arguments(port, [_Target]) ->
    ok;
validate_arguments(port, _Arguments) ->
    {error, port_target_required};
validate_arguments(otp_state, [_Target]) ->
    ok;
validate_arguments(otp_state, _Arguments) ->
    {error, otp_state_target_required};
validate_arguments(supervision_tree, []) ->
    ok;
validate_arguments(supervision_tree, _Arguments) ->
    {error, invalid_arguments};
validate_arguments(trace, ["call", MFA]) ->
    case valid_mfa_text(MFA) of
        true -> ok;
        false -> {error, invalid_mfa}
    end;
validate_arguments(trace, ["stop"]) ->
    ok;
validate_arguments(trace, _Arguments) ->
    {error, invalid_trace_command};
validate_arguments(Command, []) when
    Command =:= snapshot;
    Command =:= diagnose;
    Command =:= processes;
    Command =:= applications;
    Command =:= ets;
    Command =:= mnesia;
    Command =:= network;
    Command =:= ports;
    Command =:= sockets
->
    ok;
validate_arguments(Command, _Arguments) when
    Command =:= snapshot;
    Command =:= diagnose;
    Command =:= processes;
    Command =:= applications;
    Command =:= ets;
    Command =:= mnesia;
    Command =:= network;
    Command =:= ports;
    Command =:= sockets
->
    {error, invalid_arguments};
validate_arguments(_Command, []) ->
    ok;
validate_arguments(_Command, _Arguments) ->
    {error, invalid_arguments}.

validate_scheduler_timeout(#{timeout := _} = Options, Duration) ->
    case timeout_value(Options) of
        {ok, Timeout} when Timeout >= Duration + 5000 -> validate_target_options(Options);
        {ok, _Timeout} -> {error, timeout_too_short};
        {error, Reason} -> {error, Reason}
    end;
validate_scheduler_timeout(Options, _Duration) ->
    validate_target_options(Options).

validate_target_options(#{name_mode := Mode}) when Mode =/= "short", Mode =/= "long" ->
    {error, {unsupported_name_mode, Mode}};
validate_target_options(#{timeout := _Timeout} = Options) ->
    case timeout(Options) of
        {ok, _Milliseconds} -> validate_target_without_timeout(maps:remove(timeout, Options));
        {error, Reason} -> {error, Reason}
    end;
validate_target_options(Options) ->
    validate_target_without_timeout(Options).

validate_target_without_timeout(#{node := _Node} = Options) when
    not is_map_key(cookie_env, Options), not is_map_key(cookie_file, Options)
->
    {error, missing_cookie_source};
validate_target_without_timeout(#{node := _Node} = Options) ->
    case target(Options) of
        {ok, {_Target, _Mode}} -> ok;
        {error, Reason} -> {error, Reason}
    end;
validate_target_without_timeout(_Options) ->
    ok.

-spec target(map()) ->
    {ok, {string(), shortnames | longnames}} | {error, atom()}.
target(#{node := Text} = Options) ->
    case node_parts(Text) of
        {ok, [Name]} ->
            case erlang:apply(inet, gethostname, []) of
                {ok, Host} -> finish_target(Name, Host, Options);
                {error, _Reason} -> {error, invalid_node}
            end;
        {ok, [Name, Host]} ->
            finish_target(Name, Host, Options);
        error ->
            {error, invalid_node}
    end;
target(_Options) ->
    {error, no_active_context}.

-spec cookie_source(map()) -> {ok, binary()} | {error, atom()}.
cookie_source(#{cookie_env := Name}) ->
    case valid_env_name(Name) of
        true ->
            case os:getenv(Name) of
                false -> {error, cookie_source_unavailable};
                Value -> valid_cookie(unicode:characters_to_binary(Value))
            end;
        false ->
            {error, invalid_cookie_source}
    end;
cookie_source(#{cookie_file := Path}) ->
    read_cookie_file(Path);
cookie_source(_Options) ->
    {error, missing_cookie_source}.

-spec duration(map()) -> {ok, pos_integer()} | {error, atom()}.
duration(#{duration := Text}) ->
    case duration_ms(Text) of
        Milliseconds when is_integer(Milliseconds), Milliseconds >= 250, Milliseconds =< 10000 ->
            {ok, Milliseconds};
        _ ->
            {error, invalid_duration}
    end;
duration(_Options) ->
    {ok, 1500}.

-spec trace_duration(map()) -> {ok, pos_integer()} | {error, atom()}.
trace_duration(#{duration := Text}) ->
    case duration_ms(Text) of
        Milliseconds when is_integer(Milliseconds), Milliseconds >= 100, Milliseconds =< 60000 ->
            {ok, Milliseconds};
        _ ->
            {error, invalid_duration}
    end;
trace_duration(_Options) ->
    {ok, 10000}.

-spec trace_limit(map()) -> {ok, pos_integer() | {pos_integer(), 1000}} | {error, atom()}.
trace_limit(#{rate := Text}) ->
    case string:split(Text, "/", all) of
        [CountText, "s"] ->
            case positive_integer(CountText) of
                Count when is_integer(Count), Count =< 200 -> {ok, {Count, 1000}};
                _ -> {error, invalid_rate}
            end;
        _ ->
            {error, invalid_rate}
    end;
trace_limit(#{limit := Text}) ->
    case positive_integer(Text) of
        Count when is_integer(Count), Count =< 1000 -> {ok, Count};
        _ -> {error, invalid_limit}
    end;
trace_limit(_Options) ->
    {ok, 100}.

-spec timeout(map()) -> {ok, pos_integer()} | {error, atom()}.
timeout(#{timeout := _Text} = Options) ->
    timeout_value(Options);
timeout(#{replace_existing_trace := true} = Options) ->
    case trace_duration(Options) of
        {ok, Duration} -> {ok, max(10000, Duration + ?TRACE_TIMEOUT_MARGIN_MS)};
        {error, _Reason} -> {error, invalid_duration}
    end;
timeout(#{duration := _Text} = Options) ->
    case duration(Options) of
        {ok, Duration} -> {ok, max(10000, Duration + 5000)};
        {error, _Reason} -> {error, invalid_duration}
    end;
timeout(#{observe := Text}) ->
    case duration_ms(Text) of
        Duration when is_integer(Duration), Duration >= 5000, Duration =< 60000 ->
            {ok, Duration + 5000};
        _ ->
            {error, invalid_observation_duration}
    end;
timeout(_Options) ->
    {ok, 10000}.

timeout_value(#{timeout := Text}) ->
    case duration_ms(Text) of
        Milliseconds when is_integer(Milliseconds), Milliseconds > 0, Milliseconds =< 120000 ->
            {ok, Milliseconds};
        _ ->
            {error, invalid_timeout}
    end.

-spec context_options(map()) -> {ok, map()} | {error, atom()}.
context_options(#{node := _Node} = Options) ->
    case target(Options) of
        {ok, {Target, NameMode}} ->
            context_source_options(Options#{node => Target, name_mode => mode_text(NameMode)});
        Error ->
            Error
    end;
context_options(_Options) ->
    {error, no_active_context}.

-spec save_context(map()) -> ok | {error, atom()}.
save_context(Options) ->
    case context_options(Options) of
        {ok, ContextOptions} -> write_context(context_path(), context_term(ContextOptions));
        Error -> Error
    end.

-spec load_context() -> {ok, map()} | {error, atom()}.
load_context() ->
    case read_context(context_path()) of
        {ok, Context} -> decode_context(Context);
        Error -> Error
    end.

-spec delete_context() -> ok | {error, atom()}.
delete_context() ->
    delete_context(context_path()).

context_path() ->
    filename:join(filename:basedir(user_config, "observer_cli"), "context.etf").

context_source_options(#{cookie_env := Name} = Options) ->
    case valid_env_name(Name) of
        true -> {ok, maps:without([cookie_file], Options)};
        false -> {error, invalid_cookie_source}
    end;
context_source_options(#{cookie_file := Path} = Options) when is_list(Path), Path =/= [] ->
    Absolute = filename:absname(Path),
    case valid_text(Absolute) andalso length(Absolute) =< 4096 of
        true -> {ok, maps:without([cookie_env], Options#{cookie_file => Absolute})};
        false -> {error, invalid_cookie_source}
    end;
context_source_options(_Options) ->
    {error, missing_cookie_source}.

mode_text(shortnames) -> "short";
mode_text(longnames) -> "long".

context_term(#{node := Node, name_mode := Mode, cookie_env := Name}) ->
    #{
        <<"version">> => 1,
        <<"node">> => list_to_binary(Node),
        <<"name_mode">> => list_to_binary(Mode),
        <<"cookie_source">> => #{<<"type">> => <<"env">>, <<"name">> => list_to_binary(Name)}
    };
context_term(#{node := Node, name_mode := Mode, cookie_file := Path}) ->
    #{
        <<"version">> => 1,
        <<"node">> => list_to_binary(Node),
        <<"name_mode">> => list_to_binary(Mode),
        <<"cookie_source">> => #{<<"type">> => <<"file">>, <<"path">> => list_to_binary(Path)}
    }.

write_context(Path, Context) ->
    Dir = filename:dirname(Path),
    case ensure_context_dir(Dir) of
        ok ->
            case safe_context_destination(Path) of
                ok -> atomic_write_context(Path, term_to_binary(Context));
                Error -> Error
            end;
        Error ->
            Error
    end.

ensure_context_dir(Dir) ->
    case file:read_link_info(Dir) of
        {ok, #file_info{type = directory}} ->
            file:change_mode(Dir, 8#700);
        {ok, _Info} ->
            {error, invalid_context_directory};
        {error, enoent} ->
            case filelib:ensure_dir(filename:join(Dir, "placeholder")) of
                ok -> file:change_mode(Dir, 8#700);
                {error, _Reason} -> {error, context_unavailable}
            end;
        {error, _Reason} ->
            {error, context_unavailable}
    end.

safe_context_destination(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode}} ->
            case Mode band 8#777 of
                8#600 -> ok;
                _ -> {error, context_file_permissions}
            end;
        {ok, _Info} ->
            {error, invalid_context_file};
        {error, enoent} ->
            ok;
        {error, _Reason} ->
            {error, context_unavailable}
    end.

atomic_write_context(Path, Binary) ->
    Temp =
        Path ++ ".tmp." ++ os:getpid() ++ "." ++
            integer_to_list(erlang:unique_integer([positive, monotonic])),
    case file:open(Temp, [write, binary, raw, exclusive]) of
        {ok, File} ->
            Result = atomic_write_open(File, Temp, Path, Binary),
            _ = file:delete(Temp),
            Result;
        {error, _Reason} ->
            {error, context_unavailable}
    end.

atomic_write_open(File, Temp, Path, Binary) ->
    Result =
        case file:change_mode(Temp, 8#600) of
            ok -> file:write(File, Binary);
            Error -> Error
        end,
    Close = file:close(File),
    finish_atomic_write(Temp, Path, {Result, Close}).

finish_atomic_write(Temp, Path, Outcome) ->
    case Outcome of
        {ok, ok} ->
            case file:rename(Temp, Path) of
                ok -> ok;
                {error, _Reason} -> {error, context_unavailable}
            end;
        _ ->
            {error, context_unavailable}
    end.

read_context(Path) ->
    case readable_context_dir(filename:dirname(Path)) of
        ok -> read_context_file(Path);
        Error -> Error
    end.

readable_context_dir(Dir) ->
    case file:read_link_info(Dir) of
        {ok, #file_info{type = directory, mode = Mode}} ->
            case Mode band 8#777 of
                8#700 -> ok;
                _ -> {error, context_directory_permissions}
            end;
        {ok, _Info} ->
            {error, invalid_context_directory};
        {error, enoent} ->
            {error, no_active_context};
        {error, _Reason} ->
            {error, context_unavailable}
    end.

read_context_file(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode, size = Size}} when
            Size =< ?MAX_CONTEXT_BYTES
        ->
            case Mode band 8#777 of
                8#600 -> read_context_bytes(Path);
                _ -> {error, context_file_permissions}
            end;
        {ok, #file_info{type = regular}} ->
            {error, context_too_large};
        {ok, _Info} ->
            {error, invalid_context_file};
        {error, enoent} ->
            {error, no_active_context};
        {error, _Reason} ->
            {error, context_unavailable}
    end.

read_context_bytes(Path) ->
    case file:open(Path, [read, binary, raw]) of
        {ok, File} ->
            Result = file:read(File, ?MAX_CONTEXT_BYTES + 1),
            ok = file:close(File),
            decode_context_binary(Result);
        {error, _Reason} ->
            {error, context_unavailable}
    end.

decode_context_binary({ok, <<131, 80, _/binary>>}) ->
    {error, invalid_context};
decode_context_binary({ok, Binary}) when byte_size(Binary) =< ?MAX_CONTEXT_BYTES ->
    try binary_to_term(Binary, [safe]) of
        Context -> {ok, Context}
    catch
        _:_ -> {error, invalid_context}
    end;
decode_context_binary({ok, _Oversized}) ->
    {error, context_too_large};
decode_context_binary(eof) ->
    {error, invalid_context};
decode_context_binary({error, _Reason}) ->
    {error, context_unavailable}.

decode_context(
    #{
        <<"version">> := 1,
        <<"node">> := Node,
        <<"name_mode">> := Mode,
        <<"cookie_source">> := Source
    } = Context
) when map_size(Context) =:= 4, is_binary(Node), is_binary(Mode), is_map(Source) ->
    decode_context_fields(Node, Mode, Source);
decode_context(_Context) ->
    {error, invalid_context}.

decode_context_fields(Node, Mode, Source) ->
    try {binary_to_list(Node), binary_to_list(Mode), decode_context_source(Source)} of
        {NodeText, ModeText, {ok, SourceOptions}} ->
            Options = SourceOptions#{node => NodeText, name_mode => ModeText},
            case context_options(Options) of
                {ok, Options} -> {ok, Options};
                _Error -> {error, invalid_context}
            end;
        _ ->
            {error, invalid_context}
    catch
        _:_ -> {error, invalid_context}
    end.

decode_context_source(#{<<"type">> := <<"env">>, <<"name">> := Name} = Source) when
    map_size(Source) =:= 2, is_binary(Name)
->
    {ok, #{cookie_env => binary_to_list(Name)}};
decode_context_source(#{<<"type">> := <<"file">>, <<"path">> := Path} = Source) when
    map_size(Source) =:= 2, is_binary(Path)
->
    case filename:pathtype(binary_to_list(Path)) of
        absolute -> {ok, #{cookie_file => binary_to_list(Path)}};
        _ -> error
    end;
decode_context_source(_Source) ->
    error.

delete_context(Path) ->
    case readable_context_dir(filename:dirname(Path)) of
        ok -> delete_context_file(Path);
        {error, no_active_context} -> ok;
        Error -> Error
    end.

delete_context_file(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular, mode = Mode}} ->
            case Mode band 8#777 of
                8#600 ->
                    case file:delete(Path) of
                        ok -> ok;
                        {error, enoent} -> ok;
                        {error, _Reason} -> {error, context_unavailable}
                    end;
                _ ->
                    {error, context_file_permissions}
            end;
        {ok, _Info} ->
            {error, invalid_context_file};
        {error, enoent} ->
            ok;
        {error, _Reason} ->
            {error, context_unavailable}
    end.

node_parts(Text) when is_list(Text), length(Text) =< ?MAX_NODE_LENGTH ->
    case valid_text(Text) of
        true ->
            case string:split(Text, "@", all) of
                [Name] when Name =/= [] -> {ok, [Name]};
                [Name, Host] when Name =/= [], Host =/= [] -> {ok, [Name, Host]};
                _ -> error
            end;
        false ->
            error
    end;
node_parts(_Text) ->
    error.

finish_target(Name, Host, Options) ->
    Target = Name ++ "@" ++ Host,
    case length(Target) =< ?MAX_NODE_LENGTH of
        true -> {ok, {Target, name_mode(Options, Host)}};
        false -> {error, invalid_node}
    end.

name_mode(#{name_mode := "short"}, _Host) ->
    shortnames;
name_mode(#{name_mode := "long"}, _Host) ->
    longnames;
name_mode(_Options, Host) ->
    case lists:member($., Host) orelse lists:member($:, Host) of
        true -> longnames;
        false -> shortnames
    end.

valid_env_name(Name) when is_list(Name), Name =/= [], length(Name) =< 255 ->
    valid_text(Name) andalso not lists:member($=, Name);
valid_env_name(_Name) ->
    false.

read_cookie_file(Path) when is_list(Path), Path =/= [] ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, mode = Mode, size = Size}} when Size =< 257 ->
            case safe_cookie_file_mode(Mode) of
                true ->
                    case read_cookie_bytes(Path) of
                        {ok, Binary} -> valid_cookie(strip_cookie_newline(Binary));
                        {error, invalid_cookie} -> {error, invalid_cookie};
                        {error, _Reason} -> {error, cookie_source_unavailable}
                    end;
                false ->
                    {error, cookie_file_permissions}
            end;
        {ok, #file_info{type = regular}} ->
            {error, invalid_cookie};
        {ok, _Info} ->
            {error, cookie_source_unavailable};
        {error, _Reason} ->
            {error, cookie_source_unavailable}
    end;
read_cookie_file(_Path) ->
    {error, invalid_cookie_source}.

read_cookie_bytes(Path) ->
    case file:open(Path, [raw, binary, read]) of
        {ok, File} ->
            Result =
                case file:read(File, 258) of
                    {ok, Binary} when byte_size(Binary) =< 257 -> {ok, Binary};
                    _ -> {error, invalid_cookie}
                end,
            ok = file:close(File),
            Result;
        {error, _Reason} ->
            {error, unavailable}
    end.

safe_cookie_file_mode(Mode) ->
    case os:type() of
        {unix, _} -> (Mode band 8#077) =:= 0;
        _ -> true
    end.

strip_cookie_newline(Binary) when byte_size(Binary) >= 2 ->
    case binary:part(Binary, byte_size(Binary) - 2, 2) of
        <<"\r\n">> -> binary:part(Binary, 0, byte_size(Binary) - 2);
        _ -> strip_cookie_lf(Binary)
    end;
strip_cookie_newline(Binary) ->
    strip_cookie_lf(Binary).

strip_cookie_lf(Binary) when byte_size(Binary) >= 1 ->
    case binary:last(Binary) of
        $\n -> binary:part(Binary, 0, byte_size(Binary) - 1);
        _ -> Binary
    end;
strip_cookie_lf(Binary) ->
    Binary.

valid_cookie(Binary) when
    is_binary(Binary), byte_size(Binary) > 0, byte_size(Binary) =< ?MAX_COOKIE_LENGTH
->
    case lists:all(fun(Byte) -> Byte >= 32 andalso Byte =< 126 end, binary_to_list(Binary)) of
        true -> {ok, Binary};
        false -> {error, invalid_cookie}
    end;
valid_cookie(_Binary) ->
    {error, invalid_cookie}.

valid_text(Text) ->
    lists:all(
        fun(Character) ->
            Character >= 32 andalso not (Character >= 127 andalso Character =< 159)
        end,
        Text
    ).

valid_application_name(Name) when is_list(Name), Name =/= [], length(Name) =< 255 ->
    valid_text(Name);
valid_application_name(_Name) ->
    false.

valid_mfa_text(Text) when is_list(Text), Text =/= [], length(Text) =< 768 ->
    case string:split(Text, ":", all) of
        [Module, FunctionArity] when Module =/= [] ->
            case string:split(FunctionArity, "/", all) of
                [Function, ArityText] when Function =/= [] ->
                    exact_mfa_name(Module) andalso exact_mfa_name(Function) andalso
                        case positive_integer_or_zero(ArityText) of
                            Arity when is_integer(Arity), Arity =< 255 -> true;
                            _ -> false
                        end;
                _ ->
                    false
            end;
        _ ->
            false
    end;
valid_mfa_text(_Text) ->
    false.

exact_mfa_name(Name) ->
    Name =/= "_" andalso Name =/= "*" andalso valid_text(Name).

duration_ms(Text) when is_list(Text) ->
    case lists:reverse(Text) of
        [$s, $m | Reversed] -> positive_integer(lists:reverse(Reversed));
        [$s | Reversed] -> multiply_duration(positive_integer(lists:reverse(Reversed)), 1000);
        _ -> positive_integer(Text)
    end;
duration_ms(_Text) ->
    error.

positive_integer(Text) ->
    integer_at_least(Text, 1).

positive_integer_or_zero(Text) ->
    integer_at_least(Text, 0).

integer_at_least(Text, Minimum) ->
    try list_to_integer(Text) of
        Value when Value >= Minimum -> Value;
        _ -> error
    catch
        error:badarg -> error
    end.

multiply_duration(Value, Multiplier) when is_integer(Value) -> Value * Multiplier;
multiply_duration(error, _Multiplier) -> error.

option("--node") -> {value, node};
option("--cookie-env") -> {value, cookie_env};
option("--cookie-file") -> {value, cookie_file};
option("--name-mode") -> {value, name_mode};
option("--format") -> {value, format};
option("--json") -> {flag, json};
option("--timeout") -> {value, timeout};
option("--redact") -> {flag, redact};
option("--include-identifiers") -> {flag, include_identifiers};
option("--deep") -> {flag, deep};
option("--sort") -> {value, sort};
option("--limit") -> {value, limit};
option("--duration") -> {value, duration};
option("--info") -> {flag, info};
option("--app") -> {value, app};
option("--observe") -> {value, observe};
option("--pid") -> {value, pid};
option("--rate") -> {value, rate};
option("--replace-existing-trace") -> {flag, replace_existing_trace};
option("--all") -> {flag, all};
option("--behavior") -> {value, behavior};
option("--handler") -> {value, handler};
option("--tail") -> {value, tail};
option([$-, $- | _]) -> unknown;
option(_Argument) -> positional.

-spec command(string()) -> atom() | undefined.
command("connect") -> connect;
command("status") -> status;
command("disconnect") -> disconnect;
command("snapshot") -> snapshot;
command("memory") -> memory;
command("schedulers") -> schedulers;
command("distribution") -> distribution;
command("processes") -> processes;
command("process") -> process;
command("applications") -> applications;
command("ets") -> ets;
command("mnesia") -> mnesia;
command("network") -> network;
command("ports") -> ports;
command("port") -> port;
command("sockets") -> sockets;
command("otp-state") -> otp_state;
command("supervision-tree") -> supervision_tree;
command("logs") -> logs;
command("trace") -> trace;
command("diagnose") -> diagnose;
command(_Argument) -> undefined.

-spec schema() -> binary().
schema() -> ?SCHEMA.

argument_error(Reason) ->
    {error, #{category => argument, exit_code => 2, reason => Reason}}.

-spec response(
    atom() | binary(),
    complete | partial | error | binary(),
    null | map(),
    null | map(),
    null | map(),
    [term()]
) ->
    map().
response(Command, Outcome, Target, Capture, Data, Issues) ->
    #{
        <<"schema">> => ?SCHEMA,
        <<"command">> => command_binary(Command),
        <<"outcome">> => outcome_binary(Outcome),
        <<"data">> => Data,
        <<"meta">> => #{<<"target">> => Target, <<"capture">> => Capture},
        <<"issues">> => Issues
    }.

-spec error(atom(), term()) -> map().
error(Category, Reason) ->
    #{
        <<"severity">> => <<"error">>,
        <<"class">> => atom_to_binary(Category),
        <<"reason_code">> => reason_code(Reason),
        <<"message">> => reason_message(Reason)
    }.

-spec encode(text | term | json, map()) -> {ok, binary()} | {error, map()}.
encode(text, #{
    <<"command">> := Command,
    <<"data">> := #{
        <<"node">> := Node,
        <<"probe">> := <<"succeeded">>,
        <<"diagnostics_module">> := DiagnosticsModule,
        <<"name_mode">> := NameMode,
        <<"cookie_source">> := CookieSource,
        <<"expected_capabilities">> := Expected,
        <<"observed_capabilities">> := Observed
    },
    <<"meta">> := #{<<"target">> := #{<<"otp_release">> := OtpRelease}}
}) when Command =:= <<"connect">>; Command =:= <<"status">> ->
    Prefix =
        case Command of
            <<"connect">> -> <<"Selected ">>;
            <<"status">> -> <<"Active ">>
        end,
    DiagnosticsHint =
        case DiagnosticsModule of
            <<"missing">> ->
                <<
                    "Diagnostics are not installed on the target.\n"
                    "Install the matching observer_cli bundle in the target release.\n"
                >>;
            <<"incompatible">> ->
                <<
                    "The target diagnostics bundle is incompatible.\n"
                    "Install the matching observer_cli bundle in the target release.\n"
                >>;
            _ ->
                <<>>
        end,
    capped(
        iolist_to_binary([
            Prefix,
            escape_text(Node),
            <<"; probe succeeded.\ntarget_otp_release=">>,
            escape_text(OtpRelease),
            <<"\nname_mode=">>,
            escape_text(NameMode),
            <<"\ncookie_source=">>,
            cookie_source_text(CookieSource),
            <<"\ndiagnostics_module=">>,
            DiagnosticsModule,
            <<"\nexpected_capabilities=">>,
            capabilities_text(Expected),
            <<"\nobserved_capabilities=">>,
            capabilities_text(Observed),
            <<"\n">>,
            DiagnosticsHint,
            <<"No persistent connection is kept.\n">>
        ])
    );
encode(text, #{
    <<"command">> := <<"disconnect">>,
    <<"data">> := #{
        <<"node">> := null,
        <<"disconnected">> := true,
        <<"recovered_invalid_context">> := true
    }
}) ->
    {ok, <<"Removed invalid saved target context.\n">>};
encode(text, #{
    <<"command">> := <<"disconnect">>,
    <<"data">> := #{<<"node">> := null, <<"disconnected">> := true}
}) ->
    {ok, <<"No active context.\n">>};
encode(text, #{
    <<"command">> := <<"disconnect">>,
    <<"data">> := #{<<"node">> := Node, <<"disconnected">> := true}
}) ->
    capped(
        iolist_to_binary([
            <<"Removed saved target context for ">>, escape_text(Node), <<".\n">>
        ])
    );
encode(text, #{<<"command">> := <<"logs">>, <<"data">> := Data} = Response) when
    is_map(Data)
->
    capped(iolist_to_binary(logs_text(Response, Data)));
encode(text, #{<<"command">> := Command} = Response) ->
    capped(
        iolist_to_binary([
            <<"observer_cli ">>,
            text_command(Command),
            <<"\n">>,
            render_text_map(text_response(Response), 0, root)
        ])
    );
encode(term, Response) ->
    Body = iolist_to_binary(io_lib:format("~0tp.", [Response])),
    capped(iolist_to_binary([harden_term(Body), <<"\n">>]));
encode(json, Response) ->
    case code:ensure_loaded(json) of
        {module, json} ->
            try
                Body = iolist_to_binary(erlang:apply(json, encode, [Response])),
                capped(iolist_to_binary([harden_json(Body), <<"\n">>]))
            of
                Result -> Result
            catch
                _:_ -> encoder_error(json_encoding_failed)
            end;
        {error, _Reason} ->
            {error, controller_error(capability, json_unavailable)}
    end;
encode(_Format, _Response) ->
    {error, controller_error(format, unsupported_format)}.

text_command(<<"trace_call">>) -> <<"trace call">>;
text_command(<<"trace_stop_all">>) -> <<"trace stop">>;
text_command(Command) -> escape_text(Command).

logs_text(Response, Data) ->
    Meta = maps:get(<<"meta">>, Response),
    Target = maps:get(<<"target">>, Meta),
    Sources = maps:get(<<"sources">>, Data),
    Selected = maps:get(<<"selected_source">>, Data),
    Tail = maps:get(<<"tail">>, Data),
    [
        <<"observer_cli logs\n">>,
        logs_target_text(Target),
        logs_source_text(Selected, Sources),
        logs_tail_text(Tail),
        logs_failure_text(Response, Tail)
    ].

logs_target_text(#{<<"node">> := Node, <<"otp_release">> := Otp}) ->
    [<<"target=">>, escape_text(Node), <<" otp=">>, escape_text(Otp), <<"\n">>];
logs_target_text(_Target) ->
    [].

logs_source_text(null, Sources) ->
    [logs_summary_text(Source) || Source <- Sources];
logs_source_text(
    #{
        <<"id">> := Id,
        <<"handler_kind">> := Kind,
        <<"configured_path">> := Path
    },
    _Sources
) ->
    [
        <<"handler=">>,
        escape_text(Id),
        <<" handler_kind=">>,
        escape_text(Kind),
        <<"\nconfigured_path=">>,
        escape_text(Path),
        <<"\n">>
    ].

logs_summary_text(#{
    <<"id">> := Id,
    <<"addressable">> := Addressable,
    <<"supported">> := Supported,
    <<"reason_code">> := Reason
}) ->
    [
        <<"source=">>,
        escape_text(Id),
        <<" addressable=">>,
        text_scalar(Addressable),
        <<" supported=">>,
        text_scalar(Supported),
        <<" reason=">>,
        text_scalar(Reason),
        <<"\n">>
    ].

logs_tail_text(null) ->
    [];
logs_tail_text(Tail) ->
    Lines = maps:get(<<"lines">>, Tail),
    Truncated = maps:get(<<"truncated_line_indexes">>, Tail),
    [
        <<"scope=">>,
        escape_text(maps:get(<<"scope">>, Tail)),
        <<" active_handler_fd_match=">>,
        escape_text(maps:get(<<"active_handler_fd_match">>, Tail)),
        <<"\nvisibility=">>,
        escape_text(maps:get(<<"visibility">>, Tail)),
        <<" command_filesync_requested=">>,
        text_scalar(maps:get(<<"command_filesync_requested">>, Tail)),
        <<" consistency=">>,
        escape_text(maps:get(<<"consistency">>, Tail)),
        <<"\nrequested_lines=">>,
        text_scalar(maps:get(<<"requested_lines">>, Tail)),
        <<" returned_lines=">>,
        text_scalar(maps:get(<<"returned_lines">>, Tail)),
        <<" bytes_read=">>,
        text_scalar(maps:get(<<"bytes_read">>, Tail)),
        <<" has_more=">>,
        text_scalar(maps:get(<<"has_more">>, Tail)),
        <<"\n--- UNTRUSTED LOG CONTENT ---\n">>,
        logs_lines_text(Lines, Truncated, 0)
    ].

logs_lines_text([], _Truncated, _Index) ->
    [];
logs_lines_text([Line | Rest], Truncated, Index) ->
    Omitted =
        case lists:member(Index, Truncated) of
            true -> <<"[earlier bytes omitted] ">>;
            false -> <<>>
        end,
    [
        <<"| ">>,
        Omitted,
        logs_line_text(Line),
        <<"\n">>,
        logs_lines_text(Rest, Truncated, Index + 1)
    ].

logs_line_text(Line) when is_binary(Line) ->
    escape_text(Line);
logs_line_text(#{<<"encoding">> := <<"base64">>, <<"data">> := Data}) ->
    <<"base64:", Data/binary>>.

logs_failure_text(_Response, Tail) when Tail =/= null ->
    [];
logs_failure_text(Response, null) ->
    Capture = maps:get(<<"capture">>, maps:get(<<"meta">>, Response)),
    Reason =
        case Capture of
            #{<<"probes">> := [#{<<"reason_code">> := ProbeReason}]} -> ProbeReason;
            _ -> null
        end,
    [
        <<"outcome=">>,
        escape_text(maps:get(<<"outcome">>, Response)),
        <<" reason=">>,
        text_scalar(Reason),
        <<"\n">>
    ].

cookie_source_text(#{<<"type">> := <<"env">>, <<"name">> := Name}) ->
    [<<"env:">>, escape_text(Name)];
cookie_source_text(#{<<"type">> := <<"file">>, <<"path">> := Path}) ->
    [<<"file:">>, escape_text(Path)].

capabilities_text(null) ->
    <<"none">>;
capabilities_text(#{
    <<"protocol_version">> := Protocol, <<"bundle_version">> := Bundle
}) ->
    [
        <<"protocol=">>,
        text_scalar(Protocol),
        <<",bundle=">>,
        text_scalar(Bundle)
    ].

render_text_map(Map, Indent, Order) ->
    [render_text_field(Key, maps:get(Key, Map), Indent) || Key <- text_map_keys(Map, Order)].

render_text_field(Key, Value, Indent) when is_map(Value), map_size(Value) =:= 0 ->
    [text_indent(Indent), text_scalar(Key), <<": {}\n">>];
render_text_field(Key, Value, Indent) when is_map(Value) ->
    [
        text_indent(Indent),
        text_scalar(Key),
        <<":\n">>,
        render_text_map(Value, Indent + 2, nested)
    ];
render_text_field(Key, [], Indent) ->
    [text_indent(Indent), text_scalar(Key), <<": []\n">>];
render_text_field(Key, Value, Indent) when is_list(Value) ->
    [text_indent(Indent), text_scalar(Key), <<":\n">>, render_text_list(Value, Indent + 2, 0)];
render_text_field(Key, Value, Indent) ->
    [text_indent(Indent), text_scalar(Key), <<": ">>, text_scalar(Value), <<"\n">>].

render_text_list([], _Indent, _Index) ->
    [];
render_text_list([Value | Rest], Indent, Index) ->
    [
        render_text_item(Value, Indent, Index),
        render_text_list(Rest, Indent, Index + 1)
    ].

render_text_item(Value, Indent, Index) when is_map(Value), map_size(Value) =:= 0 ->
    [text_indent(Indent), <<"[">>, integer_to_binary(Index), <<"]: {}\n">>];
render_text_item(Value, Indent, Index) when is_map(Value) ->
    [
        text_indent(Indent),
        <<"[">>,
        integer_to_binary(Index),
        <<"]:\n">>,
        render_text_map(Value, Indent + 2, nested)
    ];
render_text_item([], Indent, Index) ->
    [text_indent(Indent), <<"[">>, integer_to_binary(Index), <<"]: []\n">>];
render_text_item(Value, Indent, Index) when is_list(Value) ->
    [
        text_indent(Indent),
        <<"[">>,
        integer_to_binary(Index),
        <<"]:\n">>,
        render_text_list(Value, Indent + 2, 0)
    ];
render_text_item(Value, Indent, Index) ->
    [
        text_indent(Indent),
        <<"[">>,
        integer_to_binary(Index),
        <<"]: ">>,
        text_scalar(Value),
        <<"\n">>
    ].

text_map_keys(Map, root) ->
    ordered_text_keys(
        Map,
        [
            <<"target">>,
            <<"data">>,
            <<"capture">>,
            <<"issues">>,
            <<"outcome">>
        ]
    );
text_map_keys(Map, nested) ->
    ordered_text_keys(
        Map,
        [
            <<"summary">>,
            <<"id">>,
            <<"severity">>,
            <<"class">>,
            <<"status">>,
            <<"reason_code">>,
            <<"message">>,
            <<"required">>
        ]
    ).

ordered_text_keys(Map, Priority) ->
    Present = [Key || Key <- Priority, maps:is_key(Key, Map)],
    Present ++ lists:sort(maps:keys(maps:without(Priority, Map))).

text_response(Response) ->
    Data = maps:get(<<"data">>, Response, null),
    Meta = maps:get(<<"meta">>, Response, #{}),
    Target = maps:get(<<"target">>, Meta, null),
    Capture = text_capture(maps:get(<<"capture">>, Meta, null)),
    Issues = maps:get(<<"issues">>, Response, []),
    Outcome = maps:get(<<"outcome">>, Response, <<"error">>),
    maps:filter(
        fun
            (_Key, null) -> false;
            (<<"issues">>, []) -> false;
            (<<"outcome">>, <<"complete">>) -> false;
            (_Key, _Value) -> true
        end,
        #{
            <<"target">> => Target,
            <<"data">> => Data,
            <<"capture">> => Capture,
            <<"issues">> => Issues,
            <<"outcome">> => Outcome
        }
    ).

text_capture(null) ->
    null;
text_capture(Capture) ->
    Probes = [
        Probe
     || #{<<"status">> := Status} = Probe <- maps:get(<<"probes">>, Capture), Status =/= <<"ok">>
    ],
    maps:filter(
        fun
            (_Key, []) -> false;
            (_Key, _Value) -> true
        end,
        #{
            <<"duration_ms">> => maps:get(<<"duration_ms">>, Capture),
            <<"probes">> => Probes,
            <<"observer_effects">> => maps:get(<<"observer_effects">>, Capture)
        }
    ).

text_indent(Width) ->
    binary:copy(<<" ">>, Width).

text_scalar(<<>>) ->
    <<"\"\"">>;
text_scalar(Value) when is_binary(Value) ->
    escape_text(Value);
text_scalar(Value) when is_atom(Value) ->
    atom_to_binary(Value);
text_scalar(Value) when is_integer(Value) ->
    integer_to_binary(Value);
text_scalar(Value) when is_float(Value) ->
    float_to_binary(Value, [short]);
text_scalar(Value) ->
    iolist_to_binary(io_lib:format("~tp", [Value])).

-spec exit_code(atom() | map()) -> 0..4.
exit_code(#{category := Category}) ->
    exit_code(Category);
exit_code(argument) ->
    2;
exit_code(format) ->
    2;
exit_code(capability) ->
    2;
exit_code(safety_refusal) ->
    3;
exit_code(scan_budget_exceeded) ->
    3;
exit_code(controller) ->
    3;
exit_code(distribution) ->
    3;
exit_code(connection) ->
    3;
exit_code(required_probe) ->
    3;
exit_code(partial) ->
    3;
exit_code(internal) ->
    4;
exit_code(cleanup) ->
    4;
exit_code(schema) ->
    4;
exit_code(_Unknown) ->
    4.

-spec escape_text(unicode:chardata()) -> binary().
escape_text(Text) ->
    case unicode:characters_to_list(Text) of
        Codepoints when is_list(Codepoints) ->
            iolist_to_binary([escape_codepoint(Codepoint) || Codepoint <- Codepoints]);
        _Invalid ->
            Raw = raw_text(Text),
            <<"base64:", (base64:encode(Raw))/binary>>
    end.

raw_text(Text) when is_binary(Text) ->
    Text;
raw_text(Text) ->
    try iolist_to_binary(Text) of
        Binary -> Binary
    catch
        _:_ -> term_to_binary(Text)
    end.

escape_codepoint(Codepoint) when
    Codepoint < 16#20;
    Codepoint >= 16#7F, Codepoint =< 16#9F
->
    io_lib:format("\\x~2.16.0B", [Codepoint]);
escape_codepoint(Codepoint) ->
    case bidi_or_separator(Codepoint) of
        true -> io_lib:format("\\u{~.16B}", [Codepoint]);
        false -> unicode:characters_to_binary([Codepoint])
    end.

harden_term(Binary) ->
    harden_unicode(Binary, fun term_codepoint/1).

term_codepoint(Codepoint) when
    Codepoint < 16#20;
    Codepoint >= 16#7F, Codepoint =< 16#9F
->
    io_lib:format("\\x~2.16.0B", [Codepoint]);
term_codepoint(Codepoint) ->
    case bidi_or_separator(Codepoint) of
        true -> io_lib:format("\\x{~.16B}", [Codepoint]);
        false -> unicode:characters_to_binary([Codepoint])
    end.

harden_json(Binary) ->
    harden_unicode(Binary, fun json_codepoint/1).

json_codepoint(Codepoint) when Codepoint < 16#20 ->
    json_unicode_escape(Codepoint);
json_codepoint(Codepoint) when Codepoint >= 16#7F, Codepoint =< 16#9F ->
    json_unicode_escape(Codepoint);
json_codepoint(Codepoint) ->
    case bidi_or_separator(Codepoint) of
        true -> json_unicode_escape(Codepoint);
        false -> unicode:characters_to_binary([Codepoint])
    end.

json_unicode_escape(Codepoint) ->
    io_lib:format("\\u~4.16.0B", [Codepoint]).

harden_unicode(Binary, Fun) ->
    case unicode:characters_to_list(Binary) of
        Codepoints when is_list(Codepoints) ->
            iolist_to_binary([Fun(Codepoint) || Codepoint <- Codepoints]);
        _Invalid ->
            Binary
    end.

bidi_or_separator(16#061C) -> true;
bidi_or_separator(16#200E) -> true;
bidi_or_separator(16#200F) -> true;
bidi_or_separator(16#2028) -> true;
bidi_or_separator(16#2029) -> true;
bidi_or_separator(Codepoint) when Codepoint >= 16#202A, Codepoint =< 16#202E -> true;
bidi_or_separator(Codepoint) when Codepoint >= 16#2066, Codepoint =< 16#2069 -> true;
bidi_or_separator(_Codepoint) -> false.

command_binary(null) ->
    null;
command_binary(Command) when is_atom(Command) ->
    atom_to_binary(Command);
command_binary(Command) when is_binary(Command) ->
    Command.

outcome_binary(Outcome) when is_atom(Outcome) ->
    atom_to_binary(Outcome);
outcome_binary(Outcome) when is_binary(Outcome) ->
    Outcome.

reason_code({Code, _Detail}) when is_atom(Code) ->
    atom_to_binary(Code);
reason_code({Code, _Left, _Right}) when is_atom(Code) ->
    atom_to_binary(Code);
reason_code(Code) when is_atom(Code) ->
    atom_to_binary(Code);
reason_code(Code) when is_binary(Code) ->
    Code;
reason_code(_Reason) ->
    <<"unknown_error">>.

reason_message({unknown_option, Option}) ->
    iolist_to_binary([<<"unknown option: ">>, escape_text(Option)]);
reason_message({unknown_command, Command}) ->
    iolist_to_binary([<<"unknown command: ">>, escape_text(Command)]);
reason_message({duplicate_option, Option}) ->
    iolist_to_binary([<<"option may only be specified once: ">>, option_text(Option)]);
reason_message({missing_option_value, Option}) ->
    iolist_to_binary([<<"missing value for option: ">>, escape_text(Option)]);
reason_message({mutually_exclusive_options, Left, Right}) ->
    iolist_to_binary([
        option_text(Left), <<" and ">>, option_text(Right), <<" cannot be used together">>
    ]);
reason_message({unsupported_format, Format}) ->
    iolist_to_binary([<<"unsupported format: ">>, escape_text(Format)]);
reason_message({unsupported_name_mode, Mode}) ->
    iolist_to_binary([<<"unsupported name mode: ">>, escape_text(Mode)]);
reason_message(json_unavailable) ->
    <<"JSON output requires OTP 27 or newer">>;
reason_message(command_unavailable) ->
    <<"command capability is not available yet">>;
reason_message(capability_unavailable) ->
    <<"install a matching observer_cli diagnostics bundle on the target">>;
reason_message(diagnostics_missing) ->
    <<"observer_cli diagnostics are not installed on the target">>;
reason_message(diagnostics_incompatible) ->
    <<"the target observer_cli diagnostics bundle is incompatible">>;
reason_message(no_active_context) ->
    <<"no saved target context; run observer_cli connect first">>;
reason_message(global_option_before_command) ->
    <<"options must appear after the command name">>;
reason_message(missing_cookie_source) ->
    <<"--node requires exactly one of --cookie-env or --cookie-file">>;
reason_message(process_target_required) ->
    <<"process requires one PID_OR_NAME">>;
reason_message(port_target_required) ->
    <<"port requires one PORT_ID">>;
reason_message(otp_state_target_required) ->
    <<"otp-state requires one PID_OR_NAME">>;
reason_message(behavior_required) ->
    <<"otp-state requires --behavior gen_server|gen_statem|gen_event">>;
reason_message(invalid_behavior) ->
    <<"--behavior must be gen_server, gen_statem, or gen_event">>;
reason_message(application_required) ->
    <<"supervision-tree requires --app APP">>;
reason_message(observe_required) ->
    <<"--deep and --app require --observe DURATION">>;
reason_message(replace_existing_trace_required) ->
    <<"trace call requires --replace-existing-trace">>;
reason_message(trace_pid_required) ->
    <<"trace call requires --pid PID">>;
reason_message(trace_all_required) ->
    <<"trace stop requires --all">>;
reason_message(timeout_too_short) ->
    <<"--timeout must cover the sampling duration plus five seconds">>;
reason_message(trace_timeout_too_short) ->
    <<"trace call --timeout must cover the trace duration plus seven seconds">>;
reason_message(trace_stop_timeout_too_short) ->
    <<"trace stop --timeout must be at least five seconds">>;
reason_message(otp_state_timeout_too_short) ->
    <<"--timeout must be at least 10s for otp-state">>;
reason_message(unaddressable_handler_id) ->
    <<"--handler must be 1..255 safe Unicode characters and must not start with --">>;
reason_message(invalid_tail) ->
    <<"--tail must be an integer from 1 to 2000">>;
reason_message(invalid_refresh_interval) ->
    <<"REFRESH_MS must be an integer of at least 1000">>;
reason_message(connection_failed) ->
    <<"target connection failed; check node name, name mode, EPMD, network, and cookie">>;
reason_message(tui_start_failed) ->
    <<"interactive TUI startup failed; check target reachability, cookie, and bundle compatibility">>;
reason_message({remote_otp_mismatch, ControllerOtp, TargetOtp}) ->
    iolist_to_binary([
        <<"TUI auto-load requires the same OTP major release; controller OTP ">>,
        escape_text(ControllerOtp),
        <<", target OTP ">>,
        escape_text(TargetOtp)
    ]);
reason_message(Reason) when
    Reason =:= invalid_command_response;
    Reason =:= invalid_snapshot_response;
    Reason =:= invalid_diagnose_response;
    Reason =:= invalid_schema
->
    <<"target response schema is incompatible; install the same observer_cli build on the controller and target">>;
reason_message(response_too_large) ->
    <<"encoded response exceeds one MiB">>;
reason_message(Reason) when is_binary(Reason) ->
    escape_text(Reason);
reason_message(Reason) when is_atom(Reason) ->
    list_to_binary(string:replace(atom_to_list(Reason), "_", " ", all));
reason_message(Reason) ->
    iolist_to_binary(io_lib:format("~tp", [Reason])).

option_text(Option) when is_atom(Option) ->
    list_to_binary(["--", string:replace(atom_to_list(Option), "_", "-", all)]).

capped(Binary) when byte_size(Binary) =< ?MAX_RESPONSE_BYTES ->
    {ok, Binary};
capped(_Binary) ->
    {error, controller_error(schema, response_too_large)}.

encoder_error(Reason) ->
    {error, controller_error(internal, Reason)}.

controller_error(Category, Reason) ->
    #{category => Category, exit_code => exit_code(Category), reason => Reason}.
