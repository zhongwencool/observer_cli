-module(observer_cli_cli).

-include_lib("kernel/include/file.hrl").

-export([
    parse/1,
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
    envelope/6,
    error/2,
    encode/2,
    exit_code/1,
    escape_text/1
]).

-ifdef(TEST).
-export([context_path/0, write_context/2, read_context/1, decode_context/1, delete_context/1]).
-endif.

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_NODE_LENGTH, 255).
-define(MAX_COOKIE_LENGTH, 255).
-define(MAX_CONTEXT_BYTES, 8192).
-define(SCHEMA, <<"observer_cli.cli/v1">>).

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
parse([[$-, $- | _] | _]) ->
    argument_error(global_option_before_command);
parse(["tui" | Arguments]) ->
    parse_tui(Arguments);
parse([First | _] = Arguments) ->
    case command(First) of
        undefined -> parse_tui(Arguments);
        Command -> parse_command(Command, tl(Arguments))
    end.

parse_tui([Target]) ->
    {ok, #{route => tui, target => Target, cookie => undefined, interval => 1500}};
parse_tui([Target, Cookie, IntervalText]) ->
    try list_to_integer(IntervalText) of
        Interval ->
            {ok, #{route => tui, target => Target, cookie => Cookie, interval => Interval}}
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
        {ok, true, [all]} -> ok;
        {ok, false, _} -> {error, trace_all_required};
        {ok, true, _} -> {error, unsupported_command_option}
    end;
validate_trace_command(Arguments, _Options) ->
    validate_arguments(trace, Arguments).

trace_mode_keys(Options) ->
    Global = [node, cookie_env, cookie_file, name_mode, format, json, timeout],
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
    case duration(Options) of
        {ok, Duration} -> validate_scheduler_timeout(Options, Duration);
        {error, Reason} -> {error, Reason}
    end;
validate_runtime_options(snapshot, Options) ->
    case only_options(Options, [deep]) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(diagnose, Options) ->
    case only_options(Options, [observe, deep, app]) of
        true -> validate_diagnose_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(distribution, #{limit := Text} = Options) ->
    case positive_integer(Text) of
        Limit when is_integer(Limit), Limit =< 200 -> validate_target_options(Options);
        _ -> {error, invalid_limit}
    end;
validate_runtime_options(processes, Options) ->
    case only_options(Options, [sort, limit, duration]) of
        true -> validate_processes_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(applications, Options) ->
    case only_options(Options, [sort, limit]) of
        true ->
            validate_list_options(
                Options, ["memory", "process_count", "reductions", "message_queue_len"]
            );
        false ->
            {error, unsupported_command_option}
    end;
validate_runtime_options(Command, Options) when Command =:= ets; Command =:= mnesia ->
    case only_options(Options, [sort, limit]) of
        true -> validate_list_options(Options, ["memory", "size"]);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(network, Options) ->
    validate_counter_list_options(Options, ["oct", "recv_oct", "send_oct"]);
validate_runtime_options(ports, Options) ->
    case only_options(Options, [sort, limit]) of
        true -> validate_list_options(Options, ["queue_size", "memory", "input", "output", "io"]);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(sockets, Options) ->
    validate_counter_list_options(
        Options, ["io", "read_bytes", "write_bytes", "packets", "waits", "fails"]
    );
validate_runtime_options(process, Options) ->
    case only_options(Options, [info]) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(gen_server_state, Options) ->
    case only_options(Options, []) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(supervision_tree, #{app := App} = Options) ->
    case only_options(Options, [app]) andalso valid_application_name(App) of
        true -> validate_target_options(Options);
        false -> {error, unsupported_command_option}
    end;
validate_runtime_options(supervision_tree, _Options) ->
    {error, application_required};
validate_runtime_options(trace, Options) ->
    validate_trace_options(Options);
validate_runtime_options(_Command, #{deep := true}) ->
    {error, unsupported_command_option};
validate_runtime_options(_Command, Options) ->
    validate_target_options(Options).

validate_trace_options(Options) ->
    case only_options(Options, [pid, limit, rate, duration, replace_existing_trace, all]) of
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
        {ok, Timeout} when Timeout >= Duration + 5000 -> validate_target_options(Options);
        {ok, _} -> {error, timeout_too_short};
        Error -> Error
    end;
validate_trace_timeout(Options, _Duration) ->
    validate_target_options(Options).

validate_counter_list_options(Options, Sorts) ->
    case only_options(Options, [sort, limit, duration]) of
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

only_options(Options, CommandOptions) ->
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
    lists:all(fun(Key) -> lists:member(Key, Global ++ CommandOptions) end, maps:keys(Options)).

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
                    case {maps:get(sort, Options, "memory"), duration(Options)} of
                        {"reductions", {ok, Duration}} ->
                            validate_scheduler_timeout(Options, Duration);
                        {"reductions", {error, Reason}} ->
                            {error, Reason};
                        {_Sort, _Duration} ->
                            {error, duration_requires_reductions_sort}
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

validate_arguments(process, [_Target]) ->
    ok;
validate_arguments(process, _Arguments) ->
    {error, process_target_required};
validate_arguments(gen_server_state, [_Target]) ->
    ok;
validate_arguments(gen_server_state, _Arguments) ->
    {error, gen_server_target_required};
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
validate_arguments(_Command, _Arguments) ->
    ok.

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
        {ok, Duration} -> {ok, max(10000, Duration + 5000)};
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
    case {Result, Close} of
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
    try list_to_integer(Text) of
        Value when Value > 0 -> Value;
        _ -> error
    catch
        error:badarg -> error
    end.

positive_integer_or_zero(Text) ->
    try list_to_integer(Text) of
        Value when Value >= 0 -> Value;
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
option([$-, $- | _]) -> unknown;
option(_Argument) -> positional.

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
command("sockets") -> sockets;
command("gen-server-state") -> gen_server_state;
command("supervision-tree") -> supervision_tree;
command("trace") -> trace;
command("diagnose") -> diagnose;
command(_Argument) -> undefined.

argument_error(Reason) ->
    {error, #{category => argument, exit_code => 2, reason => Reason}}.

-spec envelope(atom() | binary(), null | map(), null | map(), null | map(), [term()], [term()]) ->
    map().
envelope(Command, Target, Capture, Data, Warnings, Errors) ->
    #{
        <<"schema">> => ?SCHEMA,
        <<"command">> => command_binary(Command),
        <<"target">> => Target,
        <<"capture">> => Capture,
        <<"data">> => Data,
        <<"warnings">> => Warnings,
        <<"errors">> => Errors
    }.

-spec error(atom(), term()) -> map().
error(Category, Reason) ->
    #{
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
        <<"diagnostics_module">> := DiagnosticsModule
    }
}) when Command =:= <<"connect">>; Command =:= <<"status">> ->
    Prefix =
        case Command of
            <<"connect">> -> <<"Selected ">>;
            <<"status">> -> <<"Active ">>
        end,
    capped(
        iolist_to_binary([
            Prefix,
            escape_text(Node),
            <<"; probe succeeded.\ndiagnostics_module=">>,
            DiagnosticsModule,
            <<"\nNo persistent connection is kept.\n">>
        ])
    );
encode(text, #{
    <<"command">> := <<"disconnect">>,
    <<"data">> := #{<<"node">> := null, <<"disconnected">> := true}
}) ->
    {ok, <<"No active context.\n">>};
encode(text, #{
    <<"command">> := <<"disconnect">>,
    <<"data">> := #{<<"node">> := Node, <<"disconnected">> := true}
}) ->
    capped(iolist_to_binary([<<"Disconnected ">>, escape_text(Node), <<".\n">>]));
encode(text, Response) ->
    capped(iolist_to_binary(io_lib:format("~tp~n", [Response])));
encode(term, Response) ->
    capped(iolist_to_binary(io_lib:format("~tp.~n", [Response])));
encode(json, Response) ->
    case code:ensure_loaded(json) of
        {module, json} ->
            try capped(iolist_to_binary(erlang:apply(json, encode, [Response]))) of
                Result -> Result
            catch
                _:_ -> encoder_error(json_encoding_failed)
            end;
        {error, _Reason} ->
            {error, controller_error(capability, json_unavailable)}
    end;
encode(_Format, _Response) ->
    {error, controller_error(format, unsupported_format)}.

-spec exit_code(atom() | map()) -> 0..4.
exit_code(#{category := Category}) ->
    exit_code(Category);
exit_code(success) ->
    0;
exit_code(diagnose_findings) ->
    1;
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
    case unicode:characters_to_binary(Text) of
        Binary when is_binary(Binary) ->
            iolist_to_binary([escape_byte(Byte) || <<Byte>> <= Binary]);
        {_Error, Valid, Rest} ->
            Raw = iolist_to_binary([Valid, Rest]),
            <<"base64:", (base64:encode(Raw))/binary>>
    end.

command_binary(Command) when is_atom(Command) ->
    atom_to_binary(Command);
command_binary(Command) when is_binary(Command) ->
    Command.

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
reason_message({unsupported_format, Format}) ->
    iolist_to_binary([<<"unsupported format: ">>, escape_text(Format)]);
reason_message(json_unavailable) ->
    <<"JSON output requires OTP 27 or newer">>;
reason_message(command_unavailable) ->
    <<"command capability is not available yet">>;
reason_message(response_too_large) ->
    <<"encoded response exceeds one MiB">>;
reason_message(Reason) when is_binary(Reason) ->
    escape_text(Reason);
reason_message(Reason) ->
    iolist_to_binary(io_lib:format("~tp", [Reason])).

capped(Binary) when byte_size(Binary) =< ?MAX_RESPONSE_BYTES ->
    {ok, Binary};
capped(_Binary) ->
    {error, controller_error(schema, response_too_large)}.

encoder_error(Reason) ->
    {error, controller_error(internal, Reason)}.

controller_error(Category, Reason) ->
    #{category => Category, exit_code => exit_code(Category), reason => Reason}.

escape_byte(Byte) when Byte < 16#20; Byte >= 16#7F, Byte =< 16#9F ->
    io_lib:format("\\x~2.16.0B", [Byte]);
escape_byte(Byte) ->
    Byte.
