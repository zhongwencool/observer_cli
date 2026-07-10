-module(observer_cli_cli).

-include_lib("kernel/include/file.hrl").

-export([
    parse/1,
    target/1,
    cookie_source/1,
    timeout/1,
    envelope/6,
    error/2,
    encode/2,
    exit_code/1,
    escape_text/1
]).

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_NODE_LENGTH, 255).
-define(MAX_COOKIE_LENGTH, 255).
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
    case validate_options(Options) of
        ok ->
            {ok, #{
                route => command,
                command => Command,
                arguments => lists:reverse(Positionals),
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

validate_options(Options) ->
    validate_exclusive_options(
        Options,
        [
            {cookie_env, cookie_file},
            {redact, include_identifiers},
            {limit, rate}
        ]
    ).

validate_exclusive_options(Options, [{Left, Right} | Rest]) ->
    case maps:is_key(Left, Options) andalso maps:is_key(Right, Options) of
        true ->
            {error, {mutually_exclusive_options, Left, Right}};
        false ->
            validate_exclusive_options(Options, Rest)
    end;
validate_exclusive_options(Options, []) ->
    validate_format_options(Options).

validate_format_options(#{json := true, format := Format}) when Format =/= "json" ->
    {error, {mutually_exclusive_options, json, format}};
validate_format_options(#{format := Format}) when
    Format =/= "text", Format =/= "json", Format =/= "term"
->
    {error, {unsupported_format, Format}};
validate_format_options(Options) ->
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
            case inet:gethostname() of
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

-spec timeout(map()) -> {ok, pos_integer()} | {error, atom()}.
timeout(#{timeout := Text}) ->
    case duration_ms(Text) of
        Milliseconds when is_integer(Milliseconds), Milliseconds > 0, Milliseconds =< 120000 ->
            {ok, Milliseconds};
        _ ->
            {error, invalid_timeout}
    end;
timeout(_Options) ->
    {ok, 10000}.

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
