-module(observer_cli_cli).

-export([parse/1, envelope/6, error/2, encode/2, exit_code/1, escape_text/1]).

-define(MAX_RESPONSE_BYTES, 1024 * 1024).
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
validate_format_options(_Options) ->
    ok.

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
