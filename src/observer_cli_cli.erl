-module(observer_cli_cli).

-export([parse/1]).

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
