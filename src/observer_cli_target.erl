%% One invocation's atomic target selector; no persisted active target.
-module(observer_cli_target).
-export([resolve/1]).

-spec resolve(map()) -> {ok, map()} | {error, atom()}.
resolve(#{node := _} = Options) ->
    validate(Options);
resolve(Options) ->
    case lists:any(fun(K) -> maps:is_key(K, Options) end, [cookie_env, cookie_file, name_mode]) of
        true -> {error, target_option_requires_node};
        false -> environment(Options)
    end.

environment(Options) ->
    case os:getenv("OBSERVER_CLI_NODE") of
        false ->
            {error, missing_target};
        Node ->
            case {os:getenv("OBSERVER_CLI_COOKIE"), os:getenv("OBSERVER_CLI_COOKIE_FILE")} of
                {false, false} ->
                    {error, missing_cookie_source};
                {_, false} ->
                    environment_mode(Options#{node => Node, cookie_env => "OBSERVER_CLI_COOKIE"});
                {false, File} ->
                    environment_mode(Options#{node => Node, cookie_file => File});
                {_, _} ->
                    {error, conflicting_cookie_sources}
            end
    end.

environment_mode(Options) ->
    case os:getenv("OBSERVER_CLI_NAME_MODE") of
        false -> validate(Options);
        Mode -> validate(Options#{name_mode => Mode})
    end.

validate(#{name_mode := Mode}) when Mode =/= "short", Mode =/= "long" -> {error, invalid_name_mode};
validate(Options) ->
    case {maps:is_key(cookie_env, Options), maps:is_key(cookie_file, Options)} of
        {true, true} ->
            {error, conflicting_cookie_sources};
        {false, false} ->
            {error, missing_cookie_source};
        _ ->
            case observer_cli_capture:target(Options) of
                {ok, {Node, Mode}} ->
                    ModeText =
                        case Mode of
                            shortnames -> "short";
                            longnames -> "long"
                        end,
                    {ok, absolute_cookie_file(Options#{node => Node, name_mode => ModeText})};
                Error ->
                    Error
            end
    end.

absolute_cookie_file(#{cookie_file := Path} = Options) ->
    Options#{cookie_file := filename:absname(Path)};
absolute_cookie_file(Options) ->
    Options.
