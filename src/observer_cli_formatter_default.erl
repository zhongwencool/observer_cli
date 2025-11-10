-module(observer_cli_formatter_default).

-behaviour(observer_cli_formatter).

-export([
    format/1
]).

%%--------------------------------------------------------------------
-spec format(Term :: term()) ->
    string().
%%--------------------------------------------------------------------
format(Term) ->
    [_ | _] = unicode:characters_to_list(io_lib:format("~p\n", [Term])).
%%--------------------------------------------------------------------
