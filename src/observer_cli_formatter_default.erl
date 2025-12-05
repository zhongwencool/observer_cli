-module(observer_cli_formatter_default).

-behaviour(observer_cli_formatter).

-export([
    format/2
]).

%%--------------------------------------------------------------------
-spec format(Pid :: pid(), Term :: term()) ->
    string().
%%--------------------------------------------------------------------
format(Pid, Term) ->
    [_ | _] = unicode:characters_to_list(
        io_lib:format("Process: ~p~n~n~p~n", [Pid, Term])
    ).
%%--------------------------------------------------------------------
