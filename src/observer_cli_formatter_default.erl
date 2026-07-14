-module(observer_cli_formatter_default).

-behaviour(observer_cli_formatter).

-export([
    format/2
]).

-define(FORMAT_CHARS_LIMIT, 64 * 1024).

%%--------------------------------------------------------------------
-spec format(Pid :: pid(), Term :: term()) ->
    string().
%%--------------------------------------------------------------------
format(Pid, Term) ->
    [_ | _] = unicode:characters_to_list(
        io_lib:format(
            "Process: ~p~n~n~p~n",
            [Pid, Term],
            [{chars_limit, ?FORMAT_CHARS_LIMIT}]
        )
    ).
%%--------------------------------------------------------------------
