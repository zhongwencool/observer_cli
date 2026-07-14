-module(observer_cli_formatter_default).

-behaviour(observer_cli_formatter).

-export([
    format/2
]).

-define(FORMAT_DEPTH, 32).
-define(FORMAT_CHARS_LIMIT, 64 * 1024).

%%--------------------------------------------------------------------
-spec format(Pid :: pid(), Term :: term()) ->
    string().
%%--------------------------------------------------------------------
format(Pid, Term) ->
    [_ | _] = unicode:characters_to_list(
        io_lib:format(
            "Process: ~p~n~n~P~n",
            [Pid, Term, ?FORMAT_DEPTH],
            [{chars_limit, ?FORMAT_CHARS_LIMIT}]
        )
    ).
%%--------------------------------------------------------------------
