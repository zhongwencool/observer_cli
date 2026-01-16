-module(observer_cli_formatter).

-export([format/3]).

%% Function for formatting complex data structures: proplists, lists, maps, tuples, ...
-callback format(Pid :: pid(), Term :: term()) ->
    string().

%%--------------------------------------------------------------------
%% @doc
-spec format(
    Formatter :: observer_cli_formatter_default | module(),
    Pid :: pid(),
    Term :: term()
) ->
    string().
%%--------------------------------------------------------------------
format(Formatter, Pid, Term) ->
    Formatter:format(Pid, Term).
%%--------------------------------------------------------------------
