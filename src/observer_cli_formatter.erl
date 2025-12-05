-module(observer_cli_formatter).

-export([format/3]).

%% Function for formating complex data structures: proplissts, lists, maps, tuples, ...
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
