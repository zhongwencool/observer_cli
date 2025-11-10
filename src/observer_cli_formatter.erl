-module(observer_cli_formatter).

-export([format/2]).

%% Function for formating complex data structures: proplissts, lists, maps, tuples, ...
-callback format(Term :: term()) ->
    string().

%%--------------------------------------------------------------------
%% @doc
-spec format(Formatter :: observer_cli_formatter_default | module(), Term :: term()) ->
    string().
%%--------------------------------------------------------------------
format(Formatter, Term) ->
    Formatter:format(Term).
%%--------------------------------------------------------------------
