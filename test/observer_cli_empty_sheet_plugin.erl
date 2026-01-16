-module(observer_cli_empty_sheet_plugin).

-export([sheet_header/0, sheet_body/1]).

sheet_header() ->
    [].

sheet_body(Prev) ->
    {[], Prev}.
