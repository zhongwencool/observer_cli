-module(observer_cli_test_handler).

-export([start/3]).

start(_Type, _Item, _Opts) ->
    quit.
