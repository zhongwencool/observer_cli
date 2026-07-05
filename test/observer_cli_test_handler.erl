-module(observer_cli_test_handler).

-export([start/3]).

start(Type, Item, Opts) ->
    case get(observer_cli_test_handler_parent) of
        undefined -> ok;
        Parent -> Parent ! {plugin_handler, Type, Item, Opts}
    end,
    quit.
