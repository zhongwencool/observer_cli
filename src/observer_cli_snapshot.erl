-module(observer_cli_snapshot).

-export([capabilities/0]).

-define(PROTOCOL_VERSION, 1).

-spec capabilities() -> #{protocol_version := pos_integer()}.
capabilities() ->
    #{protocol_version => ?PROTOCOL_VERSION}.
