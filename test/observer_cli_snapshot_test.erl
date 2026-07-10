-module(observer_cli_snapshot_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

capabilities_test() ->
    ?assertEqual(#{protocol_version => 1}, observer_cli_snapshot:capabilities()).

-endif.
