-module(observer_cli_store_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

lookup_empty_test() ->
    StorePid = observer_cli_store:start(),
    ?assertEqual({error, undefined}, observer_cli_store:lookup_pos(StorePid, 1)),
    ?assertEqual(1, observer_cli_store:lookup_row(StorePid)),
    erlang:unlink(StorePid),
    erlang:exit(StorePid, kill).

update_and_lookup_test() ->
    StorePid = observer_cli_store:start(),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    observer_cli_store:update(StorePid, 2, [{1, Pid1}, {2, Pid2}]),
    ?assertEqual(2, observer_cli_store:lookup_row(StorePid)),
    ?assertEqual({1, Pid1}, observer_cli_store:lookup_pos(StorePid, 1)),
    ?assertEqual({2, Pid2}, observer_cli_store:lookup_pos(StorePid, 9)),
    erlang:unlink(StorePid),
    erlang:exit(StorePid, kill),
    erlang:exit(Pid1, kill),
    erlang:exit(Pid2, kill).

-endif.
