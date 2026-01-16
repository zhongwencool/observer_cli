-module(observer_cli_ets_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_ets:start(Opts))
        end
    ).

start_manager_branches_test() ->
    Inputs = ["s\n", "m\n", "2000\n", "pd\n", "pu\n", "x\n", "q\n"],
    observer_cli_test_io:with_input(
        Inputs,
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_ets:start(Opts))
        end
    ).

get_ets_info_existing_test() ->
    TabName = test_ets_table,
    ets:new(TabName, [named_table, public, set]),
    try
        {_, Size, Info} = observer_cli_ets:get_ets_info(TabName, size),
        ?assertEqual(0, Size),
        ?assertEqual(TabName, proplists:get_value(name, Info))
    after
        ets:delete(TabName)
    end.

get_ets_info_missing_test() ->
    {_, _, Info} = observer_cli_ets:get_ets_info(nonexistent_table, size),
    ?assertEqual(unread, proplists:get_value(name, Info)).

is_reg_test() ->
    register(test_owner, self()),
    ?assertEqual(test_owner, observer_cli_ets:is_reg(self())),
    unregister(test_owner).

unread_test() ->
    {_, _, Info} = observer_cli_ets:unread(),
    ?assertEqual(unread, proplists:get_value(name, Info)).

-endif.
