-module(observer_cli_mnesia_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_error_test() ->
    mnesia:stop(),
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_mnesia:start(Opts))
        end
    ).

start_manager_branches_test() ->
    Dir = filename:join(["test", "tmp", "mnesia_start"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table], 5000),
        Inputs = ["hide\n", "s\n", "m\n", "2000\n", "pd\n", "pu\n", "x\n", "q\n"],
        observer_cli_test_io:with_input(
            Inputs,
            fun() ->
                Opts = #view_opts{auto_row = false},
                ?assertEqual(quit, observer_cli_mnesia:start(Opts))
            end
        )
    after
        cleanup_mnesia(Dir)
    end.

get_table_list_error_test() ->
    mnesia:stop(),
    ?assertMatch({error, _}, observer_cli_mnesia:get_table_list(false, memory)).

get_table_list_running_test() ->
    Dir = filename:join(["test", "tmp", "mnesia"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table], 5000),
        List = observer_cli_mnesia:get_table_list(true, memory),
        ?assert(
            lists:any(
                fun({_, _, Tab}) -> proplists:get_value(name, Tab) =:= test_table end,
                List
            )
        ),
        Tab0 = [{name, test_table}],
        Tab1 = observer_cli_mnesia:with_storage_type(test_table, ram_copies, Tab0),
        ?assertEqual(ram_copies, proplists:get_value(storage, Tab1)),
        Tab2 = observer_cli_mnesia:with_storage_type(test_table, ext, Tab0),
        ?assertEqual("ext", lists:flatten(proplists:get_value(storage, Tab2)))
    after
        cleanup_mnesia(Dir)
    end.

setup_mnesia(Dir) ->
    mnesia:stop(),
    catch mnesia:delete_schema([node()]),
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    application:set_env(mnesia, dir, Dir),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().

cleanup_mnesia(Dir) ->
    mnesia:stop(),
    catch mnesia:delete_schema([node()]),
    _ = file:del_dir_r(Dir),
    ok.

-endif.
