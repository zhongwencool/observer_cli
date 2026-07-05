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

collect_mnesia_info_error_test() ->
    mnesia:stop(),
    ?assertMatch({error, _}, observer_cli_mnesia:collect_mnesia_info(false, memory)).

collect_mnesia_info_running_test() ->
    Dir = filename:join(["test", "tmp", "mnesia"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table], 5000),
        List = observer_cli_mnesia:collect_mnesia_info(true, memory),
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

render_mnesia_wide_layout_test() ->
    Base = mnesia_row_widths(80),
    Wide = mnesia_row_widths(180),
    ?assertEqual([2, 3, 4, 7], unchanged_columns(Base, Wide, [2, 3, 4, 7])),
    ?assertEqual([1, 5, 6, 8], wider_columns(Base, Wide, [1, 5, 6, 8])).

render_mnesia_size_sort_header_test() ->
    [Title, _Row] = observer_cli_mnesia:render_mnesia([mnesia_fixture()], size, 10, 1),
    Text = lists:flatten(Title),
    ?assert(string:find(Text, "Size") =/= nomatch).

mnesia_row_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Row] = observer_cli_mnesia:render_mnesia(
                [mnesia_fixture()], memory, 10, 1
            ),
            {observer_cli_test_io:column_widths(Title), observer_cli_test_io:column_widths(Row)}
        end
    ).

mnesia_fixture() ->
    {
        0,
        0,
        [
            {name, very_long_mnesia_table_name_for_layout},
            {memory, 1024},
            {size, 1},
            {type, set},
            {storage, ram_copies},
            {owner, self()},
            {index, []},
            {reg_name, very_long_registered_name_for_layout}
        ]
    }.

unchanged_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, BaseTitle) =:= lists:nth(Pos, WideTitle),
        lists:nth(Pos, BaseRow) =:= lists:nth(Pos, WideRow)
    ].

wider_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, WideTitle) > lists:nth(Pos, BaseTitle),
        lists:nth(Pos, WideRow) > lists:nth(Pos, BaseRow)
    ].

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
