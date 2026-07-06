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

start_render_worker_messages_test() ->
    Dir = filename:join(["test", "tmp", "mnesia_worker"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table], 5000),
        Inputs = ["x\n", {sleep, 30, "2000\n"}, {sleep, 30, "hide\n"}, {sleep, 30, "q\n"}],
        observer_cli_test_io:with_input(
            Inputs,
            fun() ->
                Opts = #view_opts{auto_row = false, db = #db{interval = 1}},
                ?assertEqual(quit, observer_cli_mnesia:start(Opts))
            end
        )
    after
        cleanup_mnesia(Dir)
    end.

collect_mnesia_info_error_test() ->
    mnesia:stop(),
    ?assertMatch({error, _}, observer_cli_mnesia:collect_mnesia_info(false, memory)).

collect_mnesia_render_info_error_test() ->
    mnesia:stop(),
    ?assertMatch({error, _}, observer_cli_mnesia:collect_mnesia_render_info(false, memory, 10, 1)).

collect_mnesia_info_running_test() ->
    Dir = filename:join(["test", "tmp", "mnesia"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table], 5000),
        List = observer_cli_mnesia:collect_mnesia_info(true, memory),
        [CollectedRow] = [
            Row
         || Row = {_, _, Tab} <- List,
            proplists:get_value(name, Tab) =:= test_table
        ],
        {0, SortValue, Tab} = CollectedRow,
        ?assertEqual(proplists:get_value(memory, Tab), SortValue),
        ?assertEqual(test_table, proplists:get_value(name, Tab)),
        ?assertEqual(ram_copies, proplists:get_value(storage, Tab)),
        ?assert(is_pid(proplists:get_value(owner, Tab))),
        ?assert(
            lists:all(
                fun(Key) -> proplists:is_defined(Key, Tab) end,
                [name, owner, size, reg_name, type, memory, index, fixed, compressed, storage]
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

collect_mnesia_info_disc_only_table_test() ->
    Dir = filename:join(["test", "tmp", "mnesia_disc_only"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(disc_table, [
                {attributes, [id, value]}, {disc_only_copies, [node()]}
            ]),
        ok = mnesia:wait_for_tables([disc_table], 5000),
        List = observer_cli_mnesia:collect_mnesia_info(false, memory),
        [CollectedRow] = [
            Row
         || Row = {_, _, Tab} <- List,
            proplists:get_value(name, Tab) =:= disc_table
        ],
        {0, _SortValue, Tab} = CollectedRow,
        ?assertEqual(disc_only_copies, proplists:get_value(storage, Tab)),
        ?assert(proplists:is_defined(fixed, Tab))
    after
        cleanup_mnesia(Dir)
    end.

collect_mnesia_render_info_hides_system_tables_test() ->
    Dir = filename:join(["test", "tmp", "mnesia_hidden"]),
    setup_mnesia(Dir),
    try
        {atomic, ok} =
            mnesia:create_table(test_table, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        {atomic, ok} =
            mnesia:create_table(user, [{attributes, [id, value]}, {ram_copies, [node()]}]),
        ok = mnesia:wait_for_tables([test_table, user], 5000),
        {1, HiddenRows} = observer_cli_mnesia:collect_mnesia_render_info(true, memory, 1000, 1),
        {1, VisibleRows} = observer_cli_mnesia:collect_mnesia_render_info(false, memory, 1000, 1),
        HiddenNames = mnesia_row_names(HiddenRows),
        VisibleNames = mnesia_row_names(VisibleRows),
        ?assert(lists:member(test_table, HiddenNames)),
        ?assertEqual(false, lists:member(user, HiddenNames)),
        ?assert(lists:member(user, VisibleNames)),
        ?assertEqual(false, lists:member(schema, VisibleNames))
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

render_mnesia_preserves_sorted_page_test() ->
    Small = mnesia_fixture(small_mnesia_table, 1024, 1),
    Big = mnesia_fixture(big_mnesia_table, 2048, 4),
    [_Title, Row] = observer_cli_mnesia:render_mnesia([Small, Big], size, 1, 1),
    Text = lists:flatten(Row),
    ?assert(string:find(Text, "big_mnesia_table") =/= nomatch),
    ?assertEqual(nomatch, string:find(Text, "small_mnesia_table")).

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
    mnesia_fixture(very_long_mnesia_table_name_for_layout, 1024, 1).

mnesia_fixture(Name, Memory, Size) ->
    {
        0,
        Size,
        [
            {name, Name},
            {memory, Memory},
            {size, Size},
            {type, set},
            {storage, ram_copies},
            {owner, self()},
            {index, []},
            {reg_name, very_long_registered_name_for_layout}
        ]
    }.

mnesia_row_names(Rows) ->
    [proplists:get_value(name, Tab) || {_, _, Tab} <- Rows].

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
    try
        mnesia:delete_schema([node()])
    catch
        _:_ -> ok
    end,
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    application:set_env(mnesia, dir, Dir),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start().

cleanup_mnesia(Dir) ->
    mnesia:stop(),
    try
        mnesia:delete_schema([node()])
    catch
        _:_ -> ok
    end,
    _ = file:del_dir_r(Dir),
    ok.

-endif.
