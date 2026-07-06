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

start_redraw_test() ->
    observer_cli_test_io:with_input(
        [{sleep, 30, "q\n"}],
        fun() ->
            Opts = #view_opts{auto_row = false, ets = #ets{interval = 1}},
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

collect_ets_info_test() ->
    TabName = collect_ets_table,
    ets:new(TabName, [named_table, public, set]),
    try
        Collected = observer_cli_ets:collect_ets_info(size),
        ?assert(
            lists:any(
                fun({_, _, Info}) -> proplists:get_value(name, Info) =:= TabName end,
                Collected
            )
        )
    after
        ets:delete(TabName)
    end.

collect_ets_render_info_test() ->
    TabName = collect_ets_render_table,
    ets:new(TabName, [named_table, public, set]),
    try
        RowsToRender = erlang:length(ets:all()),
        {Start, Rows} = observer_cli_ets:collect_ets_render_info(RowsToRender, 1, size),
        ?assertEqual(1, Start),
        [CollectedRow | _] = [
            Row
         || Row = {_, _, Info} <- Rows,
            proplists:get_value(name, Info) =:= TabName
        ],
        {0, SortValue, Info} = CollectedRow,
        ?assertEqual(proplists:get_value(size, Info), SortValue),
        ?assert(
            lists:all(
                fun(Key) -> proplists:is_defined(Key, Info) end,
                [
                    name,
                    size,
                    memory,
                    type,
                    protection,
                    keypos,
                    write_concurrency,
                    read_concurrency,
                    owner
                ]
            )
        )
    after
        ets:delete(TabName)
    end.

render_ets_info_preserves_sorted_page_test() ->
    Small = ets_fixture(small_ets_table, 1, 10),
    Big = ets_fixture(big_ets_table, 4, 20),
    [_Title, Row] = observer_cli_ets:render_ets_info([Small, Big], 1, 1, size),
    Text = lists:flatten(Row),
    ?assert(string:find(Text, "big_ets_table") =/= nomatch),
    ?assertEqual(nomatch, string:find(Text, "small_ets_table")).

render_ets_info_wide_layout_test() ->
    TabName = wide_layout_ets_table,
    ets:new(TabName, [named_table, public, set]),
    try
        Base = ets_row_widths(80),
        Wide = ets_row_widths(180),
        ?assertEqual([4, 5, 6, 7], unchanged_columns(Base, Wide, [4, 5, 6, 7])),
        ?assertEqual([1, 2, 3, 8], wider_columns(Base, Wide, [1, 2, 3, 8]))
    after
        ets:delete(TabName)
    end.

ets_row_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Row | _] = observer_cli_ets:render_ets_info(20, 1, size),
            {observer_cli_test_io:column_widths(Title), observer_cli_test_io:column_widths(Row)}
        end
    ).

ets_fixture(Name, Size, Memory) ->
    {
        0,
        Size,
        [
            {name, Name},
            {write_concurrency, false},
            {read_concurrency, false},
            {memory, Memory},
            {owner, self()},
            {size, Size},
            {type, set},
            {keypos, 1},
            {protection, public}
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

-endif.
