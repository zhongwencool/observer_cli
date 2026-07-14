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

mnesia_table_churn_skips_disappeared_rows_test() ->
    Dir = filename:join(["test", "tmp", "mnesia_churn"]),
    setup_mnesia(Dir),
    lists:foreach(
        fun(_) ->
            {atomic, ok} = mnesia:create_table(vanished_table, [
                {attributes, [id, value]}, {ram_copies, [node()]}
            ]),
            ok = mnesia:wait_for_tables([vanished_table], 5000),
            {atomic, ok} = mnesia:delete_table(vanished_table)
        end,
        lists:seq(1, 20)
    ),
    Tables = mnesia:system_info(tables),
    ets:insert(mnesia_gvar, [
        {{schema, tables}, [vanished_table | Tables]},
        {{vanished_table, storage_type}, ram_copies}
    ]),
    try
        ?assertEqual(undefined, mnesia:table_info(vanished_table, memory)),
        List = observer_cli_mnesia:collect_mnesia_info(false, memory),
        ?assertEqual([], [
            Tab
         || {_, _, Tab} <- List,
            proplists:get_value(name, Tab) =:= vanished_table
        ])
    after
        ets:insert(mnesia_gvar, {{schema, tables}, Tables}),
        ets:delete(mnesia_gvar, {vanished_table, storage_type}),
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
        ok = mnesia:dirty_write({disc_table, 1, value}),
        List = observer_cli_mnesia:collect_mnesia_info(false, memory),
        [CollectedRow] = [
            Row
         || Row = {_, _, Tab} <- List,
            proplists:get_value(name, Tab) =:= disc_table
        ],
        {0, _SortValue, Tab} = CollectedRow,
        ?assertEqual(disc_only_copies, proplists:get_value(storage, Tab)),
        ?assertEqual(mnesia:table_info(disc_table, memory), proplists:get_value(memory, Tab)),
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

diagnostic_mnesia_storage_units_and_lifecycle_test() ->
    RamId = make_ref(),
    DiscId = make_ref(),
    WrongDiscId = make_ref(),
    Values = #{
        ram_table => #{storage_type => ram_copies, size => 4, memory => 10},
        disc_table => #{storage_type => disc_copies, size => 3, memory => 20},
        only_table => #{storage_type => disc_only_copies, size => 2, memory => 77},
        external_table => #{storage_type => {ext, alias, module}, size => 1, memory => 999},
        vanished_table => #{}
    },
    Source = mnesia_source(
        [ram_table, disc_table, only_table, external_table, vanished_table],
        Values,
        #{ram_table => RamId, disc_table => DiscId},
        #{RamId => RamId, DiscId => WrongDiscId}
    ),
    Response = diagnostic_mnesia(#{sort => size, limit => 10, test_mnesia_source => Source}),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(1, maps:get(<<"disappeared_count">>, Data)),
    Items = maps:get(<<"items">>, Data),
    ByName = maps:from_list([{maps:get(<<"table">>, Item), Item} || Item <- Items]),
    Ram = maps:get(<<"ram_table">>, ByName),
    Disc = maps:get(<<"disc_table">>, ByName),
    Only = maps:get(<<"only_table">>, ByName),
    External = maps:get(<<"external_table">>, ByName),
    ?assertEqual(80, maps:get(<<"memory_bytes">>, Ram)),
    ?assertEqual(null, maps:get(<<"disk_bytes">>, Ram)),
    ?assertEqual(160, maps:get(<<"memory_bytes">>, Disc)),
    ?assertEqual(<<"mnesia_main_table">>, maps:get(<<"management">>, Ram)),
    ?assertEqual(<<"management_unknown">>, maps:get(<<"management">>, Disc)),
    ?assertEqual(null, maps:get(<<"memory_bytes">>, Only)),
    ?assertEqual(77, maps:get(<<"disk_bytes">>, Only)),
    ?assertEqual(null, maps:get(<<"memory_bytes">>, External)),
    ?assertEqual(null, maps:get(<<"disk_bytes">>, External)),
    ?assertEqual(<<"external_or_unknown">>, maps:get(<<"storage_type">>, External)),
    ?assertEqual(true, maps:get(<<"storage_semantics_unavailable">>, External)).

diagnostic_mnesia_recon_top_n_tie_test() ->
    FirstId = make_ref(),
    SecondId = make_ref(),
    Values = #{
        first_table => #{storage_type => ram_copies, size => 1, memory => 10},
        second_table => #{storage_type => ram_copies, size => 1, memory => 10}
    },
    Source = mnesia_source(
        [second_table, first_table],
        Values,
        #{first_table => FirstId, second_table => SecondId},
        #{FirstId => FirstId, SecondId => SecondId}
    ),
    Data = maps:get(
        <<"data">>,
        diagnostic_mnesia(#{sort => memory, limit => 2, test_mnesia_source => Source})
    ),
    Expected = [
        list_to_binary(ref_to_list(Id))
     || {_, _, Id} <- recon_lib:sublist_top_n_attrs(
            [{0, 80, FirstId}, {0, 80, SecondId}], 2
        )
    ],
    ?assertEqual(Expected, [
        maps:get(<<"ets_table_id">>, Item)
     || Item <- maps:get(<<"items">>, Data)
    ]).

diagnostic_mnesia_post_enumeration_admission_test() ->
    Parent = self(),
    Base = mnesia_source(lists:seq(1, 10001), #{}, #{}, #{}),
    Source = Base#{
        info_fun => fun(_Table, _Key) ->
            Parent ! mnesia_info_called,
            undefined
        end
    },
    Response = diagnostic_mnesia(#{test_mnesia_source => Source}),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)),
    ?assertEqual(<<"post_enumeration">>, maps:get(<<"admission_stage">>, Data)),
    receive
        mnesia_info_called -> ?assert(false)
    after 25 -> ok
    end.

diagnostic_mnesia_distinct_states_test() ->
    NotRunning = (mnesia_source([], #{}, #{}, #{}))#{running_fun := fun() -> no end},
    NotRunningData = maps:get(
        <<"data">>, diagnostic_mnesia(#{test_mnesia_source => NotRunning})
    ),
    ?assertEqual(<<"not_running">>, maps:get(<<"status">>, NotRunningData)),
    EmptyData = maps:get(
        <<"data">>, diagnostic_mnesia(#{test_mnesia_source => mnesia_source([], #{}, #{}, #{})})
    ),
    ?assertEqual(<<"empty">>, maps:get(<<"status">>, EmptyData)),
    Unavailable = (mnesia_source([], #{}, #{}, #{}))#{available_fun := fun() -> false end},
    UnavailableData = maps:get(
        <<"data">>, diagnostic_mnesia(#{test_mnesia_source => Unavailable})
    ),
    ?assertEqual(<<"unavailable">>, maps:get(<<"status">>, UnavailableData)),
    Error = (mnesia_source([], #{}, #{}, #{}))#{
        running_fun := fun() -> erlang:error(mnesia_fixture_error) end
    },
    ?assertEqual(
        <<"probe_failed">>,
        maps:get(<<"reason_code">>, diagnostic_mnesia_error(#{test_mnesia_source => Error}))
    ).

mnesia_source(Tables, Values, Whereis, EtsIds) ->
    #{
        available_fun => fun() -> true end,
        running_fun => fun() -> yes end,
        local_tables_fun => fun() -> Tables end,
        info_fun => fun(Table, Key) -> maps:get(Key, maps:get(Table, Values, #{}), undefined) end,
        whereis_fun => fun(Table) -> maps:get(Table, Whereis, undefined) end,
        ets_info_fun => fun(Tid, id) -> maps:get(Tid, EtsIds, undefined) end,
        word_size_fun => fun() -> 8 end
    }.

diagnostic_mnesia(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), mnesia, Request, #{timeout_ms => 3000, identifier_policy => include}
        ),
    Response.

diagnostic_mnesia_error(Request) ->
    observer_cli_snapshot:dispatch(
        self(), mnesia, Request, #{timeout_ms => 3000, identifier_policy => include}
    ).

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
