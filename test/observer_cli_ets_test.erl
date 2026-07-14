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

get_ets_info_invalid_identifier_test() ->
    {_, _, Info} = observer_cli_ets:get_ets_info({invalid, table}, size),
    ?assertEqual(unread, proplists:get_value(name, Info)).

is_reg_test() ->
    register(test_owner, self()),
    ?assertEqual(test_owner, observer_cli_ets:is_reg(self())),
    unregister(test_owner).

unread_test() ->
    {_, _, Info} = observer_cli_ets:unread(),
    ?assertEqual(unread, proplists:get_value(name, Info)).

ets_table_churn_renders_unread_rows_test() ->
    Rows = [
        begin
            Tab = ets:new(ets_churn, []),
            true = ets:delete(Tab),
            observer_cli_ets:get_ets_info(Tab, memory)
        end
     || _ <- lists:seq(1, 100)
    ],
    [_Title | Rendered] = observer_cli_ets:render_ets_info(Rows, 100, 1, memory),
    ?assertEqual(100, length(Rendered)),
    ?assert(
        lists:all(fun(Row) -> string:find(lists:flatten(Row), "unread") =/= nomatch end, Rendered)
    ).

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

diagnostic_ets_metadata_generation_and_recon_top_n_test() ->
    Parent = self(),
    FirstId = make_ref(),
    SecondId = make_ref(),
    Values = #{
        table_a => ets_metadata(FirstId, table_a, 10, 2),
        table_b => ets_metadata(SecondId, table_b, 10, 2)
    },
    Source = #{
        count_fun => fun() -> 2 end,
        all_fun => fun() -> [table_b, table_a] end,
        info_fun => fun(Table, Key) ->
            Parent ! {ets_info_key, Key},
            maps:get(Key, maps:get(Table, Values))
        end,
        word_size_fun => fun() -> 8 end
    },
    Response = diagnostic_ets(#{sort => memory, limit => 2, test_ets_source => Source}),
    Data = maps:get(<<"data">>, Response),
    Items = maps:get(<<"items">>, Data),
    ExpectedIds = [
        list_to_binary(ref_to_list(Id))
     || {_, _, Id} <- recon_lib:sublist_top_n_attrs(
            [{0, 80, FirstId}, {0, 80, SecondId}], 2
        )
    ],
    ?assertEqual(ExpectedIds, [maps:get(<<"table_id">>, Item) || Item <- Items]),
    ?assertEqual([80, 80], [maps:get(<<"memory_bytes">>, Item) || Item <- Items]),
    Keys = receive_ets_info_keys(22, []),
    ?assertEqual(
        [
            id,
            keypos,
            memory,
            name,
            owner,
            protection,
            read_concurrency,
            size,
            type,
            write_concurrency
        ],
        lists:usort(Keys)
    ),
    ?assertEqual(<<"metadata_only">>, hd(maps:get(<<"coverage">>, table_probe(Response)))).

diagnostic_named_ets_recreate_is_not_correlated_test() ->
    Name = observer_cli_goal09_recreated,
    Source = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() ->
            _ = ets:new(Name, [named_table, public]),
            put(goal09_recreated, false),
            [Name]
        end,
        info_fun => fun(Table, Key) ->
            case {Key, get(goal09_recreated)} of
                {size, false} ->
                    OldSize = ets:info(Table, size),
                    ets:delete(Table),
                    _ = ets:new(Name, [named_table, public]),
                    put(goal09_recreated, true),
                    OldSize;
                _ ->
                    ets:info(Table, Key)
            end
        end,
        word_size_fun => fun() -> erlang:system_info(wordsize) end
    },
    Data = maps:get(<<"data">>, diagnostic_ets(#{test_ets_source => Source})),
    ?assertEqual([], maps:get(<<"items">>, Data)),
    ?assertEqual(1, maps:get(<<"disappeared_count">>, Data)).

diagnostic_ets_admission_refuses_before_enumeration_test() ->
    Parent = self(),
    Source = #{
        count_fun => fun() -> 100001 end,
        all_fun => fun() ->
            Parent ! ets_enumerated,
            []
        end,
        info_fun => fun(_Table, _Key) ->
            Parent ! ets_info_called,
            undefined
        end,
        word_size_fun => fun() -> 8 end
    },
    Response = diagnostic_ets(#{test_ets_source => Source}),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)),
    ?assertEqual(<<"pre_enumeration">>, maps:get(<<"admission_stage">>, Data)),
    receive
        ets_enumerated -> ?assert(false)
    after 25 -> ok
    end,
    receive
        ets_info_called -> ?assert(false)
    after 25 -> ok
    end.

ets_metadata(Id, Name, Memory, Size) ->
    #{
        id => Id,
        name => Name,
        size => Size,
        memory => Memory,
        owner => self(),
        type => set,
        protection => public,
        keypos => 1,
        write_concurrency => true,
        read_concurrency => false
    }.

diagnostic_ets(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), ets, Request, #{timeout_ms => 3000, identifier_policy => include}
        ),
    Response.

table_probe(Response) ->
    [Probe] = maps:get(
        <<"probes">>, maps:get(<<"capture">>, maps:get(<<"meta">>, Response))
    ),
    Probe.

receive_ets_info_keys(0, Acc) ->
    Acc;
receive_ets_info_keys(Count, Acc) ->
    receive
        {ets_info_key, Key} -> receive_ets_info_keys(Count - 1, [Key | Acc])
    after 1000 ->
        erlang:error({missing_ets_info_calls, Count})
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
