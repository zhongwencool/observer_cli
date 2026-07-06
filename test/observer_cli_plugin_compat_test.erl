-module(observer_cli_plugin_compat_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

retained_plugin_2_controls_and_cells_test() ->
    Header = observer_cli_plugin_compat:normalize_sheet_header(header()),
    Config = observer_cli_plugin_compat:migrate_config(
        #{
            module => observer_cli_test_plugin,
            title => "Test",
            shortcut => "T",
            interval => 1500,
            cur_page => 2,
            cur_row => 3,
            sort => value
        },
        Header
    ),
    ?assertEqual(observer_cli_test_plugin, maps:get(module, Config)),
    ?assertEqual("Test", maps:get(title, Config)),
    ?assertEqual("T", maps:get(shortcut, Config)),
    ?assertEqual(1500, maps:get(interval, Config)),
    ?assertEqual(2, maps:get(cur_page, Config)),
    ?assertEqual(3, maps:get(cur_row, Config)),
    ?assertEqual(value, maps:get(sort, Config)),
    [#{shortcut := "N"}, #{shortcut := "V"}] = maps:get(columns, Header),
    Attrs = #{
        rows => [
            [#{content => {byte, 1024}, width => 8}, #{content => {percent, 0.5}, width => 8}]
        ],
        state => attrs
    },
    ?assertEqual(Attrs, observer_cli_plugin_compat:normalize_attributes(Attrs)),
    Body = #{
        rows => [#{cells => #{name => "alpha", value => 1}, handle => self()}], state => sheet
    },
    ?assertEqual(Body, observer_cli_plugin_compat:normalize_sheet_body(Body)),
    ?assertEqual(
        {observer_cli_process, self()},
        observer_cli_plugin_compat:resolve_handler(#{}, hd(maps:get(rows, Body)))
    ).

retained_missing_callback_policy_test() ->
    ?assertEqual(
        {[], 0, prev},
        observer_cli_plugin:render_attributes(#{module => missing_plugin_module}, prev)
    ),
    SheetCache = ets:new(plugin_compat_missing_callbacks, [set, public]),
    try
        Plug = #{module => missing_plugin_module, sort => name, cur_page => 1, cur_row => 1},
        ?assertEqual({[], []}, observer_cli_plugin:render_sheet(1, Plug, SheetCache, []))
    after
        ets:delete(SheetCache)
    end.

migrated_sort_column_and_handler_tuple_test() ->
    Config = observer_cli_plugin_compat:migrate_config(
        #{sort_column => 2, handler => {fun is_pid/1, observer_cli_test_handler}},
        header()
    ),
    ?assertEqual(value, maps:get(sort, Config)),
    ?assertEqual(false, maps:is_key(sort_column, Config)),
    ?assertEqual(observer_cli_test_handler, maps:get(handler, Config)),
    Row = #{cells => #{name => "alpha", value => 1}, handle => {row, 1}},
    ?assertEqual(
        {observer_cli_test_handler, {row, 1}},
        observer_cli_plugin_compat:resolve_handler(Config, Row)
    ).

rejected_legacy_callback_shapes_test() ->
    ?assertError(
        {plugin_api_error, #{source := attributes, reason := legacy_tuple_result}},
        observer_cli_plugin_compat:normalize_attributes({[], attrs})
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_header, reason := legacy_header_list}},
        observer_cli_plugin_compat:normalize_sheet_header([#{title => "Name", width => 6}])
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_body, reason := legacy_tuple_result}},
        observer_cli_plugin_compat:normalize_sheet_body({[], sheet})
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_body, reason := legacy_row_list}},
        observer_cli_plugin_compat:normalize_sheet_body(#{rows => [["alpha", 1]], state => sheet})
    ).

rejected_invalid_columns_sort_and_default_handler_test() ->
    ?assertError(
        {plugin_api_error, #{source := sheet_header, reason := duplicate_column_id}},
        observer_cli_plugin_compat:normalize_sheet_header(#{
            columns => [column(name), column(name)], default_sort => name
        })
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_header, reason := invalid_default_sort}},
        observer_cli_plugin_compat:normalize_sheet_header(#{
            columns => [column(name)], default_sort => missing
        })
    ),
    ?assertError(
        {plugin_api_error, #{source := config, reason := invalid_sort}},
        observer_cli_plugin_compat:migrate_config(#{sort => missing}, header())
    ),
    ?assertError(
        {plugin_api_error, #{source := config, reason := invalid_sort_column}},
        observer_cli_plugin_compat:migrate_config(#{sort_column => 99}, header())
    ),
    ?assertError(
        {plugin_api_error, #{source := row_handler, reason := non_pid_default_handle}},
        observer_cli_plugin_compat:resolve_handler(#{}, #{
            cells => #{name => "alpha"}, handle => not_a_pid
        })
    ).

rejected_invalid_plugin_api_shapes_test() ->
    ?assertError(
        {plugin_api_error, #{source := attributes, reason := expected_rows_state_map}},
        observer_cli_plugin_compat:normalize_attributes(not_a_map)
    ),
    ?assertError(
        {plugin_api_error, #{source := attributes, reason := invalid_attribute_row}},
        observer_cli_plugin_compat:normalize_attributes(#{rows => [bad_row], state => attrs})
    ),
    ?assertError(
        {plugin_api_error, #{source := attributes, reason := invalid_attribute_cell}},
        observer_cli_plugin_compat:normalize_attributes(#{
            rows => [[#{content => bad}]], state => attrs
        })
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_header, reason := expected_columns_map}},
        observer_cli_plugin_compat:normalize_sheet_header(not_a_map)
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_header, reason := invalid_column}},
        observer_cli_plugin_compat:normalize_sheet_header(#{
            columns => [#{id => name, title => "Name", width => 0}], default_sort => name
        })
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_body, reason := expected_rows_state_map}},
        observer_cli_plugin_compat:normalize_sheet_body(not_a_map)
    ),
    ?assertError(
        {plugin_api_error, #{source := sheet_body, reason := invalid_row}},
        observer_cli_plugin_compat:normalize_sheet_body(#{rows => [bad_row], state => sheet})
    ),
    ?assertError(
        {plugin_api_error, #{source := config, reason := invalid_handler}},
        observer_cli_plugin_compat:migrate_config(#{handler => {not_a_handler}}, header())
    ),
    ?assertError(
        {plugin_api_error, #{source := config, reason := invalid_handler}},
        observer_cli_plugin_compat:resolve_handler(#{handler => {not_a_handler}}, #{
            cells => #{name => "alpha"}, handle => self()
        })
    ),
    ?assertError(
        {plugin_api_error, #{source := row_handler, reason := invalid_row}},
        observer_cli_plugin_compat:resolve_handler(#{}, bad_row)
    ).

migrated_atom_handler_passthrough_test() ->
    Config = observer_cli_plugin_compat:migrate_config(
        #{handler => observer_cli_test_handler, sort => name},
        header()
    ),
    ?assertEqual(observer_cli_test_handler, maps:get(handler, Config)).

header() ->
    #{
        columns => [
            (column(name))#{shortcut => "N"},
            (column(value))#{shortcut => "V"}
        ],
        default_sort => name
    }.

column(Id) ->
    #{id => Id, title => atom_to_list(Id), width => 8}.

-endif.
