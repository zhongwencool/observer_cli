-module(observer_cli_plugin_render_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

render_sheet_header_test() ->
    {Headers, Columns} = observer_cli_plugin:render_sheet_header(observer_cli_test_plugin, name),
    ?assertEqual([#{id => name, width => 6}, #{id => value, width => 5}], Columns),
    Text = lists:flatten(Headers),
    ?assert(string:find(Text, "No ") =/= nomatch),
    ?assert(string:find(Text, "Name") =/= nomatch),
    ?assert(string:find(Text, "Value") =/= nomatch).

render_sheet_body_test() ->
    SheetCache = ets:new(plugin_sheet_cache, [set, public]),
    {_, Columns} = observer_cli_plugin:render_sheet_header(observer_cli_test_plugin, name),
    {Lines, _NewSheet} = observer_cli_plugin:render_sheet_body(
        observer_cli_test_plugin,
        1,
        1,
        2,
        name,
        Columns,
        SheetCache,
        []
    ),
    ?assertEqual(2, length(Lines)),
    Text = lists:flatten(Lines),
    ?assert(string:find(Text, "alph") =/= nomatch),
    ?assert(string:find(Text, "beta") =/= nomatch),
    ?assertMatch([{1, _}], ets:lookup(SheetCache, 1)),
    ets:delete(SheetCache).

render_sheet_body_sort_test() ->
    SheetCache = ets:new(plugin_sheet_cache_sort, [set, public]),
    {_, Columns} = observer_cli_plugin:render_sheet_header(observer_cli_test_plugin, value),
    {_Lines, _NewSheet} = observer_cli_plugin:render_sheet_body(
        observer_cli_test_plugin,
        1,
        1,
        2,
        value,
        Columns,
        SheetCache,
        []
    ),
    ?assertMatch([{1, #{cells := #{name := "beta", value := 2}}}], ets:lookup(SheetCache, 1)),
    ?assertMatch([{2, #{cells := #{name := "alpha", value := 1}}}], ets:lookup(SheetCache, 2)),
    ets:delete(SheetCache).

render_sheet_undef_test() ->
    SheetCache = ets:new(plugin_sheet_cache_undef, [set, public]),
    Plug = #{module => missing_plugin_module, sort => name, cur_page => 1, cur_row => 1},
    ?assertEqual({[], []}, observer_cli_plugin:render_sheet(1, Plug, SheetCache, [])),
    ets:delete(SheetCache).

mix_content_width_middle_test() ->
    Cells = observer_cli_plugin:mix_content_width(["alpha", 2, <<"omega">>], [7, 5, 8], []),
    Text = lists:flatten(observer_cli_lib:render(Cells)),
    ?assert(string:find(Text, "alpha") =/= nomatch),
    ?assert(string:find(Text, "omega") =/= nomatch).

get_sheet_width_test() ->
    ?assertEqual(12, observer_cli_plugin:get_sheet_width(observer_cli_test_plugin)),
    ?assertEqual(
        observer_cli_lib:layout_base_width(),
        observer_cli_plugin:get_sheet_width(missing_plugin_module)
    ).

match_shortcut_test() ->
    Plugs = [
        {1, #{shortcut => "A"}},
        {2, #{shortcut => "B"}}
    ],
    ?assertEqual({ok, 2}, observer_cli_plugin:match_menu_shortcut("B", Plugs)),
    ?assertEqual({error, not_found}, observer_cli_plugin:match_menu_shortcut("Z", Plugs)),
    ?assertEqual(
        {ok, name},
        observer_cli_plugin:match_sheet_shortcut(
            "N",
            observer_cli_test_plugin:sheet_header(),
            1
        )
    ),
    ?assertEqual(
        {error, not_found},
        observer_cli_plugin:match_sheet_shortcut("Z", observer_cli_test_plugin:sheet_header(), 1)
    ).

-endif.
