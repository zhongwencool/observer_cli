-module(observer_cli_plugin_render_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

render_sheet_header_test() ->
    {Headers, Widths} = observer_cli_plugin:render_sheet_header(observer_cli_test_plugin, 1),
    ?assertEqual(2, length(Widths)),
    ?assert(string:find(lists:flatten(Headers), "No ") =/= nomatch).

render_sheet_body_test() ->
    SheetCache = ets:new(plugin_sheet_cache, [set, public]),
    {_, Widths} = observer_cli_plugin:render_sheet_header(observer_cli_test_plugin, 1),
    {Lines, _NewSheet} = observer_cli_plugin:render_sheet_body(
        observer_cli_test_plugin,
        1,
        1,
        2,
        1,
        Widths,
        SheetCache,
        []
    ),
    ?assertEqual(2, length(Lines)),
    ?assertMatch([{1, _}], ets:lookup(SheetCache, 1)),
    ets:delete(SheetCache).

get_sheet_width_test() ->
    ?assertEqual(12, observer_cli_plugin:get_sheet_width(observer_cli_test_plugin)).

match_shortcut_test() ->
    Plugs = [
        {1, #{shortcut => "A"}},
        {2, #{shortcut => "B"}}
    ],
    ?assertEqual({ok, 2}, observer_cli_plugin:match_menu_shortcut("B", Plugs)),
    ?assertEqual({error, not_found}, observer_cli_plugin:match_menu_shortcut("Z", Plugs)),
    ?assertEqual(
        {ok, 1},
        observer_cli_plugin:match_sheet_shortcut(
            "N",
            observer_cli_test_plugin:sheet_header(),
            1
        )
    ).

mix_content_width_test() ->
    Result = observer_cli_plugin:mix_content_width(["alpha", 1], [6, 5], []),
    ?assertEqual(2, length(Result)).

mix_content_width_middle_test() ->
    Result = observer_cli_plugin:mix_content_width(["alpha", "beta", 1], [6, 6, 5], []),
    ?assertEqual(3, length(Result)).

get_sheet_width_missing_module_test() ->
    ?assertEqual(?COLUMN + 5, observer_cli_plugin:get_sheet_width(observer_cli_plugin)).

-endif.
