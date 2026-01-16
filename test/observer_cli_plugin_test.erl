-module(observer_cli_plugin_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

manager_quit_test() ->
    SheetCache = ets:new(test_sheet_cache_quit, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    Opts = #view_opts{plug = #plug{cur_index = 1, plugs = #{}}},
    try
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

init_config_from_env_test() ->
    Plugins = [#{module => observer_cli_test_plugin, shortcut => "T", title => "Test"}],
    with_plugins_env(
        Plugins,
        fun() ->
            Plug = observer_cli_plugin:init_config(#plug{plugs = []}),
            #{1 := Conf} = Plug#plug.plugs,
            ?assertEqual(1, Plug#plug.cur_index),
            ?assertEqual(1500, maps:get(interval, Conf)),
            ?assertEqual(1, maps:get(cur_page, Conf))
        end
    ).

init_config_passthrough_test() ->
    Plug0 = #plug{cur_index = 1, plugs = #{1 => #{module => observer_cli_test_plugin}}},
    ?assertEqual(Plug0, observer_cli_plugin:init_config(Plug0)).

update_plugins_test() ->
    Plugs = #{1 => #{interval => 1000}, 2 => #{interval => 2000}},
    Updated = observer_cli_plugin:update_plugins(1, Plugs, #{interval => 1500}),
    ?assertEqual(1500, maps:get(interval, maps:get(1, Updated))),
    ?assertEqual(2000, maps:get(interval, maps:get(2, Updated))).

maybe_shortcut_menu_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{shortcut => "T", title => "Test"}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({ok, menu, 1}, observer_cli_plugin:maybe_shortcut("T", Opts)).

maybe_shortcut_sheet_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{module => observer_cli_test_plugin}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({ok, sheet, 1}, observer_cli_plugin:maybe_shortcut("N", Opts)).

maybe_shortcut_missing_sheet_header_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{module => observer_cli_plugin}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({error, not_found}, observer_cli_plugin:maybe_shortcut("N", Opts)).

maybe_shortcut_missing_index_test() ->
    Plug = #plug{cur_index = 1, plugs = #{}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({error, not_found}, observer_cli_plugin:maybe_shortcut("N", Opts)).

render_attributes_test() ->
    {Lines, Count, _State} = observer_cli_plugin:render_attributes(
        #{module => observer_cli_test_plugin},
        undefined
    ),
    ?assertEqual(1, Count),
    ?assertEqual(1, length(Lines)).

render_attributes_missing_module_test() ->
    {Lines, Count, State} = observer_cli_plugin:render_attributes(
        #{module => observer_cli_plugin},
        undefined
    ),
    ?assertEqual([], Lines),
    ?assertEqual(0, Count),
    ?assertEqual(undefined, State).

get_sheet_width_empty_header_test() ->
    ?assertEqual(?COLUMN + 5, observer_cli_plugin:get_sheet_width(observer_cli_empty_sheet_plugin)).

render_sheet_missing_module_test() ->
    Plug = #{module => observer_cli_plugin, sort_column => 1, cur_page => 1, cur_row => 1},
    SheetCache = ets:new(test_sheet_cache, [set, public]),
    try
        ?assertEqual({[], []}, observer_cli_plugin:render_sheet(1, Plug, SheetCache, undefined))
    after
        ets:delete(SheetCache)
    end.

parse_cmd_str_test() ->
    ?assertEqual(go_home, observer_cli_plugin:parse_cmd_str("H\n")),
    ?assertEqual(page_up_top_n, observer_cli_plugin:parse_cmd_str("B\n")),
    ?assertEqual(page_down_top_n, observer_cli_plugin:parse_cmd_str("F\n")),
    ?assertEqual(quit, observer_cli_plugin:parse_cmd_str("q\n")),
    ?assertEqual(jump, observer_cli_plugin:parse_cmd_str("\n")),
    ?assertEqual({new_interval, 1500}, observer_cli_plugin:parse_cmd_str("1500")),
    ?assertEqual(quit, observer_cli_plugin:parse_cmd_str({error, estale})).

start_jump_action_test() ->
    SheetCache = ets:new(test_sheet_cache_jump, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    ets:insert(SheetCache, {1, [item]}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => {fun(_Item) -> true end, observer_cli_test_handler},
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["1\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

start_jump_default_row_test() ->
    SheetCache = ets:new(test_sheet_cache_jump_default, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    ets:insert(SheetCache, {1, [item]}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => {fun(_Item) -> true end, observer_cli_test_handler},
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

start_jump_no_match_test() ->
    SheetCache = ets:new(test_sheet_cache_jump_nomatch, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    ets:insert(SheetCache, {1, [item]}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => {fun(_Item) -> false end, observer_cli_test_handler},
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["1\n", "q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

render_menu_multi_plugins_test() ->
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{title => "Test", shortcut => "T"},
            2 => #{title => "Test2", shortcut => "U"}
        }
    },
    Line = iolist_to_binary(observer_cli_plugin:render_menu(Plug, 10)),
    ?assert(byte_size(Line) > 0),
    ?assert(binary:match(Line, <<"Home">>) =/= nomatch).

render_menu_gap_index_test() ->
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{title => "Alpha", shortcut => "A"},
            3 => #{title => "Gamma", shortcut => "G"}
        }
    },
    Line = iolist_to_binary(observer_cli_plugin:render_menu(Plug, 10)),
    ?assert(byte_size(Line) > 0),
    ?assert(binary:match(Line, <<"Home">>) =/= nomatch).

render_worker_quit_test() ->
    SheetCache = ets:new(test_sheet_cache_render_worker, [set, public]),
    Plug = test_plug(),
    Parent = self(),
    observer_cli_test_io:with_input(
        [],
        fun() ->
            Pid = spawn_link(fun() ->
                Parent ! {render_worker_result,
                    observer_cli_plugin:render_worker(
                        ?INIT_TIME_REF,
                        Plug,
                        false,
                        SheetCache,
                        undefined,
                        undefined
                    )}
            end),
            Pid ! quit,
            receive
                {render_worker_result, quit} -> ok
            after 1000 ->
                ?assert(false)
            end
        end
    ),
    ets:delete(SheetCache).

render_worker_missing_plugins_test() ->
    Plug = #plug{cur_index = 1, plugs = #{}},
    observer_cli_test_io:with_input(
        [],
        fun() ->
            ?assertEqual(
                ok,
                observer_cli_plugin:render_worker(
                    ?INIT_TIME_REF,
                    Plug,
                    false,
                    undefined,
                    undefined,
                    undefined
                )
            )
        end
    ).

manager_jump_missing_row_test() ->
    SheetCache = ets:new(test_sheet_cache_jump_missing, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => {fun(_Item) -> true end, observer_cli_test_handler},
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["1\n", "q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

manager_input_str_not_found_test() ->
    SheetCache = ets:new(test_sheet_cache_input_str, [set, public]),
    ChildPid = spawn(fun() -> receive after infinity -> ok end end),
    Plug = #plug{cur_index = 1, plugs = #{}},
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["oops\n", "q\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        )
    after
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

manager_page_down_test() ->
    ?assertEqual(quit, run_manager_inputs(["F\n", "q\n"], test_plug())).

manager_page_up_test() ->
    ?assertEqual(quit, run_manager_inputs(["B\n", "q\n"], test_plug())).

manager_new_interval_test() ->
    ?assertEqual(quit, run_manager_inputs(["1500\n", "q\n"], test_plug())).

test_plug() ->
    #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                title => "Test",
                shortcut => "T",
                module => observer_cli_test_plugin,
                interval => 1000,
                cur_page => 1,
                cur_row => 1,
                sort_column => 1,
                sheet_width => 12
            }
        }
    }.

run_manager_inputs(Inputs, Plug) ->
    Parent = self(),
    spawn_link(fun() ->
        SheetCache = ets:new(test_sheet_cache_manager, [set, public]),
        ChildPid = spawn(fun() -> receive after infinity -> ok end end),
        Opts = #view_opts{plug = Plug, auto_row = false},
        PrevRow = application:get_env(observer_cli, default_row_size),
        ok = application:set_env(observer_cli, default_row_size, 4),
        try
            Result = observer_cli_test_io:with_input(
                Inputs,
                fun() ->
                    observer_cli_plugin:manager(ChildPid, SheetCache, Opts)
                end
            ),
            Parent ! {manager_result, Result}
        after
            case PrevRow of
                {ok, Val} -> application:set_env(observer_cli, default_row_size, Val);
                undefined -> application:unset_env(observer_cli, default_row_size)
            end,
            exit(ChildPid, kill)
        end
    end),
    receive
        {manager_result, Result} -> Result
    after 2000 ->
        timeout
    end.

with_plugins_env(Plugins, Fun) ->
    Prev = application:get_env(observer_cli, plugins),
    ok = application:set_env(observer_cli, plugins, Plugins),
    try Fun()
    after restore_plugins_env(Prev)
    end.

restore_plugins_env({ok, Plugins}) ->
    ok = application:set_env(observer_cli, plugins, Plugins);
restore_plugins_env(undefined) ->
    application:unset_env(observer_cli, plugins).

-endif.
