-module(observer_cli_plugin_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_configured_plugin_quit_test() ->
    ?assertEqual(quit, run_plugin_inputs(["q\n"])).

start_configured_plugin_navigation_test() ->
    ?assertEqual(quit, run_plugin_inputs(["F\n", "B\n", "1500\n", "q\n"])).

start_configured_plugin_redraw_test() ->
    ?assertEqual(quit, run_plugin_inputs([{sleep, 30, "q\n"}], #{interval => 1})).

start_configured_plugin_go_home_test() ->
    ?assertEqual(quit, run_plugin_inputs(["H\n", "q\n"])).

start_configured_plugin_shortcuts_test() ->
    ?assertEqual(quit, run_plugin_inputs(["N\n", "T\n", "q\n"])).

init_config_from_env_test() ->
    Plugins = [#{module => observer_cli_test_plugin, shortcut => "T", title => "Test"}],
    with_plugins_env(
        Plugins,
        fun() ->
            Plug = observer_cli_plugin:init_config(#plug{plugs = []}),
            #{1 := Conf} = Plug#plug.plugs,
            ?assertEqual(1, Plug#plug.cur_index),
            ?assertEqual(observer_cli_test_plugin, maps:get(module, Conf)),
            ?assertEqual("T", maps:get(shortcut, Conf)),
            ?assertEqual("Test", maps:get(title, Conf)),
            ?assertEqual(1500, maps:get(interval, Conf)),
            ?assertEqual(1, maps:get(cur_page, Conf)),
            ?assertEqual(1, maps:get(cur_row, Conf)),
            ?assertEqual(value, maps:get(sort, Conf)),
            ?assertEqual(false, maps:is_key(sort_column, Conf)),
            ?assertEqual(
                observer_cli_plugin:get_sheet_width(observer_cli_test_plugin),
                maps:get(sheet_width, Conf)
            )
        end
    ).

init_config_missing_plugin_uses_defaults_test() ->
    Plugins = [#{module => missing_plugin_module, shortcut => "M", title => "Missing"}],
    with_plugins_env(
        Plugins,
        fun() ->
            #plug{plugs = #{1 := Conf}} = observer_cli_plugin:init_config(#plug{plugs = []}),
            ?assertEqual(missing_plugin_module, maps:get(module, Conf)),
            ?assertEqual(1, maps:get(cur_page, Conf)),
            ?assertEqual(1, maps:get(cur_row, Conf)),
            ?assertEqual(1500, maps:get(interval, Conf)),
            ?assertEqual(observer_cli_lib:layout_base_width(), maps:get(sheet_width, Conf))
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
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{shortcut => "T", title => "Test"},
            2 => #{shortcut => "U", title => "Other"}
        }
    },
    Opts = #view_opts{plug = Plug},
    ?assertEqual({ok, menu, 1}, observer_cli_plugin:maybe_shortcut("T", Opts)),
    ?assertEqual({ok, menu, 2}, observer_cli_plugin:maybe_shortcut("U", Opts)).

maybe_shortcut_sheet_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{module => observer_cli_test_plugin}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({ok, sheet, name}, observer_cli_plugin:maybe_shortcut("N", Opts)).

maybe_shortcut_missing_index_test() ->
    Plug = #plug{cur_index = 1, plugs = #{}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({error, not_found}, observer_cli_plugin:maybe_shortcut("N", Opts)).

maybe_shortcut_sheet_not_found_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{module => observer_cli_test_plugin}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({error, not_found}, observer_cli_plugin:maybe_shortcut("Z", Opts)).

maybe_shortcut_undef_header_test() ->
    Plug = #plug{cur_index = 1, plugs = #{1 => #{module => missing_plugin_module}}},
    Opts = #view_opts{plug = Plug},
    ?assertEqual({error, not_found}, observer_cli_plugin:maybe_shortcut("Z", Opts)).

render_attributes_test() ->
    {Lines, Count, _State} = observer_cli_plugin:render_attributes(
        #{module => observer_cli_test_plugin},
        undefined
    ),
    ?assertEqual(1, Count),
    Text = lists:flatten(Lines),
    ?assert(string:find(Text, "Name") =/= nomatch),
    ?assert(string:find(Text, "50.00%") =/= nomatch).

render_attributes_undef_test() ->
    ?assertEqual(
        {[], 0, prev},
        observer_cli_plugin:render_attributes(#{module => missing_plugin_module}, prev)
    ).

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
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    ets:insert(SheetCache, {2, #{cells => #{name => "target"}, handle => target}}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        put(observer_cli_test_handler_parent, self()),
        observer_cli_test_io:with_input(
            ["2\n"],
            fun() ->
                ?assertEqual(quit, observer_cli_plugin:manager(ChildPid, SheetCache, Opts))
            end
        ),
        receive
            {plugin_handler, plugin, target, #view_opts{
                plug = #plug{plugs = #{1 := #{cur_row := 2}}}
            }} ->
                ok
        after 1000 ->
            erlang:error(plugin_handler_not_called)
        end
    after
        erase(observer_cli_test_handler_parent),
        case ets:info(SheetCache) of
            undefined -> ok;
            _ -> ets:delete(SheetCache)
        end,
        exit(ChildPid, kill)
    end.

start_jump_default_row_test() ->
    SheetCache = ets:new(test_sheet_cache_jump_default, [set, public]),
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    ets:insert(SheetCache, {1, #{cells => #{name => "item"}, handle => item}}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
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
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    ets:insert(SheetCache, {1, #{cells => #{name => "item"}}}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
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
    Line = iolist_to_binary(observer_cli_plugin:render_menu(Plug, 200)),
    ?assert(binary:match(Line, <<"Home">>) =/= nomatch),
    ?assert(binary:match(Line, <<"Test(T)">>) =/= nomatch),
    ?assert(binary:match(Line, <<"Test2">>) =/= nomatch).

manager_jump_missing_row_test() ->
    SheetCache = ets:new(test_sheet_cache_jump_missing, [set, public]),
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
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

manager_default_jump_missing_row_test() ->
    SheetCache = ets:new(test_sheet_cache_default_jump_missing, [set, public]),
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["\n", "q\n"],
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

manager_default_jump_no_match_test() ->
    SheetCache = ets:new(test_sheet_cache_default_jump_nomatch, [set, public]),
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    ets:insert(SheetCache, {1, #{cells => #{name => "item"}}}),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                handler => observer_cli_test_handler,
                cur_row => 1
            }
        }
    },
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["\n", "q\n"],
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
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
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

manager_empty_plugin_inputs_test() ->
    SheetCache = ets:new(test_sheet_cache_empty_inputs, [set, public]),
    ChildPid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Plug = #plug{cur_index = 1, plugs = #{}},
    Opts = #view_opts{plug = Plug},
    try
        observer_cli_test_io:with_input(
            ["F\n", "B\n", "1500\n", "1\n", "\n", "q\n"],
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

render_worker_empty_plugin_test() ->
    SheetCache = ets:new(test_sheet_cache_empty_render, [set, public]),
    try
        ?assertEqual(
            ok,
            observer_cli_plugin:render_worker(
                ?INIT_TIME_REF, #plug{plugs = #{}}, false, SheetCache, [], []
            )
        )
    after
        ets:delete(SheetCache)
    end.

render_worker_configured_plugin_test() ->
    SheetCache = ets:new(test_sheet_cache_render, [set, public]),
    SheetWidth = observer_cli_plugin:get_sheet_width(observer_cli_test_plugin),
    Plug = #plug{
        cur_index = 1,
        plugs = #{
            1 => #{
                module => observer_cli_test_plugin,
                title => "Test",
                shortcut => "T",
                interval => 1000,
                cur_page => 1,
                cur_row => 1,
                sort => name,
                sheet_width => SheetWidth
            }
        }
    },
    Pid = spawn(fun() ->
        observer_cli_plugin:render_worker(?INIT_TIME_REF, Plug, false, SheetCache, undefined, [])
    end),
    Ref = erlang:monitor(process, Pid),
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 1000 ->
        erlang:error(render_worker_not_stopped)
    end,
    ets:delete(SheetCache).

run_plugin_inputs(Inputs) ->
    run_plugin_inputs(Inputs, #{}).

run_plugin_inputs(Inputs, ExtraConfig) ->
    Plugins = [
        maps:merge(
            #{
                module => observer_cli_test_plugin,
                shortcut => "T",
                title => "Test"
            },
            ExtraConfig
        )
    ],
    with_plugins_env(
        Plugins,
        fun() ->
            observer_cli_test_io:with_geometry(
                24,
                80,
                Inputs,
                fun() -> observer_cli_plugin:start(#view_opts{auto_row = false}) end
            )
        end
    ).

with_plugins_env(Plugins, Fun) ->
    Prev = application:get_env(observer_cli, plugins),
    ok = application:set_env(observer_cli, plugins, Plugins),
    try
        Fun()
    after
        restore_plugins_env(Prev)
    end.

restore_plugins_env({ok, Plugins}) ->
    ok = application:set_env(observer_cli, plugins, Plugins);
restore_plugins_env(undefined) ->
    application:unset_env(observer_cli, plugins).

-endif.
