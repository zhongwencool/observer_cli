-module(observer_cli_golden_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

golden_output_base_accepts_stable_fragments_test() ->
    observer_cli_test_io:with_geometry(
        24,
        80,
        [],
        fun() ->
            Output = [
                observer_cli_lib:render([
                    ?W2(?GRAY_BG, "Stable Title", 16),
                    ?W("Stable Footer", 18)
                ]),
                observer_cli_lib:render_footer("q(quit) F/B(page forward/back)")
            ],
            observer_cli_test_io:assert_stable_fragments(Output, [
                "Stable Title",
                "Stable Footer",
                "q(quit)",
                "F/B(page forward/back)"
            ]),
            observer_cli_test_io:assert_ansi_boundaries(Output)
        end
    ).

golden_output_base_rejects_bad_ansi_boundaries_test() ->
    ?assertError(_, observer_cli_test_io:assert_ansi_boundaries(<<"|\e[31mred|">>)),
    ?assertError(_, observer_cli_test_io:assert_ansi_boundaries(<<"|\e[0m |">>)).

home_golden_output_fragments_test() ->
    observer_cli_test_io:with_geometry(
        24,
        201,
        [],
        fun() ->
            LayoutWidth = observer_cli_lib:layout_width(),
            Prompt = observer_cli:get_refresh_prompt(proc_count, memory, 1500, 10),
            Menu = observer_cli_lib:render_menu(home, Prompt),
            Footer = observer_cli:render_footer(),
            StableInfo = observer_cli:get_stable_system_info(),
            SystemLines = observer_cli:render_system_line(
                "printf 'header\n 1 2\n'", StableInfo, {ok, 1000, 10}
            ),
            MemoryLines = observer_cli:render_memory_process_line({1, 2, 3, 4}, 1500),
            {_, [TopTitle]} = observer_cli:render_top_n_view(
                memory, [], 0, [{1, 1}], 1, LayoutWidth
            ),
            Output = [Menu, Footer, SystemLines, MemoryLines, TopTitle],
            observer_cli_test_io:assert_stable_fragments(Output, [
                "Home(H)",
                "Network(N)",
                "System(S)",
                "Ets(E)",
                "App(A)",
                "Doc(D)",
                "Plugin(P)",
                "recon:proc_count(memory, 10) Interval:1500ms",
                "q(quit)",
                "p(pause)",
                "F/B(page forward/back)",
                "No | Pid",
                "Memory",
                "Name|>Label|>Initial Call",
                "Current Function",
                "Count/Limit",
                "Reds(Total/SinceLastCall)",
                "Persistent Terms",
                "IO/GC:(1500ms)"
            ]),
            observer_cli_test_io:assert_ansi_boundaries(Output),
            ?assertEqual(LayoutWidth, observer_cli_lib:visible_length(TopTitle)),
            ?assert(
                lists:all(
                    fun(Line) -> Line =< LayoutWidth end,
                    observer_cli_test_io:line_widths([SystemLines, MemoryLines, TopTitle])
                )
            )
        end
    ).

-endif.
