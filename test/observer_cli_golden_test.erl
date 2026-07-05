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
                observer_cli_lib:render_last_line("q(quit) F/B(page forward/back)")
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

-endif.
