-module(less_client_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

main_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            State = less_client:init("a\nb\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_next_page_test() ->
    observer_cli_test_io:with_input(
        ["j\n", "q\n"],
        fun() ->
            State = less_client:init("a\nb\nc\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_next_page_uppercase_test() ->
    observer_cli_test_io:with_input(
        ["F\n", "q\n"],
        fun() ->
            State = less_client:init("a\nb\nc\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_prev_page_test() ->
    observer_cli_test_io:with_input(
        ["k\n", "q\n"],
        fun() ->
            State = less_client:init("a\nb\nc\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_prev_page_uppercase_test() ->
    observer_cli_test_io:with_input(
        ["B\n", "q\n"],
        fun() ->
            State = less_client:init("a\nb\nc\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_quit_uppercase_test() ->
    observer_cli_test_io:with_input(
        ["Q\n"],
        fun() ->
            State = less_client:init("a\nb\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_eof_test() ->
    observer_cli_test_io:with_input(
        [],
        fun() ->
            State = less_client:init("a\nb\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_error_test() ->
    observer_cli_test_io:with_input(
        [{error, estale}],
        fun() ->
            State = less_client:init("a\nb\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_unknown_key_test() ->
    observer_cli_test_io:with_input(
        ["x\n", "q\n"],
        fun() ->
            State = less_client:init("a\nb\nc\n"),
            ?assertEqual(none, less_client:main(State))
        end
    ).

main_nav_action_test() ->
    observer_cli_test_io:with_input(
        ["n\n"],
        fun() ->
            Nav = #{"n\n" => quit},
            State = less_client:init({"a\nb\n", "Header\n", Nav, "Footer\n"}),
            ?assertEqual(quit, less_client:main(State))
        end
    ).

main_nav_action_non_quit_test() ->
    observer_cli_test_io:with_input(
        ["n\n"],
        fun() ->
            Nav = #{"n\n" => home},
            State = less_client:init({"a\nb\n", "Header\n", Nav, "Footer\n"}),
            ?assertEqual(home, less_client:main(State))
        end
    ).

init_variants_test() ->
    Input = "a\nb\n",
    Header = "Header\n",
    Nav = #{"q\n" => quit},
    Footer = "Footer\n",

    {Less1, H1, N1, F1} = less_client:init(Input),
    ?assertEqual(undefined, H1),
    ?assertEqual(#{}, N1),
    ?assertEqual(undefined, F1),
    ok = less_server:stop(Less1),

    {Less2, H2, N2, F2} = less_client:init({Input, Header}),
    ?assertEqual(Header, H2),
    ?assertEqual(#{}, N2),
    ?assertEqual(undefined, F2),
    ok = less_server:stop(Less2),

    {Less3, H3, N3, F3} = less_client:init({Input, Header, Nav}),
    ?assertEqual(Header, H3),
    ?assertEqual(Nav, N3),
    ?assertEqual(undefined, F3),
    ok = less_server:stop(Less3),

    {Less4, H4, N4, F4} = less_client:init({Input, Header, Nav, Footer}),
    ?assertEqual(Header, H4),
    ?assertEqual(Nav, N4),
    ?assertEqual(Footer, F4),
    ok = less_server:stop(Less4).

normalize_key_test() ->
    ?assertEqual("ab\n", less_client:normalize_key("a\rb\r\n")).

line_helpers_test() ->
    ?assertEqual(0, less_client:header_lines(undefined)),
    ?assertEqual(1, less_client:header_lines("H")),
    ?assertEqual(1, less_client:footer_lines(undefined)),
    ?assertEqual(1, less_client:footer_lines("F")).

render_last_line_test() ->
    Line = less_client:render_last_line(#{}),
    ?assert(is_binary(Line)),
    ?assertMatch({_, _}, binary:match(Line, <<"q(quit)">>)).

render_last_line_with_back_test() ->
    Line = less_client:render_last_line(#{"B\n" => prev}),
    ?assertMatch({_, _}, binary:match(Line, <<"k(previous page)">>)).

-endif.
