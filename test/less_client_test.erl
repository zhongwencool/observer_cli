-module(less_client_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

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
        ["H\n"],
        fun() ->
            Nav = #{"H\n" => home},
            State = less_client:init({"a\nb\n", "Header\n", Nav, "Footer\n"}),
            ?assertEqual(home, less_client:main(State))
        end
    ).

main_nav_action_back_test() ->
    observer_cli_test_io:with_input(
        ["B\n"],
        fun() ->
            Nav = #{"B\n" => back},
            State = less_client:init({"a\nb\n", "Header\n", Nav, "Footer\n"}),
            ?assertEqual(back, less_client:main(State))
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
    ?assertNot(is_pid(Less1)),

    {Less2, H2, N2, F2} = less_client:init({Input, Header}),
    ?assertEqual(Header, H2),
    ?assertEqual(#{}, N2),
    ?assertEqual(undefined, F2),
    ?assertNot(is_pid(Less2)),

    {Less3, H3, N3, F3} = less_client:init({Input, Header, Nav}),
    ?assertEqual(Header, H3),
    ?assertEqual(Nav, N3),
    ?assertEqual(undefined, F3),
    ?assertNot(is_pid(Less3)),

    {Less4, H4, N4, F4} = less_client:init({Input, Header, Nav, Footer}),
    ?assertEqual(Header, H4),
    ?assertEqual(Nav, N4),
    ?assertEqual(Footer, F4),
    ?assertNot(is_pid(Less4)).

normalize_key_test() ->
    ?assertEqual("ab\n", less_client:normalize_key("a\rb\r\n")).

line_helpers_test() ->
    ?assertEqual(0, less_client:header_lines(undefined)),
    ?assertEqual(1, less_client:header_lines("H")),
    ?assertEqual(1, less_client:footer_lines(undefined)),
    ?assertEqual(1, less_client:footer_lines("F")).

render_footer_test() ->
    Line = less_client:render_footer(#{}),
    ?assert(is_binary(Line)),
    ?assertMatch({_, _}, binary:match(Line, <<"q(quit)">>)).

render_footer_with_back_test() ->
    Line = less_client:render_footer(#{"B\n" => prev}),
    ?assertMatch({_, _}, binary:match(Line, <<"k(previous page)">>)).

%% Compare complete frames, including whitespace and repeated redraws.
%% Rows=3 leaves two body lines when only the footer is present.
pagination_frames_test_() ->
    Text = "L1\nL2\nL3\nL4\nL5",
    First = "L1\nL2\n",
    Second = "L3\nL4\n",
    Third = "L5\n",
    [
        {Name, fun() ->
            assert_pages(3, Text, Keys ++ ["q\n"], Expected)
        end}
     || {Name, Keys, Expected} <- [
            {"carry state through next and previous", ["j\n", "j\n", "k\n"], [
                First, Second, Third, Second
            ]},
            {"uppercase next and previous", ["F\n", "B\n"], [First, Second, First]},
            {"clamp first and last pages", ["k\n", "B\n", "F\n", "j\n", "j\n", "F\n"], [
                First, First, First, Second, Third, Third, Third
            ]},
            {"unknown key keeps current position", ["j\n", "x\n", "F\n", "k\n"], [
                First, Second, Second, Third, Second
            ]},
            {"CRLF keys navigate", ["j\r\n", "k\r\n"], [First, Second, First]}
        ]
    ].

text_boundaries_test_() ->
    [
        {Name, fun() -> assert_pages(Rows, Text, Keys, Pages) end}
     || {Name, Rows, Text, Keys, Pages} <- [
            {"empty", 3, "", ["j\n", "q\n"], ["\n", "\n"]},
            {"single line", 3, "one", ["j\n", "q\n"], ["one\n", "one\n"]},
            {"exact full page", 3, "one\ntwo", ["j\n", "q\n"], ["one\ntwo\n", "one\ntwo\n"]},
            {"trailing newline remains a line", 3, "one\ntwo\n", ["j\n", "q\n"], [
                "one\ntwo\n", "\n"
            ]},
            {"consecutive empty lines", 3, "one\n\n\nfour", ["j\n", "q\n"], ["one\n\n", "\nfour\n"]},
            {"minimum page height", 1, "one\ntwo", ["j\n", "k\n", "q\n"], [
                "one\n", "two\n", "one\n"
            ]},
            {"unicode body", 3, "你好\n世界\n第三行", ["j\n", "q\n"], ["你好\n世界\n", "第三行\n"]}
        ]
    ].

header_and_footer_frames_test_() ->
    [
        {Name, fun() ->
            {none, Frames} = capture_frames(
                Rows, {"甲\n乙", "标题\n", #{}, Footer}, ["j\n", "q\n"]
            ),
            ?assertEqual(
                [frame("标题\n", "甲\n", Footer), frame("标题\n", "乙\n", Footer)],
                Frames
            )
        end}
     || {Name, Rows, Footer} <- [
            {"header reserves one row with default footer", 3, undefined},
            {"header and custom footer", 3, "页脚\n"},
            {"header in tiny terminal", 1, "页脚\n"}
        ]
    ].

navigation_override_frames_test() ->
    Nav = #{"B\n" => back},
    {back, Frames} = capture_frames(
        3, {"L1\nL2\nL3", undefined, Nav}, ["j\n", "k\n", "B\n"]
    ),
    Footer = less_client:render_footer(Nav),
    ?assertEqual(nomatch, binary:match(Footer, <<"B/k">>)),
    ?assertNotEqual(nomatch, binary:match(Footer, <<"k(previous page)">>)),
    ?assertEqual(
        [
            frame(undefined, "L1\nL2\n", Footer),
            frame(undefined, "L3\n", Footer),
            frame(undefined, "L1\nL2\n", Footer),
            <<>>
        ],
        Frames
    ).

exit_frames_test_() ->
    [
        {Name, fun() ->
            {Result, Frames} = capture_frames(3, {"body", undefined, Nav, "footer\n"}, Keys),
            ?assertEqual(Expected, Result),
            ?assertEqual([<<"body\nfooter\n">>] ++ ExtraFrames, Frames)
        end}
     || {Name, Nav, Keys, Expected, ExtraFrames} <- [
            {"q leaves last frame", #{}, ["q\n"], none, []},
            {"Q leaves last frame", #{}, ["Q\n"], none, []},
            {"EOF leaves last frame", #{}, [], none, []},
            {"input error leaves last frame", #{}, [{error, estale}], none, []},
            {"mapped q returns quit without clearing", #{"q\n" => quit}, ["q\n"], quit, []},
            {"mapped Q returns quit without clearing", #{"Q\n" => quit}, ["Q\n"], quit, []},
            {"home clears", #{"H\n" => home}, ["H\n"], home, [<<>>]},
            {"back clears", #{"B\n" => back}, ["B\n"], back, [<<>>]},
            {"view switch clears", #{"P\n" => info_view}, ["P\n"], info_view, [<<>>]}
        ]
    ].

init_local_state_test() ->
    observer_cli_test_io:with_input([], fun() ->
        {links, Before} = process_info(self(), links),
        {Page, _, _, _} = less_client:init("a\nb"),
        ?assert(is_tuple(Page)),
        ?assertNot(is_pid(Page)),
        {links, After} = process_info(self(), links),
        ?assertEqual(lists:sort(Before), lists:sort(After))
    end).

terminal_rows_fallback_test() ->
    %% The existing fake I/O server can return a geometry error directly.
    Text = string:join([integer_to_list(N) || N <- lists:seq(1, 43)], "\n"),
    First = string:join([integer_to_list(N) || N <- lists:seq(1, 42)], "\n") ++ "\n",
    assert_pages({error, enotsup}, Text, ["j\n", "q\n"], [First, "43\n"]).

assert_pages(Rows, Text, Keys, Pages) ->
    {none, Frames} = capture_frames(Rows, {Text, undefined, #{}, "footer\n"}, Keys),
    ?assertEqual([frame(undefined, Page, "footer\n") || Page <- Pages], Frames).

capture_frames(Rows, Input, Keys) ->
    {Result, Output} = observer_cli_test_io:capture_with_geometry(Rows, 80, Keys, fun() ->
        less_client:main(less_client:init(Input))
    end),
    [<<>> | Frames] = binary:split(unicode:characters_to_binary(Output), ?CLEAR, [global]),
    {Result, Frames}.

frame(Header, Page, Footer) ->
    H =
        case Header of
            undefined -> [];
            _ -> Header
        end,
    F =
        case Footer of
            undefined -> less_client:render_footer(#{});
            _ -> Footer
        end,
    unicode:characters_to_binary([H, Page, F]).

-endif.
