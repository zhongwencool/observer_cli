-module(less_server_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    [
        {"simple test", fun simple/0},
        {"next page", fun next_page/0},
        {"last page", fun last_page/0},
        {"next page after last", fun next_page_after_last/0},
        {"previous page before first", fun previous_page_before_first/0}
    ].

simple() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    ?assertEqual("a\nb\n", less_server:page(LessServer)),

    less_server:stop(LessServer).

next_page() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    ?assertEqual("b\nc\n", less_server:next(LessServer)),

    less_server:stop(LessServer).

last_page() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    less_server:next(LessServer),
    less_server:next(LessServer),
    ?assertEqual("c\nd\n", less_server:next(LessServer)),

    less_server:stop(LessServer).

next_page_after_last() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    less_server:next(LessServer),
    less_server:next(LessServer),
    less_server:next(LessServer),
    ?assertEqual("c\nd\n", less_server:next(LessServer)),

    less_server:stop(LessServer).

previous_page_before_first() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    ?assertEqual("a\nb\n", less_server:prev(LessServer)),

    less_server:stop(LessServer).

-endif.
