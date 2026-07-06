-module(less_server_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    [
        {"simple test", fun simple/0},
        {"start_link default", fun start_link_default/0},
        {"next page", fun next_page/0},
        {"last page", fun last_page/0},
        {"next page after last", fun next_page_after_last/0},
        {"previous page before first", fun previous_page_before_first/0},
        {"previous page after next", fun previous_page_after_next/0},
        {"previous page from later page", fun previous_page_from_later_page/0},
        {"ignores unknown messages", fun ignores_unknown_messages/0},
        {"lines minimum", fun lines_minimum/0}
    ].

simple() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    ?assertEqual("a\nb\n", less_server:page(LessServer)),

    less_server:stop(LessServer).

start_link_default() ->
    Lines = erlang:max(1, less_server:lines()),
    {ok, LessServer} = less_server:start_link("a\nb"),

    Expected =
        case Lines >= 2 of
            true -> "a\nb\n";
            false -> "a\n"
        end,
    ?assertEqual(Expected, less_server:page(LessServer)),

    less_server:stop(LessServer).

next_page() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    ?assertEqual("c\nd\n", less_server:next(LessServer)),

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

previous_page_after_next() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd", 2),

    less_server:next(LessServer),
    ?assertEqual("a\nb\n", less_server:prev(LessServer)),

    less_server:stop(LessServer).

previous_page_from_later_page() ->
    {ok, LessServer} = less_server:start_link("a\nb\nc\nd\ne\nf", 2),

    less_server:next(LessServer),
    less_server:next(LessServer),
    ?assertEqual("c\nd\n", less_server:prev(LessServer)),

    less_server:stop(LessServer).

ignores_unknown_messages() ->
    {ok, LessServer} = less_server:start_link("a\nb", 2),

    ?assertEqual(ok, gen_server:call(LessServer, unexpected)),
    gen_server:cast(LessServer, unexpected),
    LessServer ! unexpected,
    timer:sleep(10),
    ?assertEqual("a\nb\n", less_server:page(LessServer)),

    less_server:stop(LessServer).

lines_minimum() ->
    {ok, LessServer} = less_server:start_link("a\nb\n", 0),

    ?assertEqual("a\n", less_server:page(LessServer)),

    less_server:stop(LessServer).

-endif.
