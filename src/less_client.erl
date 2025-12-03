-module(less_client).

-export([main/1]).

-include("observer_cli.hrl").

main(LessServer) ->
    handle_current_page(LessServer),
    loop(LessServer).

loop(LessServer) ->
    case io:get_line("") of
        "j\n" ->
            handle_next_page(LessServer),
            loop(LessServer);
        "F\n" ->
            handle_next_page(LessServer),
            loop(LessServer);
        "k\n" ->
            handle_prev_page(LessServer),
            loop(LessServer);
        "B\n" ->
            handle_prev_page(LessServer),
            loop(LessServer);
        "q\n" ->
            handle_quit(LessServer),
            LessServer;
        _ ->
            handle_current_page(LessServer),
            loop(LessServer)
    end.

handle_current_page(LessServer) ->
    handle_page(less_server:page(LessServer)).

handle_next_page(LessServer) ->
    handle_page(less_server:next(LessServer)).

handle_prev_page(LessServer) ->
    handle_page(less_server:prev(LessServer)).

handle_page(Page) ->
    ?output(?CLEAR),
    ?output([Page]),
    ?output([render_last_line()]).

handle_quit(_LessServer) -> ?output(?CLEAR).

render_last_line() ->
    unicode:characters_to_binary([
        <<"|">>,
        ?GRAY_BG,
        <<"q(quit) F/j(next page) B/k(previous page)">>,
        ?RESET
    ]).
