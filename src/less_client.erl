-module(less_client).

-export([main/1]).

-include("observer_cli.hrl").

main(LessServer) ->
    ?output(?CLEAR),
    Page = less_server:page(LessServer),
    ?output([Page]),
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
            loop(LessServer)
    end.

handle_next_page(LessServer) ->
    ?output(?CLEAR),
    Page = less_server:next(LessServer),
    ?output([Page]).

handle_prev_page(LessServer) ->
    ?output(?CLEAR),
    Page = less_server:prev(LessServer),
    ?output([Page]).

handle_quit(_LessServer) -> ?output(?CLEAR).

