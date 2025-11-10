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
            ?output(?CLEAR),
            Page = less_server:next(LessServer),
            ?output([Page]),
            loop(LessServer);
        "k\n" ->
            ?output(?CLEAR),
            Page = less_server:prev(LessServer),
            ?output([Page]),
            loop(LessServer);
        "q\n" ->
            ?output(?CLEAR),
            LessServer;
        _ ->
            loop(LessServer)
    end.

