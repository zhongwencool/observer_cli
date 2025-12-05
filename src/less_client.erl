-module(less_client).

-export([init/1, main/1]).

-include("observer_cli.hrl").

%%--------------------------------------------------------------------
%% @doc
-spec init(Input :: string()) ->
    LessServer :: pid().
%%--------------------------------------------------------------------
init(Input) ->
    %% We must save 1 line for status render
    {ok, LessServer} = less_server:start_link(Input, less_server:lines() - 1),
    LessServer.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec main(LessServer :: pid()) ->
    ok.
%%--------------------------------------------------------------------
main(LessServer) ->
    handle_current_page(LessServer),
    loop(LessServer).
%%--------------------------------------------------------------------

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
            ok;
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

handle_quit(LessServer) ->
    less_server:stop(LessServer),
    ?output(?CLEAR).

handle_page(Page) ->
    ?output(?CLEAR),
    ?output([Page]),
    ?output([render_last_line()]).

render_last_line() ->
    unicode:characters_to_binary([
        <<"|">>,
        ?GRAY_BG,
        <<"q(quit) F/j(next page) B/k(previous page)">>,
        ?RESET
    ]).
