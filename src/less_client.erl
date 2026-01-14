-module(less_client).

-export([init/1, main/1]).

-include("observer_cli.hrl").

%%--------------------------------------------------------------------
%% @doc
-spec init(
    Input ::
        string()
        | {string(), iolist() | undefined}
        | {string(), iolist() | undefined, map()}
        | {string(), iolist() | undefined, map(), iolist() | undefined}
) ->
    LessServer :: {pid(), iolist() | undefined, map(), iolist() | undefined}.
%%--------------------------------------------------------------------
init(Input) when is_list(Input) ->
    init({Input, undefined, #{}, undefined});
init({Input, Header}) ->
    init({Input, Header, #{}, undefined});
init({Input, Header, Nav}) ->
    init({Input, Header, Nav, undefined});
init({Input, Header, Nav, Footer}) ->
    %% We must save 1 line for footer and 1 line for menu
    Lines0 = less_server:lines() - footer_lines(Footer) - header_lines(Header),
    Lines = erlang:max(1, Lines0),
    {ok, LessServer} = less_server:start_link(Input, Lines),
    {LessServer, Header, Nav, Footer}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec main(LessServer :: {pid(), iolist() | undefined, map(), iolist() | undefined}) ->
    atom().
%%--------------------------------------------------------------------
main(State) ->
    handle_current_page(State),
    loop(State).
%%--------------------------------------------------------------------

loop(State) ->
    Nav = nav(State),
    case io:get_line("") of
        eof ->
            handle_quit(State);
        {error, _Reason} ->
            handle_quit(State);
        Key0 ->
            Key = normalize_key(Key0),
            case maps:find(Key, Nav) of
                {ok, Action} ->
                    handle_nav(State, Action);
                error ->
                    handle_key(Key, State)
            end
    end.

handle_key("j\n", State) ->
    handle_next_page(State),
    loop(State);
handle_key("F\n", State) ->
    handle_next_page(State),
    loop(State);
handle_key("k\n", State) ->
    handle_prev_page(State),
    loop(State);
handle_key("B\n", State) ->
    handle_prev_page(State),
    loop(State);
handle_key("q\n", State) ->
    handle_quit(State);
handle_key("Q\n", State) ->
    handle_quit(State);
handle_key(_Key, State) ->
    handle_current_page(State),
    loop(State).

handle_current_page({LessServer, _Header, _Nav, _Footer} = State) ->
    handle_page(State, less_server:page(LessServer)).

handle_next_page({LessServer, _Header, _Nav, _Footer} = State) ->
    handle_page(State, less_server:next(LessServer)).

handle_prev_page({LessServer, _Header, _Nav, _Footer} = State) ->
    handle_page(State, less_server:prev(LessServer)).

handle_quit({LessServer, _Header, _Nav, _Footer}) ->
    less_server:stop(LessServer),
    ?output(?CLEAR),
    none.

normalize_key(Key) when is_list(Key) ->
    lists:filter(fun(C) -> C =/= $\r end, Key).

handle_nav({LessServer, _Header, _Nav, _Footer}, Action) ->
    less_server:stop(LessServer),
    ?output(?CLEAR),
    Action.

handle_page({_LessServer, Header, Nav, Footer}, Page) ->
    ?output(?CLEAR),
    maybe_output_header(Header),
    ?output([Page]),
    maybe_output_footer(Footer, Nav).

render_last_line(Nav) ->
    PrevKey =
        case maps:is_key("B\n", Nav) of
            true -> <<"k(previous page)">>;
            false -> <<"B/k(previous page)">>
        end,
    unicode:characters_to_binary([
        <<"|">>,
        ?GRAY_BG,
        <<"q(quit) F/j(next page) ">>,
        PrevKey,
        ?RESET,
        <<"\n">>
    ]).

header_lines(undefined) -> 0;
header_lines(_Header) -> 1.

footer_lines(_Footer) -> 1.

maybe_output_header(undefined) ->
    ok;
maybe_output_header(Header) ->
    ?output([Header]).

maybe_output_footer(undefined, Nav) ->
    ?output([render_last_line(Nav)]);
maybe_output_footer(Footer, _Nav) ->
    ?output([Footer]).

nav({_LessServer, _Header, Nav, _Footer}) ->
    Nav.
