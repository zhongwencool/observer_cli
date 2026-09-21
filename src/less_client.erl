-module(less_client).

-export([init/1, main/1]).

-include("observer_cli.hrl").

-record(page, {
    buf :: [string()],
    lines :: pos_integer(),
    position = 0 :: non_neg_integer()
}).

-type state() :: {#page{}, iolist() | undefined, map(), iolist() | undefined}.

-ifdef(TEST).
-export([normalize_key/1, header_lines/1, footer_lines/1, render_footer/1]).

-endif.

%%--------------------------------------------------------------------
%% @doc
-spec init(
    Input ::
        string()
        | {string(), iolist() | undefined}
        | {string(), iolist() | undefined, map()}
        | {string(), iolist() | undefined, map(), iolist() | undefined}
) ->
    state().
%%--------------------------------------------------------------------
init(Input) when is_list(Input) ->
    init({Input, undefined, #{}, undefined});
init({Input, Header}) ->
    init({Input, Header, #{}, undefined});
init({Input, Header, Nav}) ->
    init({Input, Header, Nav, undefined});
init({Input, Header, Nav, Footer}) ->
    %% We must save 1 line for footer and 1 line for menu
    Lines0 = terminal_lines() - footer_lines(Footer) - header_lines(Header),
    Lines = erlang:max(1, Lines0),
    Page = #page{buf = string:split(Input, "\n", all), lines = Lines},
    {Page, Header, Nav, Footer}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec main(State :: state()) ->
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
            none;
        {error, _Reason} ->
            none;
        Key0 ->
            Key = normalize_key(Key0),
            case maps:find(Key, Nav) of
                {ok, Action} ->
                    handle_nav(Action);
                error ->
                    handle_key(Key, State)
            end
    end.

handle_key(Key, State) when Key =:= "j\n"; Key =:= "F\n" ->
    main(next_page(State));
handle_key(Key, State) when Key =:= "k\n"; Key =:= "B\n" ->
    main(prev_page(State));
handle_key(Key, _State) when Key =:= "q\n"; Key =:= "Q\n" ->
    none;
handle_key(_Key, State) ->
    main(State).

handle_current_page({#page{buf = Buf, lines = Lines, position = Position}, _, _, _} = State) ->
    Page = string:join(lists:sublist(Buf, Position + 1, Lines), "\n") ++ "\n",
    handle_page(State, Page).

next_page(
    {#page{buf = Buf, lines = Lines, position = Position} = Page, Header, Nav, Footer} = State
) ->
    NewPosition = Position + Lines,
    case NewPosition >= length(Buf) of
        true -> State;
        false -> {Page#page{position = NewPosition}, Header, Nav, Footer}
    end.

prev_page({#page{lines = Lines, position = Position} = Page, Header, Nav, Footer}) ->
    {Page#page{position = erlang:max(0, Position - Lines)}, Header, Nav, Footer}.

terminal_lines() ->
    case io:rows() of
        {ok, Rows} -> Rows;
        _ -> 43
    end.

normalize_key(Key) when is_list(Key) ->
    lists:filter(fun(C) -> C =/= $\r end, Key).

handle_nav(Action) ->
    case Action of
        quit -> ok;
        _ -> ?output(?CLEAR)
    end,
    Action.

handle_page({_Page, Header, Nav, Footer}, Page) ->
    ?output(?CLEAR),
    maybe_output_header(Header),
    ?output([Page]),
    maybe_output_footer(Footer, Nav).

render_footer(Nav) ->
    PrevKey =
        case maps:is_key("B\n", Nav) of
            true -> "k(previous page)";
            false -> "B/k(previous page)"
        end,
    unicode:characters_to_binary(
        observer_cli_lib:render_footer(["q(quit) F/j(next page) ", PrevKey])
    ).

header_lines(undefined) -> 0;
header_lines(_Header) -> 1.

footer_lines(_Footer) -> 1.

maybe_output_header(undefined) ->
    ok;
maybe_output_header(Header) ->
    ?output([Header]).

maybe_output_footer(undefined, Nav) ->
    ?output([render_footer(Nav)]);
maybe_output_footer(Footer, _Nav) ->
    ?output([Footer]).

nav({_Page, _Header, Nav, _Footer}) ->
    Nav.
