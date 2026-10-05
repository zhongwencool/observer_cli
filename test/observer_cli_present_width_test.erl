-module(observer_cli_present_width_test).
-include_lib("eunit/include/eunit.hrl").

long_target_wraps_before_screen_budget_test() ->
    Node = <<(binary:copy(<<"n">>, 200))/binary, "@127.0.0.1">>,
    Response = response(Node),
    Text = observer_cli_present:render(Response, 80),
    assert_screen(Text),
    Joined = binary:replace(Text, <<"\n">>, <<>>, [global]),
    ?assertNotEqual(nomatch, binary:match(Joined, Node)),
    Crowded = Response#{<<"summary">> := binary:copy(<<"bounded evidence ">>, 150)},
    CrowdedText = observer_cli_present:render(Crowded, 80),
    assert_screen(CrowdedText),
    ?assertNotEqual(nomatch, binary:match(CrowdedText, <<"Additional evidence and next steps:">>)).

long_utf8_token_preserves_codepoints_test() ->
    Token = unicode:characters_to_binary(lists:duplicate(201, 16#E9)),
    Text = observer_cli_present:render(response(Token), 80),
    assert_screen(Text),
    ?assert(is_list(unicode:characters_to_list(Text))),
    Joined = binary:replace(Text, <<"\n">>, <<>>, [global]),
    ?assertNotEqual(nomatch, binary:match(Joined, Token)).

wide_and_combining_tokens_preserve_screen_bound_test() ->
    lists:foreach(
        fun(Characters) ->
            Token = unicode:characters_to_binary(Characters),
            Text = observer_cli_present:render(response(Token), 80),
            assert_screen(Text),
            Joined = binary:replace(Text, <<"\n">>, <<>>, [global]),
            ?assertNotEqual(nomatch, binary:match(Joined, Token))
        end,
        [lists:duplicate(121, 16#754C), lists:flatten(lists:duplicate(61, [$e, 16#301]))]
    ).

response(Node) ->
    (observer_cli_result:local(<<"check">>, #{<<"context">> => #{}}))#{
        <<"meta">> := #{<<"target">> => #{<<"node">> => Node}, <<"capture">> => null}
    }.

assert_screen(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    ?assert(length(Lines) - 1 =< 24),
    ?assert(lists:all(fun(Line) -> column_bound(Line) =< 80 end, Lines)).

column_bound(Line) ->
    lists:sum([
        case C < 128 of
            true -> 1;
            false -> 2
        end
     || C <- unicode:characters_to_list(Line)
    ]).
