-module(observer_cli_escriptize).

-export([main/1]).

%% @doc escript main
-spec main(list()) -> no_return.

main([TargetNode, Cookie]) ->
    TargetNodeAtom = list_to_atom(TargetNode),
    CookieAtom = list_to_atom(Cookie),
    observer_cli:start(TargetNodeAtom, CookieAtom);
main(_Options) ->
    io:format("Usage: observer_cli <TARGETNODE> <TARGETCOOKIE>~n").

