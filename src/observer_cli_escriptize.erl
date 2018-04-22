-module(observer_cli_escriptize).

-export([main/1]).

%% @doc escript main
-spec main(list()) -> no_return.

-define(BEAM_MODS, [
    recon, recon_alloc, recon_lib, recon_trace,
    observer_cli, observer_cli_ets, observer_cli_lib, observer_cli_process,
    observer_cli_application, observer_cli_help, observer_cli_mnesia, observer_cli_store,
    observer_cli_escriptize, observer_cli_inet, observer_cli_port, observer_cli_system
    ]).

main([TargetNode, Cookie]) ->
    TargetNodeAtom = list_to_atom(TargetNode),
    CookieAtom = list_to_atom(Cookie),
    case observer_cli:start(TargetNodeAtom, CookieAtom) of
        {badrpc, _} ->
            remote_load(TargetNodeAtom),
            io:format("~p~n", [observer_cli:start(TargetNodeAtom, CookieAtom)]);
        _ -> ok
    end;
main(_Options) ->
    io:format("Usage: observer_cli <TARGETNODE> <TARGETCOOKIE>~n").

remote_load(Node) ->
    [begin recon:remote_load([Node], Mod) end|| Mod <- ?BEAM_MODS].