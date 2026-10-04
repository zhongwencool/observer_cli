-module(observer_cli_lifecycle_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

trace_dispatch_lost_response_test() ->
    Started = node() =:= nonode@nohost,
    case Started of
        true ->
            {ok, _} = net_kernel:start([
                list_to_atom(peer:random_name("observer_cli_loss")), shortnames
            ]);
        false ->
            ok
    end,
    try
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_loss_target")}),
        try
            Forms = [
                {attribute, 1, module, observer_cli_snapshot},
                {attribute, 2, export, [{dispatch, 4}]},
                {function, 3, dispatch, 4, [
                    {clause, 3, [{var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}], [], [
                        {call, 3, {atom, 3, exit}, [{atom, 3, simulated_response_loss}]}
                    ]}
                ]}
            ],
            {ok, observer_cli_snapshot, Beam} = compile:forms(Forms, [binary]),
            {module, observer_cli_snapshot} = erpc:call(
                Node,
                code,
                load_binary,
                [observer_cli_snapshot, "lost_response_fixture", Beam]
            ),
            ?assertEqual(
                {error, cleanup, cleanup_unconfirmed},
                observer_cli_escriptize:target_dispatch(Node, trace, #{}, #{}, include, 1000)
            ),
            ?assertEqual(
                {error, required_probe, target_dispatch_failed},
                observer_cli_escriptize:target_dispatch(Node, memory, #{}, #{}, include, 1000)
            ),
            ?assertEqual(
                4,
                observer_cli_result:exit_code(
                    observer_cli_result:error(<<"trace call">>, cleanup, cleanup_unconfirmed), #{}
                )
            )
        after
            peer:stop(Peer)
        end
    after
        case Started of
            true -> net_kernel:stop();
            false -> ok
        end
    end.

-endif.
