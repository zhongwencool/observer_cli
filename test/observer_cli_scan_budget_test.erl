-module(observer_cli_scan_budget_test).

-include_lib("eunit/include/eunit.hrl").

process_window_readmits_before_second_scan_test() ->
    Parent = self(),
    Source = #{
        count_fun => fun() ->
            case get(grown) of
                true -> 100001;
                _ -> 1
            end
        end,
        fold =>
            {bounded_process_list, fun(_Fun, Acc) ->
                Parent ! enumerated,
                Acc
            end},
        sleep_fun => fun(_) ->
            put(grown, true),
            ok
        end,
        monotonic_fun => fun() -> 0 end
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Report} = observer_cli_snapshot:dispatch(
        self(),
        processes,
        #{duration_ms => 250, test_process_source => Source},
        #{timeout_ms => 5000, identifier_policy => include}
    ),
    ?assertEqual(<<"error">>, maps:get(<<"outcome">>, Report)),
    ?assertEqual(ok, observer_cli_escriptize:validate_response(processes, include, node(), Report)),
    ?assertEqual(
        <<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, maps:get(<<"data">>, Report))
    ),
    receive
        enumerated -> ok
    after 1000 -> error(first_scan_missing)
    end,
    receive
        enumerated -> error(second_scan_should_be_refused)
    after 0 -> ok
    end.

process_iterator_caps_callbacks_when_count_grows_test() ->
    put(scan_callbacks, 0),
    Source = #{
        count_fun => fun() -> 1 end,
        scan_budget_count => 2,
        fold => {bounded_process_list, fun(Fun, Acc) -> lists:foldl(Fun, Acc, [a, b, c]) end},
        info_fun => fun(_, _) ->
            put(scan_callbacks, get(scan_callbacks) + 1),
            undefined
        end,
        monotonic_fun => fun() -> 0 end
    },
    try
        ?assertThrow(
            {scan_budget_exceeded, #{status := unavailable, admission_stage := post_enumeration}},
            observer_cli_snapshot:collect_process_sample(memory, Source, #{})
        ),
        ?assertEqual(2, get(scan_callbacks))
    after
        erase(scan_callbacks)
    end.

counter_window_readmits_before_second_scan_test() ->
    erase(grown),
    put(counter_scans, 0),
    Source = #{
        count_fun => fun() ->
            case get(grown) of
                true -> 100001;
                _ -> 0
            end
        end,
        all_fun => fun() ->
            put(counter_scans, get(counter_scans) + 1),
            {ok, []}
        end,
        io_fun => fun() -> {{input, 0}, {output, 0}} end,
        sleep_fun => fun(_) ->
            put(grown, true),
            ok
        end,
        monotonic_fun => fun() -> 0 end
    },
    try
        ?assertThrow(
            {scan_budget_exceeded, #{admission_stage := pre_enumeration}},
            observer_cli_snapshot:collect_counter_resources(network, Source, oct, 20, 250, #{})
        ),
        ?assertEqual(1, get(counter_scans))
    after
        erase(grown),
        erase(counter_scans)
    end.

counter_enumeration_growth_refuses_before_resource_queries_test() ->
    Source = #{
        count_fun => fun() -> 0 end,
        all_fun => fun() -> {ok, lists:duplicate(100001, resource)} end,
        name_fun => fun(_) -> error(resource_query_must_not_run) end,
        monotonic_fun => fun() -> 0 end
    },
    lists:foreach(
        fun({Command, Sort}) ->
            ?assertThrow(
                {scan_budget_exceeded, #{admission_stage := post_enumeration}},
                observer_cli_snapshot:collect_counter_resources(
                    Command, Source, Sort, 20, undefined, #{}
                )
            )
        end,
        [{network, oct}, {sockets, io}]
    ).
