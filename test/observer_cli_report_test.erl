-module(observer_cli_report_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

header_and_safety_test() ->
    Response = (response(<<"process">>, #{<<"status">> => <<"not_found">>}))#{
        <<"outcome">> := <<"partial">>,
        <<"issues">> => [#{<<"message">> => <<"bad\e[31m\nline">>}],
        <<"meta">> := #{
            <<"target">> => #{<<"node">> => <<"node-1">>},
            <<"capture">> => #{
                <<"duration_ms">> => 1001,
                <<"probes">> => [
                    #{
                        <<"id">> => <<"detail">>,
                        <<"status">> => <<"error">>,
                        <<"required">> => true,
                        <<"reason_code">> => <<"gone">>
                    }
                ],
                <<"observer_effects">> => [<<"controller">>]
            }
        }
    },
    Text = observer_cli_report:render(Response),
    includes(Text, [
        <<"process | outcome=partial\n">>,
        <<"target: node=node-1">>,
        <<"duration_ms=1001">>,
        <<"required=true">>,
        <<"reason_code=gone">>,
        <<"not_found">>,
        <<"bad\\x1B[31m\\x0Aline">>
    ]),
    ?assertEqual(nomatch, binary:match(Text, <<27>>)).

diagnostic_rule_scope_test() ->
    lists:foreach(
        fun({Ruleset, Text}) ->
            Report = observer_cli_report:render(
                response(<<"diagnose">>, #{<<"ruleset">> => Ruleset})
            ),
            includes(Report, [Text])
        end,
        [
            {<<"observer_cli.quick">>, <<"limit pressure only">>},
            {<<"observer_cli.observation">>, <<"not root causes">>}
        ]
    ).

memory_report_test() ->
    Data = #{
        <<"memory">> => #{
            <<"beam">> => #{
                <<"total_bytes">> => 1000,
                <<"processes_bytes">> => 200,
                <<"observer_contaminated">> => true
            },
            <<"persistent_term">> => #{<<"count">> => 2, <<"memory_bytes">> => 16},
            <<"allocator">> => #{<<"secret_allocator_detail">> => 99}
        }
    },
    Text = observer_cli_report:render(response(<<"memory">>, Data)),
    includes(Text, [<<"not host RSS">>, <<"total_bytes: 1000">>, <<"processes_bytes: 200">>]),
    ?assertEqual(nomatch, binary:match(Text, <<"secret_allocator_detail">>)).

diagnostic_order_test() ->
    Finding = #{
        <<"id">> => <<"process_limit">>,
        <<"severity">> => <<"critical">>,
        <<"summary">> => <<"Process limit pressure">>,
        <<"recommendations">> => [<<"Inspect process rankings">>]
    },
    Data = #{
        <<"summary">> => <<"Partial with one finding">>,
        <<"findings">> => [Finding],
        <<"ruleset">> => <<"observer_cli.quick">>,
        <<"ruleset_version">> => 1,
        <<"sampling_plan">> => #{<<"samples">> => 2},
        <<"next_actions">> => [
            #{
                <<"id">> => <<"processes">>,
                <<"argv">> => [<<"processes">>, <<"--sort">>, <<"memory">>]
            }
        ],
        <<"skipped">> => [<<"not_calibrated">>],
        <<"context">> => #{<<"snapshot">> => #{<<"memory">> => #{<<"total_bytes">> => 1}}}
    },
    Text = observer_cli_report:render(response(<<"diagnose">>, Data)),
    Positions = [
        begin
            {P, _} = binary:match(Text, K),
            P
        end
     || K <-
            [<<"Summary:">>, <<"Findings:">>, <<"Coverage:">>, <<"Next steps">>, <<"Key context:">>]
    ],
    ?assertEqual(lists:sort(Positions), Positions),
    includes(Text, [
        <<"critical process_limit">>,
        <<"not proof of node health">>,
        <<"Inspect process rankings">>,
        <<"not_calibrated">>
    ]),
    includes(observer_cli_report:render(response(<<"diagnose">>, #{})), [<<"  none">>]),
    includes(observer_cli_report:render(response(<<"diagnose">>, null)), [<<"No diagnostic data">>]).

list_reports_test_() ->
    [
        {binary_to_list(Command), fun() ->
            Data = inventory(Sort, #{Id => <<"identity-1">>, Key => 42, <<"status">> => <<"ok">>}),
            Text = observer_cli_report:render(response(Command, Data)),
            includes(Text, [
                <<"sort=">>,
                <<"sort_semantics=current">>,
                <<"returned_count=1">>,
                <<"identity-1">>,
                <<"42">>,
                Label
            ])
        end}
     || {Command, Id, Sort, Key, Label} <- [
            {<<"processes">>, <<"pid">>, <<"memory">>, <<"memory_bytes">>, <<"memory_bytes">>},
            {<<"applications">>, <<"application">>, <<"memory">>, <<"memory_bytes">>,
                <<"application">>},
            {<<"ets">>, <<"table_id">>, <<"size">>, <<"size">>, <<"size">>},
            {<<"mnesia">>, <<"table">>, <<"memory">>, <<"memory_bytes">>, <<"table">>},
            {<<"network">>, <<"resource">>, <<"oct">>, <<"oct">>, <<"oct (bytes)">>},
            {<<"ports">>, <<"resource">>, <<"queue_size">>, <<"queue_size">>,
                <<"queue_size (bytes)">>},
            {<<"sockets">>, <<"resource">>, <<"io">>, <<"io">>, <<"io (bytes)">>}
        ]
    ].

window_and_unavailable_test() ->
    Data = (inventory(<<"memory">>, #{
        <<"pid">> => <<"<0.123.0>">>,
        <<"memory_bytes">> => 50,
        <<"memory_delta">> => -20
    }))#{
        <<"sort_semantics">> := <<"delta">>,
        <<"interval_ms">> => 1234,
        <<"reset_count">> => 1,
        <<"born_count">> => 2
    },
    Text = observer_cli_report:render(response(<<"processes">>, Data)),
    includes(Text, [
        <<"memory_delta (bytes)">>, <<"-20">>, <<"interval_ms=1234">>, <<"reset_count=1">>
    ]),
    Unavailable = inventory(<<"io">>, #{
        <<"resource">> => <<"socket-1">>,
        <<"io">> => null,
        <<"metric_states">> => #{<<"io">> => <<"unsupported">>}
    }),
    includes(observer_cli_report:render(response(<<"sockets">>, Unavailable)), [
        <<"null">>, <<"unsupported">>
    ]).

width_unicode_and_identifiers_test() ->
    Id = binary:copy(<<"long-identifier">>, 12),
    Label = unicode:characters_to_binary(lists:duplicate(60, 16#4E2D)),
    Data = inventory(<<"reductions">>, #{
        <<"pid">> => Id,
        <<"reductions">> => 0,
        <<"label">> => Label,
        <<"name">> => <<"worker">>
    }),
    Narrow = observer_cli_report:render(response(<<"processes">>, Data), 80),
    Wide = observer_cli_report:render(response(<<"processes">>, Data), 120),
    includes(Narrow, [Id, <<"0">>]),
    includes(Wide, [Id, <<"...">>, <<"worker">>]),
    ?assertEqual(nomatch, binary:match(Narrow, <<"label">>)),
    ?assert(is_list(unicode:characters_to_list(Wide))),
    ?assertEqual(Narrow, observer_cli_report:render(response(<<"processes">>, Data))).

bounded_projection_test() ->
    Trace = #{
        <<"trace_complete">> => false,
        <<"cleanup_confirmed">> => false,
        <<"events">> => lists:seq(1, 30),
        <<"truncated">> => true
    },
    Text = observer_cli_report:render(response(<<"trace_call">>, #{<<"trace">> => Trace})),
    includes(Text, [
        <<"trace_complete: false">>, <<"cleanup_confirmed: false">>, <<"22 additional entries">>
    ]),
    includes(
        observer_cli_report:render(response(<<"snapshot">>, #{<<"values">> => lists:seq(1, 12)})),
        [<<"4 additional entries">>]
    ),
    includes(observer_cli_report:render(response(<<"snapshot">>, lists:seq(1, 10))), [
        <<"2 additional entries">>
    ]),
    includes(
        observer_cli_report:render(
            response(<<"snapshot">>, #{<<"nested">> => #{<<"nested">> => #{<<"value">> => 1}}})
        ),
        [<<"fields; details in verbose">>]
    ),
    includes(
        observer_cli_report:render(
            response(
                <<"snapshot">>,
                #{<<"nested">> => #{<<"values">> => lists:seq(1, 12)}}
            )
        ),
        [<<"12 items">>]
    ),
    includes(observer_cli_report:render(response(<<"processes">>, #{<<"items">> => []})), [
        <<"No resource rows">>
    ]),
    includes(
        observer_cli_report:render(#{<<"data">> => null, <<"meta">> => #{<<"capture">> => null}}),
        [<<"outcome=error">>, <<"No data returned">>, <<"coverage unavailable">>]
    ).

distribution_observations_test() ->
    Data = #{
        <<"connected_peer_count">> => 1,
        <<"truncated">> => false,
        <<"controller_queues">> => [
            #{
                <<"peer">> => <<"peer-1">>,
                <<"status">> => <<"available">>,
                <<"observed_queue_size_bytes">> => 0,
                <<"busy_limit_bytes">> => 1048576,
                <<"health_inference">> => <<"unavailable">>
            }
        ]
    },
    includes(
        observer_cli_report:render(response(<<"distribution">>, Data)),
        [<<"peer-1">>, <<"observed_queue_size_bytes">>, <<"not buffer utilization or health">>]
    ).

missing_metric_and_sort_variants_test() ->
    lists:foreach(
        fun(Sort) ->
            includes(
                observer_cli_report:render(
                    response(<<"processes">>, inventory(Sort, #{<<"pid">> => <<"pid-1">>}))
                ),
                [<<"pid-1">>]
            )
        end,
        [<<"binary_memory">>, <<"total_heap_size">>]
    ),
    includes(
        observer_cli_report:render(
            response(<<"processes">>, #{<<"items">> => [#{<<"pid">> => <<"pid-1">>}]})
        ),
        [<<"pid-1">>]
    ).

response(Command, Data) ->
    #{
        <<"command">> => Command,
        <<"outcome">> => <<"complete">>,
        <<"data">> => Data,
        <<"meta">> => #{<<"target">> => #{<<"node">> => <<"node-1">>}, <<"capture">> => #{}}
    }.

inventory(Sort, Item) ->
    #{
        <<"items">> => [Item],
        <<"sort">> => Sort,
        <<"sort_semantics">> => <<"current">>,
        <<"returned_count">> => 1,
        <<"eligible_count">> => 1,
        <<"scanned_count">> => 1,
        <<"dropped_count">> => 0,
        <<"truncated">> => false,
        <<"complete">> => true
    }.

includes(Text, Parts) ->
    lists:foreach(fun(Part) -> ?assertNotEqual(nomatch, binary:match(Text, Part)) end, Parts).
-endif.
