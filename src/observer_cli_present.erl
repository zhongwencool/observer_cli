%% Plain terminal reports: answer first, evidence next, metadata on demand.
-module(observer_cli_present).
-export([render/2]).

-spec render(map(), pos_integer()) -> binary().
render(#{<<"command">> := <<"inspect logs">>, <<"data">> := Data} = R, _Width) when is_map(Data) ->
    {ok, Safe} = observer_cli_capture:encode(text, R#{
        <<"schema">> := observer_cli_capture:schema(), <<"command">> := <<"logs">>
    }),
    binary:replace(Safe, <<"observer_cli logs\n">>, <<"observer_cli inspect logs\n">>);
render(#{<<"command">> := <<"describe">>, <<"data">> := Data} = R, _Width) when
    is_map_key(<<"name">>, Data); is_map_key(<<"commands">>, Data)
->
    %% Detailed discovery must retain every registry constraint and risk field.
    %% Reuse the full text encoder rather than duplicating the descriptor format.
    {ok, Text} = observer_cli_capture:encode(verbose, R),
    Text;
render(R, Width0) ->
    Width = max(40, Width0),
    Meta = maps:get(<<"meta">>, R),
    Command = maps:get(<<"command">>, R),
    Header = [
        line([
            <<"observer_cli ">>, command_text(Command), <<" | ">>, text(maps:get(<<"outcome">>, R))
        ]),
        target(Meta),
        window(Meta)
    ],
    Body =
        case maps:get(<<"assessment">>, R) of
            null -> inspect_body(Command, maps:get(<<"data">>, R));
            Assessment -> check_body(Command, maps:get(<<"data">>, R), Assessment)
        end,
    Tail = [
        missing(Meta),
        issue_lines([
            I
         || I <- maps:get(<<"issues">>, R),
            maps:get(<<"message">>, I) =/= maps:get(<<"summary">>, R)
        ]),
        next(maps:get(<<"next_actions">>, R)),
        <<"Details: --verbose (text), --json or --format term.">>
    ],
    Lines0 = Header ++ [maps:get(<<"summary">>, R)] ++ Body ++ Tail,
    Lines = flatten_lines(Lines0),
    Wrapped = lists:append([wrap(text(L), Width) || L <- Lines, L =/= <<>>]),
    Final =
        case maps:get(<<"assessment">>, R) of
            null -> Wrapped;
            _ -> screen_budget(Wrapped, 24)
        end,
    iolist_to_binary([[L, <<"\n">>] || L <- Final]).

target(#{<<"target">> := #{<<"node">> := Node}}) -> line([<<"Target: ">>, text(Node)]);
target(_) -> <<>>.
window(#{<<"capture">> := #{<<"duration_ms">> := Actual, <<"requested_window_ms">> := Requested}}) ->
    line([<<"Window: requested ">>, seconds(Requested), <<"; capture ">>, seconds(Actual)]);
window(#{<<"capture">> := #{<<"duration_ms">> := Actual}}) ->
    line([<<"Capture: ">>, seconds(Actual)]);
window(_) ->
    <<>>.

check_body(_Command, null, Assessment) ->
    [assessment_line(Assessment)];
check_body(Command, Data, Assessment) ->
    Context = maps:get(<<"context">>, Data, #{}),
    [
        assessment_line(Assessment),
        finding_lines(maps:get(<<"findings">>, Assessment)),
        evidence(Command, Context),
        <<"Not evaluated: application root cause; no findings is not a health certificate.">>
    ].

assessment_line(#{<<"status">> := Status}) -> line([<<"Assessment: ">>, Status]).
finding_lines(Findings) ->
    [
        [
            line([maps:get(<<"severity">>, F), <<": ">>, maps:get(<<"summary">>, F)])
         || F <- lists:sublist(Findings, 3)
        ],
        case length(Findings) > 3 of
            true -> <<"Additional findings are retained in --verbose and JSON.">>;
            false -> <<>>
        end
    ].

evidence(<<"check cpu">>, Context) ->
    [
        scheduler_evidence(Context),
        activity_evidence(Context),
        <<"Activity is reductions/s, not process CPU time.">>
    ];
evidence(<<"check memory">>, Context) ->
    [memory_evidence(Context), process_evidence(Context, <<"memory_bytes">>, <<"memory">>)];
evidence(<<"check mailbox">>, Context) ->
    process_evidence(Context, <<"message_queue_len">>, <<"mailbox messages">>);
evidence(<<"check connections">>, Context) ->
    connection_evidence(Context);
evidence(_Command, Context) ->
    [
        memory_evidence(Context),
        scheduler_evidence(Context),
        process_evidence(Context, <<"memory_bytes">>, <<"memory">>),
        mailbox_overview(Context)
    ].

mailbox_overview(Context) ->
    Current = maps:get(<<"current">>, Context, #{}),
    line([
        <<"Largest observed mailbox: ">>,
        number(maps:get(<<"mailbox_peak">>, Current, null)),
        <<" messages (excluding observer)">>
    ]).

memory_evidence(Context) ->
    Current = maps:get(<<"current">>, Context, #{}),
    Memory = maps:get(<<"memory">>, Current, #{}),
    Trends = maps:get(<<"trends">>, Context, #{}),
    Deltas = maps:get(<<"deltas">>, maps:get(<<"global_memory">>, Trends, #{}), #{}),
    [
        line([
            <<"BEAM memory: ">>,
            bytes(maps:get(<<"total_bytes">>, Memory, null)),
            <<"; change ">>,
            signed_bytes(maps:get(<<"total_bytes">>, Deltas, null)),
            <<" (not host RSS)">>
        ]),
        line([
            <<"Binary: ">>,
            bytes(maps:get(<<"binary_bytes">>, Memory, null)),
            <<"; ETS: ">>,
            bytes(maps:get(<<"ets_bytes">>, Memory, null)),
            <<"; categories can overlap">>
        ])
    ].

scheduler_evidence(Context) ->
    Windows = maps:get(<<"scheduler_windows">>, Context, []),
    [scheduler_pool(Pool, Windows) || Pool <- [<<"normal">>, <<"dirty_cpu">>]].
scheduler_pool(Pool, Windows) ->
    Values = [
        maps:get(<<"utilization_ratio">>, P)
     || W <- Windows,
        P <- [maps:get(Pool, W, #{})],
        maps:get(<<"status">>, P, null) =:= <<"available">>,
        is_number(maps:get(<<"utilization_ratio">>, P, null))
    ],
    case Values of
        [] ->
            line([<<"Scheduler ">>, Pool, <<": unavailable / not requested">>]);
        _ ->
            line([
                <<"Scheduler ">>,
                Pool,
                <<": peak ">>,
                number(lists:max(Values) * 100),
                <<"% across measured windows">>
            ])
    end.

process_evidence(Context, Key, Label) ->
    Current = maps:get(<<"current">>, Context, #{}),
    Processes = maps:get(<<"processes">>, Current, #{}),
    Items = maps:get(<<"items">>, Processes, []),
    [
        line([
            <<"Process ">>,
            text(maps:get(<<"pid">>, I, null)),
            <<": ">>,
            Label,
            <<" ">>,
            metric_value(<<"inspect process">>, Key, maps:get(Key, I, null))
        ])
     || I <- lists:sublist(Items, 3)
    ].

activity_evidence(Context) ->
    Activity = maps:get(<<"hot_processes_by_reductions">>, Context, #{}),
    [
        line([
            <<"Process ">>,
            text(maps:get(<<"pid">>, I, maps:get(<<"id">>, I, null))),
            <<": ">>,
            number(maps:get(<<"reductions_per_second">>, I, null)),
            <<" reductions/s">>
        ])
     || I <- lists:sublist(maps:get(<<"items">>, Activity, []), 3)
    ].

connection_evidence(Context) ->
    Distribution = maps:get(<<"distribution">>, Context, #{}),
    Trends = maps:get(<<"trends">>, Context, #{}),
    Sockets = maps:get(<<"sockets">>, Trends, #{}),
    [
        line([
            <<"Erlang peers: ">>,
            number(maps:get(<<"connected_peer_count">>, Distribution, null)),
            <<"; state ">>,
            text(maps:get(<<"state">>, Distribution, null))
        ]),
        line([
            <<"OTP socket trend: ">>,
            text(maps:get(<<"status">>, Sockets, null)),
            <<"; not all host or legacy inet traffic">>
        ]),
        [
            line([
                <<"Socket ">>,
                text(maps:get(<<"resource">>, I, null)),
                <<": window I/O ">>,
                bytes(maps:get(<<"io">>, I, null))
            ])
         || I <- lists:sublist(maps:get(<<"items">>, Sockets, []), 3)
        ]
    ].

inspect_body(_Command, null) ->
    [];
inspect_body(<<"describe">>, #{<<"entries">> := Entries}) ->
    [line([maps:get(<<"name">>, E), <<" - ">>, maps:get(<<"summary">>, E)]) || E <- Entries];
inspect_body(<<"inspect vm">>, Data) ->
    Runtime = maps:get(<<"runtime">>, Data, #{}),
    Memory = maps:get(<<"beam">>, maps:get(<<"memory">>, Data, #{}), #{}),
    Resources = maps:get(<<"resources">>, Data, #{}),
    [
        line([
            <<"OTP: ">>,
            text(maps:get(<<"otp_release">>, Runtime, null)),
            <<"; architecture ">>,
            text(maps:get(<<"system_architecture">>, Runtime, null))
        ]),
        line([
            <<"BEAM memory: ">>,
            bytes(maps:get(<<"total_bytes">>, Memory, null)),
            <<" (not host RSS)">>
        ]),
        [
            resource_line(Key, maps:get(Key, Resources, #{}))
         || Key <- [<<"process">>, <<"port">>, <<"atom">>, <<"ets">>]
        ]
    ];
inspect_body(<<"inspect scheduler">>, Data) ->
    [scheduler_pool(Pool, [Data]) || Pool <- [<<"normal">>, <<"dirty_cpu">>]] ++
        [
            line([
                <<"Measured interval: ">>,
                number(maps:get(<<"interval_ms">>, Data, null)),
                <<"ms; queues are non-atomic observations">>
            ])
        ];
inspect_body(<<"inspect distribution">>, Data) ->
    [
        line([
            <<"Connected Erlang peers: ">>, number(maps:get(<<"connected_peer_count">>, Data, null))
        ]),
        [line([<<"Peer: ">>, text(Peer)]) || Peer <- maps:get(<<"connected_peers">>, Data, [])],
        <<"Queue observations are context, not buffer utilization or network health.">>
    ];
inspect_body(<<"inspect memory">>, Data) ->
    Memory = maps:get(<<"memory">>, Data, #{}),
    Beam = maps:get(<<"beam">>, Memory, #{}),
    Allocators = maps:get(<<"util_allocators">>, maps:get(<<"allocator">>, Memory, #{}), []),
    [
        [
            line([label(K), <<": ">>, bytes(V)])
         || {K, V} <- lists:sort(maps:to_list(Beam)), is_number(V)
        ],
        <<"Allocator averages (current blocks, not total allocations):">>,
        [
            line([
                text(maps:get(<<"allocator">>, A)),
                <<": MBCS ">>,
                bytes(maps:get(<<"current_mbcs_average_block_size_bytes">>, A, null)),
                <<"; SBCS ">>,
                bytes(maps:get(<<"current_sbcs_average_block_size_bytes">>, A, null))
            ])
         || A <- Allocators
        ],
        <<"BEAM categories can overlap; this is not host RSS.">>
    ];
inspect_body(Command, #{<<"trace">> := Trace}) when
    Command =:= <<"trace call">>; Command =:= <<"trace stop">>
->
    [
        line([
            <<"Node-global trace; cleanup confirmed: ">>,
            text(maps:get(<<"cleanup_confirmed">>, Trace, null))
        ]),
        line([
            <<"Trace complete: ">>,
            text(maps:get(<<"trace_complete">>, Trace, null)),
            <<"; reason ">>,
            text(maps:get(<<"reason">>, Trace, null))
        ]),
        <<"Events contain identities and offsets, never arguments or returns:">>,
        [
            line([
                number(maps:get(<<"offset_ms">>, E)),
                <<"ms | ">>,
                text(maps:get(<<"tracee">>, E)),
                <<" | ">>,
                scalar(maps:get(<<"mfa">>, E))
            ])
         || E <- maps:get(<<"events">>, Trace, [])
        ]
    ];
inspect_body(Command, #{<<"items">> := Items} = Data) ->
    inventory(Command, Data, Items);
inspect_body(<<"inspect process">>, Data) ->
    [
        line([label(K), <<": ">>, measurement(K, V)])
     || {K, V} <- lists:sort(
            maps:to_list(
                maps:with(
                    [
                        <<"pid">>,
                        <<"registered_name">>,
                        <<"status">>,
                        <<"memory_bytes">>,
                        <<"message_queue_len">>,
                        <<"reductions">>,
                        <<"current_function">>
                    ],
                    Data
                )
            )
        )
    ];
inspect_body(_Command, Data) ->
    [
        line([label(K), <<": ">>, scalar(V)])
     || {K, V} <- lists:sort(maps:to_list(Data)), not is_map(V), not is_list(V)
    ] ++
        [<<"Nested evidence is retained in --verbose and JSON.">>].

resource_line(Key, R) ->
    Count = maps:get(
        <<"observed_count_including_observer">>, R, maps:get(<<"observed_count">>, R, null)
    ),
    line([
        label(Key), <<": ">>, number(Count), <<" / limit ">>, number(maps:get(<<"limit">>, R, null))
    ]).

inventory(Command, Data, Items) ->
    Sort = maps:get(<<"sort">>, Data, <<"memory">>),
    Key = metric_key(Command, Sort),
    [
        line([
            <<"Rows: ">>,
            integer_to_binary(length(Items)),
            <<"; sort ">>,
            Sort,
            <<" (">>,
            text(maps:get(<<"sort_semantics">>, Data, null)),
            <<")">>
        ]),
        line([<<"Identity | Name | ">>, metric_label(Command, Key)]),
        [
            line([
                identity(I),
                <<" | ">>,
                text(maps:get(<<"registered_name">>, I, maps:get(<<"name">>, I, null))),
                <<" | ">>,
                metric_value(Command, Key, maps:get(Key, I, null))
            ])
         || I <- Items
        ],
        case Items of
            [] -> <<"No rows returned; review status and coverage before assuming absence.">>;
            _ -> <<>>
        end
    ].

metric_key(<<"inspect process">>, <<"memory">>) ->
    <<"memory_bytes">>;
metric_key(<<"inspect process">>, <<"memory-change">>) ->
    <<"memory_delta">>;
metric_key(<<"inspect process">>, <<"mailbox-change">>) ->
    <<"message_queue_len_delta">>;
metric_key(<<"inspect process">>, <<"reductions-rate">>) ->
    <<"reductions_per_second">>;
metric_key(<<"inspect process">>, <<"binary_memory">>) ->
    <<"binary_memory_bytes">>;
metric_key(<<"inspect process">>, <<"total_heap_size">>) ->
    <<"total_heap_size_bytes">>;
metric_key(<<"inspect process">>, <<"binary-memory-change">>) ->
    <<"binary_memory_delta">>;
metric_key(<<"inspect process">>, <<"heap-change">>) ->
    <<"total_heap_size_delta">>;
metric_key(<<"inspect port">>, <<"memory">>) ->
    <<"memory">>;
metric_key(_Command, <<"memory">>) ->
    <<"memory_bytes">>;
metric_key(_Command, Sort) ->
    case observer_cli_input:metric(binary_to_list(Sort)) of
        {Base, delta} -> list_to_binary(Base ++ "_delta");
        {Base, rate} -> list_to_binary(Base ++ "_per_second");
        _ -> Sort
    end.

command_text(null) -> <<>>;
command_text(Command) -> text(Command).

metric_label(Command, Key) ->
    BaseUnit = metric_unit(Command, Key),
    Suffix =
        case binary:match(Key, <<"_per_second">>) of
            nomatch -> BaseUnit;
            _ -> <<BaseUnit/binary, "/s">>
        end,
    line([label(Key), <<" (">>, Suffix, <<")">>]).

metric_value(Command, Key, Value) ->
    case metric_unit(Command, Key) of
        <<"bytes">> -> bytes(Value);
        _ -> scalar(Value)
    end.

metric_unit(<<"inspect network">>, Key) ->
    case binary:match(Key, <<"oct">>) of
        nomatch -> <<"packets">>;
        _ -> <<"bytes">>
    end;
metric_unit(<<"inspect socket">>, Key) ->
    case
        binary:match(Key, <<"bytes">>) =/= nomatch orelse binary:match(Key, <<"io">>) =/= nomatch
    of
        true -> <<"bytes">>;
        false -> <<"events">>
    end;
metric_unit(<<"inspect port">>, _Key) ->
    <<"bytes">>;
metric_unit(_Command, Key) ->
    case
        binary:match(Key, <<"memory">>) =/= nomatch orelse binary:match(Key, <<"heap">>) =/= nomatch
    of
        true ->
            <<"bytes">>;
        false ->
            case binary:match(Key, <<"reductions">>) of
                nomatch -> <<"count">>;
                _ -> <<"reductions">>
            end
    end.

identity(Item) ->
    text(
        maps:get(
            <<"pid">>,
            Item,
            maps:get(
                <<"resource">>,
                Item,
                maps:get(
                    <<"application">>,
                    Item,
                    maps:get(<<"table_id">>, Item, maps:get(<<"table">>, Item, null))
                )
            )
        )
    ).
measurement(Key, Value) ->
    case
        binary:match(Key, <<"memory">>) =/= nomatch orelse binary:match(Key, <<"heap">>) =/= nomatch orelse
            binary:match(Key, <<"bytes">>) =/= nomatch
    of
        true -> bytes(Value);
        false -> scalar(Value)
    end.
label(Key) -> binary:replace(Key, <<"_">>, <<" ">>, [global]).

missing(#{<<"capture">> := #{<<"probes">> := Probes}}) ->
    Failed = [
        P
     || P <- Probes,
        not lists:member(maps:get(<<"status">>, P), [<<"ok">>, <<"not_requested">>]),
        not lists:member(maps:get(<<"reason_code">>, P, null), [
            <<"not_requested">>, <<"deep_not_requested">>, <<"application_not_requested">>
        ])
    ],
    [
        line([
            <<"Missing: ">>,
            maps:get(<<"id">>, P),
            <<" (">>,
            text(maps:get(<<"reason_code">>, P, null)),
            <<")">>
        ])
     || P <- lists:sublist(Failed, 3)
    ];
missing(_) ->
    [].
issue_lines(Issues) -> [line([<<"Issue: ">>, maps:get(<<"message">>, I)]) || I <- Issues].
next([]) ->
    <<"Next: no automatic escalation; inspect missing coverage or choose a focus.">>;
next(Actions) ->
    [
        [
            line([<<"Next: observer_cli ">>, lists:join(<<" ">>, maps:get(<<"argv">>, A))])
         || A <- lists:sublist(Actions, 2)
        ],
        <<"Keep the originating target and cookie-source options for the next command.">>
    ].

bytes(null) -> <<"unavailable">>;
bytes(N) when is_number(N), abs(N) >= 1048576 -> line([number(N / 1048576), <<" MiB">>]);
bytes(N) when is_number(N), abs(N) >= 1024 -> line([number(N / 1024), <<" KiB">>]);
bytes(N) when is_number(N) -> line([number(N), <<" B">>]);
bytes(_) -> <<"unavailable">>.
signed_bytes(N) when is_number(N), N >= 0 -> <<"+", (bytes(N))/binary>>;
signed_bytes(N) -> bytes(N).
seconds(N) -> line([number(N / 1000), <<"s">>]).
number(null) -> <<"unavailable">>;
number(N) when is_integer(N) -> integer_to_binary(N);
number(N) when is_float(N) -> float_to_binary(N, [{decimals, 2}, compact]);
number(_) -> <<"unavailable">>.
scalar(#{<<"module">> := Mod, <<"function">> := Fun, <<"arity">> := Arity}) ->
    line([text(Mod), <<":">>, text(Fun), <<"/">>, number(Arity)]);
scalar(V) when is_number(V) -> number(V);
scalar(V) ->
    text(V).
text(null) -> <<"unavailable">>;
text(B) when is_binary(B) -> observer_cli_capture:escape_text(B);
text(V) -> observer_cli_capture:escape_text(io_lib:format("~tp", [V])).
line(Iodata) -> iolist_to_binary(Iodata).
flatten_lines([]) -> [];
flatten_lines([B | Rest]) when is_binary(B) -> [B | flatten_lines(Rest)];
flatten_lines([L | Rest]) when is_list(L) -> flatten_lines(L) ++ flatten_lines(Rest).

wrap(B, Width) -> wrap_words(binary:split(B, <<" ">>, [global]), Width, <<>>, []).
wrap_words([], _Width, <<>>, Acc) ->
    lists:reverse(Acc);
wrap_words([], _Width, Line, Acc) ->
    lists:reverse([Line | Acc]);
wrap_words([Word | Rest], Width, Line, Acc) ->
    Candidate =
        case Line of
            <<>> -> Word;
            _ -> <<Line/binary, " ", Word/binary>>
        end,
    case length(unicode:characters_to_list(Candidate)) =< Width of
        true ->
            wrap_words(Rest, Width, Candidate, Acc);
        false when Line =:= <<>> ->
            {Prefix, Suffix} = lists:split(Width, unicode:characters_to_list(Word)),
            wrap_words(
                [unicode:characters_to_binary(Suffix) | Rest],
                Width,
                <<>>,
                [unicode:characters_to_binary(Prefix) | Acc]
            );
        false ->
            wrap_words([Word | Rest], Width, <<>>, [Line | Acc])
    end.
screen_budget(Lines, Budget) when length(Lines) =< Budget -> Lines;
screen_budget(Lines, Budget) ->
    lists:sublist(Lines, Budget - 1) ++
        [<<"Additional evidence and next steps: --verbose or --json.">>].
