%% Concise, non-interactive reports. Detailed evidence remains in --verbose/JSON.
-module(observer_cli_report).
-export([render/1, render/2]).

-spec render(map()) -> binary().
render(Response) -> render(Response, 80).

-spec render(map(), pos_integer()) -> binary().
render(Response, Width) ->
    Meta = maps:get(<<"meta">>, Response, #{}),
    Capture = maps:get(<<"capture">>, Meta, #{}),
    Command = maps:get(<<"command">>, Response, null),
    Data = maps:get(<<"data">>, Response, null),
    Lines = [
        line([
            <<"observer_cli ">>,
            text(observer_cli_cli:command_name(Command)),
            <<" | outcome=">>,
            text(maps:get(<<"outcome">>, Response, <<"error">>))
        ]),
        line([<<"target: ">>, value(maps:get(<<"target">>, Meta, null))]),
        fields(Capture, [<<"duration_ms">>, <<"started_at">>, <<"finished_at">>]),
        case Command of
            <<"diagnose">> -> diagnosis(Data, Capture);
            _ -> [coverage(Capture), body(Command, Data, Width)]
        end,
        issues(maps:get(<<"issues">>, Response, [])),
        <<"Details: --verbose (text), --json or --format term.">>
    ],
    iolist_to_binary(lines(Lines)).

body(_Command, null, _Width) ->
    <<"No data returned.">>;
body(<<"snapshot">>, Data, _Width) when is_map(Data) ->
    [
        compact(maps:without([<<"memory">>], Data), 3),
        <<"BEAM memory (bytes; not host RSS):">>,
        compact(maps:get(<<"beam">>, maps:get(<<"memory">>, Data, #{}), #{}), 1)
    ];
body(<<"memory">>, Data, _Width) ->
    Memory = maps:get(<<"memory">>, Data, #{}),
    [
        <<"BEAM memory (bytes; not host RSS; categories can overlap):">>,
        compact(maps:get(<<"beam">>, Memory, #{}), 1),
        <<"persistent_term:">>,
        compact(maps:get(<<"persistent_term">>, Memory, #{}), 1)
    ];
body(<<"distribution">>, Data, Width) ->
    [
        compact(maps:without([<<"controller_queues">>], Data), 2),
        <<"Controller queue observations (bytes, not buffer utilization or health):">>,
        table(
            <<"distribution">>,
            #{<<"sort">> => <<"observed_queue_size_bytes">>},
            maps:get(<<"controller_queues">>, Data, []),
            Width
        )
    ];
body(Command, #{<<"trace">> := Trace} = Data, _Width) when
    (Command =:= <<"trace_call">> orelse Command =:= <<"trace_stop_all">>) andalso is_map(Trace)
->
    [
        compact(maps:remove(<<"trace">>, Data), 1),
        compact(maps:remove(<<"events">>, Trace), 2),
        <<"Trace events (bounded preview):">>,
        compact(maps:get(<<"events">>, Trace, []), 2)
    ];
body(Command, #{<<"items">> := Items} = Data, Width) ->
    [inventory(Data), table(Command, Data, Items, Width)];
body(_Command, Data, _Width) ->
    compact(Data, 3).

diagnosis(null, Capture) ->
    [coverage(Capture), <<"No diagnostic data returned.">>];
diagnosis(Data, Capture) ->
    [
        line([<<"Summary: ">>, text(maps:get(<<"summary">>, Data, <<"unavailable">>))]),
        <<"Findings:">>,
        findings(maps:get(<<"findings">>, Data, [])),
        <<"Coverage:">>,
        coverage(Capture),
        rule_scope(maps:get(<<"ruleset">>, Data, null)),
        compact(maps:with([<<"ruleset">>, <<"ruleset_version">>, <<"sampling_plan">>], Data), 2),
        line([<<"Skipped: ">>, value(maps:get(<<"skipped">>, Data, []))]),
        <<"No findings is not proof of node health; only evaluated rules are covered.">>,
        <<"Next steps (recommendations only; do not execute automatically):">>,
        recommendations(maps:get(<<"findings">>, Data, [])),
        actions(maps:get(<<"next_actions">>, Data, [])),
        <<"Key context:">>,
        compact(maps:get(<<"context">>, Data, #{}), 2)
    ].

rule_scope(<<"observer_cli.quick">>) ->
    <<"Evaluated rules: process, port, atom, and ETS limit pressure only.">>;
rule_scope(Ruleset) when
    Ruleset =:= <<"observer_cli.observation">>;
    Ruleset =:= <<"observer_cli.deep_observation">>;
    Ruleset =:= <<"observer_cli.application_observation">>
->
    <<"Evaluated rules: VM limit and supported scheduler pressure; growth/backlog trends are context, not root causes.">>;
rule_scope(_) ->
    <<"Rule scope: use the reported ruleset and skipped rules; do not infer unreported checks.">>.

actions(Actions) ->
    [
        [
            line([<<"  ">>, text(maps:get(<<"purpose">>, Action, <<"Suggested observation">>))]),
            line([
                <<"    observer_cli ">>,
                join([shell_argument(A) || A <- maps:get(<<"argv">>, Action, [])], <<" ">>)
            ]),
            <<"    Bind the original explicit target and cookie source; do not run against saved context.">>
        ]
     || Action <- Actions
    ].

shell_argument(Argument) ->
    [<<"'">>, binary:replace(text(Argument), <<"'">>, <<"'\\''">>, [global]), <<"'">>].

findings([]) ->
    <<"  none">>;
findings(Findings) ->
    [
        line([
            <<"  ">>,
            text(maps:get(<<"severity">>, F, <<"unknown">>)),
            <<" ">>,
            text(maps:get(<<"id">>, F, <<>>)),
            <<": ">>,
            text(maps:get(<<"summary">>, F, <<>>))
        ])
     || F <- Findings
    ].

recommendations(Findings) ->
    [line([<<"  - ">>, text(R)]) || F <- Findings, R <- maps:get(<<"recommendations">>, F, [])].

coverage(Capture) when is_map(Capture) ->
    Probes = maps:get(<<"probes">>, Capture, []),
    [
        line([
            <<"completed probes: ">>,
            join(
                [
                    text(maps:get(<<"id">>, P, null))
                 || P <- Probes, maps:get(<<"status">>, P, <<"unknown">>) =:= <<"ok">>
                ],
                <<", ">>
            )
        ]),
        [
            line([
                <<"probe ">>,
                text(maps:get(<<"id">>, P, null)),
                <<": ">>,
                value(
                    maps:with([<<"status">>, <<"required">>, <<"reason_code">>, <<"samples">>], P)
                )
            ])
         || P <- Probes, maps:get(<<"status">>, P, <<"unknown">>) =/= <<"ok">>
        ],
        line([
            <<"observer_effects (details: --verbose): ">>,
            join(
                [
                    text(
                        case Effect of
                            #{<<"id">> := Id} -> Id;
                            _ -> Effect
                        end
                    )
                 || Effect <- maps:get(<<"observer_effects">>, Capture, [])
                ],
                <<", ">>
            )
        ])
    ];
coverage(_) ->
    <<"Capture coverage unavailable.">>.

issues(Issues) ->
    [line([<<"issue: ">>, value(Issue)]) || Issue <- Issues].

inventory(Data) ->
    [
        fields(Data, [
            <<"status">>,
            <<"reason_code">>,
            <<"sort">>,
            <<"sort_semantics">>,
            <<"interval_ms">>,
            <<"requested_duration_ms">>
        ]),
        fields(Data, [
            <<"returned_count">>,
            <<"eligible_count">>,
            <<"scanned_count">>,
            <<"dropped_count">>,
            <<"disappeared_count">>,
            <<"exclusion_count">>,
            <<"complete">>,
            <<"truncated">>,
            <<"born_count">>,
            <<"dead_count">>,
            <<"reset_count">>
        ]),
        compact(maps:with([<<"lifecycle">>, <<"exclusions">>, <<"baseline_exclusions">>], Data), 2)
    ].

table(_Command, _Data, [], _Width) ->
    <<"No resource rows returned.">>;
table(Command, Data, Items, Width) ->
    Id = identity_key(Command),
    Metric = metric_key(maps:get(<<"sort">>, Data, <<"memory">>), Data, Items),
    Columns0 = [Id, <<"registered_name">>, Metric, <<"status">>, <<"metric_states">>],
    Columns = lists:usort(
        Columns0 ++
            case Width >= 120 of
                true -> [<<"label">>, <<"name">>];
                false -> []
            end
    ),
    Present = [
        K
     || K <- [Id | lists:delete(Id, Columns)],
        K =:= Id orelse K =:= Metric orelse lists:any(fun(I) -> maps:is_key(K, I) end, Items)
    ],
    [
        line(join([column_label(Command, K) || K <- Present], <<" | ">>)),
        [line(join([cell(K, Item, Metric, Width) || K <- Present], <<" | ">>)) || Item <- Items]
    ].

cell(<<"metric_states">>, Item, Metric, _Width) ->
    text(maps:get(Metric, maps:get(<<"metric_states">>, Item, #{}), null));
cell(Key, Item, _Metric, Width) when Key =:= <<"label">>; Key =:= <<"name">> ->
    Text = line(value(maps:get(Key, Item, null))),
    Characters = unicode:characters_to_list(Text),
    Limit = max(12, Width div 4),
    case length(Characters) > Limit of
        true -> unicode:characters_to_binary(lists:sublist(Characters, Limit - 3) ++ "...");
        false -> Text
    end;
cell(Key, Item, _Metric, _Width) ->
    value(maps:get(Key, Item, null)).

identity_key(<<"distribution">>) -> <<"peer">>;
identity_key(<<"processes">>) -> <<"pid">>;
identity_key(<<"applications">>) -> <<"application">>;
identity_key(<<"ets">>) -> <<"table_id">>;
identity_key(<<"mnesia">>) -> <<"table">>;
identity_key(_) -> <<"resource">>.

metric_key(Sort, Data, Items) ->
    Base =
        case Sort of
            <<"memory">> -> <<"memory_bytes">>;
            <<"binary_memory">> -> <<"binary_memory_bytes">>;
            <<"total_heap_size">> -> <<"total_heap_size_bytes">>;
            _ -> Sort
        end,
    Candidates =
        case maps:get(<<"sort_semantics">>, Data, null) of
            <<"delta">> -> [<<Base/binary, "_delta">>, <<Sort/binary, "_delta">>, Base, Sort];
            _ -> [Base, Sort]
        end,
    case [K || K <- Candidates, lists:any(fun(I) -> maps:is_key(K, I) end, Items)] of
        [K | _] -> K;
        [] -> Base
    end.

column_label(<<"processes">>, Key) when
    Key =:= <<"memory_delta">>;
    Key =:= <<"binary_memory_delta">>;
    Key =:= <<"total_heap_size_delta">>
->
    <<Key/binary, " (bytes)">>;
column_label(<<"sockets">>, <<"io">>) ->
    <<"io (bytes)">>;
column_label(<<"ports">>, Key) when
    Key =:= <<"queue_size">>;
    Key =:= <<"memory">>;
    Key =:= <<"input">>;
    Key =:= <<"output">>;
    Key =:= <<"io">>
->
    <<Key/binary, " (bytes)">>;
column_label(<<"network">>, Key) when
    Key =:= <<"oct">>; Key =:= <<"recv_oct">>; Key =:= <<"send_oct">>
->
    <<Key/binary, " (bytes)">>;
column_label(_Command, Key) ->
    text(Key).

%% A bounded projection, never an alternative data model. Omitted branches are
%% explicitly named so a compact report cannot masquerade as complete evidence.
compact(Map, Depth) when is_map(Map), Depth > 0 ->
    [
        line([text(K), <<": ">>, compact_value(V, Depth - 1)])
     || {K, V} <- lists:sort(maps:to_list(Map))
    ];
compact(List, Depth) when is_list(List), Depth > 0 ->
    [compact(V, Depth - 1) || V <- lists:sublist(List, 8)] ++ omitted(length(List), 8);
compact(Value, _) ->
    line(value(Value)).

compact_value(Map, Depth) when is_map(Map), Depth > 0 ->
    value(maps:map(fun(_K, V) -> summary(V) end, Map));
compact_value(Value, _Depth) ->
    value(Value).

summary(Map) when is_map(Map) ->
    <<"[", (integer_to_binary(map_size(Map)))/binary, " fields; details in verbose]">>;
summary(List) when is_list(List) ->
    <<"[", (integer_to_binary(length(List)))/binary, " items; details in verbose]">>;
summary(Scalar) ->
    Scalar.

omitted(Count, Limit) when Count > Limit ->
    [line([integer_to_binary(Count - Limit), <<" additional entries; see verbose.">>])];
omitted(_, _) ->
    [].

fields(Map, Keys) when is_map(Map) ->
    [line([text(K), <<"=">>, value(maps:get(K, Map))]) || K <- Keys, maps:is_key(K, Map)];
fields(_, _) ->
    [].

value(Map) when is_map(Map) ->
    join(
        [[text(K), <<"=">>, text(summary(V))] || {K, V} <- lists:sort(maps:to_list(Map))], <<" ">>
    );
value(List) when is_list(List) ->
    join([value(V) || V <- lists:sublist(List, 8)] ++ omitted(length(List), 8), <<", ">>);
value(Value) ->
    text(Value).

text(Binary) when is_binary(Binary) -> observer_cli_cli:escape_text(Binary);
text(Value) -> observer_cli_cli:escape_text(io_lib:format("~tp", [Value])).

join([], _Separator) -> [];
join([First | Rest], Separator) -> [First | [[Separator, V] || V <- Rest]].

line(Iodata) -> iolist_to_binary(Iodata).

lines([]) -> [];
lines(Binary) when is_binary(Binary) -> [Binary, <<"\n">>];
lines(List) -> [lines(Line) || Line <- List].
