%% Public response projection of already validated private capture records.
-module(observer_cli_result).
-export([schema/0, from_capture/2, local/2, error/3, argument_error/1, exit_code/2]).

-spec schema() -> binary().
schema() -> <<"observer_cli.cli/v2">>.

-spec from_capture(map(), map()) -> map().
from_capture(#{command := Command, options := Options} = Route, Capture) ->
    Id = maps:get(id, Route),
    Data0 = maps:get(<<"data">>, Capture),
    Assessment = assessment(Id, Capture),
    Data1 =
        case is_check(Id) andalso is_map(Data0) of
            true -> maps:without([<<"summary">>, <<"findings">>, <<"next_actions">>], Data0);
            false -> Data0
        end,
    Data = selectors(
        maps:get(capture, Route),
        rank_projection(Id, Options, Data1),
        maps:get(redact, Options, false)
    ),
    Response = Capture#{
        <<"schema">> := schema(),
        <<"command">> := Command,
        <<"data">> := Data,
        <<"summary">> => summary(Id, Capture, Assessment),
        <<"assessment">> => Assessment,
        <<"meta">> := capture_meta(Route, maps:get(<<"meta">>, Capture)),
        <<"next_actions">> => []
    },
    Actions = next_actions(Id, Response),
    SafeActions =
        case maps:get(redact, Options, false) of
            true -> [A#{<<"argv">> := maps:get(<<"argv">>, A) ++ [<<"--redact">>]} || A <- Actions];
            false -> Actions
        end,
    Response#{<<"next_actions">> := SafeActions}.

-spec local(binary() | null, map() | null) -> map().
local(Command, Data) ->
    #{
        <<"schema">> => schema(),
        <<"command">> => Command,
        <<"outcome">> => <<"complete">>,
        <<"summary">> => <<"Offline command capabilities; no target was contacted.">>,
        <<"assessment">> => empty_assessment(Command),
        <<"data">> => Data,
        <<"meta">> => #{<<"target">> => null, <<"capture">> => null},
        <<"issues">> => [],
        <<"next_actions">> => []
    }.

empty_assessment(Command) ->
    case
        lists:member(Command, [
            <<"check">>,
            <<"check cpu">>,
            <<"check memory">>,
            <<"check mailbox">>,
            <<"check connections">>
        ])
    of
        true -> #{<<"status">> => <<"not_evaluated">>, <<"findings">> => []};
        false -> null
    end.

-spec argument_error(map()) -> map().
argument_error(#{command := Command, message := Message, reason_code := Code}) ->
    error_message(Command, argument, Code, Message).

-spec error(binary() | null, atom(), term()) -> map().
error(Command, Category, Reason) ->
    Issue0 = observer_cli_capture:error(Category, Reason),
    Issue = recovery_issue(Reason, Issue0),
    (local(Command, null))#{
        <<"outcome">> := <<"error">>,
        <<"summary">> := maps:get(<<"message">>, Issue),
        <<"issues">> := [Issue]
    }.

error_message(Command, Category, Code, Message) ->
    Issue = #{
        <<"severity">> => <<"error">>,
        <<"class">> => atom_to_binary(Category),
        <<"reason_code">> => Code,
        <<"message">> => Message
    },
    (local(Command, null))#{
        <<"outcome">> := <<"error">>, <<"summary">> := Message, <<"issues">> := [Issue]
    }.

recovery_issue(missing_target, Issue) ->
    Issue#{
        <<"reason_code">> := <<"missing_target">>,
        <<"message">> :=
            <<"Choose a target: --node NODE --cookie-env NAME, or set OBSERVER_CLI_NODE ",
                "and OBSERVER_CLI_COOKIE (or OBSERVER_CLI_COOKIE_FILE). No saved context is used.">>
    };
recovery_issue(conflicting_cookie_sources, Issue) ->
    Issue#{
        <<"reason_code">> := <<"conflicting_cookie_sources">>,
        <<"message">> :=
            <<"Choose exactly one cookie source. Unset either OBSERVER_CLI_COOKIE or OBSERVER_CLI_COOKIE_FILE for shell selection.">>
    };
recovery_issue(capability_unavailable, Issue) ->
    Issue#{
        <<"message">> :=
            <<"The target needs the matching observer_cli 3.0.0 bundle and protocol 2. ",
                "Install it in the target release; command calls never load code remotely.">>
    };
recovery_issue(_Reason, Issue) ->
    Issue.

is_check(Id) ->
    lists:member(Id, [check, check_cpu, check_memory, check_mailbox, check_connections]).

assessment(Id, Response) ->
    case is_check(Id) of
        false -> null;
        true -> check_assessment(Id, Response)
    end.

check_assessment(Id, #{<<"data">> := Data, <<"meta">> := Meta}) when is_map(Data) ->
    Findings = relevant_findings(Id, maps:get(<<"findings">>, Data, [])),
    Capture = maps:get(<<"capture">>, Meta, #{}),
    Probes =
        case Capture of
            null -> [];
            _ -> maps:get(<<"probes">>, Capture, [])
        end,
    Required = [P || P <- Probes, maps:get(<<"required">>, P, false)],
    RequiredOk = Required =/= [] andalso lists:all(fun probe_ok/1, Required),
    Evaluated =
        case Id of
            check -> RequiredOk;
            check_cpu -> RequiredOk andalso probe_available(<<"scheduler_pressure">>, Probes);
            _ -> false
        end,
    Status =
        case {Findings, Evaluated} of
            {[_ | _], _} -> <<"findings">>;
            {[], true} -> <<"no_findings">>;
            {[], false} -> <<"not_evaluated">>
        end,
    #{<<"status">> => Status, <<"findings">> => Findings};
check_assessment(_Id, _Response) ->
    #{<<"status">> => <<"not_evaluated">>, <<"findings">> => []}.

relevant_findings(_Id, Findings) -> Findings.

probe_ok(P) -> maps:get(<<"status">>, P, null) =:= <<"ok">>.
probe_available(Id, Probes) ->
    lists:any(fun(P) -> maps:get(<<"id">>, P, null) =:= Id andalso probe_ok(P) end, Probes).

summary(_Id, #{<<"outcome">> := <<"error">>, <<"issues">> := Issues}, _Assessment) ->
    Errors = [M || #{<<"severity">> := <<"error">>, <<"message">> := M} <- Issues, is_binary(M)],
    case Errors of
        [Message | _] -> Message;
        [] -> <<"Operation unavailable or refused; inspect probe coverage and reason codes.">>
    end;
summary(Id, Capture, #{<<"status">> := Status, <<"findings">> := Findings}) ->
    Scope =
        case Id of
            check -> <<"limit/scheduler-pressure">>;
            check_cpu -> <<"scheduler-pressure">>;
            _ -> <<"root-cause">>
        end,
    Core =
        case Status of
            <<"findings">> ->
                iolist_to_binary(
                    io_lib:format(
                        "~B calibrated finding(s); inspect evidence before changing the system.", [
                            length(Findings)
                        ]
                    )
                );
            <<"no_findings">> ->
                <<"No calibrated ", Scope/binary, " findings in the covered window.">>;
            <<"not_evaluated">> ->
                <<"Measurements only; no calibrated ", Scope/binary, " conclusion.">>
        end,
    case maps:get(<<"outcome">>, Capture) of
        <<"partial">> -> <<"Partial evidence. ", Core/binary>>;
        _ -> Core
    end;
summary(Id, Capture, null) ->
    D = observer_cli_catalog:descriptor(Id),
    Text0 = maps:get(<<"summary">>, D),
    Text =
        case {Id, maps:get(<<"command">>, Capture)} of
            {inspect_process, <<"process">>} ->
                <<"Safe metadata for the selected process; no messages, dictionary or state values.">>;
            {inspect_port, <<"port">>} ->
                <<"Metadata for the selected Erlang port, not a TCP port number.">>;
            _ ->
                Text0
        end,
    case maps:get(<<"outcome">>, Capture) of
        <<"partial">> -> <<"Partial evidence: ", Text/binary>>;
        _ -> Text
    end.

capture_meta(#{id := Id, options := Options}, #{<<"capture">> := Capture} = Meta) when
    is_map(Capture)
->
    Window =
        case is_check(Id) of
            true ->
                observer_cli_input:duration_ms(maps:get(window, Options, "15s"));
            false ->
                case maps:find(window, Options) of
                    {ok, W} -> observer_cli_input:duration_ms(W);
                    error -> undefined
                end
        end,
    case Window of
        undefined -> Meta;
        _ -> Meta#{<<"capture">> := Capture#{<<"requested_window_ms">> => Window}}
    end;
capture_meta(_Route, Meta) ->
    Meta.

selectors(diagnose, Data, Redacted) when is_map(Data) ->
    nested_selectors(<<"context">>, diagnostic_context, Data, Redacted);
selectors(diagnostic_context, Data, Redacted) ->
    Current = nested_selectors(<<"current">>, current_context, Data, Redacted),
    Activity = nested_selectors(<<"hot_processes_by_reductions">>, processes, Current, Redacted),
    nested_selectors(<<"binary_holders">>, processes, Activity, Redacted);
selectors(current_context, Data, Redacted) ->
    nested_selectors(<<"processes">>, processes, Data, Redacted);
selectors(snapshot, Data, Redacted) when is_map(Data) ->
    Processes = nested_selectors(<<"processes">>, processes, Data, Redacted),
    nested_selectors(<<"ports">>, ports, Processes, Redacted);
selectors(Id, #{<<"items">> := Items} = Data, Redacted) when
    Id =:= processes orelse Id =:= ports
->
    Data#{
        <<"items">> := [
            add_selector(
                case Id of
                    processes -> process;
                    ports -> port
                end,
                Item,
                Redacted
            )
         || Item <- Items
        ]
    };
selectors(Id, Data, Redacted) when
    is_map(Data) andalso (Id =:= process orelse Id =:= port)
->
    add_selector(Id, Data, Redacted);
selectors(_Id, Data, _Redacted) ->
    Data.

nested_selectors(Key, Kind, Data, Redacted) ->
    case maps:find(Key, Data) of
        {ok, Nested} when is_map(Nested) ->
            Data#{Key := selectors(Kind, Nested, Redacted)};
        _ ->
            Data
    end.

add_selector(_Id, Item, true) ->
    Item#{<<"selector">> => null};
add_selector(process, Item, false) ->
    Item#{
        <<"selector">> => selector(
            <<"pid">>, maps:get(<<"pid">>, Item, null), "^<0\\.[0-9]+\\.[0-9]+>$"
        )
    };
add_selector(port, Item, false) ->
    Item#{
        <<"selector">> => selector(
            <<"port">>, maps:get(<<"resource">>, Item, null), "^#Port<0\\.[0-9]+>$"
        )
    }.

selector(Kind, Value, Pattern) when is_binary(Value) ->
    case re:run(Value, Pattern, [{capture, none}]) of
        match -> #{<<"kind">> => Kind, <<"value">> => Value};
        nomatch -> null
    end;
selector(_Kind, _Value, _Pattern) ->
    null.

rank_projection(
    Id, Options, #{<<"items">> := _, <<"sort">> := _, <<"sort_semantics">> := _} = Data
) when
    Id =:= inspect_process; Id =:= inspect_network; Id =:= inspect_socket
->
    Sort = maps:get(sort, Options, binary_to_list(maps:get(<<"sort">>, Data, <<"memory">>))),
    {_Base, Semantics} = observer_cli_input:metric(Sort),
    Data#{
        <<"sort">> := unicode:characters_to_binary(Sort),
        <<"sort_semantics">> := atom_to_binary(Semantics)
    };
rank_projection(_Id, _Options, Data) ->
    Data.

next_actions(_Id, #{<<"outcome">> := <<"error">>}) ->
    [];
next_actions(Id, Response) ->
    case is_check(Id) of
        false -> [];
        true -> check_actions(Id, Response)
    end.

check_actions(Id, #{
    <<"assessment">> := #{<<"findings">> := Findings}, <<"data">> := Data, <<"meta">> := Meta
}) ->
    Capture = maps:get(<<"capture">>, Meta, #{}),
    Probes =
        case Capture of
            null -> [];
            _ -> maps:get(<<"probes">>, Capture, [])
        end,
    RuleActions = [public_action(A) || A <- observer_cli_actions:from_findings(Findings)],
    ContextActions =
        case Id of
            check_cpu ->
                case probe_available(<<"process_inventory">>, Probes) of
                    true -> [context_action(activity)];
                    false -> []
                end;
            check_memory ->
                [context_action(allocators)] ++ process_actions(Probes, memory);
            check_mailbox ->
                process_actions(Probes, mailbox);
            check_connections ->
                [context_action(peers), context_action(network)];
            check ->
                process_actions(Probes, memory)
        end,
    case is_map(Data) of
        true -> lists:sublist(unique_actions(RuleActions ++ ContextActions), 3);
        false -> []
    end.

process_actions(Probes, Kind) ->
    case probe_available(<<"process_inventory">>, Probes) of
        true -> [context_action(Kind)];
        false -> []
    end.
unique_actions(Actions) ->
    {_, Reverse} = lists:foldl(
        fun(A, {Seen, Acc}) ->
            Id = maps:get(<<"id">>, A),
            case lists:member(Id, Seen) of
                true -> {Seen, Acc};
                false -> {[Id | Seen], [A | Acc]}
            end
        end,
        {[], []},
        Actions
    ),
    lists:reverse(Reverse).

public_action(#{<<"argv">> := Argv} = Action) -> Action#{<<"argv">> := public_argv(Argv)}.
public_argv([<<"processes">>, <<"--sort">>, <<"reductions">> | Rest]) ->
    [<<"inspect">>, <<"process">>, <<"--sort">>, <<"reductions-rate">> | window_argv(Rest)];
public_argv([<<"processes">> | Rest]) ->
    [<<"inspect">>, <<"process">> | Rest];
public_argv([<<"applications">> | Rest]) ->
    [<<"inspect">>, <<"application">> | Rest];
public_argv([<<"ports">> | Rest]) ->
    [<<"inspect">>, <<"port">> | Rest];
public_argv([Name | Rest]) ->
    [<<"inspect">>, Name | Rest].
window_argv(Argv) ->
    [
        case A of
            <<"--duration">> -> <<"--window">>;
            _ -> A
        end
     || A <- Argv
    ].

context_action(Kind) ->
    {Purpose, Argv} =
        case Kind of
            activity ->
                {<<"Attribute observed scheduler activity; reductions are not CPU time.">>, [
                    <<"inspect">>,
                    <<"process">>,
                    <<"--sort">>,
                    <<"reductions-rate">>,
                    <<"--window">>,
                    <<"2s">>
                ]};
            memory ->
                {<<"Attribute measured memory to processes; this is not leak proof.">>, [
                    <<"inspect">>, <<"process">>, <<"--sort">>, <<"memory">>
                ]};
            mailbox ->
                {<<"Inspect current mailbox lengths before interpreting their changes.">>, [
                    <<"inspect">>, <<"process">>, <<"--sort">>, <<"message_queue_len">>
                ]};
            allocators ->
                {<<"Inspect allocator context behind the measured BEAM memory.">>, [
                    <<"inspect">>, <<"memory">>
                ]};
            peers ->
                {<<"Review Erlang peer context without inferring network health.">>, [
                    <<"inspect">>, <<"distribution">>
                ]};
            network ->
                {<<"Supplement OTP socket evidence with legacy inet counters.">>, [
                    <<"inspect">>, <<"network">>
                ]}
        end,
    #{
        <<"id">> => <<"observe_", (atom_to_binary(Kind))/binary>>,
        <<"purpose">> => Purpose,
        <<"argv">> => Argv,
        <<"target_binding">> => <<"same_explicit_target">>,
        <<"risk_level">> => <<"bounded_observation">>,
        <<"requires_confirmation">> => false,
        <<"requires_identifiers">> => false
    }.

-spec exit_code(map(), map()) -> non_neg_integer().
exit_code(#{<<"outcome">> := <<"complete">>, <<"assessment">> := #{<<"findings">> := Findings}}, #{
    fail_on := Policy
}) ->
    Matching = lists:any(
        fun(F) ->
            Severity = maps:get(<<"severity">>, F),
            Severity =:= <<"critical">> orelse
                (Policy =:= "warning" andalso Severity =:= <<"warning">>)
        end,
        Findings
    ),
    case Matching of
        true -> 1;
        false -> 0
    end;
exit_code(#{<<"outcome">> := <<"complete">>}, _Options) ->
    0;
exit_code(Response, _Options) ->
    observer_cli_escriptize:response_exit_code(Response).
