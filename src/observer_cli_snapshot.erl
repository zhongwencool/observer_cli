-module(observer_cli_snapshot).

-export([capabilities/0, dispatch/4, normalize/2, truncate/1]).

-define(PROTOCOL_VERSION, 1).
-define(TARGET_MARGIN_MS, 1000).
-define(MAX_HEAP_WORDS, 8 * 1024 * 1024).
-define(MAX_RESPONSE_BYTES, 1024 * 1024).
-define(MAX_RESULT_BYTES, ?MAX_RESPONSE_BYTES - 1024).
-define(MAX_FIELD_BYTES, 64 * 1024).
-define(MAX_DEPTH, 32).

-spec capabilities() -> #{protocol_version := pos_integer()}.
capabilities() ->
    #{protocol_version => ?PROTOCOL_VERSION}.

-spec dispatch(pid(), atom(), term(), map()) -> map().
dispatch(Controller, Command, Request, Options) when is_pid(Controller), is_atom(Command) ->
    case dispatch_options(Options) of
        {ok, Policy, ControllerTimeout, MaxHeapWords} ->
            TargetTimeout = ControllerTimeout - ?TARGET_MARGIN_MS,
            case TargetTimeout > 0 of
                true ->
                    run_worker(Controller, Command, Request, Policy, TargetTimeout, MaxHeapWords);
                false ->
                    error_result(target_timeout)
            end;
        error ->
            error_result(invalid_request)
    end;
dispatch(_Controller, _Command, _Request, _Options) ->
    error_result(invalid_request).

-spec normalize(term(), redact | include) -> {ok, term()} | {error, atom()}.
normalize(Term, Policy) when Policy =:= redact; Policy =:= include ->
    case normalize_value(Term, Policy, 0, #{ids => #{}, counts => #{}}) of
        {ok, Value, _State} -> {ok, Value};
        {error, Reason} -> {error, Reason}
    end;
normalize(_Term, _Policy) ->
    {error, invalid_identifier_policy}.

-spec truncate(term()) -> {ok, term()} | {error, atom()}.
truncate(Response) ->
    case evidence_pointers(Response) of
        {ok, Pointers} ->
            case pointers_exist(Response, Pointers) of
                true -> truncate_to_cap(Response, Pointers);
                false -> {error, invalid_evidence_pointer}
            end;
        error ->
            {error, invalid_evidence_pointer}
    end.

dispatch_options(Options) when is_map(Options) ->
    Policy = maps:get(identifier_policy, Options, include),
    Timeout = maps:get(timeout_ms, Options, 10000),
    MaxHeapWords = maps:get(max_heap_words, Options, ?MAX_HEAP_WORDS),
    case
        (Policy =:= redact orelse Policy =:= include) andalso
            is_integer(Timeout) andalso Timeout > 0 andalso
            is_integer(MaxHeapWords) andalso MaxHeapWords >= 1024 andalso
            MaxHeapWords =< ?MAX_HEAP_WORDS
    of
        true -> {ok, Policy, Timeout, MaxHeapWords};
        false -> error
    end;
dispatch_options(_Options) ->
    error.

run_worker(Controller, Command, Request, Policy, TargetTimeout, MaxHeapWords) ->
    OldTrapExit = process_flag(trap_exit, true),
    ControllerRef = erlang:monitor(process, Controller),
    RunRef = make_ref(),
    Coordinator = self(),
    Deadline = erlang:monotonic_time(millisecond) + TargetTimeout,
    try
        {Worker, WorkerRef} = spawn_opt(
            fun() -> worker(Coordinator, RunRef, Command, Request, Policy, Deadline) end,
            [
                link,
                monitor,
                {max_heap_size, #{size => MaxHeapWords, kill => true, error_logger => false}}
            ]
        ),
        try
            coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline)
        catch
            _InnerClass:_InnerReason:_InnerStacktrace ->
                stop_worker(Worker, WorkerRef, internal_error)
        after
            drain_exit(Worker)
        end
    catch
        _OuterClass:_OuterReason:_OuterStacktrace -> error_result(internal_error)
    after
        erlang:demonitor(ControllerRef, [flush]),
        process_flag(trap_exit, OldTrapExit)
    end.

coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline) ->
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected);
        {RunRef, Worker, {ok, Result}} ->
            Response = success_result(Result),
            case
                json_safe(Response) andalso erlang:external_size(Response) =< ?MAX_RESPONSE_BYTES
            of
                true ->
                    finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result);
                false ->
                    stop_worker(Worker, WorkerRef, invalid_schema)
            end;
        {RunRef, Worker, {error, Reason}} when is_atom(Reason) ->
            stop_worker(Worker, WorkerRef, Reason);
        {'DOWN', WorkerRef, process, Worker, Reason} ->
            worker_down(WorkerRef, Reason);
        {'EXIT', Worker, _Reason} ->
            coordinate(ControllerRef, Worker, WorkerRef, RunRef, Deadline)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, target_timeout)
    end.

finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result) ->
    Worker ! {RunRef, finish},
    receive
        {'DOWN', ControllerRef, process, _Controller, _Reason} ->
            stop_worker(Worker, WorkerRef, controller_disconnected);
        {'DOWN', WorkerRef, process, Worker, normal} ->
            success_result(Result);
        {'DOWN', WorkerRef, process, Worker, _Reason} ->
            error_result(cleanup_unconfirmed);
        {'EXIT', Worker, _Reason} ->
            finish_worker(ControllerRef, Worker, WorkerRef, RunRef, Deadline, Result)
    after remaining(Deadline) ->
        stop_worker(Worker, WorkerRef, cleanup_unconfirmed)
    end.

stop_worker(Worker, WorkerRef, Reason) ->
    exit(Worker, kill),
    receive
        {'DOWN', WorkerRef, process, Worker, _WorkerReason} -> error_result(Reason)
    after ?TARGET_MARGIN_MS ->
        error_result(cleanup_unconfirmed)
    end.

worker_down(WorkerRef, Reason) ->
    erlang:demonitor(WorkerRef, [flush]),
    error_result(
        case Reason of
            killed -> worker_heap_limit_exceeded;
            _ -> probe_failed
        end
    ).

worker(Coordinator, RunRef, Command, Request, Policy, Deadline) ->
    Outcome =
        try probe(Command, Request, #{deadline => Deadline}) of
            {probe_error, Reason} when is_atom(Reason) ->
                {error, Reason};
            Raw ->
                case normalize(Raw, Policy) of
                    {ok, Normalized} -> truncate(Normalized);
                    {error, Reason} -> {error, Reason}
                end
        catch
            _Class:_Reason:_Stacktrace -> {error, probe_failed}
        end,
    Coordinator ! {RunRef, self(), Outcome},
    receive
        {RunRef, finish} -> ok
    end.

-ifdef(TEST).
probe(test_echo, Request, _Context) ->
    Request;
probe(test_observed_echo, {Observer, Request}, _Context) ->
    Observer ! {test_worker, self()},
    Request;
probe(test_timeout, Observer, _Context) ->
    Observer ! {test_worker, self()},
    receive
        stop -> ok
    end;
probe(test_crash, Observer, _Context) ->
    Observer ! {test_worker, self()},
    erlang:error({fixture_secret, Observer});
probe(test_heap, Observer, _Context) ->
    Observer ! {test_worker, self()},
    lists:seq(1, 1000000);
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-else.
probe(_Command, _Request, _Context) ->
    {probe_error, capability_unavailable}.
-endif.

normalize_value(_Term, _Policy, Depth, _State) when Depth > ?MAX_DEPTH ->
    {error, response_too_deep};
normalize_value(true, _Policy, _Depth, State) ->
    {ok, true, State};
normalize_value(false, _Policy, _Depth, State) ->
    {ok, false, State};
normalize_value(null, _Policy, _Depth, State) ->
    {ok, null, State};
normalize_value(Term, _Policy, _Depth, State) when is_integer(Term); is_float(Term) ->
    {ok, Term, State};
normalize_value(Term, _Policy, _Depth, State) when is_binary(Term) ->
    normalize_binary(Term, State);
normalize_value(Term, _Policy, _Depth, State) when is_atom(Term) ->
    normalize_binary(atom_to_binary(Term), State);
normalize_value(Term, Policy, _Depth, State) when is_pid(Term) ->
    normalize_identifier(pid, Term, Policy, State);
normalize_value(Term, Policy, _Depth, State) when is_port(Term) ->
    normalize_identifier(port, Term, Policy, State);
normalize_value(Term, Policy, _Depth, State) when is_reference(Term) ->
    normalize_identifier(ref, Term, Policy, State);
normalize_value({identifier, Type, Value}, Policy, _Depth, State) when is_atom(Type) ->
    normalize_identifier(Type, Value, Policy, State);
normalize_value({mfa, Module, Function, Arity}, Policy, _Depth, State) when
    is_atom(Module), is_atom(Function), is_integer(Arity), Arity >= 0
->
    case normalize_identifier(module, Module, Policy, State) of
        {ok, NormalizedModule, State1} ->
            case normalize_identifier(function, Function, Policy, State1) of
                {ok, NormalizedFunction, State2} ->
                    {ok,
                        #{
                            <<"module">> => NormalizedModule,
                            <<"function">> => NormalizedFunction,
                            <<"arity">> => Arity
                        },
                        State2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
normalize_value(Term, Policy, Depth, State) when is_map(Term) ->
    normalize_map(maps:to_list(Term), Policy, Depth + 1, State, #{});
normalize_value(Term, Policy, Depth, State) when is_list(Term) ->
    normalize_list(Term, Policy, Depth + 1, State, []);
normalize_value(_Term, _Policy, _Depth, _State) ->
    {error, invalid_schema}.

normalize_binary(Binary, State) when byte_size(Binary) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Binary) of
        Binary ->
            {ok, Binary, State};
        {_Error, _Valid, _Rest} ->
            Encoded = base64:encode(Binary),
            case byte_size(Encoded) =< ?MAX_FIELD_BYTES of
                true ->
                    {ok,
                        #{
                            <<"encoding">> => <<"base64">>,
                            <<"data">> => Encoded
                        },
                        State};
                false ->
                    {error, field_too_large}
            end
    end;
normalize_binary(_Binary, _State) ->
    {error, field_too_large}.

normalize_map([], _Policy, _Depth, State, Acc) ->
    {ok, Acc, State};
normalize_map([{Key, Value} | Rest], Policy, Depth, State, Acc) ->
    case normalize_key(Key) of
        {ok, NormalizedKey} when not is_map_key(NormalizedKey, Acc) ->
            case normalize_value(Value, Policy, Depth, State) of
                {ok, NormalizedValue, State1} ->
                    normalize_map(
                        Rest, Policy, Depth, State1, Acc#{NormalizedKey => NormalizedValue}
                    );
                Error ->
                    Error
            end;
        _ ->
            {error, invalid_schema}
    end.

normalize_key(Key) when is_atom(Key) ->
    normalize_key(atom_to_binary(Key));
normalize_key(Key) when is_binary(Key), byte_size(Key) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Key) of
        Key -> {ok, Key};
        _ -> error
    end;
normalize_key(_Key) ->
    error.

normalize_list([], _Policy, _Depth, State, Acc) ->
    {ok, lists:reverse(Acc), State};
normalize_list([Value | Rest], Policy, Depth, State, Acc) ->
    case normalize_value(Value, Policy, Depth, State) of
        {ok, NormalizedValue, State1} ->
            normalize_list(Rest, Policy, Depth, State1, [NormalizedValue | Acc]);
        Error ->
            Error
    end;
normalize_list(_Improper, _Policy, _Depth, _State, _Acc) ->
    {error, invalid_schema}.

normalize_identifier(Type, Value, Policy, State) ->
    case identifier_binary(Type, Value) of
        {ok, Binary} when Policy =:= include ->
            normalize_binary(Binary, State);
        {ok, _Binary} when Policy =:= redact ->
            stable_identifier(Type, Value, State);
        error ->
            {error, invalid_identifier}
    end.

identifier_binary(pid, Value) when is_pid(Value) ->
    {ok, list_to_binary(pid_to_list(Value))};
identifier_binary(port, Value) when is_port(Value) ->
    {ok, list_to_binary(port_to_list(Value))};
identifier_binary(ref, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(table, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(table, Value) when is_integer(Value) ->
    {ok, integer_to_binary(Value)};
identifier_binary(socket, {'$socket', Value}) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(socket, Value) when is_reference(Value) ->
    {ok, list_to_binary(ref_to_list(Value))};
identifier_binary(Type, Value) when
    Type =:= node;
    Type =:= name;
    Type =:= module;
    Type =:= function;
    Type =:= table;
    Type =:= application
->
    identifier_text(Value);
identifier_binary(_Type, _Value) ->
    error.

identifier_text(Value) when is_atom(Value) ->
    {ok, atom_to_binary(Value)};
identifier_text(Value) when is_binary(Value), byte_size(Value) =< ?MAX_FIELD_BYTES ->
    case unicode:characters_to_binary(Value) of
        Value -> {ok, Value};
        _ -> error
    end;
identifier_text(_Value) ->
    error.

stable_identifier(Type, Value, #{ids := Ids, counts := Counts} = State) ->
    Key = {Type, Value},
    case maps:find(Key, Ids) of
        {ok, Identifier} ->
            {ok, Identifier, State};
        error ->
            Number = maps:get(Type, Counts, 0) + 1,
            Identifier = iolist_to_binary([atom_to_binary(Type), $-, integer_to_binary(Number)]),
            {ok, Identifier, State#{
                ids := Ids#{Key => Identifier},
                counts := Counts#{Type => Number}
            }}
    end.

truncate_to_cap(Response, Pointers) ->
    %% ponytail: tail trimming is O(n^2); report lists cap at 200, batch it if that cap grows.
    case erlang:external_size(Response) =< ?MAX_RESULT_BYTES of
        true ->
            {ok, Response};
        false ->
            case trim_once(Response, [], Pointers) of
                {ok, Trimmed} ->
                    case pointers_exist(Trimmed, Pointers) of
                        true -> truncate_to_cap(Trimmed, Pointers);
                        false -> {error, invalid_evidence_pointer}
                    end;
                none ->
                    {error, response_too_large}
            end
    end.

trim_once(Map, Path, Pointers) when is_map(Map) ->
    case trim_items(Map, Path, Pointers) of
        {ok, Trimmed} -> {ok, Trimmed};
        none -> trim_map_values(maps:to_list(Map), Map, Path, Pointers)
    end;
trim_once(List, Path, Pointers) when is_list(List) ->
    trim_list_values(List, [], Path, Pointers, 0);
trim_once(_Value, _Path, _Pointers) ->
    none.

trim_items(#{<<"items">> := Items} = Map, Path, Pointers) when Items =/= [] ->
    Index = length(Items) - 1,
    ItemsPath = Path ++ [<<"items">>],
    case pointer_protects_tail(Pointers, ItemsPath, Index) of
        true ->
            none;
        false ->
            NewItems = lists:sublist(Items, Index),
            {ok, update_item_counts(Map#{<<"items">> := NewItems, <<"truncated">> => true})}
    end;
trim_items(_Map, _Path, _Pointers) ->
    none.

update_item_counts(Map) ->
    Items = maps:get(<<"items">>, Map),
    Map1 =
        case maps:is_key(<<"returned_count">>, Map) of
            true -> Map#{<<"returned_count">> := length(Items)};
            false -> Map
        end,
    case maps:find(<<"dropped_count">>, Map1) of
        {ok, Count} when is_integer(Count) -> Map1#{<<"dropped_count">> := Count + 1};
        _ -> Map1
    end.

trim_map_values([], _Map, _Path, _Pointers) ->
    none;
trim_map_values([{<<"items">>, _Value} | Rest], Map, Path, Pointers) ->
    trim_map_values(Rest, Map, Path, Pointers);
trim_map_values([{Key, Value} | Rest], Map, Path, Pointers) ->
    case trim_once(Value, Path ++ [Key], Pointers) of
        {ok, TrimmedValue} -> {ok, Map#{Key := TrimmedValue}};
        none -> trim_map_values(Rest, Map, Path, Pointers)
    end.

trim_list_values([], _Before, _Path, _Pointers, _Index) ->
    none;
trim_list_values([Value | Rest], Before, Path, Pointers, Index) ->
    case trim_once(Value, Path ++ [integer_to_binary(Index)], Pointers) of
        {ok, TrimmedValue} -> {ok, lists:reverse(Before, [TrimmedValue | Rest])};
        none -> trim_list_values(Rest, [Value | Before], Path, Pointers, Index + 1)
    end.

evidence_pointers(Term) ->
    evidence_pointers(Term, []).

evidence_pointers(Map, Acc) when is_map(Map) ->
    case maps:find(<<"evidence">>, Map) of
        {ok, Evidence} when is_list(Evidence) ->
            case evidence_paths(Evidence, Acc) of
                {ok, Acc1} -> evidence_pointers_values(maps:values(Map), Acc1);
                error -> error
            end;
        {ok, _Invalid} ->
            error;
        error ->
            evidence_pointers_values(maps:values(Map), Acc)
    end;
evidence_pointers(List, Acc) when is_list(List) ->
    evidence_pointers_values(List, Acc);
evidence_pointers(_Value, Acc) ->
    {ok, Acc}.

evidence_pointers_values([], Acc) ->
    {ok, Acc};
evidence_pointers_values([Value | Rest], Acc) ->
    case evidence_pointers(Value, Acc) of
        {ok, Acc1} -> evidence_pointers_values(Rest, Acc1);
        error -> error
    end.

evidence_paths([], Acc) ->
    {ok, Acc};
evidence_paths([#{<<"path">> := Pointer} | Rest], Acc) when is_binary(Pointer) ->
    case parse_pointer(Pointer) of
        {ok, Segments} -> evidence_paths(Rest, [Segments | Acc]);
        error -> error
    end;
evidence_paths([_Invalid | _Rest], _Acc) ->
    error.

parse_pointer(<<>>) ->
    {ok, []};
parse_pointer(<<"/", Rest/binary>>) ->
    parse_pointer_segments(binary:split(Rest, <<"/">>, [global]), []);
parse_pointer(_Pointer) ->
    error.

parse_pointer_segments([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_pointer_segments([Segment | Rest], Acc) ->
    case unescape_pointer(Segment, <<>>) of
        {ok, Unescaped} -> parse_pointer_segments(Rest, [Unescaped | Acc]);
        error -> error
    end.

unescape_pointer(<<>>, Acc) ->
    {ok, Acc};
unescape_pointer(<<"~0", Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, "~">>);
unescape_pointer(<<"~1", Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, "/">>);
unescape_pointer(<<"~", _Rest/binary>>, _Acc) ->
    error;
unescape_pointer(<<Byte, Rest/binary>>, Acc) ->
    unescape_pointer(Rest, <<Acc/binary, Byte>>).

pointers_exist(Response, Pointers) ->
    lists:all(fun(Pointer) -> pointer_exists(Response, Pointer) end, Pointers).

pointer_exists(_Value, []) ->
    true;
pointer_exists(Map, [Key | Rest]) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> pointer_exists(Value, Rest);
        error -> false
    end;
pointer_exists(List, [Index | Rest]) when is_list(List) ->
    case pointer_index(Index) of
        {ok, Number} when Number < length(List) ->
            pointer_exists(lists:nth(Number + 1, List), Rest);
        _ ->
            false
    end;
pointer_exists(_Value, _Segments) ->
    false.

pointer_index(<<"0">>) ->
    {ok, 0};
pointer_index(<<First, _/binary>> = Binary) when First >= $1, First =< $9 ->
    try binary_to_integer(Binary) of
        Number when Number >= 0 -> {ok, Number};
        _ -> error
    catch
        error:badarg -> error
    end;
pointer_index(_Index) ->
    error.

pointer_protects_tail(Pointers, Path, Index) ->
    lists:any(
        fun(Pointer) ->
            case length(Pointer) >= length(Path) of
                true ->
                    case lists:split(length(Path), Pointer) of
                        {Path, []} -> true;
                        {Path, [Segment | _]} -> Segment =:= integer_to_binary(Index);
                        _ -> false
                    end;
                false ->
                    false
            end
        end,
        Pointers
    ).

json_safe(Map) when is_map(Map) ->
    lists:all(
        fun({Key, Value}) -> is_binary(Key) andalso json_safe(Value) end,
        maps:to_list(Map)
    );
json_safe(List) when is_list(List) ->
    lists:all(fun json_safe/1, List);
json_safe(Value) when is_binary(Value); is_integer(Value); is_float(Value) ->
    true;
json_safe(Value) when Value =:= true; Value =:= false; Value =:= null ->
    true;
json_safe(_Value) ->
    false.

remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

success_result(Result) ->
    #{
        <<"status">> => <<"ok">>,
        <<"result">> => Result,
        <<"cleanup_confirmed">> => true
    }.

error_result(Reason) ->
    #{
        <<"status">> => <<"error">>,
        <<"reason_code">> => atom_to_binary(Reason),
        <<"cleanup_confirmed">> => true
    }.

drain_exit(Worker) ->
    receive
        {'EXIT', Worker, _Reason} -> ok
    after 0 ->
        ok
    end.
