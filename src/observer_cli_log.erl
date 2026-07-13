-module(observer_cli_log).

-include_lib("kernel/include/file.hrl").

-export([addressable_handler_id/1, capture/1, valid_request/1]).

-ifdef(TEST).
-export([
    capture/2,
    classify_config/2,
    plain_modes/1,
    read_configured_file/2,
    tail/4
]).
-endif.

-define(MAX_TAIL, 2000).
-define(MAX_HANDLER_IDS, 64).
-define(MAX_HANDLER_ID_BYTES, 1024).
-define(MAX_HANDLER_ID_CODEPOINTS, 255).
-define(MAX_PATH_BYTES, 4096).
-define(MAX_MODES, 32).
-define(MAX_READ_BYTES, 64 * 1024).
-define(MAX_LINE_BYTES, 32 * 1024).
-define(MAX_OFFSET, 16#7FFFFFFFFFFFFFFF).

-spec capture(map()) -> tuple().
capture(Request) ->
    capture(Request, default_env()).

capture(Request, Env) ->
    State = #{handler_ids_enumerated => false, lookups => 0, attempts => 0},
    case valid_request(Request) of
        true ->
            case call_env(os_type, [], Env) of
                {unix, Platform} when Platform =:= linux; Platform =:= darwin ->
                    select_source(Request, Env, State);
                _ ->
                    unavailable(
                        unsupported_target_platform,
                        data([], null, null),
                        [],
                        State
                    )
            end;
        false ->
            {probe_error, invalid_request}
    end.

-spec valid_request(term()) -> boolean().
valid_request(#{handler := Handler, tail := Tail} = Request) ->
    map_size(Request) =:= 2 andalso
        (Handler =:= null orelse
            (is_binary(Handler) andalso addressable_handler_id(Handler))) andalso
        is_integer(Tail) andalso Tail >= 1 andalso Tail =< ?MAX_TAIL;
valid_request(_Request) ->
    false.

select_source(#{handler := null, tail := Tail}, Env, State0) ->
    State = State0#{handler_ids_enumerated := true},
    case handler_ids(Env) of
        {ok, Ids} ->
            case bounded_ids(Ids, [], 0) of
                {ok, BoundedIds} ->
                    {Sources, State1} = classify_ids(BoundedIds, Env, State, []),
                    Supported = [Source || #{supported := true} = Source <- Sources],
                    select_automatic(Sources, Supported, Tail, Env, State1);
                too_many ->
                    unavailable(scan_budget_exceeded, data([], null, null), [], State);
                invalid ->
                    unavailable(
                        log_source_unavailable,
                        data([], null, null),
                        [source_classification_complete],
                        State
                    )
            end;
        error ->
            unavailable(
                log_source_unavailable,
                data([], null, null),
                [source_classification_complete],
                State
            )
    end;
select_source(#{handler := Handler, tail := Tail}, Env, State) ->
    case existing_handler_atom(Handler) of
        {ok, Id} ->
            {Result, State1} = lookup_config(Id, Env, State),
            case Result of
                {error, {not_found, _MissingId}} ->
                    unavailable(
                        log_handler_not_found,
                        data([], null, null),
                        [source_classification_complete],
                        State1
                    );
                _ ->
                    select_explicit_config(Id, Result, Tail, Env, State1)
            end;
        error ->
            unavailable(
                log_handler_not_found,
                data([], null, null),
                [source_classification_complete],
                State
            )
    end.

select_explicit_config(Id, Result, Tail, Env, State) ->
    Source = classify_config(Id, Result),
    case maps:get(supported, Source) of
        true ->
            selected_capture(Source, [Source], Tail, Env, State);
        false ->
            unavailable(
                maps:get(reason_code, maps:get(summary, Source)),
                data([public_summary(Source)], null, null),
                [source_classification_complete],
                State
            )
    end.

select_automatic(Sources, [Source], Tail, Env, State) ->
    selected_capture(Source, Sources, Tail, Env, State);
select_automatic(Sources, [], _Tail, _Env, State) ->
    unavailable(
        log_source_unavailable,
        data(public_summaries(Sources), null, null),
        [source_classification_complete],
        State
    );
select_automatic(Sources, _Multiple, _Tail, _Env, State) ->
    unavailable(
        log_handler_required,
        data(public_summaries(Sources), null, null),
        [source_classification_complete],
        State
    ).

handler_ids(Env) ->
    try call_env(handler_ids, [], Env) of
        Ids -> {ok, Ids}
    catch
        _:_ -> error
    end.

bounded_ids([], Acc, _Count) ->
    {ok, lists:reverse(Acc)};
bounded_ids([_Id | _Rest], _Acc, ?MAX_HANDLER_IDS) ->
    too_many;
bounded_ids([Id | Rest], Acc, Count) when is_atom(Id) ->
    bounded_ids(Rest, [Id | Acc], Count + 1);
bounded_ids([_Id | _Rest], _Acc, _Count) ->
    invalid;
bounded_ids(_Improper, _Acc, _Count) ->
    invalid.

classify_ids([], _Env, State, Acc) ->
    {lists:reverse(Acc), State};
classify_ids([Id | Rest], Env, State0, Acc) ->
    {Result, State} = lookup_config(Id, Env, State0),
    classify_ids(Rest, Env, State, [classify_config(Id, Result) | Acc]).

lookup_config(Id, Env, State) ->
    State1 = State#{lookups := maps:get(lookups, State) + 1},
    Result =
        try call_env(handler_config, [Id], Env) of
            Value -> Value
        catch
            _:_ -> error
        end,
    {Result, State1}.

-spec classify_config(atom(), term()) -> map().
classify_config(Id, Result) when is_atom(Id) ->
    IdText = atom_to_binary(Id, utf8),
    Addressable = addressable_handler_id(IdText),
    classify_config_result(Id, IdText, Addressable, Result);
classify_config(_Id, _Result) ->
    unsupported_source(<<"--invalid-handler-id">>, false, other, invalid_log_handler_config).

classify_config_result(
    Id,
    IdText,
    Addressable,
    {ok, #{id := ConfigId, module := Module, config := Config}}
) when ConfigId =:= Id, is_atom(Module), is_map(Config) ->
    case maps:find(type, Config) of
        {ok, Type} when Module =/= logger_std_h; Type =/= file ->
            unsupported_source(IdText, Addressable, other, unsupported_log_handler);
        {ok, file} ->
            classify_file_config(Id, IdText, Addressable, Config);
        error ->
            unsupported_source(IdText, Addressable, other, invalid_log_handler_config)
    end;
classify_config_result(_Id, IdText, Addressable, _Result) ->
    unsupported_source(IdText, Addressable, other, invalid_log_handler_config).

classify_file_config(Id, IdText, Addressable, Config) ->
    case {maps:find(file, Config), maps:find(modes, Config)} of
        {{ok, Path}, {ok, Modes}} ->
            classify_file_shape(Id, IdText, Addressable, Path, Modes);
        _ ->
            unsupported_source(
                IdText,
                Addressable,
                logger_std_h_file,
                invalid_log_handler_config
            )
    end.

classify_file_shape(Id, IdText, Addressable, Path, Modes) ->
    case valid_file_shape(Path, IdText, Modes) of
        {ok, supported} ->
            classify_display_path(Id, IdText, Addressable, Path, Modes);
        {ok, unsupported} ->
            unsupported_source(
                IdText, Addressable, logger_std_h_file, unsupported_file_modes
            );
        error ->
            unsupported_source(
                IdText, Addressable, logger_std_h_file, invalid_log_handler_config
            )
    end.

classify_display_path(Id, IdText, Addressable, Path, Modes) ->
    case display_path(Path) of
        {ok, DisplayPath} ->
            Summary = summary(
                IdText,
                Addressable,
                logger_std_h_file,
                true,
                case Addressable of
                    true -> null;
                    false -> unaddressable_handler_id
                end
            ),
            #{
                supported => true,
                id => Id,
                path => Path,
                display_path => DisplayPath,
                modes => Modes,
                critical => {Id, logger_std_h, file, Path, Modes},
                summary => Summary
            };
        error ->
            unsupported_source(
                IdText, Addressable, logger_std_h_file, log_path_unrepresentable
            )
    end.

valid_file_shape(Path, IdText, Modes) ->
    case
        proper_flat_filename(Path) andalso absolute_path(Path) andalso
            valid_handler_id_size(IdText)
    of
        true -> plain_modes(Modes);
        false -> error
    end.

-spec plain_modes(term()) -> {ok, supported | unsupported} | error.
plain_modes(Modes) ->
    bounded_modes(Modes, 0, supported).

bounded_modes([], _Count, Status) ->
    {ok, Status};
bounded_modes([_Mode | _Rest], ?MAX_MODES, _Status) ->
    error;
bounded_modes([Mode | Rest], Count, Status) ->
    case mode_status(Mode) of
        invalid -> error;
        unsupported -> bounded_modes(Rest, Count + 1, unsupported);
        supported -> bounded_modes(Rest, Count + 1, Status)
    end;
bounded_modes(_Improper, _Count, _Status) ->
    error.

mode_status(Mode) when
    Mode =:= read;
    Mode =:= write;
    Mode =:= append;
    Mode =:= exclusive;
    Mode =:= raw;
    Mode =:= binary;
    Mode =:= sync;
    Mode =:= delayed_write;
    Mode =:= read_ahead
->
    supported;
mode_status({delayed_write, Size, Delay}) when
    is_integer(Size),
    Size >= 0,
    Size =< 16#7FFFFFFF,
    is_integer(Delay),
    Delay >= 0,
    Delay =< 16#7FFFFFFF
->
    supported;
mode_status({read_ahead, Size}) when
    is_integer(Size), Size >= 1, Size =< 16#7FFFFFFF
->
    supported;
mode_status({delayed_write, _Size, _Delay}) ->
    invalid;
mode_status({read_ahead, _Size}) ->
    invalid;
mode_status(_Mode) ->
    unsupported.

unsupported_source(IdText, Addressable, Kind, Reason) ->
    #{
        supported => false,
        summary => summary(IdText, Addressable, Kind, false, Reason)
    }.

summary(Id, Addressable, Kind, Supported, Reason) ->
    #{
        id => Id,
        addressable => Addressable,
        handler_kind => Kind,
        supported => Supported,
        reason_code => Reason
    }.

public_summary(#{summary := Summary}) -> Summary.
public_summaries(Sources) -> [public_summary(Source) || Source <- Sources].

selected_source(Source) ->
    (public_summary(Source))#{
        configured_path => maps:get(display_path, Source),
        active_handler_fd_match => unknown
    }.

selected_capture(Source, Sources, TailCount, Env, State) ->
    read_selected(Source, Sources, TailCount, Env, State, 1).

read_selected(Source, Sources, TailCount, Env, State0, Attempt) ->
    case read_configured_file(Source, Env, State0) of
        {ok, Buffer, CapturedEof, Coverage, State} ->
            finish_read(
                Source, Sources, TailCount, Buffer, CapturedEof, Coverage, Env, State, Attempt
            );
        {race, Coverage, State} when Attempt =:= 1 ->
            retry_baseline(Source, Sources, TailCount, Coverage, Env, State);
        {race, Coverage, State} ->
            selected_unavailable(log_source_changed, Source, Sources, Coverage, State);
        {error, Reason, Coverage, State} ->
            selected_unavailable(Reason, Source, Sources, Coverage, State)
    end.

retry_baseline(Source, Sources, TailCount, Coverage, Env, State0) ->
    case rechecked_source(Source, Env, State0) of
        {ok, NewSource, State} ->
            read_selected(NewSource, Sources, TailCount, Env, State, 2);
        {error, State} ->
            selected_unavailable(log_source_changed, Source, Sources, Coverage, State)
    end.

finish_read(
    Source,
    Sources,
    TailCount,
    Buffer,
    CapturedEof,
    Coverage,
    Env,
    State0,
    Attempt
) ->
    case rechecked_source(Source, Env, State0) of
        {ok, CheckedSource, State} ->
            case same_source(Source, CheckedSource) of
                true ->
                    finish_tail(
                        CheckedSource,
                        Sources,
                        TailCount,
                        Buffer,
                        CapturedEof,
                        Coverage ++ [post_read_verified],
                        State
                    );
                false when Attempt =:= 1 ->
                    read_selected(CheckedSource, Sources, TailCount, Env, State, 2);
                false ->
                    selected_unavailable(log_source_changed, Source, Sources, Coverage, State)
            end;
        {error, State} ->
            selected_unavailable(log_source_changed, Source, Sources, Coverage, State)
    end.

rechecked_source(Source, Env, State0) ->
    Id = maps:get(id, Source),
    {Result, State} = lookup_config(Id, Env, State0),
    Checked = classify_config(Id, Result),
    case maps:get(supported, Checked) of
        true -> {ok, Checked, State};
        false -> {error, State}
    end.

same_source(Left, Right) ->
    maps:get(critical, Left) =:= maps:get(critical, Right).

finish_tail(Source, Sources, Requested, Buffer, CapturedEof, Coverage, State) ->
    Start = CapturedEof - byte_size(Buffer),
    Tail = tail(Buffer, Start, CapturedEof, Requested),
    Data = data(public_summaries(Sources), selected_source(Source), Tail),
    case maps:get(content_truncated, Tail) of
        false ->
            {ok, Data, Coverage, [effect(State)]};
        true ->
            Reason =
                case maps:get(truncation_reasons, Tail) of
                    [byte_cap | _] -> log_byte_cap_reached;
                    _ -> log_line_cap_reached
                end,
            {error, Reason, Data, Coverage, [effect(State)]}
    end.

selected_unavailable(Reason, Source, Sources, Coverage, State) ->
    unavailable(
        Reason,
        data(public_summaries(Sources), selected_source(Source), null),
        Coverage,
        State
    ).

-ifdef(TEST).
read_configured_file(Source, Env) ->
    State = #{handler_ids_enumerated => false, lookups => 0, attempts => 0},
    read_configured_file(Source, Env, State).
-endif.

read_configured_file(Source, Env, State0) ->
    State = State0#{attempts := maps:get(attempts, State0) + 1},
    Path = maps:get(path, Source),
    case call_file(read_link_info, [Path], Env) of
        {ok, #file_info{type = regular} = PathInfo} ->
            open_configured_file(Path, PathInfo, Env, State);
        {ok, #file_info{}} ->
            {error, unsupported_log_file_type, [source_classification_complete, source_selected],
                State};
        {error, _Reason} ->
            {error, log_file_unavailable, [source_classification_complete, source_selected], State};
        _ ->
            {error, log_file_read_failed, [source_classification_complete, source_selected], State}
    end.

open_configured_file(Path, PathInfo, Env, State) ->
    Coverage = [source_classification_complete, source_selected, path_prechecked],
    case call_file(open, [Path], Env) of
        {ok, Fd} ->
            try
                read_open_file(Path, PathInfo, Fd, Env, State, Coverage)
            after
                _ = call_file(close, [Fd], Env)
            end;
        {error, enoent} ->
            {race, Coverage, State};
        {error, _Reason} ->
            {error, log_file_unavailable, Coverage, State};
        _ ->
            {error, log_file_read_failed, Coverage, State}
    end.

read_open_file(Path, PathInfo, Fd, Env, State, Coverage) ->
    case call_file(read_file_info, [Fd], Env) of
        {ok, #file_info{type = regular} = FdInfo} ->
            case {file_identity(PathInfo), file_identity(FdInfo)} of
                {{ok, Identity}, {ok, Identity}} ->
                    read_seekable_file(
                        Path,
                        Identity,
                        Fd,
                        Env,
                        State,
                        Coverage ++ [fd_identity_verified]
                    );
                {{ok, _}, {ok, _}} ->
                    {race, Coverage, State};
                _ ->
                    {error, log_file_identity_unavailable, Coverage, State}
            end;
        {ok, #file_info{}} ->
            {error, unsupported_log_file_type, Coverage, State};
        {error, _Reason} ->
            {error, log_file_read_failed, Coverage, State};
        _ ->
            {error, log_file_read_failed, Coverage, State}
    end.

read_seekable_file(Path, Identity, Fd, Env, State, Coverage) ->
    case call_file(position, [Fd], Env) of
        {ok, CapturedEof} when
            is_integer(CapturedEof), CapturedEof >= 0, CapturedEof =< ?MAX_OFFSET
        ->
            Start = max(0, CapturedEof - ?MAX_READ_BYTES),
            Length = CapturedEof - Start,
            case pread(Fd, Start, Length, Env) of
                {ok, Buffer} when byte_size(Buffer) =:= Length ->
                    verify_after_read(
                        Path,
                        Identity,
                        Fd,
                        Buffer,
                        CapturedEof,
                        Env,
                        State,
                        Coverage ++ [bytes_captured]
                    );
                {ok, _Short} ->
                    {race, Coverage, State};
                eof ->
                    {race, Coverage, State};
                _ ->
                    {error, log_file_read_failed, Coverage, State}
            end;
        {error, _Reason} ->
            {error, unsupported_log_file_type, Coverage, State};
        _ ->
            {error, log_file_read_failed, Coverage, State}
    end.

pread(_Fd, _Start, 0, _Env) ->
    {ok, <<>>};
pread(Fd, Start, Length, Env) ->
    call_file(pread, [Fd, Start, Length], Env).

verify_after_read(Path, Identity, Fd, Buffer, CapturedEof, Env, State, Coverage) ->
    case call_file(read_file_info, [Fd], Env) of
        {ok, #file_info{type = regular, size = Size} = FdInfo} when
            is_integer(Size), Size >= CapturedEof
        ->
            case file_identity(FdInfo) of
                {ok, Identity} ->
                    verify_path_after_read(
                        Path, Identity, Buffer, CapturedEof, Env, State, Coverage
                    );
                {ok, _Changed} ->
                    {race, Coverage, State};
                error ->
                    {error, log_file_identity_unavailable, Coverage, State}
            end;
        {ok, #file_info{type = regular}} ->
            {race, Coverage, State};
        {ok, #file_info{}} ->
            {error, unsupported_log_file_type, Coverage, State};
        _ ->
            {error, log_file_read_failed, Coverage, State}
    end.

verify_path_after_read(Path, Identity, Buffer, CapturedEof, Env, State, Coverage) ->
    case call_file(read_link_info, [Path], Env) of
        {ok, #file_info{type = regular} = PathInfo} ->
            case file_identity(PathInfo) of
                {ok, Identity} -> {ok, Buffer, CapturedEof, Coverage, State};
                {ok, _Changed} -> {race, Coverage, State};
                error -> {error, log_file_identity_unavailable, Coverage, State}
            end;
        {ok, #file_info{}} ->
            {error, unsupported_log_file_type, Coverage, State};
        {error, _Reason} ->
            {race, Coverage, State};
        _ ->
            {error, log_file_read_failed, Coverage, State}
    end.

file_identity(#file_info{major_device = Major, inode = Inode}) when
    is_integer(Major), Major >= 0, is_integer(Inode), Inode > 0
->
    {ok, {Major, Inode}};
file_identity(_Info) ->
    error.

-spec tail(binary(), non_neg_integer(), non_neg_integer(), 1..?MAX_TAIL) -> map().
tail(Buffer, Start, CapturedEof, Requested) when
    is_binary(Buffer),
    is_integer(Start),
    Start >= 0,
    is_integer(CapturedEof),
    CapturedEof >= byte_size(Buffer),
    is_integer(Requested),
    Requested >= 1,
    Requested =< ?MAX_TAIL
->
    {Segments, TailLimited} = physical_tail(Buffer, Requested),
    ByteFragment = Start > 0 andalso not TailLimited,
    {Lines, LineIndexes} = cap_lines(Segments, ByteFragment, 0, [], []),
    ByteIndexes =
        case ByteFragment andalso Lines =/= [] of
            true -> [0];
            false -> []
        end,
    Indexes = lists:usort(ByteIndexes ++ LineIndexes),
    Reasons0 =
        case ByteIndexes of
            [] -> [];
            _ -> [byte_cap]
        end,
    Reasons =
        case LineIndexes of
            [] -> Reasons0;
            _ -> Reasons0 ++ [line_cap]
        end,
    ContentTruncated = Reasons =/= [],
    HasMore = Start > 0 orelse TailLimited orelse ContentTruncated,
    #{
        scope => configured_path,
        active_handler_fd_match => unknown,
        visibility => reader_visible,
        command_filesync_requested => false,
        consistency => non_atomic,
        content_trust => untrusted,
        requested_lines => Requested,
        returned_lines => length(Lines),
        captured_eof_bytes => CapturedEof,
        bytes_read => byte_size(Buffer),
        has_more => HasMore,
        content_truncated => ContentTruncated,
        truncation_reasons => Reasons,
        truncated_line_indexes => Indexes,
        lines => Lines
    }.

physical_tail(<<>>, _Requested) ->
    {[], false};
physical_tail(Buffer, Requested) ->
    Size = byte_size(Buffer),
    {Position, End} =
        case binary:at(Buffer, Size - 1) of
            $\n -> {Size - 2, Size - 1};
            _ -> {Size - 1, Size}
        end,
    scan_lines(Buffer, Position, End, Requested, []).

scan_lines(Buffer, Position, End, Remaining, Acc) when Position >= 0 ->
    case binary:at(Buffer, Position) of
        $\n ->
            Segment = segment(Buffer, Position + 1, End),
            case Remaining of
                1 -> {[Segment | Acc], true};
                _ -> scan_lines(Buffer, Position - 1, Position, Remaining - 1, [Segment | Acc])
            end;
        _ ->
            scan_lines(Buffer, Position - 1, End, Remaining, Acc)
    end;
scan_lines(Buffer, _Position, End, _Remaining, Acc) ->
    {[segment(Buffer, 0, End) | Acc], false}.

segment(Buffer, Start, End) ->
    Length = End - Start,
    Raw = binary:part(Buffer, Start, Length),
    case Raw of
        <<Prefix:(Length - 1)/binary, $\r>> when Length > 0 -> Prefix;
        _ -> Raw
    end.

cap_lines([], _ByteFragment, _Index, Lines, Indexes) ->
    {lists:reverse(Lines), lists:reverse(Indexes)};
cap_lines([Line | Rest], ByteFragment, Index, Lines, Indexes) when
    byte_size(Line) > ?MAX_LINE_BYTES
->
    Start = byte_size(Line) - ?MAX_LINE_BYTES,
    Capped = skip_leading_continuations(binary:part(Line, Start, ?MAX_LINE_BYTES), 3),
    cap_lines(Rest, ByteFragment, Index + 1, [Capped | Lines], [Index | Indexes]);
cap_lines([Line | Rest], ByteFragment, Index, Lines, Indexes) ->
    Capped =
        case ByteFragment andalso Index =:= 0 of
            true -> skip_leading_continuations(Line, 3);
            false -> Line
        end,
    cap_lines(Rest, ByteFragment, Index + 1, [Capped | Lines], Indexes).

skip_leading_continuations(<<Byte, Rest/binary>>, Remaining) when
    Remaining > 0, Byte >= 16#80, Byte =< 16#BF
->
    skip_leading_continuations(Rest, Remaining - 1);
skip_leading_continuations(Binary, _Remaining) ->
    Binary.

-spec addressable_handler_id(unicode:chardata()) -> boolean().
addressable_handler_id(Text) ->
    case unicode:characters_to_binary(Text) of
        Binary when is_binary(Binary) ->
            case unicode:characters_to_list(Binary) of
                Codepoints when is_list(Codepoints) ->
                    valid_handler_id_size(Binary) andalso
                        not starts_option(Binary) andalso
                        lists:all(fun safe_handler_codepoint/1, Codepoints);
                _ ->
                    false
            end;
        _ ->
            false
    end.

valid_handler_id_size(Binary) ->
    byte_size(Binary) >= 1 andalso byte_size(Binary) =< ?MAX_HANDLER_ID_BYTES andalso
        case unicode:characters_to_list(Binary) of
            Codepoints when is_list(Codepoints) ->
                length(Codepoints) =< ?MAX_HANDLER_ID_CODEPOINTS;
            _ ->
                false
        end.

starts_option(<<"--", _/binary>>) -> true;
starts_option(_Binary) -> false.

safe_handler_codepoint(Codepoint) ->
    not terminal_control(Codepoint).

terminal_control(Codepoint) when Codepoint < 16#20 -> true;
terminal_control(Codepoint) when Codepoint >= 16#7F, Codepoint =< 16#9F -> true;
terminal_control(16#061C) -> true;
terminal_control(16#200E) -> true;
terminal_control(16#200F) -> true;
terminal_control(16#2028) -> true;
terminal_control(16#2029) -> true;
terminal_control(Codepoint) when Codepoint >= 16#202A, Codepoint =< 16#202E -> true;
terminal_control(Codepoint) when Codepoint >= 16#2066, Codepoint =< 16#2069 -> true;
terminal_control(_Codepoint) -> false.

proper_flat_filename([]) -> false;
proper_flat_filename(Path) -> proper_flat_filename_tail(Path).

proper_flat_filename_tail([]) ->
    true;
proper_flat_filename_tail([Character | Rest]) when
    is_integer(Character), Character >= 0, Character < 16#D800
->
    proper_flat_filename_tail(Rest);
proper_flat_filename_tail([Character | Rest]) when
    is_integer(Character), Character > 16#DFFF, Character =< 16#10FFFF
->
    proper_flat_filename_tail(Rest);
proper_flat_filename_tail(_Improper) ->
    false.

absolute_path(Path) ->
    try filename:pathtype(Path) of
        absolute -> true;
        _ -> false
    catch
        _:_ -> false
    end.

display_path(Path) ->
    case unicode:characters_to_binary(Path) of
        Binary when is_binary(Binary), byte_size(Binary) =< ?MAX_PATH_BYTES ->
            case unicode:characters_to_list(Binary) of
                Path -> {ok, Binary};
                _ -> error
            end;
        _ ->
            error
    end.

existing_handler_atom(Handler) ->
    try binary_to_existing_atom(Handler, utf8) of
        Id -> {ok, Id}
    catch
        error:badarg -> error;
        error:system_limit -> error
    end.

data(Sources, Selected, Tail) ->
    #{sources => Sources, selected_source => Selected, tail => Tail}.

unavailable(Reason, Data, Coverage, State) ->
    {unavailable, Reason, Data, Coverage, [effect(State)]}.

effect(State) ->
    #{
        id => configured_log_read,
        handler_ids_enumerated => maps:get(handler_ids_enumerated, State),
        handler_config_lookups => maps:get(lookups, State),
        read_attempts => maps:get(attempts, State),
        raw_read_cap_bytes => ?MAX_READ_BYTES,
        atime_may_change => true,
        consistency => non_atomic,
        command_filesync_attempted => false
    }.

default_env() ->
    #{
        os_type => fun os:type/0,
        handler_ids => fun logger:get_handler_ids/0,
        handler_config => fun logger:get_handler_config/1,
        read_link_info => fun(Path) -> file:read_link_info(Path, [raw]) end,
        open => fun(Path) -> file:open(Path, [read, binary, raw]) end,
        read_file_info => fun(Fd) -> file:read_file_info(Fd, [raw]) end,
        position => fun(Fd) -> file:position(Fd, eof) end,
        pread => fun file:pread/3,
        close => fun file:close/1
    }.

call_env(Key, Arguments, Env) ->
    erlang:apply(maps:get(Key, Env), Arguments).

call_file(Key, Arguments, Env) ->
    try call_env(Key, Arguments, Env) of
        Result -> Result
    catch
        _:_ -> {error, failed}
    end.
