-module(observer_cli_log_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

addressable_handler_id_test() ->
    ?assert(observer_cli_log:addressable_handler_id(<<"app file">>)),
    ?assert(observer_cli_log:addressable_handler_id(<<"日志"/utf8>>)),
    ?assertNot(observer_cli_log:addressable_handler_id(<<>>)),
    ?assertNot(observer_cli_log:addressable_handler_id(<<"--app">>)),
    ?assertNot(observer_cli_log:addressable_handler_id(<<"app\nfile">>)),
    ?assertNot(observer_cli_log:addressable_handler_id(<<"app", 16#E2, 16#80, 16#AE>>)),
    ?assertNot(observer_cli_log:addressable_handler_id(binary:copy(<<"a">>, 256))),
    ?assertNot(observer_cli_log:addressable_handler_id(<<255>>)).

terminal_control_handler_ids_are_rejected_test() ->
    lists:foreach(
        fun(Codepoint) ->
            ?assertNot(
                observer_cli_log:addressable_handler_id(
                    unicode:characters_to_binary([Codepoint])
                )
            )
        end,
        [16#85, 16#061C, 16#200E, 16#200F, 16#2028, 16#2029, 16#2066]
    ).

invalid_request_is_rejected_before_access_test() ->
    ?assertEqual({probe_error, invalid_request}, observer_cli_log:capture(#{})).

plain_mode_allowlist_test() ->
    ?assertEqual(
        {ok, supported},
        observer_cli_log:plain_modes([
            read,
            write,
            append,
            exclusive,
            raw,
            binary,
            sync,
            delayed_write,
            {delayed_write, 0, 16#7FFFFFFF},
            read_ahead,
            {read_ahead, 1}
        ])
    ),
    ?assertEqual({ok, unsupported}, observer_cli_log:plain_modes([compressed])),
    ?assertEqual({ok, unsupported}, observer_cli_log:plain_modes([{zstd, 3}])),
    ?assertEqual(error, observer_cli_log:plain_modes([{read_ahead, 0}])),
    ?assertEqual(error, observer_cli_log:plain_modes([{delayed_write, -1, 1}])),
    ?assertEqual(error, observer_cli_log:plain_modes([raw | improper])),
    ?assertEqual(error, observer_cli_log:plain_modes(lists:duplicate(33, raw))).

config_classification_test() ->
    Path = absolute_path("observer-cli-log-test.log"),
    Supported = observer_cli_log:classify_config(app_file, config(app_file, Path, [raw, append])),
    ?assertMatch(
        #{
            supported := true,
            summary := #{
                id := <<"app_file">>,
                addressable := true,
                handler_kind := logger_std_h_file,
                reason_code := null
            }
        },
        Supported
    ),
    ?assertMatch(
        #{summary := #{reason_code := unsupported_file_modes}},
        observer_cli_log:classify_config(app_file, config(app_file, Path, [compressed]))
    ),
    ?assertMatch(
        #{summary := #{reason_code := invalid_log_handler_config}},
        observer_cli_log:classify_config(app_file, config(app_file, "relative.log", [raw]))
    ),
    ?assertMatch(
        #{summary := #{reason_code := unsupported_log_handler}},
        observer_cli_log:classify_config(
            app_file,
            {ok, #{
                id => app_file,
                module => logger_std_h,
                config => #{type => standard_io}
            }}
        )
    ),
    ?assertMatch(
        #{summary := #{reason_code := invalid_log_handler_config}},
        observer_cli_log:classify_config(
            app_file,
            {ok, #{
                id => another_handler,
                module => logger_std_h,
                config => #{type => file, file => Path, modes => [raw]}
            }}
        )
    ),
    %% A trusted callback can spoof this shape; the command deliberately does not claim otherwise.
    ?assertMatch(
        #{supported := true},
        observer_cli_log:classify_config(app_file, config(app_file, Path, [raw]))
    ).

invalid_config_shapes_are_rejected_test() ->
    Path = absolute_path("observer-cli-log-test.log"),
    lists:foreach(
        fun(Result) ->
            ?assertMatch(
                #{summary := #{reason_code := invalid_log_handler_config}},
                observer_cli_log:classify_config(app_file, Result)
            )
        end,
        [
            {ok, #{id => app_file, module => logger_std_h, config => #{}}},
            {ok, #{
                id => app_file,
                module => logger_std_h,
                config => #{type => file, file => Path}
            }},
            config(app_file, [], [raw]),
            config(app_file, [$/ | improper], [raw])
        ]
    ),
    SupplementaryPath = Path ++ [16#1F600],
    ?assertMatch(
        #{supported := true},
        observer_cli_log:classify_config(
            app_file, config(app_file, SupplementaryPath, [raw])
        )
    ),
    OversizedPath = "/" ++ lists:duplicate(4097, $a),
    ?assertMatch(
        #{summary := #{reason_code := log_path_unrepresentable}},
        observer_cli_log:classify_config(app_file, config(app_file, OversizedPath, [raw]))
    ).

unaddressable_supported_source_test() ->
    Path = absolute_path("observer-cli-log-test.log"),
    Source = observer_cli_log:classify_config('--hidden', config('--hidden', Path, [raw])),
    ?assertMatch(
        #{
            supported := true,
            summary := #{addressable := false, reason_code := unaddressable_handler_id}
        },
        Source
    ).

physical_tail_semantics_test() ->
    lists:foreach(
        fun({Bytes, Expected}) ->
            Tail = observer_cli_log:tail(Bytes, 0, byte_size(Bytes), 200),
            ?assertEqual(Expected, maps:get(lines, Tail)),
            ?assertEqual(false, maps:get(has_more, Tail)),
            ?assertEqual(false, maps:get(content_truncated, Tail))
        end,
        [
            {<<>>, []},
            {<<"a">>, [<<"a">>]},
            {<<"a\n">>, [<<"a">>]},
            {<<"\n">>, [<<>>]},
            {<<"a\n\n">>, [<<"a">>, <<>>]},
            {<<"a\r\n">>, [<<"a">>]},
            {<<"a\nb\nc">>, [<<"a">>, <<"b">>, <<"c">>]}
        ]
    ).

tail_limit_is_complete_content_test() ->
    Tail = observer_cli_log:tail(<<"a\nb\nc">>, 0, 5, 2),
    ?assertEqual([<<"b">>, <<"c">>], maps:get(lines, Tail)),
    ?assertEqual(true, maps:get(has_more, Tail)),
    ?assertEqual(false, maps:get(content_truncated, Tail)),
    ?assertEqual([], maps:get(truncation_reasons, Tail)).

byte_cap_fragment_test() ->
    Buffer = <<16#80, 16#80, 16#80, "retained">>,
    Tail = observer_cli_log:tail(Buffer, 1, byte_size(Buffer) + 1, 2),
    ?assertEqual([<<"retained">>], maps:get(lines, Tail)),
    ?assertEqual([byte_cap], maps:get(truncation_reasons, Tail)),
    ?assertEqual([0], maps:get(truncated_line_indexes, Tail)),
    ?assertEqual(true, maps:get(content_truncated, Tail)).

byte_cap_dropped_fragment_is_complete_test() ->
    Buffer = <<"fragment\na\nb">>,
    Tail = observer_cli_log:tail(Buffer, 10, 22, 2),
    ?assertEqual([<<"a">>, <<"b">>], maps:get(lines, Tail)),
    ?assertEqual(true, maps:get(has_more, Tail)),
    ?assertEqual(false, maps:get(content_truncated, Tail)).

line_cap_test() ->
    Long = <<16#80, 16#80, 16#80, (binary:copy(<<"x">>, 32768))/binary>>,
    Tail = observer_cli_log:tail(Long, 0, byte_size(Long), 1),
    [Line] = maps:get(lines, Tail),
    ?assertEqual(32768, byte_size(Line)),
    ?assertEqual([line_cap], maps:get(truncation_reasons, Tail)),
    ?assertEqual([0], maps:get(truncated_line_indexes, Tail)).

byte_and_line_cap_test() ->
    Buffer = binary:copy(<<"x">>, 65536),
    Tail = observer_cli_log:tail(Buffer, 1, 65537, 1),
    ?assertEqual([byte_cap, line_cap], maps:get(truncation_reasons, Tail)),
    ?assertEqual([0], maps:get(truncated_line_indexes, Tail)),
    ?assertEqual(32768, byte_size(hd(maps:get(lines, Tail)))).

complete_invalid_utf8_is_not_boundary_trimmed_test() ->
    Tail = observer_cli_log:tail(<<16#80, "x">>, 0, 2, 1),
    ?assertEqual([<<16#80, "x">>], maps:get(lines, Tail)).

empty_capture_avoids_pread_test() ->
    erase(pread_called),
    Env = (mock_env(default, <<>>))#{
        pread := fun(_Fd, _Start, _Length) ->
            put(pread_called, true),
            {ok, <<>>}
        end
    },
    {ok, Data, _Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => null, tail => 200}, Env
    ),
    ?assertEqual(undefined, get(pread_called)),
    ?assertEqual([], maps:get(lines, maps:get(tail, Data))),
    ?assertEqual(2, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(1, maps:get(read_attempts, Effect)).

automatic_capture_is_bounded_test() ->
    Env = mock_env(default, <<"before\nmarker\n">>),
    {ok, Data, Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => null, tail => 1}, Env
    ),
    ?assertEqual([<<"marker">>], maps:get(lines, maps:get(tail, Data))),
    ?assertEqual(
        [
            source_classification_complete,
            source_selected,
            path_prechecked,
            fd_identity_verified,
            bytes_captured,
            post_read_verified
        ],
        Coverage
    ),
    ?assertEqual(true, maps:get(handler_ids_enumerated, Effect)),
    ?assertEqual(2, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(1, maps:get(read_attempts, Effect)).

explicit_capture_does_not_enumerate_test() ->
    Env = (mock_env(default, <<"marker">>))#{
        handler_ids := fun() -> erlang:error(must_not_enumerate) end
    },
    {ok, _Data, _Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200}, Env
    ),
    ?assertEqual(false, maps:get(handler_ids_enumerated, Effect)),
    ?assertEqual(2, maps:get(handler_config_lookups, Effect)).

missing_explicit_existing_atom_is_not_found_test() ->
    Env = (mock_env(default, <<>>))#{
        handler_ids := fun() -> erlang:error(must_not_enumerate) end,
        handler_config := fun(Id) -> {error, {not_found, Id}} end
    },
    {unavailable, log_handler_not_found, Data, Coverage, [Effect]} =
        observer_cli_log:capture(#{handler => <<"default">>, tail => 200}, Env),
    ?assertEqual([], maps:get(sources, Data)),
    ?assertEqual([source_classification_complete], Coverage),
    ?assertEqual(false, maps:get(handler_ids_enumerated, Effect)),
    ?assertEqual(1, maps:get(handler_config_lookups, Effect)).

missing_explicit_handler_does_not_create_atom_test() ->
    Handler = <<"observer_cli_missing_log_handler_936452">>,
    ?assertError(badarg, binary_to_existing_atom(Handler, utf8)),
    Env = (mock_env(default, <<>>))#{
        handler_ids := fun() -> erlang:error(must_not_enumerate) end,
        handler_config := fun(_Id) -> erlang:error(must_not_lookup) end
    },
    {unavailable, log_handler_not_found, _Data, _Coverage, [Effect]} =
        observer_cli_log:capture(#{handler => Handler, tail => 200}, Env),
    ?assertEqual(0, maps:get(handler_config_lookups, Effect)),
    ?assertError(badarg, binary_to_existing_atom(Handler, utf8)).

missing_handler_batch_does_not_grow_atom_table_test() ->
    _ = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 1}, mock_env(default, <<>>)
    ),
    Env = (mock_env(default, <<>>))#{
        handler_ids := fun() -> erlang:error(must_not_enumerate) end,
        handler_config := fun(_Id) -> erlang:error(must_not_lookup) end
    },
    Handlers = [
        iolist_to_binary(io_lib:format("observer_cli_missing_log_handler_~B", [Index]))
     || Index <- lists:seq(1, 100)
    ],
    Before = erlang:system_info(atom_count),
    lists:foreach(
        fun(Handler) ->
            {unavailable, log_handler_not_found, _Data, _Coverage, [_Effect]} =
                observer_cli_log:capture(#{handler => Handler, tail => 1}, Env)
        end,
        Handlers
    ),
    ?assertEqual(Before, erlang:system_info(atom_count)).

maximum_addressable_non_atom_is_not_created_test() ->
    Handler = unicode:characters_to_binary(lists:duplicate(255, 16#1F600)),
    ?assert(observer_cli_log:addressable_handler_id(Handler)),
    Before = erlang:system_info(atom_count),
    {unavailable, log_handler_not_found, _Data, _Coverage, [Effect]} =
        observer_cli_log:capture(
            #{handler => Handler, tail => 1},
            (mock_env(default, <<>>))#{
                handler_ids := fun() -> erlang:error(must_not_enumerate) end,
                handler_config := fun(_Id) -> erlang:error(must_not_lookup) end
            }
        ),
    ?assertEqual(0, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(Before, erlang:system_info(atom_count)).

scan_budget_rejects_before_lookup_test() ->
    Env = (mock_env(default, <<>>))#{
        handler_ids := fun() -> lists:duplicate(65, default) end,
        handler_config := fun(_Id) -> erlang:error(must_not_lookup) end
    },
    {unavailable, scan_budget_exceeded, _Data, [], [Effect]} = observer_cli_log:capture(
        #{handler => null, tail => 200}, Env
    ),
    ?assertEqual(0, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(0, maps:get(read_attempts, Effect)).

source_discovery_failures_are_sanitized_test() ->
    Base = mock_env(default, <<>>),
    lists:foreach(
        fun(Env) ->
            ?assertMatch(
                {unavailable, log_source_unavailable, _, _, [_]},
                observer_cli_log:capture(#{handler => null, tail => 200}, Env)
            )
        end,
        [
            Base#{handler_ids := fun() -> erlang:error(logger_unavailable) end},
            Base#{handler_ids := fun() -> [default, <<"invalid">>] end},
            Base#{handler_ids := fun() -> [default | improper] end},
            Base#{handler_config := fun(_Id) -> erlang:error(logger_unavailable) end}
        ]
    ).

automatic_retry_stays_within_callback_bound_test() ->
    erase(config_callback_count),
    erase(open_count),
    Others = lists:sublist(
        [Module || {Module, _Path} <- code:all_loaded(), Module =/= default], 63
    ),
    ?assertEqual(63, length(Others)),
    Base = mock_env(default, <<"marker">>),
    Env = Base#{
        handler_ids := fun() -> [default | Others] end,
        handler_config := fun(Id) ->
            put(config_callback_count, get_count(config_callback_count) + 1),
            case Id of
                default ->
                    config(default, absolute_path("observer-cli-log-test.log"), [raw]);
                _ ->
                    {ok, #{
                        id => Id,
                        module => logger_std_h,
                        config => #{type => standard_io}
                    }}
            end
        end,
        open := fun(_Path) ->
            Count = get_count(open_count),
            put(open_count, Count + 1),
            case Count of
                0 -> {error, enoent};
                _ -> {ok, mock_fd}
            end
        end
    },
    {ok, Data, _Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => null, tail => 200}, Env
    ),
    ?assertEqual(64, length(maps:get(sources, Data))),
    ?assertEqual(66, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(66, get(config_callback_count)),
    ?assertEqual(2, maps:get(read_attempts, Effect)).

platform_gate_precedes_logger_access_test() ->
    Env = (mock_env(default, <<>>))#{
        os_type := fun() -> {win32, nt} end,
        handler_ids := fun() -> erlang:error(must_not_enumerate) end,
        handler_config := fun(_Id) -> erlang:error(must_not_lookup) end
    },
    {unavailable, unsupported_target_platform, _Data, [], [Effect]} =
        observer_cli_log:capture(#{handler => null, tail => 200}, Env),
    ?assertEqual(false, maps:get(handler_ids_enumerated, Effect)),
    ?assertEqual(0, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(0, maps:get(read_attempts, Effect)).

multiple_sources_require_handler_without_paths_test() ->
    Path = absolute_path("observer-cli-log-test.log"),
    Env = (mock_env(default, <<>>))#{
        handler_ids := fun() -> [default, error_logger] end,
        handler_config := fun(Id) -> config(Id, Path, [raw]) end
    },
    {unavailable, log_handler_required, Data, _Coverage, [_Effect]} =
        observer_cli_log:capture(#{handler => null, tail => 200}, Env),
    ?assertEqual(2, length(maps:get(sources, Data))),
    ?assertEqual(false, contains_key(configured_path, Data)).

retry_keeps_explicit_lookup_bound_test() ->
    erase(open_count),
    Base = mock_env(default, <<"marker">>),
    Env = Base#{
        open := fun(_Path) ->
            Count = get_count(open_count),
            put(open_count, Count + 1),
            case Count of
                0 -> {error, enoent};
                _ -> {ok, mock_fd}
            end
        end
    },
    {ok, _Data, _Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200}, Env
    ),
    ?assertEqual(3, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(2, maps:get(read_attempts, Effect)).

changed_config_is_retry_baseline_test() ->
    erase(config_count),
    Path1 = absolute_path("observer-cli-log-1.log"),
    Path2 = absolute_path("observer-cli-log-2.log"),
    Env = (mock_env(default, <<"marker">>))#{
        handler_config := fun(Id) ->
            Count = get_count(config_count),
            put(config_count, Count + 1),
            case Count of
                0 -> config(Id, Path1, [raw]);
                _ -> config(Id, Path2, [raw])
            end
        end
    },
    {ok, Data, _Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200}, Env
    ),
    ?assertEqual(list_to_binary(Path2), maps:get(configured_path, maps:get(selected_source, Data))),
    ?assertEqual(3, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(2, maps:get(read_attempts, Effect)).

post_read_missing_config_discards_bytes_test() ->
    erase(config_count),
    Path = absolute_path("observer-cli-log-test.log"),
    Env = (mock_env(default, <<"secret-marker">>))#{
        handler_config := fun(Id) ->
            Count = get_count(config_count),
            put(config_count, Count + 1),
            case Count of
                0 -> config(Id, Path, [raw]);
                _ -> {error, not_found}
            end
        end
    },
    {unavailable, log_source_changed, Data, Coverage, [Effect]} = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200}, Env
    ),
    ?assertEqual(null, maps:get(tail, Data)),
    ?assertNot(lists:member(post_read_verified, Coverage)),
    ?assertEqual(2, maps:get(handler_config_lookups, Effect)),
    ?assertEqual(false, contains_value(<<"secret-marker">>, Data)).

internal_markers_do_not_reach_any_encoder_test() ->
    Secret = <<"observer-cli-secret-marker">>,
    Base = mock_env(default, binary:copy(<<"x">>, 100)),
    ReadFailure = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200},
        Base#{read_link_info := fun(_Path) -> {error, {eacces, Secret}} end}
    ),
    ConfigFailure = observer_cli_log:capture(
        #{handler => null, tail => 200},
        Base#{handler_config := fun(_Id) -> {error, {callback_failed, Secret}} end}
    ),
    ShortRead = observer_cli_log:capture(
        #{handler => <<"default">>, tail => 200},
        Base#{pread := fun(_Fd, _Start, _Length) -> {ok, Secret} end}
    ),
    UnselectedPath = binary_to_list(
        <<"/tmp/", Secret/binary, "/unselected.log">>
    ),
    Multiple = observer_cli_log:capture(
        #{handler => null, tail => 200},
        Base#{
            handler_ids := fun() -> [default, error_logger] end,
            handler_config := fun
                (default) -> config(default, absolute_path("selected.log"), [raw]);
                (error_logger) -> config(error_logger, UnselectedPath, [raw])
            end
        }
    ),
    lists:foreach(
        fun(Result) ->
            ?assertEqual(nomatch, binary:match(term_to_binary(Result), Secret)),
            Response = public_error_response(Result),
            lists:foreach(
                fun(Format) ->
                    case observer_cli_cli:encode(Format, Response) of
                        {ok, Encoded} ->
                            ?assertEqual(nomatch, binary:match(Encoded, Secret));
                        {error, #{reason := json_unavailable}} when Format =:= json ->
                            ok
                    end
                end,
                [text, term, json]
            )
        end,
        [ReadFailure, ConfigFailure, ShortRead, Multiple]
    ).

nonseekable_file_is_rejected_and_closed_test() ->
    erase(close_count),
    Env = (mock_env(default, <<"marker">>))#{
        position := fun(_Fd) -> {error, espipe} end,
        close := fun(_Fd) ->
            put(close_count, get_count(close_count) + 1),
            ok
        end
    },
    {unavailable, unsupported_log_file_type, Data, _Coverage, [Effect]} =
        observer_cli_log:capture(#{handler => <<"default">>, tail => 200}, Env),
    ?assertEqual(null, maps:get(tail, Data)),
    ?assertEqual(1, maps:get(read_attempts, Effect)),
    ?assertEqual(1, get(close_count)).

file_read_failures_are_classified_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    try
        ok = file:write_file(Path, <<"marker">>),
        {ok, Info} = file:read_link_info(Path, [raw]),
        Base = file_env(),
        ChangedInfo = Info#file_info{inode = Info#file_info.inode + 1},
        InvalidInfo = Info#file_info{inode = 0},
        lists:foreach(
            fun({Expected, Overrides}) ->
                ?assertEqual(
                    Expected,
                    file_result(
                        observer_cli_log:read_configured_file(
                            source(Path), maps:merge(Base, Overrides)
                        )
                    )
                )
            end,
            [
                {log_file_read_failed, #{read_link_info => fun(_Path) -> unexpected end}},
                {log_file_unavailable, #{open => fun(_Path) -> {error, eacces} end}},
                {log_file_read_failed, #{open => fun(_Path) -> unexpected end}},
                {race, #{read_file_info => fun(_Fd) -> {ok, ChangedInfo} end}},
                {log_file_identity_unavailable, #{
                    read_file_info => fun(_Fd) -> {ok, InvalidInfo} end
                }},
                {unsupported_log_file_type, #{
                    read_file_info => fun(_Fd) -> {ok, Info#file_info{type = directory}} end
                }},
                {log_file_read_failed, #{read_file_info => fun(_Fd) -> {error, eio} end}},
                {log_file_read_failed, #{read_file_info => fun(_Fd) -> unexpected end}},
                {race, #{pread => fun(_Fd, _Start, _Length) -> eof end}},
                {log_file_read_failed, #{
                    pread => fun(_Fd, _Start, _Length) -> {error, eio} end
                }},
                {log_file_read_failed, #{position => fun(_Fd) -> unexpected end}},
                {log_file_unavailable, #{
                    read_link_info => fun(_Path) -> erlang:error(file_server_down) end
                }}
            ]
        )
    after
        file:del_dir_r(Dir)
    end.

post_read_file_changes_are_classified_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    try
        ok = file:write_file(Path, <<"marker">>),
        {ok, Info} = file:read_link_info(Path, [raw]),
        Base = file_env(),
        ChangedInfo = Info#file_info{inode = Info#file_info.inode + 1},
        InvalidInfo = Info#file_info{inode = 0},
        ShortInfo = Info#file_info{size = Info#file_info.size - 1},
        lists:foreach(
            fun({Expected, Overrides}) ->
                ?assertEqual(
                    Expected,
                    file_result(
                        observer_cli_log:read_configured_file(
                            source(Path), maps:merge(Base, Overrides)
                        )
                    )
                )
            end,
            [
                {race, #{
                    read_file_info => sequence([
                        fun(Fd) -> file:read_file_info(Fd, [raw]) end,
                        {ok, ChangedInfo}
                    ])
                }},
                {log_file_identity_unavailable, #{
                    read_file_info => sequence([
                        fun(Fd) -> file:read_file_info(Fd, [raw]) end,
                        {ok, InvalidInfo}
                    ])
                }},
                {race, #{
                    read_file_info => sequence([
                        fun(Fd) -> file:read_file_info(Fd, [raw]) end,
                        {ok, ShortInfo}
                    ])
                }},
                {unsupported_log_file_type, #{
                    read_file_info => sequence([
                        fun(Fd) -> file:read_file_info(Fd, [raw]) end,
                        {ok, Info#file_info{type = directory}}
                    ])
                }},
                {log_file_read_failed, #{
                    read_file_info => sequence([
                        fun(Fd) -> file:read_file_info(Fd, [raw]) end,
                        unexpected
                    ])
                }},
                {race, #{
                    read_link_info => sequence([
                        fun(CurrentPath) -> file:read_link_info(CurrentPath, [raw]) end,
                        {ok, ChangedInfo}
                    ])
                }},
                {log_file_identity_unavailable, #{
                    read_link_info => sequence([
                        fun(CurrentPath) -> file:read_link_info(CurrentPath, [raw]) end,
                        {ok, InvalidInfo}
                    ])
                }},
                {unsupported_log_file_type, #{
                    read_link_info => sequence([
                        fun(CurrentPath) -> file:read_link_info(CurrentPath, [raw]) end,
                        {ok, Info#file_info{type = directory}}
                    ])
                }},
                {race, #{
                    read_link_info => sequence([
                        fun(CurrentPath) -> file:read_link_info(CurrentPath, [raw]) end,
                        {error, enoent}
                    ])
                }},
                {log_file_read_failed, #{
                    read_link_info => sequence([
                        fun(CurrentPath) -> file:read_link_info(CurrentPath, [raw]) end,
                        unexpected
                    ])
                }}
            ]
        )
    after
        file:del_dir_r(Dir)
    end.

regular_file_and_symlink_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    Link = filename:join(Dir, "app-link.log"),
    try
        ok = file:write_file(Path, <<"old\nmarker\n">>),
        ok = file:make_symlink(Path, Link),
        Source = source(Path),
        {ok, <<"old\nmarker\n">>, 11, _Coverage, _State} =
            observer_cli_log:read_configured_file(Source, file_env()),
        {error, unsupported_log_file_type, _Coverage2, _State2} =
            observer_cli_log:read_configured_file(source(Link), file_env())
    after
        file:del_dir_r(Dir)
    end.

directory_fifo_and_device_are_rejected_before_open_test() ->
    Dir = temporary_directory(),
    Fifo = filename:join(Dir, "fifo"),
    try
        ok = make_fifo(Fifo),
        lists:foreach(
            fun(Path) ->
                {error, unsupported_log_file_type, _Coverage, _State} =
                    observer_cli_log:read_configured_file(source(Path), file_env())
            end,
            [Dir, Fifo, "/dev/null"]
        )
    after
        file:del_dir_r(Dir)
    end.

fifo_replacement_returns_at_deadline_test_() ->
    {timeout, 10, fun fifo_replacement_returns_at_deadline/0}.

fifo_replacement_returns_at_deadline() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    Archive = filename:join(Dir, "app.log.before-race"),
    Handler = observer_cli_log_fifo_race,
    Parent = self(),
    try
        ok = file:write_file(Path, <<"must-not-leak">>),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [append, raw]}
        }),
        BeforeIds = logger:get_handler_ids(),
        {ok, BeforeConfig} = logger:get_handler_config(Handler),
        Env = (file_env())#{
            os_type => fun() -> {unix, element(2, os:type())} end,
            handler_ids => fun() -> [Handler] end,
            handler_config => fun logger:get_handler_config/1,
            read_link_info => fun(CurrentPath) ->
                case get(fifo_replaced) of
                    undefined ->
                        {ok, Info} = file:read_link_info(CurrentPath, [raw]),
                        ok = file:rename(CurrentPath, Archive),
                        ok = make_fifo(CurrentPath),
                        put(fifo_replaced, true),
                        Parent ! {fifo_open_waiting, self()},
                        {ok, Info};
                    true ->
                        file:read_link_info(CurrentPath, [raw])
                end
            end
        },
        Result = observer_cli_snapshot:dispatch(
            self(),
            logs,
            #{
                handler => <<"observer_cli_log_fifo_race">>,
                tail => 200,
                test_log_env => Env
            },
            #{timeout_ms => 1200, identifier_policy => include}
        ),
        Worker =
            receive
                {fifo_open_waiting, Pid} -> Pid
            after 1000 ->
                error(fifo_barrier_not_reached)
            end,
        ?assertMatch(
            #{
                <<"status">> := <<"error">>,
                <<"reason_code">> := <<"target_timeout">>,
                <<"cleanup_confirmed">> := true
            },
            Result
        ),
        ?assertNot(is_process_alive(Worker)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Result), <<"must-not-leak">>)),
        ?assertEqual(BeforeIds, logger:get_handler_ids()),
        ?assertEqual({ok, BeforeConfig}, logger:get_handler_config(Handler)),
        ok = fifo_exchange(Path)
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

handler_disappearance_during_open_is_reported_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    Handler = observer_cli_log_disappearing_handler,
    try
        ok = file:write_file(Path, <<"must-not-leak">>),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [append, raw]}
        }),
        Env = (file_env())#{
            os_type => fun() -> {unix, element(2, os:type())} end,
            handler_config => fun logger:get_handler_config/1,
            open => fun(CurrentPath) ->
                ok = logger:remove_handler(Handler),
                ok = file:delete(CurrentPath),
                file:open(CurrentPath, [read, binary, raw])
            end
        },
        {unavailable, log_source_changed, Data, _Coverage, [Effect]} =
            observer_cli_log:capture(
                #{handler => <<"observer_cli_log_disappearing_handler">>, tail => 20},
                Env
            ),
        ?assertEqual(null, maps:get(tail, Data)),
        ?assertEqual(2, maps:get(handler_config_lookups, Effect)),
        ?assertEqual(1, maps:get(read_attempts, Effect))
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

repeated_real_handler_replacement_is_rejected_test() ->
    Dir = temporary_directory(),
    Path1 = filename:join(Dir, "app-1.log"),
    Path2 = filename:join(Dir, "app-2.log"),
    Path3 = filename:join(Dir, "app-3.log"),
    Handler = observer_cli_log_replaced_handler,
    try
        lists:foreach(
            fun(Path) -> ok = file:write_file(Path, <<"must-not-leak">>) end,
            [Path1, Path2, Path3]
        ),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path1, modes => [append, raw]}
        }),
        erase(handler_config_count),
        Env = (file_env())#{
            os_type => fun() -> {unix, element(2, os:type())} end,
            handler_config => fun(Id) ->
                {ok, Config} = logger:get_handler_config(Id),
                Count = get_count(handler_config_count),
                put(handler_config_count, Count + 1),
                case Count of
                    0 -> replace_file_handler(Handler, Path2);
                    1 -> replace_file_handler(Handler, Path3);
                    _ -> ok
                end,
                {ok, Config}
            end
        },
        {unavailable, log_source_changed, Data, _Coverage, [Effect]} =
            observer_cli_log:capture(
                #{handler => <<"observer_cli_log_replaced_handler">>, tail => 20}, Env
            ),
        ?assertEqual(null, maps:get(tail, Data)),
        ?assertEqual(3, maps:get(handler_config_lookups, Effect)),
        ?assertEqual(2, maps:get(read_attempts, Effect)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Data), <<"must-not-leak">>))
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

real_logger_byte_cap_is_reported_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    Handler = observer_cli_log_byte_cap,
    try
        ok = file:write_file(Path, binary:copy(<<"x">>, 65537)),
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [append, raw]}
        }),
        {error, log_byte_cap_reached, Data, _Coverage, [_Effect]} =
            observer_cli_log:capture(
                #{handler => <<"observer_cli_log_byte_cap">>, tail => 2000}
            ),
        Tail = maps:get(tail, Data),
        ?assertEqual([byte_cap, line_cap], maps:get(truncation_reasons, Tail)),
        ?assertEqual(65536, maps:get(bytes_read, Tail))
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

real_logger_capture_does_not_change_handlers_test() ->
    Dir = temporary_directory(),
    Path = filename:join(Dir, "app.log"),
    Handler = observer_cli_log_test_file,
    try
        ok = logger:add_handler(Handler, logger_std_h, #{
            config => #{type => file, file => Path, modes => [write, raw]}
        }),
        logger:notice("observer-cli historical marker"),
        ok = logger_std_h:filesync(Handler),
        Before = logger:get_handler_ids(),
        {ok, Data, _Coverage, [_Effect]} = observer_cli_log:capture(
            #{handler => <<"observer_cli_log_test_file">>, tail => 20}
        ),
        After = logger:get_handler_ids(),
        ?assertEqual(Before, After),
        Lines = maps:get(lines, maps:get(tail, Data)),
        ?assert(
            lists:any(
                fun(Line) -> binary:match(Line, <<"historical marker">>) =/= nomatch end, Lines
            )
        )
    after
        _ = logger:remove_handler(Handler),
        file:del_dir_r(Dir)
    end.

config(Id, Path, Modes) ->
    {ok, #{
        id => Id,
        module => logger_std_h,
        config => #{type => file, file => Path, modes => Modes}
    }}.

replace_file_handler(Handler, Path) ->
    ok = logger:remove_handler(Handler),
    logger:add_handler(Handler, logger_std_h, #{
        config => #{type => file, file => Path, modes => [append, raw]}
    }).

source(Path) ->
    observer_cli_log:classify_config(default, config(default, Path, [raw])).

mock_env(Id, Binary) ->
    Path = absolute_path("observer-cli-log-test.log"),
    Info = #file_info{type = regular, size = byte_size(Binary), major_device = 1, inode = 2},
    #{
        os_type => fun() -> {unix, linux} end,
        handler_ids => fun() -> [Id] end,
        handler_config => fun(HandlerId) -> config(HandlerId, Path, [raw]) end,
        read_link_info => fun(_Path) -> {ok, Info} end,
        open => fun(_Path) -> {ok, mock_fd} end,
        read_file_info => fun(_Fd) -> {ok, Info} end,
        position => fun(_Fd) -> {ok, byte_size(Binary)} end,
        pread => fun(_Fd, Start, Length) -> {ok, binary:part(Binary, Start, Length)} end,
        close => fun(_Fd) -> ok end
    }.

file_env() ->
    #{
        read_link_info => fun(Path) -> file:read_link_info(Path, [raw]) end,
        open => fun(Path) -> file:open(Path, [read, binary, raw]) end,
        read_file_info => fun(Fd) -> file:read_file_info(Fd, [raw]) end,
        position => fun(Fd) -> file:position(Fd, eof) end,
        pread => fun file:pread/3,
        close => fun file:close/1
    }.

file_result({race, _Coverage, _State}) -> race;
file_result({error, Reason, _Coverage, _State}) -> Reason.

sequence(Results) ->
    Key = make_ref(),
    put(Key, Results),
    fun(Argument) ->
        [Result | Rest] = get(Key),
        case Rest of
            [] -> erase(Key);
            _ -> put(Key, Rest)
        end,
        case Result of
            Fun when is_function(Fun, 1) -> Fun(Argument);
            Value -> Value
        end
    end.

absolute_path(Name) ->
    filename:join([filename:absname("."), Name]).

temporary_directory() ->
    Path = filename:join(
        [
            os:getenv("TMPDIR", "/tmp"),
            "observer-cli-log-" ++ integer_to_list(erlang:unique_integer([positive]))
        ]
    ),
    ok = file:make_dir(Path),
    Path.

make_fifo(Path) ->
    case os:cmd("/usr/bin/mkfifo " ++ Path) of
        [] -> ok;
        Output -> error({mkfifo_failed, Output})
    end.

fifo_exchange(Path) ->
    Parent = self(),
    Reader = spawn_monitor(fun() ->
        {ok, Fd} = file:open(Path, [read, binary, raw]),
        Result = file:read(Fd, 1),
        ok = file:close(Fd),
        Parent ! {fifo_read, Result}
    end),
    Writer = spawn_monitor(fun() ->
        {ok, Fd} = file:open(Path, [write, binary, raw]),
        ok = file:write(Fd, <<"x">>),
        ok = file:close(Fd)
    end),
    receive
        {fifo_read, {ok, <<"x">>}} -> ok
    after 2000 ->
        error(fifo_exchange_timeout)
    end,
    wait_down(Reader),
    wait_down(Writer).

wait_down({Pid, Monitor}) ->
    receive
        {'DOWN', Monitor, process, Pid, normal} -> ok;
        {'DOWN', Monitor, process, Pid, Reason} -> error({fifo_process_failed, Reason})
    after 2000 ->
        error(fifo_process_timeout)
    end.

get_count(Key) ->
    case get(Key) of
        undefined -> 0;
        Count -> Count
    end.

contains_key(Key, Map) when is_map(Map) ->
    maps:is_key(Key, Map) orelse
        lists:any(fun(Value) -> contains_key(Key, Value) end, maps:values(Map));
contains_key(Key, List) when is_list(List) ->
    lists:any(fun(Value) -> contains_key(Key, Value) end, List);
contains_key(_Key, _Value) ->
    false.

contains_value(Expected, Expected) ->
    true;
contains_value(Expected, Map) when is_map(Map) ->
    lists:any(fun(Value) -> contains_value(Expected, Value) end, maps:values(Map));
contains_value(Expected, List) when is_list(List) ->
    lists:any(fun(Value) -> contains_value(Expected, Value) end, List);
contains_value(_Expected, _Value) ->
    false.

public_error_response({unavailable, Reason, Data, Coverage, [Effect]}) ->
    Raw = observer_cli_cli:response(
        logs,
        error,
        #{node => node(), otp_release => erlang:system_info(otp_release)},
        #{
            started_at => <<"2026-07-13T00:00:00.000Z">>,
            finished_at => <<"2026-07-13T00:00:00.000Z">>,
            duration_ms => 0,
            probes => [
                #{
                    id => log_file_tail,
                    required => true,
                    status => unavailable,
                    reason_code => Reason,
                    duration_ms => 0,
                    samples => 1,
                    coverage => Coverage
                }
            ],
            observer_effects => [
                #{
                    id => diagnostics_worker,
                    affected_facts => [process_count, port_count, memory, io, garbage_collection]
                },
                #{id => module_load, module_loaded_before_sample => true},
                Effect
            ]
        },
        Data,
        []
    ),
    {ok, Response} = observer_cli_snapshot:normalize(Raw, include),
    Response.
