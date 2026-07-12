-module(observer_cli_snapshot_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2]).

capabilities_test() ->
    ?assertEqual(#{protocol_version => 1}, observer_cli_snapshot:capabilities()).

boundary_helper_contract_test() ->
    lists:foreach(
        fun({Value, Type}) -> ?assertEqual(Type, observer_cli_snapshot:shape_type(Value)) end,
        [
            {atom, atom},
            {1, number},
            {1.5, number},
            {<<"binary">>, binary},
            {<<1:1>>, bitstring},
            {#{}, map},
            {{}, tuple},
            {[], list},
            {self(), other}
        ]
    ),
    ?assertEqual(2, observer_cli_snapshot:stacktrace_arity(2)),
    ?assertEqual(2, observer_cli_snapshot:stacktrace_arity([a, b])),
    ?assertEqual(null, observer_cli_snapshot:stacktrace_arity(invalid)),
    ?assertEqual({ok, <<"name">>}, observer_cli_snapshot:target_binary("name")),
    ?assertEqual({ok, <<"name">>}, observer_cli_snapshot:target_binary(<<"name">>)),
    ?assertEqual(error, observer_cli_snapshot:target_binary(binary:copy(<<"x">>, 256))),
    ?assertEqual(error, observer_cli_snapshot:target_binary(<<16#ff>>)),
    lists:foreach(
        fun({Value, Protocol}) ->
            ?assertEqual(Protocol, observer_cli_snapshot:inet_protocol(Value))
        end,
        [
            {"tcp_inet", tcp},
            {"udp_inet", udp},
            {"sctp_inet", sctp},
            {<<"tcp_inet">>, tcp},
            {<<"udp_inet">>, udp},
            {<<"sctp_inet">>, sctp},
            {unknown, undefined}
        ]
    ),
    ?assertEqual([a], observer_cli_snapshot:port_list_field([a])),
    ?assertEqual([], observer_cli_snapshot:port_list_field(invalid)),
    ?assertEqual(1, observer_cli_snapshot:stat_value(1)),
    ?assertEqual(null, observer_cli_snapshot:stat_value(-1)),
    ?assertEqual({ok, <<"value">>}, observer_cli_snapshot:bounded_identifier_text("value")),
    ?assertEqual(error, observer_cli_snapshot:bounded_identifier_text([16#110000])),
    ?assertEqual({identifier, pid, self()}, observer_cli_snapshot:port_identifier(self())),
    ?assertEqual(null, observer_cli_snapshot:port_identifier(invalid)),
    Reference = make_ref(),
    ?assertEqual(
        {identifier, socket, Reference},
        observer_cli_snapshot:raw_resource_identifier(Reference)
    ),
    ?assertEqual(
        {identifier, socket, {'$socket', Reference}},
        observer_cli_snapshot:raw_resource_identifier({'$socket', Reference})
    ),
    Port = open_port({spawn, "cat"}, []),
    try
        ?assertEqual(
            {identifier, port, Port}, observer_cli_snapshot:raw_resource_identifier(Port)
        )
    after
        port_close(Port)
    end,
    ?assertEqual(raw, observer_cli_snapshot:raw_resource_identifier(raw)),
    ?assertEqual({ok, 0}, observer_cli_snapshot:pointer_index(<<"0">>)),
    ?assertEqual({ok, 12}, observer_cli_snapshot:pointer_index(<<"12">>)),
    ?assertEqual(error, observer_cli_snapshot:pointer_index(<<"01">>)),
    ?assertEqual(error, observer_cli_snapshot:pointer_index(<<"bad">>)),
    ?assert(observer_cli_snapshot:json_safe(#{<<"items">> => [1, 1.5, true, false, null]})),
    ?assertNot(observer_cli_snapshot:json_safe(#{atom_key => value})),
    ?assertNot(observer_cli_snapshot:json_safe(self())).

snapshot_internal_contract_test() ->
    ?assertEqual(worker_heap_limit_exceeded, observer_cli_snapshot:deep_worker_reason(killed)),
    ?assertEqual(probe_failed, observer_cli_snapshot:deep_worker_reason(other)),
    Probe = #{status => ok, reason_code => null, samples => 1, coverage => [covered]},
    ?assertEqual(
        {ok, null, data, 1, [covered]},
        observer_cli_snapshot:composed_probe(#{capture => #{probes => [Probe]}, data => data})
    ),
    ?assertEqual({error, invalid_probe_result}, observer_cli_snapshot:composed_probe(#{})),
    lists:foreach(
        fun(Result) ->
            {Report, _Data} = observer_cli_snapshot:deep_probe_result(test_probe, Result, 10),
            ?assertEqual(test_probe, maps:get(id, Report))
        end,
        [
            {ok, null, data, 1, [covered]},
            {unavailable, refused, data, 0, []},
            {timeout, target_timeout},
            {error, probe_failed},
            invalid
        ]
    ),
    ?assertEqual(error, observer_cli_snapshot:application_name_binary(kernel)),
    ?assertEqual({ok, <<"kernel">>}, observer_cli_snapshot:application_name_binary("kernel")),
    ?assertEqual(error, observer_cli_snapshot:application_name_binary(<<16#ff>>)),
    ?assertEqual(error, observer_cli_snapshot:application_name_binary([])),
    lists:foreach(
        fun(State) -> ?assertMatch({ok, _, _}, observer_cli_snapshot:state_shape(State)) end,
        [atom, 1, 1.5, <<"binary">>, <<1:1>>, #{key => value}, {a, b}, [a, b], self()]
    ),
    Port = open_port({spawn, "cat"}, []),
    try
        Signals = observer_cli_snapshot:sanitize_signal_list([
            {process, self()},
            {port, Port},
            {process, {name, node()}},
            {port, {name, node()}},
            self(),
            Port,
            invalid
        ]),
        ?assertEqual(7, length(Signals))
    after
        port_close(Port)
    end,
    ?assertEqual([], observer_cli_snapshot:sanitize_signal_list(invalid)),
    ?assertMatch(
        [#{<<"active_suspend_count">> := 1}, #{<<"type">> := <<"other">>}],
        observer_cli_snapshot:sanitize_suspending_list([{self(), 1, 2}, invalid])
    ),
    ?assertEqual([], observer_cli_snapshot:sanitize_suspending_list(invalid)),
    ?assertEqual(
        {identifier, module, error_handler},
        observer_cli_snapshot:sanitize_error_handler(error_handler)
    ),
    ?assertEqual(null, observer_cli_snapshot:sanitize_error_handler(1)),
    ?assertMatch(
        #{arity := 2},
        observer_cli_snapshot:sanitize_stacktrace_frame({module, function, [a, b], [{file, "x"}]})
    ),
    ?assertMatch(
        #{<<"type">> := <<"other">>}, observer_cli_snapshot:sanitize_stacktrace_frame(invalid)
    ),
    ?assertEqual(
        {identifier, endpoint, <<"127.0.0.1:1883">>},
        observer_cli_snapshot:port_endpoint({ok, {{127, 0, 0, 1}, 1883}})
    ),
    ?assertEqual(null, observer_cli_snapshot:port_endpoint({ok, {invalid, 1883}})),
    ?assertEqual(
        {identifier, endpoint, <<"/tmp/socket">>},
        observer_cli_snapshot:port_endpoint({ok, {local, "/tmp/socket"}})
    ),
    ?assertEqual(null, observer_cli_snapshot:port_endpoint({ok, {local, [16#110000]}})),
    ?assertEqual(null, observer_cli_snapshot:port_endpoint(error)),
    lists:foreach(
        fun({Result, Status}) ->
            Source = #{getopts_fun => fun(_Port, _Options) -> Result end},
            ?assertEqual(
                Status, maps:get(status, observer_cli_snapshot:port_option(port, nodelay, Source))
            )
        end,
        [
            {{ok, [{nodelay, true}]}, available},
            {{ok, [{nodelay, self()}]}, error},
            {{ok, []}, unsupported},
            {{error, einval}, unsupported},
            {{error, closed}, error},
            {invalid, error}
        ]
    ),
    CrashingOption = #{getopts_fun => fun(_Port, _Options) -> erlang:error(failed) end},
    ?assertEqual(
        error, maps:get(status, observer_cli_snapshot:port_option(port, nodelay, CrashingOption))
    ),
    ?assertEqual(1, observer_cli_snapshot:safe_resource_count(#{count_fun => fun() -> 1 end})),
    ?assert(
        observer_cli_snapshot:safe_resource_count(#{count_fun => fun() -> invalid end}) > 100000
    ),
    ?assert(
        observer_cli_snapshot:safe_resource_count(#{count_fun => fun() -> error(failed) end}) >
            100000
    ),
    A = #{raw_id => 1, value => 2},
    B = #{raw_id => 2, value => 1},
    ?assert(observer_cli_snapshot:resource_precedes(A, B, value)),
    ?assertNot(observer_cli_snapshot:resource_precedes(B, A, value)),
    ?assert(observer_cli_snapshot:resource_precedes(A, B, missing)),
    ?assertEqual(2, observer_cli_snapshot:top_n_value(A, value)),
    ?assertEqual(-1, observer_cli_snapshot:top_n_value(A, missing)),
    ?assertEqual(node(), observer_cli_snapshot:controller_node(self())),
    ?assertEqual(undefined, observer_cli_snapshot:controller_node(undefined)),
    ?assertEqual(
        #{<<"items">> => [one], <<"returned_count">> => 1, <<"dropped_count">> => 3},
        observer_cli_snapshot:update_item_counts(#{
            <<"items">> => [one], <<"returned_count">> => 2, <<"dropped_count">> => 2
        })
    ),
    ?assertEqual(
        #{<<"items">> => []}, observer_cli_snapshot:update_item_counts(#{<<"items">> => []})
    ),
    ?assert(observer_cli_snapshot:pointer_protects_tail([[<<"items">>]], [<<"items">>], 1)),
    ?assert(
        observer_cli_snapshot:pointer_protects_tail([[<<"items">>, <<"1">>]], [<<"items">>], 1)
    ),
    ?assertNot(observer_cli_snapshot:pointer_protects_tail([[<<"other">>]], [<<"items">>], 1)),
    ?assertNot(observer_cli_snapshot:pointer_protects_tail([[]], [<<"items">>], 1)).

snapshot_runtime_helper_contract_test() ->
    OkReport = fun(Id) -> #{id => Id, status => ok} end,
    Probes = [
        {OkReport(memory), #{memory => #{beam => beam}, runtime => #{node => node()}}},
        {OkReport(allocator), #{util_allocators => []}},
        {OkReport(runtime), #{node => node(), otp_release => <<"test">>}}
    ],
    ?assertMatch(
        #{memory := #{allocator := #{util_allocators := []}}},
        observer_cli_snapshot:memory_command_data(Probes)
    ),
    ?assertEqual(null, observer_cli_snapshot:memory_command_data([])),
    ?assertEqual(
        #{node => node(), otp_release => <<"test">>},
        observer_cli_snapshot:memory_command_target(#{
            runtime => #{node => node(), otp_release => <<"test">>}
        })
    ),
    ?assertEqual(null, observer_cli_snapshot:memory_command_target(#{})),
    ?assertEqual(
        #{node => node(), otp_release => <<"test">>},
        observer_cli_snapshot:target_from_probes(Probes)
    ),
    ?assertEqual(null, observer_cli_snapshot:target_from_probes([])),
    ?assertEqual({1, 10}, observer_cli_snapshot:binary_ref_stats([{ref, 10, 2}, invalid])),
    ?assertEqual({0, 0}, observer_cli_snapshot:binary_ref_stats(invalid)),
    ?assertEqual(
        #{<<"line">> => 12},
        observer_cli_snapshot:sanitize_stacktrace_location([{file, "x"}, {line, 12}])
    ),
    ?assertEqual(null, observer_cli_snapshot:sanitize_stacktrace_location([{line, 0}])),
    ?assertEqual(null, observer_cli_snapshot:sanitize_stacktrace_location(invalid)),
    MnesiaSource = observer_cli_snapshot:default_mnesia_source(),
    ?assert(is_boolean((maps:get(available_fun, MnesiaSource))())),
    ?assertEqual(erlang:system_info(wordsize), (maps:get(word_size_fun, MnesiaSource))()),
    ?assertMatch({ok, [_ | _]}, observer_cli_snapshot:safe_ports()),
    ?assertMatch({ok, _}, observer_cli_snapshot:safe_sockets()),
    Port = open_port({spawn, "cat"}, []),
    try
        ?assertMatch({ok, _}, observer_cli_snapshot:safe_port_info(Port, name)),
        ?assertEqual(missing, observer_cli_snapshot:safe_port_info(Port, invalid_key)),
        controller_queue_contract(Port)
    after
        port_close(Port)
    end,
    ?assertEqual(missing, observer_cli_snapshot:safe_port_info(Port, name)),
    network_helper_contract(),
    socket_helper_contract(),
    counter_helper_contract(),
    probe_helper_contract(),
    scheduler_helper_contract(),
    distribution_helper_contract(),
    ?assertMatch({ok, _}, observer_cli_snapshot:safe_system_info(schedulers)),
    ?assertEqual(
        {unavailable, capability_unavailable},
        observer_cli_snapshot:safe_system_info(not_a_system_info_key)
    ),
    ?assertEqual(2, length(observer_cli_snapshot:observer_effects(false, undefined))),
    ?assertEqual(3, length(observer_cli_snapshot:observer_effects(true, self()))).

network_helper_contract() ->
    Info = fun
        (_Port, queue_size) -> {ok, 1};
        (_Port, memory) -> {ok, 2};
        (_Port, input) -> {ok, 3};
        (_Port, output) -> {ok, 4}
    end,
    Source = #{
        all_fun => fun() -> {ok, [tcp, skipped, gone, malformed, crashed]} end,
        monotonic_fun => fun() -> 100 end,
        name_fun => fun
            (tcp) -> {ok, "tcp_inet"};
            (skipped) -> {ok, "other"};
            (gone) -> missing;
            (malformed) -> {ok, "udp_inet"};
            (crashed) -> {ok, "tcp_inet"}
        end,
        stat_fun => fun
            (tcp) -> {ok, [{recv_oct, 10}, {send_oct, 20}]};
            (malformed) -> {ok, malformed};
            (crashed) -> erlang:error(closed)
        end,
        info_fun => Info,
        peername_fun => fun(_Port) -> {error, enotconn} end,
        io_fun => fun() -> {{input, 11}, {output, 22}} end
    },
    {ok, Items, Audit, []} = observer_cli_snapshot:network_sample(Source, #{}),
    ?assertEqual(1, map_size(Items)),
    ?assertEqual(3, maps:get(disappeared_count, Audit)),
    ?assertEqual(#{input => 11, output => 22}, maps:get(vm_io_counters, Audit)),
    ?assertEqual(
        {error, failed},
        observer_cli_snapshot:network_sample(
            Source#{all_fun := fun() -> {error, failed} end}, #{}
        )
    ),
    ?assertEqual(
        {error, invalid_enumeration_shape},
        observer_cli_snapshot:network_sample(
            Source#{all_fun := fun() -> invalid end}, #{}
        )
    ),
    ?assertEqual(1, observer_cli_snapshot:network_port_field(tcp, queue_size, Source)),
    ?assertEqual(
        null,
        observer_cli_snapshot:network_port_field(
            tcp, queue_size, Source#{info_fun := fun(_, _) -> erlang:error(closed) end}
        )
    ),
    ?assertEqual(
        {ok, #{recv_oct => 1}},
        observer_cli_snapshot:parse_network_counters([
            {recv_oct, 1}
        ])
    ),
    ?assertEqual(error, observer_cli_snapshot:parse_network_counters([{<<"bad">>, 1}])),
    ?assertEqual(error, observer_cli_snapshot:parse_network_counters(invalid)),
    ?assertEqual(
        #{input => 1, output => 2},
        observer_cli_snapshot:network_io_counters(#{
            io_fun => fun() -> {{input, 1}, {output, 2}} end
        })
    ),
    ?assertEqual(#{}, observer_cli_snapshot:network_io_counters(#{io_fun => fun() -> invalid end})),
    ?assertEqual(
        #{},
        observer_cli_snapshot:network_io_counters(#{
            io_fun => fun() -> erlang:error(failed) end
        })
    ).

socket_helper_contract() ->
    One = make_ref(),
    Missing = make_ref(),
    Crashed = make_ref(),
    Source = #{
        all_fun => fun() -> {ok, [One, Missing, Crashed]} end,
        monotonic_fun => fun() -> 200 end,
        info_fun => fun
            (Value) when Value =:= One ->
                #{
                    owner => self(),
                    domain => inet,
                    type => stream,
                    protocol => tcp,
                    rstates => [bound],
                    wstates => [connected],
                    counters => #{
                        read_byte => 1,
                        write_byte => 2,
                        read_pkg_max => 8,
                        write_pkg_max => 9,
                        acc_success => 3,
                        acc_tries => 4
                    }
                };
            (Value) when Value =:= Missing ->
                #{};
            (Value) when Value =:= Crashed ->
                erlang:error(closed)
        end,
        global_fun => fun() -> #{use_registry => true} end
    },
    {ok, Items, Audit, Coverage} = observer_cli_snapshot:socket_sample(Source),
    ?assertEqual(1, map_size(Items)),
    ?assertEqual(2, maps:get(disappeared_count, Audit)),
    ?assertEqual(true, maps:get(use_registry, Audit)),
    ?assertEqual([optional_sendfile_counter_absent], Coverage),
    Item = maps:get(One, Items),
    ?assertEqual({identifier, pid, self()}, maps:get(owner, Item)),
    ?assertEqual([bound], maps:get(rstate, Item)),
    ?assertEqual([connected], maps:get(wstate, Item)),
    ?assertEqual(
        {error, failed},
        observer_cli_snapshot:socket_sample(
            Source#{all_fun := fun() -> {error, failed} end}
        )
    ),
    ?assertEqual(
        {error, invalid_enumeration_shape},
        observer_cli_snapshot:socket_sample(
            Source#{all_fun := fun() -> invalid end}
        )
    ),
    {ok, _, CrashAudit, _} = observer_cli_snapshot:socket_sample(
        Source#{
            all_fun := fun() -> {ok, []} end,
            global_fun := fun() -> erlang:error(failed) end
        }
    ),
    ?assertEqual(unknown, maps:get(use_registry, CrashAudit)),
    ?assertEqual(
        [],
        observer_cli_snapshot:socket_optional_coverage(#{
            sendfile_byte => 0,
            sendfile_pkg => 0,
            sendfile_pkg_max => 0,
            sendfile_waits => 0,
            sendfile_fails => 0
        })
    ).

counter_helper_contract() ->
    Stats = observer_cli_snapshot:port_statistics(port, #{
        stat_fun => fun(_, _) -> {ok, [{recv_oct, 1}, {send_oct, -1}]} end
    }),
    ?assertEqual(available, maps:get(status, Stats)),
    ?assertEqual(1, maps:get(recv_oct, Stats)),
    ?assertEqual(null, maps:get(send_oct, Stats)),
    ?assertEqual(
        error,
        maps:get(
            status,
            observer_cli_snapshot:port_statistics(port, #{
                stat_fun => fun(_, _) -> erlang:error(closed) end
            })
        )
    ),
    ?assertEqual(
        {ok, #{enabled => true, seconds => 3}},
        observer_cli_snapshot:safe_port_option_value(linger, {true, 3})
    ),
    ?assertEqual(error, observer_cli_snapshot:safe_port_option_value(linger, {true, -1})),
    ?assertEqual(
        {ok, {identifier, interface, <<"en0">>}},
        observer_cli_snapshot:safe_port_option_value(bind_to_device, "en0")
    ),
    ?assertEqual(
        {ok, {identifier, netns, <<"ns">>}},
        observer_cli_snapshot:safe_port_option_value(netns, <<"ns">>)
    ),
    ?assertEqual({ok, true}, observer_cli_snapshot:safe_port_option_value(active, true)),
    ?assertEqual({ok, <<"text">>}, observer_cli_snapshot:safe_port_option_value(mode, "text")),
    ?assertEqual(error, observer_cli_snapshot:safe_port_option_value(mode, self())),
    ?assertEqual(10, observer_cli_snapshot:counter_series_delta([1, 5, 11])),
    ?assertEqual(counter_reset, observer_cli_snapshot:counter_series_delta([5, 4])),
    ?assertEqual(invalid_counter, observer_cli_snapshot:counter_series_delta([1, invalid])),
    ?assertEqual(
        #{good => 2, reset => counter_reset, invalid => invalid_counter},
        observer_cli_snapshot:counter_deltas(
            #{good => 1, reset => 3, invalid => invalid},
            #{good => 3, reset => 2, invalid => 4}
        )
    ).

probe_helper_contract() ->
    ?assertEqual(
        {ok, data, [coverage]},
        observer_cli_snapshot:call_snapshot_probe(
            fun() -> {ok, data, [coverage]} end
        )
    ),
    ?assertEqual(
        {unavailable, unavailable},
        observer_cli_snapshot:call_snapshot_probe(
            fun() -> {unavailable, unavailable} end
        )
    ),
    ?assertEqual(
        {error, invalid_probe_result},
        observer_cli_snapshot:call_snapshot_probe(
            fun() -> invalid end
        )
    ),
    ?assertEqual(
        {unavailable, capability_unavailable},
        observer_cli_snapshot:call_snapshot_probe(
            fun() -> erlang:error(badarg) end
        )
    ),
    ?assertEqual(
        {error, probe_failed},
        observer_cli_snapshot:call_snapshot_probe(
            fun() -> erlang:error(other) end
        )
    ),
    lists:foreach(
        fun(Outcome) ->
            {Report, _} = observer_cli_snapshot:probe_result(probe, true, Outcome, 9),
            ?assertEqual(probe, maps:get(id, Report)),
            ?assertEqual(9, maps:get(duration_ms, Report))
        end,
        [
            {ok, data, [coverage]},
            {unavailable, unavailable},
            {timeout, timed_out},
            {error, failed},
            invalid
        ]
    ).

scheduler_helper_contract() ->
    ?assertEqual({error, invalid_counter_shape}, observer_cli_snapshot:wall_map(invalid)),
    ?assertEqual(
        {error, duplicate_scheduler_id},
        observer_cli_snapshot:wall_map([
            {1, 1, 2}, {1, 2, 3}
        ])
    ),
    ?assertEqual({error, invalid_counter_shape}, observer_cli_snapshot:wall_map([invalid])),
    ?assertEqual({ok, #{1 => {1, 2}}}, observer_cli_snapshot:wall_map([{1, 1, 2}])),
    ?assertEqual(
        {error, invalid_counter_shape},
        observer_cli_snapshot:with_wall_maps(
            #{wall_time => invalid}, #{wall_time => []}, fun(_, _) -> ok end
        )
    ),
    ?assertEqual(
        {error, duplicate_scheduler_id},
        observer_cli_snapshot:with_wall_maps(
            #{wall_time => []}, #{wall_time => [{1, 1, 2}, {1, 2, 3}]}, fun(_, _) -> ok end
        )
    ),
    ?assertEqual(
        ok,
        observer_cli_snapshot:with_wall_maps(
            #{wall_time => []}, #{wall_time => []}, fun(_, _) -> ok end
        )
    ),
    ?assertMatch({ok, #{status := unavailable}}, observer_cli_snapshot:pool_delta([], #{}, #{})),
    ?assertMatch(
        {ok, #{utilization_ratio := 0.5}},
        observer_cli_snapshot:pool_delta(
            [1], #{1 => {1, 2}}, #{1 => {2, 4}}
        )
    ),
    ?assertEqual(
        {error, zero_denominator},
        observer_cli_snapshot:pool_delta(
            [1], #{1 => {1, 2}}, #{1 => {1, 2}}
        )
    ),
    ?assertEqual(
        {error, counter_reset},
        observer_cli_snapshot:pool_delta(
            [1], #{1 => {2, 3}}, #{1 => {1, 4}}
        )
    ),
    ?assertEqual({error, missing_scheduler_id}, observer_cli_snapshot:pool_delta([1], #{}, #{})),
    ?assertEqual({ok, 3, 3}, observer_cli_snapshot:run_queue_sample(2, [1, 2, 3])),
    ?assertEqual(error, observer_cli_snapshot:run_queue_sample(2, [1, invalid, 3])),
    ?assertEqual(error, observer_cli_snapshot:run_queue_sample(2, [1, 2])),
    ?assertMatch(
        {ok, _},
        observer_cli_snapshot:run_queue_window(
            1, #{run_queue_lengths => [1, 2]}, #{run_queue_lengths => [2, 3]}
        )
    ),
    ?assertEqual(
        {error, invalid_run_queue_shape},
        observer_cli_snapshot:run_queue_window(
            1, #{run_queue_lengths => invalid}, #{run_queue_lengths => [2, 3]}
        )
    ),
    Topology = #{
        schedulers_configured => 1,
        schedulers_online => 1,
        dirty_cpu_schedulers_online => 1
    },
    First = #{
        wall_time => [{1, 1, 2}, {2, 1, 2}],
        run_queue_lengths => [0, 0],
        monotonic_ms => 10
    },
    Second = #{
        wall_time => [{1, 2, 4}, {2, 2, 4}],
        run_queue_lengths => [0, 0],
        monotonic_ms => 20
    },
    ?assertMatch(
        {ok, #{interval_ms := 10}},
        observer_cli_snapshot:scheduler_window_data(Topology, First, Second)
    ),
    ?assertEqual(
        {error, invalid_interval},
        observer_cli_snapshot:scheduler_window_data(
            Topology, First, Second#{monotonic_ms := 5}
        )
    ).

controller_queue_contract(Port) ->
    Peer = 'peer@example',
    ?assertMatch(
        #{status := available},
        observer_cli_snapshot:controller_queue(
            Peer, [{Peer, Port}], 1024, fun(_) -> {queue_size, 3} end
        )
    ),
    ?assertMatch(
        #{status := unavailable},
        observer_cli_snapshot:controller_queue(
            Peer, [{Peer, Port}], 1024, fun(_) -> invalid end
        )
    ),
    ?assertMatch(
        #{status := unavailable},
        observer_cli_snapshot:controller_queue(
            Peer, [{Peer, Port}], 1024, fun(_) -> erlang:error(closed) end
        )
    ),
    ?assertMatch(
        #{status := unavailable},
        observer_cli_snapshot:controller_queue(
            Peer, [], 1024, fun(_) -> {queue_size, 0} end
        )
    ).

distribution_helper_contract() ->
    Peer1 = {identifier, peer, one},
    Peer2 = {identifier, peer, two},
    Data = #{
        connected_peers => [Peer1, Peer2],
        visible_peers => [Peer1],
        hidden_peers => [Peer2],
        controller_queues => [#{peer => Peer1}, #{peer => Peer2}]
    },
    ?assertEqual(
        false, maps:get(truncated, observer_cli_snapshot:limit_distribution(Data, infinity))
    ),
    Limited = observer_cli_snapshot:limit_distribution(Data, 1),
    ?assertEqual([Peer1], maps:get(connected_peers, Limited)),
    ?assertEqual([], maps:get(hidden_peers, Limited)),
    ?assertEqual(1, maps:get(returned_peer_count, Limited)),
    ?assertEqual(true, maps:get(truncated, Limited)).

snapshot_internal_boundary_matrix_test() ->
    ?assertMatch(
        {ok, #{specs := 1}},
        observer_cli_snapshot:supervisor_counts([
            {specs, 1}, {active, 1}, {supervisors, 0}, {workers, 1}
        ])
    ),
    ?assertEqual(error, observer_cli_snapshot:supervisor_counts([{specs, -1}])),
    ?assertEqual(error, observer_cli_snapshot:supervisor_counts(invalid)),
    ?assert(
        observer_cli_snapshot:valid_supervisor_children([
            {child, self(), worker, [?MODULE]},
            {restarting, restarting, supervisor, [?MODULE]},
            {undefined, undefined, worker, [?MODULE]}
        ])
    ),
    ?assertNot(observer_cli_snapshot:valid_supervisor_children([invalid])),
    AliveSource = #{alive_fun => fun erlang:is_process_alive/1},
    ?assertMatch(#{state := alive}, observer_cli_snapshot:child_pid_item(self(), AliveSource)),
    ?assertMatch(#{state := restarting}, observer_cli_snapshot:child_pid_item(restarting, #{})),
    ?assertMatch(#{state := undefined}, observer_cli_snapshot:child_pid_item(undefined, #{})),
    ?assertEqual(
        {ok, kernel},
        observer_cli_snapshot:resolve_loaded_application(
            <<"kernel">>, [{kernel, "Kernel", "1"}]
        )
    ),
    ?assertEqual(
        not_found,
        observer_cli_snapshot:resolve_loaded_application(
            <<"missing">>, [{kernel, "Kernel", "1"}]
        )
    ),
    ?assertEqual(not_found, observer_cli_snapshot:resolve_loaded_application(<<>>, [])),
    {Capped, _} = observer_cli_snapshot:shape_term(value, 0, #{nodes => 10000}),
    ?assertEqual(truncated, maps:get(type, Capped)),
    {Depth, _} = observer_cli_snapshot:shape_term(value, 6, #{nodes => 0}),
    ?assertEqual(depth_cap, maps:get(truncation_reason, Depth)),
    ?assertMatch(
        {[], #{nodes := 10000}},
        observer_cli_snapshot:shape_children(
            [one], 0, #{nodes => 10000}, []
        )
    ),
    ?assertMatch(
        {[], null, false, _},
        observer_cli_snapshot:shape_list(
            [one], 0, #{nodes => 10000}, [], 0
        )
    ),
    ?assertMatch(
        {_, null, false, _},
        observer_cli_snapshot:shape_list(
            [one | improper], 0, #{nodes => 0}, [], 0
        )
    ),
    ProcessSource = #{
        alive_fun => fun erlang:is_process_alive/1,
        whereis_fun => fun erlang:whereis/1
    },
    ?assertEqual(
        {ok, self()},
        observer_cli_snapshot:resolve_pid_text(
            list_to_binary(pid_to_list(self())), ProcessSource
        )
    ),
    ?assertEqual(not_found, observer_cli_snapshot:resolve_pid_text(<<"bad">>, ProcessSource)),
    Name = observer_cli_snapshot_test_registered,
    true = register(Name, self()),
    try
        ?assertEqual(
            {ok, self()},
            observer_cli_snapshot:resolve_registered_name(
                atom_to_binary(Name), ProcessSource
            )
        )
    after
        unregister(Name)
    end,
    ?assertEqual(
        not_found,
        observer_cli_snapshot:resolve_registered_name(
            <<"observer_cli_snapshot_missing_name">>, ProcessSource
        )
    ),
    AppSource = #{
        supervisor_fun => fun
            (kernel) -> {ok, self()};
            (_) -> undefined
        end,
        root_info_fun => fun(_, group_leader) -> {group_leader, self()} end
    },
    ?assertEqual(
        #{self() => kernel},
        observer_cli_snapshot:application_leaders(
            [kernel, missing], AppSource
        )
    ),
    MnesiaSource = #{
        info_fun => fun
            (_, size) -> 1;
            (_, _) -> erlang:error(failed)
        end,
        whereis_fun => fun(_) -> make_ref() end,
        ets_info_fun => fun(Tid, id) -> Tid end
    },
    ?assertEqual(1, observer_cli_snapshot:mnesia_info(MnesiaSource, table, size)),
    ?assertEqual(undefined, observer_cli_snapshot:mnesia_info(MnesiaSource, table, memory)),
    TableId = make_ref(),
    CorrSource = MnesiaSource#{
        whereis_fun := fun(_) -> TableId end,
        ets_info_fun := fun(_, _) -> TableId end
    },
    ?assertEqual(
        {mnesia_main_table, TableId},
        observer_cli_snapshot:mnesia_correlation(
            table, ram_copies, CorrSource
        )
    ),
    ?assertEqual(
        {management_unknown, table},
        observer_cli_snapshot:mnesia_correlation(
            table, disc_only_copies, CorrSource
        )
    ),
    ?assertMatch({_, _}, observer_cli_snapshot:process_fold()),
    ?assert(
        observer_cli_snapshot:list_process_fold(
            fun(Pid, Found) -> Found orelse Pid =:= self() end, false
        )
    ),
    ?assert(observer_cli_snapshot:socket_available()),
    resource_boundary_matrix(),
    normalization_boundary_matrix().

resource_boundary_matrix() ->
    FirstAudit = #{vm_io_counters => #{input => 1, output => 2}, sample_monotonic_ms => 1},
    SecondAudit = #{
        vm_io_counters => #{input => 4, output => 6},
        sample_monotonic_ms => 2,
        scanned_count => 1
    },
    ?assertMatch(
        #{vm_port_driver_io := #{<<"io_bytes_delta">> := 7}},
        observer_cli_snapshot:delta_resource_audit(network, FirstAudit, SecondAudit)
    ),
    ?assertMatch(
        #{vm_port_driver_io := #{status := shape_change}},
        observer_cli_snapshot:delta_resource_audit(
            network,
            FirstAudit,
            SecondAudit#{vm_io_counters := #{input => 4}}
        )
    ),
    ?assertEqual(
        #{scanned_count => 1},
        observer_cli_snapshot:delta_resource_audit(
            sockets, FirstAudit, #{scanned_count => 1, sample_monotonic_ms => 2}
        )
    ),
    ?assertMatch(
        #{<<"io_bytes_total">> := 3},
        observer_cli_snapshot:vm_io_metrics(
            #{input => 1, output => 2}, total
        )
    ),
    ?assertMatch(
        #{status := missing_core},
        observer_cli_snapshot:vm_io_metrics(
            #{input => invalid, output => 2}, delta
        )
    ),
    ?assertEqual(#{}, observer_cli_snapshot:observer_port_exclusions(#{})),
    Port = open_port({spawn, "cat"}, []),
    try
        ?assertMatch(
            #{resource := {identifier, port, Port}},
            observer_cli_snapshot:port_exclusion(Port, diagnostics_controller)
        ),
        ?assertEqual(
            {ok, Port},
            observer_cli_snapshot:resolve_port_target(
                list_to_binary(port_to_list(Port))
            )
        )
    after
        port_close(Port)
    end,
    ?assertEqual(not_found, observer_cli_snapshot:resolve_port_target(<<"bad">>)),
    Info = fun
        (_, name) ->
            {ok, "efile"};
        (_, connected) ->
            {ok, self()};
        (_, Key) when
            Key =:= queue_size;
            Key =:= memory;
            Key =:= id;
            Key =:= input;
            Key =:= output
        ->
            {ok, 1};
        (_, _) ->
            missing
    end,
    Item = observer_cli_snapshot:port_resource(dummy, #{info_fun => Info}),
    ?assertEqual(2, maps:get(io, Item)),
    ?assertEqual(
        skip,
        observer_cli_snapshot:port_resource(dummy, #{
            info_fun => fun(_, name) -> {ok, "tcp_inet"} end
        })
    ),
    ?assertEqual(
        disappeared,
        observer_cli_snapshot:port_resource(dummy, #{
            info_fun => fun(_, _) -> missing end
        })
    ),
    ?assertEqual(
        ok,
        observer_cli_snapshot:call_port_fun(fun_key, dummy, #{
            fun_key => fun(_) -> ok end
        })
    ),
    ?assertEqual(
        {error, failed},
        observer_cli_snapshot:call_port_fun(fun_key, dummy, #{
            fun_key => fun(_) -> erlang:error(failed) end
        })
    ),
    ?assertEqual(
        #{status => unavailable, items => []},
        observer_cli_snapshot:socket_series_trend([])
    ).

normalization_boundary_matrix() ->
    State = #{ids => #{}, counts => #{}},
    ?assertMatch(
        {ok, <<"atom">>, _}, observer_cli_snapshot:normalize_value(atom, include, 0, State)
    ),
    ?assertMatch({ok, _, _}, observer_cli_snapshot:normalize_value(self(), redact, 0, State)),
    ?assertEqual(
        {error, invalid_schema},
        observer_cli_snapshot:normalize_value(
            fun() -> ok end, include, 0, State
        )
    ),
    ?assertEqual(
        {error, invalid_schema},
        observer_cli_snapshot:normalize_map(
            [{<<"key">>, 1}, {<<"key">>, 2}], include, 0, State, #{}
        )
    ),
    ?assertEqual({ok, <<"key">>}, observer_cli_snapshot:normalize_key(key)),
    ?assertEqual(error, observer_cli_snapshot:normalize_key(1)),
    ?assertEqual({ok, <<"module">>}, observer_cli_snapshot:identifier_text(module)),
    ?assertEqual(error, observer_cli_snapshot:identifier_text(self())),
    ?assertEqual(none, observer_cli_snapshot:trim_map_values([], #{}, [], [])),
    ?assertEqual(none, observer_cli_snapshot:trim_list_values([], [], [], [], 0)),
    Nested = #{<<"items">> => [one]},
    ?assertMatch(
        {ok, _},
        observer_cli_snapshot:trim_map_values(
            [{<<"nested">>, Nested}], #{<<"nested">> => Nested}, [], []
        )
    ),
    ?assertMatch({ok, _}, observer_cli_snapshot:trim_list_values([Nested], [], [], [], 0)),
    ?assertEqual({ok, []}, observer_cli_snapshot:evidence_pointers_values([], [])),
    ?assertEqual(
        error,
        observer_cli_snapshot:evidence_pointers_values(
            [
                #{<<"evidence">> => invalid}
            ],
            []
        )
    ),
    ?assertEqual({ok, []}, observer_cli_snapshot:parse_pointer(<<>>)),
    ?assertEqual(
        {ok, [<<"a/b">>, <<"~c">>]},
        observer_cli_snapshot:parse_pointer(
            <<"/a~1b/~0c">>
        )
    ),
    ?assertEqual(error, observer_cli_snapshot:parse_pointer(<<"bad">>)),
    ?assertEqual(error, observer_cli_snapshot:parse_pointer(<<"/~2">>)).

snapshot_worker_and_diagnostic_boundary_test() ->
    ?assertEqual(
        {ok, include, 10000, 8 * 1024 * 1024},
        observer_cli_snapshot:dispatch_options(#{})
    ),
    ?assertEqual(error, observer_cli_snapshot:dispatch_options(#{timeout_ms => 0})),
    ?assertEqual(error, observer_cli_snapshot:dispatch_options(invalid)),
    ?assertMatch(
        #{<<"reason_code">> := <<"worker_heap_limit_exceeded">>},
        observer_cli_snapshot:worker_down(make_ref(), killed)
    ),
    ?assertMatch(
        #{<<"reason_code">> := <<"probe_failed">>},
        observer_cli_snapshot:worker_down(make_ref(), failed)
    ),
    Worker = spawn(fun() -> receive
        after infinity -> ok
        end end),
    WorkerRef = erlang:monitor(process, Worker),
    ?assertMatch(
        #{<<"reason_code">> := <<"test_reason">>},
        observer_cli_snapshot:stop_worker(Worker, WorkerRef, test_reason)
    ),
    ?assertEqual(
        #{status => unavailable, reason_code => observation_not_requested},
        observer_cli_snapshot:diagnostic_memory(#{})
    ),
    ?assertEqual(
        #{status => unavailable, reason_code => observation_not_requested},
        observer_cli_snapshot:diagnostic_ets(#{})
    ),
    Ets = observer_cli_snapshot:diagnostic_ets(#{
        sample_index => 0,
        test_ets_source => #{
            count_fun => fun() -> 1 end,
            all_fun => fun() -> [table] end,
            info_fun => fun(_, _) -> erlang:error(disappeared) end
        }
    }),
    ?assertEqual(#{}, maps:get(values, Ets)),
    PortError = observer_cli_snapshot:diagnostic_ports(
        #{
            sample_index => 0,
            test_port_source => #{
                count_fun => fun() -> 1 end,
                all_fun => fun() -> invalid end
            }
        },
        #{}
    ),
    ?assertEqual(port_inventory_failed, maps:get(reason_code, PortError)),
    ?assertEqual(
        #{status => unavailable, reason_code => observation_not_requested},
        observer_cli_snapshot:diagnostic_ports(#{}, #{})
    ),
    SocketBudget = observer_cli_snapshot:diagnostic_sockets(#{
        observe => true,
        test_socket_source => #{
            available_fun => fun() -> true end,
            count_fun => fun() -> 100001 end
        }
    }),
    ?assertEqual(scan_budget_exceeded, maps:get(reason_code, SocketBudget)),
    ?assertEqual(
        #{status => unavailable, reason_code => observation_not_requested},
        observer_cli_snapshot:diagnostic_sockets(#{})
    ),
    ?assertEqual(
        #{queue_size => 1},
        observer_cli_snapshot:diagnostic_port(port, #{
            info_fun => fun
                (_, queue_size) -> {ok, 1};
                (_, _) -> missing
            end
        })
    ),
    ?assertEqual(
        #{status => unavailable, reason_code => application_not_requested},
        observer_cli_snapshot:diagnostic_application(#{})
    ),
    Normal = spawn(fun() ->
        receive
            go -> ok
        end
    end),
    NormalMon = erlang:monitor(process, Normal),
    Normal ! go,
    ?assertEqual(
        outcome,
        observer_cli_snapshot:finish_deep_probe(
            Normal, NormalMon, outcome, erlang:monotonic_time(millisecond) + 1000
        )
    ),
    Abnormal = spawn(fun() ->
        receive
            go -> exit(failed)
        end
    end),
    AbnormalMon = erlang:monitor(process, Abnormal),
    Abnormal ! go,
    ?assertEqual(
        {error, probe_failed},
        observer_cli_snapshot:finish_deep_probe(
            Abnormal, AbnormalMon, outcome, erlang:monotonic_time(millisecond) + 1000
        )
    ),
    Stuck = spawn(fun() -> receive
        after infinity -> ok
        end end),
    StuckMon = erlang:monitor(process, Stuck),
    ?assertEqual(
        {timeout, cleanup_unconfirmed},
        observer_cli_snapshot:finish_deep_probe(
            Stuck, StuckMon, outcome, erlang:monotonic_time(millisecond)
        )
    ),
    snapshot_coordinate_contract().

snapshot_coordinate_contract() ->
    ControllerRef = erlang:monitor(process, self()),
    ErrorWorker = spawn(fun() -> receive
        after infinity -> ok
        end end),
    ErrorMon = erlang:monitor(process, ErrorWorker),
    ErrorRun = make_ref(),
    self() ! {ErrorRun, ErrorWorker, {error, test_error}},
    ?assertMatch(
        #{<<"reason_code">> := <<"test_error">>},
        observer_cli_snapshot:coordinate(
            ControllerRef,
            ErrorWorker,
            ErrorMon,
            ErrorRun,
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    InvalidWorker = spawn(fun() -> receive
        after infinity -> ok
        end end),
    InvalidMon = erlang:monitor(process, InvalidWorker),
    InvalidRun = make_ref(),
    self() ! {InvalidRun, InvalidWorker, {ok, self()}},
    ?assertMatch(
        #{<<"reason_code">> := <<"invalid_schema">>},
        observer_cli_snapshot:coordinate(
            ControllerRef,
            InvalidWorker,
            InvalidMon,
            InvalidRun,
            erlang:monotonic_time(millisecond) + 1000
        )
    ),
    Normal = spawn(fun() ->
        receive
            {_Ref, finish} -> ok
        end
    end),
    NormalMon = erlang:monitor(process, Normal),
    NormalRun = make_ref(),
    ?assertMatch(
        #{<<"status">> := <<"ok">>},
        observer_cli_snapshot:finish_worker(
            ControllerRef,
            Normal,
            NormalMon,
            NormalRun,
            erlang:monotonic_time(millisecond) + 1000,
            #{<<"ok">> => true}
        )
    ),
    Abnormal = spawn(fun() ->
        receive
            {_Ref, finish} -> exit(failed)
        end
    end),
    AbnormalMon = erlang:monitor(process, Abnormal),
    AbnormalRun = make_ref(),
    ?assertMatch(
        #{<<"reason_code">> := <<"cleanup_unconfirmed">>},
        observer_cli_snapshot:finish_worker(
            ControllerRef,
            Abnormal,
            AbnormalMon,
            AbnormalRun,
            erlang:monotonic_time(millisecond) + 1000,
            #{}
        )
    ),
    Timed = spawn(fun() ->
        receive
            _ -> receive
                after infinity -> ok
                end
        end
    end),
    TimedMon = erlang:monitor(process, Timed),
    ?assertMatch(
        #{<<"reason_code">> := <<"cleanup_unconfirmed">>},
        observer_cli_snapshot:finish_worker(
            ControllerRef,
            Timed,
            TimedMon,
            make_ref(),
            erlang:monotonic_time(millisecond),
            #{}
        )
    ),
    erlang:demonitor(ControllerRef, [flush]).

snapshot_capture_boundary_contract_test() ->
    ?assertEqual({probe_error, invalid_request}, observer_cli_snapshot:capture_trace(#{}, #{})),
    NullTrace = observer_cli_snapshot:trace_response(
        trace_call,
        fun() ->
            #{
                status => error,
                category => argument,
                reason => invalid,
                capture => null,
                warnings => []
            }
        end,
        #{controller => self()}
    ),
    ?assertEqual(null, maps:get(capture, NullTrace)),
    CompleteTrace = observer_cli_snapshot:trace_response(
        trace_call,
        fun() ->
            #{
                status => ok,
                category => success,
                reason => complete,
                capture => #{status => complete},
                warnings => []
            }
        end,
        #{controller => self()}
    ),
    ?assertEqual(complete, maps:get(status, maps:get(capture, CompleteTrace))),
    PartialTrace = observer_cli_snapshot:trace_response(
        trace_call,
        fun() ->
            #{
                status => error,
                category => cleanup,
                reason => failed,
                capture => #{status => partial},
                warnings => []
            }
        end,
        #{controller => self()}
    ),
    ?assertEqual(partial, maps:get(status, maps:get(capture, PartialTrace))),
    RootSource = #{count_children_fun => fun(_) -> invalid end},
    ?assertMatch(
        {error, supervisor_count_failed, _},
        observer_cli_snapshot:collect_root_children(app, self(), RootSource, #{})
    ),
    ?assertMatch(
        {error, supervisor_count_failed, _},
        observer_cli_snapshot:collect_root_children(
            app,
            self(),
            #{count_children_fun => fun(_) -> erlang:error(failed) end},
            #{}
        )
    ),
    Counts = #{specs => 1, active => 1, supervisors => 0, workers => 1},
    ?assertMatch(
        {error, supervisor_children_failed, _},
        observer_cli_snapshot:collect_admitted_root_children(
            app, self(), #{which_children_fun => fun(_) -> [invalid] end}, #{}, Counts
        )
    ),
    ?assertMatch(
        {error, supervisor_children_failed, _},
        observer_cli_snapshot:collect_admitted_root_children(
            app, self(), #{which_children_fun => fun(_) -> invalid end}, #{}, Counts
        )
    ),
    ?assertMatch(
        {error, supervisor_children_failed, _},
        observer_cli_snapshot:collect_admitted_root_children(
            app, self(), #{which_children_fun => fun(_) -> erlang:error(failed) end}, #{}, Counts
        )
    ),
    ProcessSource = #{
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    },
    Target = list_to_binary(pid_to_list(self())),
    ?assertMatch(
        {error, state_timeout, _},
        observer_cli_snapshot:collect_gen_server_state(
            Target, #{
                process_source => ProcessSource,
                get_state_fun => fun(_, _) -> exit({timeout, state}) end
            }
        )
    ),
    ?assertMatch(
        {error, state_probe_failed, _},
        observer_cli_snapshot:collect_gen_server_state(
            Target, #{
                process_source => ProcessSource,
                get_state_fun => fun(_, _) -> erlang:error(failed) end
            }
        )
    ),
    lists:foreach(
        fun(Fun) -> ?assertEqual({probe_error, invalid_request}, Fun()) end,
        [
            fun() -> observer_cli_snapshot:capture_applications(invalid, #{}) end,
            fun() -> observer_cli_snapshot:capture_ets(invalid, #{}) end,
            fun() -> observer_cli_snapshot:capture_mnesia(invalid, #{}) end,
            fun() -> observer_cli_snapshot:capture_ports(invalid, #{}) end
        ]
    ),
    ?assertEqual(
        {probe_error, invalid_request},
        observer_cli_snapshot:capture_counter_resources(
            network, probe, #{sort => invalid}, #{}, #{}
        )
    ),
    {PastReport, undefined} = observer_cli_snapshot:run_snapshot_probe(
        probe, true, fun() -> {ok, data, []} end, #{}, erlang:monotonic_time(millisecond)
    ),
    ?assertEqual(timeout, maps:get(status, PastReport)),
    {InjectedReport, data} = observer_cli_snapshot:run_snapshot_probe(
        probe,
        true,
        fun() -> error(unexpected) end,
        #{test_probe_outcomes => #{probe => {ok, data, []}}},
        erlang:monotonic_time(millisecond) + 1000
    ),
    ?assertEqual(ok, maps:get(status, InjectedReport)).

snapshot_remaining_boundary_contract_test() ->
    lists:foreach(
        fun(Fun) -> ?assertEqual({probe_error, invalid_request}, Fun()) end,
        [
            fun() -> observer_cli_snapshot:capture_applications(#{sort => invalid}, #{}) end,
            fun() -> observer_cli_snapshot:capture_ets(#{sort => invalid}, #{}) end,
            fun() -> observer_cli_snapshot:capture_mnesia(#{sort => invalid}, #{}) end,
            fun() -> observer_cli_snapshot:capture_ports(#{sort => invalid}, #{}) end
        ]
    ),
    ?assertEqual(
        error,
        observer_cli_snapshot:application_name_binary(
            lists:duplicate(256, $x)
        )
    ),
    ?assertEqual(
        error,
        observer_cli_snapshot:application_name_binary(
            binary:copy(<<"x">>, 256)
        )
    ),
    Tid = make_ref(),
    ?assertEqual(
        {management_unknown, table},
        observer_cli_snapshot:mnesia_correlation(
            table,
            ram_copies,
            #{whereis_fun => fun(_) -> Tid end, ets_info_fun => fun(_, _) -> other end}
        )
    ),
    Source = observer_cli_snapshot:default_mnesia_source(),
    ok = ignore_snapshot_fun(maps:get(running_fun, Source)),
    ok = ignore_snapshot_fun(maps:get(local_tables_fun, Source)),
    ok = ignore_snapshot_fun(fun() -> (maps:get(whereis_fun, Source))(missing_table) end),
    ok = ignore_snapshot_fun(fun() ->
        (maps:get(ets_info_fun, Source))(missing_table, size)
    end),
    ?assertEqual(
        error,
        observer_cli_snapshot:safe_port_option_value(
            bind_to_device, [16#110000]
        )
    ),
    ?assertEqual(
        error,
        observer_cli_snapshot:safe_port_option_value(
            option, [16#110000]
        )
    ),
    SameA = #{raw_id => a, value => 1},
    SameB = #{raw_id => b, value => 1},
    ?assert(observer_cli_snapshot:resource_precedes(SameA, SameB, value)),
    ?assertNot(observer_cli_snapshot:resource_precedes(SameB, SameA, value)),
    ?assertNot(observer_cli_snapshot:resource_precedes(SameA, SameA, value)),
    ?assertEqual(
        [],
        maps:get(
            connected_peers,
            observer_cli_snapshot:limit_distribution(
                #{
                    connected_peers => [{identifier, peer, one}],
                    visible_peers => [],
                    hidden_peers => [],
                    controller_queues => []
                },
                0
            )
        )
    ),
    NormalizeState = #{ids => #{}, counts => #{}},
    ?assertEqual(
        {error, invalid_schema},
        observer_cli_snapshot:normalize_value(
            <<1:1>>, include, 0, NormalizeState
        )
    ),
    ?assertMatch(
        {ok, #{<<"arity">> := 0}, _},
        observer_cli_snapshot:normalize_value(
            {mfa, erlang, node, 0}, redact, 0, NormalizeState
        )
    ),
    ?assertEqual(
        {error, invalid_identifier},
        observer_cli_snapshot:normalize_value(
            {identifier, invalid, value}, include, 0, NormalizeState
        )
    ),
    ?assertEqual(error, observer_cli_snapshot:normalize_key(<<16#ff>>)),
    ?assertEqual(error, observer_cli_snapshot:identifier_text(<<16#ff>>)),
    ?assertEqual(error, observer_cli_snapshot:pointer_index(<<"1x">>)).

ignore_snapshot_fun(Fun) ->
    try Fun() of
        _ -> ok
    catch
        _:_ -> ok
    end.

snapshot_inventory_collector_contract_test() ->
    BaseAcc = #{
        scanned => 0,
        eligible => 0,
        disappeared => 0,
        exclusions => [],
        excluded_pids => #{},
        limit => 1,
        top => []
    },
    ?assertMatch(
        {skip, #{disappeared := 1}},
        observer_cli_snapshot:scan_process(
            self(), [memory], #{info_fun => fun(_, _) -> undefined end}, BaseAcc
        )
    ),
    ?assertMatch(
        {skip, #{exclusions := [_]}},
        observer_cli_snapshot:scan_process(
            self(), [memory], #{}, BaseAcc#{excluded_pids := #{self() => controller}}
        )
    ),
    ?assertMatch(
        {ok, #{memory := 10}, _},
        observer_cli_snapshot:scan_process(
            self(), [memory], #{info_fun => fun(_, _) -> [{memory, 10}] end}, BaseAcc
        )
    ),
    SamplePid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Fold = fun(Fun, Acc) -> Fun(SamplePid, Acc) end,
    Sample = observer_cli_snapshot:collect_process_sample(
        memory,
        #{
            fold => {test, Fold},
            info_fun => fun(_, _) -> [{memory, 10}] end,
            monotonic_fun => fun() -> 1 end
        },
        #{}
    ),
    ?assertEqual(10, maps:get(SamplePid, maps:get(values, Sample))),
    SkippedSample = observer_cli_snapshot:collect_process_sample(
        memory,
        #{
            fold => {test, Fold},
            info_fun => fun(_, _) -> undefined end,
            monotonic_fun => fun() -> 1 end
        },
        #{}
    ),
    ?assertEqual(#{}, maps:get(values, SkippedSample)),
    InvalidSample = observer_cli_snapshot:collect_process_sample(
        memory,
        #{
            fold => {test, Fold},
            info_fun => fun(_, _) -> [{memory, invalid}] end,
            monotonic_fun => fun() -> 1 end
        },
        #{}
    ),
    ?assertEqual(#{}, maps:get(values, InvalidSample)),
    SamplePid ! stop,
    ProcessSource = #{
        whereis_fun => fun(_) -> undefined end,
        alive_fun => fun(_) -> true end,
        info_fun => fun(_, _) -> undefined end
    },
    ?assertMatch(
        {ok, #{status := not_found}, _},
        observer_cli_snapshot:collect_process(
            <<"missing_name">>, ProcessSource
        )
    ),
    ?assertMatch(
        {ok, #{status := not_found}, _},
        observer_cli_snapshot:collect_process(
            list_to_binary(pid_to_list(self())), ProcessSource
        )
    ),
    ?assertMatch(
        {ok, #{status := running}, _},
        observer_cli_snapshot:collect_process(
            list_to_binary(pid_to_list(self())), ProcessSource#{
                info_fun := fun(_, _) -> [{status, running}, {group_leader, self()}] end
            }
        )
    ),
    ?assertMatch(
        {ok, #{status := not_running}, _},
        observer_cli_snapshot:collect_available_mnesia(
            #{running_fun => fun() -> no end}, memory, 20, #{}
        )
    ),
    ?assertMatch(
        {unavailable, capability_unavailable, _},
        observer_cli_snapshot:collect_available_mnesia(
            #{running_fun => fun() -> invalid end}, memory, 20, #{}
        )
    ),
    HugeTables = lists:seq(1, 10001),
    ?assertMatch(
        {unavailable, scan_budget_exceeded, _},
        observer_cli_snapshot:collect_available_mnesia(
            #{running_fun => fun() -> yes end, local_tables_fun => fun() -> HugeTables end},
            memory,
            20,
            #{}
        )
    ),
    MnesiaAcc = #{scanned => 0, disappeared => 0, eligible => 0, top => []},
    ?assertMatch(
        #{disappeared := 1},
        observer_cli_snapshot:scan_mnesia_table(
            table, #{info_fun => fun(_, _) -> undefined end}, memory, 1, MnesiaAcc
        )
    ),
    ?assertMatch(
        {unavailable, scan_budget_exceeded, _},
        observer_cli_snapshot:collect_counter_resources(
            network, #{count_fun => fun() -> 100001 end}, io, 20, undefined, #{}
        )
    ),
    CounterSource = #{
        all_fun => fun() -> {error, failed} end,
        monotonic_fun => fun() -> 1 end
    },
    ?assertMatch(
        {error, enumeration_error, _},
        observer_cli_snapshot:collect_admitted_counter_resources(
            network, CounterSource, io, 20, undefined, #{}, 1
        )
    ),
    ?assertMatch(
        {error, enumeration_error, _},
        observer_cli_snapshot:collect_admitted_counter_resources(
            network, CounterSource, io, 20, 1, #{}, 1
        )
    ),
    ?assertEqual(
        disappeared,
        observer_cli_snapshot:network_resource(port, #{
            name_fun => fun(_) -> {ok, "tcp_inet"} end,
            stat_fun => fun(_) -> invalid end
        })
    ),
    ?assertMatch(
        {unavailable, scan_budget_exceeded, _},
        observer_cli_snapshot:collect_ports(
            #{count_fun => fun() -> 100001 end}, memory, 20, #{}
        )
    ),
    ?assertMatch(
        {error, enumeration_error, _},
        observer_cli_snapshot:collect_ports(
            #{count_fun => fun() -> 1 end, all_fun => fun() -> {error, failed} end},
            memory,
            20,
            #{}
        )
    ),
    ?assertMatch(
        {error, enumeration_error, _},
        observer_cli_snapshot:collect_ports(
            #{count_fun => fun() -> 1 end, all_fun => fun() -> invalid end},
            memory,
            20,
            #{}
        )
    ),
    put(counter_sample_calls, 0),
    DeltaSource = #{
        all_fun => fun() ->
            Calls = get(counter_sample_calls),
            put(counter_sample_calls, Calls + 1),
            case Calls of
                0 -> {ok, []};
                _ -> {error, second_failed}
            end
        end,
        monotonic_fun => fun() -> 10 end,
        io_fun => fun() -> {{input, 0}, {output, 0}} end,
        sleep_fun => fun(_) -> ok end
    },
    ?assertMatch(
        {error, enumeration_error, _},
        observer_cli_snapshot:collect_admitted_counter_resources(
            network, DeltaSource, io, 20, 1, #{}, 1
        )
    ),
    erase(counter_sample_calls),
    PortInfo = fun
        (item, name) ->
            {ok, "efile"};
        (skip, name) ->
            {ok, "tcp_inet"};
        (gone, _) ->
            missing;
        (_, connected) ->
            {ok, self()};
        (_, Key) when
            Key =:= queue_size;
            Key =:= memory;
            Key =:= id;
            Key =:= input;
            Key =:= output
        ->
            {ok, 1};
        (_, _) ->
            missing
    end,
    ?assertMatch(
        {ok, #{items := [_], disappeared_count := 1}, _},
        observer_cli_snapshot:collect_ports(
            #{
                count_fun => fun() -> 3 end,
                all_fun => fun() -> {ok, [item, skip, gone]} end,
                info_fun => PortInfo
            },
            memory,
            20,
            #{}
        )
    ).

snapshot_last_pure_branches_test() ->
    Source = #{alive_fun => fun(_) -> true end, whereis_fun => fun(_) -> undefined end},
    ?assertEqual(
        not_found,
        observer_cli_snapshot:resolve_pid_text(
            <<"<0.999999999999999999999999999999999999999.0>">>, Source
        )
    ),
    ?assertEqual(
        not_found, observer_cli_snapshot:resolve_registered_name(<<"kernel">>, Source)
    ),
    ?assertEqual(
        #{},
        observer_cli_snapshot:application_leaders([kernel], #{
            supervisor_fun => fun(_) -> {ok, self()} end,
            root_info_fun => fun(_, _) -> invalid end
        })
    ),
    ?assertEqual(
        error,
        observer_cli_snapshot:safe_port_option_value(
            bind_to_device, binary:copy(<<"x">>, 65537)
        )
    ),
    Topology = #{
        schedulers_configured => 1, schedulers_online => 1, dirty_cpu_schedulers_online => 1
    },
    First = #{
        wall_time => [{1, 1, 2}, {2, 1, 2}],
        run_queue_lengths => [0, 0],
        monotonic_ms => 1
    },
    Second = #{
        wall_time => [{1, 2, 4}, {2, 2, 4}],
        run_queue_lengths => [0, 0],
        monotonic_ms => 2
    },
    ?assertEqual(
        {error, missing_scheduler_id},
        observer_cli_snapshot:scheduler_window_data(
            Topology, First#{wall_time := [{2, 1, 2}]}, Second
        )
    ),
    ?assertEqual(
        {error, invalid_run_queue_shape},
        observer_cli_snapshot:scheduler_window_data(
            Topology, First#{run_queue_lengths := invalid}, Second
        )
    ),
    State = #{ids => #{}, counts => #{}},
    ?assertEqual(
        {error, invalid_schema},
        observer_cli_snapshot:normalize_value({mfa, erlang, node, invalid}, include, 0, State)
    ),
    ?assertEqual(
        {error, invalid_schema},
        observer_cli_snapshot:normalize_map(
            [{<<"key">>, fun() -> ok end}], include, 0, State, #{}
        )
    ),
    ?assertEqual(error, observer_cli_snapshot:normalize_key(binary:copy(<<"x">>, 65537))),
    ?assertEqual(error, observer_cli_snapshot:identifier_text(binary:copy(<<"x">>, 65537))),
    Dead = spawn(fun() -> ok end),
    DeadMon = erlang:monitor(process, Dead),
    receive
        {'DOWN', DeadMon, process, Dead, normal} -> ok
    end,
    ?assertMatch(
        #{state := dead},
        observer_cli_snapshot:child_pid_item(Dead, #{alive_fun => fun(_) -> false end})
    ),
    ?assertEqual(error, observer_cli_snapshot:application_name_binary([16#110000])),
    ProcessSource = #{
        whereis_fun => fun(_) -> undefined end,
        alive_fun => fun(_) -> true end,
        info_fun => fun(_, _) -> [{status, waiting}] end
    },
    ?assertMatch(
        {ok, #{status := waiting}, _},
        observer_cli_snapshot:collect_process(
            list_to_binary(pid_to_list(self())), ProcessSource
        )
    ),
    Tid = make_ref(),
    MnesiaSource = #{
        info_fun => fun
            (_, storage_type) -> ram_copies;
            (_, size) -> 1;
            (_, memory) -> 1
        end,
        word_size_fun => fun() -> 8 end,
        whereis_fun => fun(_) -> undefined end,
        ets_info_fun => fun(_, _) -> undefined end
    },
    MnesiaAcc = #{scanned => 0, disappeared => 0, eligible => 0, top => []},
    ?assertEqual(
        0,
        maps:get(
            eligible,
            observer_cli_snapshot:scan_mnesia_table(
                Tid, MnesiaSource, unknown_sort, 1, MnesiaAcc
            )
        )
    ),
    ?assertEqual(
        {management_unknown, Tid},
        observer_cli_snapshot:mnesia_correlation(Tid, ram_copies, MnesiaSource)
    ),
    DefaultProcess = observer_cli_snapshot:default_process_source(),
    ?assert(is_integer((maps:get(monotonic_fun, DefaultProcess))())),
    DefaultMnesia = observer_cli_snapshot:default_mnesia_source(),
    ?assert(is_boolean((maps:get(available_fun, DefaultMnesia))())),
    ?assertEqual(invalid, maps:get(status, observer_cli_snapshot:scheduler_window(#{}, #{}))).

snapshot_rare_branch_contract_test() ->
    self() ! stop,
    ?assertEqual(ok, observer_cli_snapshot:probe(test_timeout, self(), #{})),
    receive
        {test_worker, _} -> ok
    end,
    ?assertMatch(
        #{command := trace_stop_all},
        observer_cli_snapshot:capture_trace(
            #{action => stop_all}, #{controller => self()}
        )
    ),
    ?assertEqual(
        scan_budget_exceeded,
        maps:get(
            reason_code,
            observer_cli_snapshot:diagnostic_ets(#{
                sample_index => 0,
                test_ets_source => #{count_fun => fun() -> 100001 end}
            })
        )
    ),
    ?assertEqual(
        scan_budget_exceeded,
        maps:get(
            reason_code,
            observer_cli_snapshot:diagnostic_ports(
                #{
                    sample_index => 0,
                    test_port_source => #{count_fun => fun() -> 100001 end}
                },
                #{}
            )
        )
    ),
    AppBase = #{
        loaded_fun => fun() -> [{kernel, "Kernel", "1"}] end,
        running_fun => fun(_) -> [{kernel, "Kernel", "1"}] end,
        supervisor_fun => fun(_) -> {ok, self()} end,
        root_info_fun => fun(_, _) -> {group_leader, self()} end,
        alive_fun => fun(_) -> true end,
        which_children_fun => fun(_) -> [] end
    },
    UnavailableApp = observer_cli_snapshot:diagnostic_application(#{
        observe => true,
        app => "kernel",
        test_application_source => AppBase#{
            count_children_fun => fun(_) ->
                [
                    {specs, 5001}, {active, 5001}, {supervisors, 0}, {workers, 5001}
                ]
            end
        }
    }),
    ?assertEqual(unavailable, maps:get(status, UnavailableApp)),
    ErrorApp = observer_cli_snapshot:diagnostic_application(#{
        observe => true,
        app => "kernel",
        test_application_source => AppBase#{count_children_fun => fun(_) -> invalid end}
    }),
    ?assertEqual(error, maps:get(status, ErrorApp)),
    ProcessSource = #{count_fun => fun() -> 100001 end},
    ?assertMatch(
        {unavailable, scan_budget_exceeded, _},
        observer_cli_snapshot:collect_admitted_applications(
            [],
            [],
            [],
            #{},
            ProcessSource,
            memory,
            20,
            #{deadline => erlang:monotonic_time(millisecond) + 1000},
            0
        )
    ),
    ?assertEqual(
        null,
        maps:get(
            connected_pid,
            observer_cli_snapshot:port_resource(item, #{
                info_fun => fun
                    (_, name) ->
                        {ok, "efile"};
                    (_, connected) ->
                        {ok, invalid};
                    (_, Key) when
                        Key =:= queue_size;
                        Key =:= memory;
                        Key =:= id;
                        Key =:= input;
                        Key =:= output
                    ->
                        {ok, 1};
                    (_, _) ->
                        missing
                end
            })
        )
    ),
    ?assertEqual(
        null,
        maps:get(
            io,
            observer_cli_snapshot:port_resource(item, #{
                info_fun => fun
                    (_, name) -> {ok, "efile"};
                    (_, connected) -> {ok, self()};
                    (_, input) -> missing;
                    (_, output) -> {ok, 1};
                    (_, Key) when Key =:= queue_size; Key =:= memory; Key =:= id -> {ok, 1};
                    (_, _) -> missing
                end
            })
        )
    ),
    ?assertEqual(
        not_found,
        observer_cli_snapshot:resolve_port_target(
            <<"#Port<0.999999999999999999999999999>">>
        )
    ),
    ?assertEqual(not_found, observer_cli_snapshot:resolve_port_target(<<"#Port<0.01>">>)),
    ?assertEqual(error, observer_cli_snapshot:bounded_identifier_text([<<"x">> | improper])),
    ?assert(
        observer_cli_snapshot:resource_precedes(
            #{raw_id => a, value => 1}, #{raw_id => b, value => null}, value
        )
    ),
    ?assertNot(
        observer_cli_snapshot:resource_precedes(
            #{raw_id => a, value => null}, #{raw_id => b, value => 1}, value
        )
    ),
    Hidden = {identifier, peer, hidden},
    Limited = observer_cli_snapshot:limit_distribution(
        #{
            connected_peers => [Hidden],
            visible_peers => [],
            hidden_peers => [Hidden],
            controller_queues => []
        },
        1
    ),
    ?assertEqual([Hidden], maps:get(hidden_peers, Limited)).

allocator_data_test() ->
    Data = observer_cli_snapshot:allocator_data(#{
        average_block_curs => [
            {binary_alloc, [{mbcs, 10.5}, {sbcs, 20}]},
            {eheap_alloc, [{mbcs, 30}, {sbcs, 40}]}
        ],
        average_block_maxes => [
            {binary_alloc, [{mbcs, 50}, {sbcs, 60}]},
            {eheap_alloc, [{mbcs, 70}, {sbcs, 80}]}
        ],
        sbcs_to_mbcs_curs => [{binary_alloc, 0.25}],
        sbcs_to_mbcs_maxes => [{binary_alloc, 0.5}],
        cache_hit_info => [
            {{instance, 2}, [{hit_rate, 0.5}, {hits, 2}, {calls, 4}]},
            {{instance, 0}, [{hit_rate, 1.0}, {hits, 0}, {calls, 0}]}
        ]
    }),
    [Binary, Eheap] = maps:get(util_allocators, Data),
    ?assertEqual(binary_alloc, maps:get(allocator, Binary)),
    ?assertEqual(10.5, maps:get(current_mbcs_average_block_size_bytes, Binary)),
    ?assertEqual(0.5, maps:get(max_sbcs_to_mbcs_ratio, Binary)),
    ?assertEqual(eheap_alloc, maps:get(allocator, Eheap)),
    ?assertEqual(null, maps:get(current_sbcs_to_mbcs_ratio, Eheap)),
    [First, Second] = maps:get(cache_hit_rates, Data),
    ?assertEqual(0, maps:get(instance, First)),
    ?assertEqual(0, maps:get(calls, First)),
    ?assertEqual(2, maps:get(instance, Second)).

memory_command_includes_allocator_metrics_test() ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(self(), memory, #{}, options(5000, include)),
    Capture = maps:get(<<"capture">>, Response),
    assert_probe(<<"memory">>, true, <<"ok">>, Capture),
    assert_probe(<<"allocator">>, true, <<"ok">>, Capture),
    Allocator = maps:get(<<"allocator">>, maps:get(<<"memory">>, maps:get(<<"data">>, Response))),
    ?assertEqual(9, length(maps:get(<<"util_allocators">>, Allocator))),
    ?assert(lists:all(fun is_map/1, maps:get(<<"cache_hit_rates">>, Allocator))),
    assert_json_safe(Response).

memory_command_keeps_beam_data_when_allocator_fails_test() ->
    Request = #{test_probe_outcomes => #{allocator => {error, probe_failed}}},
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(self(), memory, Request, options(5000, include)),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, Response))),
    Memory = maps:get(<<"memory">>, maps:get(<<"data">>, Response)),
    ?assert(is_map(maps:get(<<"beam">>, Memory))),
    ?assertEqual(null, maps:get(<<"allocator">>, Memory)),
    [Error] = maps:get(<<"errors">>, Response),
    ?assertEqual(<<"allocator">>, maps:get(<<"probe">>, Error)),
    ?assertEqual(<<"probe_failed">>, maps:get(<<"reason_code">>, Error)).

tui_resource_counts_match_snapshot_window_test() ->
    Tui = maps:get(
        sys_info,
        observer_cli_system:collect_system_info("printf 'header\\n 0 0 0 0\\n'")
    ),
    Resources = maps:get(<<"resources">>, maps:get(<<"data">>, snapshot(#{}))),
    assert_count_within(
        proplists:get_value(process_count, Tui),
        maps:get(<<"observed_count_including_observer">>, maps:get(<<"process">>, Resources)),
        5
    ),
    assert_count_within(
        proplists:get_value(port_count, Tui),
        maps:get(<<"observed_count_including_observer">>, maps:get(<<"port">>, Resources)),
        5
    ),
    assert_count_within(
        proplists:get_value(atom_count, Tui),
        maps:get(<<"observed_count_including_observer">>, maps:get(<<"atom">>, Resources)),
        1000
    ).

trace_dispatch_uses_cli_envelope_test() ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), trace, #{action => call}, options(2000, include)
        ),
    ?assertEqual(<<"observer_cli.cli/v1">>, maps:get(<<"schema">>, Response)),
    ?assertEqual(null, maps:get(<<"capture">>, Response)),
    ?assertEqual(null, maps:get(<<"data">>, Response)),
    [Error] = maps:get(<<"errors">>, Response),
    ?assertEqual(<<"argument">>, maps:get(<<"class">>, Error)),
    ?assertEqual(<<"replace_existing_trace_required">>, maps:get(<<"reason_code">>, Error)).

forced_trace_dispatch_marks_outer_capture_partial_test() ->
    Request = #{
        action => call,
        mfa => <<"erlang:node/0">>,
        pid => list_to_binary(pid_to_list(self())),
        duration_ms => 100,
        max => 1,
        replace_existing_trace => true
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(self(), trace, Request, options(2000, include)),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, Response))),
    Trace = maps:get(<<"trace">>, maps:get(<<"data">>, Response)),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, Trace)),
    ?assertEqual(false, maps:get(<<"trace_complete">>, Trace)).

default_snapshot_is_scan_free_fact_package_test() ->
    Response = snapshot(#{}),
    ?assertEqual(<<"snapshot">>, maps:get(<<"command">>, Response)),
    ?assertMatch(
        #{<<"node">> := <<"node-1">>, <<"otp_release">> := _},
        maps:get(<<"target">>, Response)
    ),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, Capture)),
    ?assert(is_binary(maps:get(<<"started_at">>, Capture))),
    ?assert(is_binary(maps:get(<<"finished_at">>, Capture))),
    ?assert(maps:get(<<"duration_ms">>, Capture) >= 0),
    assert_probe(<<"runtime">>, true, <<"ok">>, Capture),
    assert_probe(<<"resources">>, true, <<"ok">>, Capture),
    assert_probe(<<"memory">>, true, <<"ok">>, Capture),
    assert_probe(<<"schedulers">>, false, <<"ok">>, Capture),
    assert_probe(<<"distribution">>, false, <<"ok">>, Capture),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(1, maps:get(<<"snapshot_version">>, Data)),
    Resources = maps:get(<<"resources">>, Data),
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                true,
                maps:get(<<"observer_contaminated">>, maps:get(Key, Resources))
            )
        end,
        [<<"process">>, <<"port">>, <<"atom">>]
    ),
    Memory = maps:get(<<"memory">>, Data),
    ?assertEqual(
        true,
        maps:get(<<"observer_contaminated">>, maps:get(<<"beam">>, Memory))
    ),
    GC = maps:get(<<"garbage_collection">>, Memory),
    ?assertEqual(
        maps:get(<<"reclaimed_words_total">>, GC) * erlang:system_info(wordsize),
        maps:get(<<"reclaimed_bytes_total">>, GC)
    ),
    ?assertEqual(
        false,
        maps:get(
            <<"scheduler_wall_time_enabled_by_observer_cli">>,
            maps:get(<<"schedulers">>, Data)
        )
    ),
    Distribution = maps:get(<<"distribution">>, Data),
    ?assertEqual(<<"empty">>, maps:get(<<"state">>, Distribution)),
    ?assertEqual([], maps:get(<<"connected_peers">>, Distribution)),
    [ModuleEffect] = [
        Effect
     || #{<<"id">> := <<"module_load">>} = Effect <-
            maps:get(<<"observer_effects">>, Capture)
    ],
    ?assertEqual(true, maps:get(<<"module_loaded_before_sample">>, ModuleEffect)),
    ?assertEqual([], maps:get(<<"errors">>, Response)),
    ?assertEqual([], maps:get(<<"warnings">>, Response)),
    ?assertEqual(
        nomatch,
        binary:match(term_to_binary(Response), atom_to_binary(node()))
    ),
    assert_json_safe(Response).

default_snapshot_does_not_call_full_enumerators_test() ->
    Parent = self(),
    Tracer = spawn(fun() -> trace_forwarder(Parent) end),
    Enumerators = [
        {erlang, processes, 0},
        {erlang, ports, 0},
        {ets, all, 0},
        {socket, which_sockets, 0},
        {application, loaded_applications, 0},
        {application, which_applications, 0},
        {application, which_applications, 1},
        {mnesia, system_info, 1},
        {recon_alloc, average_block_sizes, 1},
        {recon_alloc, sbcs_to_mbcs, 1},
        {recon_alloc, cache_hit_rates, 0}
    ],
    lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, true, [local]) end, Enumerators),
    erlang:trace(new, true, [call, {tracer, Tracer}]),
    try
        _ = snapshot(#{}),
        receive
            {enumerator_called, Call} -> ?assertEqual(no_full_enumerator_call, Call)
        after 100 ->
            ok
        end
    after
        erlang:trace(new, false, [call]),
        lists:foreach(fun(MFA) -> erlang:trace_pattern(MFA, false, [local]) end, Enumerators),
        exit(Tracer, kill)
    end.

default_snapshot_and_diagnose_never_get_gen_server_state_test() ->
    Parent = self(),
    Tracer = spawn(fun() -> state_trace_forwarder(Parent) end),
    erlang:trace_pattern({sys, get_state, 2}, true, [local]),
    erlang:trace(new, true, [call, {tracer, Tracer}]),
    try
        _ = snapshot(#{}),
        _ = observer_cli_snapshot:dispatch(self(), diagnose, #{}, options(2000, redact)),
        receive
            state_requested -> ?assert(false)
        after 100 ->
            ok
        end
    after
        erlang:trace(new, false, [call]),
        erlang:trace_pattern({sys, get_state, 2}, false, [local]),
        exit(Tracer, kill)
    end.

deep_binary_holder_scan_is_independently_admitted_and_does_not_force_gc_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Source = process_source([Pid], fun(ItemPid, Keys) ->
        Parent ! {binary_holder_keys, Keys},
        [{binary, [{make_ref(), 123, 1}]} || ItemPid =:= Pid, Keys =:= [binary]]
    end),
    try
        Result = observer_cli_snapshot:diagnostic_binary_holders(
            #{test_process_source => Source}, #{controller => self()}
        ),
        ?assertEqual(ok, maps:get(status, Result)),
        [Item] = maps:get(items, Result),
        ?assertEqual(123, maps:get(binary_reference_bytes, Item)),
        receive
            {binary_holder_keys, [binary]} -> ok
        after 1000 -> ?assert(false)
        end,
        ?assertEqual(nomatch, binary:match(term_to_binary(Result), <<"bin_leak">>))
    after
        Pid ! stop
    end.

gen_server_state_shape_is_value_free_and_bounded_test_() ->
    {timeout, 10, fun gen_server_state_shape_is_value_free_and_bounded/0}.

gen_server_state_shape_is_value_free_and_bounded() ->
    Secret = <<"goal12-fixture-secret">>,
    State = #{
        Secret => {secret_tag, [Secret, 42, #{nested_secret => Secret}]},
        public_key => binary:copy(Secret, 100),
        types => [self(), make_ref(), fun() -> Secret end, <<1:3>>]
    },
    {ok, Server} = gen_server:start_link(?MODULE, State, []),
    unlink(Server),
    register(goal12_state_server, Server),
    try
        Response = inspection(gen_server_state, #{target => <<"goal12_state_server">>}),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(<<"high">>, maps:get(<<"risk_level">>, Data)),
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, Data)),
        Acquisition = maps:get(<<"acquisition">>, Data),
        ?assertEqual(true, maps:get(<<"full_state_copy_risk">>, Acquisition)),
        ?assertEqual(false, maps:get(<<"timeout_retracts_delivered_request">>, Acquisition)),
        Shape = maps:get(<<"state_shape">>, Data),
        ?assertEqual(<<"map">>, maps:get(<<"type">>, Shape)),
        ?assert(erlang:external_size(Shape) =< 64 * 1024),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), Secret)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"secret_tag">>)),
        ?assertMatch(
            [
                #{<<"reason_code">> := <<"sys_get_state_copies_full_state">>},
                #{<<"reason_code">> := <<"timeout_does_not_retract_delivered_request">>}
            ],
            maps:get(<<"warnings">>, Response)
        )
    after
        unregister(goal12_state_server),
        exit(Server, kill)
    end.

gen_server_state_target_resolution_is_uniform_test() ->
    Targets = [
        <<"goal12-unknown-name">>,
        <<"goal12_existing_unregistered">>,
        binary:copy(<<"x">>, 256)
    ],
    Dead = spawn(fun() -> ok end),
    timer:sleep(10),
    DeadTarget = list_to_binary(pid_to_list(Dead)),
    lists:foreach(
        fun(Target) -> _ = inspection(gen_server_state, #{target => Target}) end, Targets
    ),
    _ = inspection(gen_server_state, #{target => DeadTarget}),
    AtomCount = erlang:system_info(atom_count),
    lists:foreach(
        fun(Target) ->
            Response = inspection(gen_server_state, #{target => Target}),
            ?assertEqual(<<"not_found">>, maps:get(<<"status">>, maps:get(<<"data">>, Response)))
        end,
        Targets
    ),
    DeadResponse = inspection(gen_server_state, #{target => DeadTarget}),
    ?assertEqual(<<"not_found">>, maps:get(<<"status">>, maps:get(<<"data">>, DeadResponse))),
    ?assertEqual(AtomCount, erlang:system_info(atom_count)).

gen_server_state_depth_and_node_caps_test() ->
    lists:foreach(
        fun({State, Type}) ->
            Data = maps:get(<<"data">>, state_fixture_response(State)),
            ?assertEqual(Type, maps:get(<<"type">>, maps:get(<<"state_shape">>, Data)))
        end,
        [
            {an_atom, <<"atom">>},
            {42, <<"number">>},
            {<<"binary-secret">>, <<"binary">>},
            {<<1:3>>, <<"bitstring">>},
            {#{key_secret => value_secret}, <<"map">>},
            {{tuple_secret}, <<"tuple">>},
            {[list_secret], <<"list">>},
            {self(), <<"other">>}
        ]
    ),
    Deep = lists:foldl(fun(_, Acc) -> {Acc} end, leaf_secret, lists:seq(1, 10)),
    DeepResponse = state_fixture_response(Deep),
    ?assertNotEqual(
        nomatch, binary:match(term_to_binary(DeepResponse), <<"depth_cap">>)
    ),
    WideResponse = state_fixture_response(lists:seq(1, 20000)),
    WideData = maps:get(<<"data">>, WideResponse),
    ?assertEqual(10000, maps:get(<<"visited_node_count">>, WideData)),
    ?assertEqual(
        <<"node_cap">>,
        maps:get(<<"truncation_reason">>, maps:get(<<"state_shape">>, WideData))
    ),
    ?assert(erlang:external_size(maps:get(<<"state_shape">>, WideData)) =< 64 * 1024).

gen_server_state_timeout_crash_and_heap_are_redacted_test_() ->
    {timeout, 15, fun gen_server_state_timeout_crash_and_heap_are_redacted/0}.

gen_server_state_timeout_crash_and_heap_are_redacted() ->
    Secret = <<"goal12-error-secret">>,
    {ok, Server} = gen_server:start_link(?MODULE, Secret, []),
    unlink(Server),
    Parent = self(),
    Caller = spawn(fun() -> gen_server:call(Server, {block, Parent}, infinity) end),
    receive
        {server_blocked, Server} -> ok
    after 1000 -> erlang:error(block_fixture_timeout)
    end,
    Timeout = observer_cli_snapshot:dispatch(
        self(),
        gen_server_state,
        #{target => list_to_binary(pid_to_list(Server))},
        options(3000, redact)
    ),
    ?assertEqual(nomatch, binary:match(term_to_binary(Timeout), Secret)),
    #{<<"status">> := <<"ok">>, <<"result">> := TimeoutResponse} = Timeout,
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, TimeoutResponse))),
    {messages, PendingSystemRequests} = process_info(Server, messages),
    ?assertNotEqual([], PendingSystemRequests),
    Server ! release,
    timer:sleep(20),
    ?assert(is_process_alive(Server)),
    ?assertEqual({messages, []}, process_info(Server, messages)),
    CrashSource = state_source(fun(_Pid, _Timeout) -> erlang:error({Secret, crash}) end),
    Crash = inspection(gen_server_state, #{
        target => list_to_binary(pid_to_list(Server)), test_state_source => CrashSource
    }),
    ?assertEqual(
        <<"state_probe_failed">>, maps:get(<<"reason_code">>, maps:get(<<"data">>, Crash))
    ),
    ?assertEqual(nomatch, binary:match(term_to_binary(Crash), Secret)),
    exit(Server, kill),
    exit(Caller, kill),
    HeapServer = start_state_server(lists:duplicate(100000, Secret)),
    Heap = observer_cli_snapshot:dispatch(
        self(),
        gen_server_state,
        #{target => list_to_binary(pid_to_list(HeapServer))},
        (options(3000, redact))#{max_heap_words => 4096}
    ),
    assert_error(<<"worker_heap_limit_exceeded">>, Heap),
    ?assertEqual(nomatch, binary:match(term_to_binary(Heap), Secret)),
    exit(HeapServer, kill).

snapshot_probe_failure_semantics_test() ->
    Unavailable = snapshot(#{
        test_probe_outcomes => #{schedulers => {unavailable, capability_unavailable}}
    }),
    UnavailableCapture = maps:get(<<"capture">>, Unavailable),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, UnavailableCapture)),
    assert_probe(<<"schedulers">>, false, <<"unavailable">>, UnavailableCapture),
    ?assertNot(is_map_key(<<"schedulers">>, maps:get(<<"data">>, Unavailable))),
    ?assertMatch(
        [#{<<"probe">> := <<"schedulers">>, <<"reason_code">> := <<"capability_unavailable">>}],
        maps:get(<<"warnings">>, Unavailable)
    ),
    OptionalTimeout = snapshot(#{
        test_probe_outcomes => #{schedulers => {timeout, target_timeout}}
    }),
    ?assertEqual(
        <<"partial">>,
        maps:get(<<"status">>, maps:get(<<"capture">>, OptionalTimeout))
    ),
    ?assertMatch(
        [#{<<"class">> := <<"partial">>, <<"probe">> := <<"schedulers">>}],
        maps:get(<<"errors">>, OptionalTimeout)
    ),
    RequiredError = snapshot(#{
        test_probe_outcomes => #{resources => {error, probe_failed}}
    }),
    ?assertEqual(
        <<"partial">>, maps:get(<<"status">>, maps:get(<<"capture">>, RequiredError))
    ),
    ?assertNot(is_map_key(<<"resources">>, maps:get(<<"data">>, RequiredError))),
    ?assert(is_map_key(<<"memory">>, maps:get(<<"data">>, RequiredError))),
    ?assertMatch(
        [#{<<"class">> := <<"required_probe">>, <<"probe">> := <<"resources">>}],
        maps:get(<<"errors">>, RequiredError)
    ).

deep_snapshot_composes_narrow_probe_defaults_test() ->
    Response = snapshot(#{deep => true}),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, Capture)),
    lists:foreach(
        fun(Id) -> assert_probe(Id, false, <<"ok">>, Capture) end,
        [
            <<"processes">>,
            <<"applications">>,
            <<"ets">>,
            <<"mnesia">>,
            <<"network">>,
            <<"ports">>,
            <<"sockets">>
        ]
    ),
    Data = maps:get(<<"data">>, Response),
    lists:foreach(
        fun({Id, Sort, Semantics}) ->
            ProbeData = maps:get(Id, Data),
            ?assertEqual(Sort, maps:get(<<"sort">>, ProbeData)),
            ?assertEqual(Semantics, maps:get(<<"sort_semantics">>, ProbeData)),
            ?assert(maps:get(<<"returned_count">>, ProbeData) =< 20)
        end,
        [
            {<<"processes">>, <<"memory">>, <<"total">>},
            {<<"applications">>, <<"memory">>, <<"current">>},
            {<<"ets">>, <<"memory">>, <<"current">>},
            {<<"network">>, <<"oct">>, <<"total">>},
            {<<"ports">>, <<"queue_size">>, <<"current_or_lifetime">>},
            {<<"sockets">>, <<"io">>, <<"total">>}
        ]
    ),
    ?assertEqual(<<"memory">>, maps:get(<<"sort">>, maps:get(<<"mnesia">>, Data))),
    assert_json_safe(Response).

deep_snapshot_shares_report_identifier_dictionary_test() ->
    Pid = spawn(fun process_fixture/0),
    Table = make_ref(),
    ProcessSource = process_source([Pid], fun(Current, Keys) ->
        process_info_fixture(Current, Keys, 100)
    end),
    EtsValues = #{
        id => Table,
        name => goal_11_table,
        size => 1,
        memory => 2,
        owner => Pid,
        type => set,
        protection => public,
        keypos => 1,
        write_concurrency => true,
        read_concurrency => false
    },
    EtsSource = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() -> [Table] end,
        info_fun => fun(_Table, Key) -> maps:get(Key, EtsValues) end,
        word_size_fun => fun() -> 8 end
    },
    try
        Response = snapshot(#{
            deep => true,
            test_process_source => ProcessSource,
            test_ets_source => EtsSource,
            test_deep_probe_outcomes => #{
                applications =>
                    {unavailable, scan_budget_exceeded, #{admission_stage => fixture}, 0, [
                        admission_only
                    ]}
            }
        }),
        Data = maps:get(<<"data">>, Response),
        [Process] = maps:get(<<"items">>, maps:get(<<"processes">>, Data)),
        [TableItem] = maps:get(<<"items">>, maps:get(<<"ets">>, Data)),
        ?assertEqual(maps:get(<<"pid">>, Process), maps:get(<<"owner">>, TableItem)),
        ?assertEqual(true, maps:get(<<"write_concurrency">>, TableItem)),
        ?assertEqual(false, maps:get(<<"read_concurrency">>, TableItem))
    after
        exit(Pid, kill)
    end.

deep_snapshot_refusal_and_started_failure_contract_test() ->
    Refused = snapshot(#{
        deep => true,
        test_deep_probe_outcomes => #{
            ets =>
                {unavailable, scan_budget_exceeded,
                    #{admission_stage => pre_enumeration, observed_table_count => 100001}, 0, [
                        admission_only
                    ]}
        }
    }),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, maps:get(<<"capture">>, Refused))),
    [Skipped] = maps:get(<<"skipped">>, maps:get(<<"data">>, Refused)),
    ?assertEqual(<<"ets">>, maps:get(<<"probe">>, Skipped)),
    ?assertEqual(
        100001,
        maps:get(
            <<"observed_table_count">>, maps:get(<<"admission_evidence">>, Skipped)
        )
    ),
    lists:foreach(
        fun({Probe, Outcome}) ->
            Response = snapshot(#{
                deep => true, test_deep_probe_outcomes => #{Probe => Outcome}
            }),
            Capture = maps:get(<<"capture">>, Response),
            ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
            ?assertNot(is_map_key(atom_to_binary(Probe), maps:get(<<"data">>, Response)))
        end,
        [
            {processes, {timeout, target_timeout}},
            {applications, {error, probe_failed}}
        ]
    ).

deep_snapshot_heap_boundary_is_partial_test() ->
    Parent = self(),
    Source = process_source([self()], fun(_Pid, _Keys) -> undefined end),
    HeapSource = Source#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Parent ! {deep_heap_worker, self()},
                length(lists:seq(1, 1000000))
            end}
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            #{
                deep => true,
                test_process_source => HeapSource,
                test_deep_probe_outcomes => #{
                    applications =>
                        {unavailable, scan_budget_exceeded, #{admission_stage => fixture}, 0, [
                            admission_only
                        ]}
                }
            },
            #{timeout_ms => 5000, identifier_policy => redact, max_heap_words => 262144}
        ),
    Worker =
        receive
            {deep_heap_worker, Pid} -> Pid
        end,
    ?assertNot(is_process_alive(Worker)),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
    [ProcessProbe] = [
        Probe
     || #{<<"id">> := <<"processes">>} = Probe <- maps:get(<<"probes">>, Capture)
    ],
    ?assertEqual(<<"worker_heap_limit_exceeded">>, maps:get(<<"reason_code">>, ProcessProbe)).

deep_snapshot_started_timeout_cleans_probe_worker_test() ->
    Parent = self(),
    Source = (process_source([self()], fun(_Pid, _Keys) -> undefined end))#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Parent ! {deep_timeout_worker, self()},
                receive
                    stop -> ok
                end
            end}
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            #{deep => true, test_process_source => Source},
            options(2000, redact)
        ),
    Worker =
        receive
            {deep_timeout_worker, Pid} -> Pid
        end,
    ?assertNot(is_process_alive(Worker)),
    Capture = maps:get(<<"capture">>, Response),
    ?assertEqual(<<"partial">>, maps:get(<<"status">>, Capture)),
    [ProcessProbe] = [
        Probe
     || #{<<"id">> := <<"processes">>} = Probe <- maps:get(<<"probes">>, Capture)
    ],
    ?assertEqual(<<"timeout">>, maps:get(<<"status">>, ProcessProbe)),
    ?assertEqual(<<"target_timeout">>, maps:get(<<"reason_code">>, ProcessProbe)).

deep_snapshot_controller_disconnect_cleans_probe_worker_test() ->
    Parent = self(),
    Controller = spawn(fun process_fixture/0),
    Killer = spawn(fun() ->
        receive
            {deep_worker, Worker} ->
                Parent ! {deep_worker, Worker},
                exit(Controller, kill)
        end
    end),
    Source = (process_source([self()], fun(_Pid, _Keys) -> undefined end))#{
        fold :=
            {fixture_list, fun(_Fun, _Acc) ->
                Killer ! {deep_worker, self()},
                receive
                    stop -> ok
                end
            end}
    },
    Result = observer_cli_snapshot:dispatch(
        Controller,
        snapshot,
        #{deep => true, test_process_source => Source},
        options(5000, redact)
    ),
    Worker =
        receive
            {deep_worker, Pid} -> Pid
        end,
    ?assertEqual(<<"error">>, maps:get(<<"status">>, Result)),
    ?assertEqual(<<"controller_disconnected">>, maps:get(<<"reason_code">>, Result)),
    ?assertNot(is_process_alive(Worker)).

local_snapshot_text_and_term_envelopes_test() ->
    Response = snapshot(#{}),
    {ok, Text} = observer_cli_cli:encode(text, Response),
    ?assertNotEqual(nomatch, binary:match(Text, <<"observer_cli.cli/v1">>)),
    {ok, Term} = observer_cli_cli:encode(term, Response),
    {ok, Tokens, _EndLocation} = erl_scan:string(binary_to_list(Term)),
    ?assertEqual({ok, Response}, erl_parse:parse_term(Tokens)).

runtime_inspection_commands_test_() ->
    {timeout, 10, fun runtime_inspection_commands/0}.

runtime_inspection_commands() ->
    Memory = inspection(memory, #{}),
    ?assertMatch(
        #{
            <<"command">> := <<"memory">>,
            <<"data">> := #{
                <<"runtime">> := #{<<"word_size_bytes">> := _},
                <<"memory">> := #{<<"beam">> := #{<<"total_bytes">> := _}}
            }
        },
        Memory
    ),
    MemoryFacts = maps:get(<<"memory">>, maps:get(<<"data">>, Memory)),
    ?assertEqual(
        [<<"allocator">>, <<"beam">>, <<"persistent_term">>],
        lists:sort(maps:keys(MemoryFacts))
    ),
    Schedulers = inspection(schedulers, #{duration_ms => 250}),
    Measurement = maps:get(<<"data">>, Schedulers),
    ?assertEqual(<<"valid">>, maps:get(<<"status">>, Measurement)),
    ?assertEqual(<<"opaque_same_window">>, maps:get(<<"wall_time_unit">>, Measurement)),
    ?assertEqual(
        <<"runnable_or_running_observation_not_backlog">>,
        maps:get(<<"semantics">>, maps:get(<<"run_queues">>, Measurement))
    ),
    ?assertEqual(false, maps:get(<<"snapshot_atomic">>, maps:get(<<"run_queues">>, Measurement))),
    ?assertNot(is_map_key(<<"utilization_ns">>, Measurement)),
    Distribution = inspection(distribution, #{limit => 20}),
    DistributionData = maps:get(<<"data">>, Distribution),
    ?assertEqual(
        <<"context_only_not_backlog_health">>,
        maps:get(<<"queue_semantics">>, DistributionData)
    ),
    ?assert(is_list(maps:get(<<"controller_queues">>, DistributionData))).

invalid_runtime_inspection_requests_are_rejected_test() ->
    Cases = [
        {snapshot, invalid, invalid_request},
        {memory, invalid, invalid_request},
        {schedulers, #{duration_ms => 249}, invalid_duration},
        {schedulers, invalid, invalid_duration},
        {distribution, #{limit => 0}, invalid_limit},
        {distribution, invalid, invalid_request},
        {processes, #{sort => invalid}, invalid_request},
        {processes, invalid, invalid_request},
        {process, invalid, invalid_request},
        {port, invalid, invalid_request},
        {applications, invalid, invalid_request},
        {ets, invalid, invalid_request},
        {mnesia, invalid, invalid_request},
        {network, invalid, invalid_request},
        {ports, invalid, invalid_request},
        {sockets, invalid, invalid_request},
        {gen_server_state, invalid, invalid_request},
        {supervision_tree, invalid, invalid_request},
        {trace, invalid, invalid_request},
        {unknown_command, #{}, capability_unavailable}
    ],
    lists:foreach(
        fun({Command, Request, Reason}) ->
            Result = observer_cli_snapshot:dispatch(
                self(), Command, Request, options(3000, redact)
            ),
            ?assertNotEqual(
                nomatch,
                binary:match(term_to_binary(Result), atom_to_binary(Reason))
            )
        end,
        Cases
    ),
    lists:foreach(
        fun({Controller, Options, Reason}) ->
            Result = observer_cli_snapshot:dispatch(Controller, memory, #{}, Options),
            ?assertNotEqual(
                nomatch,
                binary:match(term_to_binary(Result), atom_to_binary(Reason))
            )
        end,
        [
            {not_a_pid, options(3000, redact), invalid_request},
            {self(), options(1000, redact), target_timeout},
            {self(), #{timeout_ms => 3000, identifier_policy => invalid}, invalid_request}
        ]
    ).

process_inventory_boundary_and_stable_top_n_test() ->
    Parent = self(),
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 4)],
    try
        Values = maps:from_list(lists:zip(Pids, [10, 20, 20, 5])),
        Source = process_source(Pids, fun(Pid, Keys) ->
            Parent ! {process_info_keys, Keys},
            process_info_fixture(Pid, Keys, maps:get(Pid, Values))
        end),
        Response = inspection_include(processes, #{
            sort => memory, limit => 2, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(4, maps:get(<<"scanned_count">>, Data)),
        ?assertEqual(4, maps:get(<<"eligible_count">>, Data)),
        ?assertEqual(2, maps:get(<<"returned_count">>, Data)),
        ?assertEqual(2, maps:get(<<"dropped_count">>, Data)),
        ?assertEqual(<<"fixture_list">>, maps:get(<<"inventory_path">>, Data)),
        [First, Second] = maps:get(<<"items">>, Data),
        ExpectedTie = [
            Pid
         || {Pid, _, _} <- recon_lib:sublist_top_n_attrs(
                [{Pid, maps:get(Pid, Values), Pid} || Pid <- Pids], 2
            )
        ],
        ?assertEqual(
            [list_to_binary(pid_to_list(Pid)) || Pid <- ExpectedTie],
            [maps:get(<<"pid">>, First), maps:get(<<"pid">>, Second)]
        ),
        KeysList = [
            receive
                {process_info_keys, K} -> K
            end
         || _ <- Pids
        ],
        ?assert(
            lists:all(
                fun(Keys) ->
                    Keys =:= [current_function, initial_call, memory, registered_name]
                end,
                KeysList
            )
        ),
        Forbidden = [
            messages,
            dictionary,
            state,
            current_stacktrace,
            links,
            monitors,
            monitored_by,
            suspending,
            binary
        ],
        ?assertEqual([], [Key || Keys <- KeysList, Key <- Keys, lists:member(Key, Forbidden)]),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"#Ref<">>))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

binary_memory_is_explicit_and_refs_do_not_escape_test() ->
    Parent = self(),
    Pid = spawn(fun process_fixture/0),
    Reference = make_ref(),
    try
        Source = process_source([Pid], fun(_Pid, Keys) ->
            Parent ! {binary_keys, Keys},
            [
                {registered_name, []},
                {current_function, {?MODULE, process_fixture, 0}},
                {initial_call, {?MODULE, process_fixture, 0}},
                {memory, 100},
                {binary, [{Reference, 42, 1}]}
            ]
        end),
        Response = inspection(processes, #{
            sort => binary_memory, limit => 1, test_process_source => Source
        }),
        receive
            {binary_keys, Keys} -> ?assertEqual(true, lists:member(binary, Keys))
        end,
        [Item] = maps:get(<<"items">>, maps:get(<<"data">>, Response)),
        ?assertEqual(42, maps:get(<<"binary_memory_bytes">>, Item)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), ref_to_binary(Reference)))
    after
        exit(Pid, kill)
    end.

process_detail_collects_tui_field_gap_items_test() ->
    Parent = self(),
    Pid = spawn(fun process_fixture/0),
    Port = open_port({spawn, "cat"}, [binary]),
    Ref = make_ref(),
    try
        Source = process_source([Pid], fun(_Pid, Keys) ->
            Parent ! {process_detail_keys, Keys},
            process_detail_info(Keys, Port, Ref)
        end),
        Response = inspection(process, #{
            target => list_to_binary(pid_to_list(Pid)), test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(<<"running">>, maps:get(<<"status">>, Data)),
        assert_json_safe(Response),
        ?assertEqual(2, maps:get(<<"binary_refs_count">>, Data)),
        ?assertEqual(128, maps:get(<<"binary_refs_bytes">>, Data)),
        ?assertEqual(128, maps:get(<<"binary_memory_bytes">>, Data)),
        ?assertEqual(<<"normal">>, maps:get(<<"priority">>, Data)),
        ?assertEqual(1, maps:get(<<"catchlevel">>, Data)),
        ?assertEqual(true, maps:get(<<"trap_exit">>, Data)),
        ?assertMatch(<<"module-", _/binary>>, maps:get(<<"error_handler">>, Data)),
        ?assertMatch(
            #{
                <<"min_bin_vheap_size">> := 2,
                <<"min_heap_size">> := 3,
                <<"fullsweep_after">> := 11,
                <<"minor_gcs">> := 17,
                <<"old_heap_size">> := 5
            },
            maps:get(<<"garbage_collection_info">>, Data)
        ),
        ?assert(type_key_present(maps:get(<<"links">>, Data))),
        ?assertEqual(30, length(maps:get(<<"links">>, Data))),
        ?assertEqual(35, maps:get(<<"links_total_count">>, Data)),
        ?assertEqual(true, maps:get(<<"links_truncated">>, Data)),
        ?assert(type_key_present(maps:get(<<"monitors">>, Data))),
        ?assertEqual(3, maps:get(<<"monitors_total_count">>, Data)),
        ?assertEqual(false, maps:get(<<"monitors_truncated">>, Data)),
        ?assert(type_key_present(maps:get(<<"monitored_by">>, Data))),
        ?assert(is_list(maps:get(<<"suspending">>, Data))),
        [Suspending] = maps:get(<<"suspending">>, Data),
        ?assertEqual(1, maps:get(<<"suspending_total_count">>, Data)),
        ?assertEqual(false, maps:get(<<"suspending_truncated">>, Data)),
        ?assertMatch(
            #{
                <<"target">> := <<"pid-", _/binary>>,
                <<"active_suspend_count">> := 1,
                <<"outstanding_suspend_count">> := 2
            },
            Suspending
        ),
        FirstStackFrame = hd(maps:get(<<"current_stacktrace">>, Data)),
        ?assertMatch(#{<<"arity">> := 2, <<"module">> := _, <<"function">> := _}, FirstStackFrame),
        ?assertMatch(<<"module-", _/binary>>, maps:get(<<"module">>, FirstStackFrame)),
        ?assertMatch(<<"function-", _/binary>>, maps:get(<<"function">>, FirstStackFrame)),
        ?assertNot(is_map_key(<<"raw">>, FirstStackFrame)),
        ?assertNot(is_map_key(<<"messages">>, Data)),
        ?assertNot(is_map_key(<<"dictionary">>, Data)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"secret">>)),
        receive
            {process_detail_keys, Keys} ->
                ?assert(not lists:member(messages, Keys)),
                ?assert(not lists:member(dictionary, Keys)),
                ?assert(not lists:member(state, Keys)),
                ?assert(lists:member(current_stacktrace, Keys))
        after 1000 ->
            ?assert(false)
        end
    after
        port_close(Port),
        exit(Pid, kill)
    end.

process_detail_collects_real_gc_tuning_test() ->
    Response = inspection(process, #{target => list_to_binary(pid_to_list(self()))}),
    GC = maps:get(<<"garbage_collection_info">>, maps:get(<<"data">>, Response)),
    ?assert(
        lists:all(
            fun(Key) -> is_integer(maps:get(Key, GC)) end,
            [
                <<"min_bin_vheap_size">>,
                <<"min_heap_size">>,
                <<"fullsweep_after">>,
                <<"minor_gcs">>
            ]
        )
    ).

port_inventory_aliases_and_missing_fields_test() ->
    Port = open_port({spawn, "cat"}, [binary]),
    try
        Values = #{
            name => "cat",
            connected => self(),
            queue_size => 1,
            memory => 2,
            id => 3,
            input => 4,
            output => 5
        },
        Source = #{
            count_fun => fun() -> 1 end,
            all_fun => fun() -> {ok, [Port]} end,
            info_fun => fun(_Item, Key) ->
                case maps:find(Key, Values) of
                    {ok, Value} -> {ok, Value};
                    error -> missing
                end
            end
        },
        Response = inspection_include(ports, #{test_port_source => Source}),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(9, maps:get(<<"tracked_field_count">>, Data)),
        [Item] = maps:get(<<"items">>, Data),
        ?assertEqual(maps:get(<<"name">>, Item), maps:get(<<"controls">>, Item)),
        ?assertEqual(maps:get(<<"display_id">>, Item), maps:get(<<"slot">>, Item)),
        ?assertEqual(null, maps:get(<<"parallelism">>, Item)),
        ?assertEqual(null, maps:get(<<"locking">>, Item)),
        ?assertEqual(
            [<<"parallelism">>, <<"locking">>],
            maps:get(<<"field_errors">>, Item)
        )
    after
        port_close(Port)
    end.

port_detail_is_bounded_and_redacts_inet_identifiers_test() ->
    Port = open_port({spawn, "cat"}, [binary]),
    try
        Stats = [
            {recv_oct, 1},
            {recv_cnt, 2},
            {recv_max, 3},
            {recv_avg, 4},
            {recv_dvi, 5},
            {send_oct, 6},
            {send_cnt, 7},
            {send_max, 8},
            {send_avg, 9},
            {send_pend, 10}
        ],
        Info = #{
            name => "tcp_inet",
            connected => self(),
            queue_size => 1,
            memory => 2,
            id => 3,
            input => 4,
            output => 5,
            parallelism => true,
            locking => port_level,
            os_pid => undefined,
            links => lists:duplicate(35, self()),
            monitors => [{process, self()}],
            monitored_by => [self()]
        },
        Source = #{
            info_fun => fun
                (_Item, os_pid) -> missing;
                (_Item, Key) -> {ok, maps:get(Key, Info)}
            end,
            sockname_fun => fun(_Item) -> {ok, {{127, 0, 0, 1}, 1883}} end,
            peername_fun => fun(_Item) -> {ok, {{10, 0, 0, 1}, 2883}} end,
            stat_fun => fun(_Item, _Keys) -> {ok, Stats} end,
            getopts_fun => fun
                (_Item, [linger]) -> {ok, [{linger, {true, 5}}]};
                (_Item, [bind_to_device]) -> {ok, [{bind_to_device, <<"en0">>}]};
                (_Item, [netns]) -> {ok, [{netns, <<"/var/run/netns/prod">>}]};
                (_Item, [packet]) -> {ok, [{packet, {unknown, term}}]};
                (_Item, [Option]) -> {ok, [{Option, false}]}
            end
        },
        Target = list_to_binary(port_to_list(Port)),
        Response = inspection(port, #{target => Target, test_port_source => Source}),
        ?assertEqual(
            ok, observer_cli_escriptize:validate_response(port, redact, node(), Response)
        ),
        lists:foreach(
            fun(Format) -> ?assertMatch({ok, _}, observer_cli_cli:encode(Format, Response)) end,
            [text, term, json]
        ),
        Data = maps:get(<<"data">>, Response),
        ?assertEqual(<<"running">>, maps:get(<<"status">>, Data)),
        ?assertMatch(<<"port-", _/binary>>, maps:get(<<"resource">>, Data)),
        ?assertEqual(30, length(maps:get(<<"links">>, Data))),
        ?assertEqual(35, maps:get(<<"links_total_count">>, Data)),
        ?assertEqual(true, maps:get(<<"links_truncated">>, Data)),
        Inet = maps:get(<<"inet">>, Data),
        ?assertMatch(<<"endpoint-", _/binary>>, maps:get(<<"sockname">>, Inet)),
        ?assertMatch(<<"endpoint-", _/binary>>, maps:get(<<"peername">>, Inet)),
        Statistics = maps:get(<<"statistics">>, Inet),
        ?assertEqual(10, maps:get(<<"send_pend">>, Statistics)),
        Options = maps:get(<<"options">>, Inet),
        ?assertEqual(31, length(Options)),
        ?assertMatch(
            #{<<"status">> := <<"available">>, <<"value">> := <<"interface-", _/binary>>},
            option_by_name(<<"bind_to_device">>, Options)
        ),
        ?assertMatch(
            #{<<"status">> := <<"available">>, <<"value">> := <<"netns-", _/binary>>},
            option_by_name(<<"netns">>, Options)
        ),
        ?assertEqual(<<"error">>, maps:get(<<"status">>, option_by_name(<<"packet">>, Options))),
        ?assertEqual(
            #{<<"enabled">> => true, <<"seconds">> => 5},
            maps:get(<<"value">>, option_by_name(<<"linger">>, Options))
        ),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"127.0.0.1">>)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Response), <<"/var/run/netns/prod">>))
    after
        port_close(Port)
    end.

port_detail_not_found_and_non_inet_test() ->
    ?assertEqual(
        <<"not_found">>,
        maps:get(<<"status">>, maps:get(<<"data">>, inspection(port, #{target => <<"port-1">>})))
    ),
    Port = open_port({spawn, "cat"}, [binary]),
    Target = list_to_binary(port_to_list(Port)),
    try
        Data = maps:get(<<"data">>, inspection_include(port, #{target => Target})),
        ?assertEqual(<<"not_inet">>, maps:get(<<"status">>, maps:get(<<"inet">>, Data)))
    after
        port_close(Port)
    end,
    Gone = maps:get(<<"data">>, inspection(port, #{target => Target})),
    ?assertEqual(<<"not_found">>, maps:get(<<"status">>, Gone)).

option_by_name(Name, Options) ->
    hd([Option || #{<<"name">> := OptionName} = Option <- Options, OptionName =:= Name]).

processes_duration_supports_all_window_sorts_test() ->
    WordSize = erlang:system_info(wordsize),
    lists:foreach(
        fun({Sort, Factor}) ->
            assert_process_window_sort(
                Sort,
                Factor
            )
        end,
        [
            {memory, 1},
            {message_queue_len, 1},
            {reductions, 1},
            {binary_memory, 1},
            {total_heap_size, WordSize}
        ]
    ).

assert_process_window_sort(Sort, Factor) ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    Key = process_window_key(Sort, delta),
    RateKey = process_window_key(Sort, per_second),
    try
        Source = #{
            count_fun => fun() -> 5 end,
            fold =>
                {fixture_window, fun(Fun, Acc) ->
                    Sample =
                        case get({process_window_sort, Sort}) of
                            undefined -> 1;
                            N -> N + 1
                        end,
                    put({process_window_sort, Sort}, Sample),
                    Current =
                        case Sample of
                            1 -> [Stable, LateHot, Reset, Dead];
                            2 -> [Stable, LateHot, Reset, Born]
                        end,
                    lists:foldl(Fun, Acc, Current)
                end},
            info_fun => fun(Pid, _Keys) ->
                Sample = get({process_window_sort, Sort}),
                Values0 = #{
                    Stable => 10,
                    LateHot => 11,
                    Reset => 100,
                    Dead => 1,
                    Born => 9999
                },
                Values1 = #{
                    Stable => 15,
                    LateHot => 1010,
                    Reset => 2,
                    Dead => 0,
                    Born => 9999
                },
                Value =
                    maps:get(
                        Pid,
                        case Sample of
                            1 -> Values0;
                            2 -> Values1
                        end
                    ),
                case Sort of
                    binary_memory ->
                        [{binary, [{erlang:make_ref(), Value, 1}]}];
                    _ ->
                        [{Sort, Value}]
                end
            end,
            sleep_fun => fun(_Duration) -> ok end,
            monotonic_fun => fun() ->
                case get({process_window_clock, Sort}) of
                    undefined ->
                        put({process_window_clock, Sort}, 250),
                        0;
                    N ->
                        N
                end
            end,
            whereis_fun => fun erlang:whereis/1,
            alive_fun => fun erlang:is_process_alive/1
        },
        Response = inspection_include(processes, #{
            sort => Sort, limit => 1, duration_ms => 250, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        [Item] = maps:get(<<"items">>, Data),
        Delta = 999 * Factor,
        ?assertEqual(list_to_binary(pid_to_list(LateHot)), maps:get(<<"pid">>, Item)),
        ?assertEqual(Delta, maps:get(Key, Item)),
        ?assertEqual(Delta * 1000 / 250, maps:get(RateKey, Item)),
        ?assertEqual(4, maps:get(<<"baseline_count">>, Data)),
        ?assertEqual(1, maps:get(<<"born_count">>, Data)),
        ?assertEqual(1, maps:get(<<"dead_count">>, Data)),
        ?assertEqual(1, maps:get(<<"reset_count">>, Data)),
        ?assertEqual(2, maps:get(<<"retained_sample_count">>, Data))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        erase({process_window_sort, Sort}),
        erase({process_window_clock, Sort})
    end.

process_window_key(memory, delta) -> <<"memory_delta">>;
process_window_key(message_queue_len, delta) -> <<"message_queue_len_delta">>;
process_window_key(reductions, delta) -> <<"reductions_delta">>;
process_window_key(binary_memory, delta) -> <<"binary_memory_delta">>;
process_window_key(total_heap_size, delta) -> <<"total_heap_size_delta">>;
process_window_key(memory, per_second) -> <<"memory_per_second">>;
process_window_key(message_queue_len, per_second) -> <<"message_queue_len_per_second">>;
process_window_key(reductions, per_second) -> <<"reductions_per_second">>;
process_window_key(binary_memory, per_second) -> <<"binary_memory_per_second">>;
process_window_key(total_heap_size, per_second) -> <<"total_heap_size_per_second">>.

stable_process_window_lifecycle_and_late_hot_test() ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    try
        Window = observer_cli_snapshot:stable_process_window(
            #{Stable => 10, LateHot => 10, Reset => 50, Dead => 1},
            #{Stable => 20, LateHot => 1010, Reset => 2, Born => 9999},
            250
        ),
        ?assertEqual(#{Stable => 10, LateHot => 1000}, maps:get(stable, Window)),
        ?assertEqual([Born], maps:get(born, Window)),
        ?assertEqual([Dead], maps:get(dead, Window)),
        ?assertEqual([Reset], maps:get(reset, Window))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

reduction_window_keeps_full_baseline_and_stable_pids_test() ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    try
        Source = #{
            count_fun => fun() -> 5 end,
            fold =>
                {fixture_window, fun(Fun, Acc) ->
                    Sample =
                        case get(goal08_sample) of
                            undefined -> 1;
                            N -> N + 1
                        end,
                    put(goal08_sample, Sample),
                    Current =
                        case Sample of
                            1 -> [Stable, LateHot, Reset, Dead];
                            2 -> [Stable, LateHot, Reset, Born]
                        end,
                    lists:foldl(Fun, Acc, Current)
                end},
            info_fun => fun(Pid, [reductions]) ->
                Sample = get(goal08_sample),
                Values =
                    case Sample of
                        1 -> #{Stable => 10, LateHot => 10, Reset => 50, Dead => 1};
                        2 -> #{Stable => 20, LateHot => 1010, Reset => 2, Born => 9999}
                    end,
                [{reductions, maps:get(Pid, Values)}]
            end,
            sleep_fun => fun(_Duration) -> ok end,
            monotonic_fun => fun() ->
                case get(goal08_clock) of
                    undefined ->
                        put(goal08_clock, 250),
                        0;
                    N ->
                        N
                end
            end,
            whereis_fun => fun erlang:whereis/1,
            alive_fun => fun erlang:is_process_alive/1
        },
        Response = inspection_include(processes, #{
            sort => reductions, limit => 1, duration_ms => 250, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        [Item] = maps:get(<<"items">>, Data),
        ?assertEqual(list_to_binary(pid_to_list(LateHot)), maps:get(<<"pid">>, Item)),
        ?assertEqual(1000, maps:get(<<"reductions_delta">>, Item)),
        ?assertEqual(4, maps:get(<<"baseline_count">>, Data)),
        ?assertEqual(1, maps:get(<<"born_count">>, Data)),
        ?assertEqual(1, maps:get(<<"dead_count">>, Data)),
        ?assertEqual(1, maps:get(<<"reset_count">>, Data)),
        ?assertEqual(2, maps:get(<<"retained_sample_count">>, Data)),
        ?assert(maps:get(<<"working_set_estimated_bytes">>, Data) > 0)
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

memory_window_keeps_full_baseline_and_stable_pids_test() ->
    Pids = [spawn(fun process_fixture/0) || _ <- lists:seq(1, 5)],
    [Stable, LateHot, Reset, Dead, Born] = Pids,
    try
        Source = #{
            count_fun => fun() -> 5 end,
            fold =>
                {fixture_window, fun(Fun, Acc) ->
                    Sample =
                        case get(goal12_sample) of
                            undefined -> 1;
                            N -> N + 1
                        end,
                    put(goal12_sample, Sample),
                    Current =
                        case Sample of
                            1 -> [Stable, LateHot, Reset, Dead];
                            2 -> [Stable, LateHot, Reset, Born]
                        end,
                    lists:foldl(Fun, Acc, Current)
                end},
            info_fun => fun(Pid, [memory]) ->
                Sample = get(goal12_sample),
                Values =
                    case Sample of
                        1 -> #{Stable => 10, LateHot => 11, Reset => 100, Dead => 1};
                        2 -> #{Stable => 15, LateHot => 1010, Reset => 2, Born => 9999}
                    end,
                [{memory, maps:get(Pid, Values)}]
            end,
            sleep_fun => fun(_Duration) -> ok end,
            monotonic_fun => fun() ->
                case get(goal12_clock) of
                    undefined ->
                        put(goal12_clock, 250),
                        0;
                    N ->
                        N
                end
            end,
            whereis_fun => fun erlang:whereis/1,
            alive_fun => fun erlang:is_process_alive/1
        },
        Response = inspection_include(processes, #{
            sort => memory, limit => 1, duration_ms => 250, test_process_source => Source
        }),
        Data = maps:get(<<"data">>, Response),
        [Item] = maps:get(<<"items">>, Data),
        ?assertEqual(list_to_binary(pid_to_list(LateHot)), maps:get(<<"pid">>, Item)),
        ?assertEqual(999, maps:get(<<"memory_delta">>, Item)),
        ?assertEqual(3996.0, maps:get(<<"memory_per_second">>, Item)),
        ?assertEqual(4, maps:get(<<"baseline_count">>, Data)),
        ?assertEqual(1, maps:get(<<"born_count">>, Data)),
        ?assertEqual(1, maps:get(<<"dead_count">>, Data)),
        ?assertEqual(1, maps:get(<<"reset_count">>, Data)),
        ?assertEqual(2, maps:get(<<"retained_sample_count">>, Data))
    after
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end.

process_scan_admission_refuses_before_enumeration_test() ->
    Parent = self(),
    Source = (process_source([], fun(_Pid, _Keys) -> undefined end))#{
        count_fun => fun() -> 100001 end,
        fold =>
            {must_not_scan, fun(_Fun, Acc) ->
                Parent ! scanned,
                Acc
            end}
    },
    Response = inspection(processes, #{sort => memory, test_process_source => Source}),
    Data = maps:get(<<"data">>, Response),
    ?assertEqual(<<"scan_budget_exceeded">>, maps:get(<<"reason_code">>, Data)),
    ?assertEqual(<<"pre_enumeration">>, maps:get(<<"admission_stage">>, Data)),
    receive
        scanned -> ?assert(false)
    after 50 -> ok
    end,
    [Probe] = maps:get(<<"probes">>, maps:get(<<"capture">>, Response)),
    ?assertEqual(<<"unavailable">>, maps:get(<<"status">>, Probe)),
    ?assertEqual(<<"complete">>, maps:get(<<"status">>, maps:get(<<"capture">>, Response))).

scheduler_window_invalidates_unsafe_samples_test() ->
    First = scheduler_sample_fixture(#{1 => {10, 20}, 3 => {5, 10}}, 0),
    Valid = observer_cli_snapshot:scheduler_window(
        First,
        scheduler_sample_fixture(#{1 => {20, 40}, 3 => {10, 20}}, 250)
    ),
    ?assertEqual(valid, maps:get(status, Valid)),
    ?assertEqual(0.5, maps:get(utilization_ratio, maps:get(normal, Valid))),
    assert_invalid_scheduler_window(
        topology_changed,
        observer_cli_snapshot:scheduler_window(
            First,
            (scheduler_sample_fixture(#{1 => {20, 40}, 3 => {10, 20}}, 250))#{
                topology => (maps:get(topology, First))#{schedulers_online => 2}
            }
        )
    ),
    assert_invalid_scheduler_window(
        missing_scheduler_id,
        observer_cli_snapshot:scheduler_window(
            First,
            scheduler_sample_fixture(#{1 => {20, 40}}, 250)
        )
    ),
    assert_invalid_scheduler_window(
        zero_denominator,
        observer_cli_snapshot:scheduler_window(First, First#{monotonic_ms => 250})
    ).

scheduler_wall_time_cleanup_is_paired_test() ->
    put(scheduler_flags, []),
    FlagFun = fun(Enabled) -> put(scheduler_flags, [Enabled | get(scheduler_flags)]) end,
    ?assertException(
        error,
        sample_failed,
        observer_cli_snapshot:measure_scheduler(
            250,
            FlagFun,
            fun() -> erlang:error(sample_failed) end,
            fun(_Duration) -> ok end
        )
    ),
    ?assertEqual([false, true], get(scheduler_flags)),
    CleanupFlagFun = fun
        (true) -> ok;
        (false) -> erlang:error(cleanup_failed)
    end,
    ?assertException(
        error,
        cleanup_failed,
        observer_cli_snapshot:measure_scheduler(
            250,
            CleanupFlagFun,
            fun() -> scheduler_sample_fixture(#{1 => {10, 20}, 3 => {5, 10}}, 0) end,
            fun(_Duration) -> ok end
        )
    ),
    erase(scheduler_flags).

distribution_controller_exclusion_and_capability_test() ->
    Controller = 'controller@host',
    Visible = 'visible@host',
    Hidden = 'hidden@host',
    Port = open_port({spawn, "cat"}, []),
    try
        Data = observer_cli_snapshot:distribution_context(
            Controller,
            [Hidden, Controller, Visible],
            [Controller, Visible],
            [Hidden],
            {ok, [{Visible, Port}, {Controller, Port}, {Hidden, alternative_carrier}]},
            {ok, 1048576},
            fun(P) -> erlang:port_info(P, queue_size) end
        ),
        ?assertEqual(2, maps:get(connected_peer_count, Data)),
        ?assertEqual(
            [#{peer => {identifier, peer, Controller}, reason => diagnostics_controller}],
            maps:get(excluded_peers, Data)
        ),
        [HiddenQueue, VisibleQueue] = maps:get(controller_queues, Data),
        ?assertEqual(unavailable, maps:get(status, HiddenQueue)),
        ?assertEqual(capability_unavailable, maps:get(reason_code, HiddenQueue)),
        ?assertEqual(available, maps:get(status, VisibleQueue)),
        ?assertEqual(unavailable, maps:get(health_inference, VisibleQueue)),
        Unavailable = observer_cli_snapshot:distribution_context(
            undefined,
            [Visible],
            [Visible],
            [],
            {unavailable, capability_unavailable},
            {unavailable, capability_unavailable},
            fun(_P) -> erlang:error(unexpected_port_info) end
        ),
        ?assertEqual(
            #{status => unavailable, reason_code => capability_unavailable},
            maps:get(controller_queue_capability, Unavailable)
        ),
        [UnavailableQueue] = maps:get(controller_queues, Unavailable),
        ?assertEqual(capability_unavailable, maps:get(reason_code, UnavailableQueue))
    after
        port_close(Port)
    end.

normalization_and_identifier_policy_test() ->
    Reference = make_ref(),
    Raw = #{
        node => {identifier, node, 'target@host'},
        pid => self(),
        same_pid => self(),
        reference => Reference,
        table => {identifier, table, Reference},
        socket => {identifier, socket, {'$socket', Reference}},
        mfa => {mfa, observer_cli_snapshot, capabilities, 0},
        values => [1, 1.5, true, false, null, value]
    },
    {ok, Redacted} = observer_cli_snapshot:normalize(Raw, redact),
    ?assertEqual(<<"node-1">>, maps:get(<<"node">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"pid">>, Redacted)),
    ?assertEqual(<<"pid-1">>, maps:get(<<"same_pid">>, Redacted)),
    ?assertEqual(<<"ref-1">>, maps:get(<<"reference">>, Redacted)),
    ?assertEqual(<<"table-1">>, maps:get(<<"table">>, Redacted)),
    ?assertEqual(<<"socket-1">>, maps:get(<<"socket">>, Redacted)),
    ?assertEqual(
        #{<<"module">> => <<"module-1">>, <<"function">> => <<"function-1">>, <<"arity">> => 0},
        maps:get(<<"mfa">>, Redacted)
    ),
    assert_json_safe(Redacted),
    {ok, Included} = observer_cli_snapshot:normalize(Raw, include),
    ?assertEqual(<<"target@host">>, maps:get(<<"node">>, Included)),
    ?assertEqual(list_to_binary(pid_to_list(self())), maps:get(<<"pid">>, Included)),
    ?assertEqual(
        #{
            <<"module">> => <<"observer_cli_snapshot">>,
            <<"function">> => <<"capabilities">>,
            <<"arity">> => 0
        },
        maps:get(<<"mfa">>, Included)
    ).

invalid_utf8_and_field_cap_test() ->
    {ok, Tagged} = observer_cli_snapshot:normalize(<<16#FF, 0, 16#FE>>, include),
    ?assertEqual(<<"base64">>, maps:get(<<"encoding">>, Tagged)),
    ?assertEqual(<<16#FF, 0, 16#FE>>, base64:decode(maps:get(<<"data">>, Tagged))),
    ?assertEqual(
        {error, field_too_large},
        observer_cli_snapshot:normalize(binary:copy(<<"x">>, 64 * 1024 + 1), include)
    ),
    ?assertEqual(
        {error, field_too_large},
        observer_cli_snapshot:normalize(binary:copy(<<16#ff>>, 64 * 1024), include)
    ).

normalization_edge_contract_test() ->
    Port = open_port({spawn, "cat"}, []),
    Reference = make_ref(),
    try
        Values = #{
            port => Port,
            table_integer => {identifier, table, 42},
            socket_reference => {identifier, socket, Reference},
            child => {identifier, child, <<"worker">>},
            endpoint => {identifier, endpoint, <<"127.0.0.1:1883">>},
            interface => {identifier, interface, en0},
            netns => {identifier, netns, <<"default">>},
            application => {identifier, application, kernel}
        },
        {ok, Included} = observer_cli_snapshot:normalize(Values, include),
        ?assertEqual(list_to_binary(port_to_list(Port)), maps:get(<<"port">>, Included)),
        ?assertEqual(<<"42">>, maps:get(<<"table_integer">>, Included)),
        {ok, Redacted} = observer_cli_snapshot:normalize(Values, redact),
        ?assertEqual(<<"port-1">>, maps:get(<<"port">>, Redacted))
    after
        port_close(Port)
    end,
    lists:foreach(
        fun(Term) ->
            ?assertEqual({error, invalid_schema}, observer_cli_snapshot:normalize(Term, include))
        end,
        [
            #{foo => 1, <<"foo">> => 2},
            #{1 => value},
            [head | tail],
            {mfa, erlang, node, -1},
            fun() -> ok end
        ]
    ),
    ?assertEqual(
        {error, invalid_identifier},
        observer_cli_snapshot:normalize({identifier, unknown, value}, include)
    ),
    ?assertEqual(
        {error, invalid_identifier_policy}, observer_cli_snapshot:normalize(#{}, invalid)
    ),
    Deep = lists:foldl(fun(_, Acc) -> [Acc] end, value, lists:seq(1, 34)),
    ?assertEqual({error, response_too_deep}, observer_cli_snapshot:normalize(Deep, include)).

evidence_pointer_contract_test() ->
    ?assertEqual({ok, #{}}, observer_cli_snapshot:truncate(#{})),
    lists:foreach(
        fun(Response) ->
            ?assertEqual(
                {error, invalid_evidence_pointer}, observer_cli_snapshot:truncate(Response)
            )
        end,
        [
            #{<<"evidence">> => invalid},
            #{<<"evidence">> => [#{}]},
            #{<<"evidence">> => [#{<<"path">> => 1}]},
            #{<<"evidence">> => [#{<<"path">> => <<"missing-slash">>}]},
            #{<<"evidence">> => [#{<<"path">> => <<"/~2">>}]},
            #{<<"items">> => [], <<"evidence">> => [#{<<"path">> => <<"/items/1">>}]},
            #{<<"items">> => value, <<"evidence">> => [#{<<"path">> => <<"/items/0">>}]},
            #{<<"items">> => [value], <<"evidence">> => [#{<<"path">> => <<"/items/00">>}]},
            #{<<"items">> => [value], <<"evidence">> => [#{<<"path">> => <<"/items/nope">>}]}
        ]
    ),
    Escaped = #{
        <<"a/b">> => #{<<"~key">> => value},
        <<"evidence">> => [#{<<"path">> => <<"/a~1b/~0key">>}]
    },
    ?assertEqual({ok, Escaped}, observer_cli_snapshot:truncate(Escaped)).

dispatch_success_and_schema_failures_test() ->
    ?assertMatch(
        #{
            <<"status">> := <<"ok">>,
            <<"result">> := #{<<"pid">> := <<"pid-1">>},
            <<"cleanup_confirmed">> := true
        },
        dispatch_observed(#{pid => self()}, 2000, redact)
    ),
    assert_error(
        <<"field_too_large">>,
        dispatch_observed(binary:copy(<<"x">>, 64 * 1024 + 1), 2000, include)
    ),
    Oversized = #{
        required => lists:duplicate(20, binary:copy(<<"x">>, 60 * 1024))
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(Oversized, 3000, include)
    ),
    assert_error(
        <<"invalid_evidence_pointer">>,
        dispatch_observed(
            #{data => #{value => 1}, findings => [#{evidence => [#{path => <<"/data/missing">>}]}]},
            2000,
            include
        )
    ).

evidence_preserving_truncation_test() ->
    Item = #{value => binary:copy(<<"x">>, 60 * 1024)},
    Items = lists:duplicate(20, Item),
    Report = #{
        items => Items,
        returned_count => 20,
        dropped_count => 0,
        truncated => false,
        findings => [#{evidence => [#{path => <<"/items/0/value">>}]}]
    },
    #{<<"status">> := <<"ok">>, <<"result">> := Result} = dispatch_observed(
        Report, 3000, include
    ),
    ResultItems = maps:get(<<"items">>, Result),
    ?assert(length(ResultItems) < 20),
    ?assertEqual(true, maps:get(<<"truncated">>, Result)),
    ?assertEqual(length(ResultItems), maps:get(<<"returned_count">>, Result)),
    ?assertEqual(20 - length(ResultItems), maps:get(<<"dropped_count">>, Result)),
    ?assertMatch(#{<<"value">> := _}, hd(ResultItems)),
    ?assert(erlang:external_size(Result) =< 1024 * 1024),
    ProtectedTail = Report#{
        findings := [#{evidence => [#{path => <<"/items/19/value">>}]}]
    },
    assert_error(
        <<"response_too_large">>,
        dispatch_observed(ProtectedTail, 3000, include)
    ).

timeout_crash_and_heap_cleanup_test_() ->
    {timeout, 10, fun timeout_crash_and_heap_cleanup/0}.

timeout_crash_and_heap_cleanup() ->
    TimeoutResult = observer_cli_snapshot:dispatch(
        self(), test_timeout, self(), options(1050, include)
    ),
    TimeoutWorker = receive_worker(),
    assert_error(<<"target_timeout">>, TimeoutResult),
    ?assertNot(is_process_alive(TimeoutWorker)),
    CrashResult = observer_cli_snapshot:dispatch(
        self(), test_crash, self(), options(2000, include)
    ),
    CrashWorker = receive_worker(),
    assert_error(<<"probe_failed">>, CrashResult),
    ?assertNot(is_process_alive(CrashWorker)),
    ?assertEqual(nomatch, binary:match(term_to_binary(CrashResult), <<"fixture_secret">>)),
    HeapResult = observer_cli_snapshot:dispatch(
        self(), test_heap, self(), (options(3000, include))#{max_heap_words => 4096}
    ),
    HeapWorker = receive_worker(),
    assert_error(<<"worker_heap_limit_exceeded">>, HeapResult),
    ?assertNot(is_process_alive(HeapWorker)).

controller_disconnect_cleanup_test_() ->
    {timeout, 10, fun controller_disconnect_cleanup/0}.

controller_disconnect_cleanup() ->
    Parent = self(),
    Controller = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Coordinator = spawn(fun() ->
        Result = observer_cli_snapshot:dispatch(
            Controller, test_timeout, Parent, options(5000, include)
        ),
        Parent ! {dispatch_result, self(), Result}
    end),
    CoordinatorRef = erlang:monitor(process, Coordinator),
    Worker = receive_worker(),
    exit(Controller, kill),
    Result =
        receive
            {dispatch_result, Coordinator, DispatchResult} -> DispatchResult
        after 2000 ->
            erlang:error(dispatch_timeout)
        end,
    assert_error(<<"controller_disconnected">>, Result),
    receive
        {'DOWN', CoordinatorRef, process, Coordinator, normal} -> ok
    after 2000 ->
        erlang:error(coordinator_cleanup_timeout)
    end,
    ?assertNot(is_process_alive(Worker)),
    ?assertNot(is_process_alive(Coordinator)).

options(Timeout, Policy) ->
    #{timeout_ms => Timeout, identifier_policy => Policy}.

dispatch_observed(Request, Timeout, Policy) ->
    Result = observer_cli_snapshot:dispatch(
        self(), test_observed_echo, {self(), Request}, options(Timeout, Policy)
    ),
    Worker = receive_worker(),
    ?assertNot(is_process_alive(Worker)),
    Result.

snapshot(Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            snapshot,
            Request,
            options(3000, redact)
        ),
    Response.

assert_count_within(First, Second, Tolerance) ->
    ?assert(abs(First - Second) =< Tolerance).

inspection(Command, Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(),
            Command,
            Request,
            options(3000, redact)
        ),
    Response.

inspection_include(Command, Request) ->
    #{<<"status">> := <<"ok">>, <<"result">> := Response} =
        observer_cli_snapshot:dispatch(
            self(), Command, Request, options(3000, include)
        ),
    Response.

process_source(Pids, InfoFun) ->
    #{
        count_fun => fun() -> length(Pids) end,
        fold => {fixture_list, fun(Fun, Acc) -> lists:foldl(Fun, Acc, Pids) end},
        info_fun => InfoFun,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end,
        whereis_fun => fun erlang:whereis/1,
        alive_fun => fun erlang:is_process_alive/1
    }.

process_info_fixture(_Pid, Keys, Metric) ->
    Values = #{
        registered_name => [],
        current_function => {?MODULE, process_fixture, 0},
        initial_call => {?MODULE, process_fixture, 0},
        memory => Metric,
        message_queue_len => Metric,
        reductions => Metric,
        total_heap_size => Metric
    },
    [{Key, maps:get(Key, Values)} || Key <- Keys].

process_detail_info(Keys, Port, Ref) ->
    Values = #{
        registered_name => [],
        current_function =>
            {observer_cli_snapshot_test, process_detail_collects_tui_field_gap_items_test, 0},
        initial_call =>
            {observer_cli_snapshot_test, process_detail_collects_tui_field_gap_items_test, 0},
        status => running,
        memory => 6,
        message_queue_len => 1,
        reductions => 2,
        heap_size => 16,
        total_heap_size => 32,
        stack_size => 8,
        group_leader => self(),
        binary => [
            {Ref, 64, 1},
            {make_ref(), 64, 2}
        ],
        garbage_collection => [
            {min_bin_vheap_size, 2},
            {min_heap_size, 3},
            {fullsweep_after, 11},
            {minor_gcs, 17}
        ],
        garbage_collection_info => [
            {old_heap_size, 5}
        ],
        priority => normal,
        links =>
            [self(), {process, self()}, {process, {a, node()}}, {port, Port}, {unknown, Ref}] ++
            lists:duplicate(30, self()),
        monitors => [{process, self()}, self(), {port, Port}],
        monitored_by => [{process, {b, node()}}, self()],
        catchlevel => 1,
        suspending => [{self(), 1, 2}],
        error_handler => goal12_error_handler,
        trap_exit => true,
        current_stacktrace => [
            {observer_cli_snapshot_test, process_detail_info, 2, [{file, "test.erl"}, {line, 1}]},
            {observer_cli_snapshot_test, process_detail_info, [secret, Ref], []},
            {erlang, apply, 3, []}
        ]
    },
    [{Key, maps:get(Key, Values)} || Key <- Keys].

type_key_present(List) when is_list(List) ->
    lists:all(
        fun
            (Item) when is_map(Item) ->
                maps:is_key(<<"type">>, Item);
            (_) ->
                false
        end,
        List
    );
type_key_present(_List) ->
    false.

process_fixture() ->
    receive
        stop -> ok
    end.

ref_to_binary(Reference) ->
    list_to_binary(ref_to_list(Reference)).

scheduler_sample_fixture(Wall, Monotonic) ->
    #{
        topology => #{
            schedulers_configured => 2,
            schedulers_online => 1,
            dirty_cpu_schedulers_configured => 1,
            dirty_cpu_schedulers_online => 1
        },
        wall_time => [{Id, Active, Total} || {Id, {Active, Total}} <- maps:to_list(Wall)],
        run_queue_lengths => [0, 0, 0],
        monotonic_ms => Monotonic
    }.

assert_invalid_scheduler_window(Reason, Window) ->
    ?assertEqual(invalid, maps:get(status, Window)),
    ?assertEqual(Reason, maps:get(reason_code, Window)),
    ?assertEqual(opaque_same_window, maps:get(wall_time_unit, Window)),
    ?assertEqual(false, maps:get(run_queue_snapshot_atomic, Window)).

assert_probe(Id, Required, Status, Capture) ->
    Probes = maps:get(<<"probes">>, Capture),
    [Probe] = [Item || #{<<"id">> := ProbeId} = Item <- Probes, ProbeId =:= Id],
    ?assertMatch(
        #{
            <<"required">> := Required,
            <<"status">> := Status,
            <<"reason_code">> := _,
            <<"duration_ms">> := _,
            <<"samples">> := _,
            <<"coverage">> := _
        },
        Probe
    ).

trace_forwarder(Parent) ->
    receive
        {trace, _Pid, call, Call} ->
            Parent ! {enumerator_called, Call},
            trace_forwarder(Parent);
        _Other ->
            trace_forwarder(Parent)
    end.

state_trace_forwarder(Parent) ->
    receive
        {trace, _Pid, call, {sys, get_state, _Arguments}} ->
            Parent ! state_requested,
            state_trace_forwarder(Parent);
        _Other ->
            state_trace_forwarder(Parent)
    end.

state_fixture_response(State) ->
    Server = start_state_server(State),
    try
        inspection(gen_server_state, #{target => list_to_binary(pid_to_list(Server))})
    after
        exit(Server, kill)
    end.

start_state_server(State) ->
    {ok, Server} = gen_server:start_link(?MODULE, State, []),
    unlink(Server),
    Server.

state_source(GetStateFun) ->
    #{
        process_source => process_source([], fun(_Pid, _Keys) -> undefined end),
        get_state_fun => GetStateFun
    }.

init(State) ->
    {ok, State}.

handle_call({block, Parent}, _From, State) ->
    Parent ! {server_blocked, self()},
    receive
        release -> {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

receive_worker() ->
    receive
        {test_worker, Worker} -> Worker
    after 2000 ->
        erlang:error(worker_start_timeout)
    end.

assert_error(ReasonCode, Result) ->
    ?assertEqual(
        #{
            <<"status">> => <<"error">>,
            <<"reason_code">> => ReasonCode,
            <<"cleanup_confirmed">> => true
        },
        Result
    ).

assert_json_safe(Map) when is_map(Map) ->
    lists:foreach(
        fun({Key, Value}) ->
            ?assert(is_binary(Key)),
            assert_json_safe(Value)
        end,
        maps:to_list(Map)
    );
assert_json_safe(List) when is_list(List) ->
    lists:foreach(fun assert_json_safe/1, List);
assert_json_safe(Value) when is_binary(Value); is_integer(Value); is_float(Value) ->
    ok;
assert_json_safe(Value) when Value =:= true; Value =:= false; Value =:= null ->
    ok.

socket_trend_stable_reset_and_shape_test() ->
    Socket = make_ref(),
    First = #{Socket => socket_trend_item(Socket, 10, false)},
    Second = #{Socket => socket_trend_item(Socket, 20, false)},
    Stable = observer_cli_snapshot:diagnostic_socket_trend([First, Second]),
    [StableItem] = maps:get(items, Stable),
    ?assertEqual(20, maps:get(io, StableItem)),
    Reset = observer_cli_snapshot:diagnostic_socket_trend([
        First, #{Socket => socket_trend_item(Socket, 5, false)}, Second
    ]),
    [ResetItem] = maps:get(items, Reset),
    ?assertEqual(counter_reset, maps:get(state, ResetItem)),
    Shape = observer_cli_snapshot:diagnostic_socket_trend([
        First, #{Socket => socket_trend_item(Socket, 20, true)}
    ]),
    [ShapeItem] = maps:get(items, Shape),
    ?assertEqual(shape_change, maps:get(state, ShapeItem)).

socket_present_invalid_optional_counter_is_not_zero_test() ->
    Metrics = observer_cli_snapshot:socket_metrics(#{
        read_byte => 1,
        write_byte => 2,
        sendfile_byte => invalid,
        read_pkg => 1,
        write_pkg => 1,
        acc_waits => 1,
        read_waits => 1,
        write_waits => 1,
        acc_fails => 1,
        read_fails => 1,
        write_fails => 1
    }),
    ?assertEqual(invalid_optional, maps:get(status, maps:get(io, Metrics))),
    ?assertEqual(invalid_optional, maps:get(status, maps:get(write_bytes, Metrics))).

socket_p0_fields_keep_current_metadata_and_window_semantics_test() ->
    Socket = make_ref(),
    First = socket_parity_item(Socket, self(), [bound], 2, 10),
    Second = socket_parity_item(Socket, self(), [connected], 5, 20),
    Window = observer_cli_snapshot:counter_window(
        sockets, #{Socket => First}, #{Socket => Second}
    ),
    [WindowItem] = maps:get(items, Window),
    ?assertEqual(6, maps:get(accepts, WindowItem)),
    ?assertEqual(20, maps:get(max_packet, WindowItem)),
    ?assertEqual([connected], maps:get(rstate, WindowItem)),
    ?assertEqual({identifier, pid, self()}, maps:get(owner, WindowItem)),
    Source = #{
        available_fun => fun() -> true end,
        count_fun => fun() -> 1 end,
        global_fun => fun() -> #{use_registry => true} end,
        all_fun => fun() -> {ok, [Socket]} end,
        info_fun => fun(Requested) when Requested =:= Socket ->
            (maps:without([raw_id, resource, counter_shape], Second))#{owner => self()}
        end,
        sleep_fun => fun(_Duration) -> ok end,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    },
    Data = maps:get(
        <<"data">>,
        inspection(sockets, #{sort => io, limit => 1, test_socket_source => Source})
    ),
    [Item] = maps:get(<<"items">>, Data),
    ?assertEqual(<<"pid-1">>, maps:get(<<"owner">>, Item)),
    ?assertEqual([<<"connected">>], maps:get(<<"rstate">>, Item)),
    ?assertEqual(10, maps:get(<<"accepts">>, Item)),
    ?assertEqual(20, maps:get(<<"max_packet">>, Item)),
    States = maps:get(<<"metric_states">>, Item),
    ?assertEqual(<<"available">>, maps:get(<<"accepts">>, States)),
    ?assertEqual(<<"available">>, maps:get(<<"max_packet">>, States)).

diagnostic_inventory_admission_uses_mode_sample_count_test() ->
    Request = diagnostic_inventory_request(),
    Context = #{controller => self()},
    Quick = observer_cli_snapshot:diagnostic_sample(Request#{sample_index => 0}, Context),
    Observe = observer_cli_snapshot:diagnostic_sample(Request#{observe => <<"5s">>}, Context),
    Deep = observer_cli_snapshot:diagnostic_sample(
        Request#{observe => <<"5s">>, deep => true}, Context
    ),
    lists:foreach(
        fun(Field) ->
            ?assertEqual(ok, maps:get(status, maps:get(Field, Quick))),
            ?assertEqual(ok, maps:get(status, maps:get(Field, Observe))),
            ?assertEqual(scan_budget_exceeded, maps:get(reason_code, maps:get(Field, Deep)))
        end,
        [ets_inventory, port_inventory]
    ),
    ?assertEqual(
        observation_not_requested,
        maps:get(
            reason_code, maps:get(socket_inventory, Quick)
        )
    ),
    ?assertEqual(ok, maps:get(status, maps:get(socket_inventory, Observe))),
    ?assertEqual(
        scan_budget_exceeded,
        maps:get(
            reason_code, maps:get(socket_inventory, Deep)
        )
    ).

diagnostic_ets_replacement_during_sample_is_dropped_test() ->
    FirstGeneration = make_ref(),
    SecondGeneration = make_ref(),
    put(diagnostic_ets_info_calls, 0),
    Info = fun(_Table, Key) ->
        Call = get(diagnostic_ets_info_calls),
        put(diagnostic_ets_info_calls, Call + 1),
        case {Call, Key} of
            {0, id} -> FirstGeneration;
            {1, size} -> 10;
            {2, memory} -> 20;
            {3, id} -> SecondGeneration
        end
    end,
    Request = (diagnostic_inventory_request())#{
        sample_index => 0,
        test_ets_source => #{
            count_fun => fun() -> 1 end,
            all_fun => fun() -> [named_table] end,
            info_fun => Info,
            word_size_fun => fun() -> erlang:system_info(wordsize) end
        }
    },
    Sample = observer_cli_snapshot:diagnostic_sample(Request, #{controller => self()}),
    ?assertEqual(#{}, maps:get(values, maps:get(ets_inventory, Sample))).

diagnostic_port_uses_default_source_result_shape_test() ->
    Port = open_port({spawn, "cat"}, []),
    try
        Request = (diagnostic_inventory_request())#{
            observe => <<"5s">>,
            test_port_source => #{
                count_fun => fun() -> 1 end,
                all_fun => fun() -> {ok, [Port]} end,
                info_fun => fun(_Item, Key) ->
                    {ok, maps:get(Key, #{queue_size => 9, memory => 8, input => 7, output => 6})}
                end
            }
        },
        Sample = observer_cli_snapshot:diagnostic_sample(Request, #{controller => self()}),
        Values = maps:get(values, maps:get(port_inventory, Sample)),
        ?assertEqual(
            #{queue_size => 9, memory => 8, input => 7, output => 6},
            maps:get(Port, Values)
        )
    after
        port_close(Port)
    end.

diagnostic_runtime_failure_paths_test() ->
    BadProcess = (process_source([], fun(_Pid, _Keys) -> undefined end))#{
        count_fun => fun() -> erlang:error(process_count_failed) end
    },
    PortError = #{
        count_fun => fun() -> 1 end,
        all_fun => fun() -> {error, port_registry_failed} end,
        info_fun => fun(_Port, _Key) -> missing end
    },
    Base = (diagnostic_inventory_request())#{
        observe => <<"5s">>,
        test_process_source => BadProcess,
        test_port_source => PortError,
        test_socket_source => #{available_fun => fun() -> false end}
    },
    Sample = observer_cli_snapshot:diagnostic_sample(Base, #{controller => self()}),
    ?assertEqual(
        process_inventory_failed, maps:get(reason_code, maps:get(process_inventory, Sample))
    ),
    ?assertEqual(port_inventory_failed, maps:get(reason_code, maps:get(port_inventory, Sample))),
    ?assertEqual(capability_unavailable, maps:get(reason_code, maps:get(socket_inventory, Sample))),
    SocketError = #{
        available_fun => fun() -> true end,
        count_fun => fun() -> 0 end,
        global_fun => fun() -> #{} end,
        all_fun => fun() -> {error, socket_registry_failed} end,
        info_fun => fun(_Socket) -> #{} end,
        monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
    },
    FailedSocket = observer_cli_snapshot:diagnostic_sample(
        Base#{test_socket_source := SocketError}, #{controller => self()}
    ),
    ?assertEqual(
        socket_registry_failed, maps:get(reason_code, maps:get(socket_inventory, FailedSocket))
    ).

diagnostic_inventory_request() ->
    #{
        test_process_source => process_source([], fun(_Pid, _Keys) -> undefined end),
        test_ets_source => #{
            count_fun => fun() -> 60000 end,
            all_fun => fun() -> [] end,
            info_fun => fun(_Table, _Key) -> undefined end,
            word_size_fun => fun() -> erlang:system_info(wordsize) end
        },
        test_port_source => #{
            count_fun => fun() -> 40000 end,
            all_fun => fun() -> {ok, []} end,
            info_fun => fun(_Port, _Key) -> missing end
        },
        test_socket_source => #{
            available_fun => fun() -> true end,
            count_fun => fun() -> 10000 end,
            global_fun => fun() -> #{use_registry => true} end,
            all_fun => fun() -> {ok, []} end,
            info_fun => fun(_Socket) -> #{} end,
            sleep_fun => fun(_Duration) -> ok end,
            monotonic_fun => fun() -> erlang:monotonic_time(millisecond) end
        }
    }.

socket_trend_item(Socket, Value, Sendfile) ->
    Counters0 = #{
        read_byte => Value,
        write_byte => Value,
        read_pkg => Value,
        write_pkg => Value,
        acc_waits => Value,
        read_waits => Value,
        write_waits => Value,
        acc_fails => Value,
        read_fails => Value,
        write_fails => Value
    },
    Counters =
        case Sendfile of
            true -> Counters0#{sendfile_byte => Value};
            false -> Counters0
        end,
    #{
        raw_id => Socket,
        resource => {identifier, socket, Socket},
        counters => Counters,
        counter_shape => lists:sort(maps:keys(Counters))
    }.

socket_parity_item(Socket, Owner, RState, Value, MaxPacket) ->
    Counters = #{
        read_byte => Value,
        write_byte => Value,
        read_pkg => Value,
        write_pkg => Value,
        read_pkg_max => MaxPacket - 1,
        write_pkg_max => MaxPacket,
        acc_success => Value,
        acc_tries => Value,
        acc_waits => Value,
        read_waits => Value,
        write_waits => Value,
        acc_fails => Value,
        read_fails => Value,
        write_fails => Value
    },
    #{
        raw_id => Socket,
        resource => {identifier, socket, Socket},
        owner => {identifier, pid, Owner},
        domain => inet,
        type => stream,
        protocol => tcp,
        rstate => RState,
        wstate => [],
        counters => Counters,
        counter_shape => lists:sort(maps:keys(Counters))
    }.

-endif.
