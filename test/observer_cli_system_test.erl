-module(observer_cli_system_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").
-compile(nowarn_untyped_record).
-include_lib("kernel/include/net_address.hrl").

start_manager_branches_test() ->
    observer_cli_test_io:with_input(
        ["2000\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_system:start(Opts))
        end
    ).

start_manager_unknown_test() ->
    observer_cli_test_io:with_input(
        ["x\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_system:start(Opts))
        end
    ).

info_fields_test() ->
    {Info, Stat} = observer_cli_system:info_fields(),
    ?assertEqual(2, length(Info)),
    ?assertEqual(2, length(Stat)).

to_list_test() ->
    ?assertEqual("10", observer_cli_system:to_list(10)),
    ?assertEqual("ok", observer_cli_system:to_list(ok)),
    ?assertEqual("1.0000 KiB", lists:flatten(observer_cli_system:to_list({bytes, 1024}))).

fill_info_test() ->
    Data = [{a, 1}, {b, 1024}, {dyn, {"Dyn", 2}}],
    Fields = [
        {dynamic, dyn},
        {"A", a},
        {"Attr", bold, a},
        {"B", {bytes, b}},
        {"AttrBytes", bold, {bytes, b}},
        {"Group", [{"A2", a}]}
    ],
    Result = observer_cli_system:fill_info(Fields, Data),
    ?assertEqual({"Dyn", 2}, lists:nth(1, Result)),
    ?assertEqual({"A", 1}, lists:nth(2, Result)),
    ?assertEqual({"Attr", bold, 1}, lists:nth(3, Result)),
    ?assertEqual({"AttrBytes", bold, {bytes, 1024}}, lists:nth(5, Result)).

fill_info_undefined_test() ->
    Data = [{present, 1}],
    Fields = [
        {dynamic, missing_dyn},
        {"Static", missing_static},
        {"StaticFormat", {bytes, missing_bytes}},
        {"Attr", bold, missing_attr},
        {"Format", {bytes, missing_format}},
        {"AttrFormat", bold, {bytes, missing_attr_format}},
        {"Group", [{"Nested", missing_nested}]}
    ],
    Result = observer_cli_system:fill_info(Fields, Data),
    ?assertEqual(undefined, lists:nth(1, Result)),
    ?assertEqual(undefined, lists:nth(2, Result)),
    ?assertEqual(undefined, lists:nth(3, Result)).

get_cachehit_info_test() ->
    CacheHitInfo = [
        {{instance, 1}, [{hit_rate, 0.5}, {hits, 5}, {calls, 10}]}
    ],
    {SeqStr, Hit, Call, HitRateStr} = observer_cli_system:get_cachehit_info(1, CacheHitInfo),
    ?assert(string:find(SeqStr, "01|") =/= nomatch),
    ?assertEqual("5", Hit),
    ?assertEqual("10", Call),
    ?assertEqual("50.00%", lists:flatten(HitRateStr)).

render_sys_info_test() ->
    System = [
        {"System Version", "A"},
        {"Erts Version", "B"},
        {"Compiled for", "C"},
        {"Emulator Wordsize", 8},
        {"Process Wordsize", 8},
        {"Smp Support", true},
        {"Thread Support", true},
        {"Async thread pool size", 2}
    ],
    CPU = [
        {"Logical CPU's", 1},
        {"Online Logical CPU's", 1},
        {"Available Logical CPU's", 1},
        {"Schedulers", 1},
        {"Online schedulers", 1},
        {"Available schedulers", 1}
    ],
    Memory = [
        {"Total", {bytes, 100}},
        {"Processes", {bytes, 10}},
        {"Atoms", {bytes, 5}},
        {"Binaries", {bytes, 2}},
        {"Code", {bytes, 3}},
        {"Ets", {bytes, 4}}
    ],
    Statistics = [
        {"ps -o pcpu", "1%"},
        {"ps -o pmem", "2%"},
        {"ps -o rss", {bytes, 3}},
        {"ps -o vsz", {bytes, 4}},
        {"Total IOIn", {bytes, 5}},
        {"Total IOOut", {bytes, 6}}
    ],
    Line = observer_cli_system:render_sys_info(System, CPU, Memory, Statistics),
    ?assert(string:find(lists:flatten(Line), "System/Architecture") =/= nomatch).

render_sys_info_wide_layout_test() ->
    Base = sys_info_widths(80),
    Wide = sys_info_widths(180),
    {BaseTitle, BaseRow, BaseCompile} = Base,
    {WideTitle, WideRow, WideCompile} = Wide,
    ?assertEqual([1, 3, 5, 7], same_columns(BaseTitle, WideTitle, [1, 3, 5, 7])),
    ?assertEqual([2, 4, 6, 8], wider_columns(BaseTitle, WideTitle, [2, 4, 6, 8])),
    ?assertEqual([1, 3, 5, 7, 8], same_columns(BaseRow, WideRow, [1, 3, 5, 7, 8])),
    ?assertEqual([2, 4, 6, 9], wider_columns(BaseRow, WideRow, [2, 4, 6, 9])),
    ?assertEqual(lists:nth(1, BaseCompile), lists:nth(1, WideCompile)),
    ?assert(lists:nth(2, WideCompile) > lists:nth(2, BaseCompile)).

render_sys_info_empty_ps_test() ->
    Line = observer_cli_system:render_sys_info(
        observer_cli_system:collect_sys_info(unavailable_metrics())
    ),
    ?assert(string:find(lists:flatten(Line), "System/Architecture") =/= nomatch).

render_sys_info_runtime_limits_test() ->
    observer_cli_test_io:with_geometry(
        24,
        201,
        [],
        fun() ->
            Line = observer_cli_system:render_sys_info(
                observer_cli_system:collect_sys_info(metrics_fixture())
            ),
            Output = lists:flatten(Line),
            ?assert(string:find(Output, "System Statistics / Limit") =/= nomatch),
            ?assert(string:find(Output, "Dist busy limit (bytes)") =/= nomatch),
            ?assert(string:find(Output, "Dirty CPU schedulers") =/= nomatch),
            ?assert(string:find(Output, "Modules") =/= nomatch),
            ?assertEqual(nomatch, string:find(Output, "Up time")),
            ?assert(string:find(Output, "% used") =/= nomatch),
            ?assert(
                lists:all(fun(Width) -> Width =< 200 end, observer_cli_test_io:line_widths(Line))
            )
        end
    ).

collect_sys_info_test() ->
    Cmd = metrics_fixture(),
    OsProcessInfo = observer_cli_system:collect_os_process_info(Cmd),
    ?assertEqual("236.0%", lists:flatten(proplists:get_value(beam_cpu, OsProcessInfo))),
    ?assertEqual("182 MiB (+3.0 MiB)", lists:flatten(proplists:get_value(beam_rss, OsProcessInfo))),
    ?assertEqual("1.51s", lists:flatten(proplists:get_value(cpu_window, OsProcessInfo))),
    Info = observer_cli_system:collect_sys_info(Cmd),
    ?assertEqual("236.0%", lists:flatten(proplists:get_value(beam_cpu, Info))),
    ?assertEqual("182 MiB (+3.0 MiB)", lists:flatten(proplists:get_value(beam_rss, Info))),
    ?assertEqual("1.51s", lists:flatten(proplists:get_value(cpu_window, Info))),
    ?assertEqual("unavailable", proplists:get_value(beam_vsz, Info)).

collect_system_info_test() ->
    Info = observer_cli_system:collect_system_info(metrics_fixture()),
    ?assertEqual(
        lists:sort([allocator_info, dist_nodes_info, os_process_info, sys_info]),
        lists:sort(maps:keys(Info))
    ),
    AllocatorInfo = maps:get(allocator_info, Info),
    ?assertEqual(
        lists:sort([
            average_block_curs,
            average_block_maxes,
            cache_hit_info,
            sbcs_to_mbcs_curs,
            sbcs_to_mbcs_maxes
        ]),
        lists:sort(maps:keys(AllocatorInfo))
    ),
    ?assert(is_list(maps:get(cache_hit_info, AllocatorInfo))),
    ?assert(is_list(maps:get(average_block_curs, AllocatorInfo))),
    ?assert(is_list(maps:get(average_block_maxes, AllocatorInfo))),
    ?assert(is_list(maps:get(sbcs_to_mbcs_curs, AllocatorInfo))),
    ?assert(is_list(maps:get(sbcs_to_mbcs_maxes, AllocatorInfo))),
    OsProcessInfo = maps:get(os_process_info, Info),
    SysInfo = maps:get(sys_info, Info),
    DistNodesInfo = maps:get(dist_nodes_info, Info),
    ?assertEqual(
        lists:sort([beam_cpu, beam_rss, cpu_window, beam_vsz]),
        lists:sort([Key || {Key, _} <- OsProcessInfo])
    ),
    ?assertEqual("236.0%", lists:flatten(proplists:get_value(beam_cpu, OsProcessInfo))),
    ?assertEqual(undefined, proplists:get_value(beam_cpu, SysInfo)),
    ?assert(lists:keymember(otp_release, 1, SysInfo)),
    ?assert(lists:keymember(schedulers_online, 1, SysInfo)),
    ?assert(lists:keymember(io_input, 1, SysInfo)),
    ?assert(lists:keymember(ets_count, 1, SysInfo)),
    ?assert(lists:keymember(ets_limit, 1, SysInfo)),
    ?assert(lists:keymember(dist_buf_busy_limit, 1, SysInfo)),
    ?assert(lists:keymember(dirty_cpu_schedulers, 1, SysInfo)),
    ?assert(lists:keymember(module_count, 1, SysInfo)),
    ?assert(is_list(DistNodesInfo)),
    [
        ?assertMatch(
            {
                _Node,
                #{
                    pending_packets := _,
                    stats := _,
                    address := _,
                    type := _,
                    state := _
                }
            },
            Row
        )
     || Row <- DistNodesInfo
    ].

render_system_sections_test() ->
    FullSysInfo = observer_cli_system:collect_sys_info(metrics_fixture()),
    {OsProcessInfo, SysInfo} = split_os_process_info(FullSysInfo),
    [Sys, Allocator, DistNodes, CacheHit] = observer_cli_system:render_system_sections(#{
        os_process_info => OsProcessInfo,
        sys_info => SysInfo,
        allocator_info => #{
            average_block_curs => allocator_curs(),
            average_block_maxes => allocator_maxes(),
            sbcs_to_mbcs_curs => allocator_sbcs_curs(),
            sbcs_to_mbcs_maxes => allocator_sbcs_maxes(),
            cache_hit_info => cache_hit_fixture()
        },
        dist_nodes_info => []
    }),
    ?assertEqual(observer_cli_system:render_sys_info(FullSysInfo), Sys),
    ?assertEqual(
        observer_cli_system:render_block_size_info(
            allocator_curs(), allocator_maxes(), allocator_sbcs_curs(), allocator_sbcs_maxes()
        ),
        Allocator
    ),
    ?assertEqual(observer_cli_system:render_dist_node_info([]), DistNodes),
    ?assertEqual(observer_cli_system:render_cache_hit_rates(cache_hit_fixture(), 12), CacheHit).

render_cache_hit_rates_test() ->
    CacheHitInfo = [
        {{instance, 0}, [{hit_rate, 0.5}, {hits, 1}, {calls, 2}]},
        {{instance, 1}, [{hit_rate, 0.25}, {hits, 2}, {calls, 8}]},
        {{instance, 2}, [{hit_rate, 0.0}, {hits, 0}, {calls, 0}]}
    ],
    Small = observer_cli_system:render_cache_hit_rates(CacheHitInfo, 3),
    ?assert(string:find(lists:flatten(Small), "Hit Rate") =/= nomatch),
    LargeList =
        lists:map(
            fun(Seq) ->
                {{instance, Seq}, [{hit_rate, 0.1}, {hits, Seq}, {calls, Seq + 1}]}
            end,
            lists:seq(1, 12)
        ),
    Large = observer_cli_system:render_cache_hit_rates(LargeList, 12),
    ?assert(string:find(lists:flatten(Large), "IN|") =/= nomatch).

render_cache_hit_rates_wide_layout_test() ->
    Base = cache_hit_widths(80),
    Wide = cache_hit_widths(180),
    ?assertEqual([1, 3, 4, 6, 7, 9, 10], same_columns(Base, Wide, [1, 3, 4, 6, 7, 9, 10])),
    ?assertEqual([2, 5, 8, 11], wider_columns(Base, Wide, [2, 5, 8, 11])).

render_block_size_info_test() ->
    Allocators = [
        binary_alloc,
        driver_alloc,
        eheap_alloc,
        ets_alloc,
        fix_alloc,
        ll_alloc,
        sl_alloc,
        std_alloc,
        temp_alloc
    ],
    Curs = [{A, [{mbcs, 1}, {sbcs, 2}]} || A <- Allocators],
    Maxes = [{A, [{mbcs, 3}, {sbcs, 4}]} || A <- Allocators],
    STMCurs = [{A, "1"} || A <- Allocators],
    STMMaxs = [{A, "2"} || A <- Allocators],
    Lines = observer_cli_system:render_block_size_info(Curs, Maxes, STMCurs, STMMaxs),
    ?assert(string:find(lists:flatten(Lines), "Allocator Type") =/= nomatch),
    ?assertEqual(
        ["binary_alloc", "1 B", "3 B", "2 B", "4 B", "1", "2"],
        [
            lists:flatten(Item)
         || Item <- observer_cli_system:get_alloc(binary_alloc, Curs, Maxes, STMCurs, STMMaxs)
        ]
    ).

render_block_size_info_wide_layout_test() ->
    Base = block_size_widths(80),
    Wide = block_size_widths(180),
    ?assertEqual([1], same_columns(Base, Wide, [1])),
    ?assertEqual([2, 3, 4, 5, 6, 7], wider_columns(Base, Wide, [2, 3, 4, 5, 6, 7])).

system_golden_output_fragments_test() ->
    observer_cli_test_io:with_geometry(
        24,
        201,
        [],
        fun() ->
            Output = system_golden_output(),
            observer_cli_test_io:assert_stable_fragments(Output, [
                "System(S)",
                "Interval: 1500ms",
                "System/Architecture",
                "CPU's and Threads",
                "Memory Usag",
                "Statistics",
                "compiled for",
                "Allocator Type",
                "Current Mbcs",
                "Max SbcsToMbcs",
                "binary_alloc",
                "IN|",
                "Hits/Calls",
                "HitRat",
                "01|"
            ]),
            observer_cli_test_io:assert_ansi_boundaries(Output),
            assert_system_golden_value_columns()
        end
    ).

get_address_invalid_test() ->
    Info = [{address, #net_address{address = {foo, 1234}}}],
    Addr = observer_cli_system:get_address(Info),
    ?assert(string:find(Addr, "foo") =/= nomatch).

get_address_unknown_test() ->
    Info = [{address, #net_address{address = undefined}}],
    ?assertEqual("unknown", observer_cli_system:get_address(Info)).

render_dist_node_info_unavailable_test() ->
    {Rows, _} = observer_cli_system:sample_distribution(
        [
            {node(), #{
                stats => unavailable,
                pending_packets => unavailable,
                address => "unknown",
                type => normal,
                state => up
            }}
        ],
        #{}
    ),
    Output = lists:flatten(observer_cli_system:render_dist_node_info(Rows)),
    ?assert(string:find(Output, "N/A") =/= nomatch),
    ?assertEqual(nomatch, string:find(Output, "Health")),
    ?assertEqual(nomatch, string:find(Output, "%")).

render_dist_node_info_empty_test() ->
    Output = lists:flatten(observer_cli_system:render_dist_node_info([])),
    ?assertEqual(nomatch, string:find(Output, "Health")),
    ?assert(string:find(Output, atom_to_list(node())) =/= nomatch),
    ?assert(string:find(Output, "no connected nodes") =/= nomatch).

render_dist_node_info_disabled_test() ->
    Output = lists:flatten(
        observer_cli_system:render_dist_node_info([
            {nonode@nohost, #{
                pending_packets => unavailable,
                address => "dist disabled",
                type => "-",
                state => "-"
            }}
        ])
    ),
    ?assert(string:find(Output, "nonode@nohost") =/= nomatch),
    ?assert(string:find(Output, "dist disabled") =/= nomatch).

render_dist_node_info_no_health_inference_test() ->
    {Rows, _} = observer_cli_system:sample_distribution(
        [dist_sample(conn, 1000, 5, 10, 1000000)], #{}
    ),
    [Title, Row] = observer_cli_system:render_dist_node_info(Rows),
    Output = lists:flatten([Title, Row]),
    [
        ?assertEqual(nomatch, string:find(Output, Text))
     || Text <- ["warn", "Health", "Percent", "%"]
    ],
    [
        ?assert(string:find(Output, Text) =/= nomatch)
     || Text <-
            ["Pending pkts", "1000000", "Rx pkt/s", "Tx pkt/s", "Recent pkts (old>new)"]
    ],
    ?assertEqual(pipe_positions(Title), pipe_positions(Row)).

render_dist_node_info_wide_layout_test() ->
    Base = dist_node_widths(80),
    Wide = dist_node_widths(180),
    ?assertEqual([2, 3, 4, 5, 8], unchanged_columns(Base, Wide, [2, 3, 4, 5, 8])),
    ?assertEqual([1, 6, 7], wider_columns(Base, Wide, [1, 6, 7])).

render_dist_node_info_bounded_layout_test() ->
    [
        observer_cli_test_io:with_geometry(40, Width, [], fun() ->
            [{Node, Info}] = dist_node_fixture(),
            Lines = observer_cli_system:render_dist_node_info([
                {Node, Info#{
                    recent_pending := [123456789, 234567890, 345678901],
                    pending_packets := 345678901
                }}
            ]),
            [Title, Row] = Lines,
            ?assertEqual(pipe_positions(Title), pipe_positions(Row)),
            ?assert(lists:all(fun(N) -> N =< Width end, observer_cli_test_io:line_widths(Lines))),
            ?assert(string:find(lists:flatten(Row), "345678901") =/= nomatch),
            observer_cli_test_io:assert_ansi_boundaries(Lines)
        end)
     || Width <- [139, 180, 201]
    ].

get_dist_stats_unavailable_test() ->
    case ets:info(sys_dist) of
        undefined ->
            ?assertEqual(unavailable, observer_cli_system:get_dist_stats(peer)),
            ets:new(sys_dist, [named_table, set, {keypos, 2}]),
            try
                [
                    begin
                        ets:insert(sys_dist, Tuple),
                        ?assertEqual(unavailable, observer_cli_system:get_dist_stats(peer))
                    end
                 || Tuple <- [
                        {connection, peer, invalid_handle},
                        {barred_connection, peer},
                        {changed_shape, peer, invalid_handle}
                    ]
                ]
            after
                ets:delete(sys_dist)
            end;
        _ ->
            ok
    end.

sample_distribution_rates_and_history_test() ->
    {_, First} = observer_cli_system:sample_distribution([dist_sample(conn, 1000, 10, 20, 8)], #{}),
    ?assertEqual("-", maps:get(rx_rate, maps:get(peer, First))),
    {_, Second} = observer_cli_system:sample_distribution(
        [dist_sample(conn, 3000, 16, 28, 21)], First
    ),
    ?assertMatch(
        #{rx_rate := 3.0, tx_rate := 4.0, recent_pending := [8, 21]}, maps:get(peer, Second)
    ),
    {_, Third} = observer_cli_system:sample_distribution(
        [dist_sample(conn, 4000, 16, 28, 38)], Second
    ),
    ?assertMatch(
        #{rx_rate := +0.0, tx_rate := +0.0, recent_pending := [8, 21, 38]}, maps:get(peer, Third)
    ),
    {Rows, Fourth} = observer_cli_system:sample_distribution(
        [dist_sample(conn, 4500, 17, 29, 0)], Third
    ),
    ?assertMatch(
        #{rx_rate := 2.0, tx_rate := 2.0, recent_pending := [21, 38, 0]}, maps:get(peer, Fourth)
    ),
    ?assert(
        string:find(lists:flatten(observer_cli_system:render_dist_node_info(Rows)), "21 > 38 > 0") =/=
            nomatch
    ).

sample_distribution_discontinuity_test() ->
    {_, First} = observer_cli_system:sample_distribution([dist_sample(conn, 1000, 10, 20, 8)], #{}),
    [
        begin
            {_, Next} = observer_cli_system:sample_distribution([Sample], First),
            ?assertMatch(
                #{rx_rate := "-", tx_rate := "-", recent_pending := [0]}, maps:get(peer, Next)
            )
        end
     || Sample <- [
            dist_sample(new_conn, 2000, 100, 200, 0),
            dist_sample(conn, 2000, 9, 30, 0),
            dist_sample(conn, 2000, 11, 19, 0),
            dist_sample(conn, 1000, 11, 21, 0),
            dist_sample(conn, 999, 11, 21, 0)
        ]
    ],
    ?assertEqual({[], #{}}, observer_cli_system:sample_distribution([], First)),
    {peer, Info} = dist_sample(conn, 2000, 11, 21, 0),
    [
        begin
            {_, Gap} = observer_cli_system:sample_distribution([{peer, Failed}], First),
            ?assertMatch(#{rx_rate := "N/A", recent_pending := []}, maps:get(peer, Gap)),
            {_, Recovered} = observer_cli_system:sample_distribution(
                [dist_sample(conn, 3000, 12, 22, 0)], Gap
            ),
            ?assertMatch(#{rx_rate := "-", recent_pending := [0]}, maps:get(peer, Recovered))
        end
     || Failed <- [
            Info#{stats := unavailable, pending_packets := unavailable}, Info#{state := pending}
        ]
    ].

dist_sample(Connection, Time, Rx, Tx, Pending) ->
    {peer, #{
        stats => {ok, Connection, Rx, Tx, Pending},
        sampled_at => Time,
        pending_packets => Pending,
        state => up,
        type => normal,
        address => "127.0.0.1:1234"
    }}.

render_worker_redraw_test() ->
    Cmd = unsupported_context(),
    Pid = spawn(fun() -> observer_cli_system:render_worker(Cmd, 1, ?INIT_TIME_REF) end),
    Ref = erlang:monitor(process, Pid),
    Pid ! redraw,
    Pid ! {new_interval, 2},
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        ok
    end.

render_worker_empty_sys_dist_test() ->
    case ets:info(sys_dist, owner) of
        undefined ->
            ets:new(sys_dist, [named_table, public, set]),
            try
                Cmd = unsupported_context(),
                Pid = spawn(fun() ->
                    observer_cli_system:render_worker(Cmd, 1, ?INIT_TIME_REF)
                end),
                Ref = erlang:monitor(process, Pid),
                Pid ! quit,
                receive
                    {'DOWN', Ref, process, Pid, _} -> ok
                after 1000 ->
                    ok
                end
            after
                ets:delete(sys_dist)
            end;
        _ ->
            ok
    end.

render_dist_node_info_live_peer_test() ->
    with_distribution(fun() ->
        {ok, Peer, Node} = peer:start_link(#{
            name => peer:random_name("observer_cli_sys"),
            connection => standard_io,
            args => ["+S", "2"]
        }),
        try
            pong = net_adm:ping(Node),
            ?assertMatch([_ | _], ets:lookup(sys_dist, Node)),
            NodesInfo = observer_cli_system:collect_distribution_info(),
            ?assertMatch([_ | _], NodesInfo),
            Lines = observer_cli_system:render_dist_node_info(NodesInfo),
            ?assertEqual(nomatch, string:find(lists:flatten(Lines), "%")),
            ?assertMatch({ok, _, _, _, _}, observer_cli_system:get_dist_stats(Node)),
            {_, Baseline} = observer_cli_system:sample_distribution(NodesInfo, #{}),
            [Node = erpc:call(Node, erlang, node, []) || _ <- lists:seq(1, 10)],
            timer:sleep(20),
            {_, Next} = observer_cli_system:sample_distribution(
                observer_cli_system:collect_distribution_info(), Baseline
            ),
            ?assert(is_float(maps:get(rx_rate, maps:get(Node, Next)))),
            ?assert(is_float(maps:get(tx_rate, maps:get(Node, Next)))),
            ?assert(maps:get(rx_rate, maps:get(Node, Next)) > 0),
            ?assert(maps:get(tx_rate, maps:get(Node, Next)) > 0),
            true = erlang:disconnect_node(Node),
            ?assertEqual(unavailable, observer_cli_system:get_dist_stats(Node)),
            pong = net_adm:ping(Node),
            {_, Reconnected} = observer_cli_system:sample_distribution(
                observer_cli_system:collect_distribution_info(), Next
            ),
            ?assertEqual("-", maps:get(rx_rate, maps:get(Node, Reconnected))),
            ?assertEqual(1, length(maps:get(recent_pending, maps:get(Node, Reconnected))))
        after
            peer:stop(Peer)
        end
    end).

system_golden_output() ->
    [
        observer_cli_lib:render_menu(allocator, "Interval: 1500ms"),
        observer_cli_system:render_sys_info(
            system_fixture(), cpu_fixture(), memory_fixture(), statistics_fixture()
        ),
        observer_cli_system:render_block_size_info(
            allocator_curs(), allocator_maxes(), allocator_sbcs_curs(), allocator_sbcs_maxes()
        ),
        observer_cli_system:render_cache_hit_rates(cache_hit_fixture(), 12),
        observer_cli_lib:render_footer("q(quit)")
    ].

assert_system_golden_value_columns() ->
    {_, BaseSysRow, _} = sys_info_widths(80),
    {_, WideSysRow, _} = sys_info_widths(201),
    ?assertEqual([2, 4, 6, 9], wider_columns(BaseSysRow, WideSysRow, [2, 4, 6, 9])),
    ?assertEqual(
        [2, 3, 4, 5, 6, 7],
        wider_columns(block_size_widths(80), block_size_widths(201), [2, 3, 4, 5, 6, 7])
    ),
    ?assertEqual(
        [2, 5, 8, 11],
        wider_columns(cache_hit_widths(80), cache_hit_widths(201), [2, 5, 8, 11])
    ).

with_distribution(Fun) ->
    WasAlive = erlang:is_alive(),
    case WasAlive of
        true ->
            Fun();
        false ->
            Name = list_to_atom(peer:random_name("observer_cli_sys_origin")),
            {ok, _} = net_kernel:start([Name, shortnames]),
            try
                Fun()
            after
                net_kernel:stop()
            end
    end.

sys_info_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Row | Rest] = observer_cli_system:render_sys_info(
                system_fixture(), cpu_fixture(), memory_fixture(), statistics_fixture()
            ),
            Compile = lists:last(Rest),
            {
                observer_cli_test_io:column_widths(Title),
                observer_cli_test_io:column_widths(Row),
                observer_cli_test_io:column_widths(Compile)
            }
        end
    ).

dist_node_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title, Row] = observer_cli_system:render_dist_node_info(dist_node_fixture()),
            {observer_cli_test_io:column_widths(Title), observer_cli_test_io:column_widths(Row)}
        end
    ).

cache_hit_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title | _] = observer_cli_system:render_cache_hit_rates(cache_hit_fixture(), 12),
            observer_cli_test_io:column_widths(Title)
        end
    ).

block_size_widths(Columns) ->
    observer_cli_test_io:with_geometry(
        24,
        Columns,
        [],
        fun() ->
            [Title | _] = observer_cli_system:render_block_size_info(
                allocator_curs(), allocator_maxes(), allocator_sbcs_curs(), allocator_sbcs_maxes()
            ),
            observer_cli_test_io:column_widths(Title)
        end
    ).

cache_hit_fixture() ->
    [
        {{instance, Seq}, [{hit_rate, 0.1}, {hits, Seq}, {calls, Seq + 1}]}
     || Seq <- lists:seq(1, 12)
    ].

allocator_curs() ->
    [{A, [{mbcs, 1}, {sbcs, 2}]} || A <- allocators()].

allocator_maxes() ->
    [{A, [{mbcs, 3}, {sbcs, 4}]} || A <- allocators()].

allocator_sbcs_curs() ->
    [{A, "1"} || A <- allocators()].

allocator_sbcs_maxes() ->
    [{A, "2"} || A <- allocators()].

allocators() ->
    [
        binary_alloc,
        driver_alloc,
        eheap_alloc,
        ets_alloc,
        fix_alloc,
        ll_alloc,
        sl_alloc,
        std_alloc,
        temp_alloc
    ].

system_fixture() ->
    [
        {"System Version", "A"},
        {"Erts Version", "B"},
        {"Compiled for", "C"},
        {"Emulator Wordsize", 8},
        {"Process Wordsize", 8},
        {"Smp Support", true},
        {"Thread Support", true},
        {"Async thread pool size", 2}
    ].

cpu_fixture() ->
    [
        {"Logical CPU's", 1},
        {"Online Logical CPU's", 1},
        {"Available Logical CPU's", 1},
        {"Schedulers", 1},
        {"Online schedulers", 1},
        {"Available schedulers", 1}
    ].

memory_fixture() ->
    [
        {"Total", {bytes, 100}},
        {"Processes", {bytes, 10}},
        {"Atoms", {bytes, 5}},
        {"Binaries", {bytes, 2}},
        {"Code", {bytes, 3}},
        {"Ets", {bytes, 4}}
    ].

statistics_fixture() ->
    [
        {"ps -o pcpu", "1%"},
        {"ps -o pmem", "2%"},
        {"ps -o rss", {bytes, 3}},
        {"ps -o vsz", {bytes, 4}},
        {"Total IOIn", {bytes, 5}},
        {"Total IOOut", {bytes, 6}}
    ].

dist_node_fixture() ->
    [
        {'very_long_fake_node_for_layout@127.0.0.1', #{
            pending_packets => 1,
            recent_pending => [0, 1, 1],
            address => "127.0.0.1:1234",
            rx_rate => 1.0,
            tx_rate => 2.0,
            type => normal,
            state => up
        }}
    ].

split_os_process_info(SysInfo) ->
    lists:partition(
        fun({Key, _}) -> lists:member(Key, [beam_cpu, beam_rss, cpu_window, beam_vsz]) end,
        SysInfo
    ).

ensure_sys_dist() ->
    case ets:info(sys_dist, owner) of
        undefined ->
            ets:new(sys_dist, [named_table, public, set]),
            true;
        _ ->
            false
    end.

maybe_delete_sys_dist(true) ->
    ets:delete(sys_dist);
maybe_delete_sys_dist(false) ->
    ok.

pipe_positions(Line) ->
    Plain = observer_cli_test_io:plain(Line),
    [Pos || {Char, Pos} <- lists:zip(Plain, lists:seq(1, length(Plain))), Char =:= $|].

unchanged_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, BaseTitle) =:= lists:nth(Pos, WideTitle),
        lists:nth(Pos, BaseRow) =:= lists:nth(Pos, WideRow)
    ].

wider_columns({BaseTitle, BaseRow}, {WideTitle, WideRow}, Columns) ->
    [
        Pos
     || Pos <- Columns,
        lists:nth(Pos, WideTitle) > lists:nth(Pos, BaseTitle),
        lists:nth(Pos, WideRow) > lists:nth(Pos, BaseRow)
    ];
wider_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Wide) > lists:nth(Pos, Base)].

same_columns(Base, Wide, Columns) ->
    [Pos || Pos <- Columns, lists:nth(Pos, Base) =:= lists:nth(Pos, Wide)].

system_private_helper_contract_test() ->
    ?assertNotEqual([], observer_cli_system:format_count_limit(1, 10)),
    ?assertNotEqual([], observer_cli_system:format_count_limit(unknown, unknown)),
    ?assert(is_list(observer_cli_system:collect_runtime_info())),
    ?assert(is_list(observer_cli_system:alloc_info())),
    ?assert(is_integer(observer_cli_system:maybe_system_info(schedulers))),
    ?assertEqual(undefined, observer_cli_system:maybe_system_info(not_a_system_info_key)),
    Created = ensure_sys_dist(),
    try
        ?assertEqual(unavailable, observer_cli_system:get_dist_stats(missing_node)),
        ets:insert(sys_dist, {dummy}),
        ?assertMatch(
            [{_, #{address := "no connected nodes"}}],
            observer_cli_system:collect_distribution_info()
        )
    after
        maybe_delete_sys_dist(Created)
    end,
    Previous = erlang:system_flag(multi_scheduling, block),
    try
        ?assert(is_list(observer_cli_system:collect_runtime_info()))
    after
        _ = erlang:system_flag(multi_scheduling, unblock),
        Previous
    end.

metrics_fixture() ->
    #{
        cpu => #{status => available, percent => 236.0, interval_us => 1510000},
        rss_bytes => 182 * 1048576,
        rss_delta_bytes => 3 * 1048576
    }.

unavailable_metrics() ->
    #{cpu => #{status => unavailable}, rss_bytes => undefined, rss_delta_bytes => undefined}.

unsupported_context() ->
    #{platform => unsupported, identity => {node(), os:getpid()}, timeout_ms => 500}.

-endif.
