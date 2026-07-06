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
        observer_cli_system:collect_sys_info("printf 'header\\n'")
    ),
    ?assert(string:find(lists:flatten(Line), "System/Architecture") =/= nomatch).

collect_sys_info_test() ->
    Cmd = "printf 'header\\n 1 2 3 4\\n'",
    OsProcessInfo = observer_cli_system:collect_os_process_info(Cmd),
    ?assertEqual("1%", proplists:get_value(ps_cpu, OsProcessInfo)),
    ?assertEqual("2%", proplists:get_value(ps_mem, OsProcessInfo)),
    ?assertEqual(3 * 1024, proplists:get_value(ps_rss, OsProcessInfo)),
    ?assertEqual(4 * 1024, proplists:get_value(ps_vsz, OsProcessInfo)),
    Info = observer_cli_system:collect_sys_info(Cmd),
    ?assertEqual("1%", proplists:get_value(ps_cpu, Info)),
    ?assertEqual("2%", proplists:get_value(ps_mem, Info)),
    ?assertEqual(3 * 1024, proplists:get_value(ps_rss, Info)),
    ?assertEqual(4 * 1024, proplists:get_value(ps_vsz, Info)).

collect_system_info_test() ->
    Info = observer_cli_system:collect_system_info("printf 'header\\n 1 2 3 4\\n'"),
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
        lists:sort([ps_cpu, ps_mem, ps_rss, ps_vsz]),
        lists:sort([Key || {Key, _} <- OsProcessInfo])
    ),
    ?assertEqual("1%", proplists:get_value(ps_cpu, OsProcessInfo)),
    ?assertEqual(undefined, proplists:get_value(ps_cpu, SysInfo)),
    ?assert(lists:keymember(otp_release, 1, SysInfo)),
    ?assert(lists:keymember(schedulers_online, 1, SysInfo)),
    ?assert(lists:keymember(io_input, 1, SysInfo)),
    ?assert(is_list(DistNodesInfo)),
    [
        ?assertMatch(
            {
                _Node,
                #{
                    queue_size := _,
                    queue_limit := _,
                    address := _,
                    in := _,
                    out := _,
                    type := _,
                    state := _
                }
            },
            Row
        )
     || Row <- DistNodesInfo
    ].

render_system_sections_test() ->
    FullSysInfo = observer_cli_system:collect_sys_info("printf 'header\\n 1 2 3 4\\n'"),
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

render_dist_node_info_unsupported_queue_test() ->
    Lines = observer_cli_system:render_dist_node_info([
        {node(), #{
            queue_size => not_found,
            queue_limit => 1024,
            address => "unknown",
            in => 0,
            out => 0,
            type => normal,
            state => up
        }}
    ]),
    ?assert(string:find(lists:flatten(Lines), "unsupp") =/= nomatch).

render_dist_node_info_wide_layout_test() ->
    Created = ensure_sys_dist(),
    try
        Base = dist_node_widths(80),
        Wide = dist_node_widths(180),
        ?assertEqual([3, 5, 6, 7, 8], unchanged_columns(Base, Wide, [3, 5, 6, 7, 8])),
        ?assertEqual([1, 2, 4], wider_columns(Base, Wide, [1, 2, 4]))
    after
        maybe_delete_sys_dist(Created)
    end.

render_worker_redraw_test() ->
    Cmd = "printf 'header\\n 1 2 3 4\\n'",
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
                Cmd = "printf 'header\\n 1 2 3 4\\n'",
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
        {ok, Peer, Node} = peer:start_link(#{name => peer:random_name("observer_cli_sys")}),
        try
            ?assertMatch([_ | _], ets:lookup(sys_dist, Node)),
            NodesInfo = observer_cli_system:collect_distribution_info(),
            ?assertMatch([_ | _], NodesInfo),
            Lines = observer_cli_system:render_dist_node_info(NodesInfo),
            ?assert(string:find(lists:flatten(Lines), "%") =/= nomatch)
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
            queue_size => 1,
            queue_limit => 1024,
            address => "127.0.0.1:1234",
            in => 1,
            out => 2,
            type => normal,
            state => connected
        }}
    ].

split_os_process_info(SysInfo) ->
    lists:partition(
        fun({Key, _}) -> lists:member(Key, [ps_cpu, ps_mem, ps_rss, ps_vsz]) end,
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

-endif.
