-module(observer_cli_system_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").
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

clean_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Ref = erlang:monitor(process, Pid),
    observer_cli_system:clean([Pid]),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        ok
    end.

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
        {"B", {bytes, b}},
        {"Group", [{"A2", a}]}
    ],
    Result = observer_cli_system:fill_info(Fields, Data),
    ?assertEqual({"Dyn", 2}, lists:nth(1, Result)),
    ?assertEqual({"A", 1}, lists:nth(2, Result)).

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

render_sys_info_empty_ps_test() ->
    Line = observer_cli_system:render_sys_info("printf 'header\\n'"),
    ?assert(string:find(lists:flatten(Line), "System/Architecture") =/= nomatch).

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

get_address_invalid_test() ->
    Info = [{address, #net_address{address = {foo, 1234}}}],
    Addr = observer_cli_system:get_address(Info),
    ?assert(string:find(Addr, "foo") =/= nomatch).

get_address_unknown_test() ->
    Info = [{address, #net_address{address = undefined}}],
    ?assertEqual("unknown", observer_cli_system:get_address(Info)).

render_dist_node_info_fake_test() ->
    Node = 'fake@node',
    Created =
        case ets:info(sys_dist, owner) of
            undefined ->
                ets:new(sys_dist, [named_table, public, set]),
                true;
            _ ->
                false
        end,
    Info = [
        {Node, [
            {state, connected},
            {type, normal},
            {address, #net_address{address = {{127, 0, 0, 1}, 1234}}},
            {in, 1},
            {out, 2}
        ]}
    ],
    Lines = observer_cli_system:render_dist_node_info(Info),
    ?assert(is_list(Lines)),
    case Created of
        true -> ets:delete(sys_dist);
        false -> ok
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

-endif.
