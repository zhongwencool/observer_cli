-module(observer_cli_core_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

transform_seq_test() ->
    ?assertEqual(3, observer_cli:transform_seq(1, 2, 3)),
    ?assertEqual(1000, observer_cli:transform_seq(3, 2, 3)).

warning_color_test() ->
    ?assertEqual(?RED, observer_cli:warning_color(0.9)),
    ?assertEqual(?GREEN, observer_cli:warning_color(0.1)).

format_atom_info_test() ->
    {Color1, Atom1} = observer_cli:format_atom_info(100, 90),
    ?assertEqual(?RED, Color1),
    ?assertEqual("90/100", lists:flatten(Atom1)),
    {Color2, Atom2} = observer_cli:format_atom_info(100, 10),
    ?assertEqual(<<"">>, Color2),
    ?assertEqual("10/100", lists:flatten(Atom2)).

process_bar_format_style_test() ->
    Format2 = observer_cli:process_bar_format_style([0.1, 0.2], true),
    ?assert(is_binary(Format2)),
    ?assertMatch({0, _}, binary:match(Format2, ?UNDERLINE)),
    Format4 = observer_cli:process_bar_format_style([0.1, 0.2, 0.3, 0.4], false),
    ?assert(is_binary(Format4)),
    Format10 = observer_cli:process_bar_format_style(
        [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
        false
    ),
    ?assert(is_binary(Format10)).

get_refresh_prompt_test() ->
    Count = lists:flatten(observer_cli:get_refresh_prompt(proc_count, memory, 1500, 5)),
    Window = lists:flatten(observer_cli:get_refresh_prompt(proc_window, reductions, 1500, 5)),
    ?assert(lists:prefix("recon:proc_count", Count)),
    ?assert(lists:prefix("recon:proc_window", Window)).

get_current_initial_call_test() ->
    Call = [
        {current_function, {lists, map, 2}},
        {initial_call, {erlang, apply, 3}}
    ],
    {CurFun, InitialCall} = observer_cli:get_current_initial_call(Call),
    ?assertEqual("lists:map/2", lists:flatten(CurFun)),
    ?assertEqual({erlang, apply, 3}, InitialCall).

render_system_line_test() ->
    PsCmd = "printf 'header\\n 1 2\\n'",
    {StableInfo, _} = observer_cli:get_stable_system_info(),
    Line = observer_cli:render_system_line(PsCmd, StableInfo),
    ?assert(string:find(lists:flatten(Line), "System") =/= nomatch).

render_system_line_missing_output_test() ->
    PsCmd = "printf ''",
    {StableInfo, _} = observer_cli:get_stable_system_info(),
    Line = observer_cli:render_system_line(PsCmd, StableInfo),
    ?assert(string:find(lists:flatten(Line), "ps -o pcpu") =/= nomatch).

render_memory_process_line_test() ->
    MemSum = {1, 2, 3, 4},
    {_, PortParallelism} = observer_cli:get_stable_system_info(),
    Line = observer_cli:render_memory_process_line(MemSum, PortParallelism, 1500),
    ?assert(string:find(lists:flatten(Line), "Total") =/= nomatch).

render_memory_process_line_error_logger_test() ->
    MemSum = {1, 2, 3, 4},
    {_, PortParallelism} = observer_cli:get_stable_system_info(),
    Prev = whereis(error_logger),
    TempPid =
        case Prev of
            undefined ->
                Pid = spawn(fun() ->
                    receive
                        stop -> ok
                    end
                end),
                true = register(error_logger, Pid),
                Pid;
            _ ->
                Prev
        end,
    try
        Line = observer_cli:render_memory_process_line(MemSum, PortParallelism, 1500),
        ?assert(string:find(lists:flatten(Line), "RunQueue") =/= nomatch)
    after
        case Prev of
            undefined ->
                unregister(error_logger),
                TempPid ! stop;
            _ ->
                ok
        end
    end.

render_scheduler_usage_test() ->
    ?assertEqual({0, []}, observer_cli:render_scheduler_usage(undefined)),
    {2, _} = observer_cli:render_scheduler_usage([{1, 0.1}, {2, 0.2}, {3, 0.3}, {4, 0.4}]),
    {3, _} = observer_cli:render_scheduler_usage([{1, 0.1}, {2, 0.2}, {3, 0.3}, {4, 0.4}, {5, 0.5}]),
    {2, _} = observer_cli:render_scheduler_usage(
        [{1, 0.1}, {2, 0.2}, {3, 0.3}, {4, 0.4}, {5, 0.5}, {6, 0.6}, {7, 0.7}, {8, 0.8}]
    ),
    {3, _} = observer_cli:render_scheduler_usage(
        lists:map(fun(N) -> {N, 0.1} end, lists:seq(1, 9))
    ),
    {11, _} = observer_cli:render_scheduler_usage(
        lists:map(fun(N) -> {N, 0.1} end, lists:seq(1, 101))
    ),
    {11, _} = observer_cli:render_scheduler_usage(
        lists:map(fun(N) -> {N, 0.1} end, lists:seq(1, 110))
    ).

render_top_n_view_test() ->
    Pid1 = self(),
    Pid2 = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Call1 = [test_name, {current_function, {lists, map, 2}}, {initial_call, {erlang, apply, 3}}],
    Call2 = [
        {registered_name, undefined},
        {current_function, {lists, map, 2}},
        {initial_call, {erlang, apply, 3}}
    ],
    Items = [
        {Pid1, 1000, Call1},
        {Pid2, 2000, Call2}
    ],
    Pages = [{1, 1}],
    {PidList1, Rows1} = observer_cli:render_top_n_view(memory, Items, 2, Pages, 1),
    {PidList2, Rows2} = observer_cli:render_top_n_view(binary_memory, Items, 2, Pages, 1),
    {PidList3, Rows3} = observer_cli:render_top_n_view(reductions, Items, 2, Pages, 1),
    {PidList4, Rows4} = observer_cli:render_top_n_view(total_heap_size, Items, 2, Pages, 1),
    {PidList5, Rows5} = observer_cli:render_top_n_view(message_queue_len, Items, 2, Pages, 1),
    ?assertEqual(2, length(PidList1)),
    ?assertEqual(3, length(Rows1)),
    ?assertEqual(2, length(PidList2)),
    ?assertEqual(3, length(Rows2)),
    ?assertEqual(2, length(PidList3)),
    ?assertEqual(3, length(Rows3)),
    ?assertEqual(2, length(PidList4)),
    ?assertEqual(3, length(Rows4)),
    ?assertEqual(2, length(PidList5)),
    ?assertEqual(3, length(Rows5)),
    erlang:exit(Pid2, kill),
    {PidList1, Rows1, Rows2, Rows3, Rows4, Rows5}.

get_top_n_info_translated_call_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Call = [
        {registered_name, undefined},
        {current_function, {lists, map, 2}},
        {initial_call, {proc_lib, init_p, 5}}
    ],
    {Pid, _Val, _CurFun, Flag} = observer_cli:get_top_n_info({Pid, 1, Call}),
    ?assert(is_list(Flag)),
    erlang:exit(Pid, kill).

atom_status_test() ->
    case observer_cli:get_atom_status() of
        {ok, Limit, Count} ->
            ?assert(is_integer(Limit)),
            ?assert(is_integer(Count));
        {error, unsupported} ->
            ok
    end.

get_pid_info_test() ->
    {Val1, Val2} = observer_cli:get_pid_info(self(), [reductions, message_queue_len]),
    ?assert(Val1 =/= "dead"),
    ?assert(Val2 =/= "dead").

get_pid_info_dead_test() ->
    Pid = spawn(fun() -> receive
        after infinity -> ok
        end end),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        ok
    end,
    ?assertEqual({"dead", "dead"}, observer_cli:get_pid_info(Pid, [reductions, message_queue_len])).

port_proc_info_test() ->
    {PortWarning, ProcWarning, PortCountStr, ProcCountStr} =
        observer_cli:get_port_proc_info(999999, 999999),
    ?assert(is_binary(PortWarning)),
    ?assert(is_binary(ProcWarning)),
    ?assert(string:find(lists:flatten(PortCountStr), "/") =/= nomatch),
    ?assert(string:find(lists:flatten(ProcCountStr), "/") =/= nomatch).

port_proc_info_warning_test() ->
    {PortWarning, ProcWarning, _PortCountStr, _ProcCountStr} =
        observer_cli:get_port_proc_info(1, 1),
    ?assertEqual(?RED, PortWarning),
    ?assertEqual(?RED, ProcWarning).

display_unique_flag_label_false_test() ->
    case erlang:function_exported(proc_lib, get_label, 1) of
        true ->
            Prev = proc_lib:get_label(self()),
            proc_lib:set_label(false),
            try
                Flag = observer_cli:display_unique_flag(123, {erlang, apply, 3}, self()),
                ?assert(string:find(lists:flatten(Flag), "erlang:apply/3") =/= nomatch)
            after
                proc_lib:set_label(Prev)
            end;
        false ->
            ok
    end.

display_unique_flag_label_value_test() ->
    case erlang:function_exported(proc_lib, get_label, 1) of
        true ->
            Prev = proc_lib:get_label(self()),
            proc_lib:set_label(my_label),
            try
                Flag = observer_cli:display_unique_flag(123, {erlang, apply, 3}, self()),
                ?assert(string:find(lists:flatten(Flag), "my_label") =/= nomatch)
            after
                proc_lib:set_label(Prev)
            end;
        false ->
            ok
    end.

node_stats_test() ->
    Stats = observer_cli:get_incremental_stats(?DISABLE),
    {Diffs, _Sched, _New} = observer_cli:node_stats(Stats, ?DISABLE),
    {InDiff, OutDiff, _GcDiff, _WordsDiff} = Diffs,
    ?assert(string:find(lists:flatten(InDiff), "/") =/= nomatch),
    ?assert(string:find(lists:flatten(OutDiff), "/") =/= nomatch).

check_auto_row_test() ->
    ?assert(is_boolean(observer_cli:check_auto_row())).

-endif.
