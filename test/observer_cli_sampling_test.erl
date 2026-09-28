-module(observer_cli_sampling_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

home_window_values_test() ->
    First = [{stable, 100, []}, {gone, 50, []}, {reset, 50, []}],
    Last = [{stable, 400, []}, {born, 9000, []}, {reset, 40, []}],
    %% 300 reductions in 2.5 actual seconds, not the configured 1.5 seconds.
    ?assertEqual(
        {[{stable, 120, []}], 2, 1},
        observer_cli:process_window(reductions, First, Last, 2500000)
    ),
    {Memory, 2, 0} = observer_cli:process_window(memory, First, Last, 2500000),
    ?assertEqual([{reset, -10, []}, {stable, 300, []}], lists:sort(Memory)),
    ?assertEqual({[], 4, 0}, observer_cli:process_window(memory, First, Last, 0)),
    ?assertEqual({[], 4, 0}, observer_cli:process_window(memory, First, Last, -1)).

home_warming_and_labels_test() ->
    Home = #home{func = proc_window, type = memory},
    {Snapshot, _} = observer_cli:collect_home_snapshot(
        "printf ''",
        Home,
        observer_cli:get_stable_system_info(),
        observer_cli:get_incremental_stats(?DISABLE),
        24,
        true
    ),
    ?assertEqual([], maps:get(top_processes, Snapshot)),
    ?assertNotEqual(
        nomatch, string:find(lists:flatten(maps:get(refresh_prompt, Snapshot)), "warming up")
    ),
    lists:foreach(
        fun({Type, Expected}) ->
            {[], [Title]} = observer_cli:render_top_n_view({window, Type}, [], 0, [], 1),
            observer_cli_test_io:assert_stable_fragments(Title, [Expected]),
            ?assert(observer_cli_lib:visible_length(Title) =< observer_cli_lib:layout_width() + 1)
        end,
        [
            {memory, "Mem change"},
            {binary_memory, "BinMem change"},
            {total_heap_size, "Heap change"},
            {message_queue_len, "Queue change"},
            {reductions, "Reds/s"}
        ]
    ),
    {[], [CountTitle]} = observer_cli:render_top_n_view(reductions, [], 0, [], 1),
    observer_cli_test_io:assert_stable_fragments(CountTitle, ["Reds total", "Memory"]),
    observer_cli_test_io:assert_stable_fragments(
        observer_cli:render_memory_process_line({1, 2, 3, 4}, 1500),
        ["Proc used", "Atom used", "IO/GC since sample", "Total/Delta"]
    ).

sampling_header_layout_test() ->
    Text = [
        observer_cli:get_refresh_prompt(proc_window, message_queue_len, 1500, 41),
        " | Sample:1503ms"
    ],
    lists:foreach(
        fun(Columns) ->
            observer_cli_test_io:with_geometry(24, Columns, [], fun() ->
                {0, Header} = observer_cli_lib:render_sampling_menu(home, Text),
                Plain = observer_cli_test_io:plain(Header),
                observer_cli_test_io:assert_stable_fragments(Header, [
                    "Home(H)",
                    "recon:proc_window(message_queue_len, 41, 1500)",
                    "1500ms",
                    "Sample:1503ms",
                    "Days"
                ]),
                ?assertEqual(
                    [observer_cli_lib:layout_width()], observer_cli_test_io:line_widths(Header)
                ),
                ?assertEqual(nomatch, string:find(Plain, "configured")),
                ?assertEqual(
                    nomatch, binary:match(unicode:characters_to_binary(Header), ?ANSI_INVERSE)
                ),
                observer_cli_test_io:assert_ansi_boundaries(Header)
            end)
        end,
        [200, 240]
    ),
    observer_cli_test_io:with_geometry(24, 139, [], fun() ->
        {1, Header} = observer_cli_lib:render_sampling_menu(home, [
            Text, " | missing:12 | reset:3 | paused"
        ]),
        observer_cli_test_io:assert_stable_fragments(Header, [
            "recon:proc_window(message_queue_len, 41, 1500)",
            "missing:12",
            "reset:3",
            "paused",
            "Sample:1503ms"
        ]),
        ?assertEqual([139, 139], observer_cli_test_io:line_widths(Header)),
        ?assertEqual(nomatch, binary:match(unicode:characters_to_binary(Header), ?ANSI_INVERSE)),
        {Extra, LongHeader} = observer_cli_lib:render_sampling_menu(home, lists:duplicate(300, $x)),
        ?assertEqual(3, Extra),
        ?assert(lists:all(fun(W) -> W =:= 139 end, observer_cli_test_io:line_widths(LongHeader)))
    end).

zero_sample_counts_are_hidden_test() ->
    Healthy = lists:flatten(observer_cli:process_sample_status(1503100, 0, 0)),
    ?assertEqual(" | Sample:1503ms", Healthy),
    ?assertEqual(
        " | Sample:1503ms | missing:2",
        lists:flatten(observer_cli:process_sample_status(1503100, 2, 0))
    ),
    ?assertEqual(
        " | Sample:1503ms | reset:1",
        lists:flatten(observer_cli:process_sample_status(1503100, 0, 1))
    ).

system_allocated_labels_keep_sources_test() ->
    {_, Stat} = observer_cli_system:info_fields(),
    {"Memory Usage", right, Fields} = lists:keyfind("Memory Usage", 1, Stat),
    ?assertEqual({bytes, processes}, proplists:get_value("Processes", Fields)),
    ?assertEqual({bytes, atom}, proplists:get_value("Atoms", Fields)),
    observer_cli_test_io:assert_stable_fragments(
        observer_cli_system:render_sys_info(
            observer_cli_system:collect_sys_info("printf 'header\n 1 2 3 4\n'")
        ),
        ["Size (allocated)", "Processes", "Atoms"]
    ).

home_live_pause_resume_test() ->
    {quit, Output} = observer_cli_test_io:capture_with_geometry(
        24,
        80,
        [{sleep, 120, "p\n"}, {sleep, 80, "p\n"}, {sleep, 100, "q\n"}],
        fun() ->
            observer_cli:start(#view_opts{
                auto_row = false,
                home = #home{func = proc_window, type = reductions, interval = 20}
            })
        end
    ),
    Text = unicode:characters_to_binary(observer_cli_test_io:plain(Output)),
    WarmLines = [
        Line
     || Line <- string:split(observer_cli_test_io:plain(Output), "\n", all),
        string:find(Line, "recon:proc_window(reductions,") =/= nomatch,
        string:find(Line, "warming up") =/= nomatch
    ],
    ?assertEqual(2, length(WarmLines)),
    observer_cli_test_io:assert_stable_fragments(
        Output,
        ["PAUSE", "Reds/s", "Sample:"]
    ),
    ?assertEqual(nomatch, binary:match(Text, <<"missing:0">>)),
    ?assertEqual(nomatch, binary:match(Text, <<"reset:0">>)).

paused_timer_cannot_resume_test() ->
    {quit, Output} = observer_cli_test_io:capture_with_geometry(24, 160, [], fun() ->
        self() ! {proc_window, memory},
        self() ! quit,
        %% Deliberately invalid runtime state: a redraw here is a bug.
        observer_cli:redraw_pause(
            undefined,
            undefined,
            #home{func = proc_window, type = memory},
            undefined,
            undefined,
            make_ref(),
            false
        )
    end),
    observer_cli_test_io:assert_stable_fragments(Output, ["PAUSE"]),
    ?assertEqual(nomatch, string:find(observer_cli_test_io:plain(Output), "recon:proc_window")).

socket_first_reset_missing_and_identity_test() ->
    Info = socket_fixture(),
    [Warm] = observer_cli_socket:add_delta_counters([Info], #{}),
    ?assertEqual(warming_up, maps:get(delta_counters, Warm)),
    ?assertEqual(-1, observer_cli_socket:sort_value(io, Warm)),
    assert_socket_text(Warm, ["warming up", "warm", "lifetime max"]),
    Baseline = observer_cli_socket:current_counters([Info]),
    [Stable] = observer_cli_socket:add_delta_counters([Info], Baseline),
    ?assertEqual(0, observer_cli_socket:sort_value(io, Stable)),
    Counters = maps:get(counters, Info),
    [Reset] = observer_cli_socket:add_delta_counters(
        [Info#{counters := Counters#{read_byte := 1}}], Baseline
    ),
    ?assertEqual(-1, observer_cli_socket:sort_value(rb, Reset)),
    assert_socket_text(Reset, ["reset"]),
    [Missing] = observer_cli_socket:add_delta_counters(
        [Info#{counters := maps:remove(read_byte, Counters)}], Baseline
    ),
    ?assertEqual(-1, observer_cli_socket:sort_value(rb, Missing)),
    assert_socket_text(Missing, ["miss"]),
    %% A reused display string or descriptor is not a reused socket identity.
    [Reconnected] = observer_cli_socket:add_delta_counters(
        [Info#{id := {'$socket', make_ref()}}], Baseline
    ),
    ?assertEqual(warming_up, maps:get(delta_counters, Reconnected)),
    %% Missing an individual sample must not bridge across the gap.
    [Gap] = observer_cli_socket:add_delta_counters(
        [Info#{sample_state => missing, counters := #{}}], Baseline
    ),
    assert_socket_text(Gap, ["missing", "miss"]),
    [AfterGap] = observer_cli_socket:add_delta_counters(
        [Info], observer_cli_socket:current_counters([Gap])
    ),
    ?assertEqual(-1, observer_cli_socket:sort_value(io, AfterGap)),
    ?assertEqual(64, observer_cli_socket:sort_value(mx, Warm)).

socket_optional_shape_change_test() ->
    Info = socket_fixture(),
    Counters = maps:get(counters, Info),
    Before = observer_cli_socket:current_counters([Info]),
    [Changed] = observer_cli_socket:add_delta_counters(
        [Info#{counters := Counters#{sendfile_byte => 5}}], Before
    ),
    ?assertEqual(-1, observer_cli_socket:sort_value(wb, Changed)),
    ?assertEqual(
        "miss",
        maps:get(
            sendfile_byte,
            observer_cli_socket:counter_delta(Counters, Counters#{sendfile_byte => 5})
        )
    ).

socket_controls_preserve_baseline_test() ->
    {ok, Socket} = socket:open(inet, dgram, udp),
    try
        {quit, Output} = observer_cli_test_io:capture_with_geometry(
            24,
            80,
            [
                {sleep, 80, "rb\n"},
                {sleep, 80, "pd\n"},
                {sleep, 80, "pu\n"},
                {sleep, 80, "2000\n"},
                {sleep, 80, "q\n"}
            ],
            fun() -> observer_cli_socket:start(#view_opts{auto_row = false}) end
        ),
        Text = unicode:characters_to_binary(observer_cli_test_io:plain(Output)),
        ?assertEqual(1, length(binary:matches(Text, <<" warming up">>))),
        ?assert(length(binary:matches(Text, <<"Sample:">>)) >= 4),
        observer_cli_test_io:assert_stable_fragments(
            Output,
            ["2000ms", "Current page is 2", "Read chg", "Write chg"]
        )
    after
        socket:close(Socket)
    end.

socket_live_udp_delta_test() ->
    {ok, Receiver} = socket:open(inet, dgram, udp),
    {ok, Sender} = socket:open(inet, dgram, udp),
    try
        ok = socket:bind(Receiver, #{family => inet, addr => {127, 0, 0, 1}, port => 0}),
        {ok, Address} = socket:sockname(Receiver),
        ok = socket:sendto(Sender, <<"before">>, Address),
        {ok, {_, <<"before">>}} = socket:recvfrom(Receiver, 0, 1000),
        {First, Baseline} = observer_cli_socket:collect_socket_info(rb, #{}),
        Warm = find_socket(Receiver, First),
        ?assertEqual(warming_up, maps:get(delta_counters, Warm)),
        ok = socket:sendto(Sender, <<"after">>, Address),
        {ok, {_, <<"after">>}} = socket:recvfrom(Receiver, 0, 1000),
        {Second, _} = observer_cli_socket:collect_socket_info(rb, Baseline),
        ?assertEqual(5, observer_cli_socket:sort_value(rb, find_socket(Receiver, Second)))
    after
        socket:close(Receiver),
        socket:close(Sender)
    end.

find_socket(Socket, Items) ->
    hd([
        Info
     || {_, _, Fields} <- Items, Info <- [maps:from_list(Fields)], maps:get(id, Info) =:= Socket
    ]).

assert_socket_text(Info, Fragments) ->
    {_, Rows} = observer_cli_socket:render_socket_rows({1, [{0, 0, maps:to_list(Info)}]}, io),
    observer_cli_test_io:assert_stable_fragments(Rows, Fragments).

socket_fixture() ->
    #{
        id => {'$socket', make_ref()},
        id_str => "same display ID",
        owner => self(),
        domain => inet,
        type => stream,
        protocol => tcp,
        rstate => [],
        wstate => [],
        counters => #{
            read_byte => 1024,
            write_byte => 512,
            read_pkg => 4,
            write_pkg => 3,
            acc_success => 0,
            acc_tries => 0,
            acc_waits => 0,
            acc_fails => 0,
            read_waits => 0,
            write_waits => 0,
            read_fails => 0,
            write_fails => 0,
            read_pkg_max => 64,
            write_pkg_max => 32
        }
    }.

-endif.
