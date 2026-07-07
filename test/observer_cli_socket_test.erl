-module(observer_cli_socket_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

collect_general_info_test() ->
    Info = observer_cli_socket:collect_general_info(),
    ?assert(is_map(Info)),
    ?assert(maps:is_key(iov_max, Info)),
    ?assert(maps:is_key(num_cnt_bits, Info)),
    ?assert(maps:is_key(num_sockets, Info)),
    ?assert(maps:is_key(num_monitors, Info)).

collect_socket_detail_test() ->
    with_socket(fun(Socket) ->
        Detail = observer_cli_socket:collect_socket_detail(Socket),
        ?assertMatch(#{owner := _, domain := inet, type := stream, protocol := tcp}, Detail),
        ?assert(maps:is_key(counters, Detail)),
        ?assert(maps:is_key(options, Detail)),
        ?assert(lists:keymember(read_byte, 1, maps:get(counters, Detail)))
    end).

render_general_info_test() ->
    Text = plain(observer_cli_socket:render_general_info(general_fixture())),
    ?assert(string:find(Text, "General") =/= nomatch),
    ?assert(string:find(Text, "num_sockets") =/= nomatch),
    ?assert(string:find(Text, "num_ptcp") =/= nomatch).

render_socket_rows_test() ->
    {_SocketList, Rows} = observer_cli_socket:render_socket_rows(
        {1, [ranked_overview_fixture()]}, io
    ),
    Text = plain(Rows),
    ?assert(string:find(Text, "Legend: io=read+write") =/= nomatch),
    ?assert(string:find(Text, "Read rb") =/= nomatch),
    ?assert(string:find(Text, "Write wb") =/= nomatch),
    ?assert(string:find(Text, "Pkt/Acc") =/= nomatch),
    ?assert(string:find(Text, "#Socket<0.1.2>") =/= nomatch),
    ?assert(string:find(Text, "inet/tcp") =/= nomatch),
    ?assert(string:find(Text, "3/1") =/= nomatch).

render_socket_detail_test() ->
    Text = plain(observer_cli_socket:render_socket_detail(detail_fixture())),
    ?assert(string:find(Text, "Overview") =/= nomatch),
    ?assert(string:find(Text, "local_address") =/= nomatch),
    ?assert(string:find(Text, "read_byte") =/= nomatch),
    ?assert(string:find(Text, "socket:sndbuf") =/= nomatch).

render_socket_layout_width_test() ->
    observer_cli_test_io:with_geometry(
        24,
        205,
        [],
        fun() ->
            {_SocketList, SocketRows} = observer_cli_socket:render_socket_rows(
                {1, [ranked_overview_fixture()]}, owner
            ),
            Output = [
                observer_cli_socket:render_general_info(general_fixture()),
                SocketRows,
                observer_cli_socket:render_socket_detail(detail_fixture())
            ],
            observer_cli_test_io:assert_ansi_boundaries(Output),
            LayoutWidth = observer_cli_lib:layout_width(),
            ?assert(
                lists:all(
                    fun(Width) -> Width =< LayoutWidth end,
                    observer_cli_test_io:line_widths(Output)
                )
            )
        end
    ).

select_socket_test() ->
    with_socket(fun(Socket) ->
        SocketCount = length(observer_cli_socket:collect_socket_overviews()),
        ?assert(
            lists:any(
                fun(Pos) ->
                    observer_cli_socket:select_socket(Pos, #sockets{sort = id}) =:= {ok, Socket}
                end,
                lists:seq(1, SocketCount)
            )
        )
    end).

start_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_socket:start(Opts))
        end
    ).

start_manager_branches_test() ->
    Inputs = [
        "io\n",
        "rb\n",
        "wb\n",
        "pk\n",
        "wt\n",
        "fl\n",
        "mx\n",
        "ac\n",
        "ow\n",
        "pd\n",
        "pu\n",
        "x\n",
        "q\n"
    ],
    observer_cli_test_io:with_input(
        Inputs,
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_socket:start(Opts))
        end
    ).

start_detail_quit_test() ->
    with_socket(fun(Socket) ->
        observer_cli_test_io:with_input(
            ["q\n"],
            fun() ->
                Opts = #view_opts{auto_row = false},
                ?assertEqual(quit, observer_cli_socket:start(Socket, Opts))
            end
        )
    end).

socket_options_dynamic_test() ->
    with_socket(fun(Socket) ->
        #{domain := Domain, type := Type, protocol := Protocol} = socket:info(Socket),
        Options = observer_cli_socket:socket_options(Socket, #{
            domain => Domain,
            type => Type,
            protocol => Protocol
        }),
        ?assert(
            lists:any(fun({Key, _Value}) -> string:find(Key, "socket:") =/= nomatch end, Options)
        )
    end).

with_socket(Fun) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    try
        Fun(Socket)
    after
        socket:close(Socket)
    end.

plain(IoData) ->
    observer_cli_test_io:plain(IoData).

general_fixture() ->
    #{
        iov_max => 1024,
        num_cnt_bits => 64,
        num_sockets => 1,
        num_monitors => 0,
        num_dinet => 1,
        num_dinet6 => 0,
        num_dlocal => 0,
        num_tstreams => 1,
        num_tdgrams => 0,
        num_tseqpkgs => 0,
        num_pip => 0,
        num_psctp => 0,
        num_ptcp => 1,
        num_pudp => 0
    }.

overview_fixture() ->
    #{
        id => fake_socket,
        id_str => "#Socket<0.1.2>",
        owner => self(),
        fd => 42,
        domain => inet,
        type => stream,
        protocol => tcp,
        counters => counter_fixture(),
        delta_counters => counter_fixture(),
        rstate => [],
        wstate => [select]
    }.

ranked_overview_fixture() ->
    {0, "fixture", maps:to_list(overview_fixture())}.

counter_fixture() ->
    #{
        acc_success => 1,
        acc_tries => 2,
        acc_waits => 3,
        acc_fails => 4,
        read_byte => 2048,
        read_pkg => 2,
        read_pkg_max => 512,
        read_waits => 5,
        read_fails => 6,
        write_byte => 1024,
        write_pkg => 1,
        write_pkg_max => 256,
        write_waits => 7,
        write_fails => 8
    }.

detail_fixture() ->
    (overview_fixture())#{
        laddress => "127.0.0.1:1234",
        raddress => "-",
        monitored_by => [self()],
        counters => maps:to_list(counter_fixture()),
        options => [{"socket:sndbuf", 131072}, {"tcp:nodelay", false}]
    }.

-endif.
