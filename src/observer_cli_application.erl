%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_application).

-include("observer_cli.hrl").

%% API
-export([start/1]).

-ifdef(TEST).
-export([
    app_status/1,
    collect_app_info/0,
    collect_app_info/4,
    collect_app_render_info/3,
    find_group_leader/1,
    app_render_info/4,
    render_app_info/3,
    update_app_stats/6
]).
-endif.

%% API
-define(LAST_LINE,
    "refresh: ~wms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is ~w"
).

%% erlang:processes_iterator/0 is not exported before OTP 27
-dialyzer([{nowarn_function, [collect_app_info/0, app_info_iter/4]}]).
-ignore_xref({erlang, processes_iterator, 0}).
-ignore_xref({erlang, processes_next, 1}).

%% @doc List application info

-spec start(ViewOpts) -> quit when ViewOpts :: view_opts().
start(#view_opts{app = App, auto_row = AutoRow} = ViewOpts) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(App, AutoRow)
    end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, Opts = #view_opts{app = App = #app{cur_page = CurPage}}) ->
    case observer_cli_lib:parse_cmd(Opts, ?MODULE, [Pid]) of
        quit ->
            erlang:unlink(Pid),
            erlang:send(Pid, quit),
            quit;
        {func, proc_count, message_queue_len} ->
            restart(Pid, Opts#view_opts{app = App#app{type = {message_queue_len, 4}}});
        {func, proc_count, reductions} ->
            restart(Pid, Opts#view_opts{app = App#app{type = {reductions, 3}}});
        {func, proc_count, memory} ->
            restart(Pid, Opts#view_opts{app = App#app{type = {memory, 2}}});
        pause_or_resume ->
            restart(Pid, Opts#view_opts{app = App#app{type = {proc_count, 1}}});
        {new_interval, NewInterval} ->
            restart(Pid, Opts#view_opts{app = App#app{interval = NewInterval}});
        page_down_top_n ->
            restart_page(Pid, Opts, CurPage, 1);
        page_up_top_n ->
            restart_page(Pid, Opts, CurPage, -1);
        _ ->
            manager(Pid, Opts)
    end.

restart_page(Pid, Opts = #view_opts{app = App}, CurPage, Delta) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    restart(Pid, Opts#view_opts{app = App#app{cur_page = NewPage}}).

restart(Pid, Opts) ->
    observer_cli_lib:exit_processes([Pid]),
    start(Opts).

render_worker(App, AutoRow) ->
    #app{type = Type, interval = Interval, cur_page = CurPage} = App,
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 5, 0),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_top_menu(app, Text),
    AppInfo = collect_app_render_info(Rows, CurPage, Type),
    Info = render_app_info(AppInfo, Type),
    LastText = io_lib:format(?LAST_LINE, [Interval, CurPage]),
    LastLine = observer_cli_lib:render_footer(LastText),
    ?output([?CURSOR_TOP, Menu, Info, LastLine]),
    erlang:send_after(Interval, self(), redraw),
    receive
        quit -> quit;
        redraw -> render_worker(App, AutoRow)
    end.

collect_app_render_info(Row, CurPage, Type) ->
    app_render_info(collect_app_info(), Row, CurPage, Type).

app_render_info(AppInfo, Row, CurPage, {_Type, N}) ->
    Rows = [
        begin
            {0, {element(N, I), S}, [App, C, M, R, Q, S, V]}
        end
     || {App, I = {C, M, R, Q, S, V}} <- maps:to_list(AppInfo)
    ],
    observer_cli_lib:sublist(Rows, Row, CurPage).

render_app_info({StartPos, SortList}, {Type, _N}) ->
    InitColor = [
        {memory, ?GRAY_BG},
        {proc_count, ?GRAY_BG},
        {reductions, ?GRAY_BG},
        {message_queue_len, ?GRAY_BG}
    ],
    [
        {_, MemColor},
        {_, ProcessColor},
        {_, RedColor},
        {_, MsgQColor}
    ] = lists:keyreplace(Type, 1, InitColor, {Type, ?RED_BG}),
    [
        IdTitleW,
        AppTitleW,
        ProcessTitleW,
        MemoryTitleW,
        ReductionsTitleW,
        MsgQTitleW,
        StatusTitleW,
        VersionTitleW
    ] =
        app_title_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Id", IdTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "App", AppTitleW),
        ?UNDERLINE,
        ?W2(ProcessColor, "ProcessCount(p)", ProcessTitleW),
        ?UNDERLINE,
        ?W2(MemColor, "Memory(m)", MemoryTitleW),
        ?UNDERLINE,
        ?W2(RedColor, "Reductions(r)", ReductionsTitleW),
        ?UNDERLINE,
        ?W2(MsgQColor, "MsgQ(mq)", MsgQTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Status", StatusTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "version", VersionTitleW)
    ]),
    [IdW, AppW, ProcessW, MemoryW, ReductionsW, MsgQW, StatusW, VersionW] = app_row_widths(),
    {_, View} = lists:foldl(
        fun({_, _, Item}, {Pos, Acc}) ->
            [App, C, M, R, Q, S, V] = Item,
            {Pos + 1, [
                ?render([
                    ?W(Pos, IdW),
                    ?W(App, AppW),
                    ?W(C, ProcessW),
                    ?W({byte, M}, MemoryW),
                    ?W(R, ReductionsW),
                    ?W(Q, MsgQW),
                    ?W(S, StatusW),
                    ?W(V, VersionW)
                ])
                | Acc
            ]}
        end,
        {StartPos, []},
        SortList
    ),
    [Title | lists:reverse(View)].

-ifdef(TEST).
render_app_info(Row, CurPage, Type) ->
    render_app_info(collect_app_render_info(Row, CurPage, Type), Type).
-endif.

app_title_widths() ->
    observer_cli_lib:weighted_widths(
        [3, 31, 20, 20, 17, 10, 12, 16],
        [0, 4, 0, 0, 1, 0, 0, 4]
    ).

app_row_widths() ->
    observer_cli_lib:weighted_widths(
        [2, 29, 18, 18, 15, 8, 10, 15],
        [0, 4, 0, 0, 1, 0, 0, 4]
    ).

collect_app_info() ->
    Info = application:info(),
    AllApps = app_status(Info),
    Leaders = leader_info(Info),
    case erlang:function_exported(erlang, processes_iterator, 0) of
        true ->
            app_info_iter(AllApps, Leaders, erlang:processes_iterator(), self());
        false ->
            app_info(AllApps, Leaders, erlang:processes(), self())
    end.

-ifdef(TEST).
collect_app_info(AllApps, Leaders, Processes, Self) ->
    app_info(AllApps, Leaders, Processes, Self).
-endif.

app_info_iter(AllApps, Leaders, Iter, Self) ->
    case erlang:processes_next(Iter) of
        {Pid, NewIter} when is_pid(Pid) ->
            case erlang:process_info(Pid, [group_leader, memory, reductions, message_queue_len]) of
                undefined ->
                    app_info_iter(AllApps, Leaders, NewIter, Self);
                Prop ->
                    [
                        {group_leader, Group},
                        {memory, Memory},
                        {reductions, Reds},
                        {message_queue_len, MsgQ}
                    ] = Prop,
                    NewAllApps = update_app_stats(Group, Memory, Reds, MsgQ, AllApps, Leaders),
                    app_info_iter(NewAllApps, Leaders, NewIter, Self)
            end;
        none ->
            AllApps
    end.

%% @doc Update application statistics for a process
update_app_stats(Group, Memory, Reds, MsgQ, AllApps, Leaders) ->
    case maps:find(Group, Leaders) of
        error ->
            handle_group_not_in_leaders(Group, Memory, Reds, MsgQ, AllApps, Leaders);
        {ok, App} ->
            increment_app_stats(App, Memory, Reds, MsgQ, AllApps)
    end.

%% @doc Handle case when group leader is not found in leaders map
handle_group_not_in_leaders(Group, Memory, Reds, MsgQ, AllApps, Leaders) ->
    case find_group_leader(Group) of
        no_group ->
            increment_app_stats(no_group, Memory, Reds, MsgQ, AllApps);
        GroupLeader ->
            handle_found_group_leader(GroupLeader, Memory, Reds, MsgQ, AllApps, Leaders)
    end.

%% @doc Handle case when a group leader is found
handle_found_group_leader(GroupLeader, Memory, Reds, MsgQ, AllApps, Leaders) ->
    case maps:find(GroupLeader, Leaders) of
        error ->
            increment_app_stats(no_group, Memory, Reds, MsgQ, AllApps);
        {ok, App} ->
            increment_app_stats(App, Memory, Reds, MsgQ, AllApps)
    end.

%% @doc Increment statistics for an application
increment_app_stats(App, Memory, Reds, MsgQ, AllApps) ->
    {ok, {C, M, R, Q, S, V}} = maps:find(App, AllApps),
    maps:put(App, {C + 1, M + Memory, R + Reds, Q + MsgQ, S, V}, AllApps).

app_info(AllApps, _Leaders, [], _Self) ->
    AllApps;
app_info(AllApps, Leaders, [Self | Process], Self) ->
    app_info(AllApps, Leaders, Process, Self);
app_info(AllApps, Leaders, [Pid | Process], Self) ->
    case erlang:process_info(Pid, [group_leader, memory, reductions, message_queue_len]) of
        undefined ->
            app_info(AllApps, Leaders, Process, Self);
        Prop ->
            [
                {group_leader, Group},
                {memory, Memory},
                {reductions, Reds},
                {message_queue_len, MsgQ}
            ] = Prop,
            NewAllApps =
                case maps:find(Group, Leaders) of
                    error ->
                        {ok, {C1, M1, R1, Q1, S1, V1}} = maps:find(no_group, AllApps),
                        NewInfo = {C1 + 1, M1 + Memory, R1 + Reds, Q1 + MsgQ, S1, V1},
                        maps:put(no_group, NewInfo, AllApps);
                    {ok, App} ->
                        {ok, {C, M, R, Q, S, V}} = maps:find(App, AllApps),
                        maps:put(App, {C + 1, M + Memory, R + Reds, Q + MsgQ, S, V}, AllApps)
                end,
            app_info(NewAllApps, Leaders, Process, Self)
    end.

leader_info(Info) ->
    {running, Running} = lists:keyfind(running, 1, Info),
    leader_info(Running, #{}).

leader_info([{App, Sup} | Running], Acc) when is_pid(Sup) ->
    NewAcc =
        case erlang:process_info(Sup, group_leader) of
            undefined ->
                Acc;
            {group_leader, Pid} ->
                Acc#{Pid => App}
        end,
    leader_info(Running, NewAcc);
leader_info([_ | Running], Acc) ->
    leader_info(Running, Acc);
leader_info([], Acc) ->
    Acc.

app_status(Info) ->
    {loaded, Loaded} = lists:keyfind(loaded, 1, Info),
    {loading, Loading} = lists:keyfind(loading, 1, Info),
    {started, Started} = lists:keyfind(started, 1, Info),
    {start_p_false, StartPFalse} = lists:keyfind(start_p_false, 1, Info),
    {starting, Starting} = lists:keyfind(starting, 1, Info),
    R0 = #{no_group => {0, 0, 0, 0, "Unknown", "unknown"}},
    R1 = lists:foldl(
        fun({App, _From}, Acc) ->
            Acc#{App => {0, 0, 0, 0, "Loading", "unknown"}}
        end,
        R0,
        Loading
    ),
    R2 = lists:foldl(
        fun({App, _Desc, Version}, Acc) ->
            Acc#{App => {0, 0, 0, 0, "Loaded", Version}}
        end,
        R1,
        Loaded
    ),
    R3 = lists:foldl(
        fun({App, _RestartType, _Type, _From}, Acc) ->
            Version = get_version(App, Acc),
            Acc#{App => {0, 0, 0, 0, "Starting", Version}}
        end,
        R2,
        Starting
    ),
    R4 = lists:foldl(
        fun({App, _RestartType}, Acc) ->
            Version = get_version(App, Acc),
            Acc#{App => {0, 0, 0, 0, "Started", Version}}
        end,
        R3,
        Started
    ),
    lists:foldl(
        fun({App, _RestartType, _Type, _From}, Acc) ->
            Version = get_version(App, Acc),
            Acc#{App => {0, 0, 0, 0, "StartPFalse", Version}}
        end,
        R4,
        StartPFalse
    ).

get_version(App, Maps) ->
    case maps:find(App, Maps) of
        {ok, {_, _, _, _, _, V}} -> V;
        _ -> "unknown"
    end.

find_group_leader(Pid) ->
    case erlang:process_info(Pid, group_leader) of
        undefined -> no_group;
        {group_leader, Pid} -> Pid;
        {group_leader, Group} -> find_group_leader(Group)
    end.
