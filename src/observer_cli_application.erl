%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_application).

-include("observer_cli.hrl").

%% API
-export([start/1]).
-export([clean/1]).

%% API
-define(LAST_LINE,
    "refresh: ~wms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is ~w"
).

%% @doc List application info

-spec start(ViewOpts) -> no_return when ViewOpts :: view_opts().
start(#view_opts{app = App, auto_row = AutoRow} = ViewOpts) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(App, AutoRow)
    end),
    manager(Pid, ViewOpts).

-spec clean(list()) -> ok.
clean(Pids) -> observer_cli_lib:exit_processes(Pids).

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
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {message_queue_len, 4}}});
        {func, proc_count, reductions} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {reductions, 3}}});
        {func, proc_count, memory} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {memory, 2}}});
        pause_or_resume ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {proc_count, 1}}});
        {new_interval, NewInterval} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{interval = NewInterval}});
        page_down_top_n ->
            NewPage = max(CurPage + 1, 1),
            clean([Pid]),
            start(Opts#view_opts{app = App#app{cur_page = NewPage}});
        page_up_top_n ->
            NewPage = max(CurPage - 1, 1),
            clean([Pid]),
            start(Opts#view_opts{app = App#app{cur_page = NewPage}});
        _ ->
            manager(Pid, Opts)
    end.

render_worker(App, AutoRow) ->
    #app{type = Type, interval = Interval, cur_page = CurPage} = App,
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 5, 0),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(app, Text),
    Info = render_app_info(Rows, CurPage, Type),
    LastText = io_lib:format(?LAST_LINE, [Interval, CurPage]),
    LastLine = observer_cli_lib:render_last_line(LastText),
    ?output([?CURSOR_TOP, Menu, Info, LastLine]),
    erlang:send_after(Interval, self(), redraw),
    receive
        quit -> quit;
        redraw -> render_worker(App, AutoRow)
    end.

render_app_info(Row, CurPage, {Type, N}) ->
    List = [
        begin
            {0, {element(N, I), S}, [App, C, M, R, Q, S, V]}
        end
        || {App, I = {C, M, R, Q, S, V}} <- maps:to_list(app_info())
    ],
    {StartPos, SortList} = observer_cli_lib:sublist(List, Row, CurPage),
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
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Id", 3),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "App", 31),
        ?UNDERLINE,
        ?W2(ProcessColor, "ProcessCount(p)", 20),
        ?UNDERLINE,
        ?W2(MemColor, "Memory(m)", 20),
        ?UNDERLINE,
        ?W2(RedColor, "Reductions(r)", 17),
        ?UNDERLINE,
        ?W2(MsgQColor, "MsgQ(mq)", 10),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Status", 12),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "version", 16)
    ]),
    {_, View} = lists:foldl(
        fun({_, _, Item}, {Pos, Acc}) ->
            [App, C, R, M, Q, S, V] = Item,
            {Pos + 1, [
                ?render([
                    ?W(Pos, 2),
                    ?W(App, 29),
                    ?W(C, 18),
                    ?W({byte, M}, 18),
                    ?W(R, 15),
                    ?W(Q, 8),
                    ?W(S, 10),
                    ?W(V, 15)
                ])
                | Acc
            ]}
        end,
        {StartPos, []},
        SortList
    ),
    [Title | lists:reverse(View)].

app_info() ->
    Info = application:info(),
    AllApps = app_status(Info),
    Leaders = leader_info(Info),
    app_info(AllApps, Leaders, erlang:processes(), self()).

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
                        {ok, {C1, M1, R1, Q1, S1, V1}} = maps:find(unknown, AllApps),
                        NewInfo = {C1 + 1, M1 + Memory, R1 + Reds, Q1 + MsgQ, S1, V1},
                        maps:put(unknown, NewInfo, AllApps);
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
    R0 = #{unknown => {0, 0, 0, 0, "Unknown", "unknown"}},
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
            Acc#{App => {0, 0, 0, 0, "StartPFlase", Version}}
        end,
        R4,
        StartPFalse
    ).

get_version(App, Maps) ->
    case maps:find(App, Maps) of
        {ok, {_, _, _, _, _, V}} -> V;
        _ -> "unknown"
    end.
