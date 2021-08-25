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
        {func, proc_count, reductions} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {reductions, 1}}});
        {func, proc_count, memory} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {memory, 2}}});
        {func, proc_count, message_queue_len} ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {message_queue_len, 3}}});
        pause_or_resume ->
            clean([Pid]),
            start(Opts#view_opts{app = App#app{type = {process_count, 4}}});
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
            {0, element(N, I), [App, C, R, M, Q, V]}
        end
        || {{App, V}, I = {R, M, Q, C}} <- maps:to_list(app_info())
    ],
    {StartPos, SortList} = observer_cli_lib:sublist(List, Row, CurPage),
    InitColor = [
        {memory, ?GRAY_BG},
        {process_count, ?GRAY_BG},
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
        ?W2(RedColor, "Reductions(r)", 20),
        ?UNDERLINE,
        ?W2(MsgQColor, "MsgQ(mq)", 15),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "version", 21)
    ]),
    {_, View} = lists:foldl(
        fun({_, _, Item}, {Pos, Acc}) ->
            [App, C, R, M, Q, V] = Item,
            {Pos + 1, [
                ?render([
                    ?W(Pos, 2),
                    ?W(App, 29),
                    ?W(C, 18),
                    ?W({byte, M}, 18),
                    ?W(R, 18),
                    ?W(Q, 13),
                    ?W(V, 20)
                ])
                | Acc
            ]}
        end,
        {StartPos, []},
        SortList
    ),
    [Title | lists:reverse(View)].

-define(Unknown, {unknown, unknown}).

app_info() ->
    Info = application:info(),
    {running, Running} = lists:keyfind(running, 1, Info),
    {loaded, Loaded} = lists:keyfind(loaded, 1, Info),
    Leaders =
        lists:foldl(
            fun
                ({_App, undefined}, Acc) ->
                    Acc;
                ({App, Sup}, Acc) ->
                    case erlang:process_info(Sup, group_leader) of
                        undefined ->
                            Acc;
                        {group_leader, Pid} ->
                            {_, _, Version} = lists:keyfind(App, 1, Loaded),
                            Acc#{Pid => {App, Version}}
                    end
            end,
            #{},
            Running
        ),
    lists:foldl(
        fun(Pid, Acc) ->
            case erlang:process_info(Pid, [group_leader, memory, reductions, message_queue_len]) of
                undefined ->
                    Acc;
                Prop ->
                    [
                        {group_leader, Group},
                        {memory, Memory},
                        {reductions, Reds},
                        {message_queue_len, MsgQ}
                    ] = Prop,
                    case maps:find(Group, Leaders) of
                        error ->
                            {ok, {R1, M1, Q1, C1}} = maps:find(?Unknown, Acc),
                            NewInfo = {R1 + Reds, M1 + Memory, Q1 + MsgQ, C1 + 1},
                            maps:put(?Unknown, NewInfo, Acc);
                        {ok, AppInfo} ->
                            case maps:find(AppInfo, Acc) of
                                {ok, {R, M, Q, C}} ->
                                    maps:put(AppInfo, {R + Reds, M + Memory, Q + MsgQ, C + 1}, Acc);
                                error ->
                                    maps:put(AppInfo, {Reds, Memory, MsgQ, 1}, Acc)
                            end
                    end
            end
        end,
        #{?Unknown => {0, 0, 0, 0}},
        erlang:processes()
    ).
