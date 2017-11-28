%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_application).

-include("observer_cli.hrl").
%% API
-export([start/1]).

-define(INTERVAL, 2000).

%% @doc List application info

-spec start(ViewOpts) -> no_return when ViewOpts :: view_opts().
start(#view_opts{} = ViewOpts) ->
    Pid = spawn(fun() ->
        ?output(?CLEAR),
        render_worker(?INTERVAL)
                end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(Pid, ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, Pid) of
        quit -> erlang:send(Pid, quit);
        _ -> manager(Pid, ViewOpts)
    end.

render_worker(Interval) ->
    Menu = observer_cli_lib:render_menu(app, "", 126),
    Info = render_application_info(),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Info, LastLine]),
    erlang:send_after(Interval, self(), redraw),
    receive
        quit -> quit;
        redraw -> render_worker(Interval)
    end.

render_last_line() ->
    ?render([?UNDERLINE, ?RED, "INPUT:", ?RESET, ?BLUE_BG, "q(quit)",
        ?W(" ", ?COLUMN - 17), ?RESET_BG]).

render_application_info() ->
    [
        {loaded, LoadedApps},
        {loading, LoadingApps},
        {started, StartedApps},
        {start_p_false, StartPFlase},
        {running, RunningApps},
        {starting, StartingApps}
    ]
        = application_controller:info(),
    {LoadedNotRunning, RunView} = render_running_apps(RunningApps, StartedApps, LoadedApps),
    LoadedView = draw_loaded_apps(LoadedNotRunning),
    StartingView = render_starting_apps(StartingApps),
    SPFView = render_start_p_false(StartPFlase),
    LoadingView = render_loading_apps(LoadingApps),
    [RunView, LoadedView, StartingView, SPFView, LoadingView].

render_running_apps(RunningApps, StartedApps, LoadedApps) ->
    Title = ?render([?BLUE_BG, ?W("Status", 8),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?RESET_BG]),
    draw_running_apps_1(RunningApps, StartedApps, LoadedApps, Title).

draw_running_apps_1([], _StartedApps, LoadedApps, Render) -> {LoadedApps, Render};
draw_running_apps_1([{RunningApp, _}], StartedApps, LoadedApps, Acc) ->
    {_, Type} = lists:keyfind(RunningApp, 1, StartedApps),
    {value, {_, _Desc, Vsn}, NewLoadedApps} = lists:keytake(RunningApp, 1, LoadedApps),
    {NewLoadedApps, Acc ++ [?render([?W("Running", 8),
        ?GREEN, ?W(RunningApp, 14), ?RESET, ?W(Vsn, 10), ?W(Type, 90)])]};
draw_running_apps_1([{App1, _}, {App2, _}], StartedApps, LoadedApps, Acc) ->
    {_, Type1} = lists:keyfind(App1, 1, StartedApps),
    {_, Type2} = lists:keyfind(App2, 1, StartedApps),
    {value, {_, _Desc1, Vsn1}, LoadedApps1} = lists:keytake(App1, 1, LoadedApps),
    {value, {_, _Desc2, Vsn2}, LoadedApps2} = lists:keytake(App2, 1, LoadedApps1),
    {LoadedApps2, Acc ++ [?render([?W("Running", 8),
        ?GREEN, ?W(App1, 14), ?RESET, ?W(Vsn1, 8), ?W(Type1, 10),
        ?GREEN, ?W(App2, 14), ?RESET, ?W(Vsn2, 8), ?W(Type2, 51)])]
    };
draw_running_apps_1([{App1, _}, {App2, _}, {App3, _} | Rest], StartedApps, LoadedApps, Acc) ->
    {_, Type1} = lists:keyfind(App1, 1, StartedApps),
    {_, Type2} = lists:keyfind(App2, 1, StartedApps),
    {_, Type3} = lists:keyfind(App3, 1, StartedApps),
    {value, {_, _Desc1, Vsn1}, LoadedApps1} = lists:keytake(App1, 1, LoadedApps),
    {value, {_, _Desc2, Vsn2}, LoadedApps2} = lists:keytake(App2, 1, LoadedApps1),
    {value, {_, _Desc3, Vsn3}, LoadedApps3} = lists:keytake(App3, 1, LoadedApps2),
    NewAcc = Acc ++ [?render([?W("Running", 8),
        ?GREEN, ?W(App1, 14), ?RESET, ?W(Vsn1, 8), ?W(Type1, 10),
        ?GREEN, ?W(App2, 14), ?RESET, ?W(Vsn2, 8), ?W(Type2, 10),
        ?GREEN, ?W(App3, 14), ?RESET, ?W(Vsn3, 8), ?W(Type3, 10)])],
    draw_running_apps_1(Rest, StartedApps, LoadedApps3, NewAcc).

draw_loaded_apps([]) -> [];
draw_loaded_apps(Apps) ->
    Title = ?render([?BLUE_BG, ?W("Status", 8),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 14), ?W("Vsn", 8), ?W("Type", 10),
        ?RESET_BG]),
    draw_loaded_apps_1(Apps, Title).

draw_loaded_apps_1([], Acc) -> Acc;
draw_loaded_apps_1([{App, _Desc, Vsn}], Acc) ->
    Acc ++ [?render([?W("Loaded", 8),
        ?YELLOW, ?W(App, 14), ?RESET, ?W(Vsn, 10), ?W("******", 90)])];
draw_loaded_apps_1([{App1, _Desc1, Vsn1}, {App2, _Desc2, Vsn2}], Acc) ->
    Acc ++ [?render([?W("Loaded", 8),
        ?YELLOW, ?W(App1, 14), ?RESET, ?W(Vsn1, 8), ?W("******", 10),
        ?YELLOW, ?W(App2, 14), ?RESET, ?W(Vsn2, 8), ?W("******", 51)])];
draw_loaded_apps_1([{App1, _Desc1, Vsn1}, {App2, _Desc2, Vsn2}, {App3, _Desc3, Vsn3} | Apps], Acc) ->
    NewAcc = Acc ++
        [?render([?W("Loaded", 8),
            ?YELLOW, ?W(App1, 14), ?RESET, ?W(Vsn1, 8), ?W("******", 10),
            ?YELLOW, ?W(App2, 14), ?RESET, ?W(Vsn2, 8), ?W("******", 10),
            ?YELLOW, ?W(App3, 14), ?RESET, ?W(Vsn3, 8), ?W("******", 10)])],
    draw_loaded_apps_1(Apps, NewAcc).

render_starting_apps([]) -> [];
render_starting_apps(Apps) ->
    Title = ?render([?BLUE_BG, ?W("Status", 8),
        ?W("Application", 14), ?W("RestartType", 11), ?W("Type", 11), ?W("From", 14),
        ?W("Application", 14), ?W("RestartType", 11), ?W("Type", 11), ?W("From", 13),
        ?RESET_BG]),
    draw_starting_apps_1(Apps, Title).

draw_starting_apps_1([], Acc) -> Acc;
draw_starting_apps_1([{App, RestartType, Type, From}], Acc) ->
    FromStr = observer_cli_lib:to_list(From),
    Acc ++ [?render([?W("Starting", 8),
        ?L_GREEN, ?W(App, 14), ?RESET, ?W(RestartType, 11), ?W(Type, 11), ?W(FromStr, 75)])];
draw_starting_apps_1([{App1, RestartType1, Type1, From1},
    {App2, RestartType2, Type2, From2} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    NewAcc = Acc ++ [?render([?W("Starting", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(RestartType1, 11), ?W(Type1, 11), ?W(From1Str, 14),
        ?L_GREEN, ?W(App2, 14), ?RESET, ?W(RestartType2, 11), ?W(Type2, 11), ?W(From2Str, 13)])],
    draw_starting_apps_1(Apps, NewAcc).

render_start_p_false([]) -> [];
render_start_p_false(Apps) ->
    draw_start_p_false_1(Apps, []).

draw_start_p_false_1([], Acc) -> Acc;
draw_start_p_false_1([{App, RestartType, Type, From}], Acc) ->
    FromStr = observer_cli_lib:to_list(From),
    Acc ++ [?render([?W("SPFalse", 8),
        ?L_GREEN, ?W(App, 14), ?RESET, ?W(RestartType, 11), ?W(Type, 11), ?W(FromStr, 75)])];
draw_start_p_false_1([{App1, RestartType1, Type1, From1}, {App2, RestartType2, Type2, From2} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    NewAcc = Acc ++ [?render([?W("SPFalse", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(RestartType1, 11), ?W(Type1, 11), ?W(From1Str, 14),
        ?L_GREEN, ?W(App2, 14), ?RESET, ?W(RestartType2, 11), ?W(Type2, 11), ?W(From2Str, 13)])],
    draw_start_p_false_1(Apps, NewAcc).

render_loading_apps([]) -> [];
render_loading_apps(Apps) ->
    Title = ?render([?BLUE_BG, ?W("Status", 8),
        ?W("Application", 14), ?W("From", 11), ?W("Application", 14), ?W("From", 11),
        ?W("Application", 14), ?W("From", 11), ?W("Application", 14), ?W("From", 10),
        ?RESET_BG]),
    draw_loading_apps_1(Apps, Title).

draw_loading_apps_1([], Acc) -> Acc;
draw_loading_apps_1([{App1, From1}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    Acc ++ [?render([?W("Loading", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(From1Str, 103)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    Acc ++ [?render([?W("Loading", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(From1Str, 11),
        ?L_GREEN, ?W(App2, 14), ?RESET, ?W(From2Str, 72)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}, {App3, From3}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    From3Str = observer_cli_lib:to_list(From3),
    Acc ++ [?render([?W("Loading", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(From1Str, 11),
        ?L_GREEN, ?W(App2, 14), ?RESET, ?W(From2Str, 11),
        ?L_GREEN, ?W(App3, 14), ?RESET, ?W(From3Str, 43)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}, {App3, From3}, {App4, From4} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    From3Str = observer_cli_lib:to_list(From3),
    From4Str = observer_cli_lib:to_list(From4),
    NewAcc = Acc ++ [?render([?W("Loading", 8),
        ?L_GREEN, ?W(App1, 14), ?RESET, ?W(From1Str, 11),
        ?L_GREEN, ?W(App2, 14), ?RESET, ?W(From2Str, 11),
        ?L_GREEN, ?W(App3, 14), ?RESET, ?W(From3Str, 11),
        ?L_GREEN, ?W(App4, 14), ?RESET, ?W(From4Str, 10)])],
    draw_loading_apps_1(Apps, NewAcc).
