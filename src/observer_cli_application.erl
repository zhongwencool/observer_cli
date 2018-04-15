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
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(app, Text),
    Info = render_application_info(),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Info, LastLine]),
    erlang:send_after(Interval, self(), redraw),
    receive
        quit -> quit;
        redraw -> render_worker(Interval)
    end.

render_last_line() ->
    ?render([?UNDERLINE, ?RED, "INPUT:", ?RESET, ?UNDERLINE, ?GRAY_BG, "q(quit)",
        ?W(" ", ?COLUMN - 10), ?RESET]).

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
    Title = ?render([?UNDERLINE, ?GRAY_BG, ?W("Status", 7),
        ?W("Application", 16), ?W("Vsn", 9), ?W("Type", 10),
        ?W("Application", 16), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 16), ?W("Vsn", 8), ?W("Type", 10),
        ?RESET]),
    draw_running_apps_1(RunningApps, StartedApps, LoadedApps, Title).

draw_running_apps_1([], _StartedApps, LoadedApps, Render) -> {LoadedApps, Render};
draw_running_apps_1([{RunningApp, _}], StartedApps, LoadedApps, Acc) ->
    {_, Type} = lists:keyfind(RunningApp, 1, StartedApps),
    {value, {_, _Desc, Vsn}, NewLoadedApps} = lists:keytake(RunningApp, 1, LoadedApps),
    {NewLoadedApps, Acc ++ [?render([?W("Running", 7),
        ?W2(?GREEN, RunningApp, 17), ?W(Vsn, 10), ?W(Type, 96)])]};
draw_running_apps_1([{App1, _}, {App2, _}], StartedApps, LoadedApps, Acc) ->
    {_, Type1} = lists:keyfind(App1, 1, StartedApps),
    {_, Type2} = lists:keyfind(App2, 1, StartedApps),
    {value, {_, _Desc1, Vsn1}, LoadedApps1} = lists:keytake(App1, 1, LoadedApps),
    {value, {_, _Desc2, Vsn2}, LoadedApps2} = lists:keytake(App2, 1, LoadedApps1),
    {LoadedApps2, Acc ++ [?render([?W("Running", 7),
        ?W2(?GREEN, App1, 17), ?W(Vsn1, 10), ?W(Type1, 10),
        ?W2(?GREEN, App2, 17), ?W(Vsn2, 9), ?W(Type2, 53)])]
    };
draw_running_apps_1([{App1, _}, {App2, _}, {App3, _} | Rest], StartedApps, LoadedApps, Acc) ->
    {_, Type1} = lists:keyfind(App1, 1, StartedApps),
    {_, Type2} = lists:keyfind(App2, 1, StartedApps),
    {_, Type3} = lists:keyfind(App3, 1, StartedApps),
    {value, {_, _Desc1, Vsn1}, LoadedApps1} = lists:keytake(App1, 1, LoadedApps),
    {value, {_, _Desc2, Vsn2}, LoadedApps2} = lists:keytake(App2, 1, LoadedApps1),
    {value, {_, _Desc3, Vsn3}, LoadedApps3} = lists:keytake(App3, 1, LoadedApps2),
    NewAcc = Acc ++ [?render([?W("Running", 7),
        ?W2(?GREEN, App1, 17), ?RESET, ?W(Vsn1, 10), ?W(Type1, 10),
        ?W2(?GREEN, App2, 17), ?RESET, ?W(Vsn2, 9), ?W(Type2, 10),
        ?W2(?GREEN, App3, 17), ?RESET, ?W(Vsn3, 9), ?W(Type3, 10)])],
    draw_running_apps_1(Rest, StartedApps, LoadedApps3, NewAcc).

draw_loaded_apps([]) -> [];
draw_loaded_apps(Apps) ->
    Title = ?render([?UNDERLINE, ?GRAY_BG, ?W("Status", 7),
        ?W("Application", 16), ?W("Vsn", 9), ?W("Type", 10),
        ?W("Application", 16), ?W("Vsn", 8), ?W("Type", 10),
        ?W("Application", 16), ?W("Vsn", 8), ?W("Type", 10),
        ?RESET]),
    draw_loaded_apps_1(Apps, Title).

draw_loaded_apps_1([], Acc) -> Acc;
draw_loaded_apps_1([{App, _Desc, Vsn}], Acc) ->
    Acc ++ [?render([?W("Loaded", 7),
        ?W2(?YELLOW, App, 17), ?W(Vsn, 10), ?W("******", 96)])];
draw_loaded_apps_1([{App1, _Desc1, Vsn1}, {App2, _Desc2, Vsn2}], Acc) ->
    Acc ++ [?render([?W("Loaded", 7),
        ?W2(?YELLOW, App1, 17), ?W(Vsn1, 10), ?W("******", 10),
        ?W2(?YELLOW, App2, 17), ?W(Vsn2, 9), ?W("******", 53)])];
draw_loaded_apps_1([{App1, _Desc1, Vsn1}, {App2, _Desc2, Vsn2}, {App3, _Desc3, Vsn3} | Apps], Acc) ->
    NewAcc = Acc ++
        [?render([?W("Loaded", 7),
            ?W2(?YELLOW, App1, 17), ?W(Vsn1, 10), ?W("******", 10),
            ?W2(?YELLOW, App2, 17), ?W(Vsn2, 9), ?W("******", 10),
            ?W2(?YELLOW, App3, 17), ?W(Vsn3, 9), ?W("******", 10)])],
    draw_loaded_apps_1(Apps, NewAcc).

render_starting_apps([]) -> [];
render_starting_apps(Apps) ->
    Title = ?render([?UNDERLINE, ?GRAY_BG, ?W("Status", 7),
        ?W("Application", 18), ?W("RestartType", 11), ?W("Type", 11), ?W("From", 13),
        ?W("Application", 18), ?W("RestartType", 11), ?W("Type", 11), ?W("From", 13),
        ?RESET]),
    draw_starting_apps_1(Apps, Title).

draw_starting_apps_1([], Acc) -> Acc;
draw_starting_apps_1([{App, RestartType, Type, From}], Acc) ->
    FromStr = observer_cli_lib:to_list(From),
    Acc ++ [?render([?W("Starting", 7),
         ?W2(?L_GREEN, App, 19), ?W(RestartType, 12), ?W(Type, 11), ?W(FromStr, 78)])];
draw_starting_apps_1([{App1, RestartType1, Type1, From1},
    {App2, RestartType2, Type2, From2} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    NewAcc = Acc ++ [?render([?W("Starting", 7),
        ?W2(?L_GREEN, App1, 19), ?W(RestartType1, 12), ?W(Type1, 11), ?W(From1Str, 13),
        ?W2(?L_GREEN, App2, 19), ?W(RestartType2, 12), ?W(Type2, 11), ?W(From2Str, 13)])],
    draw_starting_apps_1(Apps, NewAcc).

render_start_p_false([]) -> [];
render_start_p_false(Apps) ->
    draw_start_p_false_1(Apps, []).

draw_start_p_false_1([], Acc) -> Acc;
draw_start_p_false_1([{App, RestartType, Type, From}], Acc) ->
    FromStr = observer_cli_lib:to_list(From),
    Acc ++ [?render([?W("SPFalse", 7),
        ?W2(?L_GREEN, App, 19), ?W(RestartType, 12), ?W(Type, 11), ?W(FromStr, 78)])];
draw_start_p_false_1([{App1, RestartType1, Type1, From1}, {App2, RestartType2, Type2, From2} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    NewAcc = Acc ++ [?render([?W("SPFalse", 7),
        ?W2(?L_GREEN, App1, 19), ?W(RestartType1, 12), ?W(Type1, 11), ?W(From1Str, 13),
        ?W2(?L_GREEN, App2, 19), ?W(RestartType2, 12), ?W(Type2, 11), ?W(From2Str, 13)])],
    draw_start_p_false_1(Apps, NewAcc).

render_loading_apps([]) -> [];
render_loading_apps(Apps) ->
    Title = ?render([?UNDERLINE, ?GRAY_BG, ?W("Status", 7),
        ?W("Application", 16), ?W("From", 11), ?W("Application", 16), ?W("From", 10),
        ?W("Application", 16), ?W("From", 11), ?W("Application", 16), ?W("From", 10),
        ?RESET]),
    draw_loading_apps_1(Apps, Title).

draw_loading_apps_1([], Acc) -> Acc;
draw_loading_apps_1([{App1, From1}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    Acc ++ [?render([?W("Loading", 7),
        ?W2(?L_GREEN, App1, 17), ?W(From1Str, 109)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    Acc ++ [?render([?W("Loading", 7),
        ?W2(?L_GREEN, App1, 17), ?W(From1Str, 12),
        ?W2(?L_GREEN, App2, 17), ?W(From2Str, 76)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}, {App3, From3}], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    From3Str = observer_cli_lib:to_list(From3),
    Acc ++ [?render([?W("Loading", 7),
        ?W2(?L_GREEN, App1, 17), ?W(From1Str, 12),
        ?W2(?L_GREEN, App2, 17), ?W(From2Str, 11),
        ?W2(?L_GREEN, App3, 17), ?W(From3Str, 44)])];
draw_loading_apps_1([{App1, From1}, {App2, From2}, {App3, From3}, {App4, From4} | Apps], Acc) ->
    From1Str = observer_cli_lib:to_list(From1),
    From2Str = observer_cli_lib:to_list(From2),
    From3Str = observer_cli_lib:to_list(From3),
    From4Str = observer_cli_lib:to_list(From4),
    NewAcc = Acc ++ [?render([?W("Loading", 7),
        ?W2(?L_GREEN, App1, 17), ?W(From1Str, 12),
        ?W2(?L_GREEN, App2, 17), ?W(From2Str, 11),
        ?W2(?L_GREEN, App3, 17), ?W(From3Str, 12),
        ?W2(?L_GREEN, App4, 17), ?W(From4Str, 11)])],
    draw_loading_apps_1(Apps, NewAcc).
