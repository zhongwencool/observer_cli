%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_help).

-include("observer_cli.hrl").
-import_type({observer_cli_types, [view_opts/0]}).

%% API
-export([start/1]).

-ifdef(TEST).
-export([render_help/0, render_doc/1, render_worker/1]).
-endif.

-define(HELP_COLUMN_WIDTH, 85).
-define(SHORTCUT_COLUMN_WIDTH, 12).

-spec start(view_opts()) -> no_return().
start(#view_opts{help = #help{interval = Interval}} = ViewOpts) ->
    ChildPid = spawn_link(fun() ->
        Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
        Menu = observer_cli_lib:render_top_menu(doc, Text),
        Help = render_help(),
        LastLine = observer_cli_lib:render_footer("q(quit)"),
        ?output([?CLEAR, Menu, Help, ?UNDERLINE, ?GRAY_BG, LastLine, ?RESET_BG, ?RESET]),
        render_worker(Interval)
    end),
    manager(ChildPid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [ChildPid]) of
        quit -> erlang:send(ChildPid, quit);
        _ -> manager(ChildPid, ViewOpts)
    end.

render_worker(Interval) ->
    ?output(?CURSOR_TOP),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_top_menu(doc, Text),
    ?output([?CURSOR_TOP, Menu]),
    render_doc(Text),
    erlang:send_after(Interval, self(), redraw),
    receive
        redraw ->
            render_worker(Interval);
        quit ->
            ok
    end.

render_doc(Text) ->
    MenuQ = observer_cli_lib:render_top_menu(doc, Text),
    HelpQ = render_help(),
    LastLine = observer_cli_lib:render_footer("q(quit)"),
    ?output([?CURSOR_TOP, MenuQ, HelpQ, ?UNDERLINE, ?GRAY_BG, LastLine, ?RESET_BG, ?RESET]).

render_help() ->
    [
        "|Doc - Shortcuts and command examples\n",
        section("1. Start Mode"),
        "| \e[48;2;80;80;80m1.1\e[0m  observer_cli:start(). \n",
        "| \e[48;2;80;80;80m1.2\e[0m  observer_cli:start(Node).\n",
        "| \e[48;2;80;80;80m1.3\e[0m  observer_cli:start(Node, Cookie).\n",

        section("2. Global Commands"),
        shortcut("PageDown", "pd or F(forward)."),
        shortcut("PageUp", "pu or B(back)."),
        shortcut("3000", "set interval time to 3000ms, the integer must >= 1500."),
        shortcut("p", "pause/unpause the view."),
        shortcut("O", "open all ports view."),

        section("3. HOME(H) Commands"),
        shortcut("`", "enable/disable schedule usage."),
        shortcut("r", "switch mode to reduction(proc_count)."),
        shortcut("rr", "switch mode to reduction(proc_window)."),
        shortcut("m", "switch mode to memory(proc_count)."),
        shortcut("mm", "switch mode to memory(proc_window)."),
        shortcut("b", "switch mode to bin memory(proc_count)."),
        shortcut("bb", "switch mode to bin memory(proc_window)."),
        shortcut("mq", "switch mode to message queue len(proc_count)."),
        shortcut("mmq", "switch mode to message queue len(proc_window)."),
        shortcut("t", "switch mode to total heap size(proc_count)."),
        shortcut("tt", "switch mode to total heap size(proc_window)."),

        section("4. Process Select Examples"),
        shortcut("13", "choose the 13th process(green line), the integer must in top list."),
        shortcut(
            "<0.43.0>", "choose the <0.43.0> process, the pid does not need to be in the top list."
        ),
        shortcut(
            "<431 or >431",
            "choose the <0.431.0> process, the pid does not need to be in the top list."
        ),

        section("5. Reference"),
        "|More information about recon:proc_count/2 and recon:proc_window/3 \n",
        "|refer to https://github.com/ferd/recon/blob/master/src/recon.erl  \n",
        "|Any issue please visit: https://github.com/zhongwencool/observer_cli/issues  \n"
    ].

section(Text) ->
    ["|\e[44m", Text, "\e[49m \n"].

shortcut(Key, Desc) ->
    [
        "| \e[48;2;80;80;80m",
        io_lib:format("~-*s", [?SHORTCUT_COLUMN_WIDTH, Key]),
        "\e[0m ",
        Desc,
        "\n"
    ].
