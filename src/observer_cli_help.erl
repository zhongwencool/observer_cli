%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_help).

-include("observer_cli.hrl").
%% API
-export([start/1]).

-define(HELP_COLUMN_WIDTH, 85).

-spec start(view_opts()) -> no_return.
start(#view_opts{help = #help{interval = Interval}} = ViewOpts) ->
    ChildPid = spawn(fun() ->
        Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
        Menu = observer_cli_lib:render_menu(doc, Text),
        Help = render_help(),
        LastLine = observer_cli_lib:render_last_line("q(quit)"),
        ?output([?CLEAR, Menu, Help, ?UNDERLINE, ?GRAY_BG, LastLine, ?RESET_BG, ?RESET]),
        render_worker(Interval)
                     end),
    manager(ChildPid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ChildPid) of
        quit -> erlang:send(ChildPid, quit);
        _ -> manager(ChildPid, ViewOpts)
    end.

render_worker(Interval) ->
    ?output(?CURSOR_TOP),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_menu(doc, Text),
    ?output([?CURSOR_TOP, Menu]),
    erlang:send_after(Interval, self(), redraw),
    receive
        redraw -> render_worker(Interval);
        quit ->
            MenuQ = observer_cli_lib:render_menu(doc, Text),
            HelpQ = render_help(),
            LastLine = observer_cli_lib:render_last_line("q(quit)"),
            ?output([?CURSOR_TOP, MenuQ, HelpQ, ?UNDERLINE, ?GRAY_BG, LastLine, ?RESET_BG, ?RESET])
    end.

render_help() ->
    [
        "|\e[44m1. Start Mode\e[49m                                                                     \n",
        "| \e[48;2;80;80;80m1.1\e[0m  observer_cli:start()                                                        \n",
        "| \e[48;2;80;80;80m1.2\e[0m  observer_cli:start(Node)                                                    \n",
        "| \e[48;2;80;80;80m1.3\e[0m  observer_start:start(Node, Cookie)                                          \n",
        
        "|\e[44m2. HOME(H) Commands\e[49m                                                               \n",
        "| \e[48;2;80;80;80mr     \e[0m switch mode to reduction(proc_count)                                      \n",
        "| \e[48;2;80;80;80mrr    \e[0m switch mode to reduction(proc_window)                                     \n",
        "| \e[48;2;80;80;80mi3000 \e[0m set interval time to 3000ms                                             \n",
        "| \e[48;2;80;80;80mr5000 \e[0m switch mode to reduction(proc_count) and refresh time(5000ms)             \n",
        "| \e[48;2;80;80;80mrr6000\e[0m switch mode to reduction(proc_window) and refresh time(5000ms)            \n",
        "| \e[48;2;80;80;80mj13   \e[0m choose the 13th process(yellow line) recon process by recon:info/1        \n",
        "| \e[48;2;80;80;80mp     \e[0m pause/unpause the view                                                    \n",
        
        "|\e[44m4. About HOME(H)'s IO Output/Input\e[49m                                                \n",
        "| Due to bytes in and out of the node, number of garbage collector runs, words of  \n",
        "| memory that were garbage collected, and the global reductions count for the node \n",
        "| never stop increasing, \e[48;2;80;80;80mHOME(H)\e[0m's \"IO input/out\", \"Gc Words Reclaimed\", \"Gc       \n",
        "| Count\" only represents the increments between two refresh interval               \n",
        "| The total bytes in and out in \e[48;2;80;80;80mSystem(S)\e[0m view.                                    \n",
        
        "|\e[44m5. Reference\e[49m                                                                      \n",
        "|More infomation about recon:proc_count/2 and recon:proc_window/3                  \n",
        "refer to https://github.com/ferd/recon/blob/master/src/recon.erl                   \n",
        "|Any issue please visit: https://github.com/zhongwencool/observer_cli/issues       \n"
    ].
