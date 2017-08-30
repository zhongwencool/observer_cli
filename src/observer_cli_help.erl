%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_help).

-include("observer_cli.hrl").
%% API
-export([start/2]).

-define(HELP_COLUMN_WIDTH, 85).

-spec start(atom(), view_opts()) -> no_return.
start(Node, #view_opts{help = #help{interval = Interval}} = ViewOpts) ->
    ChildPid = spawn(fun() ->
                             observer_cli_lib:clear_screen(),
                             draw_menu(Node),
                             draw_help(),
                             loop(Interval, Node) 
                     end),
    waiting(Node, ChildPid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, ChildPid, ViewOpts) ->
    Input = observer_cli_lib:get_line(""),
    case  Input of
        "q\n" ->
            erlang:send(ChildPid, quit),
            "";
        "o\n" ->
            erlang:send(ChildPid, go_to_other_view),
            observer_cli:start_node(Node, ViewOpts);
        "e\n" ->
            erlang:send(ChildPid, go_to_other_view),
            observer_cli_system:start(Node, ViewOpts);
        "n\n" ->
            erlang:send(ChildPid, go_to_other_view),
            observer_cli_inet:start(Node, ViewOpts);
        "db\n" ->
            erlang:send(ChildPid, go_to_other_view),
            observer_cli_mnesia:start(Node, ViewOpts);
        "a\n" ->
            erlang:send(ChildPid, go_to_other_view),
            observer_cli_allocator:start(Node, ViewOpts);
        _ -> waiting(Node, ChildPid, ViewOpts)
    end.

loop(Interval, Node) ->
    observer_cli_lib:move_cursor_to_top_line(),
    draw_menu(Node),
    erlang:send_after(Interval, self(), refresh),
    receive
        refresh -> loop(Interval, Node);
        quit ->
            observer_cli_lib:move_cursor_to_top_line(),
            draw_menu(Node),
            draw_help(),
            quit;
        go_to_other_view -> quit
    end.

draw_help() ->

    io:format("|\e[42m1. Stare Mode\e[49m                                                                      |~n"),
    io:format("| \e[48;2;80;80;80m1.1\e[0m  observer_cli:start()                                                         |~n"),
    io:format("| \e[48;2;80;80;80m1.2\e[0m  observer_cli:start(Node)                                                     |~n"),
    io:format("| \e[48;2;80;80;80m1.3\e[0m  observer_start:start(Node, Cookie)                                           |~n"),

    io:format("|\e[42m2. o(OBSERVER) Commands\e[49m                                                            |~n"),
    io:format("| \e[48;2;80;80;80mr     \e[0m switch mode to reduction(proc_count)                                       |~n"),
    io:format("| \e[48;2;80;80;80mrr    \e[0m switch mode to reduction(proc_window)                                      |~n"),
    io:format("| \e[48;2;80;80;80mi3000   \e[0m set interval time to 3000ms                                              |~n"),
    io:format("| \e[48;2;80;80;80mr5000 \e[0m switch mode to reduction(proc_count) and refresh time(5000ms)              |~n"),
    io:format("| \e[48;2;80;80;80mrr6000\e[0m switch mode to reduction(proc_window) and refresh time(5000ms)             |~n"),
    io:format("| \e[48;2;80;80;80mj13   \e[0m choose the 13th process(yellow line) recon process by recon:info/1         |~n"),
    io:format("| \e[48;2;80;80;80mrow10    \e[0m show 10 rank process rows                                               |~n"),
    io:format("| \e[48;2;80;80;80mp     \e[0m pause/unpause the view                                                     |~n"),

    io:format("|\e[42m3. About o(OBSERVER)'s Interval\e[49m                                                    |~n"),
    io:format("| If \e[48;2;80;80;80mo(OBSERVER)\e[0m's refreshtime is 2000ms, it will be divided into two sections      |~n"),
    io:format("| 1. collect IO information:(2000 div 2) = 1000 ms by using recon:node_stats_list/2;|~n"),
    io:format("| 2. the time of collecting process info deps on which mode you choose:             |~n"),
    io:format("| |... using r mode's(recon:proc_count/2), the cost time is closed to 0             |~n"),
    io:format("| |... using rr mode's(recon:proc_window/3), (2 * 1000 - 1000 div 2) = 3000 ms.     |~n"),

    io:format("|\e[42m4. About o(OBSERVER)'s IO Output/Input\e[49m                                             |~n"),
    io:format("| Due to bytes in and out of the node, number of garbage collector runs, words of   |~n"),
    io:format("| memory that were garbage collected, and the global reductions count for the node  |~n"),
    io:format("| never stop increasing, \e[48;2;80;80;80mo(OBSERVER)\e[0m's \"IO input/out\", \"Gc Words Reclaimed\", \"Gc    |~n"),
    io:format("| Count\" only represents the increments between two refresh interval                |~n"),
    io:format("| The total bytes in and out in \e[48;2;80;80;80me(ETS)\e[0m view.                                        |~n"),

    io:format("|\e[42m5. Reference\e[49m                                                                       |~n"),
    io:format("|More infomation about recon:proc_count/2 and recon:proc_window/3                   |~n"),
    io:format("refer to https://github.com/ferd/recon/blob/master/src/recon.erl                    |~n"),
    io:format("|Any issue please visit: https://github.com/zhongwencool/observer_cli/issues        |~n"),
    io:format("|___________________________________________________________________________________|~n").

draw_menu(Node) ->
    Title = observer_cli_lib:get_menu_title(help),
    UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
    Space = lists:duplicate(?HELP_COLUMN_WIDTH - erlang:length(Title)    - erlang:length(UpTime)+ 130, " "),
    io:format("~s~n", [Title ++ Space ++ UpTime]).

