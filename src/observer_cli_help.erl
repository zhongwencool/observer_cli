%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_help).

-include("observer_cli.hrl").
%% API
-export([start/0]).
-export([start/2]).

-define(BROAD, 133).

-spec start() -> quit.
start() ->
  start(local_node, ?HELP_MIN_INTERVAL).

-spec start(atom(), pos_integer()) -> quit.
start(Node, Interval) ->
  ChildPid = spawn_link(fun() ->
    observer_cli_lib:clear_screen(),
    draw_menu(Node),
    draw_help(),
    loop(Interval, Node) end),
  waiting(Node, ChildPid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, ChildPid) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" ->
      erlang:send(ChildPid, quit),
      "";
    "o\n" ->
      erlang:send(ChildPid, go_to_other_view),
      observer_cli:start(Node, ?HOME_MIN_INTERVAL);
    "e\n" ->
      erlang:send(ChildPid, go_to_other_view),
      observer_cli_system:start(Node, ?SYSTEM_MIN_INTERVAL);
    "db\n" ->
      erlang:send(ChildPid, go_to_other_view),
      observer_cli_mnesia:start(Node, ?MNESIA_MIN_INTERVAL);
    "a\n" ->
      erlang:send(ChildPid, go_to_other_view),
      observer_cli_allocator:start(Node, ?ALLOCATOR_MIN_INTERVAL);
    _ -> waiting(Node, ChildPid)
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
  io:format("|Due to bytes in and out of the node, number of garbage colelctor runs, words of memory that were garbage collected, and the global |~n"),
  io:format("|reductions count for the node never stop increasing, \e[48;2;80;80;80mo(OBSERVER)\e[0m's \"IO input/out\", \"Gc Count\", \"Gc Words Reclaimed\"                |~n"),
  io:format("|only represents the increments between two refresh interval. The total bytes in and out in \e[48;2;80;80;80me(Ets)\e[0m.                                 |~n"),

  io:format("|\e[42mAbout o(OBSERVER)'s Interval\e[49m                                                                                                       |~n"),
  io:format("|If the refresh interval of \e[48;2;80;80;80mo(OBSERVER)\e[0m's is 2000ms, the 2000ms will be divided into two sections:                                  |~n"),
  io:format("|1. collect IO information it take (2000 div 2) = 1000 ms by using recon:node_stats_list/2;                                         |~n"),
  io:format("|2. the time of collecting process info deps on which mode you choose:                                                              |~n"),
  io:format("| 2.1 if you use r mode's(recon:proc_count/2), it will be  very fast, the time can be ignored,                                      |~n"),
  io:format("| 2.2 if you use rr mode's(recon:proc_window/3), it will took (2 * 1000 - 1000 div 2) = 3000 ms.                                    |~n"),

  io:format("|\e[42mAbout o(OBSERVER)'s Command\e[49m                                                                                                        |~n"),
  io:format("|\e[48;2;80;80;80mr:5000\e[0m will switch mode to reduction(proc_count) and set the refresh  time to 5000ms                                               |~n"),
  io:format("|\e[48;2;80;80;80mrr:5000\e[0m will switch mode to reduction(proc_window) and set the refresh time to 5000ms                                              |~n"),
  io:format("|\e[48;2;80;80;80mobserver_cli:start(Node, Cookie, Interval) or observer_start:start(Node, Interval)\e[0m Remote node  support:                           |~n"),
  io:format("|\e[48;2;80;80;80mh(down),k(up)\e[0m to choose the rank process(yellow),it will jump to the process view(recon:info/1) when pressing the enter button     |~n"),
  io:format("|\e[42mReference\e[49m                                                                                                                          |~n"),
  io:format("|More infomation about recon:proc_count/2 and recon:proc_window/3 refer to https://github.com/ferd/recon/blob/master/src/recon.erl  |~n"),
  io:format("|Any issue please visit: https://github.com/zhongwencool/observer_cli/issues                                                        |~n").

draw_menu(Node) ->
  [Home, Ets, Alloc, Mnesia, Help]  = observer_cli_lib:get_menu_title(help),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Mnesia, "|", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
  Space = lists:duplicate(?BROAD - erlang:length(Title)    - erlang:length(UpTime)+ 110, " "),
  io:format("~s~n", [Title ++ Space ++ UpTime]).
