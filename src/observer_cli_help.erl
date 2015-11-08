%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_help).


%% API
-export([start/0]).

-define(TOP_MIN_REFLUSH_INTERVAL, 5000).
-define(BROAD, 133).

start() ->
  Pid = spawn_link(fun() ->
    observer_cli_lib:clear_screen(),
    loop(?TOP_MIN_REFLUSH_INTERVAL) end),
  waiting(Pid).

waiting(Pid) ->
  Input = io:get_line(""),
  case  Input of
    "q\n" -> erlang:send(Pid, quit);
    "o\n" ->
      erlang:send(Pid, quit),
      observer_cli:start();
    "a\n" ->
      erlang:send(Pid, quit),
      observer_cli_allocator:start();
    _ -> waiting(Pid)
  end.

loop(Interval) ->
  observer_cli_lib:move_cursor_to_top_line(),
  draw_help(),
  erlang:send_after(Interval, self(), refresh),
  receive
    refresh -> loop(Interval);
    quit -> quit
  end.

draw_help() ->
  draw_menu(),
  Io = "o(OBSERVER)'s \"IO input/out\", \"Gc Count\", \"Gc Words Reclaimed\" only represent the increments between Interval,
   bytes in and out of the node, number of garbage colelctor runs, words of memory that were garbage collected, and
   the global reductions count for the node never stop increasing. Your can find the total bytes in and out in \"E(Ets)\".~n",
  io:format(Io),

  io:format("Any question or proposal please visit: https://github.com/zhongwencool/observer_cli/issues~n").

draw_menu() ->
  [Home, Ets, Alloc, Help]  = observer_cli_lib:get_menu_title(help),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "| ", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime()) ++ "|",
  Space = lists:duplicate(?BROAD - erlang:length(Title)    - erlang:length(UpTime)+ 90, " "),
  io:format("~s~n", [Title ++ Space ++ UpTime]).