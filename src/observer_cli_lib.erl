%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

%% API

-export([uptime/1]).
-export([move_cursor_to_top_line/0]).
-export([clear_screen/0]).
-export([float_to_list_with_two_digit/1]).

-spec move_cursor_to_top_line() -> ok.
move_cursor_to_top_line() ->
  io:format("\e[H").

-spec clear_screen() -> ok.
clear_screen() ->
  io:format("\e[H\e[J").

%% @doc  return format "124Days 12:12:12"
-spec uptime(pos_integer()) -> string().
uptime(UpTime) ->
  {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
  lists:flatten(io_lib:format("~pDays ~p:~p:~p", [D, H, M, S])).

%% @doc 0.98.2342 -> 98.23%, 1 -> 100.0%
-spec float_to_list_with_two_digit(float()) -> string().
float_to_list_with_two_digit(Float) ->
  Val = trunc(Float*10000),
  Integer = Val div 100,
  Decmial = Val - Integer * 100,
  case Integer of
    100 -> "100.0%";
    _ -> lists:flatten(io_lib:format("~2..0w.~2..0w%", [Integer, Decmial]))
  end.

