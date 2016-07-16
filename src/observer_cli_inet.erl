%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_inet).

-include("observer_cli.hrl").
%% API
-export([start/2]).
%%for rpc call
-export([get_inet_info/6]).

-define(INET_COLUMN_WIDTH, 85).

-spec start(atom(), view_opts()) -> no_return.
start(Node, #view_opts{inet = #inet{interval = Interval,
                                    rows = Rows,
                                    func = Function,
                                    type = Type}} = ViewOpts) ->
    ParentPid = self(),
    Pid = spawn(fun() -> observer_cli_lib:clear_screen(),
                         loop(Node, Function, Type, Interval, Rows, erlang:make_ref(), ParentPid, 0)
                end),
    waiting(Node, Pid, ViewOpts).

%%for fetching data from remote data by rpc:call/4
-spec get_inet_info(Node, Function, Type, Rows, Ms, Count) -> [tuple()] when
      Node:: atom(),
      Function:: atom(),
      Type:: atom(),
      Rows:: integer(),
      Count:: integer(),
      Ms:: integer().
get_inet_info(local_node, Function, Type, Rows, Ms, Count) -> 
    inet_info(Function, Type, Rows, Ms, Count);
get_inet_info(Node, Function, Type, Rows, Ms, Count) ->
    rpc:call(Node, ?MODULE, get_inet_info, [local_node, Function, Type, Rows, Ms, Count]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting(Node, ChildPid, ViewOpts = #view_opts{inet = InetOpts}) ->
    Input = observer_cli_lib:get_line(""),
    case  Input of
        "q\n" ->
            erlang:send(ChildPid, quit),
            "";
        "o\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli:start_node(Node, ViewOpts);
        "e\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_system:start(Node, ViewOpts);
        "h\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_help:start(Node, ViewOpts);
        "db\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_mnesia:start(Node, ViewOpts);
        "a\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_allocator:start(Node, ViewOpts);
        "ic\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{func = inet_count}});
        "iw\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{func = inet_window}});
        "rc\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = recv_cnt}});
        "ro\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = recv_oct}});
        "sc\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = send_cnt}});
        "so\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = send_oct}});
        "cnt\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = cnt}});
        "oct\n" ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(Node, ViewOpts#view_opts{inet = InetOpts#inet{type = oct}});
        [$i| Interval] ->
            case string:to_integer(Interval) of
                {error, no_integer} -> waiting(Node, ChildPid, ViewOpts);
                {NewInterval, _} when NewInterval >= ?INET_MIN_INTERVAL ->
                    erlang:send(ChildPid, {new_interval, NewInterval}),
                    waiting(Node, ChildPid, ViewOpts#view_opts{inet = InetOpts#inet{interval = NewInterval}});
                {_Interval, _} -> waiting(Node, ChildPid, ViewOpts)
            end;
        [$r, $o, $w| Rows] ->
            case string:to_integer(Rows) of
                {error, no_integer} -> waiting(Node, ChildPid, ViewOpts);
                {NewRows, _} ->
                    erlang:send(ChildPid, {new_rows, NewRows}),
                    waiting(Node, ChildPid, ViewOpts#view_opts{inet = InetOpts#inet{rows = NewRows}})
            end;
        _ -> waiting(Node, ChildPid, ViewOpts)
    end.

loop(Node, Function, Type, Interval, Rows, LastTimeRef, ParentPid, Count) ->
    observer_cli_lib:move_cursor_to_top_line(),
    draw_menu(Node, Function, Type, Interval, Rows),
    Inets = get_inet_info(Node, Function, Type, Rows, Interval, Count),
    draw_inet_rows(Inets, Type, Function, Interval, Rows),
    draw_last_line(Interval),
    erlang:cancel_timer(LastTimeRef),
    NextInterval = case Function of inet_count -> Interval; inet_window -> 10 end,
    TimeRef = erlang:send_after(NextInterval, self(), refresh),
    receive
        quit ->
            quit;
        {new_interval, NewInterval} -> 
            observer_cli_lib:clear_screen(),
            loop(Node, Function, Type, NewInterval, Rows, TimeRef, ParentPid, Count + 1);
        {new_rows, NewRows} -> 
            observer_cli_lib:clear_screen(),
            loop(Node, Function, Type, Interval, NewRows, TimeRef, ParentPid, Count + 1);
        _ ->
            loop(Node, Function, Type, Interval, Rows, TimeRef, ParentPid, Count + 1)
    end.

draw_menu(Node, Func, Type, Interval, Rows) ->
    Title  = observer_cli_lib:get_menu_title(inet),
    RefreshStr = get_refresh_str(Func, Type, Interval, Rows),
    UpTime = observer_cli_lib:green(" " ++ observer_cli_lib:uptime(Node)) ++ "|",
    NewTitle = lists:flatten([Title, RefreshStr]),
    SpaceLen = ?COLUMN_WIDTH - erlang:length(NewTitle) - erlang:length(UpTime) + 130,
    Space = case SpaceLen > 0 of  true -> lists:duplicate(SpaceLen, " "); false -> [] end,
    io:format("~s~n", [NewTitle ++ Space ++ UpTime]).

draw_inet_rows([], Type, inet_count, _Interval, Rows) ->
    io:format("Get nothing for recon:inet_count(~p, ~p)~n", [Type, Rows]);
draw_inet_rows([], Type, inet_window, Interval, Rows) ->
    io:format("Get nothing for recon:inet_window(~p, ~p, ~p)~n", [Type, Rows, Interval]);
draw_inet_rows(Inets, Type, _, _, _) ->
    NewType = case Type of
                  cnt -> "cnt(recv_cnt+send_cnt)";
                  oct -> "oct(recv_oct+send_oct)";
                  _ -> atom_to_list(Type)
              end,
    io:format("\e[0m\e[44m|~-12.12s|~12.12s| ~-28.28s|~15.15s| ~-15.15s|~20.20s| ~-20.20s\e[49m|~n",
              ["Port", "Name", NewType, "QueueSize", "Memory", "Input", "Output"]),
    [begin
         {meta, Meta} = recon:port_info(Port, meta),
         {memory_used, MemoryUsed} = recon:port_info(Port, memory_used),
         {io, IO} = recon:port_info(Port, io),
         Name = observer_cli_lib:to_list(proplists:get_value(name, Meta)),
         Input = observer_cli_lib:to_list(proplists:get_value(input, IO)),
         Output = observer_cli_lib:to_list(proplists:get_value(output, IO)),
         QueueSize = observer_cli_lib:to_list(proplists:get_value(queue_size, MemoryUsed)),
         Memory = observer_cli_lib:to_list(proplists:get_value(memory, MemoryUsed)),
         NewValue = case Info of
                        [{Type, Value}] -> observer_cli_lib:to_list(Value);
                        [{_, Value1}, {_, Value2}]-> io_lib:format("~w(~w+~w)", [Value, Value1, Value2])
                    end,
         io:format("|~-12.12s|~12.12s| ~-28.28s|~15.15s| ~-15.15s|~20.20s| ~-20.20s|~n",
                   [observer_cli_lib:to_list(Port), Name, NewValue, QueueSize, Memory, Input, Output])
     end|| {Port, Value, Info} <- Inets].

draw_last_line(Interval)  ->
    Format = "i~w(Interval ~wms must>=1000) ic(inet_count) iw(inet_window) rc(recv_cnt) ro(recv_oct) sc(send_cnt) so(send_oct) cnt oct",
    Text = io_lib:format(Format, [Interval, Interval]),
    io:format("|\e[31;1mINPUT: \e[0m\e[44m~-124.124s\e[49m|~n", [Text]).

get_refresh_str(inet_count, Type, Interval, Rows) ->
    io_lib:format("recon:inet_count(~p,~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_str(inet_window, Type, Interval, Rows) ->
    io_lib:format("recon:inet_window(~p,~w,~w) Interval:~wms", [Type, Rows, Interval, Interval]).

inet_info(inet_count, Type, Num, _, _) ->
    recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, _, 0) ->
    recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, Ms, _) ->
    recon:inet_window(Type, Num, Ms).

