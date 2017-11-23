%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_inet).

-include("observer_cli.hrl").
%% API
-export([start/1]).

-define(INET_COLUMN_WIDTH, 85).

-spec start(view_opts()) -> no_return.
start(#view_opts{inet = #inet{interval = Interval, func = Function, type = Type},
                 terminal_row = TerminalRow} = ViewOpts) ->
    Pid = spawn(fun() ->
        ?output(?CLEAR),
        render_worker(Function, Type, Interval, undefined, 0, TerminalRow)
                end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, ViewOpts = #view_opts{inet = InetOpts}) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ChildPid) of
        quit -> erlang:send(ChildPid, quit);
        {new_interval, NewInterval} ->
            erlang:send(ChildPid, {new_interval, NewInterval}),
            NewInet = InetOpts#inet{interval = NewInterval},
            manager(ChildPid, ViewOpts#view_opts{inet = NewInet});
        Func when Func =:= inet_count; Func =:= inet_window ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(ViewOpts#view_opts{inet = InetOpts#inet{func = Func}});
        Type when Type =:= recv_cnt; Type =:= recv_oct; Type =:= send_cnt;
            Type =:= send_oct; Type =:= cnt; Type =:= oct ->
            erlang:exit(ChildPid, stop),
            observer_cli_inet:start(ViewOpts#view_opts{inet = InetOpts#inet{type = Type}});
        _ -> manager(ChildPid, ViewOpts)
    end.

render_worker(Function, Type, Interval, LastTimeRef, Count, TerminalRow0) ->
    {ok, TerminalRow} =
        case TerminalRow0 of
            undefined -> io:rows();
            _ -> {ok, TerminalRow0}
        end,
    Rows = TerminalRow - 4,
    Text = get_refresh_str(Function, Type, Interval, Rows),
    Menu = observer_cli_lib:render_menu(inet, Text, 133),
    InetInfo = inet_info(Function, Type, Rows, Interval, Count),
    InetView = render_inet_rows(InetInfo, Type, Function, Interval, Rows),
    LastLine = render_last_line(Interval),
    ?output([?CURSOR_TOP, Menu, InetView, LastLine]),
    NewInterval = case Function of inet_count -> Interval; inet_window -> 10 end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, NewInterval),
    receive
        quit -> quit;
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(Function, Type, NewInterval, TimeRef, Count + 1, TerminalRow);
        _ -> render_worker(Function, Type, Interval, TimeRef, Count + 1, TerminalRow)
    end.

render_inet_rows([], Type, inet_count, _Interval, Rows) ->
    io_lib:format("Get nothing for recon:inet_count(~p, ~p)~n", [Type, Rows]);
render_inet_rows([], Type, inet_window, Interval, Rows) ->
    io_lib:format("Get nothing for recon:inet_window(~p, ~p, ~p)~n", [Type, Rows, Interval]);
render_inet_rows(Inets, Type, Function, _, _) ->
    NewType = case Type of
                  cnt -> "cnt(recv_cnt+send_cnt)";
                  oct -> "oct(recv_oct+send_oct)";
                  _ -> atom_to_list(Type)
              end,
    Title = ?render([?BLUE_BG,
        ?W("Port", 12), ?W("Name", 12),?W(NewType, 15), ?W("QueueSize", 23),
        ?W("Memory", 23), ?W("Input", 15), ?W("Output", 20),
        ?RESET_BG]),
    View =
        [begin
             {meta, Meta} = recon:port_info(Port, meta),
             {memory_used, MemoryUsed} = recon:port_info(Port, memory_used),
             {io, IO} = recon:port_info(Port, io),
             Name = observer_cli_lib:to_list(proplists:get_value(name, Meta)),
             Input = observer_cli_lib:to_list(proplists:get_value(input, IO)),
             Output = observer_cli_lib:to_list(proplists:get_value(output, IO)),
             QueueSize = observer_cli_lib:to_list(proplists:get_value(queue_size, MemoryUsed)),
             Memory = observer_cli_lib:to_list(proplists:get_value(memory, MemoryUsed)),
             NewValue =
                 case {Function, Info} of
                     {inet_window, [{Type, Value1}]} -> io_lib:format("Diff:~w Now:~w)", [Value, Value1]);
                     {inet_count, [{Type, Value}]} -> observer_cli_lib:to_list(Value);
                     {_, [{_, Value1}, {_, Value2}]} -> io_lib:format("~w(~w+~w)", [Value, Value1, Value2])
                 end,
             ?render([
                 ?W(observer_cli_lib:to_list(Port), 12), ?W(Name, 12),
                 ?W(NewValue, 15), ?W(QueueSize, 23),
                 ?W(Memory, 23), ?W(Input, 15), ?W(Output, 20)
             ])
         end || {Port, Value, Info} <- Inets],
    [Title|View].

render_last_line(Interval) ->
    Format = "i~w(Interval ~wms must>=1000) ic(inet_count) iw(inet_window) rc(recv_cnt) ro(recv_oct) sc(send_cnt) so(send_oct) cnt oct",
    Text = io_lib:format(Format, [Interval, Interval]),
    ?render([?UNDERLINE,?RED, "INPUT:", ?RESET, ?BLUE_BG, "q(quit) ",
        ?W(Text, ?COLUMN - 11), ?RESET_BG]).

get_refresh_str(inet_count, Type, Interval, Rows) ->
    io_lib:format("recon:inet_count(~p,~w) Interval:~wms", [Type, Rows, Interval]);
get_refresh_str(inet_window, Type, Interval, Rows) ->
    io_lib:format("recon:inet_window(~p,~w,~w) Interval:~wms", [Type, Rows, Interval, Interval]).

inet_info(inet_count, Type, Num, _, _) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, _, 0) -> recon:inet_count(Type, Num);
inet_info(inet_window, Type, Num, Ms, _) -> recon:inet_window(Type, Num, Ms).
