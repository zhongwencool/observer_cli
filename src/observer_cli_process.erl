-module(observer_cli_process).

-include("observer_cli.hrl").

-dialyzer([
    {nowarn_function, [
        render_worker/8, render_reduction_memory/4, get_chart_format/1, chart_format/2
    ]}
]).

-export([start/3]).

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    chart_format/2,
    replace_first_line/2,
    render_process_info/1,
    render_link_monitor/3,
    render_reduction_memory/4,
    render_menu/3,
    render_last_line/0,
    render_footer_line/2,
    state_footer_text/1,
    render_worker/8,
    render_state/3,
    state_nav/1,
    state_title/1,
    state_footer/2,
    truncate_str/2,
    format_mod/1,
    format/1
]).
-endif.

%% lists:foldl(fun(_X, Acc) -> queue:in('NaN', Acc) end, queue:new(), lists:seq(1, 5))
-define(INIT_QUEUE, {['NaN', 'NaN', 'NaN', 'NaN'], ['NaN']}).

-spec start(Type, pid(), view_opts()) -> no_return() when Type :: home | plugin.
start(Type, Pid, Opts) ->
    #view_opts{process = #process{interval = RefreshMs}} = Opts,
    ManagerPid = self(),
    RenderPid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(
            info,
            Type,
            RefreshMs,
            Pid,
            ?INIT_TIME_REF,
            ?INIT_QUEUE,
            ?INIT_QUEUE,
            ManagerPid
        )
    end),
    manager(RenderPid, Type, Pid, Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(RenderPid, Type, Pid, Opts) ->
    handle_action(parse_cmd(), RenderPid, Type, Pid, Opts).

handle_action(quit, RenderPid, _Type, _Pid, _Opts) ->
    erlang:exit(RenderPid, stop);
handle_action(
    {new_interval, NewInterval}, RenderPid, Type, Pid, #view_opts{process = ProcOpts} = Opts
) ->
    erlang:send(RenderPid, {new_interval, NewInterval}),
    NewOpt = Opts#view_opts{process = ProcOpts#process{interval = NewInterval}},
    manager(RenderPid, Type, Pid, NewOpt);
handle_action(home, RenderPid, _Type, _Pid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli:start(Opts);
handle_action(back, RenderPid, home, _Pid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli:start(Opts);
handle_action(back, RenderPid, plugin, _Pid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli_plugin:start(Opts);
handle_action(state_view, RenderPid, Type, Pid, Opts) ->
    erlang:send(RenderPid, state_view),
    wait_for_state_view(RenderPid, Type, Pid, Opts);
handle_action(ViewAction, RenderPid, Type, Pid, Opts) ->
    erlang:send(RenderPid, ViewAction),
    manager(RenderPid, Type, Pid, Opts).

wait_for_state_view(RenderPid, Type, Pid, Opts) ->
    receive
        {state_view_done, {ok, none}} ->
            manager(RenderPid, Type, Pid, Opts);
        {state_view_done, {ok, Action}} ->
            handle_action(Action, RenderPid, Type, Pid, Opts);
        {state_view_done, error} ->
            manager(RenderPid, Type, Pid, Opts)
    end.

render_worker(info, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    ProcessInfo = recon:info(Pid),
    Meta = proplists:get_value(meta, ProcessInfo),
    case Meta of
        undefined ->
            output_die_view(Pid, Type, Interval),
            next_draw_view(info, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        _ ->
            WordSize = erlang:system_info(wordsize),

            RegisteredName = proplists:get_value(registered_name, Meta),
            GroupLeader = proplists:get_value(group_leader, Meta),
            Status = proplists:get_value(status, Meta),

            Signals = proplists:get_value(signals, ProcessInfo),
            Link = proplists:get_value(links, Signals),
            Monitors = proplists:get_value(monitors, Signals),
            MonitoredBy = proplists:get_value(monitored_by, Signals),
            TrapExit = proplists:get_value(trap_exit, Signals),

            Location = proplists:get_value(location, ProcessInfo),
            InitialCall = proplists:get_value(initial_call, Location),

            MemoryUsed = proplists:get_value(memory_used, ProcessInfo),
            Memory = proplists:get_value(memory, MemoryUsed),
            MessageQueueLen = proplists:get_value(message_queue_len, MemoryUsed),
            HeapSize = proplists:get_value(heap_size, MemoryUsed, 0) * WordSize,
            TotalHeapSize = proplists:get_value(total_heap_size, MemoryUsed, 0) * WordSize,
            GarbageCollection = proplists:get_value(garbage_collection, MemoryUsed),

            Work = proplists:get_value(work, ProcessInfo),
            Reductions = proplists:get_value(reductions, Work),

            Menu = render_menu(info, Type, Interval),

            ProcessView = #{
                pid => Pid,
                registered_name => RegisteredName,
                group_leader => GroupLeader,
                status => Status,
                trap_exit => TrapExit,
                initial_call => InitialCall,
                message_queue_len => MessageQueueLen,
                heap_size => HeapSize,
                total_heap_size => TotalHeapSize,
                garbage_collection => GarbageCollection
            },
            Line1 = render_process_info(ProcessView),

            Line2 = render_link_monitor(Link, Monitors, MonitoredBy),

            {NewRedQ, NewMemQ, Line3} = render_reduction_memory(Reductions, Memory, RedQ, MemQ),

            LastLine = render_last_line(),

            ?output([?CURSOR_TOP, Menu, Line1, Line2, Line3, LastLine]),
            next_draw_view(info, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid)
    end;
render_worker(message, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            Line =
                if
                    Len =:= 0 ->
                        "\e[32;1mNo messages were found.\e[0m\n";
                    Len > 10000 ->
                        io_lib:format("\e[31mToo many message(~w)!\e[0m~n", [Len]);
                    true ->
                        {messages, Messages} = recon:info(Pid, messages),
                        [
                            io_lib:format("~p Message Len:~p~n", [Pid, Len]),
                            truncate_str(Pid, Messages)
                        ]
                end,
            Menu = render_menu(message, Type, Interval),
            LastLine = render_last_line(),
            ?output([?CURSOR_TOP, Menu, Line, LastLine]),
            next_draw_view(message, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        undefined ->
            render_worker(
                info,
                Type,
                Interval,
                Pid,
                ?INIT_TIME_REF,
                ?INIT_QUEUE,
                ?INIT_QUEUE,
                ManagerPid
            )
    end;
render_worker(dict, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, List} ->
            Len = erlang:length(List),
            Line1 = io_lib:format(
                "erlang:process_info(~p, dictionary). dictionary_len:~p       ~n",
                [Pid, Len]
            ),
            Line2 =
                case Len of
                    0 -> "\e[32;1mNo dictionary was found\e[0m\n";
                    _ -> truncate_str(Pid, List)
                end,
            Menu = render_menu(dict, Type, Interval),
            LastLine = render_last_line(),
            ?output([?CURSOR_TOP, Menu, Line1, Line2, LastLine]),
            next_draw_view(dict, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        undefined ->
            render_worker(
                info,
                Type,
                Interval,
                Pid,
                ?INIT_TIME_REF,
                ?INIT_QUEUE,
                ?INIT_QUEUE,
                ManagerPid
            )
    end;
render_worker(stack, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, Stack} ->
            Menu = render_menu(stack, Type, Interval),
            Prompt = io_lib:format("erlang:process_info(~p, current_stacktrace).      ~n", [Pid]),
            LastLine = render_last_line(),
            {_, Line} =
                lists:foldr(
                    fun({Mod, Func, Arity, Location}, {Nth, Acc}) ->
                        Mfa = observer_cli_lib:mfa_to_list({Mod, Func, Arity}),
                        File = proplists:get_value(file, Location, "undefined"),
                        Line = proplists:get_value(line, Location, 0),
                        FileLine = File ++ ":" ++ erlang:integer_to_list(Line),
                        case Nth =:= 1 of
                            false -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62), ?NEW_LINE | Acc]};
                            true -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62) | Acc]}
                        end
                    end,
                    {1, []},
                    lists:sublist(Stack, 30)
                ),
            ?output([?CURSOR_TOP, Menu, Prompt, ?render(Line), LastLine]),
            next_draw_view(stack, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        undefined ->
            render_worker(
                info,
                Type,
                Interval,
                Pid,
                ?INIT_TIME_REF,
                ?INIT_QUEUE,
                ?INIT_QUEUE,
                ManagerPid
            )
    end;
render_worker(state, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    Result = render_state(Pid, Type, Interval),
    erlang:send(ManagerPid, {state_view_done, Result}),
    case Result of
        {ok, _Action} ->
            next_draw_view(state, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        error ->
            next_draw_view_2(state, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid)
    end.

%% state_view is static. user left state view and may stay long after. no need for redraw
next_draw_view(state, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid) ->
    observer_cli_lib:flush_redraw_timer(TimeRef),
    next_draw_view_2(state, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid);
next_draw_view(Status, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid) ->
    NewTimeRef = observer_cli_lib:next_redraw(TimeRef, Interval),
    next_draw_view_2(Status, Type, NewTimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid).

next_draw_view_2(Status, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid) ->
    receive
        quit ->
            exit(stop);
        {new_interval, NewInterval} ->
            ?output(?CLEAR),
            render_worker(Status, Type, NewInterval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        info_view ->
            ?output(?CLEAR),
            render_worker(info, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        message_view ->
            ?output(?CLEAR),
            render_worker(message, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        dict_view ->
            ?output(?CLEAR),
            render_worker(dict, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        stack_view ->
            ?output(?CLEAR),
            render_worker(stack, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        state_view ->
            %% state view is static. user chose state view - no need for redraw
            observer_cli_lib:flush_redraw_timer(TimeRef),
            ?output(?CLEAR),
            render_worker(state, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid);
        redraw ->
            render_worker(Status, Type, Interval, Pid, TimeRef, NewRedQ, NewMemQ, ManagerPid)
    end.

render_process_info(#{
    pid := Pid,
    registered_name := RegisteredName,
    group_leader := GroupLeader,
    status := Status,
    trap_exit := TrapExit,
    initial_call := InitialCall,
    message_queue_len := MessageQueueLen,
    heap_size := HeapSize,
    total_heap_size := TotalHeapSize,
    garbage_collection := GarbageCollection
}) ->
    MinBinVHeapSize = proplists:get_value(min_bin_vheap_size, GarbageCollection),
    MinHeapSize = proplists:get_value(min_heap_size, GarbageCollection),
    FullSweepAfter = proplists:get_value(fullsweep_after, GarbageCollection),
    MinorGcs = integer_to_list(proplists:get_value(minor_gcs, GarbageCollection)),

    InitialCallStr = observer_cli_lib:mfa_to_list(InitialCall),
    GroupLeaderStr = erlang:pid_to_list(GroupLeader),
    PidStr = erlang:pid_to_list(Pid),
    Name =
        case RegisteredName of
            "" -> PidStr;
            _ -> PidStr ++ "/" ++ erlang:atom_to_list(RegisteredName)
        end,
    MessageQueueLenStr = erlang:integer_to_list(MessageQueueLen),
    MessageQueueLenColor =
        case MessageQueueLen > 0 of
            true -> ?RED;
            false -> ?GREEN
        end,

    [
        ?render([
            ?GRAY_BG,
            ?W("Meta", 16),
            ?W("Value", 42),
            ?W("Memory Used", 16),
            ?W("Value", 12),
            ?W("Garbage Collection", 18),
            ?W("Value", 12)
        ]),
        ?render([
            ?W("registered_name", 16),
            ?W(Name, 42),
            ?W("msg_queue_len", 16),
            ?W2(MessageQueueLenColor, MessageQueueLenStr, 13),
            ?W(" min_bin_vheap_size", 19),
            ?W({byte, MinBinVHeapSize}, 12),
            ?NEW_LINE,
            ?W("initial_call", 16),
            ?W(InitialCallStr, 42),
            ?W("heap_size", 16),
            ?W({byte, HeapSize}, 12),
            ?W("min_heap_size", 18),
            ?W({byte, MinHeapSize}, 12),
            ?NEW_LINE,
            ?W("group_leader", 16),
            ?W(GroupLeaderStr, 42),
            ?W("total_heap_size", 16),
            ?W({byte, TotalHeapSize}, 12),
            ?W("fullsweep_after", 18),
            ?W(FullSweepAfter, 12),
            ?NEW_LINE,
            ?W("status", 16),
            ?W(Status, 42),
            ?W("trap_exit", 16),
            ?W(TrapExit, 12),
            ?W("minor_gcs", 18),
            ?W(MinorGcs, 12)
        ])
    ].

render_link_monitor(Link, Monitors, MonitoredBy) ->
    LinkStr = [
        begin
            observer_cli_lib:to_list(P)
        end
     || P <- lists:sublist(Link, 30)
    ],
    MonitorsStr = [
        begin
            case P of
                {process, {RegName, Node}} ->
                    observer_cli_lib:to_list(RegName) ++ "/" ++ observer_cli_lib:to_list(Node);
                {process, Pid} ->
                    observer_cli_lib:to_list(Pid);
                {port, {RegName, Node}} ->
                    observer_cli_lib:to_list(RegName) ++ "/" ++ observer_cli_lib:to_list(Node);
                {port, Port} ->
                    observer_cli_lib:to_list(Port)
            end
        end
     || P <- lists:sublist(Monitors, 30)
    ],
    MonitoredByStr = [
        begin
            observer_cli_lib:to_list(P)
        end
     || P <- lists:sublist(MonitoredBy, 30)
    ],
    LinkInfo = "Links(" ++ erlang:integer_to_list(erlang:length(Link)) ++ ")",
    MonitorInfo = "Monitors(" ++ erlang:integer_to_list(erlang:length(Monitors)) ++ ")",
    MonitoredByInfo = "MonitoredBy(" ++ erlang:integer_to_list(erlang:length(MonitoredBy)) ++ ")",
    ?render([
        ?W(LinkInfo, 16),
        ?W(LinkStr, 112),
        ?NEW_LINE,
        ?W(MonitorInfo, 16),
        ?W(MonitorsStr, 112),
        ?NEW_LINE,
        ?W(MonitoredByInfo, 16),
        ?W(MonitoredByStr, 112)
    ]).

render_reduction_memory(Reduction, Memory, ReductionQ, MemoryQ) ->
    {NewRed, NewMem} =
        case queue:len(ReductionQ) >= 20 of
            true ->
                RestRed = queue:tail(ReductionQ),
                RestMem = queue:tail(MemoryQ),
                {queue:in(Reduction, RestRed), queue:in(Memory, RestMem)};
            false ->
                {queue:in(Reduction, ReductionQ), queue:in(Memory, MemoryQ)}
        end,
    View = [
        io_lib:format("|Reductions: ~120.120s|~n", [get_chart_format(NewRed)]),
        io_lib:format("|Memory: ~124.124s|~n", [get_chart_format(NewMem)])
    ],
    {NewRed, NewMem, View}.

render_last_line() ->
    io_lib:format("|\e[7mq(quit) ~124.124s\e[0m|~n", [" "]).

get_chart_format(Queue) ->
    List = queue:to_list(Queue),
    chart_format(List, "").

chart_format([_R], Lines) ->
    Lines;
chart_format([R, R | RestRed], Lines) ->
    chart_format([R | RestRed], Lines ++ observer_cli_lib:to_list(R) ++ "->");
chart_format([R1, R2 | RestRed], Lines) when R1 > R2 ->
    chart_format([R2 | RestRed], Lines ++ observer_cli_lib:to_list(R1) ++ "->");
chart_format([R1, R2 | RestRed], Lines) when R1 < R2 ->
    chart_format([R2 | RestRed], Lines ++ observer_cli_lib:to_list(R1) ++ "->").

render_menu(Type, Menu, Interval) ->
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Title = get_menu_title(Type, Menu),
    UpTime = observer_cli_lib:uptime(),
    TitleWidth = ?COLUMN + 104 - erlang:length(UpTime),
    ?render([?W([Title | Text], TitleWidth) | UpTime]).

get_menu_title(Type, Menu) ->
    MenuStr =
        case Menu of
            home -> "Home(H)";
            plugin -> "Back(B)"
        end,
    [Home, Process, Messages, Dict, Stack, State] = get_menu_title2(Type, MenuStr),
    [Home, "|", Process, "|", Messages, "|", Dict, "|", Stack, "|", State, "|"].

get_menu_title2(info, Menu) ->
    [
        ?UNSELECT(Menu),
        ?SELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(message, Menu) ->
    [
        ?UNSELECT(Menu),
        ?UNSELECT("Process Info(P)"),
        ?SELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(dict, Menu) ->
    [
        ?UNSELECT(Menu),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?SELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(stack, Menu) ->
    [
        ?UNSELECT(Menu),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?SELECT("Current Stack(C)"),
        ?UNSELECT("State(S)")
    ];
get_menu_title2(state, Menu) ->
    [
        ?UNSELECT(Menu),
        ?UNSELECT("Process Info(P)"),
        ?UNSELECT("Messages(M)"),
        ?UNSELECT("Dictionary(D)"),
        ?UNSELECT("Current Stack(C)"),
        ?SELECT("State(S)")
    ].

parse_cmd() ->
    parse_cmd_str(observer_cli_lib:to_list(io:get_line(""))).

parse_cmd_str(Key) ->
    case Key of
        "q\n" ->
            quit;
        "Q\n" ->
            quit;
        "P\n" ->
            info_view;
        "M\n" ->
            message_view;
        "D\n" ->
            dict_view;
        "C\n" ->
            stack_view;
        "S\n" ->
            state_view;
        "H\n" ->
            home;
        "B\n" ->
            back;
        %% {error, estale}|{error, terminated}
        {error, _Reason} ->
            quit;
        Number ->
            observer_cli_lib:parse_integer(Number)
    end.

render_state(Pid, Type, Interval) ->
    Menu = render_menu(state, Type, Interval),
    PromptRes = io_lib:format("recon:get_state(~p, 2500).                            ~n", [Pid]),
    PromptBefore = io_lib:format("\e[32;1mWaiting recon:get_state(~p, 2500) return...\e[0m~n", [Pid]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, PromptBefore]),
    try
        State = recon:get_state(Pid, 2500),
        Nav = state_nav(Type),
        Line0 = truncate_str(Pid, State),
        Line = replace_first_line(Line0, state_title(Pid)),
        Footer = state_footer(Menu, Nav),
        Action = print_with_less(Line, Menu, Nav, Footer),
        case Action of
            quit ->
                {ok, quit};
            _ ->
                ?output([?CURSOR_TOP, Menu, PromptRes, "", LastLine]),
                {ok, Action}
        end
    catch
        Class:Reason:Stacktrace ->
            log_render_state_error(Class, Reason, Stacktrace, Pid, Type, Interval),
            Error =
                "Information could not be retrieved, system messages may not be handled by this process.\n",
            ?output([?CURSOR_TOP, Menu, PromptRes, Error, LastLine]),
            error
    end.

log_render_state_error(Class, Reason, Stacktrace, Pid, Type, Interval) ->
    error_logger:warning_msg(
        "observer_cli render_state failed: class=~p reason=~p stacktrace=~p pid=~p type=~p interval=~p~n",
        [Class, Reason, Stacktrace, Pid, Type, Interval]
    ).

output_die_view(Pid, Type, Interval) ->
    Menu = render_menu(info, Type, Interval),
    Line = io_lib:format("\e[31mProcess(~p) has already died.\e[0m~n", [Pid]),
    LastLine = render_last_line(),
    ?output([?CURSOR_TOP, Menu, Line, LastLine]).

print_with_less(Input, Menu, Nav, Footer) ->
    observer_cli_lib:pipe({Input, Menu, Nav, Footer}, [
        fun less_client:init/1,
        fun less_client:main/1
    ]).

state_nav(Type) ->
    Base = #{
        "q\n" => quit,
        "Q\n" => quit,
        "H\n" => home,
        "P\n" => info_view,
        "M\n" => message_view,
        "D\n" => dict_view,
        "C\n" => stack_view
    },
    case Type of
        plugin -> maps:put("B\n", back, Base);
        home -> Base
    end.

state_title(Pid) ->
    io_lib:format("recon:get_state(~p, 2500).", [Pid]).

replace_first_line(Line, NewLine) ->
    case string:split(Line, "\n", leading) of
        [_First, Rest] -> NewLine ++ "\n" ++ Rest;
        [_Only] -> NewLine ++ "\n"
    end.

state_footer(_Menu, Nav) ->
    Text = state_footer_text(Nav),
    render_footer_line(Text, ?COLUMN).

state_footer_text(_Nav) ->
    "q(quit)    F/B(page forward/back)".

render_footer_line(Text, Width) ->
    InnerWidth = Width - 3,
    Padding = lists:duplicate(InnerWidth - erlang:length(Text), $\s),
    ["|", ?GRAY_BG, Text, Padding, " ", ?RESET, "|", "\n"].

truncate_str(Pid, Term) ->
    State = #{
        pid => Pid,
        term => Term,
        %% we need default mod, cause user can override conf
        formatter_default => observer_cli_formatter_default,
        formatter => undefined
    },
    observer_cli_lib:pipe(State, [
        fun format_mod/1,
        fun format/1
    ]).

format_mod(State) ->
    #{formatter_default := FormatModDefault} = State,
    observer_cli_lib:pipe(State, [
        fun(StateAcc) ->
            Formatter = application:get_env(observer_cli, formatter, ?DEFAULT_FORMATTER),
            StateAcc#{formatter => Formatter}
        end,
        fun(StateAcc) ->
            #{formatter := Formatter} = StateAcc,
            StateAcc#{formatter => maps:get(mod, Formatter, FormatModDefault)}
        end
    ]).

format(
    State = #{formatter := FormatModDefault, formatter_default := FormatModDefault}
) ->
    #{pid := Pid, term := Term} = State,
    observer_cli_formatter:format(FormatModDefault, Pid, Term);
format(State) ->
    #{
        pid := Pid,
        term := Term,
        formatter := FormatMod,
        formatter_default := FormatModDefault
    } =
        State,
    try
        observer_cli_formatter:format(FormatMod, Pid, Term)
    catch
        _:_ -> observer_cli_formatter:format(FormatModDefault, Pid, Term)
    end.
