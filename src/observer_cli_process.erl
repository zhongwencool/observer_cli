-module(observer_cli_process).

-include("observer_cli.hrl").

-dialyzer([
    {nowarn_function, [
        render_worker/8,
        render_process_sections/3,
        render_reduction_memory/4,
        get_chart_format/1,
        chart_format/2
    ]}
]).

-export([start/3]).

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    collect_process_info/1,
    collect_process_messages/1,
    collect_process_dictionary/1,
    collect_process_stack/1,
    collect_process_state/1,
    chart_format/2,
    replace_first_line/2,
    render_process_sections/3,
    render_process_info/1,
    render_process_messages/1,
    render_process_dictionary/1,
    render_process_stack/1,
    render_process_state/2,
    render_stateless_view/4,
    render_link_monitor/3,
    render_reduction_memory/4,
    render_menu/3,
    render_footer/0,
    state_footer_text/1,
    render_worker/8,
    render_state/3,
    output_die_view/3,
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
    open_home(RenderPid, Opts);
handle_action(back, RenderPid, home, _Pid, Opts) ->
    open_home(RenderPid, Opts);
handle_action(back, RenderPid, plugin, _Pid, Opts) ->
    open_plugin(RenderPid, Opts);
handle_action(state_view, RenderPid, Type, Pid, Opts) ->
    erlang:send(RenderPid, state_view),
    wait_for_state_view(RenderPid, Type, Pid, Opts);
handle_action(ViewAction, RenderPid, Type, Pid, Opts) ->
    erlang:send(RenderPid, ViewAction),
    manager(RenderPid, Type, Pid, Opts).

open_plugin(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli_plugin:start(Opts).

open_home(RenderPid, Opts) ->
    erlang:exit(RenderPid, stop),
    observer_cli:start(Opts).

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
    case collect_process_info(Pid) of
        dead ->
            output_die_view(Pid, Type, Interval),
            next_draw_view(info, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        ProcessInfo ->
            {NewRedQ, NewMemQ, Lines} = render_process_sections(ProcessInfo, RedQ, MemQ),
            Menu = render_menu(info, Type, Interval),
            LastLine = render_footer(),
            ?output([?CURSOR_TOP, Menu, Lines, LastLine]),
            next_draw_view(info, Type, TimeRef, Interval, Pid, NewRedQ, NewMemQ, ManagerPid)
    end;
render_worker(message, Type, Interval, Pid, TimeRef, RedQ, MemQ, ManagerPid) ->
    case collect_process_messages(Pid) of
        {ok, MessagesInfo} ->
            Line = render_process_messages(MessagesInfo),
            ?output([?CURSOR_TOP, render_stateless_view(message, Type, Interval, Line)]),
            next_draw_view(message, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        dead ->
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
    case collect_process_dictionary(Pid) of
        {ok, DictionaryInfo} ->
            Line = render_process_dictionary(DictionaryInfo),
            ?output([?CURSOR_TOP, render_stateless_view(dict, Type, Interval, Line)]),
            next_draw_view(dict, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        dead ->
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
    case collect_process_stack(Pid) of
        {ok, #{pid := Pid, stack := Stack}} ->
            Prompt = io_lib:format("erlang:process_info(~p, current_stacktrace).      ~n", [Pid]),
            Line = [Prompt, render_process_stack(Stack)],
            ?output([?CURSOR_TOP, render_stateless_view(stack, Type, Interval, Line)]),
            next_draw_view(stack, Type, TimeRef, Interval, Pid, RedQ, MemQ, ManagerPid);
        dead ->
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

render_process_sections(
    #{
        process := ProcessView,
        links := Link,
        monitors := Monitors,
        monitored_by := MonitoredBy,
        reductions := Reductions,
        memory := Memory
    },
    RedQ,
    MemQ
) ->
    ProcessSection = render_process_info(ProcessView),
    LinkSection = render_link_monitor(Link, Monitors, MonitoredBy),
    {NewRedQ, NewMemQ, ReductionSection} = render_reduction_memory(Reductions, Memory, RedQ, MemQ),
    {NewRedQ, NewMemQ, [ProcessSection, LinkSection, ReductionSection]}.

render_stateless_view(View, Type, Interval, Line) ->
    Menu = render_menu(View, Type, Interval),
    LastLine = render_footer(),
    [Menu, Line, LastLine].

collect_process_info(Pid) ->
    ProcessInfo = recon:info(Pid),
    Meta = proplists:get_value(meta, ProcessInfo),
    case Meta of
        undefined ->
            dead;
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
            GarbageCollection = collect_process_gc(
                proplists:get_value(garbage_collection, MemoryUsed)
            ),

            Work = proplists:get_value(work, ProcessInfo),
            Reductions = proplists:get_value(reductions, Work),
            case collect_process_extra(Pid, WordSize) of
                dead ->
                    dead;
                Extra ->
                    ProcessView = maps:merge(
                        #{
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
                        Extra
                    ),
                    #{
                        process => ProcessView,
                        links => Link,
                        monitors => Monitors,
                        monitored_by => MonitoredBy,
                        reductions => Reductions,
                        memory => Memory
                    }
            end
    end.

collect_process_extra(Pid, WordSize) ->
    case
        erlang:process_info(Pid, [
            priority,
            stack_size,
            binary,
            catchlevel,
            suspending,
            error_handler
        ])
    of
        undefined ->
            dead;
        Info ->
            Binaries = proplists:get_value(binary, Info, []),
            #{
                priority => proplists:get_value(priority, Info),
                stack_size => proplists:get_value(stack_size, Info, 0) * WordSize,
                binary_refs => binary_refs_summary(Binaries),
                catchlevel => proplists:get_value(catchlevel, Info),
                suspending => proplists:get_value(suspending, Info, []),
                error_handler => proplists:get_value(error_handler, Info)
            }
    end.

binary_refs_summary(Binaries) ->
    Bytes = lists:foldl(
        fun
            ({_, Size, _}, Acc) when is_integer(Size) -> Acc + Size;
            (_, Acc) -> Acc
        end,
        0,
        Binaries
    ),
    {erlang:length(Binaries), Bytes}.

collect_process_gc(GarbageCollection) ->
    #{
        min_bin_vheap_size => proplists:get_value(min_bin_vheap_size, GarbageCollection),
        min_heap_size => proplists:get_value(min_heap_size, GarbageCollection),
        fullsweep_after => proplists:get_value(fullsweep_after, GarbageCollection),
        minor_gcs => proplists:get_value(minor_gcs, GarbageCollection)
    }.

collect_process_messages(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when Len =:= 0 ->
            {ok, #{pid => Pid, message_queue_len => Len, messages => []}};
        {message_queue_len, Len} when Len > 10000 ->
            {ok, #{pid => Pid, message_queue_len => Len, too_many => true, messages => []}};
        {message_queue_len, Len} ->
            {messages, Messages} = recon:info(Pid, messages),
            {ok, #{pid => Pid, message_queue_len => Len, messages => Messages}};
        undefined ->
            dead
    end.

render_process_messages(#{message_queue_len := 0}) ->
    "\e[32;1mNo messages were found.\e[0m\n";
render_process_messages(#{message_queue_len := Len, too_many := true}) ->
    io_lib:format("\e[31mToo many message(~w)!\e[0m~n", [Len]);
render_process_messages(#{pid := Pid, message_queue_len := Len, messages := Messages}) ->
    [
        io_lib:format("~p Message Len:~p~n", [Pid, Len]),
        truncate_str(Pid, Messages)
    ].

collect_process_dictionary(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, List} ->
            {ok, #{pid => Pid, dictionary => List}};
        undefined ->
            dead
    end.

render_process_dictionary(#{pid := Pid, dictionary := List}) ->
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
    [Line1, Line2].

collect_process_stack(Pid) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, Stack} ->
            {ok, #{pid => Pid, stack => Stack}};
        undefined ->
            dead
    end.

render_process_stack(Stack) ->
    {_, Line} =
        lists:foldr(
            fun({Mod, Func, Arity, Location}, {Nth, Acc}) ->
                Mfa = observer_cli_lib:mfa_to_list({Mod, Func, Arity}),
                File = proplists:get_value(file, Location, "undefined"),
                LineNo = proplists:get_value(line, Location, 0),
                FileLine = File ++ ":" ++ erlang:integer_to_list(LineNo),
                case Nth =:= 1 of
                    false -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62), ?NEW_LINE | Acc]};
                    true -> {Nth + 1, [?W(Mfa, 66), ?W(FileLine, 62) | Acc]}
                end
            end,
            {1, []},
            lists:sublist(Stack, 30)
        ),
    ?render(Line).

collect_process_state(Pid) ->
    recon:get_state(Pid, 2500).

render_process_state(Pid, State) ->
    Line = truncate_str(Pid, State),
    replace_first_line(Line, state_title(Pid)).

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

render_process_info(ProcessView) ->
    Meta = process_meta_fields(ProcessView),
    Memory = process_memory_fields(ProcessView),
    GC = process_gc_fields(ProcessView),
    Widths = process_info_widths([16, 42, 16, 12, 18, 12]),
    [
        render_process_info_title(Widths),
        render_process_info_rows(Meta, Memory, GC, Widths)
    ].

process_meta_fields(#{
    pid := Pid,
    registered_name := RegisteredName,
    group_leader := GroupLeader,
    status := Status,
    initial_call := InitialCall,
    priority := Priority,
    catchlevel := CatchLevel,
    suspending := Suspending,
    error_handler := ErrorHandler
}) ->
    PidStr = erlang:pid_to_list(Pid),
    Name =
        case RegisteredName of
            "" -> PidStr;
            _ -> PidStr ++ "/" ++ erlang:atom_to_list(RegisteredName)
        end,
    #{
        registered_name => Name,
        initial_call => observer_cli_lib:mfa_to_list(InitialCall),
        group_leader => erlang:pid_to_list(GroupLeader),
        status => Status,
        priority => Priority,
        catchlevel => CatchLevel,
        suspending => format_suspending(Suspending),
        error_handler => ErrorHandler
    }.

process_memory_fields(#{
    message_queue_len := MessageQueueLen,
    heap_size := HeapSize,
    total_heap_size := TotalHeapSize,
    stack_size := StackSize,
    binary_refs := BinaryRefs,
    trap_exit := TrapExit
}) ->
    MessageQueueLenColor =
        case MessageQueueLen > 0 of
            true -> ?RED;
            false -> ?GREEN
        end,
    #{
        message_queue_len => {erlang:integer_to_list(MessageQueueLen), MessageQueueLenColor},
        heap_size => HeapSize,
        total_heap_size => TotalHeapSize,
        stack_size => StackSize,
        binary_refs => BinaryRefs,
        trap_exit => TrapExit
    }.

format_binary_refs({Count, Bytes}) ->
    [erlang:integer_to_list(Count), $/, observer_cli_lib:to_byte(Bytes)].

format_suspending([]) ->
    "0";
format_suspending(Suspending) ->
    Items = [observer_cli_lib:to_list(P) || P <- lists:sublist(Suspending, 3)],
    [erlang:integer_to_list(erlang:length(Suspending)), $:, string:join(Items, ",")].

process_gc_fields(#{garbage_collection := GarbageCollection}) ->
    #{
        min_bin_vheap_size => maps:get(min_bin_vheap_size, GarbageCollection),
        min_heap_size => maps:get(min_heap_size, GarbageCollection),
        fullsweep_after => maps:get(fullsweep_after, GarbageCollection),
        minor_gcs => integer_to_list(maps:get(minor_gcs, GarbageCollection))
    }.

render_process_info_title([MetaW, MetaValueW, MemoryW, MemoryValueW, GcW, GcValueW]) ->
    ?render([
        ?GRAY_BG,
        ?W("Meta", MetaW),
        ?W("Value", MetaValueW),
        ?W("Memory Used", MemoryW),
        ?W("Value", MemoryValueW),
        ?W("Garbage Collection", GcW),
        ?W("Value", GcValueW)
    ]).

render_process_info_rows(Meta, Memory, GC, [
    MetaW, MetaValueW, MemoryW, MemoryValueW, GcW, GcValueW
]) ->
    #{
        registered_name := Name,
        initial_call := InitialCallStr,
        group_leader := GroupLeaderStr,
        status := Status,
        priority := Priority,
        catchlevel := CatchLevel,
        suspending := Suspending,
        error_handler := ErrorHandler
    } = Meta,
    #{
        message_queue_len := {MessageQueueLenStr, MessageQueueLenColor},
        heap_size := HeapSize,
        total_heap_size := TotalHeapSize,
        stack_size := StackSize,
        binary_refs := BinaryRefs,
        trap_exit := TrapExit
    } = Memory,
    #{
        min_bin_vheap_size := MinBinVHeapSize,
        min_heap_size := MinHeapSize,
        fullsweep_after := FullSweepAfter,
        minor_gcs := MinorGcs
    } = GC,
    ?render([
        ?W("registered_name", MetaW),
        ?W(Name, MetaValueW),
        ?W("msg_queue_len", MemoryW),
        ?W2(MessageQueueLenColor, MessageQueueLenStr, MemoryValueW + 1),
        " ",
        ?W("min_bin_vheap_size", GcW),
        ?W({byte, MinBinVHeapSize}, GcValueW),
        ?NEW_LINE,
        ?W("initial_call", MetaW),
        ?W(InitialCallStr, MetaValueW),
        ?W("heap_size", MemoryW),
        ?W({byte, HeapSize}, MemoryValueW),
        ?W("min_heap_size", GcW),
        ?W({byte, MinHeapSize}, GcValueW),
        ?NEW_LINE,
        ?W("group_leader", MetaW),
        ?W(GroupLeaderStr, MetaValueW),
        ?W("total_heap_size", MemoryW),
        ?W({byte, TotalHeapSize}, MemoryValueW),
        ?W("fullsweep_after", GcW),
        ?W(FullSweepAfter, GcValueW),
        ?NEW_LINE,
        ?W("status", MetaW),
        ?W(Status, MetaValueW),
        ?W("trap_exit", MemoryW),
        ?W(TrapExit, MemoryValueW),
        ?W("minor_gcs", GcW),
        ?W(MinorGcs, GcValueW),
        ?NEW_LINE,
        ?W("priority", MetaW),
        ?W(Priority, MetaValueW),
        ?W("stack_size", MemoryW),
        ?W({byte, StackSize}, MemoryValueW),
        ?W("", GcW),
        ?W("", GcValueW),
        ?NEW_LINE,
        ?W("catchlevel", MetaW),
        ?W(CatchLevel, MetaValueW),
        ?W("binary_refs", MemoryW),
        ?W(format_binary_refs(BinaryRefs), MemoryValueW),
        ?W("", GcW),
        ?W("", GcValueW),
        ?NEW_LINE,
        ?W("suspending", MetaW),
        ?W(Suspending, MetaValueW),
        ?W("", MemoryW),
        ?W("", MemoryValueW),
        ?W("", GcW),
        ?W("", GcValueW),
        ?NEW_LINE,
        ?W("error_handler", MetaW),
        ?W(ErrorHandler, MetaValueW),
        ?W("", MemoryW),
        ?W("", MemoryValueW),
        ?W("", GcW),
        ?W("", GcValueW)
    ]).

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
    LinkValueW = 112 + observer_cli_lib:layout_extra_width() + wide_fill(5),
    ?render([
        ?W(LinkInfo, 16),
        ?W(LinkStr, LinkValueW),
        ?NEW_LINE,
        ?W(MonitorInfo, 16),
        ?W(MonitorsStr, LinkValueW),
        ?NEW_LINE,
        ?W(MonitoredByInfo, 16),
        ?W(MonitoredByStr, LinkValueW)
    ]).

process_info_widths(Base) ->
    fill_last(observer_cli_lib:weighted_widths(Base, [0, 4, 0, 1, 0, 1]), wide_fill(5)).

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
    Extra = observer_cli_lib:layout_extra_width(),
    RedWidth = 120 + Extra + wide_fill(5),
    MemWidth = 124 + Extra + wide_fill(5),
    View = [
        io_lib:format("|Reductions: ~*.*s|~n", [RedWidth, RedWidth, get_chart_format(NewRed)]),
        io_lib:format("|Memory: ~*.*s|~n", [MemWidth, MemWidth, get_chart_format(NewMem)])
    ],
    {NewRed, NewMem, View}.

wide_fill(Amount) ->
    case observer_cli_lib:layout_extra_width() of
        0 -> 0;
        _ -> Amount
    end.

fill_last([Last], Amount) ->
    [Last + Amount];
fill_last([Width | Rest], Amount) ->
    [Width | fill_last(Rest, Amount)].

render_footer() ->
    observer_cli_lib:render_footer("q(quit)").

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
    TitleWidth =
        observer_cli_lib:layout_base_width() + 99 - erlang:length(UpTime) +
            observer_cli_lib:layout_extra_width(),
    observer_cli_lib:render_menu_header(Title, Text, TitleWidth).

get_menu_title(Type, Menu) ->
    MenuStr =
        case Menu of
            home -> "Home(H)";
            plugin -> "Back(B)"
        end,
    observer_cli_lib:menu_items(Type, [
        {back, MenuStr},
        {info, "Process Info(P)"},
        {message, "Messages(M)"},
        {dict, "Dictionary(D)"},
        {stack, "Current Stack(C)"},
        {state, "State(S)"}
    ]).

parse_cmd() ->
    parse_cmd_str(observer_cli_lib:to_list(io:get_line(""))).

parse_cmd_str(Key) ->
    case Key of
        Cmd when Cmd =:= "q\n"; Cmd =:= "Q\n" ->
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
            observer_cli_command:parse_integer(Number)
    end.

render_state(Pid, Type, Interval) ->
    Menu = render_menu(state, Type, Interval),
    PromptRes = io_lib:format("recon:get_state(~p, 2500).                            ~n", [Pid]),
    PromptBefore = [
        observer_cli_lib:ansi_green(
            io_lib:format("Waiting recon:get_state(~p, 2500) return...", [Pid])
        ),
        "\n"
    ],
    LastLine = render_footer(),
    ?output([?CURSOR_TOP, Menu, PromptBefore]),
    try
        State = collect_process_state(Pid),
        Nav = state_nav(Type),
        Line = render_process_state(Pid, State),
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
    Line = [observer_cli_lib:ansi_red(io_lib:format("Process(~p) has already died.", [Pid])), "\n"],
    LastLine = render_footer(),
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
    observer_cli_lib:render_footer(state_footer_text(Nav)).

state_footer_text(_Nav) ->
    "q(quit)    F/B(page forward/back)".

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
