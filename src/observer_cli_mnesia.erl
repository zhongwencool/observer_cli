%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_mnesia).

%% API
-export([start/1]).

-ifdef(TEST).
-export([
    collect_mnesia_info/2,
    collect_mnesia_render_info/4,
    render_mnesia/4,
    with_storage_type/3
]).
-endif.

-include("observer_cli.hrl").

-define(LAST_LINE,
    "q(quit) s(sort by size) m(sort by memory) pd/pu(page:down/up) F/B(forward/back)"
    " hide(switch between hide and display system table)"
).

-spec start(view_opts()) -> any().
start(
    #view_opts{
        db = #db{
            interval = MillSecond,
            hide_sys = HideSys,
            cur_page = CurPage,
            attr = Attr
        },
        auto_row = AutoRow
    } = HomeOpts
) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(MillSecond, ?INIT_TIME_REF, HideSys, AutoRow, Attr, CurPage)
    end),
    manager(Pid, HomeOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

manager(ChildPid, #view_opts{db = DBOpts = #db{cur_page = CurPage, hide_sys = Hide}} = HomeOpts) ->
    case observer_cli_lib:parse_cmd(HomeOpts, ?MODULE, [ChildPid]) of
        quit ->
            erlang:send(ChildPid, quit);
        {new_interval, NewMs} = Msg ->
            erlang:send(ChildPid, Msg),
            manager(ChildPid, HomeOpts#view_opts{db = DBOpts#db{interval = NewMs}});
        hide ->
            NewHide = not Hide,
            erlang:send(ChildPid, {system_table, NewHide}),
            manager(ChildPid, HomeOpts#view_opts{db = DBOpts#db{hide_sys = NewHide}});
        size ->
            restart(ChildPid, HomeOpts#view_opts{db = DBOpts#db{attr = size}});
        %% Home
        {func, proc_count, memory} ->
            restart(ChildPid, HomeOpts#view_opts{db = DBOpts#db{attr = memory}});
        page_down_top_n ->
            restart_page(ChildPid, HomeOpts, CurPage, 1);
        page_up_top_n ->
            restart_page(ChildPid, HomeOpts, CurPage, -1);
        _ ->
            manager(ChildPid, HomeOpts)
    end.

restart_page(ChildPid, HomeOpts = #view_opts{db = DBOpts}, CurPage, Delta) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    restart(ChildPid, HomeOpts#view_opts{db = DBOpts#db{cur_page = NewPage}}).

restart(ChildPid, HomeOpts) ->
    observer_cli_lib:exit_processes([ChildPid]),
    start(HomeOpts).

render_worker(Interval, LastTimeRef, HideSystemTable, AutoRow, Attr, CurPage) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 5, 0),
    Text =
        "Interval: " ++
            integer_to_list(Interval) ++
            "ms" ++
            " HideSystemTable:" ++ atom_to_list(HideSystemTable),
    Menu = observer_cli_lib:render_top_menu(mnesia, Text),
    LastLine = observer_cli_lib:render_footer(?LAST_LINE),
    case collect_mnesia_render_info(HideSystemTable, Attr, Rows, CurPage) of
        {error, Reason} ->
            ErrInfo = io_lib:format("Mnesia Error   ~p~n", [Reason]),
            ?output([?CURSOR_TOP, Menu, ErrInfo, LastLine]);
        MnesiaInfo ->
            Info = render_mnesia(MnesiaInfo, Attr),
            ?output([?CURSOR_TOP, Menu, Info, LastLine])
    end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit ->
            quit;
        {new_interval, NewInterval} ->
            render_worker(NewInterval, TimeRef, HideSystemTable, AutoRow, Attr, CurPage);
        {system_table, NewHideSystemTable} ->
            render_worker(Interval, TimeRef, NewHideSystemTable, AutoRow, Attr, CurPage);
        _ ->
            render_worker(Interval, TimeRef, HideSystemTable, AutoRow, Attr, CurPage)
    end.

collect_mnesia_render_info(HideSys, Attr, Rows, CurPage) ->
    case collect_mnesia_info(HideSys, Attr) of
        {error, _Reason} = Error -> Error;
        MnesiaList -> observer_cli_lib:sublist(MnesiaList, Rows, CurPage)
    end.

render_mnesia({_StartPos, SortMnesia}, Attr) ->
    {MemColor, SizeColor} =
        case Attr of
            memory -> {?RED_BG, ?GRAY_BG};
            _ -> {?GRAY_BG, ?RED_BG}
        end,
    [
        NameTitleW,
        MemoryTitleW,
        SizeTitleW,
        TypeTitleW,
        StorageTitleW,
        OwnerTitleW,
        IndexTitleW,
        RegNameTitleW
    ] =
        mnesia_title_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Name", NameTitleW),
        ?UNDERLINE,
        ?W2(MemColor, "    Memory    ", MemoryTitleW),
        ?UNDERLINE,
        ?W2(SizeColor, "Size", SizeTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Type", TypeTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Storage", StorageTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Owner", OwnerTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Index", IndexTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Reg_name", RegNameTitleW)
    ]),
    [NameW, MemoryW, SizeW, TypeW, StorageW, OwnerW, IndexW, RegNameW] =
        mnesia_row_widths(),
    View = [
        begin
            Name = proplists:get_value(name, Mnesia),
            Memory = proplists:get_value(memory, Mnesia),
            Size = proplists:get_value(size, Mnesia),
            Type = proplists:get_value(type, Mnesia),
            RegName = proplists:get_value(reg_name, Mnesia),
            Index = proplists:get_value(index, Mnesia),
            Owner = proplists:get_value(owner, Mnesia),
            Storage = proplists:get_value(storage, Mnesia),
            ?render([
                ?W(Name, NameW),
                ?W({byte, Memory}, MemoryW),
                ?W(Size, SizeW),
                ?W(Type, TypeW),
                ?W(Storage, StorageW),
                ?W(Owner, OwnerW),
                ?W(Index, IndexW),
                ?W(RegName, RegNameW)
            ])
        end
     || {_, _, Mnesia} <- SortMnesia
    ],
    [Title | View].

-ifdef(TEST).
render_mnesia(MnesiaList, Attr, Rows, CurPage) ->
    render_mnesia(observer_cli_lib:sublist(MnesiaList, Rows, CurPage), Attr).
-endif.

mnesia_title_widths() ->
    observer_cli_lib:weighted_widths(
        [25, 16, 16, 12, 15, 14, 11, 20],
        [4, 0, 0, 0, 1, 1, 0, 4]
    ).

mnesia_row_widths() ->
    observer_cli_lib:weighted_widths(
        [24, 14, 14, 10, 13, 12, 9, 19],
        [4, 0, 0, 0, 1, 1, 0, 4]
    ).

mnesia_tables() ->
    [
        ir_AliasDef,
        ir_ArrayDef,
        ir_AttributeDef,
        ir_ConstantDef,
        ir_Contained,
        ir_Container,
        ir_EnumDef,
        ir_ExceptionDef,
        ir_IDLType,
        ir_IRObject,
        ir_InterfaceDef,
        ir_ModuleDef,
        ir_ORB,
        ir_OperationDef,
        ir_PrimitiveDef,
        ir_Repository,
        ir_SequenceDef,
        ir_StringDef,
        ir_StructDef,
        ir_TypedefDef,
        ir_UnionDef,
        logTable,
        logTransferTable,
        mesh_meas,
        mesh_type,
        mnesia_clist,
        orber_CosNaming,
        orber_objkeys,
        user
    ].

collect_mnesia_info(HideSys, Attr) ->
    Owner = ets:info(schema, owner),
    case Owner of
        undefined -> {error, "Mnesia is not running on: " ++ atom_to_list(node())};
        _ -> get_table_list2(Owner, HideSys, Attr)
    end.

-dialyzer([{nowarn_function, [get_table_list2/3]}]).
get_table_list2(Owner, HideSys, Attr) ->
    {registered_name, RegName} = process_info(Owner, registered_name),
    WordSize = erlang:system_info(wordsize),
    CollectFun = fun(Id, Acc) ->
        case HideSys andalso ordsets:is_element(Id, mnesia_tables()) orelse Id =:= schema of
            %% ignore system table
            true ->
                Acc;
            false ->
                try
                    Size = mnesia:table_info(Id, size),
                    Storage = mnesia:table_info(Id, storage_type),
                    RawMemory = mnesia:table_info(Id, memory),
                    case is_integer(Size) andalso is_integer(RawMemory) of
                        true ->
                            Memory = mnesia_memory(Storage, RawMemory, WordSize),
                            Tab0 = [
                                {name, Id},
                                {owner, Owner},
                                {size, Size},
                                {reg_name, RegName},
                                {type, mnesia:table_info(Id, type)},
                                {memory, Memory},
                                {index, mnesia:table_info(Id, index)}
                            ],
                            Tab = with_storage_type(Id, Storage, Tab0),
                            [{0, proplists:get_value(Attr, Tab), Tab} | Acc];
                        false ->
                            Acc
                    end
                catch
                    _:_ -> Acc
                end
        end
    end,
    lists:foldl(CollectFun, [], mnesia:system_info(tables)).

mnesia_memory(disc_only_copies, RawMemory, _WordSize) -> RawMemory;
mnesia_memory(_Storage, RawMemory, WordSize) -> RawMemory * WordSize.

with_storage_type(Id, Storage, Tab0) when Storage =:= ram_copies orelse Storage =:= disc_copies ->
    [
        {fixed, ets:info(Id, fixed)},
        {compressed, ets:info(Id, compressed)},
        {storage, Storage}
        | Tab0
    ];
with_storage_type(Id, disc_only_copies = Storage, Tab0) ->
    [
        {fixed, dets:info(Id, safe_fixed)},
        {storage, Storage}
        | Tab0
    ];
%% {ext, _, _} or unknown
with_storage_type(_Id, Storage, Tab0) ->
    [
        {storage, io_lib:format("~tp", [Storage])}
        | Tab0
    ].
