%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_mnesia).

%% API
-export([start/1]).

-include("observer_cli.hrl").

-spec start(#view_opts{}) -> any().

start(#view_opts{db = #db{interval = MillSecond}, auto_row = AutoRow} = HomeOpts) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(MillSecond, ?INIT_TIME_REF, true, AutoRow)
                end),
    manager(Pid, HomeOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
render_worker(Interval, LastTimeRef, HideSystemTable, AutoRow) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(TerminalRow - 5, 0),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms"
        ++ " HideSystemTable:" ++ atom_to_list(HideSystemTable),
    Menu = observer_cli_lib:render_menu(mnesia, Text),
    LastLine = observer_cli_lib:render_last_line("q(quit) system:false/true"),
    case get_table_list(false) of
        {error, Reason} ->
            ErrInfo = io_lib:format("Mnesia Error   ~p~n", [Reason]),
            ?output([?CURSOR_TOP, Menu, ErrInfo, LastLine]);
        MnesiaList ->
            Info = render_mnesia(MnesiaList, Rows),
            ?output([?CURSOR_TOP, Menu, Info, LastLine])
    end,
    TimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        {new_interval, NewInterval} -> render_worker(NewInterval, TimeRef, HideSystemTable, AutoRow);
        {system_table, NewHideSystemTable} -> render_worker(Interval, TimeRef, NewHideSystemTable, AutoRow);
        _ -> render_worker(Interval, TimeRef, HideSystemTable, AutoRow)
    end.

manager(ChildPid, #view_opts{db = DBOpts} = HomeOpts) ->
    case observer_cli_lib:parse_cmd(HomeOpts, [ChildPid]) of
        quit -> erlang:send(ChildPid, quit);
        {new_interval, NewMs} = Msg ->
            erlang:send(ChildPid, Msg),
            manager(ChildPid, HomeOpts#view_opts{db = DBOpts#db{interval = NewMs}});
        "system:true\n" ->
            erlang:send(ChildPid, {system_table, true}),
            manager(ChildPid, HomeOpts);
        "system:false\n" ->
            erlang:send(ChildPid, {system_table, false}),
            manager(ChildPid, HomeOpts);
        _ -> manager(ChildPid, HomeOpts)
    end.

render_mnesia(MnesiaList, Rows) ->
    SortMnesiaList = lists:sort(fun(Table1, Table2) ->
        proplists:get_value(memory, Table1) > proplists:get_value(memory, Table2)
                                end, MnesiaList),
    Title = ?render([?UNDERLINE, ?GRAY_BG,
        ?W("name", 24), ?W("memory", 14), ?W("size", 14),
        ?W("type", 10), ?W("storage", 13), ?W("owner", 12),
        ?W("index", 9), ?W("reg_name", 19)
        ]),
    View =
        [begin
             Name = proplists:get_value(name, Mnesia), Memory = proplists:get_value(memory, Mnesia),
             Size = proplists:get_value(size, Mnesia), Type = proplists:get_value(type, Mnesia),
             RegName = proplists:get_value(reg_name, Mnesia), Index = proplists:get_value(index, Mnesia),
             Owner = proplists:get_value(owner, Mnesia), Storage = proplists:get_value(storage, Mnesia),
             ?render([
                 ?W(Name, 24), ?W(Memory, 14), ?W(Size, 14),
                 ?W(Type, 10), ?W(Storage, 13), ?W(Owner, 12),
                 ?W(Index, 9), ?W(RegName, 19)
             ])
         end || Mnesia <- lists:sublist(SortMnesiaList, Rows)],
    [Title | View].

mnesia_tables() ->
    [ir_AliasDef, ir_ArrayDef, ir_AttributeDef, ir_ConstantDef,
        ir_Contained, ir_Container, ir_EnumDef, ir_ExceptionDef,
        ir_IDLType, ir_IRObject, ir_InterfaceDef, ir_ModuleDef,
        ir_ORB, ir_OperationDef, ir_PrimitiveDef, ir_Repository,
        ir_SequenceDef, ir_StringDef, ir_StructDef, ir_TypedefDef,
        ir_UnionDef, logTable, logTransferTable, mesh_meas,
        mesh_type, mnesia_clist, orber_CosNaming,
        orber_objkeys, user
    ].

get_table_list(HideSys) ->
    Owner = ets:info(schema, owner),
    case Owner of
        undefined -> {error, "Mnesia is not running on: " ++ atom_to_list(node())};
        _ -> get_table_list2(Owner, HideSys)
    end.

get_table_list2(Owner, HideSys) ->
    {registered_name, RegName} = process_info(Owner, registered_name),
    CollectFun = fun(Id, Acc) ->
        case HideSys andalso ordsets:is_element(Id, mnesia_tables()) orelse Id =:= schema of
            true -> Acc; %% ignore system table
            false ->
                Storage = mnesia:table_info(Id, storage_type),
                Tab0 = [{name, Id},
                    {owner, Owner},
                    {size, mnesia:table_info(Id, size)},
                    {reg_name, RegName},
                    {type, mnesia:table_info(Id, type)},
                    {memory, mnesia:table_info(Id, memory) * erlang:system_info(wordsize)},
                    {storage, Storage},
                    {index, mnesia:table_info(Id, index)}
                ],
                Tab =
                    case Storage of
                        _ when Storage =:= ram_copies orelse Storage =:= disc_copies ->
                            [{fixed, ets:info(Id, fixed)}, {compressed, ets:info(Id, compressed)} | Tab0];
                        disc_only_copies ->
                            [{fixed, dets:info(Id, safe_fixed)} | Tab0];
                        _ -> Tab0
                    end,
                [Tab | Acc]
        end
                 end,
    lists:foldl(CollectFun, [], mnesia:system_info(tables)).
