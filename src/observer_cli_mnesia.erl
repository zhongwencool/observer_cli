%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_mnesia).

%% API
-export([start/2]).

%%for rpc
-export([get_table_list/2]).

-include("observer_cli.hrl").

-define(MAX_SHOW_LEN, 37).

-spec start(atom(), list()) -> no_return.
start(Node, #view_opts{db = #db{interval = RefreshMillSecond}, incr_rows = IncrRows} = HomeOpts) ->
  ParentPid = self(),
  Pid = spawn(fun() ->
    observer_cli_lib:clear_screen(),
    loop(Node, RefreshMillSecond, erlang:make_ref(), ParentPid, IncrRows, true) end),
  waiting(Node, Pid, HomeOpts).

-spec get_table_list(atom(), true|false) -> list().
get_table_list(local_node, HideSys) ->
  Owner = ets:info(schema, owner),
  case Owner of
    undefined -> {error, "Mnesia is not running on: " ++ atom_to_list(node())};
    _->
      {registered_name, RegName} = process_info(Owner, registered_name),
      CollectFun = fun(Id, Acc) ->
        Name = Id,
        case HideSys andalso ordsets:is_element(Name, mnesia_tables()) orelse Name =:= schema of
          true -> Acc; %% ignore system table
          false ->
            Storage = mnesia:table_info(Id, storage_type),
            Tab0 = [{name, Name},
              {owner, Owner},
              {size, mnesia:table_info(Id, size)},
              {reg_name, RegName},
              {type, mnesia:table_info(Id, type)},
              {memory, mnesia:table_info(Id, memory) * erlang:system_info(wordsize)},
              {storage, Storage},
              {index, mnesia:table_info(Id, index)}
            ],
            Tab =
              case Storage  of
                _ when Storage =:= ram_copies orelse Storage =:=  disc_copies ->
                  [{fixed, ets:info(Id, fixed)}, {compressed, ets:info(Id, compressed)}|Tab0];
                disc_only_copies -> [{fixed, dets:info(Id, safe_fixed)}|Tab0];
                _ -> Tab0
              end,
            [Tab|Acc]
        end
      end,
      lists:foldl(CollectFun, [], mnesia:system_info(tables))
  end;
get_table_list(Node, HideSys) -> rpc:call(Node, ?MODULE, get_table_list, [local_node, HideSys]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Node, Interval, LastTimeRef, ParentPid, IncrRows, HideSystemTable) ->
  erlang:cancel_timer(LastTimeRef),
  case get_table_list(Node, false) of
    {error, Reason} ->
      observer_cli_lib:move_cursor_to_top_line(),
      draw_menu(Node, Interval, HideSystemTable),
      io:format("Mnesia Error   ~p~n", [Reason]),
      draw_last_line(Node, Interval);
    MnesiaList ->
      observer_cli_lib:move_cursor_to_top_line(),
      draw_menu(Node, Interval, HideSystemTable),
      draw_mnesia(MnesiaList, IncrRows),
      draw_last_line(Node, Interval)
  end,
  TimeRef = erlang:send_after(Interval, self(), refresh),
  receive
    quit -> quit;
    go_to_home_view -> erlang:send(ParentPid, draw_work_done_to_home_view), quit;
    go_to_allocator_view -> erlang:send(ParentPid, draw_work_done_to_allocator_view), quit;
    go_to_help_view -> erlang:send(ParentPid, draw_work_done_to_help_view), quit;
    go_to_ets_view ->  erlang:send(ParentPid, draw_work_done_to_ets_view), quit;
    {new_interval, NewInterval} -> loop(Node, NewInterval, TimeRef, ParentPid, IncrRows, HideSystemTable);
    {system_table, NewHideSystemTable} -> loop(Node, Interval, TimeRef, ParentPid, IncrRows, NewHideSystemTable);
    {incr_rows, NewIncrRows} -> loop(Node, Interval, TimeRef, ParentPid, NewIncrRows, HideSystemTable);
    _ -> loop(Node, Interval, TimeRef, ParentPid, IncrRows, HideSystemTable)
  end.

waiting(Node, ChildPid, #view_opts{db = DBOpts} = HomeOpts) ->
  Input = observer_cli_lib:get_line(""),
  case  Input of
    "q\n" -> erlang:send(ChildPid, quit);
    "o\n" ->
      erlang:exit(ChildPid, stop),
      observer_cli:start_node(Node, HomeOpts);
    "a\n" ->
      erlang:exit(ChildPid, stop),
      observer_cli_allocator:start(Node, HomeOpts);
    "e\n" ->
      erlang:exit(ChildPid, stop),
      observer_cli_system:start(Node, HomeOpts);
    "h\n" ->
      erlang:exit(ChildPid, stop),
      observer_cli_help:start(Node, HomeOpts);
    "system:true\n" ->
      erlang:send(ChildPid, {system_table, true}),
      waiting(Node, ChildPid, HomeOpts);
    "system:false\n" ->
      erlang:send(ChildPid, {system_table, false}),
      waiting(Node, ChildPid, HomeOpts);
    [$r| RefreshInterval] ->
      case string:to_integer(RefreshInterval) of
        {error, no_integer} -> waiting(Node, ChildPid, HomeOpts);
        {NewInterval, _} when NewInterval >= ?MNESIA_MIN_INTERVAL ->
          erlang:send(ChildPid, {new_interval, NewInterval}),
          waiting(Node, ChildPid, HomeOpts#view_opts{db = DBOpts#db{interval = NewInterval}})
      end;
    [$i| IncrRows] ->
      case string:to_integer(IncrRows) of
        {error, no_integer} -> waiting(Node, ChildPid, HomeOpts);
        {NewIncrRows, _} ->
          erlang:send(ChildPid, {incr_rows, NewIncrRows}),
          waiting(Node, ChildPid, HomeOpts#view_opts{incr_rows = NewIncrRows})
      end;
    _ -> waiting(Node, ChildPid, HomeOpts)
  end.

draw_menu(Node, Interval, HideSystemTable) ->
  [Home, Ets, Alloc, Mnesia, Help]  = observer_cli_lib:get_menu_title(mnesia),
  Title = lists:flatten(["|", Home, "|", Ets, "|", Alloc, "|", Mnesia, "|", Help, "|"]),
  UpTime = observer_cli_lib:green(" Uptime:" ++ observer_cli_lib:uptime(Node)) ++ "|",
  RefreshStr = "Refresh: " ++ integer_to_list(Interval) ++ "ms" ++ " HideSystemTable:" ++ atom_to_list(HideSystemTable),
  SpaceLen = ?COLUMN_WIDTH - erlang:length(Title)  - erlang:length(RefreshStr)  - erlang:length(UpTime)+ 110,
  Space = case SpaceLen > 0 of  true -> lists:duplicate(SpaceLen, " "); false -> [] end,
  io:format("~s~n", [Title ++ RefreshStr ++ Space ++ UpTime]).

draw_mnesia(MnesiaList, IncrRows) ->
  SortMneisaList = lists:sort(fun(Table1, Table2) ->
    proplists:get_value(memory, Table1) > proplists:get_value(memory, Table2)
  end, MnesiaList),
  io:format("|\e[46m~-24.24s|~-14.14s|~-14.14s|~-10.10s|~18.18s|~-12.12s|~-12.12s|~20.20s\e[49m|~n",
    ["name", "memory", "size", "type", "storage", "owner", "index", "reg_name"]),
  [begin
     Name = get_value(name, Mnesia), Memory = get_value(memory, Mnesia),
     Size = get_value(size, Mnesia), Type = get_value(type, Mnesia),
     RegName = get_value(reg_name, Mnesia), Index = get_value(index, Mnesia),
     Owner = get_value(owner, Mnesia), Storage = get_value(storage, Mnesia),
     io:format("|~-24.24s|~-14.14s|~-14.14s|~-10.10s|~18.18s|~-12.12s|~-12.12s|~20.20s|~n",
       [Name, Memory, Size, Type, Storage, Owner, Index, RegName])
   end||Mnesia <- lists:sublist(SortMneisaList, ?MAX_SHOW_LEN + IncrRows)],
  ok.

draw_last_line(Node, Interval) ->
  io:format("|\e[31;1mINPUT: \e[0m\e[44mq(quit)  system:false/true  r~w(refresh every ~wms)   ~66.66s\e[49m|~n",
    [Interval, Interval, atom_to_list(Node)]).

get_value(Key, List) ->
  observer_cli_lib:to_list(proplists:get_value(Key, List)).

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