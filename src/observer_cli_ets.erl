%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_ets).

%% API
-export([draw_ets_info/2]).

-define(MAX_SHOW_LEN, 25).

%% @doc List include all metrics in observer's Table Viewer.

-spec draw_ets_info(atom(), integer()) -> ok.
draw_ets_info(local_node, IncrRows) ->
  AllEtsInfo = [begin get_ets_info(Tab)  end||Tab <- ets:all()],
  SorEtsInfo = lists:sort(fun({_, Ets1}, {_, Ets2}) ->
    proplists:get_value(memory, Ets1) > proplists:get_value(memory, Ets2)
  end, AllEtsInfo),
  io:format("|\e[46m~-24.24s|~-12.12s|~-12.12s|~-12.12s|~-10.10s|~6.6s|~-24.24s|~-12.12s|~-11.11s\e[49m|~n",
    ["id or name", "memory", "size", "type", "protection",
      "keypos", "write/read concurrency", "owner", "named_table"]),
  [begin
     Name = get_value(name, Ets), Memory = get_value(memory, Ets),
     Size = get_value(size, Ets), Type = get_value(type, Ets),
     Protect = get_value(protection, Ets), KeyPos = get_value(keypos, Ets),
     Write = get_value(write_concurrency, Ets), Read = get_value(read_concurrency, Ets),
     Owner = get_value(owner, Ets), NamedTable = get_value(named_table, Ets),
     IdOrName =
       case is_atom(Id) of
         true -> atom_to_list(Id);
         false -> observer_cli_lib:to_list(Id) ++ "/" ++ Name
       end,
     io:format("|~-24.24s|~-12.12s|~-12.12s|~-12.12s|~-10.10s|~6.6s|~-24.24s|~-12.12s|~10.10s |~n",
     [IdOrName, Memory, Size, Type, Protect, KeyPos, Write ++ "/" ++ Read, Owner, NamedTable])
   end||{Id, Ets} <- lists:sublist(SorEtsInfo, ?MAX_SHOW_LEN + IncrRows)],
  ok;
draw_ets_info(Node, IncrRows) ->
  rpc:call(Node, ?MODULE, draw_ets_info, [local_node, IncrRows]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_value(Key, List) ->
   observer_cli_lib:to_list(proplists:get_value(Key, List)).

get_ets_info(Tab) ->
  case catch ets:info(Tab) of
    {'EXIT', _} -> {Tab, [{name, unread}, %%it maybe die
      {write_concurrency, unread},
      {read_concurrency, unread},
      {compressed, unread},
      {memory, 0 },
      {owner, unread},
      {heir, unread},
      {size, unread},
      {node, unread},
      {named_table, unread},
      {type, unread},
      {keypos, unread},
      {protection, unread}]};
    Info when is_list(Info)->
      Owner = proplists:get_value(owner, Info),
      case is_reg(Owner) of
        Owner -> {Tab, Info};
        Reg -> {Tab, lists:keyreplace(Owner, 1, Info, {owner, Reg})}
      end
  end.

is_reg(Owner) ->
  case process_info(Owner, registered_name) of
    {registered_name, Name} -> Name;
    _ -> Owner
  end.
