%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_ets).

-include("observer_cli.hrl").
%% API
-export([render_ets_info/1]).

%% @doc List include all metrics in observer's Table Viewer.

-spec render_ets_info(integer()) -> ok.
render_ets_info(Rows) ->
    AllEtsInfo = [begin get_ets_info(Tab) end || Tab <- ets:all()],
    SorEtsInfo = lists:sort(fun({_, Ets1}, {_, Ets2}) ->
        proplists:get_value(memory, Ets1) > proplists:get_value(memory, Ets2)
                            end, AllEtsInfo),
    Title = ?render([?BLUE_BG,
        ?W("id or name", 24), ?W("memory", 12), ?W("size", 10),
        ?W("type", 11), ?W("protection", 10), ?W("keypos", 6),
        ?W("write/read concurrency", 22), ?W("owner", 9), ?W("namedtable", 10),
        ?RESET_BG]),
    RowView =
        [begin
             Name = get_value(name, Ets), Memory = get_value(memory, Ets),
             Size = get_value(size, Ets), Type = get_value(type, Ets),
             Protect = get_value(protection, Ets), KeyPos = get_value(keypos, Ets),
             Write = get_value(write_concurrency, Ets), Read = get_value(read_concurrency, Ets),
             Owner = get_value(owner, Ets), NamedTable = get_value(named_table, Ets),
             IdOrName =
                 case is_atom(Id) of
                     true -> atom_to_list(Id);
                     false -> Name ++ "/" ++ observer_cli_lib:to_list(Id)
                 end,
             ?render([ ?W(IdOrName, 24), ?W(Memory, 12), ?W(Size, 10),
                 ?W(Type, 11), ?W(Protect, 10), ?W(KeyPos, 6),
                 ?W(Write ++ "/" ++ Read, 22), ?W(Owner, 9), ?W(NamedTable, 10)
             ])
         end || {Id, Ets} <- lists:sublist(SorEtsInfo, Rows)],
    [Title|RowView].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_value(Key, List) ->
    observer_cli_lib:to_list(proplists:get_value(Key, List)).

get_ets_info(Tab) ->
    case catch ets:info(Tab) of
        {'EXIT', _} ->
            {Tab, [{name, unread}, %%it maybe die
                {write_concurrency, unread},
                {read_concurrency, unread},
                {compressed, unread},
                {memory, 0},
                {owner, unread},
                {heir, unread},
                {size, unread},
                {node, unread},
                {named_table, unread},
                {type, unread},
                {keypos, unread},
                {protection, unread}]};
        Info when is_list(Info) ->
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
