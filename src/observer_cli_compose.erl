-module(observer_cli_compose).

-export([pipe/2]).

pipe(Acc, FunList) ->
    lists:foldl(
        fun(Fun, Acc2) when is_function(Fun, 1) -> Fun(Acc2) end, Acc, FunList
    ).

