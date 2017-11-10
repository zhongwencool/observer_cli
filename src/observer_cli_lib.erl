%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_lib).

%% API
-export([uptime/1]).
-export([move_cursor_to_top_line/0]).
-export([clear_screen/0]).
-export([float_to_percent_with_two_digit/1]).
-export([to_list/1]).
-export([get_menu_title/1]).
-export([green/1]).
-export([to_megabyte_str/1]).
-export([mfa_to_list/1]).
-export([get_line/1]).

-spec move_cursor_to_top_line() -> ok.
move_cursor_to_top_line() ->
    io:format("\e[H").

-spec clear_screen() -> ok.
clear_screen() ->
    io:format("\e[H\e[J").

-spec get_line(list()) -> list().
get_line(Remind) ->
    Input = io:get_line(Remind),
    to_list(Input).

%% @doc  return format "124Days 12:12:12"
-spec uptime(atom()) -> string().
uptime(local_node) ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~pDays ~p:~p:~p", [D, H, M, S]));
uptime(Node) -> rpc:call(Node, ?MODULE, uptime, [local_node]).

%% @doc 0.98.2342 -> 98.23%, 1 -> 100.0%
-spec float_to_percent_with_two_digit(float()) -> string().
float_to_percent_with_two_digit(Float) ->
    Val = trunc(Float*10000),
    Integer = Val div 100,
    Decimal = Val - Integer * 100,
    case Integer of
        100 -> "100.0%";
        _ -> lists:flatten(io_lib:format("~2..0w.~2..0w%", [Integer, Decimal]))
    end.

-spec to_list(term()) -> list().
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Pid) when is_pid(Pid) -> erlang:pid_to_list(Pid);
to_list(Binary) when is_binary(Binary) -> erlang:binary_to_list(Binary);
to_list(Port)when is_port(Port) -> erlang:port_to_list(Port);
to_list(Ref) when is_reference(Ref) -> erlang:ref_to_list(Ref);
to_list(Val) -> Val.

-spec get_menu_title('allocator'|'ets'|'help'|'home'|'inet'|'mnesia') -> list().
get_menu_title(Type) ->
    [Home, Ets, Inet, Alloc, Mnesia, Help] = get_menu_title2(Type),
    lists:flatten(["|", Home, "|", Ets, "|", Inet, "|", Alloc, "|", Mnesia, "|", Help, "|"]).

get_menu_title2(home) ->
    [select("o(OBSERVER)"), unselect("e(ETS)"), unselect("n(NET)"),
     unselect("a(ALLOCATOR)"), unselect("db(MNESIA)"), unselect("h(HELP)")];
get_menu_title2(ets) ->
    [unselect("o(OBSERVER)"), select("e(ETS)"), unselect("n(NET)"),
     unselect("a(ALLOCATOR)"), unselect("db(MNESIA)"), unselect("h(HELP)")];
get_menu_title2(allocator) ->
    [unselect("o(OBSERVER)"), unselect("e(ETS)"), unselect("n(NET)"),
     select("a(ALLOCATOR)"), unselect("db(MNESIA)"), unselect("h(HELP)")];
get_menu_title2(help) ->
    [unselect("o(OBSERVER)"), unselect("e(ETS)"), unselect("n(NET)"),
     unselect("a(ALLOCATOR)"), unselect("db(MNESIA)"), select("h(HELP)")];
get_menu_title2(inet) ->
    [unselect("o(OBSERVER)"), unselect("e(ETS)"), select("n(NET)"),
     unselect("a(ALLOCATOR)"), unselect("db(MNESIA)"), unselect("h(HELP)")];
get_menu_title2(mnesia) ->
    [unselect("o(OBSERVER)"), unselect("e(ETS)"), unselect("n(NET)"),
     unselect("a(ALLOCATOR)"), select("db(MNESIA)"), unselect("h(HELP)")].

select(Title) -> "\e[48;2;184;0;0m" ++ Title ++ "\e[0m".
unselect(Title) -> "\e[48;2;80;80;80m" ++ Title ++ "\e[0m".

-spec green(list()) -> list().
green(String) -> "\e[32;1m" ++ String ++ "\e[0m".

-spec to_megabyte_str(pos_integer()) -> list().
to_megabyte_str(M) ->
    Val = trunc(M/(1024*1024)*1000),
    Integer = Val div 1000,
    Decimal = Val - Integer * 1000,
    lists:flatten(io_lib:format("~w.~4..0wM", [Integer, Decimal])).

-spec mfa_to_list({atom(), atom(), integer()}) -> nonempty_string().

mfa_to_list({Module, Fun, Arg}) ->
    atom_to_list(Module) ++ ":" ++
        atom_to_list(Fun) ++ "/" ++
        integer_to_list(Arg).

