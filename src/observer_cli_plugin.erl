%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_plugin).

%% API
-export([start/1]).

-include("observer_cli.hrl").

-ifdef(TEST).
-export([
    parse_cmd_str/1,
    render_sheet_header/2,
    render_sheet_body/8,
    render_sheet/4,
    render_menu/2,
    get_sheet_width/1,
    mix_content_width/3,
    match_menu_shortcut/2,
    match_sheet_shortcut/3,
    init_config/1,
    update_plugins/3,
    maybe_shortcut/2,
    render_attributes/2,
    render_worker/6,
    manager/3
]).
-endif.

-callback attributes(PreState) -> #{rows := [Rows], state := NewState} when
    PreState :: any(),
    Rows :: [attr_cell()],
    NewState :: any().

-callback sheet_header() -> #{columns := [SheetHeader], default_sort := atom()} when
    SheetHeader :: #{
        id := atom(),
        title := string(),
        width := pos_integer(),
        shortcut => string()
    }.

-callback sheet_body(PreState) -> #{rows := [SheetBody], state := NewState} when
    PreState :: any(),
    SheetBody :: #{cells := map(), handle => term()},
    NewState :: any().

-type attr_cell() :: #{
    content => string() | integer() | {byte, pos_integer()} | {percent, float()},
    width => pos_integer(),
    color => binary()
}.

-define(LAST_LINE,
    "refresh: ~wms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is ~w"
).

-spec start(ViewOpts) -> no_return() when ViewOpts :: view_opts().
start(#view_opts{plug = Plugs, auto_row = AutoRow} = ViewOpts) ->
    NewPlugs = init_config(Plugs),
    SheetCache = ets:new(?MODULE, [set, public]),
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(?INIT_TIME_REF, NewPlugs, AutoRow, SheetCache, undefined, undefined)
    end),
    manager(Pid, SheetCache, ViewOpts#view_opts{plug = NewPlugs}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_config(#plug{plugs = []}) ->
    application:ensure_all_started(observer_cli),
    Plugs = application:get_env(observer_cli, plugins, []),
    {_, NewPlugs} = lists:foldl(
        fun(M = #{module := _Mod}, {Index, Acc}) ->
            {Index + 1, maps:put(Index, init_plugin_config(M), Acc)}
        end,
        {1, #{}},
        Plugs
    ),
    #plug{cur_index = 1, plugs = NewPlugs};
init_config(Plugs) ->
    Plugs.

init_plugin_config(Config) ->
    Defaults = #{
        cur_page => 1,
        cur_row => 1,
        interval => 1500,
        sheet_width => observer_cli_lib:layout_base_width()
    },
    WithDefaults = maps:merge(Defaults, Config),
    #{module := Mod} = WithDefaults,
    try
        Header = plugin_header(Mod),
        Migrated = observer_cli_plugin_compat:migrate_config(WithDefaults, Header),
        maps:put(sheet_width, get_sheet_width(Mod), Migrated)
    catch
        error:undef ->
            WithDefaults
    end.

manager(ChildPid, SheetCache, ViewOpts) ->
    #view_opts{plug = PlugOpts = #plug{cur_index = CurIndex, plugs = Plugs}} = ViewOpts,
    case parse_cmd() of
        quit ->
            observer_cli_lib:exit_processes([ChildPid]),
            ets:delete(SheetCache),
            quit;
        go_home ->
            observer_cli_lib:exit_processes([ChildPid]),
            ets:delete(SheetCache),
            observer_cli:start(ViewOpts);
        {new_interval, NewMs} ->
            observer_cli_lib:exit_processes([ChildPid]),
            ets:delete(SheetCache),
            NewPlugs = update_plugins(CurIndex, Plugs, #{interval => NewMs}),
            start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
        page_down_top_n ->
            update_page(ChildPid, SheetCache, ViewOpts, 1);
        page_up_top_n ->
            update_page(ChildPid, SheetCache, ViewOpts, -1);
        {jump, CurRow} ->
            jump_row(ChildPid, SheetCache, ViewOpts, CurRow, true);
        jump ->
            jump_current_row(ChildPid, SheetCache, ViewOpts);
        {input_str, Cmd} ->
            case maybe_shortcut(Cmd, ViewOpts) of
                {ok, menu, Index} ->
                    observer_cli_lib:exit_processes([ChildPid]),
                    ets:delete(SheetCache),
                    start(ViewOpts#view_opts{plug = PlugOpts#plug{cur_index = Index}});
                {ok, sheet, Sort} ->
                    NewPlugs = update_plugins(CurIndex, Plugs, #{sort => Sort}),
                    observer_cli_lib:exit_processes([ChildPid]),
                    ets:delete(SheetCache),
                    start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}});
                {error, _} ->
                    manager(ChildPid, SheetCache, ViewOpts)
            end
    end.

update_page(ChildPid, SheetCache, ViewOpts, Delta) ->
    case current_plugin(ViewOpts) of
        {ok, CurIndex, Plugs, CurPlugs} ->
            CurPage = maps:get(cur_page, CurPlugs),
            NewPage = observer_cli_lib:next_page(CurPage, Delta),
            NewPlugs = update_plugins(CurIndex, Plugs, #{cur_page => NewPage}),
            restart_with_plugins(ChildPid, SheetCache, ViewOpts, NewPlugs);
        error ->
            manager(ChildPid, SheetCache, ViewOpts)
    end.

jump_current_row(ChildPid, SheetCache, ViewOpts) ->
    case current_plugin(ViewOpts) of
        {ok, _CurIndex, _Plugs, CurPlugs} ->
            CurRow = maps:get(cur_row, CurPlugs, observer_cli_process),
            jump_row(ChildPid, SheetCache, ViewOpts, CurRow, false);
        error ->
            manager(ChildPid, SheetCache, ViewOpts)
    end.

jump_row(ChildPid, SheetCache, ViewOpts, CurRow, SaveRow) ->
    case current_plugin(ViewOpts) of
        {ok, CurIndex, Plugs, CurPlugs} ->
            maybe_start_row(ChildPid, SheetCache, ViewOpts, CurIndex, Plugs, {
                CurPlugs, CurRow, SaveRow
            });
        error ->
            manager(ChildPid, SheetCache, ViewOpts)
    end.

maybe_start_row(
    ChildPid, SheetCache, ViewOpts, CurIndex, Plugs, {CurPlugs, CurRow, SaveRow}
) ->
    case ets:lookup(SheetCache, CurRow) of
        [{CurRow, Row}] ->
            start_first_match(ChildPid, SheetCache, ViewOpts, CurIndex, Plugs, {
                CurPlugs, Row, CurRow, SaveRow
            });
        _ ->
            manager(ChildPid, SheetCache, ViewOpts)
    end.

start_first_match(
    ChildPid, SheetCache, ViewOpts, CurIndex, Plugs, {CurPlugs, Row, CurRow, SaveRow}
) ->
    case observer_cli_plugin_compat:resolve_handler(CurPlugs, Row) of
        {Handler, ChooseItem} ->
            observer_cli_lib:exit_processes([ChildPid]),
            ets:delete(SheetCache),
            Handler:start(
                plugin, ChooseItem, view_opts_with_row(ViewOpts, CurIndex, Plugs, CurRow, SaveRow)
            );
        none ->
            manager(ChildPid, SheetCache, ViewOpts)
    end.

view_opts_with_row(ViewOpts, CurIndex, Plugs, CurRow, true) ->
    #view_opts{plug = PlugOpts} = ViewOpts,
    NewPlugs = update_plugins(CurIndex, Plugs, #{cur_row => CurRow}),
    ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}};
view_opts_with_row(ViewOpts, _CurIndex, _Plugs, _CurRow, false) ->
    ViewOpts.

restart_with_plugins(ChildPid, SheetCache, #view_opts{plug = PlugOpts} = ViewOpts, NewPlugs) ->
    observer_cli_lib:exit_processes([ChildPid]),
    ets:delete(SheetCache),
    start(ViewOpts#view_opts{plug = PlugOpts#plug{plugs = NewPlugs}}).

current_plugin(#view_opts{plug = #plug{cur_index = CurIndex, plugs = Plugs}}) ->
    case maps:find(CurIndex, Plugs) of
        {ok, CurPlugs} -> {ok, CurIndex, Plugs, CurPlugs};
        error -> error
    end.

update_plugins(CurIndex, Lists, UpdateItems) ->
    case maps:find(CurIndex, Lists) of
        {ok, CurPlugs} ->
            NewPlugs = maps:merge(CurPlugs, UpdateItems),
            maps:put(CurIndex, NewPlugs, Lists);
        error ->
            Lists
    end.

maybe_shortcut(Cmd, ViewOpts) ->
    #view_opts{plug = #plug{cur_index = CurIndex, plugs = Plugs}} = ViewOpts,
    case match_menu_shortcut(Cmd, maps:to_list(Plugs)) of
        {ok, Index} ->
            {ok, menu, Index};
        {error, not_found} ->
            case maps:find(CurIndex, Plugs) of
                {ok, #{module := CurMod}} ->
                    try
                        case match_sheet_shortcut(Cmd, plugin_header(CurMod), 1) of
                            {ok, Sort} -> {ok, sheet, Sort};
                            {error, _Reason} = Err -> Err
                        end
                    catch
                        error:undef ->
                            {error, not_found}
                    end;
                _ ->
                    {error, not_found}
            end
    end.

match_menu_shortcut(_Cmd, []) ->
    {error, not_found};
match_menu_shortcut(Cmd, [{Index, Plug} | Plugs]) ->
    case Plug of
        #{shortcut := Cmd} -> {ok, Index};
        _ -> match_menu_shortcut(Cmd, Plugs)
    end.

match_sheet_shortcut(_Cmd, [], _Index) ->
    {error, not_found};
match_sheet_shortcut(Cmd, #{columns := Columns}, Index) ->
    match_sheet_shortcut(Cmd, Columns, Index);
match_sheet_shortcut(Shortcut, [#{shortcut := Shortcut, id := Id} | _], _Index) ->
    {ok, Id};
match_sheet_shortcut(Cmd, [_ | T], Index) ->
    match_sheet_shortcut(Cmd, T, Index + 1).

render_worker(
    LastTimeRef,
    #plug{cur_index = CurIndex, plugs = Plugs} = PlugInfo,
    AutoRow,
    SheetCache,
    PrevAttrs,
    PrevSheet
) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    case maps:find(CurIndex, Plugs) of
        {ok, #{interval := Interval, cur_page := CurPage, sheet_width := SheetWidth} = CurPlug} ->
            Menu = render_menu(PlugInfo, SheetWidth),
            {Labels, LabelLine, NewAttrs} = render_attributes(CurPlug, PrevAttrs),
            {SheetLine, NewSheet} = render_sheet(
                erlang:max(0, TerminalRow - LabelLine - 4),
                CurPlug,
                SheetCache,
                PrevSheet
            ),
            LastText = io_lib:format(?LAST_LINE, [Interval, CurPage]),
            LastLine = observer_cli_lib:render_footer(LastText, SheetWidth + 4),
            ?output([?CURSOR_TOP, Menu, Labels, SheetLine, LastLine]),
            NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
            receive
                quit -> quit;
                _ -> render_worker(NextTimeRef, PlugInfo, AutoRow, SheetCache, NewAttrs, NewSheet)
            end;
        error ->
            Menu = ?render([
                ?ANSI_UNDERLINE,
                ?W(observer_cli_lib:unselected_menu_item("Home(H)"), 30),
                ?W(observer_cli_lib:selected_menu_item("EmptyPlugin"), 144)
            ]),
            ErrInfo =
                "| No plugins found.\n|Please visit \"How to write your own plugin\" in readme.\n",
            ?output([?CURSOR_TOP, Menu, ErrInfo])
    end.

parse_cmd() ->
    parse_cmd_str(observer_cli_lib:read_cmd()).

parse_cmd_str(Key) ->
    case Key of
        "H\n" -> go_home;
        %% backward
        "B\n" -> page_up_top_n;
        %% forward
        "F\n" -> page_down_top_n;
        "q\n" -> quit;
        "\n" -> jump;
        %% {error, estale}|{error, terminated}
        {error, _Reason} -> quit;
        Number -> observer_cli_command:parse_integer(Number)
    end.

render_menu(#plug{cur_index = CurIndex, plugs = Plugs}, SheetWidth) ->
    Num = maps:size(Plugs),
    Title = get_menu_title(CurIndex, Plugs, Num, []),
    [Time] = observer_cli_lib:uptime(),
    ?render([
        ?ANSI_UNDERLINE,
        ?W(
            [
                observer_cli_lib:unselected_menu_item("Home(H)"),
                "|",
                Title
            ],
            SheetWidth + Num * 21 + 5
        ),
        Time
    ]).

get_menu_title(CurIndex, Plugs, CurIndex, Acc) ->
    {ok, #{title := Title, shortcut := Shortcut}} = maps:find(CurIndex, Plugs),
    NewTitle = Title ++ "(" ++ Shortcut ++ ")",
    NewAcc = [observer_cli_lib:menu_item(CurIndex, CurIndex, NewTitle), "|" | Acc],
    get_menu_title(CurIndex, Plugs, CurIndex - 1, NewAcc);
get_menu_title(CurIndex, Plugs, Pos, Acc) ->
    case maps:find(Pos, Plugs) of
        error ->
            Acc;
        {ok, #{title := Title, shortcut := Shortcut}} ->
            NewTitle = Title ++ "(" ++ Shortcut ++ ")",
            NewAcc = [observer_cli_lib:menu_item(CurIndex, Pos, NewTitle), "|" | Acc],
            get_menu_title(CurIndex, Plugs, Pos - 1, NewAcc)
    end.

render_attributes(#{module := Module}, PrevAttrs) ->
    try
        #{rows := DiffAttrs, state := NewAttrs} =
            observer_cli_plugin_compat:normalize_attributes(Module:attributes(PrevAttrs)),
        Render = [
            begin
                L = [
                    begin
                        #{content := Content, width := Width} = Item,
                        Value =
                            case Content of
                                {percent, Float} -> observer_cli_lib:to_percent(Float);
                                _ -> Content
                            end,
                        case maps:find(color, Item) of
                            {ok, Color} -> ?W2(Color, Value, Width);
                            error -> ?W(Value, Width)
                        end
                    end
                 || Item <- Label
                ],
                ?render(L)
            end
         || Label <- DiffAttrs
        ],
        {Render, length(Render), NewAttrs}
    catch
        error:undef ->
            {[], 0, PrevAttrs}
    end.

render_sheet(Rows, Plug, SheetCache, PrevSheet) ->
    #{
        cur_page := CurPage,
        cur_row := CurRow,
        module := Module
    } = Plug,
    try
        Header = plugin_header(Module),
        Sort = maps:get(sort, Plug, maps:get(default_sort, Header)),
        {Headers, Columns} = render_sheet_header(Module, Sort),
        {Body, NewSheet} = render_sheet_body(
            Module,
            CurPage,
            CurRow,
            Rows,
            Sort,
            Columns,
            SheetCache,
            PrevSheet
        ),
        {[Headers | Body], NewSheet}
    catch
        error:undef ->
            {[], []}
    end.

render_sheet_header(Module, Sort) when is_atom(Module) ->
    render_sheet_header(plugin_header(Module), Sort);
render_sheet_header(#{columns := SheetHeader}, Sort) ->
    {Headers, Columns, _} = lists:foldl(
        fun(
            #{id := Id, title := Header, width := Width} = H,
            {HeaderAcc, ColumnAcc, Index}
        ) ->
            Title =
                case maps:get(shortcut, H, "") of
                    "" -> Header;
                    Shortcut -> Header ++ "(" ++ Shortcut ++ ")"
                end,
            Line =
                case Id =:= Sort of
                    true -> [?UNDERLINE, ?W2(?RED_BG, Title, Width) | HeaderAcc];
                    false -> [?UNDERLINE, ?W2(?GRAY_BG, Title, Width) | HeaderAcc]
                end,
            {Line, [#{id => Id, width => Width} | ColumnAcc], Index - 1}
        end,
        {[], [], length(SheetHeader)},
        lists:reverse(SheetHeader)
    ),
    {?render([?W2(?GRAY_BG, "No ", 3) | Headers]), Columns}.

render_sheet_body(Module, CurPage, CurRow, Rows, Sort, Columns, SheetCache, PrevSheet) ->
    #{rows := Diff, state := NewSheet} =
        observer_cli_plugin_compat:normalize_sheet_body(Module:sheet_body(PrevSheet)),
    DataSet = lists:map(fun(Row) -> {0, sheet_cell(Sort, Row), Row} end, Diff),
    Widths = column_widths(Columns),
    {StartAt, SortData} = observer_cli_lib:sublist(DataSet, Rows, CurPage),
    {Line, _} = lists:foldl(
        fun({_, _, Item}, {List, Pos}) ->
            L = mix_content_width(row_cells(Item, Columns), Widths, []),
            ets:insert(SheetCache, {Pos, Item}),
            case CurRow =:= Pos of
                false -> {[?render([?W(Pos, 2) | L])] ++ List, Pos + 1};
                true -> {[?render([?W(?CHOOSE_BG, Pos, 4) | L])] ++ List, Pos + 1}
            end
        end,
        {[], StartAt},
        SortData
    ),
    {lists:reverse(Line), NewSheet}.

row_cells(#{cells := Cells}, Columns) ->
    [maps:get(Id, Cells, "") || #{id := Id} <- Columns].

sheet_cell(Id, #{cells := Cells}) ->
    maps:get(Id, Cells, "").

column_widths(Columns) ->
    [Width || #{width := Width} <- Columns].

plugin_header(Module) ->
    observer_cli_plugin_compat:normalize_sheet_header(Module:sheet_header()).

sheet_width(#{columns := Columns}) ->
    Width = lists:foldl(fun(#{width := W}, Acc) -> Acc + W + 1 end, 1, Columns),
    case Width > 1 of
        true -> Width - 2;
        false -> observer_cli_lib:layout_base_width()
    end.

mix_content_width([], _, Acc) ->
    lists:reverse(Acc);
%% first
mix_content_width([I | IRest], [W | WRest], []) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width(IRest, WRest, [?W(IList, W - 2)]);
%% last
mix_content_width([I], [W], Acc) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width([], [], [?W(IList, W - 1) | Acc]);
%% middle
mix_content_width([I | IRest], [W | WRest], Acc) ->
    IList = observer_cli_lib:to_list(I),
    mix_content_width(IRest, WRest, [?W(IList, W - 2) | Acc]).

get_sheet_width(Mod) ->
    try
        sheet_width(plugin_header(Mod))
    catch
        error:undef ->
            observer_cli_lib:layout_base_width()
    end.
