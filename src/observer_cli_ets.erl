%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_ets).

-include("observer_cli.hrl").

%% API
-export([start/1]).

-ifdef(TEST).
-export([
    collect_ets_info/1,
    collect_ets_render_info/3,
    get_ets_info/2,
    is_reg/1,
    render_ets_info/3,
    render_ets_info/4,
    unread/0
]).
-endif.

-define(LAST_LINE,
    "q(quit) s(sort by size) m(sort by memory) pd/pu(page:down/up) F/B(forward/back)"
).

-spec start(ViewOpts) -> no_return() when ViewOpts :: view_opts().
start(
    #view_opts{
        ets = #ets{interval = Interval, attr = Attr, cur_page = CurPage},
        auto_row = AutoRow
    } = ViewOpts
) ->
    Pid = spawn_link(fun() ->
        ?output(?CLEAR),
        render_worker(Interval, ?INIT_TIME_REF, Attr, CurPage, AutoRow)
    end),
    manager(Pid, ViewOpts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager(ChildPid, #view_opts{ets = EtsOpts = #ets{cur_page = CurPage}} = ViewOpts) ->
    case observer_cli_lib:parse_cmd(ViewOpts, ?MODULE, [ChildPid]) of
        quit ->
            erlang:send(ChildPid, quit);
        {new_interval, NewMs} ->
            restart(ChildPid, ViewOpts#view_opts{ets = EtsOpts#ets{interval = NewMs}});
        size ->
            restart(ChildPid, ViewOpts#view_opts{ets = EtsOpts#ets{attr = size}});
        %% Home
        {func, proc_count, memory} ->
            restart(ChildPid, ViewOpts#view_opts{ets = EtsOpts#ets{attr = memory}});
        page_down_top_n ->
            restart_page(ChildPid, ViewOpts, CurPage, 1);
        page_up_top_n ->
            restart_page(ChildPid, ViewOpts, CurPage, -1);
        _ ->
            manager(ChildPid, ViewOpts)
    end.

restart_page(ChildPid, ViewOpts = #view_opts{ets = EtsOpts}, CurPage, Delta) ->
    NewPage = observer_cli_lib:next_page(CurPage, Delta),
    restart(ChildPid, ViewOpts#view_opts{ets = EtsOpts#ets{cur_page = NewPage}}).

restart(ChildPid, ViewOpts) ->
    observer_cli_lib:exit_processes([ChildPid]),
    start(ViewOpts).

render_worker(Interval, LastTimeRef, Attr, CurPage, AutoRow) ->
    TerminalRow = observer_cli_lib:get_terminal_rows(AutoRow),
    Rows = erlang:max(0, TerminalRow - 4),
    Text = "Interval: " ++ integer_to_list(Interval) ++ "ms",
    Menu = observer_cli_lib:render_top_menu(ets, Text),
    EtsInfo = collect_ets_render_info(Rows, CurPage, Attr),
    Ets = render_ets_info(EtsInfo, Attr),
    LastLine = observer_cli_lib:render_footer(?LAST_LINE),
    ?output([?CURSOR_TOP, Menu, Ets, LastLine]),
    NextTimeRef = observer_cli_lib:next_redraw(LastTimeRef, Interval),
    receive
        quit -> quit;
        _ -> render_worker(Interval, NextTimeRef, Attr, CurPage, AutoRow)
    end.

collect_ets_info(Attr) ->
    [get_ets_info(Tab, Attr) || Tab <- ets:all()].

collect_ets_render_info(Rows, CurPage, Attr) ->
    observer_cli_lib:sublist(collect_ets_info(Attr), Rows, CurPage).

render_ets_info({_StartPos, SortEts}, Attr) ->
    WordSize = erlang:system_info(wordsize),
    {MemColor, SizeColor} =
        case Attr of
            memory -> {?RED_BG, ?GRAY_BG};
            _ -> {?GRAY_BG, ?RED_BG}
        end,
    [
        NameTitleW,
        SizeTitleW,
        MemoryTitleW,
        TypeTitleW,
        ProtectionTitleW,
        KeyPosTitleW,
        WriteReadTitleW,
        OwnerTitleW
    ] =
        ets_title_widths(),
    Title = ?render([
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Table Name", NameTitleW),
        ?UNDERLINE,
        ?W2(SizeColor, "Size", SizeTitleW),
        ?UNDERLINE,
        ?W2(MemColor, "    Memory    ", MemoryTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Type", TypeTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Protection", ProtectionTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "KeyPos", KeyPosTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Write/Read", WriteReadTitleW),
        ?UNDERLINE,
        ?W2(?GRAY_BG, "Owner Pid", OwnerTitleW)
    ]),
    [NameW, SizeW, MemoryW, TypeW, ProtectionW, KeyPosW, WriteReadW, OwnerW] = ets_row_widths(),
    RowView = [
        begin
            Name = proplists:get_value(name, Ets),
            Memory = proplists:get_value(memory, Ets),
            MemoryBytes =
                case Memory of
                    Words when is_integer(Words) -> Words * WordSize;
                    _ -> Memory
                end,
            Size = proplists:get_value(size, Ets),
            Type = proplists:get_value(type, Ets),
            Protect = proplists:get_value(protection, Ets),
            KeyPos = proplists:get_value(keypos, Ets),
            Write = observer_cli_lib:to_list(proplists:get_value(write_concurrency, Ets)),
            Read = observer_cli_lib:to_list(proplists:get_value(read_concurrency, Ets)),
            Owner = proplists:get_value(owner, Ets),
            ?render([
                ?W(Name, NameW),
                ?W(Size, SizeW),
                ?W({byte, MemoryBytes}, MemoryW),
                ?W(Type, TypeW),
                ?W(Protect, ProtectionW),
                ?W(KeyPos, KeyPosW),
                ?W(Write ++ "/" ++ Read, WriteReadW),
                ?W(Owner, OwnerW)
            ])
        end
     || {_, _, Ets} <- SortEts
    ],
    [Title | RowView].

-ifdef(TEST).
render_ets_info(Rows, CurPage, Attr) ->
    render_ets_info(collect_ets_render_info(Rows, CurPage, Attr), Attr).

render_ets_info(AllEts, Rows, CurPage, Attr) ->
    render_ets_info(observer_cli_lib:sublist(AllEts, Rows, CurPage), Attr).
-endif.

ets_title_widths() ->
    observer_cli_lib:weighted_widths(
        [37, 14, 14, 15, 12, 8, 14, 15],
        [4, 1, 1, 0, 0, 0, 0, 4]
    ).

ets_row_widths() ->
    observer_cli_lib:weighted_widths(
        [36, 12, 12, 13, 10, 6, 12, 14],
        [4, 1, 1, 0, 0, 0, 0, 4]
    ).

get_ets_info(Tab, Attr) ->
    try ets:info(Tab) of
        undefined ->
            unread();
        Info when is_list(Info) ->
            Owner = proplists:get_value(owner, Info),
            NewInfo =
                case is_reg(Owner) of
                    Owner -> Info;
                    Reg -> lists:keyreplace(Owner, 1, Info, {owner, Reg})
                end,
            {
                0,
                proplists:get_value(Attr, NewInfo),
                NewInfo
            }
    catch
        _:_ ->
            unread()
    end.

is_reg(Owner) ->
    case process_info(Owner, registered_name) of
        {registered_name, Name} -> Name;
        _ -> Owner
    end.

unread() ->
    {
        0,
        0,
        [
            %% it may have died
            {name, unread},
            {write_concurrency, unread},
            {read_concurrency, unread},
            {compressed, unread},
            {memory, unread},
            {owner, unread},
            {heir, unread},
            {size, unread},
            {node, unread},
            {named_table, unread},
            {type, unread},
            {keypos, unread},
            {protection, unread}
        ]
    }.
