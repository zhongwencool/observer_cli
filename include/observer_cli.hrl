-define(COLUMN, 134).
-define(INIT_TIME_REF, undefined).

-define(MIN_INTERVAL, 1000).
-define(DEFAULT_INTERVAL, 1500).
-define(DISABLE, disable).
-define(ENABLE, enable).

-record(home, {
    func = proc_count :: atom(),
    type = memory :: atom(),
    cur_page = 1 :: pos_integer(),
    pages = [{1, 1}] :: list(),
    interval = ?DEFAULT_INTERVAL :: pos_integer(),
    scheduler_usage = application:get_env(observer_cli, scheduler_usage, ?DISABLE) ::
        ?DISABLE | ?ENABLE
}).

-record(app, {
    type = {proc_count, 1} :: {atom(), pos_integer()},
    cur_page = 1 :: pos_integer(),
    interval = ?DEFAULT_INTERVAL :: pos_integer()
}).

-record(ets, {
    interval = 2000 :: integer(),
    attr = memory :: atom(),
    cur_page = 1 :: integer()
}).

-record(system, {interval = 2000 :: integer()}).

-record(db, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    hide_sys = true :: boolean(),
    cur_page = 1 :: integer(),
    attr = memory :: atom()
}).

-record(help, {interval = ?DEFAULT_INTERVAL :: integer()}).
-record(inet, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    func = inet_count :: atom(),
    type = cnt :: atom(),
    cur_page = 1 :: pos_integer(),
    pages = [{1, 1}] :: list()
}).

-record(ports, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    attr = queue_size :: atom(),
    cur_page = 1 :: pos_integer()
}).

-record(sockets, {
    interval = ?DEFAULT_INTERVAL :: integer(),
    sort = io :: atom(),
    cur_page = 1 :: pos_integer()
}).

-record(process, {interval = ?DEFAULT_INTERVAL :: integer()}).

-record(plug, {cur_index = 1 :: pos_integer(), plugs = [] :: map() | []}).

-record(view_opts, {
    home = #home{} :: #home{},
    ets = #ets{} :: #ets{},
    sys = #system{} :: #system{},
    app = #app{} :: #app{},
    db = #db{} :: #db{},
    help = #help{} :: #help{},
    inet = #inet{} :: #inet{},
    ports = #ports{} :: #ports{},
    sockets = #sockets{} :: #sockets{},
    process = #process{} :: #process{},
    port = ?DEFAULT_INTERVAL :: pos_integer(),
    plug = #plug{} :: #plug{},
    auto_row = true :: boolean()
}).

-export_type([
    view_opts/0,
    home/0,
    app/0,
    system/0,
    ets/0,
    db/0,
    help/0,
    inet/0,
    ports/0,
    sockets/0,
    process/0,
    plug/0
]).

-type view_opts() :: #view_opts{}.
-type home() :: #home{}.
-type app() :: #app{}.
-type system() :: #system{}.
-type ets() :: #ets{}.
-type db() :: #db{}.
-type help() :: #help{}.
-type inet() :: #inet{}.
-type ports() :: #ports{}.
-type sockets() :: #sockets{}.
-type process() :: #process{}.
-type plug() :: #plug{}.

-define(CURSOR_TOP, <<"\e[H">>).
-define(CLEAR, <<"\e[H\e[J">>).

-define(ANSI_RESET_BG, <<"\e[49m">>).
-define(ANSI_RESET, <<"\e[0m">>).
-define(ANSI_INVERSE, <<"\e[7m">>).
-define(ANSI_YELLOW, <<"\e[33m">>).
-define(ANSI_RED, <<"\e[31m">>).
-define(ANSI_BRIGHT_GREEN, <<"\e[32;1m">>).
-define(ANSI_BRIGHT_GREEN_2, <<"\e[92m">>).
-define(ANSI_GREEN_BG, <<"\e[42m">>).
-define(ANSI_LEGACY_RED_BG, <<"\e[48m">>).
-define(ANSI_UNDERLINE, <<"\e[4m">>).
-define(MENU_SELECTED_BG, <<"\e[48;2;184;0;0m">>).
-define(MENU_UNSELECTED_BG, <<"\e[48;2;80;80;80m">>).

-define(RESET_BG, ?ANSI_RESET_BG).
-define(RESET, ?ANSI_RESET).
-define(GRAY_BG, ?ANSI_INVERSE).
-define(YELLOW, ?ANSI_YELLOW).
-define(RED, ?ANSI_RED).
-define(L_RED, ?ANSI_LEGACY_RED_BG).
-define(GREEN, ?ANSI_BRIGHT_GREEN).
-define(L_GREEN, ?ANSI_BRIGHT_GREEN_2).
-define(CHOOSE_BG, ?ANSI_INVERSE).
-define(RED_BG, ?MENU_SELECTED_BG).
-define(L_GRAY_BG, ?MENU_UNSELECTED_BG).
-define(UNDERLINE, ?ANSI_UNDERLINE).

-define(NEW_LINE, "\e[0m\n|").
-define(I, <<" | ">>).
-define(I2, <<"|">>).
-define(W(_C_, _A_, _W_), {width_color, _C_, _A_, _W_}).
-define(W2(_C_, _A_, _W_), {width_color_2, _C_, _A_, _W_}).
-define(W(_A_, _W_), {width, _A_, _W_}).

-define(SELECTED_MENU_ITEM(Text), observer_cli_lib:selected_menu_item(Text)).
-define(UNSELECTED_MENU_ITEM(Text), observer_cli_lib:unselected_menu_item(Text)).
-define(SELECT(Text), ?SELECTED_MENU_ITEM(Text)).
-define(UNSELECT(Text), ?UNSELECTED_MENU_ITEM(Text)).

-define('render'(_FA_), observer_cli_lib:render(_FA_)).
-define('output'(_F_, _A_), io:format(iolist_to_binary(_F_), _A_)).
-define('output'(_L_), io:put_chars(_L_)).

-define(DEFAULT_FORMATTER, #{application => observer_cli, mod => observer_cli_formatter_default}).
