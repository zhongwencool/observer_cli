-define(COLUMN, 135).

-define(MIN_INTERVAL, 1000).
-define(DEFAULT_INTERVAL, 1500).

-record(home, {func = proc_count :: atom(),
    type = memory :: atom(),
    cur_pos = 1 :: integer(),
    tid = undefined :: reference(),
    interval = ?DEFAULT_INTERVAL :: integer()}).

-record(system, {interval = ?DEFAULT_INTERVAL :: integer()}).
-record(allocate, {interval = ?DEFAULT_INTERVAL :: integer()}).

-record(db, {interval = ?DEFAULT_INTERVAL :: integer()}).

-record(help, {interval = ?DEFAULT_INTERVAL :: integer()}).
-record(inet, {interval = ?DEFAULT_INTERVAL :: integer(),
    func = inet_count :: atom(),
    type = cnt :: atom()}).

-record(process, {interval = ?DEFAULT_INTERVAL :: integer()}).

-record(view_opts, {home = #home{} :: home(),
    sys = #system{} :: system(),
    allocate = #allocate{} :: allocate(),
    db = #db{} :: db(),
    help = #help{} :: help(),
    inet = #inet{} :: inet(),
    process = #process{} :: process(),
    auto_row = true :: boolean()
}).

-export_type([view_opts/0]).

-type(view_opts() :: #view_opts{}).
-type(home() :: #home{}).
-type(system() :: #system{}).
-type(allocate() :: #allocate{}).
-type(db() :: #db{}).
-type(help() :: #help{}).
-type(inet() :: #inet{}).
-type(process() :: #process{}).

-define(CURSOR_TOP, <<"\e[H">>).
-define(CLEAR, <<"\e[H\e[J">>).

-define(RESET_BG, <<"\e[49m">>).
-define(RESET, <<"\e[0m">>).
-define(GRAY_BG, <<"\e[7m">>).
-define(YELLOW, <<"\e[33m">>).
-define(RED, <<"\e[31m">>).
-define(L_RED, <<"\e[48m">>).
-define(GREEN, <<"\e[32;1m">>).
-define(L_GREEN, <<"\e[92m">>).
-define(RED_BG, <<"\e[48;2;184;0;0m">>).
-define(L_GRAY_BG, <<"\e[48;2;80;80;80m">>).
-define(UNDERLINE, <<"\e[4m">>).
-define(I, <<" | ">>).
-define(W(_C_, _A_, _W_), {extend_color, _C_, _A_, _W_}).
-define(W(_A_, _W_), {extend, _A_, _W_}).

-define(render(_FA_), observer_cli_lib:render(_FA_)).
-define(output(_F_, _A_), io:format(_F_, _A_)).
-define(output(_L_), io:format(_L_)).


