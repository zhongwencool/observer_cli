
-define(COLUMN_WIDTH, 133).

-define(SYSTEM_MIN_INTERVAL, 5000).
-define(ALLOCATOR_MIN_INTERVAL, 5000).
-define(HOME_MIN_INTERVAL, 2000).
-define(HELP_MIN_INTERVAL, 1000).
-define(PROCESS_MIN_INTERVAL, 1000).
-define(MNESIA_MIN_INTERVAL, 5000).
-define(INET_MIN_INTERVAL, 2000).

-record(home, {func = proc_count :: atom(),
               type = memory :: atom(),
               cur_pos = 1 :: integer(),
               rows = 26 :: integer(),
               interval = ?SYSTEM_MIN_INTERVAL :: integer()}).

-record(system, {interval = ?SYSTEM_MIN_INTERVAL :: integer(),
                 rows = 30 :: integer()}).
-record(allocate, {interval = ?SYSTEM_MIN_INTERVAL :: integer()}).

-record(db, {interval = ?MNESIA_MIN_INTERVAL :: integer(),
             rows = 37 :: integer()}).

-record(help, {interval = ?HELP_MIN_INTERVAL :: integer()}).
-record(inet, {interval = ?INET_MIN_INTERVAL :: integer(),
               rows = 39 :: integer(),
               func = inet_count :: atom(),
               type = cnt :: atom()}).

-record(process,{interval = ?PROCESS_MIN_INTERVAL :: integer()}).

-record(view_opts, {home = #home{} :: home(),
                    sys = #system{} :: system(),
                    allocate = #allocate{} :: allocate(),
                    db = #db{} :: db(),
                    help = #help{} :: help(),
                    inet = #inet{} :: inet(),
                    process = #process{} :: process()
                   }).

-export_type([view_opts/0]).

-type(view_opts():: #view_opts{}).
-type(home():: #home{}).
-type(system():: #system{}).
-type(allocate():: #allocate{}).
-type(db():: #db{}).
-type(help():: #help{}).
-type(inet():: #inet{}).
-type(process():: #process{}).

