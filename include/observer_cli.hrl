
-define(SYSTEM_MIN_INTERVAL, 5000).
-define(ALLOCATOR_MIN_INTERVAL, 5000).
-define(HOME_MIN_INTERVAL, 2000).
-define(HELP_MIN_INTERVAL, 1000).
-define(PROCESS_MIN_INTERVAL, 1000).
-define(MNESIA_MIN_INTERVAL, 5000).

-define(HOME_BROAD, 133).
-define(ALLOCATOR_BROAD, 133).
-define(SYSTEM_BROAD, 133).
-define(HELP_BROAD, 133).
-define(PROCESS_BROAD, 133).
-define(MNESIA_BROAD, 133).

-record(home, {func = proc_count, type = memory, cur_pos = 1, interval = ?SYSTEM_MIN_INTERVAL}).
-record(system, {interval = ?SYSTEM_MIN_INTERVAL}).
-record(allocate, {interval = ?SYSTEM_MIN_INTERVAL}).
-record(db, {interval = ?MNESIA_MIN_INTERVAL}).
-record(help, {interval = ?HELP_MIN_INTERVAL}).
-record(process,{interval = ?PROCESS_MIN_INTERVAL}).

-record(view_opts, {home = #home{},
                    sys = #system{},
                    allocate = #allocate{},
                    db = #db{},
                    help = #help{},
                    process = #process{}}).

-export_type([view_opts/0]).

-type(view_opts():: #view_opts{}).


