# Goal 50 manual terminal QA

Date: 2026-07-06 Asia/Shanghai
Scope: checklist item 50 only.

Terminal and commands:

- Real Codex PTY with `stty rows 40 cols 120`.
- `rebar3 shell`, then `observer_cli:start().`
- Navigation inputs used in the PTY: `S`, `N`, `E`, `M`, `A`, `D`, `P`, `H`, and Home row `1` for Process Info.
- Dedicated Port Info PTY: `rr("include/observer_cli.hrl").`, `gen_tcp:listen(0, [binary, {active,false}, {reuseaddr,true}, {ip,{127,0,0,1}}])`, then `observer_cli_port:start(LS, #view_opts{}).`

Observed pages:

- Home: rendered top menu, system summary, process table, and footer at 120x40.
- System: rendered System/Architecture, CPU/thread, memory, allocator, cache-hit, and quit footer sections.
- Process: selecting Home row `1` rendered Process Info with Meta, Memory Used, Garbage Collection, Links, Monitors, Reductions, and Memory sections.
- Port: live TCP listener fixture rendered Port Info with Attr/Value rows, Links, Monitors, sockname/peername, recv/send stats, socket options, and quit footer.
- Network: rendered byte input/output summary and handled the local empty `recon:inet_count(cnt, 35)` state without crashing.
- ETS: rendered ETS table rows and columns for table name, size, memory, type, protection, key position, write/read, and owner PID.
- Mnesia: rendered the expected local error state: `Mnesia is not running on: nonode@nohost`.
- Application: rendered application rows with process count, memory, reductions, message queue, status, and version columns.
- Doc: rendered Start Mode, Global Commands, Home commands, process select examples, and references.
- Empty Plugin state: rendered `No plugins found.` and the plugin authoring hint.

Notes:

- No user-visible stale redraw, obvious wrap regression, navigation failure, or crash was observed in this 120x40 PTY sweep.
- This was normal `rebar3 shell` TUI validation. Raw `-noshell`, remote startup, and escript startup were not run because they belong to neighboring/raw-startup goals, including checklist item 51.
