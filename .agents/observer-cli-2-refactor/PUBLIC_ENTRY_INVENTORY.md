# observer_cli public-entry inventory

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 2 only

This file is an inventory only. It does not change startup behavior, command behavior, plugin callbacks, formatter behavior, or the 2.0 plugin API.

## Source scan used for this inventory

- GitHub issue source: `gh issue view 133 --repo zhongwencool/observer_cli --json number,title,state,updatedAt,body,comments`.
- Startup/API source: `src/observer_cli.erl`, `src/observer_cli_escriptize.erl`, `include/observer_cli.hrl`, `rebar.config`, `README.md`.
- Plugin source/docs: `src/observer_cli_plugin.erl`, `docs/plugin.md`.
- Formatter source/docs: `src/observer_cli_formatter.erl`, `src/observer_cli_formatter_default.erl`, `src/observer_cli_process.erl`, `docs/formatter.md`.
- Interactive command source: `src/observer_cli_lib.erl`, built-in page modules, `src/less_client.erl`, `src/observer_cli_help.erl`.

## Startup and configuration entries

| Entry family | Public/user entry | Source touchpoints | Current behavior to preserve |
| --- | --- | --- | --- |
| Normal startup | `observer_cli:start/0` | `src/observer_cli.erl` | Starts Home with default `#view_opts{}`, creates the store/render worker, enables trap exits, manages scheduler wall-time restoration, and exits through `q` as `quit`. |
| Local startup variants | `observer_cli:start(node())`, `observer_cli:start(#view_opts{})`, `observer_cli:start(Interval)` where `Interval >= ?MIN_INTERVAL`, and `observer_cli:start(Node, _Cookie)` when `Node =:= node()` | `src/observer_cli.erl`, `include/observer_cli.hrl` | Local variants stay in the same VM. Integer startup applies the interval to Home/System/ETS/Mnesia/Network/Process/Port/Doc options. Internal `#view_opts{}` restarts are used by page managers and must keep page state records compatible. |
| Remote startup | `observer_cli:start(Node)`, `observer_cli:start(Node, Cookie)`, `observer_cli:start(Node, Options)` with `[{cookie, Cookie}, {interval, Ms}]` | `src/observer_cli.erl`, `README.md` | Uses `net_kernel:hidden_connect_node/1`, optional `erlang:set_cookie/2`, net ticktime sync, then `rpc:call(Node, observer_cli, start, [Interval])`. Connection refusal/ignored local node return `{badrpc, nodedown}` after printing the existing error text. |
| Escript startup | `_build/default/bin/observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]` | `rebar.config`, `src/observer_cli_escriptize.erl`, `README.md` | `observer_cli_escriptize:main/1` accepts one target arg with default cookie `undefined` and interval `1500`, or three args with explicit cookie/interval. It starts a hidden local distributed node, tries remote start, loads required observer_cli/recon/formatter modules and env to the target, waits only when test stop env is enabled, then starts remote monitoring again. |
| Escript remote env support | `observer_cli_escriptize:ensure_set_env/2` | `src/observer_cli_escriptize.erl` | RPC helper copies app env to the remote node only when the app env is empty there. This is part of escript remote-load behavior, not a separate user command. |
| Plugin startup | `observer_cli:start_plugin/0`; top-menu `P`; plugin shim modules may call `observer_cli:start_plugin()` | `src/observer_cli.erl`, `src/observer_cli_plugin.erl`, `docs/plugin.md` | Starts directly in Plugin mode after `application:ensure_all_started(observer_cli)`. Built-in pages still start from Home by default. Empty plugin config renders the existing empty-plugin message and remains inert until navigation/quit. |
| Plugin configuration | `application` env key `plugins` | `src/observer_cli_plugin.erl`, `docs/plugin.md` | Config entries are maps with required `module`, `title`, `shortcut`; optional `interval`, `sort_column`, and `handler`. `init_config/1` assigns defaults (`cur_page = 1`, `cur_row = 1`, `sort_column = 2`, `interval = 1500`, computed `sheet_width`). |
| Plugin callbacks | `observer_cli_plugin` callbacks `attributes/1`, `sheet_header/0`, `sheet_body/1` | `src/observer_cli_plugin.erl`, `docs/plugin.md` | Attributes render banner rows, sheet headers render sortable columns/shortcuts, sheet body returns rows and state. Missing callbacks are caught as `undef` and render empty sections rather than changing built-in pages. |
| Plugin row handler | Optional config `handler => {PredicateFun, HandlerModule}` | `src/observer_cli_plugin.erl`, `docs/plugin.md` | Row selection searches the selected row for the first item matching the predicate. Default is `{fun is_pid/1, observer_cli_process}`. Handler modules use `HandlerModule:start(plugin, Item, ViewOpts)` like the process detail entry. |
| Formatter configuration | `application` env key `formatter`, default `#{application => observer_cli, mod => observer_cli_formatter_default}` | `include/observer_cli.hrl`, `src/observer_cli_formatter.erl`, `src/observer_cli_formatter_default.erl`, `src/observer_cli_process.erl`, `docs/formatter.md` | Formatter modules implement `observer_cli_formatter:format/2`; process State/Messages/Dictionary call through `observer_cli_formatter:format/3`. Custom formatter failures fall back to `observer_cli_formatter_default`. Escript remote-load includes the configured formatter application. |

## Major interactive command inventory

All command strings are line-based input through `io:get_line("")`; uppercase/lowercase differences are current behavior.

### Shared top menu and shared parser

Source: `observer_cli_lib:parse_cmd_str/1` and `observer_cli_lib:parse_cmd/3`.

| Command | Action |
| --- | --- |
| `H` | Home |
| `S` | System |
| `A` | Application |
| `N` | Network |
| `M` | Mnesia; the menu title is hidden when the `schema` table is absent, but parsing still recognizes `M`. |
| `E` | ETS |
| `D` | Doc/help |
| `P` | Plugin |
| `q` / `Q` | Quit current page/session |
| `F`, `PD`, `pd` | Page forward for pages that consume pagination |
| `B`, `PU`, `pu` | Page back for pages that consume pagination |
| Integer `>= 1000` | Set refresh interval in milliseconds |
| Integer `1..999` | Row jump for pages that maintain row stores/current rows |
| Other strings | `{input_str, String}`; used by Plugin shortcuts and ignored by most built-in pages |
| Read error / stale input | Quit |

### Home

Source: `observer_cli:manager/4`, `observer_cli_lib:parse_cmd_str/1`.

| Command | Action |
| --- | --- |
| `p` | Pause/resume Home redraw |
| `` ` `` | Toggle scheduler usage display |
| `r`, `m`, `b`, `t`, `mq` | Sort Home top list by reductions, memory, binary memory, total heap size, or message queue length using `proc_count` |
| `rr`, `mm`, `bb`, `tt`, `mmq` | Same sort targets using `proc_window` |
| `F`/`B` or `pd`/`pu` | Page top list forward/back |
| Integer `1..999` or Enter | Jump to the selected/current process row |
| `<X.Y.Z>`, `>X.Y.Z`, `<Y`, `>Y` | Jump directly to a process pid |
| `H/S/A/N/M/E/D/P`, `q/Q`, integer interval | Shared top-menu/quit/interval behavior |

### System

Source: `observer_cli_system:manager/2`.

| Command | Action |
| --- | --- |
| Integer `>= 1000` | Update System refresh interval without changing page |
| `q/Q`, `H/A/N/M/E/D/P` | Shared quit/top-menu behavior |
| Other shared page/sort commands | Ignored by the System manager |

### Application

Source: `observer_cli_application:manager/2`, README Application shortcut note.

| Command | Action |
| --- | --- |
| `p` | Sort by process count (`pause_or_resume` action is interpreted as ProcessCount here) |
| `m` | Sort by memory |
| `r` | Sort by reductions |
| `mq` | Sort by message queue length |
| `F`/`B` or `pd`/`pu` | Page forward/back |
| Integer `>= 1000` | Update refresh interval |
| `q/Q`, `H/S/N/M/E/D/P` | Shared quit/top-menu behavior |

### Network and Port detail

Source: `observer_cli_inet:manager/3`, `observer_cli_port:parse_cmd_str/1`.

| Command | Action |
| --- | --- |
| `ic`, `iw` | Switch Network mode between `inet_count` and `inet_window` |
| `rc`, `ro`, `sc`, `so`, `cnt`, `oct` | Switch Network sort/stat type |
| `F`/`B` or `pd`/`pu` | Page Network rows forward/back |
| Integer `1..999` or Enter | Open selected/current port detail from Network |
| Port detail `H`, `N`, `P` | Go Home, Network, or stay/show Port Info |
| Integer `>= 1000` | Update Network or Port detail refresh interval |
| `q/Q`, shared top-menu commands | Quit/top-menu behavior where the current manager consumes it |

### ETS and Mnesia

Source: `observer_cli_ets:manager/2`, `observer_cli_mnesia:manager/2`.

| Command | Action |
| --- | --- |
| `s` | Sort by size |
| `m` | Sort by memory |
| Mnesia `hide` | Toggle hidden system tables |
| `F`/`B` or `pd`/`pu` | Page forward/back |
| Integer `>= 1000` | Update refresh interval |
| `q/Q`, shared top-menu commands | Quit/top-menu behavior |

### Process detail and less-backed state view

Source: `observer_cli_process:parse_cmd_str/1`, `observer_cli_process:state_nav/1`, `less_client:loop/1`.

| Command | Action |
| --- | --- |
| `P` | Process Info subview |
| `M` | Messages subview |
| `D` | Dictionary subview |
| `C` | Current Stack subview |
| `S` | State subview |
| `H` | Home |
| `B` | Back to Home when opened from Home, or Plugin when opened from Plugin |
| Integer `>= 1000` | Update Process detail refresh interval |
| State/less `F` or `j` | Next less page |
| State/less `B` or `k` | Previous less page, unless `B` is reserved by the state nav map for plugin back |
| State/less nav keys `H/P/M/D/C` and plugin-origin `B` | Leave less and route through the Process manager |
| Process parser `q/Q` | Quit Process detail; in the less-backed State view, `q/Q` leaves the less page and returns to the detail loop |

### Plugin page

Source: `observer_cli_plugin:parse_cmd_str/1`, `observer_cli_plugin:maybe_shortcut/2`.

| Command | Action |
| --- | --- |
| `H` | Home |
| `F`/`B` | Page plugin sheet forward/back |
| Enter | Jump selected/current plugin row |
| Integer `1..999` | Jump that plugin row |
| Integer `>= 1000` | Update current plugin refresh interval |
| Configured plugin menu shortcut | Switch current plugin tab |
| Configured sheet-header shortcut | Sort plugin sheet by that column |
| `q` | Quit plugin page |

### Doc/help

Source: `observer_cli_help:start/1`, `observer_cli_help:render_help/0`.

| Command | Action |
| --- | --- |
| `q/Q` | Quit Doc/help |
| Shared top-menu commands | Leave Doc/help for the selected page |
| Rendered help | Documents start modes, global page navigation and interval commands, Home commands, and process-selection examples |

## Guardrail notes for later slices

- This inventory intentionally does not add smoke tests; checklist item 3 owns startup/page-switch/interval/pagination/process-jump tests.
- `rebar3 shell` validates the normal shell entry, but it is not proof for any future raw `-noshell` terminal-input design.
- Escript remote startup and remote-node monitoring are runtime/distribution paths; deterministic tests should prefer the existing peer/distribution helpers and must avoid entering the full remote TUI unless the slice explicitly asks for that.
- Plugin API breaking changes remain deferred to the plugin 2.0 phase. Until then, the current callback/config/handler behavior above is the compatibility surface.

## Validation for this slice

- `gh issue view 133 --repo zhongwencool/observer_cli --json number,title,state,updatedAt,body,comments` confirmed the source issue was open and still updated at `2026-07-05T12:39:22Z`.
- Static source/docs scan refreshed the startup, plugin, formatter, and command surfaces listed above.
- `rebar3 as test eunit --module=observer_cli_start_test,observer_cli_lib_test,observer_cli_process_test,observer_cli_port_test,observer_cli_plugin_test,observer_cli_escriptize_test` passed: 176 tests, 0 failures.
- Known non-fatal test output: `observer_cli_process:render_state/3` can log a timeout warning while EUnit still passes.
- No manual terminal QA was run because this slice records inventory only; runtime smoke/terminal checks belong to later checklist items 3 and 50.
