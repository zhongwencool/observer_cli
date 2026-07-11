# Developer notes: 2.0 module responsibilities

The 2.0 refactor keeps normal CLI behavior stable while making ownership easier
to see. Keep future changes page-scoped unless a shared helper already exists;
do not add a generic TUI framework.

## Responsibility map

| Area | Owning modules | Responsibility |
| --- | --- | --- |
| Entry points | `observer_cli`, `observer_cli_escriptize`, page `start/1` modules | Public local, remote, escript, and plugin startup; carry page state in `#view_opts{}`. |
| Commands | `observer_cli_command`, `observer_cli_lib`, page managers | Parse shared keys once, route top-menu commands, clean old workers, then let each page manager apply page-specific state changes. |
| Collection | Page modules | Gather BEAM, OS, recon, plugin, and formatter inputs into Erlang terms before text rendering. |
| Rendering | Page modules, `observer_cli_lib`, `observer_cli_help` | Turn collected terms into the existing TUI rows, menus, ANSI styling, footer text, and width-aware padding. |
| Pagination | `observer_cli_lib`, `observer_cli_store`, page records | Own page math, selected-row positions, and per-page `cur_page` / `pages` state. |
| Plugin | `observer_cli_plugin`, `observer_cli_plugin_compat` | Own plugin registration, 2.0 callback shapes, compatibility migration, sheet rendering, sorting, shortcuts, and row drill-down. |
| Formatter | `observer_cli_formatter`, `observer_cli_formatter_default`, `observer_cli_process` | Own process State formatting; default behavior must remain compatible and custom formatter failures fall back to the default formatter. |
| Diagnostics CLI | `observer_cli_cli`, `observer_cli_escriptize` | Parse command-first arguments, manage bounded context metadata, start the outbound-only controller, encode envelopes, and preserve the positional TUI route. |
| Diagnostics target | `observer_cli_snapshot`, `observer_cli_diagnostic`, `observer_cli_trace` | Dispatch bounded target workers, normalize/cap results, compose snapshots and findings, and own the recon 2.5.6 trace lifecycle. |

## Current page boundaries

| Page / area | Entry and lifecycle | Collection seam | Render seam |
| --- | --- | --- | --- |
| Home | `observer_cli:start/0,1,2`, `manager/4`, `render_worker/4` | `collect_home_snapshot/6`, `collect_top_n/5` | `render_home_snapshot/2`, `render_top_n_view/5` |
| System | `observer_cli_system:start/1`, `manager/2`, `render_worker/3` | `collect_system_info/1`, `collect_sys_info/1` | `render_system_sections/1`, `render_sys_info/1` |
| Application | `observer_cli_application:start/1`, `manager/2`, `render_worker/2` | `collect_app_info/0`, `collect_app_render_info/3` | `render_app_info/2` |
| ETS | `observer_cli_ets:start/1`, `manager/2`, `render_worker/5` | `collect_ets_info/1`, `collect_ets_render_info/3` | `render_ets_info/2` |
| Mnesia | `observer_cli_mnesia:start/1`, `manager/2`, `render_worker/6` | `collect_mnesia_info/2`, `collect_mnesia_render_info/4` | `render_mnesia/2` |
| Network | `observer_cli_inet:start/1`, `manager/3`, `render_worker/6` | `collect_io_info/1`, `collect_inet_info/5`, `collect_inet_render_info/3` | `render_io_info/1`, `render_inet_rows/3` |
| Process detail | `observer_cli_process:start/3`, detail `render_worker/*` loops | `collect_process_info/1`, `collect_process_messages/1`, `collect_process_dictionary/1`, `collect_process_stack/1`, `collect_process_state/1` | `render_process_info/1`, `render_process_messages/1`, `render_process_dictionary/1`, `render_process_stack/1`, `render_process_state/2` |
| Port detail | `observer_cli_port:start/2`, `manager/2`, `render_worker/3` | `collect_port_info/1` | `render_port_sections/1`, `render_port_info/1` |
| Plugin | `observer_cli_plugin:start/1`, `manager/3`, `render_worker/6` | plugin `attributes/1`, `sheet_header/0`, `sheet_body/1` through `observer_cli_plugin_compat` | `render_attributes/2`, `render_sheet/4`, `render_menu/2` |
| Formatter | `observer_cli_process` State view | app env `{formatter, #{mod := Mod}}` | `observer_cli_formatter:format/3` with `observer_cli_formatter_default` fallback |
| Help / Doc | `observer_cli_help:start/1`, `manager/2`, `render_worker/1` | static command/help text | `render_help/0`, `render_doc/1` |

## Rules for future slices

- Put new collection work beside the page that owns the runtime call.
- Keep shared command parsing in `observer_cli_command`; keep routing and worker
  cleanup in `observer_cli_lib:parse_cmd/3`.
- Use `observer_cli_lib` pagination helpers instead of open-coded page math.
- Keep plugin 2.0 changes in `observer_cli_plugin` and
  `observer_cli_plugin_compat`, with docs and tests for accepted, migrated, and
  rejected shapes.
- Keep TUI page collection/rendering unchanged. Diagnostics commands use the four
  diagnostics modules and must not parse ANSI-rendered page output.
- Add no per-command modules or provider abstractions. New target probes must stay
  deadline-, heap-, schema-, and response-cap bounded and must normalize sensitive
  values before they cross distribution.
- Never reuse the legacy `remote_load/1` path for command-first diagnostics. Cross-OTP
  operation requires a compatible target-side installation built for that OTP major.
- Run real disposable-node cleanup tests for distribution, scheduler flags, target
  workers, and recon tracing; a controller timeout alone does not cancel target work.
