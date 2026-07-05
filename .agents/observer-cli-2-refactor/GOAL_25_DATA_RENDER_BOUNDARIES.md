# Goal 25: collected data to TUI render input boundaries

Checklist item: Identify the boundary between "collected data" and "TUI render input" for each major page. Use existing Erlang terms / maps first; do not introduce a behavior layer.

Result:

- No production code changed for this slice.
- The snapshot seam is the existing Erlang term passed into the first page renderer, not rendered TUI text and not a new callback/behavior.
- Page option records such as `#home{}`, `#inet{}`, `#db{}`, and `#plug{}` remain control/render options, not collected runtime data.
- Future JSON/term output should start from the collected terms below and should not parse ANSI/iolist output.

## Page boundary inventory

| Page | Existing collected data term | TUI render input boundary | Notes for later refactor goals |
|---|---|---|---|
| Home | `get_stable_system_info/0` returns `{[OtpRelease, SysVersionNode, ProcessLimit, PortLimit, EtsLimit], PortParallelism}`; `node_stats/2` returns `{Diffs, SchedulerUsageDiff, NewStats}`; `collect_top_n/5` returns the `recon:proc_count/2` or `recon:proc_window/3` process list. | Summary rows: `system_summary/3` and `memory_process_summary/3` feed `render_home_summary/1`; scheduler term feeds `render_scheduler_usage/1`; top-N list feeds `render_top_n_view/5`. | Goal 26 should keep these terms first. `node_stats/2` already formats some byte strings, so raw runtime sampling can be split there without changing the TUI. |
| System | `collect_system_info/1` returns a map with `cache_hit_info`, allocator block-size terms, `sbcs_to_mbcs` terms, `sys_info`, and `dist_nodes_info`. | `render_system_sections/1` consumes that map and dispatches to the existing section renderers. | This is already a clean map seam; keep it instead of adding a page behavior. |
| Application | `collect_app_info/0` returns a map: `App => {ProcCount, Memory, Reductions, MsgQueueLen, Status, Version}`. | Current renderer builds a sorted list from that map inside `render_app_info/3`, then renders rows. | Goal 28 can make the map/list handoff explicit, preserving the existing tuple shape and sort tuple `{0, {SortValue, Status}, Row}`. |
| ETS | `collect_ets_info/1` returns a list of `{0, SortValue, EtsInfoProplist}` from `ets:info/1`, with `unread/0` for dead/unreadable tables. | `render_ets_info/4` consumes that list, applies pagination, and renders rows. | This is already a direct list seam; keep the proplist rather than inventing a schema. |
| Mnesia | `collect_mnesia_info/2` returns `{error, Reason}` or a list of `{0, SortValue, TableInfoProplist}`. | The worker handles `{error, Reason}` directly; otherwise `render_mnesia/4` consumes the list. | Goal 29 should preserve the error/list split and hidden-system-table filter behavior. |
| Network | `collect_inet_info/5` returns the `recon:inet_count/2` or `recon:inet_window/3` list, normally `{Port, Value, StatTerms}` rows. | `render_inet_rows/3` consumes that list and returns `{PortList, Rows}` for row selection plus TUI output; `render_io_rows/1` currently samples and renders IO deltas together. | Goal 30 should collect per-port memory/io/peer fields before rendering. Do not add a generic network behavior; use the current list plus existing port metadata terms. |
| Process detail | `collect_process_info/1` returns `dead` or a map containing `process => ProcessViewMap`, `links`, `monitors`, `monitored_by`, `reductions`, and `memory`. Subviews return maps from `collect_process_messages/1`, `collect_process_dictionary/1`, `collect_process_stack/1`, and raw state from `collect_process_state/1`. | `render_process_sections/3` consumes the info map. Subviews render through `render_process_messages/1`, `render_process_dictionary/1`, `render_process_stack/1`, and `render_process_state/2`. | Goal 31 should keep the existing maps and the `dead` branch. The less/state path can stay a process-detail subview seam. |
| Port detail | `collect_port_info/1` returns `dead` or `#{port => PortViewMap, links => Links, monitors => Monitors, type => Type}`. | `render_port_sections/1` consumes the map and dispatches to `render_port_info/1`, `render_link_monitor/2`, and `render_type_line/1`. | This is already a clean map seam; keep the existing attr/value map. |
| Plugin | Plugin modules provide data through existing callbacks: `attributes/1` returns rows of maps, `sheet_header/0` returns header maps, and `sheet_body/1` returns row lists plus state. `init_config/1` normalizes plugin config maps. | `render_attributes/2`, `render_sheet_header/2`, `render_sheet_body/8`, and `render_sheet/4` consume those callback return terms. | Phase 5 can change plugin API explicitly, but Goal 25 does not add a behavior layer or change callback shapes. |
| Doc | There is no runtime collection. `render_help/0` returns the static help iolist. | `render_doc/1` combines menu/footer with `render_help/0`. | No snapshot collector is needed for the static Doc page. |

## Decisions

- Reuse current maps, proplists, tuples, and lists as the first snapshot seam.
- Treat ANSI/iolists from `?render`, `observer_cli_lib:render_top_menu/2`, and `observer_cli_lib:render_footer/1` as after the TUI boundary.
- Do not introduce a `page_behavior`, generic renderer interface, or new dependency for this item.
- No prerequisite checklist item was touched.

Validation:

- `rebar3 compile` -> passed.
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_system_test,observer_cli_application_test,observer_cli_ets_test,observer_cli_mnesia_test,observer_cli_inet_test,observer_cli_process_test,observer_cli_port_test,observer_cli_plugin_test,observer_cli_plugin_render_test,observer_cli_help_test` -> 225 tests, 0 failures.
- Known non-fatal baseline output remains: `observer_cli_process:render_state/3` logged the existing timeout warning during the focused EUnit run.

Skipped:

- Manual terminal QA is not required for this docs-only boundary inventory; no user-visible rendering code changed.
