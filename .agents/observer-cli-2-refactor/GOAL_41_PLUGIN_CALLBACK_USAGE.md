# Goal 41: current plugin callback usage

Checklist item: Evaluate the real current plugin callback usage and list capabilities that must remain: attributes, sheet header, sheet body, shortcut, sorting, and row handler.

## Scope

No Erlang source, tests, generated docs, or public plugin API changed in this slice. This is the Phase 5 inventory that feeds goal 42; it deliberately does not design the breaking 2.0 shape yet.

Checked sources:

- `src/observer_cli_plugin.erl`
- `docs/plugin.md`
- `test/observer_cli_test_plugin.erl`
- `test/observer_cli_plugin_test.erl`
- `test/observer_cli_plugin_render_test.erl`
- public example `zhongwencool/os_stats` via `gh api`
- public example `processone/ejabberd-contrib/ejabberd_observer_cli` via `gh api`

## Current callback and config contract

| Capability | Current usage that exists today | Must remain for plugin 2.0 migration |
| --- | --- | --- |
| Attributes | `attributes/1` is called on every render with the previous attribute state. It returns `{Rows, NewState}` where `Rows` is a list of row lists; each cell is a map with `content` and `width`, plus optional `color`. Runtime renders `{percent, Float}` specially and `observer_cli_lib:render/1` handles `{byte, Bytes}`. Public examples use prior state for rate deltas and use ANSI color cells. | Keep a banner/attribute area with per-cell width, optional color, scalar content, byte formatting, percent formatting, and independent callback state. Missing `attributes/1` currently degrades to no attributes; if 2.0 rejects that, the error must be explicit and tested. |
| Sheet header | `sheet_header/0` returns ordered header maps. `title` and `width` drive the visible columns; `shortcut` is rendered as `Title(Key)` when present. Runtime accepts headers without `shortcut` through `maps:get(shortcut, H, "")`, and `os_stats` currently omits header shortcuts. `get_sheet_width/1` also derives plugin page width from these widths. | Keep ordered headers, explicit column widths, visible titles, no-shortcut columns, and page width derived from headers unless 2.0 replaces it with an equally explicit width field. |
| Sheet body | `sheet_body/1` is called with previous sheet state and returns `{Rows, NewState}`. Each row is an ordered list of cell terms. Rendering assumes row order matches header order, formats cells through `observer_cli_lib:to_list/1` / `{byte, Bytes}`, stores each displayed row in ETS, and paginates rendered rows. | Keep independent sheet state, ordered rows, existing scalar cell rendering, byte formatting, pagination, and row-cache semantics needed by numeric row selection. If 2.0 names cells, migration must still preserve the existing ordered display. |
| Shortcut | Plugin config `shortcut` switches the active plugin page. Header `shortcut` switches the active sort column. Both are exact string matches after the user enters a command line. Tests cover menu shortcuts and sheet shortcuts; ejabberd examples rely on both. | Keep plugin menu shortcuts and sheet sort shortcuts as user-visible commands. Existing lower/upper-case strings must remain exact matches unless migration docs/tests explicitly reject a value. |
| Sorting | `init_config/1` defaults `sort_column` to `2`; config can override it. A sheet shortcut updates `sort_column` to the 1-based header index. Body rows are ranked by `lists:nth(SortColumn, Row)`, passed through `observer_cli_lib:sublist/3`, and the selected header is highlighted. | Keep config/default sort column behavior, header shortcut sort changes, visible selected-sort highlighting, and sorted pagination. Goal 42 may remove the implicit `lists:nth/2` guessing, but must preserve the ability to sort by declared columns. |
| Row handler | Plugin config may include `handler => {PredicateFun, HandlerModule}`. Without it, the default is `{fun is_pid/1, observer_cli_process}`. Numeric input saves `cur_row`; Enter uses current row. The manager looks up the displayed ETS row, picks the first row item matching the predicate, and calls `HandlerModule:start(plugin, Item, ViewOpts)`. If the row or match is missing, the command is a no-op. | Keep row selection, current-row memory, default pid drill-down to Process, custom predicate/module handlers, `start(plugin, Item, ViewOpts)` handler origin, and no-op behavior for missing rows/matches. |

## Real public usage observed

- `docs/plugin.md` documents config maps with `module`, `title`, `shortcut`, `interval`, `sort_column`, and optional `handler`; it documents the three callbacks and custom row handlers.
- `zhongwencool/os_stats` implements only `attributes/1`, `sheet_header/0`, and `sheet_body/1`. Its headers omit sort shortcuts, its plugin config still sets `shortcut => "O"` and `sort_column => 2`, and its attributes/sheet callbacks keep previous sampling state.
- `processone/ejabberd-contrib/ejabberd_observer_cli` sets multiple plugin menu shortcuts and `sort_column => 2`. Its plugin modules implement the same three callbacks and rely on header shortcuts for sorting; no custom handler is configured there.
- Local tests already cover plugin config defaults, menu shortcuts, sheet shortcuts, sheet sorting through the ETS row cache, default row jump behavior, custom row handler dispatch, missing-row no-op behavior, empty plugin behavior, and missing-callback fallbacks.

## 2.0 boundary notes for the next slice

- The current source callback spec and docs disagree in small ways: runtime supports `{percent, Float}` cells, and runtime allows header maps without `shortcut`.
- Sorting and row handling are position-based today: sorted columns use `lists:nth/2`, and handlers guess the first matching item in an ordered row.
- `handler` is a config option, not an `observer_cli_plugin` behaviour callback.
- `attributes/1` and `sheet_body/1` each carry independent previous-state values; do not merge those states by accident.

These are the only API seams this inventory names for goal 42. No prerequisite checklist item was touched.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body` -> confirmed issue #133 updated at `2026-07-05T12:39:22Z` and checklist item 41.
- `grep -RIn --exclude-dir=_build --exclude-dir=.git --exclude-dir=deps -E 'attributes\\(|sheet_header\\(|sheet_body\\(|shortcut|sort_column|handler' src include test docs README.md rebar.config` -> checked local callback/config call sites and tests.
- `gh search code 'observer_cli_plugin attributes sheet_header sheet_body language:Erlang' --limit 20 --json repository,path,url` -> found current public plugin examples; private results were ignored.
- `gh api repos/zhongwencool/os_stats/contents/src/os_stats_plug.erl` and `gh api repos/zhongwencool/os_stats/contents/README.md` -> checked the documented public example plugin.
- `gh api repos/processone/ejabberd-contrib/contents/ejabberd_observer_cli/src/ejabberd_observer_cli.erl` plus its plugin modules -> checked another public plugin package.
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test` -> 33 tests, 0 failures.
- `git diff --no-index --check -- /dev/null .agents/observer-cli-2-refactor/GOAL_41_PLUGIN_CALLBACK_USAGE.md` -> no whitespace errors; command exits 1 for a new file diff.

Skipped:

- Runtime terminal QA is not required for this docs-only inventory; no rendering, command, callback, or handler code changed.
