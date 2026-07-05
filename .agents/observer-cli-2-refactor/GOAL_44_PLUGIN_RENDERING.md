# Goal 44: plugin renderer internal data wiring

Checklist item: Update plugin rendering to use the new internal data structure while keeping normal plugin page behavior unchanged for users.

## Scope

Changed only the plugin renderer path, plugin-focused tests, the local test plugin, and this evidence file:

- `src/observer_cli_plugin.erl`
- `test/observer_cli_test_plugin.erl`
- `test/observer_cli_plugin_test.erl`
- `test/observer_cli_plugin_render_test.erl`
- `test/observer_cli_plugin_compat_test.erl`
- `.agents/observer-cli-2-refactor/GOAL_44_PLUGIN_RENDERING.md`

No built-in non-plugin page, manager framework, `docs/plugin.md`, README, or migration guide changed. Goal 45 still owns public plugin migration documentation.

No prerequisite checklist item was touched.

## Behavior and API decision

Plugin rendering now consumes the goal 42/43 internal structure directly:

- `attributes/1` returns `#{rows := Rows, state := NewState}` and is normalized before rendering.
- `sheet_header/0` returns `#{columns := Columns, default_sort := ColumnId}`.
- Sheet header highlighting and shortcut routing use column ids instead of 1-based indexes.
- `sheet_body/1` returns `#{rows := Rows, state := NewState}` where each row is `#{cells := #{ColumnId => Value}}` with optional `handle`.
- Body rendering reads cells by declared column id, renders missing display cells as empty, ignores extra cells, and keeps pagination and row-cache behavior.
- Row selection uses explicit row `handle` plus optional handler module instead of scanning display cells with a predicate.
- Config initialization still migrates `sort_column => N` and handler tuples through `observer_cli_plugin_compat` so the compatibility decisions from goal 43 remain exercised.

Normal plugin page user behavior is preserved by tests for startup/quit, menu shortcut routing, sheet shortcut routing, pagination, interval changes, row selection, empty plugin rendering, missing callback fallback, percent/byte formatting, and full EUnit coverage.

## Validation

- `gh issue view 133 --repo zhongwencool/observer_cli --json number,title,body,updatedAt,url` -> confirmed issue #133 and checklist item 44.
- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test,observer_cli_plugin_compat_test` -> 38 tests, 0 failures.
- `rebar3 compile`
- `git diff --check`
- `rebar3 eunit` -> 352 tests, 0 failures. Known non-fatal `observer_cli_process:render_state/3` timeout warning appeared, same as prior plugin slices.
- `rebar3 as ci compile`
- `rebar3 check`

Skipped:

- Runtime terminal QA; this slice is covered by plugin manager/render EUnit and does not alter non-plugin terminal pages. Goal 50 still owns broad manual terminal inspection.
