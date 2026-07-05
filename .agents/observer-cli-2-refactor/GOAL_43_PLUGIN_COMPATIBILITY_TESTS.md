# Goal 43: plugin compatibility tests

Checklist item: Add plugin compatibility tests that state whether 1.x plugin behavior is retained, migrated, or rejected with a clear error.

## Scope

Changed only plugin-compatibility contract code/tests and this evidence file:

- `src/observer_cli_plugin_compat.erl`
- `test/observer_cli_plugin_compat_test.erl`
- `.agents/observer-cli-2-refactor/GOAL_43_PLUGIN_COMPATIBILITY_TESTS.md`

No renderer, manager loop, docs/plugin.md, or normal built-in CLI page behavior changed in this slice. Goal 44 still owns wiring the plugin renderer to the new internal data structure.

## Compatibility decisions covered by tests

Retained:

- plugin config keys `module`, `title`, `shortcut`, `interval`, `cur_page`, and `cur_row`
- header `shortcut` fields
- byte and percent attribute cell values
- default pid drill-down through `observer_cli_process`
- missing-callback handling policy for absent plugin callback modules

Migrated:

- `sort_column => N` becomes `sort => ColumnId`
- `handler => {PredicateFun, HandlerModule}` becomes `handler => HandlerModule`, with the selected row value coming from explicit row `handle`

Rejected with `error({plugin_api_error, #{source := Source, reason := Reason}})`:

- 1.x `{Rows, State}` attribute callback return
- 1.x list-only `sheet_header/0` return
- 1.x `{Rows, State}` sheet callback return
- list rows inside 2.0 sheet body data
- duplicate column ids
- invalid `default_sort`, `sort`, or `sort_column`
- default handler selection for a non-pid row `handle`

## Validation

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test,observer_cli_plugin_compat_test` -> 38 tests, 0 failures
- `rebar3 compile`
- `git diff --check`
- `rebar3 as ci compile`
- `rebar3 eunit` -> 352 tests, 0 failures
- `rebar3 check`

Known warning remains the existing non-fatal `observer_cli_process:render_state/3` timeout warning during full EUnit.

Skipped:

- Runtime terminal QA; this slice adds compatibility contract tests and does not wire plugin rendering changes yet.
