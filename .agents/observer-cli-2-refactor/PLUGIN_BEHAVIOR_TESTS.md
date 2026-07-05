# Goal 08 plugin behavior tests

Issue #133 Phase 0 item 8: add plugin behavior tests for current plugin configuration, menu shortcuts, sheet sorting, and row drill-down baseline.

## Scope

Changed only plugin-focused test fixtures:

- `test/observer_cli_plugin_test.erl`
- `test/observer_cli_plugin_render_test.erl`
- `test/observer_cli_test_handler.erl`

## Evidence added

- Env-backed plugin config now asserts preserved `module`, `title`, `shortcut`, `sheet_width`, and default `interval`, `cur_page`, `cur_row`, `sort_column`.
- Menu shortcut coverage now covers multiple configured plugin entries.
- Sheet sorting now asserts sort-column order through the plugin sheet ETS row cache.
- Numeric row drill-down now proves the matched row item, `plugin` handler origin, and saved `cur_row` passed to the handler.

## Validation

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test` -> 33 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 eunit` -> 321 tests, 0 failures
- `rebar3 check` -> passed

Known warning remains the existing non-fatal `observer_cli_process:render_state/3` timeout warning during full EUnit.
