# Goal 17 width helper boundary

Issue #133 Phase 2 item 17: move width-related helpers behind a single boundary: base width, extra width, weighted distribution, and anti-auto-wrap line padding.

Changed files:

- `src/observer_cli_lib.erl`
- `src/observer_cli.erl`
- `src/observer_cli_plugin.erl`
- `src/observer_cli_process.erl`
- `src/observer_cli_port.erl`
- focused width/helper tests

Result:

- Added `observer_cli_lib:layout_base_width/0` plus `layout_extra_width/1,2` so base-width and extra-width calculations route through the shared rendering helper boundary.
- Kept existing `layout_width/0`, `layout_extra_width/0`, `weighted_widths/2`, and `pad_rendered/1` behavior while moving remaining `?COLUMN + 5` / `?COLUMN + 6` callers to the boundary helpers.
- Preserved the anti-auto-wrap width rule (`io:columns() - 1` for wide terminals) and existing line padding behavior.
- No prerequisite checklist item was touched.

Validation:

- `rebar3 fmt` -> passed
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_core_test,observer_cli_process_test,observer_cli_port_test,observer_cli_plugin_render_test,observer_cli_golden_test` -> 151 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 check` -> passed
- `rebar3 eunit` -> 328 tests, 0 failures

Notes:

- The known non-fatal `observer_cli_process:render_state/3` timeout warning still appears during EUnit.
- Full manual terminal QA was not rerun because this slice only moved width calculations behind existing helper functions without changing rendered widths.
