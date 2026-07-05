# Goal 19: Home summary rendering refactor

Changed files:

- `src/observer_cli.erl`

Result:

- Refactored Home System and memory/process summary rendering so each path first builds stable row/cell block data, then sends those blocks through a shared Home summary renderer.
- Preserved the existing rendered grouping, widths, ANSI styles, warning-color cells, and unsupported atom-count fallback text.
- Did not touch Home top-n rendering; that remains for goal 20.

Validation:

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_golden_test` -> 31 tests, 0 failures
- `rebar3 check` -> passed
- `git diff --check` -> passed
- `rebar3 eunit` -> 329 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning still appeared
- `rebar3 as ci compile` -> passed
