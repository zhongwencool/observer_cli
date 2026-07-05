# Goal 15 command branch cleanup

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 1 checklist item 15 only

## Result

- Deleted duplicated command parser branches without changing returned actions:
  - shared `q`/`Q` quit aliases now share one guarded branch;
  - shared page backward aliases `pu`/`PU`/`B` now share one guarded branch;
  - shared page forward aliases `pd`/`PD`/`F` now share one guarded branch;
  - shared PID jump prefixes `<` and `>` now share one guarded branch;
  - Process and Port `q`/`Q` quit aliases now share one guarded branch.
- Kept all command atoms, tuple shapes, parser fallbacks, manager behavior, rendering, plugin behavior, and user-visible CLI commands unchanged.

## Validation

- Before deletion: `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_process_test,observer_cli_port_test` -> 113 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- After deletion: `rebar3 fmt`.
- After deletion: `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_process_test,observer_cli_port_test` -> 113 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- After deletion: `git diff --check`.
- Final broad validation: `rebar3 eunit` -> 328 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- Final broad validation: `rebar3 check`.

Manual terminal QA was not run because this slice only removes duplicate parser clauses while preserving the same parsed actions covered by the focused parser tests; no rendering, raw terminal mode, IO loop, or manager behavior changed.
