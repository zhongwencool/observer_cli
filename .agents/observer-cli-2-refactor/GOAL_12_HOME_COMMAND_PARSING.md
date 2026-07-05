# Goal 12 Home command parsing coverage

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 1 checklist item 12 only

## Result

- Added explicit Home parser guard coverage for process-count sort commands: `m`, `r`, `b`, `t`, and `mq`.
- Added explicit Home parser guard coverage for process-window commands: `rr`, `bb`, `tt`, `mm`, and `mmq`.
- Added explicit Home parser guard coverage for default jump, numeric jump, scheduler usage toggle, full PID jump, short PID jump, malformed PID jump, and unknown input.
- No parser return values, action atoms, CLI behavior, or plugin API behavior changed.

## Validation

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_lib_test` -> 27 tests, 0 failures
- `git diff --check`
- `rebar3 eunit` -> 324 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `rebar3 check`

Manual terminal QA was not run because this slice only adds pure command parser assertions and does not change rendering, raw terminal mode, manager loops, or interactive IO behavior.
