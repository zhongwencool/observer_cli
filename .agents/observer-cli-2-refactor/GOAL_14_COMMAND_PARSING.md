# Goal 14 Process / Port / less command parsing coverage

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 1 checklist item 14 only

## Result

- Added explicit Process parser coverage for back, home, process subviews (`P`, `M`, `D`, `C`, `S`), interval, jump, unknown input, and quit.
- Added explicit State/less navigation-map coverage for home, back, quit, and process subview actions.
- Added explicit Port parser coverage for home, network/detail actions, jump, interval, unknown input, and the `{error, _}` quit path.
- Added less-client navigation coverage for real Home (`H`) and Back (`B`) keys while preserving existing page forward/back and quit behavior tests.
- Fixed Port parsing so terminal input errors return `quit` instead of falling through to numeric parsing.

## Validation

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_process_test,observer_cli_port_test,less_client_test` -> 100 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `git diff --check`
- `rebar3 eunit` -> 328 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `rebar3 check`

Manual terminal QA was not run because this slice only covers pure command parsing / less key dispatch and changes the Port input-error path to the same quit behavior already used by Process/shared parsing; no rendering, raw terminal mode, or manager loop behavior was otherwise changed.
