# Goal 13 Network / ETS / Mnesia / Application command parsing coverage

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 1 checklist item 13 only

## Result

- Added explicit shared-parser coverage for Network mode/type sorting commands: `ic`, `iw`, `rc`, `ro`, `sc`, `so`, `cnt`, and `oct`.
- Added explicit shared-parser coverage for ETS/Mnesia sorting and Mnesia hidden-system-table toggle: `s`, `m`, and `hide`.
- Added explicit shared-parser coverage for Application sorting commands: `p`, `r`, `m`, and `mq`.
- Added explicit shared-parser coverage shared by these pages for pagination (`pd`, `pu`, `PD`, `PU`, `F`, `B`), interval input, quit (`q`, `Q`), and the stale/terminated input quit path.
- No parser return values, manager behavior, rendering, CLI behavior, or plugin API behavior changed.

## Validation

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_lib_test` -> 30 tests, 0 failures
- `git diff --check`
- `rebar3 eunit` -> 327 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `rebar3 check`

Manual terminal QA was not run because this slice only adds pure shared command parser assertions and does not change rendering, raw terminal mode, manager loops, or interactive IO behavior.
