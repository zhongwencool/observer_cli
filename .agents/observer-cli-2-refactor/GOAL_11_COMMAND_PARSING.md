# Goal 11 command parsing boundary

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 1 checklist item 11 only

## Result

- Moved the shared pure line-command parser from `observer_cli_lib` into `observer_cli_command:parse_shared/1`.
- Moved the shared interval/jump/input fallback into `observer_cli_command:parse_integer/1`.
- Left view-specific parsers in their owning modules: `observer_cli_process`, `observer_cli_port`, and `observer_cli_plugin` still map their own local commands before using the shared numeric fallback.
- Kept `observer_cli_lib:parse_cmd/3` as the IO/routing boundary and `observer_cli_lib:parse_integer/1` as a compatibility delegate.
- No action atoms, tuple shapes, parser return values, plugin API, or user-visible CLI command behavior changed.

## Validation

- `rebar3 fmt`
- `git diff --check`
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_process_test,observer_cli_port_test,observer_cli_plugin_test` -> 134 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `rebar3 compile`
- `rebar3 as ci compile`
- `rebar3 eunit` -> 322 tests, 0 failures; existing non-fatal `observer_cli_process:render_state/3` timeout warning observed.
- `rebar3 check`

Manual terminal QA was not run because this slice only moves pure line parsing and keeps the same parser return values; no rendering, raw terminal mode, or interactive IO behavior changed.
