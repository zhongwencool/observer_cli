# Goal 35: small lifecycle helpers only

Checklist item: Extract only small, stable helpers such as worker cleanup, timer refresh, and page-state updates. Do not create a generic view framework.

## Scope

This slice only touched the repeated page lower-bound update in manager loops. Worker cleanup and timer refresh already use the existing small helpers `observer_cli_lib:exit_processes/1`, `observer_cli_lib:next_redraw/2`, and `observer_cli_lib:flush_redraw_timer/1`, so no broader manager abstraction was added.

## Changes

- Added `observer_cli_lib:next_page/2` as the shared page-state helper for `max(CurPage + Delta, 1)`.
- Replaced repeated page up/down math in Home, Application, ETS, Mnesia, Network, and Plugin managers with `next_page/2`.
- Added `observer_cli_lib_test:next_page_test/0` for increment and lower-bound behavior.

## Non-goals kept

- No generic view framework.
- No worker ownership or cleanup behavior changes.
- No interval/timer behavior changes.
- No user-visible CLI behavior changes.

## Validation

- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_plugin_test` -> 58 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 eunit` -> 346 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 as ci compile` -> passed.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves page arithmetic behind a tiny helper and leaves rendering, timer cadence, cleanup semantics, and command actions unchanged.
