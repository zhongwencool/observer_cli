# Phase 0 smoke behavior tests

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 3 only

## Added guardrail

`test/observer_cli_start_test.erl:phase0_smoke_behaviors_test/0` now drives the public `observer_cli:start/1` Home entry with fake terminal input for:

- startup then quit: `q`
- major page switches: `H`, `S`, `A`, `N`, `M`, `E`, `D`, `P`, then quit
- interval input: `2000`, then quit
- pagination input: `F`, `B`, then quit
- process jump input: direct live pid string, then quit from Process Info

The process jump case runs the CLI in an isolated test process with trapped exits so the linked render-worker shutdown does not cancel EUnit.

## Validation

- `rebar3 fmt` passed.
- `rebar3 as test eunit --module=observer_cli_start_test` passed: 12 tests, 0 failures.
- `rebar3 eunit` passed: 312 tests, 0 failures.
- Known non-fatal baseline output: `observer_cli_process:render_state/3` logged the existing timeout warning during full EUnit, but the suite passed.
