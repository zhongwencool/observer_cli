# Phase 0 golden output test base

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 4 only

## Added guardrail

`observer_cli_test_io` now provides a small golden-output base for later page-specific tests:

- `assert_stable_fragments/2` strips ANSI and asserts caller-chosen stable text fragments only.
- `assert_ansi_boundaries/1` rejects malformed/unstripped escape boundaries, missing SGR reset, and the stale `\e[0m |` reset-before-border artifact.

`test/observer_cli_golden_test.erl` self-tests the base with deterministic rendered output and negative ANSI-boundary cases. It intentionally does not add Home/System/Process/Port golden coverage; those are separate checklist items.

## Validation

- `rebar3 fmt` passed.
- `rebar3 as test eunit --module=observer_cli_golden_test` passed: 2 tests, 0 failures.
- `rebar3 eunit` passed: 314 tests, 0 failures.
- `git diff --check` passed.
- `rebar3 check` passed.
- Known non-fatal baseline output remains: `observer_cli_process:render_state/3` logged the existing timeout warning during full EUnit.

## Skipped

- Manual terminal QA was not run because this slice only adds deterministic test helpers and a self-test; no runtime CLI rendering behavior changed.
