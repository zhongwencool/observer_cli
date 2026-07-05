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

## 2026-07-06 Home golden output tests

Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 5 only

Added `home_golden_output_fragments_test/0` for stable Home rendering fragments:

- menu labels and the default `recon:proc_count(memory, 10) Interval:1500ms` prompt;
- Home footer commands, including quit, pause, and page navigation;
- the default top-n title (`No | Pid`, `Memory`, `Name|>Label|>Initial Call`, `Current Function`);
- wide-layout Home summary fragments, including `Count/Limit`, `Reds(Total/SinceLastCall)`, `Port Parallelism (+spp)`, and `IO/GC:(1500ms)`.

Direct prerequisite kept minimal: the shared border-padding helper now pads before a trailing reset/border suffix, so the Home golden test can enforce the existing ANSI-boundary guard without leaving the stale `\e[0m |` artifact.

Validation:

- `rebar3 fmt` passed.
- `rebar3 as test eunit --module=observer_cli_golden_test` passed: 3 tests, 0 failures.
- `rebar3 as test eunit --module=observer_cli_golden_test,observer_cli_core_test,observer_cli_lib_test` passed: 56 tests, 0 failures.
- `git diff --check` passed.
- `rebar3 eunit` passed: 315 tests, 0 failures.
- `rebar3 check` passed.
- Known non-fatal baseline output remains: `observer_cli_process:render_state/3` logged the existing timeout warning during full EUnit.

Skipped:

- Manual terminal QA was not run because this slice adds deterministic golden output coverage and only adjusts reset/border padding to remove a stale redraw artifact; no navigation or runtime collection path changed.

## 2026-07-06 System golden output tests

Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 6 only

Added `system_golden_output_fragments_test/0` for stable System rendering fragments:

- selected System menu state and refresh interval text;
- System/Architecture, CPU, Memory, Statistics, and compiled-for section grouping;
- allocator block titles and a stable allocator row;
- cache-hit grouping fragments for the four wide Hits/Calls groups;
- wide-layout assertions that System value columns, allocator value columns, and cache Hits/Calls columns grow between base and wide terminal widths.

Validation:

- `rebar3 fmt` passed.
- `rebar3 as test eunit --module=observer_cli_system_test` passed: 24 tests, 0 failures.
- `rebar3 as test eunit --module=observer_cli_golden_test,observer_cli_system_test` passed: 27 tests, 0 failures.
- `git diff --check` passed.
- `rebar3 eunit` passed: 316 tests, 0 failures.
- `rebar3 check` passed.
- Known non-fatal baseline output remains: `observer_cli_process:render_state/3` logged the existing timeout warning during full EUnit.

Skipped:

- Manual terminal QA was not run because this slice adds deterministic System golden output coverage only; no runtime collection, navigation, or rendering behavior changed.
