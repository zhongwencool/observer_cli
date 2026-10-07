# task-first CLI design contract

The redesign starts from baseline `9682931`: users should choose an investigation,
not learn the collector's internal modes. This document explains implementation
choices; [CLI reference](../reference/cli.md) owns invocation rules and bounds,
[core concepts](core-concepts.md) owns interpretation and trust, and the
[migration guide](../guides/migrate-2-1.md) owns v2 replacements.

## One workflow for people and agents

`check -> inspect -> explicit escalation` reduces uncertainty without pretending
to identify a root cause. People and agents share command and metric meanings;
concise text and complete JSON are projections of the same evidence.

The five entrypoints are `check`, `inspect`, `trace`, `tui`, and offline `describe`.
Inventory and detail share a resource path, using explicit typed selectors.
There is no saved active target: each invocation resolves an atomic explicit
selector or its process environment. Legacy `context.etf` is untouched.

## Reuse collection, separate responsibilities

- The public registry owns paths, options, constraints, examples and risk;
  parsing, help and discovery consume it.
- Target resolution, lifecycle execution, result projection and presentation
  are separate. Existing collectors, response validation and cleanup are reused.
- Checks share one target worker and sampling plan, not multiple full diagnostic
  runs. The default is 15 seconds / five samples; explicit deep memory uses seven.
- Current values, signed changes, counters and rates retain fixed meanings.
  Actual monotonic sampling intervals exclude unrelated scan time.
- Budget admission is checked per sample and during scans. Row limits do not
  replace scan limits; legacy enumeration APIs may still allocate an ID list
  before its post-enumeration cap is checked.

The public response is `observer_cli.cli/v2`; the reused private record is
`observer_cli.capture/v1`, not a public v1 compatibility executor. Bundle `2.1.0`
and protocol `2` must match between controller and target. Generated public
schema remains standalone so consumers need no private-file resolver.

## Non-negotiable boundaries

- Execution completeness and findings are independent. Supported findings survive
  partial capture; evidence pointers resolve against the returned measurements.
- Missing, warming and reset are not zero. Uncalibrated trends never become
  leak, process CPU-time or network-health conclusions.
- Credentials are never printed. Real selectors enable local follow-up;
  redacted selectors are null and cannot be executed.
- Next actions use closed argument arrays and originating-target binding,
  never log-derived instructions, `eval` or automatic consent.
- Default checks do not collect business contents, logs, state or trace events.
  State/trace consent and opt-in same-OTP TUI loading remain explicit.
- At 80 columns, root help fits within 40 lines and the default report within
  24 lines. Requested inventory rows are not dropped to meet this budget.
- Subcommand help stays within 80 columns, with required and command-specific
  options before target and output options. It shows positional arguments,
  applicable defaults and constraints, and shell-quoted examples. Its length
  is not limited to the root help budget; safety requirements are never hidden
  to fit a screen. JSON examples remain argument arrays, not shell strings.

## Acceptance and deferred scope

Verify help-led overview/detail, all four focused checks, JSON-only typed
follow-up, pre-connection rejection, partial findings, lifecycle/reset cases,
scan refusal, cleanup uncertainty and untrusted log rendering. Use temporary
HOME/configuration, owned EPMD and disposable nodes; never production targets.

```sh
mise exec -- rebar3 as ci check
mise exec -- rebar3 as test eunit
mise exec -- rebar3 escriptize
mise exec -- python3 scripts/generate-cli-schema.py --check
mise exec -- python3 scripts/cli-agent-smoke.py --require-json
```

CI also validates Rebar/Mix packaging, producer fixtures and OTP 26–29. Automated
acceptance and a terminal walkthrough are not a first-time-user study or proof
of zero defects. Builds and tests do not authorize publication or deployment.

Deferred: TUI page redesign, named targets, daemon/MCP, automatic investigation,
aggregate concurrency limiting, new root-cause rules and business-content collection.
