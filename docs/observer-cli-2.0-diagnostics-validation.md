# observer_cli 2.0 diagnostics release validation

Date: 2026-07-12
Design: `docs/observer-cli-2.0-diagnostics-design.md`, sections 17-23
Result: **design-reviewed; OTP 29 local gates and generated-escript smoke passed;
cross-version target proof pending**

This document separates commands that were actually run from gates that are only
tracked or configured. CI configuration is not reported as a passing result before
the jobs finish.

## Evidence from the current CLI contract change

The following commands were actually run on the local OTP 29 controller:

```text
rebar3 fmt
rebar3 as test eunit --module=observer_cli_cli_test,observer_cli_snapshot_test,observer_cli_diagnostic_test,observer_cli_trace_test,observer_cli_escriptize_test
rebar3 eunit
rebar3 compile
rebar3 as ci compile
rebar3 lint
rebar3 xref
rebar3 dialyzer
rebar3 ex_doc
rebar3 check
sh -n scripts/escript-smoke.sh
dash -n scripts/escript-smoke.sh
dash scripts/escript-smoke.sh
scripts/escript-smoke.sh
git diff --check
```

All commands passed. The focused suite ran 240 tests and full EUnit ran 649 tests,
both with zero failures. Both smoke executions ran `rebar3 escriptize` and passed all
ten generated-binary cases. The static closeout also removed the existing Elvis and
Dialyzer failures by matching recon's declared ranking payload type and extracting
the nested snapshot helpers without changing returned data. No OTP 26-28 runtime,
controller-target cross-version matrix, disposable 10,000/100,000-resource run, or
Trace benchmark was run for this change.

## Tracked generated-escript smoke

`scripts/escript-smoke.sh` captures stdout and stderr separately and checks these
ten cases against the generated binary:

| Case | Exit | Stdout | Stderr |
| --- | ---: | --- | --- |
| `--help` | 0 | usage | empty |
| no arguments | 0 | usage | empty |
| `-h` | 0 | usage | empty |
| `trace call --help` | 0 | subcommand usage | empty |
| `--version` | 0 | version | empty |
| unknown option | 2 | empty | error |
| bare `target@host` | 2 | empty | unknown command |
| `process --bogus` | 2 | empty | command error |
| `process --bogus --format term` | 2 | error envelope | empty |
| trace call without `--replace-existing-trace` | 2 | empty | safety argument error |

The GitHub workflow is configured to run this script in each OTP 26, 27, 28, and 29
job after `rebar3 check` and before EUnit/coverage. A green job would prove the local
generated-escript contract for that controller OTP only; it would not prove a
controller on one OTP can diagnose a target on another.

## CLI contract under test

### Help, version, streams, and exits

- No arguments, `--help`, `-h`, `help COMMAND`, command help, `tui --help`, and Trace
  subcommand help are local stdout success paths.
- `--version` reports the local observer_cli bundle version, CLI schema, protocol,
  and controller OTP without starting distribution.
- Known malformed commands return exit 2 instead of successful top-level help.
  Text errors use stderr and point to the relevant command or Trace subcommand help.
- Trace errors and successes use stable machine identities `trace_call` and
  `trace_stop_all`; human errors use `trace call` and `trace stop`.
- Term and JSON errors use the `observer_cli.cli/v1` envelope on stdout after encoder
  selection. Bootstrap and encoder failures use stable plain-text stderr.
- Exit codes are fixed: 0 success, 1 complete diagnosis with findings, 2
  argument/format/direct capability, 3 runtime/refusal/partial, and 4
  internal/schema/cleanup.
- Successful diagnostics, inspection, and Trace commands use the shared indented
  text renderer. Context commands retain concise text summaries; no ordinary command
  falls back to a raw Erlang map.

### Context and target capabilities

- `connect` writes node, name mode, and cookie-source metadata only; it never writes
  the cookie or keeps a daemon connection.
- The new context is saved only after the temporary controller stops and cleanup is
  confirmed. Connection, capability, target-OTP, and cleanup failures leave the
  previous context unchanged.
- `connect` and `disconnect` preflight the selected encoder before mutating context.
  Observed capability versions are bounded to printable public values, so invalid
  UTF-8, oversized integers, and extra target fields cannot poison output.
- `disconnect` can remove malformed or oversized protected context contents, but
  continues to refuse unsafe directory permissions, file permissions, symlinks, and
  non-regular paths.
- `connect` and `status` report target OTP, name mode, non-secret cookie-source
  metadata, expected/observed protocol and bundle versions, and one of
  `compatible|missing|incompatible`.
- Reachable missing or incompatible targets return exit 0 with a warning and may be
  selected as context. Commands requiring diagnostics return `capability_unavailable`
  until the matching bundle is installed in the target release.
- `connect --load-diagnostics` is an unknown option. Command-first routes never call
  the legacy remote loader.

### TUI boundary

- `observer_cli tui NODE [COOKIE REFRESH_MS]` is the only interactive escript entry.
- Bare `observer_cli NODE [COOKIE REFRESH_MS]` input is an unknown command with exit 2.
- Only the TUI route retains automatic loading of a missing or incompatible bundle.
  Command-first diagnostics require a target-side installation.
- TUI refresh defaults to 1500 ms and rejects values below 1000 ms. A positional
  cookie remains visible in argv and shell history.

## Tracked unit and scenario fixtures

The repository contains focused fixtures for:

- parser command identity, help routing, removed positional TUI rejection, refresh
  validation, and rejection of `--load-diagnostics`;
- independent Trace stop timeout validation and canonical call/stop error identities;
- compatible, missing, and incompatible capability probes with bounded observed
  version fields;
- hostile capability values and JSON-unavailable mutation preflight;
- connect/status text and envelopes, secret-free cookie-source metadata, and target
  OTP reporting;
- preserving the previous context when cleanup is unconfirmed;
- recovering from malformed protected context contents without weakening path or
  permission checks;
- explicit TUI loading for missing and incompatible bundles;
- shared structured text rendering for every public command;
- term round trips, JSON availability, response caps, redaction, schema validation,
  worker/controller cleanup, diagnostics, and Trace cleanup boundaries.

These fixtures passed in the focused and full EUnit runs recorded above.

## Executed final gates

The local closeout executed:

```text
rebar3 fmt
rebar3 as test eunit --module=observer_cli_cli_test,observer_cli_snapshot_test,observer_cli_diagnostic_test,observer_cli_trace_test,observer_cli_escriptize_test
rebar3 eunit
rebar3 compile
rebar3 as ci compile
rebar3 xref
rebar3 dialyzer
rebar3 check
scripts/escript-smoke.sh
git diff --check
```

CI must also complete its OTP 26-29 jobs. If release notes promise cross-version
controller-target operation, add and run a tracked matrix that starts targets from
their own OTP-specific builds; the current generated-escript smoke does not provide
that evidence.

## Open release proof

- OTP 26-28 generated-escript results depend on CI and are not claimed locally.
- No current tracked cross-version controller-target matrix is present here.
- Large-resource admission and disposable Trace cleanup benchmarks were not rerun for
  this CLI contract change. Historical measurements are intentionally not treated as
  current release evidence.

## Operator boundaries

- Use trusted targets and networks only. Erlang distribution is bidirectional and
  normally unencrypted; the outbound-only controller is not a sandbox.
- New commands require compatible target modules and never use legacy
  `remote_load/1`.
- Snapshot and diagnose do not acquire process messages/dictionaries, table rows,
  application env, cookies, arbitrary state, trace values, or stacks.
- `gen-server-state` copies full state inside a bounded target worker before returning
  only a value-free shape. Supervision inspection is one public application root and
  its direct children.
- Trace is one exact MFA and local PID, call-only and node-global at setup and cleanup.
  There is no provider, daemon, cluster fan-out, eval, auto-fix, trace session
  registry, scoped clear, or command-first remote loader.
