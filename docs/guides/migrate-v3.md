# Migrate from v2 to v3

Version 3.0.0 is unreleased. Keep a versioned 2.0.0 binary for existing runbooks
until their commands and response handling have been migrated. v3 has no v1
public compatibility executor and never silently translates old paths.

## Replace paths and remove pretend sessions

| v2 | v3 |
| --- | --- |
| `connect`, `status`, `disconnect` | Explicit target options or current shell environment; start with `check`. |
| `diagnose` | `check [cpu\|memory\|mailbox\|connections]`. Default window is 15s. |
| `diagnose --observe 10s` | `check --window 10s`. |
| `diagnose --observe 10s --deep` | `check memory --window 10s --deep`. |
| `snapshot [--deep]` | `inspect vm [--deep]`. |
| `processes` | `inspect process`. |
| `process PID_OR_NAME` | `inspect process --pid PID` or `--name NAME`. |
| `ports`, `port PORT_ID` | `inspect port`, optionally `--id PORT_ID`. |
| `applications`, `sockets` | `inspect application`, `inspect socket`. |
| `schedulers --duration 2s` | `inspect scheduler --window 2s`. |
| `memory`, `distribution`, `network`, `ets`, `mnesia`, `logs` | Corresponding `inspect RESOURCE`. |
| `otp-state` | `inspect state` with selector, behavior and `--allow-state-read`. |
| `supervision-tree` | `inspect supervision --app APP`. |
| `tui NODE [COOKIE REFRESH_MS]` | `tui` with protected target options, optional `--interval`; loading requires `--load-code`. |

v3 never reads, modifies or removes old `context.etf` files. There is no active
profile or connection to disconnect. Use `OBSERVER_CLI_NODE` plus exactly one
of `OBSERVER_CLI_COOKIE` or `OBSERVER_CLI_COOKIE_FILE` for a human shell, or
explicit `--node` and one cookie-source option on every agent invocation.
Explicit selectors cannot inherit environment components. Options may appear
before or after the path.

## Fix sampling intent, not just spelling

Adding `--window` no longer converts a base metric into a delta:

```sh
observer_cli inspect process --sort memory --window 2s       # current bytes
observer_cli inspect process --sort memory-change --window 2s
observer_cli inspect process --sort reductions-rate --window 2s
```

Change/rate sorts require the window. Base counters retain current/lifetime
meaning; sampled fields use `*_delta` and `*_per_second`. Current rankings can
include new identities with no delta baseline. Gauge decreases are valid;
cumulative counter decreases remain resets.

## Migrate response consumers

The public schema is now `observer_cli.cli/v2`. The existing six envelope fields
remain, with common `summary`, `assessment` and `next_actions` added. Findings
move from command-specific `data.findings` to `assessment.findings`. Suggestions
move to top-level `next_actions`. `data` retains measurements and `meta` retains
coverage. Public command identity is the readable path, such as `inspect process`.

Inventory/detail PID and port selectors are typed `{kind, value}`. They are null
when redacted or unavailable. Do not reinterpret `pid-1` as a registered name or
as a persistent handle. Use an explicit `--name` only for a genuine known
registration, not an alias lookup.

Execution and findings are separate: complete checks now exit 0 even with
findings. Add `--fail-on warning|critical` only to a runbook that intentionally
uses exit 1 for findings. Exits 2–4 retain invocation/capability, runtime/partial,
and internal/schema/cleanup categories and take precedence over finding policy.

Identifiers are included by default; remove `--include-identifiers` and add
`--redact` explicitly when sharing. Review report destinations during migration.
Log text remains a sensitive-content exception, not reliably redacted metadata.

## Deploy matching code deliberately

Controller and target bundle identity must both be `3.0.0`, target protocol `2`.
Do not infer compatibility from protocol alone. CLI calls never upload missing
code. TUI loading is opt-in and limited to the same OTP major. There is no
fallback to v2 dispatch, persisted context, arbitrary evaluation, automatic
consent or automatic incident action.
