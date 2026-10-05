# Migrate from 2.0 to 2.1

2.1.0 is unreleased. Keep a versioned 2.0 binary until runbooks migrate. 2.1 returns
replacement hints for old commands; it has no public compatibility executor.
Despite the minor package version, this release changes CLI commands, identifier
defaults and exit behavior. Existing runbooks need the migration below.

## Replace paths and remove pretend sessions

| 2.0 | 2.1 |
| --- | --- |
| `connect`, `status`, `disconnect` | Explicit target options or shell environment; start with `check`. |
| `diagnose [--observe 10s]` | `check [--window 10s]`; bare check defaults to 15s. |
| `diagnose --observe 10s --deep` | `check memory --window 10s --deep`. |
| `snapshot [--deep]` | `inspect vm [--deep]`. |
| `processes`, `process PID_OR_NAME` | `inspect process`, optionally `--pid PID` or `--name NAME`. |
| `ports`, `port PORT_ID` | `inspect port`, optionally `--id PORT_ID`. |
| `applications`, `sockets` | `inspect application`, `inspect socket`. |
| `schedulers --duration 2s` | `inspect scheduler --window 2s`. |
| `memory`, `distribution`, `network`, `ets`, `mnesia`, `logs` | Corresponding `inspect RESOURCE`. |
| `otp-state` | `inspect state` with selector, behavior and `--allow-state-read`. |
| `supervision-tree` | `inspect supervision --app APP`. |
| `tui NODE [COOKIE REFRESH_MS]` | Protected target options and optional `--interval`; loading requires `--load-code`. |

Legacy `context.etf` files are untouched. Use `OBSERVER_CLI_NODE` plus exactly
one cookie environment/file source, or explicit target options on every agent
call. Explicit selectors cannot borrow environment components. See
[target selection](../reference/cli.md#invocation-and-targets) for details.

## Make sampling intent explicit

A window no longer changes a base sort into a delta:

```sh
observer_cli inspect process --sort memory --window 2s       # current bytes
observer_cli inspect process --sort memory-change --window 2s
observer_cli inspect process --sort reductions-rate --window 2s
```

Change/rate sorts require a window; base counters retain lifetime meaning.
New identities can have current values without a delta baseline. Gauge decreases
are valid; decreasing cumulative counters are resets.

## Update response consumers and deployment

- Schema is `observer_cli.cli/v2`. Add `summary`, `assessment`, `next_actions` to
  the existing six envelope fields. Findings move to `assessment.findings`;
  suggestions move to top-level `next_actions`. Command identity is the public path.
- Follow typed `{kind, value}` PID/port selectors. Null or response-local aliases
  cannot address a target; do not reinterpret `pid-1` as a registered name.
- Complete checks exit `0` even with findings. Use `--fail-on warning|critical`
  only for an intentional finding threshold. Execution failures take precedence;
  see [exit codes](../reference/cli.md#output-and-exits).
- Identifiers are included by default. Remove `--include-identifiers`; add
  `--redact` when sharing and review destinations. Logs are not reliably redacted.
- Both controller and target need bundle `2.1.0`, protocol `2`; protocol alone
  is insufficient. Commands never upload code. TUI loading is explicit and
  same-OTP only; no automatic consent or incident action is introduced.
