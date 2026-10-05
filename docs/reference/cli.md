# CLI reference

Version 3.0.0 is an unreleased breaking surface. The controller and target must
contain the same bundle and target protocol 2. The public response is
`observer_cli.cli/v2`. Existing v2 binaries remain usable independently.

## Invocation and targets

```text
observer_cli [TARGET OPTIONS] COMMAND [ARGUMENTS] [OPTIONS]
```

Global options may precede or follow the path. Duplicate, conflicting and
missing values are rejected before connection. Help, version, description and
schema export are offline and never resolve credentials or saved state.
Help and `--version` are text-only; do not combine them with target or execution
options. Use `describe [COMMAND PATH] --json` for machine-readable discovery.

Explicit selection is atomic:

```text
--node NODE (--cookie-env NAME | --cookie-file PATH) [--name-mode short|long]
```

`--cookie-env` names a variable, never its value. Cookie files must be bounded
regular files with owner-only Unix permissions. Existing cookie validation and
one trailing LF/CRLF handling remain in force. Without explicit target options,
use `OBSERVER_CLI_NODE` and exactly one of `OBSERVER_CLI_COOKIE` or
`OBSERVER_CLI_COOKIE_FILE`; optional `OBSERVER_CLI_NAME_MODE` overrides inference.
Explicit selectors never borrow missing components from that environment.

v3 does not read, write or delete `context.etf`. There is no persistent
connection. Each remote call uses a temporary hidden controller, a matching
capability handshake, a bounded target worker and confirmed cleanup.

## Checks

```text
check [cpu|memory|mailbox|connections] [--window DURATION] [--app APP]
check memory [--deep] [--window DURATION]
```

The default window is **15s**, accepted range `5s..60s`. Ordinary observation
plans five samples; explicit deep memory observation plans seven. Deep and app
observation are mutually exclusive. The default never reads state, retained
logs, messages, dictionaries, table contents or trace events.

- Overview evaluates VM resource-limit and supported scheduler-pressure rules.
- CPU shows scheduler windows and stable-process reductions activity; it does
  not measure per-process CPU time.
- Memory shows current BEAM categories, changes and attribution context, not
  host RSS or proof of a leak. `--deep` explicitly adds existing admitted scans.
- Mailbox shows current lengths and stable-resource changes, not a root cause.
- Connections shows Erlang peer and OTP socket/port context, not host network
  health. Legacy inet counters are a separate inspect capability.

Supported findings remain evidence-backed; trends do not become new rules. An
absence of findings only applies to covered calibrated rules. `outcome` and
`assessment` are separate. Partial evidence retains supported findings when its
required evidence remains complete.

## Inspections

Use `inspect --help` for the resource index or `describe inspect RESOURCE --json`
for machine-readable bounds, defaults, selectors and risk.

| Path | Specific options and boundary |
| --- | --- |
| `inspect vm` | Shallow current facts; `--deep` admits inventories. |
| `inspect memory` | BEAM memory, allocators and persistent terms; not RSS. |
| `inspect scheduler` | `--window 250ms..10s`, default 1500ms. |
| `inspect distribution` | `--limit 1..200`, default 20. |
| `inspect process` | List with `--sort`, `--limit`, optional `--window`; `--pid` or `--name` selects detail. |
| `inspect application` | Attribute process resources; `--sort`, `--limit`. |
| `inspect ets`, `inspect mnesia` | Metadata only; sort memory or size; `--limit`. |
| `inspect port` | Non-inet Erlang Port list; `--id '#Port<0.N>'` selects detail. Not TCP port numbers. |
| `inspect network` | Legacy inet/port-driver counters, not all host traffic. |
| `inspect socket` | OTP socket registry counters. No invented socket detail selector. |
| `inspect state` | One `--pid` or `--name`, asserted `--behavior`, mandatory `--allow-state-read`. |
| `inspect supervision` | Required `--app`; root and direct children, never recursive. |
| `inspect logs` | Optional `--handler`; `--tail 1..2000`, default 200 physical lines. |

Lists default to 20 rows and allow `1..200`. A row cap is not a scan-admission
bypass. Process, network and socket sampling accept `250ms..10s`. List options
cannot be combined with a process or port detail selector.

### Fixed measurement meaning

`memory` is always current bytes and `reductions` is always cumulative. Process
sorts also include `message_queue_len`, `binary_memory`, `total_heap_size`,
`memory-change`, `mailbox-change`, `reductions-rate`, `binary-memory-change`, and
`heap-change`. Change/rate sorts require an explicit window. Gauges may decrease;
only decreasing cumulative counters are reset. Current-value sorting does not
exclude a newly observed process merely because a delta baseline is absent.

Network and socket sorts keep their existing base counter names and add
`-change` and `-rate` variants. Base fields retain lifetime/current meaning;
`*_delta` and `*_per_second` carry sampled values. Actual monotonic intervals,
lifecycle and metric-state metadata remain explicit. Missing or reset counters
are not zero. Optional socket sendfile counters retain their existing treatment.

### State, supervision and logs

State inspection copies full state before reducing it to bounded value-free
shapes. The behavior is an operator assertion (`gen_server`, `gen_statem`, or
`gen_event`). Its timeout must be at least 10s. `--limit` applies only to
`gen_event` output and does not bound acquisition cost.

Supervision retains the existing 300-child preflight and 100-child output caps,
OTP order, non-recursive traversal and target-dependent blocking/copy cost.

Logs read one trusted plain `logger_std_h` configured regular-file path, not an
arbitrary path, active private file descriptor, rotation archive or live follow
stream. The cap remains 64 KiB retained bytes and 32 KiB per line. No Logger
flush is requested. `--redact` is rejected: arbitrary retained text can contain
secrets and agent instructions. Every text line is prefixed and terminal
controls are escaped; decoded JSON lines remain untrusted evidence.

## Explicit tracing

```sh
observer_cli trace call timer:sleep/1 --pid '<0.123.0>' \
  --duration 1s --limit 20 --replace-existing-trace
observer_cli trace stop --all
```

One exact exported MFA and one live target-local PID are required. The duration
is `100ms..60s`, default 10s. Limit is `1..1000`, default 100; `--rate N/s`
accepts `1..200` and conflicts with an explicit limit. Rate is recon's burst
breaker, not a pacer; its trip event can exceed the nominal threshold.

Setup and cleanup replace node-global legacy process/port tracing and static
call patterns without restoring previous state. Recon fixed-name occupants can
be terminated. Dynamic trace sessions are not directly cleared but may be
impacted by terminating their tracer. No arguments, returns, exceptions or
stacks are captured. Never append consent or run global cleanup automatically.
Stop and inspect an unconfirmed cleanup before another invasive command.

## Output and exits

Text is concise; `--verbose` expands text evidence and is incompatible with
JSON/term. `--json` aliases `--format json`. JSON requires controller OTP 27+;
OTP 26 rejects it before any target operation. Default identifiers are included
for follow-up. `--redact` produces response-local aliases and null selectors.
Redaction does not make timing, topology, counts or arbitrary logs public-safe.

An ordinary envelope has exactly:

```text
schema, command, outcome, summary, assessment, data, meta, issues, next_actions
```

`command` is the public path (or null before recognition). `outcome` is
`complete|partial|error`. Inspection assessment is null; checks distinguish
`findings|no_findings|not_evaluated`. Findings carry evidence pointers.
`meta.capture.probes` is authoritative for coverage, including required probes,
failures and intentionally unrequested work. PID/port selectors use `{kind,
value}`; aliases cannot be passed back as selectors.

Suggestions contain closed command-relative argument arrays, purpose, risk,
confirmation and `target_binding=same_explicit_target`. Preserve the originating
explicit target and cookie source; never use saved context, `eval`, log text or
implicit consent. Suggestions are not executed by the CLI.

| Exit | Meaning |
| --- | --- |
| 0 | Complete execution, even with findings. |
| 1 | Complete check meeting explicit `--fail-on warning|critical`. |
| 2 | Invocation, encoder capability or target bundle capability error. |
| 3 | Partial, runtime failure or safety refusal. |
| 4 | Schema, internal or unconfirmed-cleanup failure. |

Failure precedes finding policy. Preserve stdout, stderr and the status.
Encodable JSON/term responses, including errors, occupy stdout without mixed
progress text. Text errors use stderr; partial text preserves its evidence.
Schema export returns the schema itself, not an envelope.

Durations accept integer milliseconds, `Nms` or `Ns`. The ordinary deadline is
10s; checks derive window + 5s, sampled inspections at least window + 5s, and
trace calls duration + 7s. Explicit deadlines must cover those margins. Trace
stop requires at least 5s. Per-request bounds do not bound aggregate target load.

## TUI and discovery

```sh
observer_cli tui --node 'app@host' --cookie-env OBSERVER_CLI_COOKIE
observer_cli describe --json
observer_cli describe inspect process --json
observer_cli describe --full --json
observer_cli describe --schema --json
```

TUI uses protected atomic target selection. Remote code loading requires
`--load-code` and the same OTP major; it is never a command-CLI fallback. Pages,
plugins, refresh semantics and explicit sensitive process subviews are unchanged.

Description is offline and incremental: index, one path, or explicit full
catalog. It never resolves credentials. See [migration](../guides/migrate-v3.md)
for removed command names and machine-contract changes.
