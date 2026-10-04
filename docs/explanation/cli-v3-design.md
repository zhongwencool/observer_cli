# CLI v3 design contract

Observer CLI 3.0.0 replaces the command inventory with a bounded investigation
workflow. The baseline is `9682931`. This document records the implementation
contract, not evidence that a release or a usability study has happened.

## Jobs, not implementation modes

An investigation follows `check -> inspect -> explicit escalation`. People and
agents use the same commands and measurement meanings. Text is a concise
projection; JSON is the complete machine interface. Neither presentation claims
that an absence of calibrated findings proves node health.

The public command families are:

| Family | Purpose |
| --- | --- |
| `check [cpu\|memory\|mailbox\|connections]` | Gather bounded evidence and choose the next observation. |
| `inspect RESOURCE` | Inspect current facts or one explicitly selected resource. |
| `trace call` / `trace stop` | Explicitly authorized node-global tracing. |
| `tui` | Interactive observation, without changing pages or plugins. |
| `describe [COMMAND PATH]` | Offline, incremental capability discovery. |

`inspect` supports `vm`, `memory`, `scheduler`, `distribution`, `network`,
`process`, `application`, `ets`, `mnesia`, `port`, `socket`, `state`,
`supervision`, and `logs`.

Process and port list/detail operations share a path:

```text
inspect process [--pid PID | --name NAME]
inspect port [--id PORT_ID]
inspect state (--pid PID | --name NAME) --behavior BEHAVIOR --allow-state-read
inspect supervision --app APP
```

No selector means a list only where a list capability already exists. A
registered name is never inferred from a redacted PID alias. State acquisition
still copies full state before returning bounded, value-free shapes.

## Observation defaults and cost

The default check window is **15 seconds**, with five planned samples. `--window`
accepts `5s..60s`. Deep memory observation is explicit and uses seven samples.
Application observation and deep observation are mutually exclusive. Deep mode
is available only through `check memory --deep` or `inspect vm --deep`.

The overview uses the existing limit and scheduler-pressure rules. Focused
checks collect and present relevant evidence without inventing memory-leak,
mailbox-root-cause, or network-health rules. They share one target worker,
sampling clock, deadline, and cleanup boundary. They do not start several
complete observations in parallel or chain several independently sleeping
commands.

Checks do not read logs, process state, messages, dictionaries, table contents,
or trace events by default. Missing, warming-up, reset, and unavailable
measurements remain different from zero. Scans retain their admission and heap
budgets. Reducing the output row limit does not bypass scan admission.

## Stateless target selection

Explicit target options are atomic:

```text
--node NODE (--cookie-env NAME | --cookie-file PATH) [--name-mode short|long]
```

Explicit options do not borrow missing selector components from the environment.
Without explicit target options, read `OBSERVER_CLI_NODE` and exactly one of
`OBSERVER_CLI_COOKIE` or `OBSERVER_CLI_COOKIE_FILE`. The former contains a cookie
in the process environment; it is never printed. `OBSERVER_CLI_NAME_MODE` may
override name-mode inference for this environment selector.

No v3 command reads, writes, or deletes a saved `context.etf`. Legacy context
files are left untouched. Agents pass an explicit target on every remote call.
Target options may precede or follow the command path. Duplicate options,
conflicts, missing values, invalid windows, consent failures, and unsupported
output encoders are rejected before target work.

CLI calls never load missing target code. TUI loading requires `--load-code` and
matching controller/target OTP majors. TUI credentials use the same protected
sources as command calls, not positional cookie values.

## Stable measurements

`memory` is always current bytes and `reductions` is always a cumulative BEAM
reduction count. `memory-change` is a signed change; `reductions-rate` is
reductions per second over the actual monotonic sample interval, not process CPU
time. Window options add sampling; they never change a metric's meaning.

Change/rate sorts require an explicit window. Process inspection retains the
current and sampled measurements needed for deterministic sorting. New and
terminated identities or reset counters are not silently treated as stable
resources. Existing per-collector window bounds remain in force.

## Public response and exits

The public schema is `observer_cli.cli/v2`. Every ordinary structured response
has these fields:

```text
schema, command, outcome, summary, assessment, data, meta, issues, next_actions
```

`command` is the public command path, or null before command recognition.
`outcome` is `complete`, `partial`, or `error` and describes execution only.
`assessment` is null for factual inspection; for a check it contains a `status`
(`findings`, `no_findings`, or `not_evaluated`) and a `findings` array. Supported
findings survive optional-probe failures; unsupported conclusions are never
manufactured from incomplete required evidence.

`data` contains measurements; `meta.capture.probes` contains authoritative
coverage and sampling metadata. Finding evidence pointers resolve against this
public response. Process and port rows/details expose a selector of
`{kind, value}` when further inspection is supported. Redacted selectors are
null, never executable aliases.

Next actions contain an ID, purpose, command-relative argument array, risk,
confirmation requirement, and `target_binding=same_explicit_target`. Consumers
retain the originating target and cookie source. Recommendations are derived
from validated evidence and closed command definitions, not target text or log
content. They never execute automatically and never add consent flags.

| Exit | Meaning |
| --- | --- |
| `0` | Complete execution, including a check with findings. |
| `1` | Complete check meeting explicit `--fail-on warning|critical`. |
| `2` | Arguments, output encoding, or target bundle capability error. |
| `3` | Partial, runtime failure, or safety refusal. |
| `4` | Schema, internal, or unconfirmed-cleanup failure. |

Failure exits take precedence over finding policy. Encodable JSON errors remain
one envelope on stdout with no mixed progress output. OTP 26 rejects JSON on
stderr before connecting; text and Erlang terms remain supported.

Identifiers are included by default. `--redact` applies consistently to
shareable metadata reports, selectors, and next actions. It is not secret
removal. Logs reject redaction because arbitrary retained text is sensitive and
untrusted. Trace keeps its specific global-replacement acknowledgement. There
is no general `--yes` flag.

## Presentation and discovery

At 80 columns, root help and the default overview fit within 24 lines. Reports
prioritize target/window, conclusions, evidence, missing coverage, and next
steps. Requested inventory rows are not dropped to satisfy a screen budget.
Verbose text and JSON retain evidence omitted by concise presentation.

One public command registry owns paths, options, bounds, examples, prerequisite
constraints, and risk. Parser, help, discovery, and error hints consume it.
`describe` returns an entry index; a path returns its full descriptor; `--full`
returns the complete catalog. Schema export remains explicitly offline.

## Migration and delivery boundaries

The bundle version is `3.0.0`, target protocol is `2`, and public schema is `v2`.
Controller and target must match. Existing bounded collection primitives and
their private validated capture records are reused; this is not a v1 public
command compatibility executor.

| v2 | v3 |
| --- | --- |
| `connect`, `status`, `disconnect` | Explicit options or shell environment; start with `check`. |
| `diagnose` | `check`, with focus and `--window` as needed. |
| `snapshot` | `inspect vm`. |
| `processes`, `process` | `inspect process`, optionally `--pid` or `--name`. |
| `ports`, `port` | `inspect port`, optionally `--id`. |
| `applications`, `sockets` | `inspect application`, `inspect socket`. |
| `otp-state` | `inspect state` with explicit state-read consent. |
| `supervision-tree` | `inspect supervision`. |
| Other resource commands | The corresponding `inspect RESOURCE` path. |

Old paths fail with a replacement hint; they do not silently execute. Old v2
binaries and selectors can remain installed until their consumers migrate.
Implementation commits separate contract, execution, presentation, and
migration/verification. No publishing, deployment, production access, or
automatic incident action is authorized by this implementation.

Deferred: TUI pages, named profiles, daemons/MCP, automatic workflows, aggregate
target-wide concurrency control, new root-cause rules, and business-payload
collection.

## Acceptance stories

1. With an already prepared target, root help leads to an overview and a process
   detail without a separate setup document.
2. CPU, memory, mailbox, and connection checks distinguish measured evidence
   from uncalibrated or unavailable conclusions.
3. An agent completes check, inventory, and detail using only JSON and the same
   explicit target; redacted selectors stop follow-up.
4. Invalid input, unsupported format, and missing consent do not touch a target
   or real user configuration.
5. Partial captures, lifecycle changes, reset counters, trace cleanup, and
   hostile-looking log text retain their safety and truthfulness contracts.

Acceptance uses temporary HOME/configuration, an owned EPMD, and owned local
nodes. Required checks include formatting, CI compile/lint/xref/Dialyzer,
EUnit, Rebar/Mix packaging, schema fixtures, the agent smoke, OTP 26-29
compatibility, and a terminal walkthrough. Automated stories are not evidence
of an actual first-time-user study.
