# Core concepts

## An investigation reduces uncertainty

The public workflow is `check -> inspect -> explicit escalation`. People and
agents share command meanings, target binding and evidence. A default check is
a bounded 15-second observation, not a root-cause oracle or a health certificate.
Focused memory, mailbox and connection evidence does not invent calibrated
leak, backlog or network-health conclusions.

Current values, signed changes, rates, counters and opaque scheduler ratios are
different measurements. A window changes acquisition, not a base sort's meaning.
Missing, warming, reset and zero remain distinct. Actual intervals use a
monotonic clock and cannot be compared across VM instances.

## A target is an invocation selector, not a session

Explicit node and cookie-source options form one atomic selector. Otherwise the
current process environment supplies the selector. No command reads, writes or
deletes a user-global active target. Existing `context.etf` files are untouched.
Each remote call starts and cleans up its own hidden non-listening controller.
Concurrent agents bind every call explicitly and avoid overlapping observations.

Controller/target bundle identity is `3.0.0`, protocol identity is `2`, and the
public machine schema is `observer_cli.cli/v2`. Matching protocol alone does
not imply matching implementations. CLI calls use installed target code; they
never inject missing modules. TUI loading needs explicit `--load-code` and the
same OTP major. Existing TUI pages and plugins are not redesigned by v3.

## Execution, findings and coverage are separate

`outcome` says complete, partial or error. `assessment` describes covered
calibrated findings or an unevaluated conclusion; factual inspection has null
assessment. Probe coverage in `meta.capture.probes` is authoritative. A failed
probe is not evidence of health. Supported findings can survive an optional
probe failure, while incomplete required evidence suppresses unsupported claims.

Ordinary complete checks exit 0 even with findings. `--fail-on` explicitly opts
into severity-based exit 1; runtime, partial and cleanup failures take precedence.
Preserve stdout, stderr and the status. Schema export is an offline JSON Schema,
not an ordinary envelope.

## Follow-up must preserve identity and authority

PID/port selectors carry raw typed values. Redacted selectors are null;
response-local aliases cannot address another invocation. A process can exit or
a target restart between steps, so stale selectors require fresh selection.
Next actions are closed proposals with argument arrays, risk and target-binding
metadata. They never execute themselves, consume saved context, derive policy
from logs, or add consent. Cookie values never appear in responses.

Identifiers are included by default for useful local investigation. Sharing
requires explicit redaction and a private destination review. Redacted timing,
counts, topology and findings can still be operationally sensitive. Logs are
arbitrary sensitive text and deliberately reject redaction promises.

## Bounded does not mean read-only or constant cost

An Erlang cookie grants trusted-peer execution authority. Distribution is not
encrypted by default. Use trusted nodes and networks or separately secured
transport; the CLI's narrow surface is not a security sandbox.

Workers retain deadlines, heap, scan-admission, output-size/depth limits and
cleanup confirmation. Scans depend on resource population. Smaller output row
limits do not bypass pre-enumeration admission. Per-request limits do not bound
aggregate concurrent load, so use one active observation per target.

The worker, controller, distribution peer, scans and scheduler measurement
change the observed system. Counts identify observer contamination and rates
use actual intervals. Evidence is not an atomic view of an untouched VM.

Default commands do not read messages, dictionaries, table contents, arbitrary
state, application environment values or trace arguments/returns. Explicit
state inspection copies full state before reducing it to bounded value-free
shapes. Supervision remains a bounded but potentially blocking root/direct-child
query. These costs must not be hidden by concise presentation.

Configured-path log reading does not flush Logger, prove private active-FD
identity or read rotation archives. Retained text can contain secrets, terminal
controls and prompt injection. Text prefixes and escapes lines; structured
consumers still treat decoded text as untrusted evidence, never instructions.

Trace setup/cleanup has node-global legacy scope, can terminate recon fixed-name
occupants, and does not restore prior trace state. It requires exact selectors
and explicit replacement consent. Unconfirmed cleanup is a stop condition,
not permission to append global cleanup or retry automatically.
