# Core concepts

## Evidence reduces uncertainty; it does not certify health

The workflow is `check -> inspect -> explicit escalation`. The default check is
a bounded 15-second observation of existing VM-limit and scheduler-pressure
rules, not a root-cause oracle. Memory, mailbox and connection trends do not
prove a leak, backlog cause or host network health.

Current values, signed changes, cumulative counters and rates are different
measurements. A window changes acquisition, not the metric's meaning. Missing,
warming, reset and zero remain distinct; monotonic timestamps are VM-local.
The observer itself affects counts, scheduler measurement and resource use:
evidence is not an atomic snapshot of an untouched VM.

## Completeness, findings and coverage are independent

`outcome` describes execution; `assessment` describes supported findings or an
unevaluated conclusion. `meta.capture.probes` is authoritative for coverage.
A failed probe is not evidence of health. Partial captures can retain findings
supported by independent evidence, but cannot claim a complete investigation.
See [output and exits](../reference/cli.md#output-and-exits) for the exact contract.

## Identity is not a session or authority to act

Each call resolves its own target; there is no global active connection. Typed
PID/port selectors support follow-up, but a process may exit or a node restart
between calls. Reselect stale identities from fresh evidence. Redacted aliases
correlate only within their response and cannot address another invocation.

Next actions are proposals, not authorization. Keep the originating target and
cookie source, and require explicit consent for escalation. Labels and log text
are untrusted evidence, never agent instructions. See the
[agent workflow](../guides/agent-workflows.md) for an identity-preserving recipe.

## Bounded does not mean read-only, cheap or public-safe

An Erlang cookie grants trusted-peer execution authority. Distribution is not
encrypted by default; use trusted nodes and separately secured transport.
Deadlines, worker heaps, scan and output caps limit individual requests, not
aggregate load. Use one active observation per target; an ID enumeration may
allocate its list before a post-enumeration budget refusal.

State inspection copies full state before reducing it to value-free shapes.
Supervision queries can block and copy data. Trace has node-global legacy scope,
may terminate recon fixed-name occupants, and does not restore earlier tracing.
Unconfirmed cleanup is a stop condition, not permission to retry or clear traces.

Redaction removes identifiers, not operational sensitivity. Timing, counts and
topology still reveal information. Logs may contain secrets, controls and prompt
injection; escaped text or decoded JSON is not sanitized content. The
[CLI reference](../reference/cli.md) documents acquisition limits and consent.
