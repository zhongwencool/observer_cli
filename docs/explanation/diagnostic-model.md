# Diagnostic model

`observer_cli diagnose` separates collection from interpretation. The target
records what it sampled, which probes succeeded, and what it could not establish.
Versioned rules then produce findings only when their required evidence is
complete.

This distinction matters because a failed or refused probe is not evidence that a
node is healthy.

## A report has evidence layers

Every diagnostic report uses the `observer_cli.cli/v1` envelope. Inside it:

- `capture` describes timing, probes, coverage, and known observer effects;
- `data.findings` contains calibrated warning or critical conclusions;
- `data.suspects` is reserved for weaker diagnostic hypotheses;
- `data.context` contains measurements and trends that help an operator inspect
  the node without claiming a root cause;
- `data.skipped` records checks that were unavailable, refused, unrequested, or
  not calibrated; and
- `warnings` and `errors` describe degraded collection.

Evidence in a finding points back to a path in the same response. This lets a
human or program inspect the measurement behind the conclusion instead of
accepting an unexplained label.

## Quick diagnostics use two samples

Running `observer_cli diagnose` without mode options takes two samples roughly
1.5 seconds apart. The required rule checks process, port, atom, and ETS counts
against their VM limits. It emits:

- a warning above 85 percent; and
- a critical finding at or above 95 percent.

The quick report also carries bounded current context for processes, ETS tables,
ports, distribution, and scheduler activity when those probes are available.
Current context is not automatically promoted to a finding.

Growth checks such as mailbox backlog, memory growth, ETS growth, and port-queue
growth remain listed as skipped when the ruleset is not calibrated for a sound
conclusion. The report states that boundary instead of inventing a diagnosis from
two points.

## Observation modes use planned samples

`--observe DURATION` accepts 5 to 60 seconds and collects five planned samples.
It enables scheduler wall-time measurement for the capture and turns it off in
an `after` path. The current implementation does not restore a setting that was
already enabled by another tool.

Observation adds global memory and stable-resource trends. Resource identity is
tracked across samples so that newly created, terminated, or replaced resources
are not mistaken for growth in one stable resource.

`--observe DURATION --deep` uses seven samples and requests an independently
admitted final binary-holder ranking. Deep mode does more work; it does not relax
the scan budgets.

`--observe DURATION --app APP` adds application-scoped samples and child identity
context. Ambiguous, duplicate, or unsafe child identifiers are reported as
unavailable rather than guessed.

## Required and optional coverage differ

A probe entry records whether it is required, how many samples succeeded, and the
facts it covers. If required coverage is incomplete:

- the capture is `partial`;
- findings are suppressed; and
- the command exits with status `3`.

Optional failures can also make a result partial, but a missing optional probe is
not rewritten as a healthy measurement. Scan admission refusals, sampling gaps,
capability limits, and target errors remain visible through reason codes.

This is why the following outcomes mean different things:

| Outcome | Meaning |
| --- | --- |
| Complete, no findings | All required checks ran and none crossed a calibrated threshold |
| Complete, findings | Required checks ran and at least one calibrated threshold was crossed |
| Partial | At least one required or started probe could not establish its promised coverage |
| Skipped check | The check was unrequested, unavailable, refused, or intentionally not calibrated |

"No findings" is narrower than "the node is healthy." It describes only the
rules and coverage named in that report.

## Findings affect exit status

A complete diagnosis with no findings exits `0`. A complete diagnosis with one or
more findings exits `1`. Argument or direct capability failures exit `2`, partial
or refused runtime outcomes exit `3`, and internal/schema/cleanup failures exit
`4`.

Automation should read both the exit status and a machine-readable envelope. It
should not infer health by searching human text.

## Redaction is stable within one response

Diagnosis and snapshot commands redact identifiers by default. A target node,
PID, registered name, application, table, port, socket, or MFA becomes a typed
stable identifier such as `node-1` or `pid-2`. Repeated appearances of the same
identifier within the response remain correlatable without exposing its raw
value.

Pass `--include-identifiers` only when the destination of the report is allowed to
receive the original identifiers. Redaction limits disclosure; it does not remove
all operational sensitivity from counts, timings, topology, or findings.

## Observer effects are part of the evidence

The diagnostics worker, distribution controller, module state, scans, and
scheduler measurement can change facts while they are being measured. The report
records known effects and uses contaminated-count field names where appropriate.

Sampling is therefore evidence from a bounded observation window, not an atomic
snapshot of the entire VM. See [Safety and observer effect](safety-and-observer-effect.md)
for the operational consequences.
