# Automate diagnostic capture

Use structured Observer CLI output and exit statuses to collect bounded reports
from scripts, schedulers, or incident tooling.

## Choose an output format

- `text` is for people and is the default.
- `term` is an Erlang term containing the complete command envelope.
- `json` contains the same envelope and requires an OTP 27 or newer controller.
- `--json` is an alias for `--format json`.

Use `term` or `json` for automation. Do not scrape the human text format.

## Run without user-specific saved context

Pass the target and cookie source explicitly when a job must not depend on a
login user's context file:

```sh
export OBSERVER_NODE='app@host'
: "${OBSERVER_COOKIE:?populate this variable with your secret manager}"

observer_cli snapshot --deep --format term \
  --node "$OBSERVER_NODE" --cookie-env OBSERVER_COOKIE > snapshot.term
```

Exactly one of `--cookie-env` and `--cookie-file` is required with `--node`.
The cookie value is read at execution time and is not written into the output.

## Preserve non-zero diagnostic reports

`diagnose` uses exit status `1` when a complete report contains warning or
critical findings. Treat that as a report outcome, not a command crash.

This POSIX shell script preserves stdout, stderr, and the exit status:

```sh
#!/bin/sh
set -u

bin=${OBSERVER_CLI_BIN:-observer_cli}
stamp=$(date -u +%Y%m%dT%H%M%SZ)
report="observer-cli-$stamp.term"
errors="observer-cli-$stamp.stderr"

if "$bin" diagnose \
    --observe 30s \
    --deep \
    --timeout 40s \
    --format term \
    --node "$OBSERVER_NODE" \
    --cookie-env OBSERVER_COOKIE \
    >"$report" 2>"$errors"
then
    status=0
else
    status=$?
fi

case "$status" in
    0) echo "diagnosis complete: $report" ;;
    1) echo "diagnosis found warnings or critical findings: $report" ;;
    2) echo "usage, format, or capability failure: $report" >&2 ;;
    3) echo "runtime refusal or partial capture: $report" >&2 ;;
    4) echo "internal, schema, or cleanup failure: $report" >&2 ;;
    *) echo "unexpected observer_cli exit status: $status" >&2 ;;
esac

exit "$status"
```

The explicit `40s` deadline covers the `30s` observation plus the required
five-second cleanup margin. Command deadlines accept milliseconds or seconds
and cannot exceed `120s`.

## Interpret the streams and status

| Status | Meaning | Automation action |
| --- | --- | --- |
| `0` | Successful command with no diagnose findings | Store or process the report. |
| `1` | Complete diagnosis found warning or critical findings | Store the report and alert according to its findings. |
| `2` | Argument, output-format, or capability error | Fix the invocation or target capability. |
| `3` | Safety refusal, scan-budget refusal, connection failure, required-probe failure, or ordinary partial capture | Keep the report and retry or escalate deliberately. |
| `4` | Internal, schema, or cleanup failure | Keep all output and investigate before another invasive action. |

Successful structured output is written to stdout. With `term` or `json`,
command errors are also returned as an envelope on stdout when the selected
encoder is available. Human-text command errors are written to stderr. Capture
both streams regardless of format.

A trace can report a partial trace and still exit `0` when its bounded probe
completed without errors. For every command, inspect `capture.status`,
`warnings`, and `errors` instead of treating the exit code as the whole report.

## Control identifier exposure

`snapshot` and `diagnose` redact node, PID, name, and MFA identifiers by
default. Use that default for reports sent to external systems:

```sh
observer_cli diagnose --observe 10s --deep --format json > diagnosis.json
```

Add `--include-identifiers` only for a protected incident store where real
identifiers are required. Inspection and trace commands use the opposite
default and include identifiers; add `--redact` when automating their export:

```sh
observer_cli processes --sort memory --limit 20 --redact --format term > processes.term
```

`--redact` and `--include-identifiers` are mutually exclusive.

## Validate the job before scheduling it

Run the exact command interactively, then check all three outcomes:

```sh
observer_cli status --format term
observer_cli diagnose --observe 5s --format term
printf 'exit=%s\n' "$?"
```

Confirm that the target has the matching diagnostics bundle, the job can read
its cookie source, and the report destination protects unredacted output.
