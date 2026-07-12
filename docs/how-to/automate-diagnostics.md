# Automate diagnostic capture

Collect bounded reports from scripts, schedulers, or incident tooling with
structured output and exit statuses.

## Choose an output format

- `text` is for people and is the default.
- `term` is an Erlang term containing the complete command envelope.
- `json` contains the same envelope, requires an OTP 27 or newer controller,
  and can also be selected with `--json`.

For automation, use `term` or `json`; do not scrape `text`.

## Run without user-specific saved context

Pass the target and cookie source explicitly to avoid a login user's context
file:

```sh
export OBSERVER_NODE='app@host'
: "${OBSERVER_COOKIE:?populate this variable with your secret manager}"

observer_cli snapshot --deep --format term \
  --node "$OBSERVER_NODE" --cookie-env OBSERVER_COOKIE > snapshot.term
```

Exactly one of `--cookie-env` and `--cookie-file` is required with `--node`.
The cookie value is read at execution time and is not written into the output.

## Preserve non-zero diagnostic reports

`diagnose` exits `1` when a complete report contains warning or critical
findings. Preserve that report outcome, stdout, stderr, and the exit status:

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

The `40s` deadline covers the `30s` observation and required five-second
cleanup margin. Deadlines accept milliseconds or seconds, up to `120s`.

## Interpret the streams and status

| Status | Meaning | Automation action |
| --- | --- | --- |
| `0` | Successful command with no diagnose findings | Store or process the report. |
| `1` | Complete diagnosis found warning or critical findings | Store the report and alert according to its findings. |
| `2` | Argument, output-format, or capability error | Fix the invocation or target capability. |
| `3` | Safety refusal, scan-budget refusal, connection failure, required-probe failure, or ordinary partial capture | Keep the report and retry or escalate deliberately. |
| `4` | Internal, schema, or cleanup failure | Keep all output and investigate before another invasive action. |

Successful structured output goes to stdout. With `term` or `json`, command
errors also use a stdout envelope when the encoder is available. Human-text
errors go to stderr. Always capture both streams.

A trace can report a partial trace and still exit `0` when its bounded probe
completed without errors. For every command, inspect `capture.status`,
`warnings`, and `errors` instead of treating the exit code as the whole report.

## Control identifier exposure

`snapshot` and `diagnose` redact node, PID, name, and MFA identifiers by
default. Keep this default for reports sent to external systems:

```sh
observer_cli diagnose --observe 10s --deep --format json > diagnosis.json
```

Use `--include-identifiers` only when a protected incident store needs real
identifiers. Inspection and trace commands include identifiers by default; add
`--redact` when automating their export:

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

Confirm the target has the matching diagnostics bundle, the job can read its
cookie source, and the destination protects unredacted output.
