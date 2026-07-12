# Trace calls safely

Use call tracing only after snapshots and process inspection have narrowed the
problem to one target-local PID and one exact MFA.

Observer CLI tracing is bounded, but it is node-global infrastructure. Starting
a trace clears existing node-static traces, and emergency cleanup can clear
traces created by other tools.

## 1. Coordinate trace ownership

Before running the command, confirm that no operator or tool depends on an
existing static trace on the target. There is no isolated Observer CLI trace
namespace.

The target must have the matching Observer CLI bundle and `recon` 2.5.6
available.

## 2. Select one process

Find a candidate and verify that it is still alive:

```sh
observer_cli processes --sort reductions --duration 2s --limit 20
observer_cli process '<0.123.0>'
```

`trace call` accepts a local PID, not a registered name or a remote PID.

## 3. Start with a small capture

Trace one exact `module:function/arity` and acknowledge global replacement:

```sh
observer_cli trace call my_worker:handle_call/3 \
  --pid '<0.123.0>' \
  --duration 2s \
  --limit 20 \
  --replace-existing-trace \
  --format term > trace.term
```

The acknowledgement flag is mandatory. Wildcard module or function names are
rejected. The capture covers external global calls only; it does not promise
local-call, return-value, message, or process-state tracing.

Bounds are:

- duration: `100ms` to `60s`, default `10s`;
- event limit: `1` to `1000`, default `100`;
- rate: `1/s` to `200/s`.

Use `--rate` instead of `--limit` when a sustained rate cap is more useful:

```sh
observer_cli trace call my_worker:handle_call/3 \
  --pid '<0.123.0>' \
  --duration 10s \
  --rate 20/s \
  --replace-existing-trace \
  --format term > trace.term
```

`--rate` and `--limit` are mutually exclusive. If you set `--timeout`, it must
cover the trace duration plus five seconds and cannot exceed `120s`.

## 4. Verify completion and cleanup

In structured output, inspect `data.trace`:

- `trace_complete` says whether the requested capture remained valid;
- `truncated` says whether the event bound stopped collection;
- `module_reloaded` explains a capture invalidated by code reload;
- `cleanup_confirmed` confirms that call flags, trace patterns, and fixed-name
  trace helpers were removed.

A cleanup failure exits with status `4`. Do not assume tracing stopped merely
because the controller process ended.

Trace output includes real identifiers by default. Add `--redact` before saving
a report that will leave the incident boundary.

## 5. Run emergency cleanup only when needed

A normally completed `trace call` performs and verifies its own cleanup. If the
capture was interrupted or reports unconfirmed cleanup, coordinate again and
run:

```sh
observer_cli trace stop --all
```

`--all` is mandatory because this operation always calls
`recon_trace:clear/0`. With `recon` 2.5.6 it can also terminate the fixed-name
tracer or formatter processes. It may therefore remove unrelated node-static
traces.
