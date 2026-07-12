# Command-line reference

`observer_cli` 2.0 combines bounded inspection commands with an interactive
TUI. This reference covers syntax, defaults, limits, and exit behavior. The
built escript exposes the same contract through `observer_cli --help` and
`observer_cli COMMAND --help`.

## Invocation

```text
observer_cli COMMAND [ARGUMENTS] [OPTIONS]
observer_cli tui NODE [COOKIE REFRESH_MS]
```

Options follow the command name. A global option before the command is rejected.

Local informational forms are:

| Form | Result |
| --- | --- |
| `observer_cli`, `observer_cli --help`, `observer_cli -h` | Root help |
| `observer_cli help COMMAND` | Help for one root command |
| `observer_cli COMMAND --help` | Help for one root command |
| `observer_cli trace call --help` | Trace-call help and safety contract |
| `observer_cli trace stop --help` | Trace-cleanup help and safety contract |
| `observer_cli --version` | Bundle version, CLI schema, protocol version, and controller OTP release |

`version` without the leading `--` is not a command.

## Target selection

Every command except `connect`, `status`, `disconnect`, and `tui` accepts a
saved or explicit target.

### Saved target

`connect` saves target metadata. A later command with no `--node` uses that context:

```text
observer_cli connect --node NODE \
  (--cookie-env NAME | --cookie-file PATH) [OPTIONS]
```

The context stores the node, name mode, and cookie source. It does not store the cookie and does not keep a connection open. `status` starts a new probe, and `disconnect` deletes the context.

A reachable target with a missing or incompatible diagnostics bundle can still be saved. `connect` and `status` report that state as a warning; diagnostic and inspection commands require the compatible bundle.

### Explicit target

Pass all of the following together:

```text
--node NODE (--cookie-env NAME | --cookie-file PATH)
```

`--cookie-env` and `--cookie-file` are mutually exclusive. `--name-mode`
accepts `short` or `long`. Without it, a host containing neither `.` nor `:`
selects short names; other hosts select long names. A node without `@HOST`
uses the controller hostname for this inference.

| Option | Meaning |
| --- | --- |
| `--node NODE` | Target distributed Erlang node |
| `--cookie-env NAME` | Read the cookie from environment variable `NAME` |
| `--cookie-file PATH` | Read the cookie from a protected regular file |
| `--name-mode short\|long` | Override name-mode inference |

Cookie-source and saved-context security rules are defined in [Output and storage contract](output-contract.md#saved-target-context).

## Output options

| Option | Default | Constraint |
| --- | --- | --- |
| `--format text\|term\|json` | `text` | Select one output encoding |
| `--json` | Off | Alias for `--format json`; JSON requires an OTP 27 or newer controller |
| `--redact` | See below | Replace identifiers in inspection and trace output |
| `--include-identifiers` | See below | Reveal identifiers in `snapshot` and `diagnose` |
| `--timeout DURATION` | Command-dependent | Positive duration, at most `120s` |

`--json` conflicts with a non-JSON `--format`. `--redact` and `--include-identifiers` are mutually exclusive.

`snapshot` and `diagnose` redact identifiers by default; use
`--include-identifiers` to reveal them. Other remote inspection and trace
commands include identifiers by default; use `--redact` to hide them. Context
commands accept neither option.

A duration is a positive integer in milliseconds (`1500` or `1500ms`) or seconds (`2s`). Fractional durations are not accepted.

The ordinary deadline is `10s`. Derived defaults are:

- a command with an explicit `--duration`: the larger of `10s` and the sample duration plus `5s`;
- `diagnose --observe`: the observation duration plus `5s`;
- `trace call`: the larger of `10s` and the trace duration plus `5s` (`15s` with defaults).

An explicit timeout for sampled work must cover the sample or observation duration plus `5s`. In particular, an explicit timeout for `schedulers` must be at least `6.5s` when its default `1500ms` sample is used.

## Target context commands

### `connect`

```text
observer_cli connect --node NODE \
  (--cookie-env NAME | --cookie-file PATH) \
  [--name-mode short|long] [--timeout DURATION] \
  [--format text|term|json] [--json]
```

Verifies distribution reachability, reads target OTP information, probes diagnostics capabilities, and saves target metadata. It reports expected and observed bundle/protocol versions. It never stores the cookie or leaves a controller node running.

Defaults: inferred name mode, `10s` timeout, text output.

### `status`

```text
observer_cli status [--timeout DURATION] \
  [--format text|term|json] [--json]
```

Loads and probes the saved context. The result includes target OTP, name mode, cookie-source metadata, and diagnostics compatibility. It creates no persistent connection.

Defaults: `10s` timeout, text output.

### `disconnect`

```text
observer_cli disconnect [--format text|term|json] [--json]
```

Deletes the saved target context. It performs no network disconnect. Repeating the command with no context succeeds.

## Diagnostics

### `diagnose`

```text
observer_cli diagnose [--observe DURATION] [--deep | --app APP] \
  [--include-identifiers] [TARGET OPTIONS] [OUTPUT OPTIONS]
```

With no diagnostic option, performs a two-sample diagnosis over roughly 1.5
seconds. A completed diagnosis with warning or critical findings exits `1`.

| Option | Default | Constraint |
| --- | --- | --- |
| `--observe DURATION` | No observation window | `5s` to `60s` |
| `--deep` | Off | Requires `--observe`; conflicts with `--app` |
| `--app APP` | No application scope | Requires `--observe`; conflicts with `--deep` |
| `--include-identifiers` | Off | Include node, PID, name, application, and MFA identifiers |

### `snapshot`

```text
observer_cli snapshot [--deep] [--include-identifiers] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Collects a bounded fact bundle. The default captures core VM facts without resource inventories. `--deep` adds bounded process, application, table, network, port, and socket inventories.

Defaults: shallow capture, redacted identifiers.

## VM health commands

### `memory`

```text
observer_cli memory [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Returns point-in-time BEAM memory, allocator, and runtime facts. Values are VM facts, not host RSS. There are no command-specific options.

### `schedulers`

```text
observer_cli schedulers [--duration DURATION] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Measures normal and dirty scheduler utilization and run queues.

| Option | Default | Range |
| --- | --- | --- |
| `--duration` | `1500ms` | `250ms` to `10s` |

### `distribution`

```text
observer_cli distribution [--limit N] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Returns visible and hidden connected nodes plus distribution context.

| Option | Default | Range |
| --- | --- | --- |
| `--limit` | `20` | `1` to `200` |

### `network`

```text
observer_cli network [--sort KEY] [--limit N] [--duration DURATION] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Returns VM port-driver and legacy `inet` counters. It does not represent all host network traffic. Without `--duration`, counters are totals; with it, the result contains interval deltas.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `oct` | `oct`, `recv_oct`, `send_oct`, `cnt`, `recv_cnt`, `send_cnt` |
| `--limit` | `20` | `1` to `200` |
| `--duration` | No interval | `250ms` to `10s` |

## Runtime-resource commands

### `processes`

```text
observer_cli processes [--sort KEY] [--limit N] [--duration DURATION] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Lists top local processes using bounded, explicit-key inspection. `--duration` reports interval deltas for changing metrics instead of cumulative totals.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `memory` | `memory`, `message_queue_len`, `reductions`, `binary_memory`, `total_heap_size` |
| `--limit` | `20` | `1` to `200` |
| `--duration` | No interval | `250ms` to `10s` |

### `process`

```text
observer_cli process PID_OR_NAME [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Inspects one target-local PID, such as `"<0.123.0>"`, or one registered name.
The bounded response includes process metadata and a normalized current
stacktrace, but excludes messages, the process dictionary, and arbitrary
process state.

### `applications`

```text
observer_cli applications [--sort KEY] [--limit N] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Groups process count, memory, reductions, and message queues by application.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `memory` | `memory`, `process_count`, `reductions`, `message_queue_len` |
| `--limit` | `20` | `1` to `200` |

### `ets`

```text
observer_cli ets [--sort KEY] [--limit N] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Lists ETS metadata without reading table contents.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `memory` | `memory`, `size` |
| `--limit` | `20` | `1` to `200` |

### `mnesia`

```text
observer_cli mnesia [--sort KEY] [--limit N] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Lists local Mnesia table metadata. A stopped Mnesia application is represented as `not_running`.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `memory` | `memory`, `size` |
| `--limit` | `20` | `1` to `200` |

### `ports`

```text
observer_cli ports [--sort KEY] [--limit N] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Lists non-`inet` Erlang Port resources. These are VM Port objects, not TCP or UDP port numbers.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `queue_size` | `queue_size`, `memory`, `input`, `output`, `io` |
| `--limit` | `20` | `1` to `200` |

### `port`

```text
observer_cli port '#Port<0.N>' [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Inspects one target-local Erlang Port. Only the canonical target text `#Port<0.N>` is accepted. `--redact` hides the port, process, endpoint, interface, and network-namespace identifiers.

### `sockets`

```text
observer_cli sockets [--sort KEY] [--limit N] [--duration DURATION] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Lists sockets exposed by the OTP `socket` registry. Without `--duration`, counters are totals; with it, the result contains interval deltas.

| Option | Default | Accepted values |
| --- | --- | --- |
| `--sort` | `io` | `io`, `read_bytes`, `write_bytes`, `packets`, `waits`, `fails` |
| `--limit` | `20` | `1` to `200` |
| `--duration` | No interval | `250ms` to `10s` |

## OTP-structure commands

### `gen-server-state`

```text
observer_cli gen-server-state PID_OR_NAME [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Resolves one local PID or registered name and returns only a bounded structural
shape of its `gen_server` state, never the full values. The response records
its risk level; `--redact` hides identifiers in the shape.

### `supervision-tree`

```text
observer_cli supervision-tree --app APP \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Returns a bounded supervision tree rooted in one running application. `--app` is required.

## Trace commands

Tracing is node-global. Both trace operations require an acknowledgement option because they can clear tracing established by another tool.

### `trace call`

```text
observer_cli trace call MODULE:FUNCTION/ARITY \
  --pid PID --replace-existing-trace \
  [--duration DURATION] [--limit N | --rate N/s] \
  [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Runs call-only tracing for one exact MFA and one target-local PID. Module and function wildcards are not accepted. Arity is `0` to `255`.

| Option | Default | Range or requirement |
| --- | --- | --- |
| `--pid` | None | Required target-local PID |
| `--duration` | `10s` | `100ms` to `60s` |
| `--limit` | `100` | `1` to `1000` events; conflicts with `--rate` |
| `--rate` | Off | `1/s` to `200/s`; conflicts with `--limit` |
| `--replace-existing-trace` | Off | Required acknowledgement |

Setup clears existing node-static tracing before installing the exact pattern.

### `trace stop`

```text
observer_cli trace stop --all [TARGET OPTIONS] [OUTPUT OPTIONS]
```

Clears observer_cli tracing and is also the emergency cleanup operation. `--all` is required. The operation always calls `recon_trace:clear/0`, so it can remove unrelated node-static traces. With the bundled recon 2.5.6 behavior, fixed-name tracer or formatter processes may also be terminated.

## Interactive TUI command

```text
observer_cli tui NODE [COOKIE REFRESH_MS]
```

Starts the terminal UI. `REFRESH_MS` defaults to `1500` and must be at least `1000`. The three-argument positional form requires both `COOKIE` and `REFRESH_MS`; there is no form with `NODE COOKIE` only.

When `COOKIE` is omitted, the controller's current cookie is used. A positional
cookie is visible in process arguments and shell history. Before starting the
UI, the escript loads its TUI module bundle when the target copy is missing or
incompatible. See [TUI reference](tui.md) for pages and keys.

## Exit statuses

| Code | Meaning |
| --- | --- |
| `0` | Success, including a complete diagnosis with no findings |
| `1` | Complete `diagnose` result with warning or critical findings |
| `2` | Argument, format, or direct capability error |
| `3` | Runtime failure, safety refusal, scan-budget failure, required-probe failure, or ordinary partial capture |
| `4` | Internal, schema-validation, or cleanup failure |

A command may return structured data with a nonzero status. A bounded trace is
the exception to the ordinary partial-capture mapping: it may be partial and
still exit `0` when its probe completed without errors. Automation must inspect
both the exit status and the envelope's `capture`, `warnings`, and `errors`
fields. See
[Output and storage contract](output-contract.md).
