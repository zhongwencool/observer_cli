# Investigate memory pressure and busy processes

Use this guide when a BEAM node is using more memory than expected, building
message queues, or spending time in a small set of processes.

## Before you begin

Build or install the `observer_cli` escript and select a target as described in
[Connect to a node](connect-to-a-node.md). The target must have a compatible
Observer CLI diagnostics bundle.

The examples below use the saved target context. You can instead append
`--node NODE` and exactly one of `--cookie-env NAME` or `--cookie-file PATH` to
each command.

## 1. Establish a baseline

Start with the quick diagnosis:

```sh
observer_cli diagnose
```

If the symptom changes over time, collect a short observation with the admitted
deep inventories:

```sh
observer_cli diagnose --observe 10s --deep --include-identifiers
```

`--deep` requires `--observe`. Snapshot and diagnosis output redact node, PID,
name, and MFA identifiers by default; `--include-identifiers` makes later
drill-down possible. Keep that output private.

## 2. Separate VM memory from scheduler pressure

Inspect point-in-time BEAM memory and allocator facts:

```sh
observer_cli memory
```

This reports memory known to the BEAM. It is not the operating system's RSS for
the VM process.

Measure scheduler utilization over a two-second window:

```sh
observer_cli schedulers --duration 2s
```

The scheduler command enables scheduler wall-time collection for the window and
turns the flag off afterward. Its capture metadata records that observer effect.
It does not restore an already-enabled setting, so coordinate with any other
tool using scheduler wall-time statistics. The accepted sampling range is
`250ms` to `10s`.

## 3. Rank likely process causes

List the largest processes first:

```sh
observer_cli processes --sort memory --limit 20
```

Change the sort key to test a specific hypothesis:

```sh
observer_cli processes --sort message_queue_len --limit 20
observer_cli processes --sort binary_memory --limit 20
observer_cli processes --sort total_heap_size --limit 20
```

Use a duration when totals would hide current activity. With `--duration`, the
ranking uses interval deltas rather than lifetime totals:

```sh
observer_cli processes --sort reductions --duration 2s --limit 20
observer_cli processes --sort memory --duration 2s --limit 20
```

List limits range from `1` to `200`; the default is `20`.

## 4. Inspect one process

Copy a PID or registered name from the ranking and inspect it. Quote a PID so
the shell does not interpret angle brackets:

```sh
observer_cli process '<0.123.0>'
observer_cli process my_registered_server
```

The result contains bounded process metadata and a normalized current
stacktrace. It deliberately omits messages, the process dictionary, and
arbitrary process state. A process can exit between the ranking and this
command; rerun the ranking if that happens.

For a `gen_server`, request the bounded shape of its state only when that shape
will answer the question:

```sh
observer_cli gen-server-state my_registered_server --redact
```

This command never returns the full state values, but it must copy the state
before reducing it to a shape and therefore reports `risk_level=high`. Avoid it
when the process may hold a very large state. See
[Safety and observer effect](../explanation/safety-and-observer-effect.md#state-and-supervision-inspection).

## 5. Attribute the process to an application

Group process resources by application:

```sh
observer_cli applications --sort memory --limit 20
observer_cli applications --sort message_queue_len --limit 20
```

Then inspect the bounded supervision tree for the application you identified:

```sh
observer_cli supervision-tree --app my_app
```

Supervision inspection also reports `risk_level=high`: its supervisor calls can
block, and the result is not an atomic tree snapshot.

Application attribution follows process group-leader chains. Unattributed
processes can appear separately instead of being assigned speculatively.

## 6. Check table growth

If process memory does not explain the node total, rank table metadata:

```sh
observer_cli ets --sort memory --limit 20
observer_cli ets --sort size --limit 20
observer_cli mnesia --sort memory --limit 20
```

The ETS command reads metadata, not table contents. The Mnesia command lists
local tables only and reports `not_running` when Mnesia is stopped. Tables can
disappear during a scan; the structured audit fields report those races.

## Share a sanitized capture

Inspection commands include identifiers by default. Rerun the relevant command
with `--redact` before attaching output to a public issue:

```sh
observer_cli processes --sort memory --limit 20 --redact --format term > processes.term
```

Redacted identifiers are suitable for correlation within that response, but
cannot be copied into a later `process` command.
