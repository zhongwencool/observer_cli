# Investigate memory pressure and busy processes

Use these steps for unexpected BEAM memory, growing message queues, or process
hotspots.

## Before you begin

Build or install the `observer_cli` escript and select a target with a
compatible diagnostics bundle. See [Connect to a node](connect-to-a-node.md).

Commands below use the saved context. For one-shot use, append `--node NODE`
and exactly one of `--cookie-env NAME` or `--cookie-file PATH`.

## 1. Establish a baseline

Start with a quick diagnosis:

```sh
observer_cli diagnose
```

If the symptom changes over time, collect a short observation with deep
inventories:

```sh
observer_cli diagnose --observe 10s --deep --include-identifiers
```

`--deep` requires `--observe`. Snapshots and diagnoses redact node, PID, name,
and MFA identifiers by default. `--include-identifiers` enables later
drill-down; keep that output private.

## 2. Separate VM memory from scheduler pressure

Inspect BEAM memory and allocator facts:

```sh
observer_cli memory
```

This is BEAM-accounted memory, not the VM process's operating-system RSS.

Measure scheduler utilization over a two-second window:

```sh
observer_cli schedulers --duration 2s
```

The command enables scheduler wall-time collection for the window, then turns
the flag off. Capture metadata records that observer effect. It does not
restore an already-enabled setting, so coordinate with other tools using the
same statistics. The sampling range is `250ms` to `10s`.

## 3. Rank likely process causes

Rank processes by memory:

```sh
observer_cli processes --sort memory --limit 20
```

Try other sort keys for specific hypotheses:

```sh
observer_cli processes --sort message_queue_len --limit 20
observer_cli processes --sort binary_memory --limit 20
observer_cli processes --sort total_heap_size --limit 20
```

Add a duration when lifetime totals hide current activity. The ranking then
uses interval deltas:

```sh
observer_cli processes --sort reductions --duration 2s --limit 20
observer_cli processes --sort memory --duration 2s --limit 20
```

List limits range from `1` to `200`; the default is `20`.

## 4. Inspect one process

Inspect a PID or registered name from the ranking. Quote PIDs to protect angle
brackets from the shell:

```sh
observer_cli process '<0.123.0>'
observer_cli process my_registered_server
```

The result contains bounded metadata and a normalized current stacktrace, but
not messages, the process dictionary, or arbitrary state. A process can exit
between commands; rerun the ranking if needed.

For a `gen_server`, request the bounded state shape only when needed:

```sh
observer_cli gen-server-state my_registered_server --redact
```

The command never returns full state values, but it copies the state before
reducing it to a shape and therefore reports `risk_level=high`. Avoid it for a
potentially large state. See
[Safety and observer effect](../explanation/safety-and-observer-effect.md#state-and-supervision-inspection).

## 5. Attribute the process to an application

Group process resources by application:

```sh
observer_cli applications --sort memory --limit 20
observer_cli applications --sort message_queue_len --limit 20
```

Inspect the application's bounded supervision tree:

```sh
observer_cli supervision-tree --app my_app
```

Supervision inspection reports `risk_level=high`: supervisor calls can block,
and the result is not an atomic tree snapshot.

Attribution follows process group-leader chains. Unattributed processes remain
separate rather than being assigned speculatively.

## 6. Check table growth

If process memory does not explain the node total, rank table metadata:

```sh
observer_cli ets --sort memory --limit 20
observer_cli ets --sort size --limit 20
observer_cli mnesia --sort memory --limit 20
```

ETS reads metadata, not table contents. Mnesia lists local tables only and
reports `not_running` when stopped. Tables can disappear during a scan;
structured audit fields report those races.

## Share a sanitized capture

Inspection commands include identifiers by default. Before sharing output,
rerun the command with `--redact`:

```sh
observer_cli processes --sort memory --limit 20 --redact --format term > processes.term
```

Redacted identifiers support correlation within one response, but not a later
`process` lookup.
