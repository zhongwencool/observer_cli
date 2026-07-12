# Safety and observer effect

`observer_cli` bounds diagnostic work and reports known side effects, but every
distribution connection, resource enumeration, state copy, and trace consumes
target resources.

Use the least invasive command that can answer the question.

## Erlang distribution is the security boundary

The cookie authenticates an Erlang distribution peer; it is not a read-only
credential. A connected peer can execute code on the target, and distribution is
not encrypted by default.

Operationally:

- connect only to trusted nodes;
- use a trusted network or separately secured transport;
- prefer `--cookie-env` or a protected `--cookie-file` over command-line cookies;
- do not publish context files or machine-readable reports; and
- treat a compromised controller host as a compromised trust peer.

The command interface narrows what `observer_cli` itself requests. It does not
reduce the authority that Erlang distribution grants to the controller.

## Choose work by invasiveness

The command set has four practical levels:

### Point-in-time facts

`snapshot`, `memory`, `distribution`, and similar commands read bounded runtime
facts. A default snapshot avoids process, table, port, and socket inventories,
but still creates a distribution peer and target worker.

### Inventories and sampling windows

Commands such as `processes`, `applications`, `ets`, `mnesia`, `ports`, and
`sockets` enumerate target resources. `--duration`, `--observe`, and `--deep`
repeat work or retain samples across a window. The dispatcher can refuse a scan
when its admission budget or working-set estimate is exceeded.

When a scan is refused, narrow the command or use another operational path
rather than repeatedly forcing it.

### State and supervision inspection

`gen-server-state` and `supervision-tree` report `risk_level=high`.

`gen-server-state` must copy process state before reducing it to a bounded shape.
The returned document omits full values, but acquisition can still copy a large
term, and a timeout cannot retract a request already delivered to the process.

`supervision-tree` is intentionally bounded to one application root and its direct
children. Its supervisor calls can still block and its snapshot is not atomic.

### Active tracing

`trace call` changes node-global static tracing. It requires:

- one exact exported MFA;
- one live target-local PID;
- a duration between 100 ms and 60 seconds;
- a limit of 1 to 1000 events or a rate of 1 to 200 events/second; and
- `--replace-existing-trace` as explicit acknowledgement.

Setup and cleanup call `recon_trace:clear/0`. With the pinned `recon` 2.5.6
behavior, this can remove unrelated node-static traces and can terminate processes
occupying recon's fixed tracer or formatter names. `trace stop --all` has the same
node-global scope.

The trace output records only the tracee identifier, MFA module/function/arity,
and a session-relative offset. It does not collect call arguments, return values,
exceptions, or stacks. The trace remains node-global despite this limited output.

## Bounded does not mean constant cost

The target dispatcher applies deadlines, heap limits, response caps, maximum
depth, and scan budgets. They bound each request; cost still depends on the
target:

- an inventory grows with the number of resources it must consider;
- process and application attribution reads metadata from many processes;
- duration modes take multiple samples and match stable identities;
- scheduler wall-time measurement changes a node-global VM flag and can turn
  off a setting another tool had enabled; and
- state inspection may copy data before output bounds can be applied.

Start with defaults. Increase `--limit`, add `--duration`, or choose `--deep` only
when the preceding result shows why more data is necessary.

## Sampling changes the sample

The diagnostics controller and worker add processes, memory, reductions, ports,
and a distribution peer while measurements run. Reports identify known observer
effects, and some count fields explicitly say that the observer is included.

Observation mode and the `schedulers` command set scheduler wall-time statistics
to `true` for sampling and then set them to `false`; they do not restore a
preexisting enabled setting. The TUI can also toggle that VM flag and
continuously refresh data. Measurements from either interface should be read as
evidence from a window, not as an atomic and untouched VM state.

Deltas can also be distorted when:

- a process or resource can be born, die, or be replaced between samples; and
- a heavy probe can overlap the interval whose scheduler or counter change is
  being measured.

The report records lifecycle and sampling-gap context instead of treating every
pair of values as one stable resource.

## Data collection boundaries

Default snapshot and diagnosis workflows collect metadata and bounded metrics,
not arbitrary application payloads. They do not read:

- process mailbox contents;
- process dictionaries;
- ETS or Mnesia table contents;
- application environment values;
- cookies;
- trace arguments or return values; or
- arbitrary expressions supplied by the operator.

The `process` inspection includes a bounded, normalized current stacktrace.
`gen-server-state` is the deliberate high-risk exception that acquires state and
returns only its bounded shape.

The interactive TUI has explicit process subviews for messages, the process
dictionary, the current stack, and process state. Opening those views reads and
renders the selected data. Treat the terminal and any custom formatter as part of
the trusted environment.

Raw identifiers can still be sensitive. Snapshot and diagnosis redact them by
default; inspection and trace commands require `--redact` when the report will
leave the trusted operational environment.

## A safe operating sequence

For an unfamiliar production incident:

1. run `status` to confirm the target and capability versions;
2. run a default `diagnose` or `snapshot`;
3. inspect the reported probe coverage and skipped checks;
4. use one narrow resource command for the observed domain;
5. add a sampling duration only when a point-in-time value is insufficient;
6. use high-risk inspection only with a specific target; and
7. trace only after checking for existing tracing and accepting node-global
   replacement.
