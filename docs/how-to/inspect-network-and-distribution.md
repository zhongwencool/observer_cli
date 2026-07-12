# Inspect network and Erlang distribution activity

Use this guide to distinguish Erlang distribution state, legacy `inet` traffic,
OTP socket-registry activity, and non-network Erlang ports.

## Before you begin

Select a compatible target as described in
[Connect to a node](connect-to-a-node.md). These examples use its saved target
context.

## 1. Confirm the target is reachable

Probe the saved target before interpreting node-to-node traffic:

```sh
observer_cli status
```

`status` starts a fresh controller connection. A successful result does not
mean that Observer CLI keeps a daemon or persistent connection running.

## 2. List distributed Erlang peers

```sh
observer_cli distribution --limit 50
```

The result separates visible and hidden connected nodes. It excludes the
temporary Observer CLI controller peer and records that exclusion in the
capture. Where the OTP runtime exposes distribution-controller queues, their
sizes are context only; Observer CLI does not infer backlog health from them.

The limit accepts `1` to `200` peers and defaults to `20`.

## 3. Find active legacy `inet` connections

Use a sampling duration to rank current traffic instead of lifetime totals:

```sh
observer_cli network --duration 2s --sort oct --limit 20
observer_cli network --duration 2s --sort recv_oct --limit 20
observer_cli network --duration 2s --sort send_oct --limit 20
```

You can also rank packet counts with `cnt`, `recv_cnt`, or `send_cnt`.
Durations range from `250ms` to `10s`. Without `--duration`, the command reports
counter totals.

`network` covers VM port-driver I/O and legacy TCP, UDP, and SCTP `inet` ports.
It is not a host-wide network monitor.

## 4. Inspect OTP socket-registry activity

The socket API has a separate inventory:

```sh
observer_cli sockets --duration 2s --sort io --limit 20
observer_cli sockets --duration 2s --sort waits --limit 20
observer_cli sockets --duration 2s --sort fails --limit 20
```

Other sort keys are `read_bytes`, `write_bytes`, and `packets`. The command
reports `capability_unavailable` when the target runtime does not expose the
required socket registry functions. An empty result means no registry-known
sockets; it does not prove that the operating system has no sockets.

## 5. Check non-`inet` Erlang ports

List port drivers that are not covered by the legacy network inventory:

```sh
observer_cli ports --sort queue_size --limit 20
observer_cli ports --sort io --limit 20
```

`ports` lists Erlang Port objects, not TCP or UDP port numbers. Its other sort
keys are `memory`, `input`, and `output`.

Copy a raw target-local port identifier from unredacted output to inspect one
port:

```sh
observer_cli port '#Port<0.12>'
```

Quote the identifier to protect `#` and angle brackets from the shell. Add
`--redact` only when exporting the result; it hides port, process, endpoint,
interface, and network-namespace identifiers.

## 6. Account for short-lived resources

Ports and sockets can open or close between samples. In term or JSON output,
check `lifecycle`, `disappeared_count`, `interval_ms`, and `metric_states`
before treating a missing item or `null` delta as a network failure:

```sh
observer_cli network --duration 2s --format term
observer_cli sockets --duration 2s --format term
```

Use `--redact` on inspection commands before sharing these captures outside the
incident team.
