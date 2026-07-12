# Inspect network and Erlang distribution activity

Use these commands to distinguish Erlang distribution state, legacy `inet`
traffic, OTP socket-registry activity, and non-network Erlang ports.

## Before you begin

Select a compatible target and save its context. See
[Connect to a node](connect-to-a-node.md).

## 1. Confirm the target is reachable

Probe the saved target before interpreting node-to-node traffic:

```sh
observer_cli status
```

`status` starts a fresh controller connection; success does not imply a daemon
or persistent connection.

## 2. List distributed Erlang peers

```sh
observer_cli distribution --limit 50
```

The result separates visible and hidden connected nodes. It excludes the
temporary Observer CLI controller peer and records that exclusion in the
capture. Distribution-controller queue sizes, when exposed by OTP, are context
only; Observer CLI does not infer backlog health from them.

The limit accepts `1` to `200` peers and defaults to `20`.

## 3. Find active legacy `inet` connections

Sample to rank current traffic instead of lifetime totals:

```sh
observer_cli network --duration 2s --sort oct --limit 20
observer_cli network --duration 2s --sort recv_oct --limit 20
observer_cli network --duration 2s --sort send_oct --limit 20
```

Packet-count sort keys are `cnt`, `recv_cnt`, and `send_cnt`. Durations range
from `250ms` to `10s`; without one, the command reports counter totals.

`network` covers VM port-driver I/O and legacy TCP, UDP, and SCTP `inet` ports.
It is not a host-wide network monitor.

## 4. Inspect OTP socket-registry activity

The socket API has a separate inventory:

```sh
observer_cli sockets --duration 2s --sort io --limit 20
observer_cli sockets --duration 2s --sort waits --limit 20
observer_cli sockets --duration 2s --sort fails --limit 20
```

Other sort keys are `read_bytes`, `write_bytes`, and `packets`.
`capability_unavailable` means the target lacks the required socket-registry
functions. An empty result means no registry-known sockets, not that the
operating system has none.

## 5. Check non-`inet` Erlang ports

List port drivers that are not covered by the legacy network inventory:

```sh
observer_cli ports --sort queue_size --limit 20
observer_cli ports --sort io --limit 20
```

`ports` lists Erlang Port objects, not TCP or UDP port numbers. Its other sort
keys are `memory`, `input`, and `output`.

Inspect a raw target-local port identifier from unredacted output:

```sh
observer_cli port '#Port<0.12>'
```

Quote the identifier to protect `#` and angle brackets from the shell. Use
`--redact` only when exporting; it hides port, process, endpoint, interface,
and network-namespace identifiers.

## 6. Account for short-lived resources

Ports and sockets can open or close between samples. Before treating a missing
item or `null` delta as failure, check `lifecycle`, `disappeared_count`,
`interval_ms`, and `metric_states` in term or JSON output:

```sh
observer_cli network --duration 2s --format term
observer_cli sockets --duration 2s --format term
```

Use `--redact` before sharing captures outside the incident team.
