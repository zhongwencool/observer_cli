# Execution model

`observer_cli` has two interfaces because interactive exploration and repeatable
diagnostics need different runtime behavior. Both use Erlang distribution, but
they differ in how code reaches the target, how long the controller lives, and
what they promise to callers.

## The command interface is ephemeral

Each command invocation starts from a non-distributed escript. It then:

1. resolves the target, name mode, and cookie source;
2. starts a hidden distribution controller with no listening distribution port;
3. connects to the target and reads its diagnostics capabilities;
4. invokes `observer_cli_snapshot:dispatch/4` on the target;
5. validates and encodes the response; and
6. stops the controller before returning.

```mermaid
sequenceDiagram
    participant CLI as observer_cli command
    participant C as temporary controller
    participant T as target node
    CLI->>C: start hidden, outbound-only distribution
    C->>T: connect and read capabilities
    C->>T: dispatch bounded request
    T-->>C: normalized response and cleanup status
    C-->>CLI: validated envelope
    CLI->>C: stop and confirm cleanup
```

There is no command daemon and no persistent network connection. A later command
creates a new controller and performs a new capability probe.

The command controller must itself start from `nonode@nohost`. Running the
escript from an already distributed Erlang node is refused because the command
cannot provide its normal controller lifecycle guarantees in that state.

## The saved context is a selector

`connect` verifies a target and writes one active context under the operating
system's user configuration directory:

```text
<user-config>/observer_cli/context.etf
```

The context contains:

- target node text;
- `short` or `long` name mode; and
- either an environment-variable name or an absolute cookie-file path.

It does not contain the cookie value. The directory is restricted to mode `0700`
and the context file to `0600`. A new context is installed atomically only after
the temporary controller has stopped successfully. A failed connection, probe,
or cleanup therefore leaves the previous context unchanged.

`status` reads the selector and performs a fresh probe. `disconnect` removes the
selector; it does not close a persistent connection because none exists.

Scripts can avoid shared state entirely:

```sh
observer_cli memory \
  --node app@host \
  --cookie-file "$HOME/.erlang.cookie" \
  --format term
```

## Command targets must provide the diagnostics bundle

Before dispatch, the controller checks the target's
`observer_cli_snapshot:capabilities/0` result. This documentation describes:

```text
bundle_version = 2.0.0
protocol_version = 1
```

`connect` and `status` can still report a reachable target whose diagnostics
bundle is missing or incompatible. Commands that require target probes then fail
with a capability error. The command interface does not inject or replace BEAM
modules to repair that condition; the matching bundle belongs in the target
release.

This boundary makes automation easier to reason about: the target executes the
version that its release installed, while the controller verifies the contract
before requesting data.

## The TUI can load code remotely

The explicit `tui` route favors immediate interactive use. It starts a hidden
controller, connects to the target, and checks for a compatible
`observer_cli` installation. When the modules are missing or incompatible, it
uses `recon` to load the required `observer_cli` and configured formatter modules
before starting the remote TUI.

That convenience changes the operational boundary. The TUI:

- may load code and application environment into the target;
- runs continuously until the operator quits;
- refreshes and scans repeatedly; and
- renders terminal-oriented views rather than a versioned response envelope.

Use the TUI when an operator needs to explore. Use commands when a script, agent,
or repeatable runbook needs bounded data and explicit exit status.

## Target workers are bounded

Command probes execute in a monitored target worker. The dispatcher applies:

- a target-side deadline shorter than the controller deadline;
- a worker heap limit;
- response size and depth limits;
- scan admission budgets for inventories; and
- response normalization before data crosses distribution.

The dispatcher reports success only after the worker has exited normally. A
timeout, heap limit, invalid response, or unconfirmed cleanup is returned as an
error or partial result rather than being hidden behind a successful envelope.

These controls limit the work performed by the tool. They do not turn Erlang
distribution into a security sandbox; a connected distribution peer remains
trusted code. See [Safety and observer effect](safety-and-observer-effect.md).
