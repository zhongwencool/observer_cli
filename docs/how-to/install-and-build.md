# Install and build Observer CLI

Install the `observer_cli` application in the target release for diagnostics.
Build the standalone escript on the operator machine for the CLI and remote
TUI.

## CI-tested OTP releases

The project CI builds and tests on Erlang/OTP 26, 27, 28, and 29. Use a
`rebar3` version compatible with your chosen OTP release.

## Add Observer CLI to an Erlang target

Add the dependency to `rebar.config`:

```erlang
{deps, [
    {observer_cli, "2.0.0"}
]}.
```

Fetch and compile:

```sh
rebar3 compile
```

The release must contain the `observer_cli` and `recon` applications. The CLI
does not upload missing code to a target.

## Add Observer CLI to an Elixir target

Add the dependency to `mix.exs`:

```elixir
defp deps do
  [
    {:observer_cli, "~> 2.0"}
  ]
end
```

Fetch and compile:

```sh
mix deps.get
mix compile
```

When assembling a release, verify that its application set includes
`observer_cli` and `recon`.

## Build the standalone command from source

From an Observer CLI repository checkout:

```sh
rebar3 escriptize
./_build/default/bin/observer_cli --version
./_build/default/bin/observer_cli --help
```

`rebar3 escriptize` writes `_build/default/bin/observer_cli`. The escript embeds
its BEAM files, so rebuild it after changing Observer CLI or its dependencies.

To install it for the current user:

```sh
mkdir -p "$HOME/.local/bin"
install -m 0755 _build/default/bin/observer_cli \
  "$HOME/.local/bin/observer_cli"
```

Add `$HOME/.local/bin` to `PATH` if it is not already present.

## Verify both sides

Verify the local command:

```sh
observer_cli --version
```

Version 2.0.0 reports bundle `2.0.0`, schema `observer_cli.cli/v1`, and protocol
`1`.

Probe the target. Success reports `diagnostics_module=compatible` and matching
expected and observed capabilities:

```sh
observer_cli connect --node 'app@host.example' \
  --cookie-file '/secure/path/app.cookie'
observer_cli status
```

See [Connect to a node](connect-to-a-node.md) for cookie and node-name setup.

## Start the TUI

Use the explicit 2.0 entry point:

```sh
observer_cli tui 'app@host.example'
```

This form uses the Erlang cookie available to the controller. Avoid the
positional cookie form: it exposes the cookie in process arguments and shell
history. See [Open your first TUI session](../tutorials/first-tui-session.md)
for a protected cookie-file setup.
