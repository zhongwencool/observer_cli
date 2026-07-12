# Install and build Observer CLI

Observer CLI has two deployable parts:

- the `observer_cli` application in the target release; and
- the standalone `observer_cli` escript on the operator machine.

Install the target application for command-first diagnostics. Build the
escript when you want the command interface or the remote TUI.

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

Then fetch and compile it:

```sh
rebar3 compile
```

Ensure the built release contains the `observer_cli` and `recon` applications.
The command interface does not upload missing code to a target.

## Add Observer CLI to an Elixir target

Add the dependency to `mix.exs`:

```elixir
defp deps do
  [
    {:observer_cli, "~> 2.0"}
  ]
end
```

Then fetch and compile it:

```sh
mix deps.get
mix compile
```

When assembling a release, verify that its application set includes
`observer_cli` and `recon`.

## Build the standalone command from source

From an Observer CLI repository checkout, run:

```sh
rebar3 escriptize
./_build/default/bin/observer_cli --version
./_build/default/bin/observer_cli --help
```

`rebar3 escriptize` writes the executable to
`_build/default/bin/observer_cli`. Re-run it after changing Observer CLI or its
dependencies because the escript embeds their BEAM files.

To install that build for your user:

```sh
mkdir -p "$HOME/.local/bin"
install -m 0755 _build/default/bin/observer_cli \
  "$HOME/.local/bin/observer_cli"
```

Add `$HOME/.local/bin` to `PATH` if it is not already present.

## Verify both sides

First verify the local command:

```sh
observer_cli --version
```

For this documentation version, the output should report bundle `2.0.0`,
schema `observer_cli.cli/v1`, and protocol `1`.

Then connect to the target. A successful probe reports
`diagnostics_module=compatible` and matching expected and observed
capabilities:

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

This form uses the Erlang cookie already available to the controller. Avoid
the positional cookie form because it exposes the cookie in process arguments
and shell history. See [Open your first TUI session](../tutorials/first-tui-session.md)
for a protected cookie-file setup.
