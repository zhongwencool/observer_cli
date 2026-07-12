# Run your first diagnosis

Build the standalone command, start a disposable BEAM node, then diagnose it and
inspect one process.

## Before you begin

You need Erlang/OTP, `rebar3`, a Unix-like shell, and a checkout of this
repository. Run every command from the repository root.

## 1. Build the command

Build the escript and add it to this shell's `PATH`:

```sh
rebar3 escriptize
export PATH="$PWD/_build/default/bin:$PATH"
observer_cli --version
```

The version output identifies `observer_cli 2.0.0`, schema
`observer_cli.cli/v1`, protocol `1`, and the OTP release used to build the
controller.

## 2. Start an isolated target node

Create a temporary Erlang home directory and an owner-only cookie file. The
same checkout supplies the target-side diagnostics bundle.

```sh
export OBSERVER_CLI_DEMO_HOME="$(mktemp -d)"
export OBSERVER_CLI_DEMO_NODE="observer_cli_demo_$$@127.0.0.1"

printf '%s\n' 'observer_cli_demo_cookie' \
  > "$OBSERVER_CLI_DEMO_HOME/.erlang.cookie"
chmod 600 "$OBSERVER_CLI_DEMO_HOME/.erlang.cookie"

HOME="$OBSERVER_CLI_DEMO_HOME" erl \
  -pa "$PWD/_build/default/lib/observer_cli/ebin" \
      "$PWD/_build/default/lib/recon/ebin" \
  -name "$OBSERVER_CLI_DEMO_NODE" \
  -noshell \
  -eval '{ok, _} = application:ensure_all_started(observer_cli), timer:sleep(infinity).' \
  > "$OBSERVER_CLI_DEMO_HOME/node.log" 2>&1 &

export OBSERVER_CLI_DEMO_PID=$!
sleep 1
```

The node runs in the background. Its log is in
`$OBSERVER_CLI_DEMO_HOME/node.log`.

## 3. Select the target

Use the same temporary home directory for the controller so the tutorial does
not replace your normal saved target:

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" \
  observer_cli connect \
    --node "$OBSERVER_CLI_DEMO_NODE" \
    --cookie-file "$OBSERVER_CLI_DEMO_HOME/.erlang.cookie"
```

Look for:

```text
probe succeeded
diagnostics_module=compatible
expected_capabilities=protocol=1,bundle=2.0.0
observed_capabilities=protocol=1,bundle=2.0.0
```

`connect` saves the node name and cookie-file path, not the cookie value. It
does not leave a connection or daemon running.

Confirm the saved context:

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" observer_cli status
```

## 4. Run a quick diagnosis

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" observer_cli diagnose
echo "diagnose exit status: $?"
```

Read the report in this order:

1. `data.summary` states the overall result.
2. `data.findings` contains detected warning or critical conditions.
3. `data.skipped` explains checks that were not applicable or unavailable.
4. `capture.status`, `capture.probes`, `warnings`, and `errors` show whether the
   evidence collection completed.

Exit status `0` means the complete report has no findings. Exit status `1`
means the complete report has findings; it does not mean the command failed.
The demo node normally returns `0`, although runtime activity can change its
measurements.

## 5. Drill into a process

List the three largest processes by BEAM process memory:

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" \
  observer_cli processes --sort memory --limit 3
```

The list includes a PID, registered name, current function, memory,
reductions, and mailbox length for each returned process. Inspect a stable
registered process from the demo node:

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" \
  observer_cli process application_controller
```

## 6. Clean up

Remove the tutorial context, stop the target, and delete its temporary files:

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" observer_cli disconnect
kill "$OBSERVER_CLI_DEMO_PID"
wait "$OBSERVER_CLI_DEMO_PID" 2>/dev/null || true
rm -rf "$OBSERVER_CLI_DEMO_HOME"
unset OBSERVER_CLI_DEMO_HOME OBSERVER_CLI_DEMO_NODE OBSERVER_CLI_DEMO_PID
```

`disconnect` only removes the saved context. Stopping the demo node is a
separate step.

## Next steps

- [Open your first TUI session](first-tui-session.md).
- [Connect to a node](../how-to/connect-to-a-node.md).
