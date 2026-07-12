# Open your first TUI session

Start a disposable BEAM node and inspect its Home, Process, System, and Doc views
without placing a cookie on the command line.

## Before you begin

You need Erlang/OTP, `rebar3`, a Unix-like shell, and a checkout of this
repository. Run every command from the repository root in a terminal that is
at least 140 columns wide.

## 1. Build Observer CLI

```sh
rebar3 escriptize
export PATH="$PWD/_build/default/bin:$PATH"
```

Observer CLI 2.0 requires the explicit `tui` subcommand. The removed
`observer_cli NODE ...` form is not accepted.

## 2. Start a demo node with a protected cookie

```sh
export OBSERVER_CLI_DEMO_HOME="$(mktemp -d)"
export OBSERVER_CLI_DEMO_NODE="observer_cli_tui_demo_$$@127.0.0.1"

printf '%s\n' 'observer_cli_tui_demo_cookie' \
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

Both the target and the TUI controller will read the cookie from the temporary
Erlang home directory. This avoids the positional `COOKIE` argument, which
would be visible in process listings and shell history.

## 3. Open the Home view

```sh
HOME="$OBSERVER_CLI_DEMO_HOME" \
  observer_cli tui "$OBSERVER_CLI_DEMO_NODE"
```

The TUI refreshes every 1500 ms by default. Its top menu identifies the active
view. The Home view combines runtime limits and counters with a Top-N process
table. Commands are line-oriented: type a command, then press **Enter**.

> **Screenshot TODO — `docs/images/tui-home.png`**
>
> This page needs a current Home screenshot from OTP 29. It must show the
> selected Home tab, target and OTP information, system and memory summaries,
> named process rows, and the footer. Follow
> [Capture the current TUI Home screenshot](../how-to/contribute.md#9-capture-the-current-tui-home-screenshot)
> for the exact one-paste scene and terminal requirements.

## 4. Change the process ranking

Enter `r` to switch the Home table from process memory to total reductions.
Enter `m` to return to memory.

These commands select current totals. The doubled forms, such as `rr` and
`mm`, use an interval window instead.

## 5. Inspect one process

Enter `1` to open the first process in the current Top-N table. The Process Info
view shows identity, status, memory, reductions, links, and other live metadata.

Enter `H` to return to Home.

## 6. Visit System and Doc

Enter `S` to open the System view. It shows VM, CPU, memory, allocator, OS, and
distribution information.

Return with `H`, then open the built-in shortcut guide with `D`. Return to Home
with `H` again.

## 7. Quit and clean up

Enter `q` to leave the TUI. Stop the background node and remove its temporary
Erlang home:

```sh
kill "$OBSERVER_CLI_DEMO_PID"
wait "$OBSERVER_CLI_DEMO_PID" 2>/dev/null || true
rm -rf "$OBSERVER_CLI_DEMO_HOME"
unset OBSERVER_CLI_DEMO_HOME OBSERVER_CLI_DEMO_NODE OBSERVER_CLI_DEMO_PID
```

For a non-interactive workflow, continue with
[Run your first diagnosis](first-diagnosis.md).
