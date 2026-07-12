# Connect to a node

Use `connect` to verify a BEAM target and save it as the default for later
commands. The target must be reachable through Erlang distribution. Diagnostic
and inspection commands require a compatible Observer CLI bundle; `connect`
can still save a reachable missing or incompatible target and report the
mismatch.

## Gather the target details

You need:

- the full distributed node name, such as `app@host.example`;
- the target's Erlang distribution cookie; and
- either a protected cookie file or an environment variable populated by your
  secret-management process.

Use only trusted targets and networks. Erlang distribution grants powerful,
bidirectional access and is not encrypted by default.

## Connect with a cookie file

A protected file is the preferred reusable source because it keeps the secret
out of command arguments and shell history. On Unix, remove all group and
other permissions before connecting:

```sh
chmod 600 /secure/path/app.cookie
observer_cli connect --node 'app@host.example' \
  --cookie-file '/secure/path/app.cookie'
```

Observer CLI accepts only a regular cookie file with safe permissions. The
file should contain only the cookie, optionally followed by one newline.

## Connect with an environment variable

Have your secret manager populate the variable without writing the cookie
literal in shell history, then name that variable in the command:

```sh
observer_cli connect --node 'app@host.example' \
  --cookie-env OBSERVER_CLI_COOKIE
```

`--cookie-env` receives the variable name, not the cookie value.

## Confirm the saved context

A successful connection reports the target OTP release, name mode, cookie
source metadata, and diagnostics capabilities. Confirm it at any time:

```sh
observer_cli status
```

`connect` does not keep a daemon or persistent connection. It stores only the
node name, name mode, and cookie-source metadata in an owner-only context
file. Each later command reads the cookie source again and creates a fresh
temporary controller connection.

Run commands without repeating the target:

```sh
observer_cli diagnose
observer_cli memory
observer_cli processes --sort reductions --limit 20
```

## Override node-name inference

Observer CLI infers long names when the host part contains a dot or colon and
short names otherwise. Override that choice when it does not match the target:

```sh
observer_cli connect --node 'app@internal-host' \
  --name-mode long \
  --cookie-file '/secure/path/app.cookie'
```

Valid modes are `short` and `long`. The controller's name mode must match the
target's mode.

## Run without a saved context

For stateless automation, pass the target and exactly one cookie source on
every command:

```sh
observer_cli diagnose \
  --node 'app@host.example' \
  --cookie-file '/secure/path/app.cookie' \
  --format json
```

An explicit `--node` without `--cookie-env` or `--cookie-file` is rejected.

## Change or remove the context

Run `connect` again to replace the saved target after the new target has been
probed successfully:

```sh
observer_cli connect --node 'app2@host.example' \
  --cookie-file '/secure/path/app2.cookie'
```

Remove the saved context when it is no longer needed:

```sh
observer_cli disconnect
```

`disconnect` removes local metadata. It is not a network disconnect
operation.

## Resolve common failures

- `missing_cookie_source`: pass exactly one of `--cookie-env` or
  `--cookie-file` with an explicit node.
- `cookie_file_permissions`: remove group and other permissions from the file.
- `no_active_context`: run `connect`, or supply the target options directly.
- A connection failure: verify the full node name, name mode, cookie, EPMD and
  network reachability.
- `capability_unavailable`: install the matching Observer CLI bundle in the
  target release. Command-first routes do not inject code into the node.
