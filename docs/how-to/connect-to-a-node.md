# Connect to a node

`connect` probes a BEAM target over Erlang distribution and saves it for later
commands. Diagnostic and inspection commands require a compatible Observer CLI
bundle. A reachable target with a missing or incompatible bundle can still be
saved; `connect` reports the mismatch.

## Gather the target details

Provide:

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

Observer CLI accepts only a regular cookie file with safe permissions
containing the cookie and, optionally, one trailing newline.

## Connect with an environment variable

Have your secret manager populate the variable without exposing the cookie in
shell history, then pass its name:

```sh
observer_cli connect --node 'app@host.example' \
  --cookie-env OBSERVER_CLI_COOKIE
```

`--cookie-env` receives the variable name, not the cookie value.

## Confirm the saved context

A successful connection reports the target OTP release, name mode, cookie
source metadata, and diagnostics capabilities:

```sh
observer_cli status
```

`connect` stores only the node name, name mode, and cookie-source metadata in
an owner-only context file. It does not keep a daemon or connection. Each
command rereads the cookie source and creates a temporary controller
connection.

Run commands without repeating the target:

```sh
observer_cli diagnose
observer_cli memory
observer_cli processes --sort reductions --limit 20
```

## Override node-name inference

Observer CLI infers long names when the host contains a dot or colon, and short
names otherwise. Override a wrong inference:

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

Run `connect` again to replace the saved target after the new target is probed
successfully:

```sh
observer_cli connect --node 'app2@host.example' \
  --cookie-file '/secure/path/app2.cookie'
```

Remove an unused context:

```sh
observer_cli disconnect
```

`disconnect` removes local metadata; it does not perform a network disconnect.

## Resolve common failures

- `missing_cookie_source`: pass exactly one of `--cookie-env` or
  `--cookie-file` with an explicit node.
- `cookie_file_permissions`: remove group and other permissions from the file.
- `no_active_context`: run `connect`, or supply the target options directly.
- A connection failure: verify the full node name, name mode, cookie, EPMD and
  network reachability.
- `capability_unavailable`: install the matching Observer CLI bundle in the
  target release. Command-first routes do not inject code into the node.
