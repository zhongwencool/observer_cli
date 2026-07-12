# Output and storage contract

The command-oriented interface returns a versioned response envelope, a process exit status, and a defined stdout/stderr split. This contract does not apply to the interactive TUI screen.

## Contract identities

The current identities are:

| Identity | Value |
| --- | --- |
| CLI schema | `observer_cli.cli/v1` |
| Diagnostics bundle | `2.0.0` |
| Target protocol | `1` |

`observer_cli --version` reports all three local identities and the controller OTP release.

A target is command-compatible only when `observer_cli_snapshot:capabilities/0` reports both the exact bundle and protocol values. Extra capability-map fields do not change that comparison. `connect` and `status` can report a missing or incompatible bundle, but diagnostic and inspection commands reject it as a capability error.

## Response envelope

A normal structured response has exactly these top-level keys:

```erlang
#{
  <<"schema">> => <<"observer_cli.cli/v1">>,
  <<"command">> => Command,
  <<"target">> => Target,
  <<"capture">> => Capture,
  <<"data">> => Data,
  <<"warnings">> => Warnings,
  <<"errors">> => Errors
}
```

Maps use binary keys after normalization. Public values are JSON-safe: maps, lists, UTF-8 binaries, integers, floats, booleans, and `null`. Non-UTF-8 binary data is represented by an object with `encoding: base64` and `data` fields when admitted by the schema.

### `command`

The value is the stable binary command identity. Most identities match the command name. Trace responses use `trace_call` and `trace_stop_all`.

### `target`

For target responses:

```erlang
#{
  <<"node">> => NodeOrRedactedId,
  <<"otp_release">> => OtpReleaseBinary
}
```

Controller-side failures that occur before a target is established use `null`.

### `capture`

Collected responses contain:

```erlang
#{
  <<"status">> => <<"complete">> | <<"partial">>,
  <<"started_at">> => Rfc3339Utc,
  <<"finished_at">> => Rfc3339Utc,
  <<"duration_ms">> => NonNegativeInteger,
  <<"probes">> => Probes,
  <<"observer_effects">> => ObserverEffects
}
```

Each probe has exactly:

```erlang
#{
  <<"id">> => ProbeId,
  <<"required">> => Boolean,
  <<"status">> => <<"ok">> | <<"unavailable">> | <<"timeout">> | <<"error">>,
  <<"reason_code">> => null | ReasonCode,
  <<"duration_ms">> => NonNegativeInteger,
  <<"samples">> => NonNegativeInteger,
  <<"coverage">> => CoverageList
}
```

An `ok` probe has `reason_code => null`. Optional unavailable probes can coexist with useful data. Required-probe failures and incomplete captures affect the exit status.

Context-only responses and failures without a capture may use `null` for `capture`.

### `data`

`data` is a command-specific map. List commands include explicit status, returned-count, truncation, sort, and resource fields rather than returning a bare list. Failures before collection use `null`.

The envelope contract intentionally does not define one universal shape below
`data`. Consumers must branch on `command`, require the command-specific fields
they use, and tolerate additional fields. The
[command-line reference](cli.md) describes each command's data domain; probe
coverage in the response states what the current capture actually established.

### `warnings` and `errors`

Both are lists. Error maps have `class` and `reason_code`; target-probe errors may also have `probe`, while controller-created errors include a human-readable `message`.

`reason_code` is an extensible machine-readable code, not a closed enumeration.
Automation should handle known codes and preserve unknown codes for reporting;
the numeric exit class remains the stable coarse-grained action boundary.

Callers must not infer success from a nonempty `data` map alone. Inspect the process exit status, `capture.status`, probe status, and `errors`.

## Encodings

### Text

Text is the default. Context commands use concise summaries. Other commands render the shared envelope as indented, line-oriented text with stable field labels and escaped control bytes.

Text is intended for terminal reading. Use term or JSON when a parser needs types and nesting.

### Erlang term

```text
--format term
```

The output is one consultable Erlang map written with `~tp`, followed by `.` and a newline. It is textual Erlang syntax, not Erlang external term format.

### JSON

```text
--format json
--json
```

The output is one JSON object followed by a newline. JSON encoding uses the OTP `json` module and therefore requires an OTP 27 or newer controller. Target OTP does not supply the encoder.

### Size limit

An encoded response is capped at 1 MiB. The target response is also schema-checked and size-checked before the controller accepts it. Bounded list payloads may be trimmed while preserving required evidence references; a response that still exceeds the cap fails with a schema error.

## Identifier policy

`snapshot` and `diagnose` use redaction by default. `--include-identifiers` changes their policy to include.

Inspection and trace commands include identifiers by default. `--redact` changes their policy to redact.

Redaction replaces admitted node, peer, PID, Port, reference, socket, table, name, application, label, endpoint, interface, network-namespace, module, and function identifiers with typed aliases such as:

```text
node-1
pid-1
module-1
function-1
```

An alias is consistent for repeated occurrences of the same typed identifier within one response. Alias numbering is generated per response and is not a persistent cross-run identity.

Redaction does not remove numeric metrics, statuses, timestamps, reason codes, or coverage metadata. Context commands report the selected node and cookie-source name or path and do not accept identifier-policy options; they never report the cookie value.

## Standard streams

| Situation | stdout | stderr |
| --- | --- | --- |
| Successful text, term, or JSON command | Encoded response | Empty |
| Text command error | Empty | Plain diagnostic; argument errors include a help hint |
| Term or JSON command error, with working encoder | Error envelope | Empty |
| JSON requested on OTP before 27 | Empty | Plain encoder diagnostic |
| Encoder/bootstrap failure that cannot encode an envelope | Empty | Plain diagnostic |
| Help or version | Help/version text | Empty |

The TUI owns stdout while interactive and is outside this table.

## Exit statuses

| Code | Categories |
| --- | --- |
| `0` | Success; complete diagnosis with no findings |
| `1` | Complete diagnosis with one or more findings |
| `2` | Argument, output-format, or direct capability error |
| `3` | Safety refusal, scan-budget exhaustion, controller/distribution/connection failure, required-probe failure, or partial capture |
| `4` | Internal, cleanup, or schema failure; unknown categories also map here |

Trace capture can have `capture.status = partial` while still exiting `0` when the bounded trace probe itself completed and no errors were recorded. Use the command-specific data and probe status in addition to the generic capture status.

## Saved target context

The context path is:

```erlang
filename:join(filename:basedir(user_config, "observer_cli"), "context.etf")
```

For example, on macOS it resolves to:

```text
~/Library/Application Support/observer_cli/context.etf
```

The platform's standard user-configuration base directory determines the location. Evaluate the Erlang expression above to obtain the exact path on a controller.

The context is a version-1 Erlang external-term map containing only:

- normalized node text;
- `short` or `long` name mode;
- cookie-source type and its environment-variable name or absolute file path.

The cookie value and a live connection are never stored.

### Filesystem requirements

- The context directory must be a real directory with mode `0700`.
- `context.etf` must be a regular, non-symlink file with mode `0600`.
- The file is capped at 8192 bytes and decoded with `binary_to_term(Binary, [safe])`.
- Writes use a mode-`0600` temporary file followed by rename.
- Unsafe file type or permissions are rejected rather than followed or repaired during read.

`connect` writes a new context only after output-format preflight, target probing, and temporary controller cleanup succeed. A failed connection or unconfirmed cleanup leaves the previous context unchanged.

`disconnect` can remove a malformed or oversized context only when the directory and file destination still satisfy the path and permission checks. Repeating `disconnect` with no context succeeds.

### Cookie-file requirements

On Unix, `--cookie-file` accepts only a regular file with no group or other permission bits. One trailing LF or CRLF is removed. The remaining cookie must contain 1 to 255 printable ASCII bytes. The environment-variable form applies the same content validation.

## OTP constraints

The repository's current CI builds and tests controllers on OTP 26, 27, 28, and 29. JSON is the only output encoding with a stricter controller requirement: OTP 27 or newer.

Command diagnostics also require the exact target bundle/protocol handshake above. Individual target probes can report unavailable capabilities when an OTP API or subsystem is absent. The CI controller matrix does not by itself guarantee every controller/target OTP cross-version pair; treat an untested pair as unverified rather than inferred compatible.
