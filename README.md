# observer_cli

**Bounded BEAM investigations for people and automation.**

Start with a question, inspect the evidence, and choose the next smallest
observation. People and agents use the same commands; text is concise and JSON
retains the complete measurements, coverage, and uncertainty.

> This checkout implements **3.0.0**, an unreleased breaking redesign. It is not
> evidence of a published Hex package or GitHub Release. Existing 2.0 users
> should keep their versioned binary until they migrate.

## Start an investigation

Prepare a trusted target with the matching `observer_cli` bundle first. Have
its cookie injected into a protected environment, not pasted into arguments:

```sh
export OBSERVER_CLI_NODE='app@host'
# OBSERVER_CLI_COOKIE is provided by your protected secret source.
observer_cli check
```

The default check observes for **15 seconds**. It evaluates existing VM-limit
and scheduler-pressure rules, shows memory and queue evidence, and suggests
bounded follow-up. No findings is not a certificate of health.

```sh
observer_cli check cpu --window 5s
observer_cli check memory
observer_cli check mailbox
observer_cli check connections

observer_cli inspect process --sort memory
observer_cli inspect process --sort reductions-rate --window 5s
observer_cli inspect process --pid '<0.123.0>'
observer_cli inspect process --name my_server
```

`memory` always means current bytes. `memory-change` means a signed window
change. `reductions-rate` means reductions/s over the measured interval, **not
process CPU time**. Adding a window never silently changes a sort's meaning.

There is no daemon, persistent connection, saved active target, or global
`connect`/`disconnect` step. Every command resolves one atomic selector and
cleans up its temporary hidden controller.

## The five entrypoints

| Entry | Use it for |
| --- | --- |
| `check [cpu\|memory\|mailbox\|connections]` | A bounded overview or focused investigation. |
| `inspect RESOURCE` | Current facts, rankings, or a selected process/port. |
| `trace call` / `trace stop` | Explicitly authorized node-global tracing. |
| `tui` | Continuous interactive exploration. |
| `describe [COMMAND PATH]` | Offline, incremental capability discovery. |

```sh
observer_cli --help
observer_cli inspect --help
observer_cli describe inspect process --json
observer_cli describe --full --json
observer_cli describe --schema --json
```

Inspection covers VM facts, memory/allocators, schedulers, distribution, legacy
inet traffic, processes, applications, ETS, Mnesia, Erlang ports, OTP sockets,
bounded OTP state shapes, direct supervision children, and retained configured
logs. It does not invent unsupported resource details.

## Build this unreleased version

Controllers are escripts, not standalone native executables. Erlang/OTP and
`escript` must be installed. The supported runtime range remains OTP 26–29;
JSON requires controller OTP 27+. Text and Erlang-term output work on OTP 26.

From this checkout:

```sh
rebar3 escriptize
BIN=./_build/default/bin/observer_cli
"$BIN" --version
"$BIN" --help
```

Or, with the CI toolchain family OTP 29 / Elixir 1.20:

```sh
mix deps.get
mix escript.build
./observer_cli --version
```

For a local target application during development, use this checkout as a path
dependency (Elixir) or a checkout dependency (Rebar). Include `observer_cli` and
`recon` in the target release. The target must run as a distributed node. A
3.0.0 controller requires the matching **3.0.0 target bundle, protocol 2**.
Command diagnostics never upload missing target code.

Versioned release installation instructions apply only after a 3.0.0 release
has actually been published. No release is created by these build commands.

## Explicit targets and agents

Agents and runbooks bind the target on **every remote invocation**:

```sh
observer_cli check cpu --window 5s \
  --node 'app@host' --cookie-env OBSERVER_CLI_COOKIE --json
```

Global options may appear before or after the command path. An explicit node
requires exactly one explicit cookie source and cannot borrow environment
selector components. Without explicit target options, use
`OBSERVER_CLI_NODE` plus exactly one of `OBSERVER_CLI_COOKIE` or
`OBSERVER_CLI_COOKIE_FILE`; `OBSERVER_CLI_NAME_MODE` is optional.

Structured responses use `observer_cli.cli/v2`. Execution completeness is
separate from findings. Exit `0` means complete execution, even with findings;
use `--fail-on warning|critical` for an explicit runbook finding threshold.
Preserve stdout, stderr and the exit status, especially for partial results.

Identifiers are included by default for follow-up. Use `--redact` for sharing;
redacted aliases are response-local and never executable selectors. Counts,
timing and topology can still be sensitive. Retained logs cannot be reliably
sanitized and reject redaction.

## Safety and escalation

An Erlang distribution cookie grants trusted-peer code execution, not read-only
access. Distribution is not encrypted by default. Use only trusted targets and
networks or separately secured transport.

Default checks do not read messages, dictionaries, table contents, state values,
logs or trace events. State inspection requires `--allow-state-read`; tracing
requires its specific global replacement/stop acknowledgement. Suggestions are
never executed automatically. Coordinate one active observation per target;
per-request limits do not bound aggregate concurrent load.

```sh
observer_cli tui --node 'app@host' --cookie-env OBSERVER_CLI_COOKIE
```

The TUI retains its existing pages and plugins. If remote code loading is
needed, explicitly add `--load-code` and use the same controller/target OTP
major. Positional cookie values and implicit code loading are not supported.

## Documentation

- [CLI reference](docs/reference/cli.md): paths, options, output, exits and bounds.
- [Agent workflows](docs/guides/agent-workflows.md): JSON-only follow-up and sharing.
- [Core concepts](docs/explanation/core-concepts.md): evidence, uncertainty and trust.
- [Migration](docs/guides/migrate-v3.md): v2 command and contract replacements.
- [TUI reference](docs/reference/tui.md) and [plugins](docs/reference/tui-plugins.md).
- [Public JSON Schema](priv/schema/observer_cli.cli.v2.schema.json).
