# observer_cli

**Production-ready BEAM diagnostics for operators, automation, and AI agents.**

`observer_cli` is an LLM-friendly command-line tool for inspecting live Erlang
and Elixir systems. Its command-first CLI provides bounded operations, explicit
exit codes, and stable text, Erlang-term, and JSON output, so people, scripts,
and AI agents can use the same production diagnostics interface. An interactive
TUI remains available for exploratory work.

The project is built on Erlang/OTP and [`recon`](https://hex.pm/packages/recon).
Version 2.0 provides two explicit interfaces:

| Interface | Best for | What it provides |
| --- | --- | --- |
| **Command CLI — featured** | Production diagnostics, automation, and LLM workflows | Bounded commands with structured output; requires the matching diagnostics bundle on the target |
| Interactive TUI | Exploring a node and drilling into live runtime views | A live terminal interface that can load the matching `observer_cli` bundle on the target |

> **Trust boundary**
>
> Both interfaces use Erlang distribution. Run them only against trusted nodes
> over a trusted network. Read [Safety and observer effect](docs/explanation/safety-and-observer-effect.md)
> before using scans, state inspection, or tracing in production.

## Start here

Choose the path that matches your goal:

- Follow [Your first diagnosis](docs/tutorials/first-diagnosis.md) to build the
  standalone command and inspect a disposable node.
- Follow [Your first TUI session](docs/tutorials/first-tui-session.md) to learn
  the interactive views and navigation.
- Use [Install and build](docs/how-to/install-and-build.md) when adding
  `observer_cli` to an Erlang or Elixir release.
- Open the [CLI reference](docs/reference/cli.md) or
  [TUI reference](docs/reference/tui.md) when you already know what you need.

To discover the command interface from a source checkout:

```sh
rebar3 escriptize
./_build/default/bin/observer_cli --help
./_build/default/bin/observer_cli diagnose --help
```

The command interface uses an explicit target and cookie source:

```sh
export OBSERVER_CLI_COOKIE='replace-me'

./_build/default/bin/observer_cli connect \
  --node app@host \
  --cookie-env OBSERVER_CLI_COOKIE
./_build/default/bin/observer_cli status
./_build/default/bin/observer_cli diagnose
./_build/default/bin/observer_cli disconnect
```

`connect` saves target metadata, not the cookie and not a persistent connection.
Every later command starts a temporary hidden controller, probes the target, and
stops the controller before returning. For stateless automation, pass `--node`
and the cookie source on every command instead.

Start the interactive interface explicitly with `tui`:

```sh
./_build/default/bin/observer_cli tui app@host replace-me 1500
```

The positional cookie is visible in process arguments and shell history. Prefer
starting from an Erlang shell that already has the correct cookie, or use this
form only in a controlled environment. The old bare
`observer_cli NODE [COOKIE REFRESH_MS]` escript syntax is not supported in 2.0.

> **Screenshot TODO — `docs/images/tui-home.png`**
>
> Capture the full Home view from a terminal at least 150 columns wide and 30
> rows high. It must show the selected Home tab, target and OTP information,
> system and memory summaries, named process rows, and the footer. Use the
> reproducible one-paste scene under **Capture the current TUI Home
> screenshot** in [Contribute a change](docs/how-to/contribute.md#9-capture-the-current-tui-home-screenshot).

## What you can inspect

The command interface covers:

- VM memory, allocators, schedulers, distribution, and network counters;
- top processes and bounded per-process metadata;
- applications, ETS tables, Mnesia tables, Erlang ports, and OTP sockets;
- bounded supervision trees and behavior-aware OTP state shapes;
- quick, sampled, deep, and application-scoped diagnostics;
- one exact, bounded function trace with explicit node-global acknowledgement.

Snapshot and diagnosis commands redact identifiers by default. Inspection and
trace commands include identifiers by default and accept `--redact`. Text is for
operators; consultable Erlang terms and JSON use the versioned
`observer_cli.cli/v1` envelope. JSON requires an OTP 27 or newer controller.

## Documentation

The ExDoc site uses this README as its home page and organizes the remaining
material by purpose:

| Section | Use it when you want to... |
| --- | --- |
| [Tutorials](docs/tutorials/first-diagnosis.md) | learn through a complete first session |
| [How-to guides](docs/how-to/install-and-build.md) | complete a specific operational or development task |
| [Reference](docs/reference/cli.md) | look up commands, keys, configuration, or output contracts |
| [Explanation](docs/explanation/execution-model.md) | understand execution, diagnostics, and safety decisions |
| [Changelog](docs/CHANGELOG.md) | review behavior changes by release |

### Use the documentation with an LLM

The generated ExDoc site includes an `llms.txt` index whose links point to
Markdown versions of every documentation page. After the updated documentation
is published, HexDocs serves it at
[`/observer_cli/llms.txt`](https://hexdocs.pm/observer_cli/llms.txt). Give that
URL to an LLM or agent so it can discover the relevant pages without parsing
the HTML site. To provide one page as context, open it in ExDoc and use **Copy
Markdown**.

From a source checkout, generate the same files locally:

```sh
rebar3 docs
cat doc/llms.txt
```

The `docs` alias runs `rebar3_ex_doc` with the HTML, Markdown, and EPUB
formatters. Local Markdown pages are written beside the HTML pages under
`doc/`. When preparing a HexDocs upload, build with `rebar3 docs` first and
pass that directory to `rebar3_hex` with `--doc-dir doc`; otherwise
`rebar3_ex_doc`'s default provider invocation generates only HTML and EPUB.

## Compatibility and verification

The repository CI compiles, checks, and tests the generated escript on OTP 26,
27, 28, and 29. This is a controller build matrix, not proof of every possible
controller/target version pair. Command diagnostics require target capabilities
matching bundle `2.0.0` and protocol `1`.

See [Output contract and compatibility](docs/reference/output-contract.md) for
the exact format and runtime boundaries.

## License

`observer_cli` is released under the [MIT License](LICENSE).
