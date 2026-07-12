# Configuration reference

`observer_cli` application configuration affects only the interactive TUI.
Inspection and diagnostic commands use command-line options.

## TUI start API

The `observer_cli` module provides the following start functions.

### `observer_cli:start/0`

```erlang
observer_cli:start().
```

Starts the TUI against the current node. Local defaults are `1500ms` for most views and `2000ms` for the System and ETS views.

### `observer_cli:start/1`

```erlang
observer_cli:start(Node).
observer_cli:start(RefreshMs).
```

| Argument | Behavior |
| --- | --- |
| Node atom | Uses a hidden connection from the already distributed calling node and starts the TUI on `Node` with a `1500ms` refresh interval |
| Integer at least `1000` | Starts the current-node TUI and assigns that interval to every view |

If `Node` is the current node, the local TUI is started directly. Direct remote starts require compatible observer_cli modules on the target; the generated escript's `tui` command performs its own remote-loading step before calling this API.

### `observer_cli:start/2`

```erlang
observer_cli:start(Node, Cookie).
observer_cli:start(Node, Options).
```

`Node` and `Cookie` are atoms. The proplist form accepts:

| Key | Value | Default |
| --- | --- | --- |
| `cookie` | Cookie atom | Do not change the target-specific cookie |
| `interval` | Integer at least `1000` | `1500` |

For a remote `Node`, the cookie is installed with
`erlang:set_cookie(Node, Cookie)` before the hidden connection is attempted. If
`Node =:= node()`, `start/2` starts the local TUI directly and ignores its second
argument.

### `observer_cli:start_plugin/0`

```erlang
observer_cli:start_plugin().
```

Ensures the `observer_cli` application is started and opens the configured plugin view on the current node.

## Application environment

The TUI reads four `observer_cli` application environment keys.

| Key | Accepted value | Default | Used by |
| --- | --- | --- | --- |
| `scheduler_usage` | `enable` or `disable` | `disable` | Initial Home scheduler-wall-time display |
| `default_row_size` | Positive integer | `30` | Row count when terminal height cannot be read |
| `formatter` | Formatter map | Default formatter map | Process messages, dictionary, and state views |
| `plugins` | List of plugin configuration maps | `[]` | Plugin view |

Example `sys.config` fragment:

```erlang
[
  {observer_cli, [
    {scheduler_usage, disable},
    {default_row_size, 30},
    {formatter, #{
      application => my_formatter,
      mod => my_formatter
    }},
    {plugins, [
      #{
        module => my_observer_plugin,
        title => "Queues",
        shortcut => "Q"
      }
    ]}
  ]}
].
```

The TUI escript copies the local `observer_cli` application environment when it
loads the TUI bundle remotely.

## Formatter contract

The configured formatter map has two required keys:

```erlang
#{
  application => Application,
  mod => Module
}
```

- `application` identifies the OTP application whose modules and non-core
  dependencies must be available. The TUI escript uses it when loading remote
  formatter code.
- `mod` implements `observer_cli_formatter` and exports `format/2`.

The callback is:

```erlang
-callback format(Pid :: pid(), Term :: term()) -> string().
```

It must return a Unicode character list for the built-in pager. Its arguments
are the inspected process PID and a term from the message list, process
dictionary, or `gen_server` state.

The default is:

```erlang
#{
  application => observer_cli,
  mod => observer_cli_formatter_default
}
```

It renders a `Process:` heading followed by Erlang term syntax. If a custom
formatter raises, exits, or throws, observer_cli falls back to
`observer_cli_formatter_default` for that value.

## Plugin configuration

`plugins` is an ordered list. Each map requires:

| Key | Value | Purpose |
| --- | --- | --- |
| `module` | Module atom | Implements the plugin callbacks |
| `title` | String | Menu label |
| `shortcut` | String | Exact input used to select this plugin |

Optional keys are:

| Key | Value | Default |
| --- | --- | --- |
| `interval` | Integer refresh interval | `1500` |
| `sort` | Column ID atom | `default_sort` from `sheet_header/0` |
| `handler` | Module atom | Process detail for PID handles; otherwise no custom handler |

observer_cli maintains the page, selected row, and computed sheet width.

### Callback behavior

A plugin module implements `observer_cli_plugin`:

```erlang
-behaviour(observer_cli_plugin).
```

#### `attributes/1`

```erlang
attributes(PreviousState) -> #{
  rows => AttributeRows,
  state => NextState
}.
```

The first `PreviousState` is `undefined`; later calls receive the preceding `state`. Each row is a list of cells:

```erlang
#{
  content => Content,
  width => PositiveInteger,
  color => AnsiBinary             % optional
}
```

`content` may be a string, integer, `{byte, Bytes}`, or `{percent, Fraction}`.
If `attributes/1` is undefined, the view renders no attributes.

#### `sheet_header/0`

```erlang
sheet_header() -> #{
  columns => [
    #{
      id => ColumnId,
      title => "Title",
      width => PositiveInteger,
      shortcut => "T"             % optional
    }
  ],
  default_sort => ColumnId
}.
```

Column IDs must be unique atoms. `default_sort` and configured `sort` must identify one of those columns. A column shortcut changes the active sort key.

#### `sheet_body/1`

```erlang
sheet_body(PreviousState) -> #{
  rows => [
    #{
      cells => #{ColumnId => Value},
      handle => Selection          % optional
    }
  ],
  state => NextState
}.
```

Missing cells render as empty strings. Rows are ordered by the active column using Erlang term ordering before pagination.

For a selected row with a `handle`:

- with `handler => Module`, observer_cli calls
  `Module:start(plugin, Selection, ViewOpts)`;
- a PID handle without a configured handler opens the built-in process detail
  view;

A row without `handle` has no selection action.

### Plugin-view input

| Input | Action |
| --- | --- |
| Configured plugin shortcut | Select plugin |
| Configured column shortcut | Change sort column |
| Positive row number below `1000` | Select that visible indexed row |
| Enter | Select the remembered row |
| `F`, `B` | Next or previous page |
| Integer at least `1000` | Change the active plugin refresh interval |
| `H` | Return to Home |
| `q` | Quit |

Invalid plugin callback shapes raise `{plugin_api_error, Details}`. The details identify the source (`attributes`, `sheet_header`, `sheet_body`, `config`, or `row_handler`) and a reason atom.
