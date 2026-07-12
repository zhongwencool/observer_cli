# Extend the TUI

Use a plugin page for a target-specific table. Use a formatter for
domain-specific process messages, dictionaries, or state.

Both extension types run on the node that hosts the TUI runtime. Include their
modules in that node's release or code path.

## Add a plugin page

### 1. Implement the 2.0 callbacks

Create a module in an application that depends on `observer_cli`:

```erlang
-module(my_runtime_plugin).

-behaviour(observer_cli_plugin).

-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(State) ->
    Total = erlang:memory(total),
    #{
        rows => [[
            #{content => "BEAM memory", width => 16},
            #{content => {byte, Total}, width => 16}
        ]],
        state => State
    }.

sheet_header() ->
    #{
        columns => [
            #{id => metric, title => "Metric", width => 24},
            #{id => value, title => "Value", width => 16, shortcut => "V"}
        ],
        default_sort => value
    }.

sheet_body(State) ->
    #{
        rows => [
            #{cells => #{metric => "Processes", value => erlang:system_info(process_count)}},
            #{cells => #{metric => "Ports", value => erlang:system_info(port_count)}}
        ],
        state => State
    }.
```

Callback contracts:

- `attributes/1`: `#{rows => AttributeRows, state => NewState}`. Each cell
  needs `content` and a positive `width`; `color` is optional.
- `sheet_header/0`: unique atom column IDs and a `default_sort` naming one of
  them. A column `shortcut` is optional.
- `sheet_body/1`: `#{rows => Rows, state => NewState}`. Each row needs a `cells`
  map keyed by declared column IDs.

Each callback receives its returned state on the next refresh. Missing sheet
cells render empty; undeclared cells are ignored.

### 2. Register the plugin

Add the plugin to the `observer_cli` application environment, normally in the
target release's `sys.config`:

```erlang
{observer_cli, [
    {plugins, [
        #{
            module => my_runtime_plugin,
            title => "Runtime",
            shortcut => "R",
            interval => 1500,
            sort => value
        }
    ]}
]}.
```

`module`, `title`, and `shortcut` are required. `interval` defaults to `1500`
milliseconds, and `sort` defaults to the callback's `default_sort`. Use refresh
intervals of at least `1000` milliseconds.

For a remote TUI, package the module in the target release; automatic
core-module loading does not discover plugin applications.

### 3. Open and verify the page

In the TUI, press `P` for **Plugin**, then the configured `R` shortcut. Or start
plugin mode from an Erlang shell:

```erlang
observer_cli:start_plugin().
```

Verify refresh, sorting with `V`, forward/back pagination with `F` and `B`, and
return to Home with `H`.

### 4. Add process drill-down only when needed

To open the built-in process view, put a PID in an explicit row handle:

```erlang
#{
    cells => #{metric => "Worker", value => 1},
    handle => WorkerPid
}
```

A PID handle opens the built-in Process detail view when no custom handler is
configured. Omit `handle` from non-selectable rows.

Custom handlers use the internal TUI page lifecycle, not a public behavior.
Their dispatch signature is in the
[configuration reference](../reference/configuration.md#plugin-configuration);
use one only when the built-in PID drill-down cannot represent the selected
resource.

## Add a process formatter

### 1. Implement `format/2`

```erlang
-module(my_observer_formatter).

-behaviour(observer_cli_formatter).

-export([format/2]).

format(Pid, Term) ->
    unicode:characters_to_list(
        io_lib:format("Process: ~p~n~n~tp~n", [Pid, Term])
    ).
```

Return a character list for the process Messages, Dictionary, and State views.
Handle every Erlang term; if `format/2` raises, Observer CLI falls back to
`observer_cli_formatter_default`.

### 2. Configure the formatter application and module

```erlang
{observer_cli, [
    {formatter, #{
        application => my_formatter_app,
        mod => my_observer_formatter
    }}
]}.
```

Both keys are required remotely. `application` identifies the modules and
dependencies to load on the target; `mod` implements `format/2`.

Include `my_formatter_app` in the release and open a process's Messages,
Dictionary, and State views to verify normal terms, Unicode, and large nested
terms.

## Migrate a 1.x plugin to 2.0

Update callback return values first:

| 1.x | 2.0 |
| --- | --- |
| `attributes/1 -> {Rows, State}` | `#{rows => Rows, state => State}` |
| header list with positional columns | `#{columns => [#{id => Id, ...}], default_sort => Id}` |
| `sheet_body/1 -> {RowLists, State}` | `#{rows => [#{cells => #{Id => Value}}], state => State}` |
| implicit PID selection | explicit `handle => Pid` |
| `sort_column => N` | `sort => ColumnId` |
| `handler => {Filter, Module}` | `handler => Module` plus explicit row handles |

Observer CLI translates `sort_column` and the old handler tuple at startup, but
rejects legacy callback shapes with `plugin_api_error`. Migrate the callbacks;
do not add an adapter.
