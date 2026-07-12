# Extend the TUI

Add a plugin page when you need a small target-specific table in the
interactive TUI. Add a formatter when process messages, dictionaries, or state
need domain-specific rendering.

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

The callback contracts are:

- `attributes/1` returns `#{rows => AttributeRows, state => NewState}`. Each
  cell needs `content` and a positive `width`; `color` is optional.
- `sheet_header/0` returns unique atom column IDs and a `default_sort` that
  names one of them. A column `shortcut` is optional.
- `sheet_body/1` returns `#{rows => Rows, state => NewState}`. Each row needs a
  `cells` map keyed by the declared column IDs.

Observer CLI passes each callback's returned state back to that callback on the
next refresh. Missing sheet cells render empty; undeclared cells are ignored.

### 2. Register the plugin

Add it to the `observer_cli` application environment, normally in the target
release's `sys.config`:

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

For a remote TUI, package the plugin module in the target release. The TUI's
automatic core-module loading does not discover arbitrary plugin applications.

### 3. Open and verify the page

Start the TUI, press `P` for **Plugin**, then press the configured `R` shortcut.
You can also start directly in plugin mode from an Erlang shell:

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

When a selected row has a PID handle and the plugin has no custom handler,
Observer CLI opens the built-in Process detail view. Omit `handle` from rows
that should not be selectable.

Custom handlers participate in the internal TUI page lifecycle rather than a
standalone public behavior. Their dispatch signature is recorded in the
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

The callback must return a character list. It is used for the process Messages,
Dictionary, and State views. Keep it total for every Erlang term; if it raises,
Observer CLI falls back to `observer_cli_formatter_default`.

### 2. Configure the formatter application and module

```erlang
{observer_cli, [
    {formatter, #{
        application => my_formatter_app,
        mod => my_observer_formatter
    }}
]}.
```

Both keys are required for the remote TUI path. `application` tells Observer CLI
which application's modules and dependencies to load on the target; `mod` is
the module that implements `format/2`.

Include `my_formatter_app` in the release and open a process's Messages,
Dictionary, and State views to verify normal terms, Unicode, and large nested
terms.

## Migrate a 1.x plugin to 2.0

Update callback return values before changing optional configuration:

| 1.x | 2.0 |
| --- | --- |
| `attributes/1 -> {Rows, State}` | `#{rows => Rows, state => State}` |
| header list with positional columns | `#{columns => [#{id => Id, ...}], default_sort => Id}` |
| `sheet_body/1 -> {RowLists, State}` | `#{rows => [#{cells => #{Id => Value}}], state => State}` |
| implicit PID selection | explicit `handle => Pid` |
| `sort_column => N` | `sort => ColumnId` |
| `handler => {Filter, Module}` | `handler => Module` plus explicit row handles |

Observer CLI can translate `sort_column` and the old handler tuple at startup,
but it rejects legacy callback shapes with `plugin_api_error`. Migrate the
callbacks rather than adding an adapter layer.
