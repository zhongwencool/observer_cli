# How to write your own plugin?

Observer CLI exposes a small behaviour (`observer_cli_plugin`) that lets you present custom metrics alongside the built-in views. The 2.0 plugin API is explicit: headers declare stable column ids, rows return maps keyed by those ids, and row drill-down uses an explicit `handle` value.

Maintaining a 1.x plugin? Start with [1.x to 2.0 migration](#4-1-x-to-2-0-migration), then use the callback examples below as the target shape.

## 1. Register the plugin module

Add a `plugins` entry to the Observer CLI environment (for example in `mix.exs` or `observer_cli.app.src`):

```erlang
{plugins,
 [
  #{module => observer_cli_plug_behaviour_x,
    title => "XPlug",
    shortcut => "X",
    interval => 1600,
    sort => memory},
  #{module => observer_cli_plug_behaviour_y,
    title => "YPlug",
    shortcut => "Y",
    interval => 2000,
    sort => reductions,
    handler => observer_cli_plug_item_behaviour_y}
 ]}.
```

**Option reference**

- `module` - module implementing the behaviour (required).
- `title` - label rendered in the menu bar (required).
- `shortcut` - single key used to jump to the plugin (required).
- `interval` - refresh rate in milliseconds (optional, defaults to `1500`).
- `sort` - column id used when sorting the sheet (optional, defaults to `default_sort` from `sheet_header/0`).
- `handler` - module used for custom row handling (optional, see [Custom handlers](#3-custom-handlers)).

The default entry point is still the `HOME` view (`observer_cli:start()`). To boot straight into plugin mode expose a shim:

```erlang
-module(your_cli).

start() ->
    observer_cli:start_plugin().
```

## 2. Implement `observer_cli_plugin`

The behaviour defines three callbacks.

### `attributes/1`

```erlang
-callback attributes(PrevState) -> #{rows := Rows, state := NewState} when
    Rows :: [[attr_cell()]],
    NewState :: any().
```

This callback drives the banner directly under the menu. The structure is a list of rows; each row is a list of maps describing individual cells.

```erlang
attributes(PrevState) ->
    Attrs = [
        [
            #{content => "XXX ETS Size", width => 15},
            #{content => 122, width => 10},
            #{content => "Memory Capacity", width => 16},
            #{content => {percent, 0.12}, width => 10},
            #{content => "XYZ1 Process Mem", width => 20},
            #{content => {byte, 1023 * 1203}, width => 14}
        ],
        [
            #{content => "YYY ETS Size", width => 15},
            #{content => 43, width => 10},
            #{content => "Disk Capacity", width => 15},
            #{content => {percent, 0.23}, width => 10},
            #{content => "XYZ2 Process Mem", width => 20},
            #{content => {byte, 2034 * 220}, width => 14}
        ]
    ],
    #{rows => Attrs, state => PrevState}.
```

Rendered banner:

```
|Home(H)|XPlug(X)|YPlug(Y)| | 0Days 3:34:50 |
|XXX ETS Size | 122 | Memory Capacity | 12.00% | XYZ1 Process Mem | 1.1737 MB |
|YYY ETS Size | 43  | Disk Capacity   | 23.00% | XYZ2 Process Mem | 436.9922 KB |
```

### `sheet_header/0`

```erlang
-callback sheet_header() -> #{columns := Columns, default_sort := ColumnId} when
    Columns :: [#{id := atom(), title := string(), width := pos_integer()}],
    ColumnId :: atom().
```

Defines the tabular columns shown underneath the banner. Each column needs a stable `id`; shortcuts let the user sort the sheet by pressing the letter. `default_sort` must be one of the declared column ids.

```erlang
sheet_header() ->
    #{
        columns => [
            #{id => pid, title => "Pid", width => 15},
            #{id => register, title => "Register", width => 20},
            #{id => memory, title => "Memory", width => 20, shortcut => "S"},
            #{id => reductions, title => "Reductions", width => 23, shortcut => "R"},
            #{id => message_queue_len, title => "Message Queue Len", width => 23, shortcut => "Q"}
        ],
        default_sort => memory
    }.
```

Result:

```
|No |Pid        |Register           |Memory(S) |Reductions(R) |Message Queue Len(Q) |
```

### `sheet_body/1`

```erlang
-callback sheet_body(PrevState) -> #{rows := Rows, state := NewState} when
    Rows :: [#{cells := #{atom() => term()}, handle => term()}],
    NewState :: any().
```

Return the table rows. `cells` is displayed in the order declared by `sheet_header/0`; missing cells render as empty and extra cells are ignored. `handle` is optional, but it is required when the row should open a detail view.

```erlang
sheet_body(PrevState) ->
    Rows = [
        begin
            Register =
                case erlang:process_info(Pid, registered_name) of
                    [] -> [];
                    {_, Name} -> Name
                end,
            #{
                cells => #{
                    pid => Pid,
                    register => Register,
                    memory => {byte, element(2, erlang:process_info(Pid, memory))},
                    reductions => element(2, erlang:process_info(Pid, reductions)),
                    message_queue_len => element(2, erlang:process_info(Pid, message_queue_len))
                },
                handle => Pid
            }
        end
     || Pid <- erlang:processes()
    ],
    #{rows => Rows, state => PrevState}.
```

Rendered sample:

```
|No |Pid       |Register           |Memory(S) |Reductions(R) |Message Queue Len(Q) |
|1  |<0.242.0> |                   |4.5020 MB | 26544288     | 0 |
|2  |<0.206.0> |                   |1.2824 MB | 13357885     | 0 |
|3  |<0.10.0>  |erl_prim_loader    |1.0634 MB | 10046775     | 0 |
...
|refresh: 1600ms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current page is 1 |
```

### Formatting helpers

- `{byte, Value}` automatically renders human-readable byte units.
- `{percent, Value}` outputs a percentage with two decimals.
- `color` can be any ANSI color escape (for example `?RED_BG`) to highlight critical cells.

## 3. Custom handlers

By default, selecting a row with `handle => Pid` opens the standard `observer_cli_process` view. To open a custom detail view, set `handler` to a module in the plugin definition and put the selected value in each row's `handle`.

```erlang
{plugins,
 [
  #{module => observer_cli_plug_behaviour_x,
    title => "XPlug",
    shortcut => "X",
    interval => 1600,
    sort => memory,
    handler => observer_cli_plug_item_behaviour_x}
 ]}.
```

When the selected row contains `#{handle := Handle}`, Observer CLI calls `HandlerModule:start(plugin, Handle, ViewOpts)` with the same contract as `observer_cli_process:start/3`.

Use this when a row selection should drill into a custom detail view (for example, ETS metadata or OS metrics). If the row should not be selectable, omit `handle`.

## 4. 1.x to 2.0 migration

2.0 keeps plugin registration and normal plugin-page navigation, but plugin callback return values now use maps with explicit keys. Migrate old plugins as follows:

| 1.x shape | 2.0 shape |
| --- | --- |
| `attributes/1 -> {Rows, State}` | `attributes/1 -> #{rows => Rows, state => State}` |
| `sheet_header/0 -> [#{title => Title, width => Width, shortcut => Shortcut}]` | `sheet_header/0 -> #{columns => [#{id => Id, title => Title, width => Width, shortcut => Shortcut}], default_sort => Id}` |
| `sheet_body/1 -> {RowLists, State}` | `sheet_body/1 -> #{rows => [#{cells => #{ColumnId => Value}, handle => Handle}], state => State}` |
| config `sort_column => N` | config `sort => ColumnId`, or omit it and use `default_sort` |
| config `handler => {PredicateFun, HandlerModule}` | config `handler => HandlerModule`, with the selected value in row `handle` |
| implicit first-`pid` drill-down | explicit `handle => Pid` for process drill-down |

Minimal callback migration example:

```erlang
%% 1.x
sheet_header() ->
    [#{title => "Name", width => 12, shortcut => "N"},
     #{title => "Value", width => 8, shortcut => "V"}].

sheet_body(State) ->
    {[["alpha", 1], ["beta", 2]], State}.

%% 2.0
sheet_header() ->
    #{
        columns => [
            #{id => name, title => "Name", width => 12, shortcut => "N"},
            #{id => value, title => "Value", width => 8, shortcut => "V"}
        ],
        default_sort => value
    }.

sheet_body(State) ->
    #{
        rows => [
            #{cells => #{name => "alpha", value => 1}},
            #{cells => #{name => "beta", value => 2}}
        ],
        state => State
    }.
```

For process drill-down, add `handle => Pid` to the row and leave `handler` unset. For custom drill-down, add `handler => HandlerModule` to the plugin config and set `handle` to the term your handler expects.

During startup, Observer CLI still migrates `sort_column => N` to the matching column id and accepts old handler tuples by keeping the handler module. Legacy callback return shapes are rejected with `error({plugin_api_error, #{source => Source, reason => Reason}})` so plugin authors see the API mismatch early.

## 5. Example plugin

[`os_stats`](https://github.com/zhongwencool/os_stats) shows a complete implementation that surfaces Linux kernel information, load averages, disk usage, memory, CPU, and IO statistics via the same behaviour. Use it as inspiration for structuring larger dashboards.
