# How to write your own plugin?

Observer CLI exposes a small behaviour (`observer_cli_plugin`) that lets you present custom metrics alongside the built-in views. This guide walks through the required configuration and each callback so you can build your own panels quickly.

## 1. Register the plugin module

Add a `plugins` entry to the Observer CLI environment (for example in `mix.exs` or `observer_cli.app.src`):

```erlang
{plugins,
 [
  #{module => observer_cli_plug_behaviour_x,
    title => "XPlug",
    shortcut => "X",
    interval => 1600,
    sort_column => 3},
  #{module => observer_cli_plug_behaviour_y,
    title => "YPlug",
    shortcut => "Y",
    interval => 2000,
    sort_column => 3}
 ]}.
```

**Option reference**

- `module` - module implementing the behaviour (required).
- `title` - label rendered in the menu bar (required).
- `shortcut` - single key used to jump to the plugin (required).
- `interval` - refresh rate in milliseconds (optional, defaults to `1500`).
- `sort_column` - index used when sorting the sheet (optional, defaults to `2`).
- `handler` - tuple `{PredicateFun, Module}` for custom row handling (optional, see [Custom handlers](#4-custom-handlers)).

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
-callback attributes(PrevState) -> {[Rows], NewState} when
    Rows :: [
        #{content => string() | integer() | {byte, pos_integer()} | {percent, float()},
          width   => pos_integer(),
          color   => binary()}
    ],
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
    {Attrs, PrevState}.
```

Rendered banner:

```
|Home(H)|XPlug(X)|YPlug(Y)| | 0Days 3:34:50 |
|XXX ETS Size | 122 | Memory Capacity | 12.00% | XYZ1 Process Mem | 1.1737 MB |
|YYY ETS Size | 43  | Disk Capacity   | 23.00% | XYZ2 Process Mem | 436.9922 KB |
```

### `sheet_header/0`

```erlang
-callback sheet_header() -> [SheetHeader] when
    SheetHeader :: #{title => string(),
                     width => pos_integer(),
                     shortcut => string()}.
```

Defines the tabular columns shown underneath the banner. Shortcuts let the user sort the sheet by pressing the letter.

```erlang
sheet_header() ->
    [
        #{title => "Pid", width => 15},
        #{title => "Register", width => 20},
        #{title => "Memory", width => 20, shortcut => "S"},
        #{title => "Reductions", width => 23, shortcut => "R"},
        #{title => "Message Queue Len", width => 23, shortcut => "Q"}
    ].
```

Result:

```
|No |Pid        |Register           |Memory(S) |Reductions(R) |Message Queue Len(Q) |
```

### `sheet_body/1`

```erlang
-callback sheet_body(PrevState) -> {[SheetBody], NewState}.
```

Return the table rows. Each row is a list; Observer CLI paginates automatically (PageDown/PageUp or `F/B` keys).

```erlang
sheet_body(PrevState) ->
    Rows = [
        begin
            Register =
                case erlang:process_info(Pid, registered_name) of
                    [] -> [];
                    {_, Name} -> Name
                end,
            [
                Pid,
                Register,
                {byte, element(2, erlang:process_info(Pid, memory))},
                element(2, erlang:process_info(Pid, reductions)),
                element(2, erlang:process_info(Pid, message_queue_len))
            ]
        end
     || Pid <- erlang:processes()
    ],
    {Rows, PrevState}.
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
- `color` can be any ANSI color escape (e.g., `?RED_BG`) to highlight critical cells.

## 3. Custom handlers

By default, selecting a row in your plugin opens the standard `observer_cli_process` view for the first `pid` found in that row. To override this, add a `handler` tuple to the plugin definition. The predicate receives every element in the row and should return true for the items you need. When the predicate matches, `HandlerModule:start/3` is invoked with the same contract as `observer_cli_process:start/3`.

```erlang
{plugins,
 [
  #{module => observer_cli_plug_behaviour_x,
    title => "XPlug",
    shortcut => "X",
    interval => 1600,
    sort_column => 3,
    handler => {fun is_pid/1, observer_cli_plug_item_behaviour_x}},
  #{module => observer_cli_plug_behaviour_y,
    title => "YPlug",
    shortcut => "Y",
    interval => 2000,
    sort_column => 3,
    handler => {fun is_binary/1, observer_cli_plug_item_behaviour_y}}
 ]}.
```

Use this when a row selection should drill into a custom detail view (for example, ETS metadata or OS metrics).

## 4. Example plugin

[`os_stats`](https://github.com/zhongwencool/os_stats) shows a complete implementation that surfaces Linux kernel information, load averages, disk usage, memory, CPU, and IO statistics via the same behaviour. Use it as inspiration for structuring larger dashboards.
