# How to write your own plugin?

If you need to customize some of your internal metrics and integrate it into observer_ci,
you only need to write a `observer_cli_plugin` behaviour in a few simple steps to get a nice presentation.

1. Configure observer_cli，tell observer_cli how to find your plugin.

```erlang
%% module       - Specific module implements plugin behavior. It's mandatory.
%% title        - Menu title. It's mandatory.
%% shortcut     - Switch plugin by shortcut. It's mandatory.
%% interval     - Refresh interval ms. It's optional. default is 1500ms.
%% sort_column  - Sort the sheet by this index. It's optional default is 2.

{plugins,
  [
    #{module => observer_cli_plug_behaviour_x, title => "XPlug",
      interval => 1600, shortcut => "X", sort_column => 3},
    #{module => observer_cli_plug_behaviour_y, title => "YPlug",
      interval =>2000, shortcut => "Y", sort_column => 3}
  ]
}
```

The main view is `HOME` by default(`observer_cli:start()`).
If you want to plugin view as main view, DO:`your_cli:start().`

```erlang
% your_cli.erl
start() -> observer_cli:start_plugin().
```

2. Write observer_cli_plugin behaviour.
   observer_cli_plugin has 3 callbacks.

2.1 attributes.

```erlang
-callback attributes(PrevState) -> {[Rows], NewState} when
    Rows :: #{content => string()|integer()|{byte, pos_integer()},
              width => pos_integer(), color => binary()}.
```

for example:

```erlang
attributes(PrevState) ->
  Attrs = [
    [
      #{content => "XXX Ets Size", width => 15},
      #{content => 122, width => 10},
      #{content => "Memory Capcity", width => 15},
      #{content => {percent, 0.12}, width => 16},
      #{content => "XYZ1 Process Mem", width => 19},
      #{content => {byte, 1023 * 1203}, width => 19}
    ],
    [
      #{content => "YYY Ets Size", width => 15},
      #{content => 43, width => 10},
      #{content => "Disk Capcity", width => 15},
      #{content => {percent, 0.23}, width => 16},
      #{content => "XYZ2 Process Mem", width => 19},
      #{content => {byte, 2034 * 220}, width => 19}
    ],
    [
      #{content => "ZZZ Ets Size", width => 15},
      #{content => 108, width => 10},
      #{content => "Volume Capcity", width => 15},
      #{content => {percent, 0.101}, width => 16},
      #{content => "XYZ3 Process Mem", width => 19},
      #{content => {byte, 12823}, width => 19}
    ]
  ],
  NewState = PrevState,
  {Attrs, NewState}.
```

```markdown
|Home(H)|XPlug(X)|YPlug(Y)| | 0Days 3:34:50 |
|XXX Ets Size | 122 | Memory Capcity | 12.00% | XYZ1 Process Mem | 1.1737 MB |
|YYY Ets Size | 43 | Disk Capcity | 23.00% | XYZ2 Process Mem | 436.9922 KB |
|ZZZ Ets Size | 108 | Volume Capcity | 10.10% | XYZ3 Process Mem | 12.5225 KB |
```

```erlang
-callback sheet_header() -> [SheetHeader] when
    SheetHeader :: #{title => string(), width => pos_integer(), shortcut => string()}.
```

for example:

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

```markdown
|No |Pid |Register |Memory(S) |Reductions(R) |Message Queue Len(Q) |
```

```erlang
-callback sheet_body(PrevState) -> {[SheetBody], NewState} when
    PrevState :: any(),
    SheetBody :: list(),
    NewState :: any().

```

for example:

```erlang
sheet_body(PrevState) ->
  Body = [
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
  NewState = PrevState,
  {Body, NewState}.
```

Support `{byte, 1024*10}` to ` 10.0000 KB`; `{percent, 0.12}` to `12.00%`.

```markdown
|No |Pid |Register |Memory(S) |Reductions(R) |Message Queue Len(Q) |
|1 |<0.242.0> | | 4.5020 MB | 26544288 | 0 |
|2 | <0.206.0> | | 1.2824 MB | 13357885 | 0 |
|3 | <0.10.0> | erl_prim_loader | 1.0634 MB | 10046775 | 0 |
|4 | <0.434.0> | | 419.1719 KB | 10503690 | 0 |
|5 | <0.44.0> | application_contro | 416.6250 KB | 153598 | 0 |
|6 | <0.50.0> | code_server | 416.4219 KB | 301045 | 0 |
|7 | <0.9.0> | rebar_agent | 136.7031 KB | 1337603 | 0 |
|8 | <0.207.0> | | 99.3125 KB | 9629 | 0 |
|9 | <0.58.0> | file_server_2 | 41.3359 KB | 34303 | 0 |
|10 | <0.209.0> | | 27.3438 KB | 31210 | 0 |
|11 | <0.0.0> | init | 25.8516 KB | 8485 | 0 |
|refresh: 1600ms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is 1 |
```

Support F/B to page up/down.

[A more specific plugin](https://github.com/zhongwencool/os_stats) can collect linux system information such as kernel vsn, loadavg, disk, memory usage, cpu utilization, IO statistics.