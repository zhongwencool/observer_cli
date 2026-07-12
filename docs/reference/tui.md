# TUI reference

The terminal UI is a line-oriented live view of one BEAM node. Type a shortcut and press Enter. An empty line means Enter by itself.

## Start forms

From the generated escript:

```text
observer_cli tui NODE [COOKIE REFRESH_MS]
```

`REFRESH_MS` defaults to `1500` and must be at least `1000`. If a cookie is supplied, the refresh value is also required. The escript starts a hidden controller, loads a compatible TUI bundle into the target when needed, and starts the UI there.

From an Erlang shell:

```erlang
observer_cli:start().
observer_cli:start(Node).
observer_cli:start(Node, Cookie).
observer_cli:start(Node, [{cookie, Cookie}, {interval, 2000}]).
```

See [Configuration reference](configuration.md#tui-start-api) for exact API behavior.

## Main pages

The main menu exposes these pages:

| Page | Key | Contents |
| --- | --- | --- |
| Home | `H` | VM summary, memory and GC deltas, optional scheduler utilization, and ranked processes |
| Network | `N` | VM I/O totals and legacy `inet` Port counters |
| Ports | `O` | Non-`inet` Erlang Port metadata and counters |
| Sockets | `K` | OTP `socket` registry summary, counters, and socket details |
| System | `S` | System/architecture facts, allocators, cache rates, and distribution state |
| ETS | `E` | ETS table metadata |
| Mnesia | `M` | Local Mnesia table metadata; the menu entry appears only while the schema table exists |
| App | `A` | Process resources grouped by application |
| Doc | `D` | Built-in shortcut summary |
| Plugin | `P` | Configured plugin sheets |

The Home, Network, Ports, Sockets, ETS, Mnesia, App, and Plugin lists use
terminal height to choose the visible row count. If terminal geometry is
unavailable, `default_row_size` defaults to 30 rows.

## Shared input

These inputs are parsed on the built-in non-plugin main pages:

| Input | Action |
| --- | --- |
| `H`, `N`, `O`, `K`, `S`, `E`, `M`, `A`, `D`, `P` | Switch main page |
| `q` or `Q` | Quit the TUI |
| `F`, `pd`, or `PD` | Next page |
| `B`, `pu`, or `PU` | Previous page; page 1 is the lower bound |
| Integer at least `1000` | Set the active page's refresh interval in milliseconds |
| Positive integer below `1000` | Select that row on pages with row drill-down; otherwise ignored |
| Enter | Open the remembered row on Home and Network; otherwise page-specific |

A refresh-interval change is retained in that page's state while navigating. It does not rewrite every page's interval.

## Home

The Home ranking has two collection modes:

- count mode calls `recon:proc_count/2` once per refresh;
- window mode calls `recon:proc_window/3` for the configured interval.

| Input | Ranking |
| --- | --- |
| `r` / `rr` | Reductions, count/window |
| `m` / `mm` | Memory, count/window |
| `b` / `bb` | Referenced binary memory, count/window |
| `t` / `tt` | Total heap size, count/window |
| `mq` / `mmq` | Message queue length, count/window |
| `p` | Pause or resume Home redraw |
| `` ` `` | Enable or disable scheduler wall-time utilization rows |
| Row number | Open that ranked process |
| Enter | Open the remembered ranked process |
| Full PID, for example `<0.43.0>` | Open a local process even if it is not ranked |
| `<431` or `>431` | Open `<0.431.0>` |

Enabling scheduler utilization changes the node-wide `scheduler_wall_time` system flag while the view is active. observer_cli restores the preceding setting when it cleans up the Home view.

## Network

Network has count and interval-window modes:

| Input | Action |
| --- | --- |
| `ic` | Use `recon:inet_count/2` |
| `iw` | Use `recon:inet_window/3` with the page interval |
| `rc` | Sort by received packet count |
| `ro` | Sort by received octets |
| `sc` | Sort by sent packet count |
| `so` | Sort by sent octets |
| `cnt` | Sort by total packet count |
| `oct` | Sort by total octets |
| Row number or Enter | Open the corresponding Port detail |

The page also displays VM input/output deltas from `erlang:statistics(io)`. Its ranked rows cover legacy `inet` Ports, not every host connection and not the OTP `socket` registry.

## Ports

The Ports page excludes `inet` Ports already represented on Network.

| Input | Action |
| --- | --- |
| `qs` | Sort by queue size |
| `m` | Sort by memory |
| Row number | Open Port detail |

Port detail accepts `H` for Home, `N` for Network, `O` for the Ports list, `P` for the current Port detail, and an integer at least `1000` to change its refresh interval.

## Sockets

The Sockets page reads the OTP socket registry. If the required socket API is unavailable, it displays an unavailable state.

| Input | Sort key |
| --- | --- |
| `io` | Read plus write bytes |
| `rb` | Read bytes |
| `wb` | Write bytes |
| `pk` | Packets |
| `wt` | Waits |
| `fl` | Failures |
| `mx` | Maximum packet value |
| `ac` | Accept activity |
| `id` | Socket ID |
| `fd` | File descriptor |
| `ow` | Owner |
| `dm` | Domain |
| `tp` | Type |
| `pt` | Protocol |

A row number opens socket detail. `K` returns to the Sockets list. Counter-oriented sorting uses deltas after the first refresh; identity-oriented sorting uses current metadata.

## ETS and Mnesia

Both table pages accept:

| Input | Action |
| --- | --- |
| `m` | Sort by memory |
| `s` | Sort by object count |
| `F` / `B` or `pd` / `pu` | Change page |

Mnesia also accepts `hide` to toggle system tables. These views report metadata only; they do not read table contents.

## App

The App page aggregates process resources by application.

| Input | Sort key |
| --- | --- |
| `p` | Process count |
| `m` | Memory |
| `r` | Reductions |
| `mq` | Message queue length |
| `F` / `B` | Next or previous page |

## Process detail

Process detail is reached from Home or from a plugin PID row.

| View | Key | Contents |
| --- | --- | --- |
| Process Info | `P` | Metadata, signals, memory, and reduction history |
| Messages | `M` | Message list when queue length is at most 10,000 |
| Dictionary | `D` | Process dictionary |
| Current Stack | `C` | Up to 30 current stack frames |
| State | `S` | `recon:get_state(Pid, 2500)` rendered through the configured formatter |

`H` returns Home. When opened from a plugin, `B` returns to the plugin sheet. An integer at least `1000` changes the detail refresh interval. State is a static capture and uses the built-in pager.

Pager input is:

| Input | Action |
| --- | --- |
| `F` or `j` | Next page |
| `B` or `k` | Previous page |
| `q` or `Q` | Quit the TUI from the process-state pager |

When `B` is reserved for returning to a plugin, use `k` for the previous pager page.

## Plugin page

The Plugin page menu and column shortcuts come from application configuration. Built-in input is:

| Input | Action |
| --- | --- |
| Plugin shortcut | Select a plugin |
| Column shortcut | Sort by that column |
| Row number | Select that row |
| Enter | Open the remembered row |
| `F` / `B` | Next or previous page |
| Integer at least `1000` | Change plugin refresh interval |
| `H` | Return Home |
| `q` | Quit |

See [Configuration reference](configuration.md#plugin-configuration) for callback and row-handler contracts.

## Redraw behavior

Main views clear the screen once, then redraw from the cursor origin to avoid accumulating output. Each view schedules its next refresh after completing the current collection. Window modes spend the configured interval sampling before the next render, so their wall-clock cycle also includes collection and rendering time.

Home pause stops Home collection until resumed. Process state does not auto-refresh. Resource disappearance during a refresh is represented as a dead, missing, or unavailable view rather than retaining the prior detail indefinitely.
