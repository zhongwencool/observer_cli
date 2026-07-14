# Changelog

Release and maintenance changes, newest first.

## 2.0.0 (release candidate)

- Added `mix escript.build` as an alternative source build for the controller,
  using the same Erlang entry point, VM flags, dependencies, and bundled schema.
- Added `observer_cli logs` for bounded retained evidence from one trusted plain
  `logger_std_h` configured file path, with strict source, file-identity, tail,
  response, and output-encoding limits.
- Kept logs separate from default snapshot, diagnosis, and TUI flows; the
  command does not flush Logger, read archives, accept arbitrary paths, or
  claim reliable redaction of arbitrary log text.
- Hardened shared text, term, and JSON output against raw terminal control and
  bidirectional-control codepoints while preserving ordinary Unicode.
- Added a command-first interface for production diagnostics, automation, and
  AI agents, covering health checks, snapshots, resource inspection, OTP state,
  and bounded call tracing.
- Added reusable remote-target workflows with `connect`, `status`, and
  `disconnect`, including target compatibility checks.
- Required command targets to contain the matching diagnostics bundle; command
  diagnostics do not upload missing code, while automatic loading remains
  TUI-only.
- Added structured text, Erlang term, and OTP 27+ JSON output with stable exit
  statuses and a versioned response schema.
- Made `tui` the explicit interactive command and removed the old positional
  launch form. Updated the TUI plugin API for 2.0; 1.x callback shapes must be
  migrated.
- Fixed CLI help, error, output-stream, and exit-status behavior, trace cleanup,
  and malformed saved-context removal.

## 1.8.8

- Adapted terminal width automatically across Home, Application, ETS, Mnesia,
  Network, Process, Port, System, and pager footers.
- Reorganized Doc-view shortcuts into clearer command groups.
- Relaxed the Elvis god-module rule for existing large renderer modules.

## 1.8.7

- Handled the unchanged result from `net_kernel:set_net_ticktime/1`.
- Fixed `rebar3 check` by updating the Elvis macro rule name.
- Simplified `observer_cli_escriptize` test shutdown.

## 1.8.6

- Added external process formatter support; the formatter callback now receives
  the process PID.
- Fixed process State redraw timers and repeated-render edge cases.
- Improved pager navigation and status output.
- Updated `net_ticktime` automatically for remote TUI sessions.
- Fixed Mix compilation and excluded test sources from release builds.
- Removed `recon` from `included_applications`.

## 1.8.5

- Synchronized default application environment between `mix.exs` and the
  application resource file.
- Refactored application statistics and command parsing.
- Updated plugin documentation and terminal images.
- Removed OTP 24 and 25 from the CI matrix.

## 1.8.4

- Fixed an OTP 28 Dialyzer warning.
- Used the OTP 28 process iterator APIs when available.

## 1.8.3

- Handled unknown Mnesia storage types without crashing.
- Fixed an OTP 27 warning.
- Added ExDoc generation.

## 1.8.2

- Corrected `fullsweep_after`: it is a count, not a byte value.

## 1.8.1

- Displayed the node name in the System view.

## 1.8.0

- Added direct PID input with `<PID` shorthand.
- Displayed labels set with `proc_lib:set_label/1`.
- Displayed distribution output-queue bytes in the System view.
- Fixed the Doc view on OTP 27.

## 1.7.5

- Handled Mnesia external-copy storage types such as `{ext, Module, State}`.
- Corrected the Memory and Reductions column order in the Application view.

## 1.7.4

- Handled ETS tables that disappear before `ets:info/1` returns.

## 1.7.3

- Prevented System-view failure when the `ps` command fails.

## 1.7.2

- Handled process monitors addressed by `{RegisteredName, Node}`.

## 1.7.1

- Displayed application lifecycle states in the Application view.
- Fixed remote RPC shutdown with `Ctrl+C`.
- Corrected the version in `mix.exs`.

## 1.7.0

- Added process-count, memory, and reductions sorting to the Application view.
- Added `{byte, Value}` and `{percent, Value}` plugin formatting.
- Added process drill-down from plugin rows.

## 1.6.2

- Prevented a crash when `ps` is unavailable on Windows.

## 1.6.1

- Removed the precise OTP-version display.

## 1.6.0

- Hid scheduler utilization by default.
- Adopted `erlfmt` formatting.
- Added BEAM OS-process CPU, memory, RSS, and VSZ data from `ps`.
- Removed the expensive `recon_alloc:memory/1` call from Home.

## 1.5.4

- Upgraded `recon` to 2.5.1 for OTP 23 allocator compatibility.

## 1.5.3

- Made `default_row_size` configurable through the application environment.
- Added current and maximum SBCS-to-MBCS allocator ratios to the System view.

## 1.5.2

- Used `erlang:system_info(otp_release)` when the `OTP_VERSION` file is absent.

## 1.5.1

- Hid the Mnesia tab when Mnesia is not running.
- Displayed the detailed ERTS version when available.

## 1.5.0

- Upgraded `recon` to 2.5.0.

## 1.4.5

- Added a minimal `mix.exs` build file.
- Ensured stale `EXIT` messages are cleared.

## 1.4.4

- Handled connection failures without crashing.

## 1.4.3

- Upgraded `recon` to 2.4.0.

## 1.4.2

- Hid the scheduler bar on systems with more than 100 cores.
- Added inet6 distribution support to the escript build.
- Renamed the plugin `kv_label/0` callback to `attributes/1`.

## 1.4.1

- Corrected ETS-view memory usage.
- Reported Mnesia memory in bytes.

## 1.4.0

- Added custom TUI plugins.

## 1.3.4

- Added paging and memory/size sorting to ETS and Mnesia views.
- Fixed pause handling.
- Made the refresh interval configurable.

## 1.3.3

- Avoided passing iolists as `io:format/2` format strings on OTP 21.

## 1.3.2

- Ensured every `observer_cli` process exits when the TUI quits.
- Upgraded `recon` to 2.3.6.

## 1.3.1

- Added atom count and limit data to Home.
- Added short-name and long-name escript support.
- Fixed store-process cleanup.
- Upgraded `recon` to 2.3.5.

## 1.3.0

- Rewrote the Network and Process views.
- Added paging to the Home Top-N process list.
- Added TUI escript loading when `observer_cli` is absent from the target.

## 1.2.2

- Fixed scheduler rendering on systems with 32 or more schedulers.
- Improved byte-unit rendering.

## 1.2.1

- Fixed automatic terminal sizing.
- Improved terminal color adaptation.

## 1.2.0

- Added the Application view.
- Reorganized the TUI and its render loop.
- Adapted the layout to terminal size automatically.

## 1.1.0

- Added the original remote-node escript interface.

## 1.0.9

- Upgraded `rebar3` to 3.3.3 for Hex publishing.
