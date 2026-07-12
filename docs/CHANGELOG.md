# Changelog

This page records release and maintenance changes to `observer_cli`, newest
first.

## 2.0.0 (release candidate)

- Added command-first snapshot, inspection, diagnostics, saved-target context,
  and bounded `recon` call-trace commands.
- Made `tui` the only interactive escript command and removed the positional
  TUI shorthand. Automatic remote module loading remains TUI-only.
- Added local help and version routes, command-specific usage errors, fixed exit
  classes, and separate stdout/stderr contracts.
- Kept canonical trace command identities across parser, context, runtime, and
  success paths, with trace-stop timeouts validated independently from call
  sampling.
- Rendered successful commands as structured text while retaining consultable
  Erlang terms and OTP 27+ JSON for machine output.
- Reported compatible, missing, and incompatible target diagnostics, including
  expected and observed bundle/protocol versions.
- Saved target context only after controller cleanup is confirmed, bounded and
  sanitized observed capabilities, and preflighted encoders before context
  changes.
- Allowed `disconnect` to remove malformed or oversized protected context data
  without weakening file-type or permission checks.
- Removed command-first remote BEAM injection. Command targets now require a
  compatible diagnostics bundle in their release.
- Added generated-escript help, version, stream, and exit-code smoke checks to
  every configured OTP 26–29 CI job.
- Restored green lint, Dialyzer, ExDoc, and aggregate check gates.
- Published an ExDoc `llms.txt` index, per-page Markdown documents, and
  copy-ready Markdown controls for LLM and agent workflows.
- Documented scan admission, Erlang distribution trust, high-risk state and
  supervision inspection, and node-global `recon` cleanup.
- Introduced explicit 2.0 plugin callback shapes. See
  [Extend the TUI](how-to/extend-the-tui.md) for the migration table.

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
