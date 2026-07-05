# observer_cli 2.0 refactor baseline

Date: 2026-07-06 Asia/Shanghai
Source issue: `#133`, updated `2026-07-05T12:39:22Z`
Slice: Phase 0 checklist item 1 only

## Repository and toolchain

- Branch: `v2.x`
- HEAD before this evidence file: `278b53f` (`Prepare observer_cli internals for 2.0 guardrails`)
- Tracked worktree before this evidence file: clean (`git status --short --branch` showed only `## v2.x`)
- Local Erlang: OTP 29 / ERTS 17.0
- Local rebar3: 3.26.0
- CI matrix from `.github/workflows/erlang.yml`: OTP 29/rebar3 3.26, OTP 28/rebar3 3.24, OTP 27/rebar3 3.22, OTP 26/rebar3 3.22

## Current validation commands

Use `rebar3`, not `mix test`.

Normal local checks:

```sh
git status --short --branch
git diff --name-status
git diff --stat
rebar3 fmt
rebar3 compile
rebar3 as ci compile
rebar3 eunit
rebar3 xref
rebar3 check
```

Coverage check matching CI:

```sh
epmd -daemon && rebar3 as test do eunit, covertool generate
```

`rebar3 check` currently expands through the repo alias to `compile`, `lint`, `fmt`, `xref`, `dialyzer`, and `ex_doc`.

## Validation refreshed for this baseline

- `gh issue view 133 --json number,title,state,updatedAt,body` confirmed the source issue is open and still updated at `2026-07-05T12:39:22Z`.
- `epmd -daemon && rebar3 as test do eunit, covertool generate` passed: 311 tests, 0 failures; generated `_build/test/covertool/observer_cli.covertool.xml`.
- `rebar3 check` passed.
- Known non-fatal test output: `observer_cli_process:render_state/3` can log a timeout warning while EUnit still passes.
- Skipped manual terminal QA for this slice because this is a baseline evidence-only change; real TTY validation belongs to the later manual inspection checklist item.

## Current coverage

Covertool XML root: line-rate `0.969`, 2013/2077 lines covered.

| Module | Coverage | Lines |
|---|---:|---:|
| observer_cli | 95.4% | 391/410 |
| observer_cli_application | 96.8% | 121/125 |
| observer_cli_escriptize | 95.5% | 84/88 |
| observer_cli_ets | 97.0% | 65/67 |
| observer_cli_formatter | 100.0% | 1/1 |
| observer_cli_formatter_default | 100.0% | 1/1 |
| observer_cli_help | 100.0% | 28/28 |
| observer_cli_inet | 95.7% | 132/138 |
| observer_cli_lib | 98.2% | 220/224 |
| observer_cli_mnesia | 95.1% | 77/81 |
| observer_cli_plugin | 98.9% | 182/184 |
| observer_cli_port | 99.3% | 145/146 |
| observer_cli_process | 97.1% | 265/273 |
| observer_cli_store | 100.0% | 21/21 |
| observer_cli_system | 97.4% | 186/191 |
| less_client | 100.0% | 56/56 |
| less_server | 88.4% | 38/43 |

No key `observer_cli_*` module is below the issue's 95% threshold in this snapshot. The remaining uncovered lines are narrow runtime/error branches; no coverage exception is approved by this slice.

## Public entry points to preserve

This is the baseline list only. The fuller entry inventory is checklist item 2.

- Normal/local Erlang API: `observer_cli:start/0`, `observer_cli:start/1`, `observer_cli:start/2`.
- Plugin startup API: `observer_cli:start_plugin/0` and `observer_cli_plugin:start/1` internal startup path.
- Remote startup: `observer_cli:start(Node)`, `observer_cli:start(Node, Cookie)`, `observer_cli:start(Node, Options)` using hidden node connection and RPC.
- Escript startup: `observer_cli_escriptize:main/1`, command shape `observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]`; `ensure_set_env/2` is exported for remote RPC support.
- Plugin callbacks: `observer_cli_plugin` behaviour `attributes/1`, `sheet_header/0`, `sheet_body/1`; configured through the `plugins` app env.
- Formatter callbacks: `observer_cli_formatter` behaviour `format/2`, called via `observer_cli_formatter:format/3`; configured through the `formatter` app env with default fallback.
- Shared option/type surface: exported record types in `include/observer_cli.hrl`, especially `view_opts/0` and per-page option records.

## Compatibility boundaries

- Preserve normal user-visible CLI behavior: Home, System, Application, Network, ETS, Mnesia, Process, Port, Doc/help, Plugin page, navigation, sorting, pagination, refresh interval, process/port drill-down, escript startup, and remote-node monitoring.
- Plugin API breaking changes are allowed only in the plugin 2.0 phase, with tests and migration documentation; they must not change built-in pages for users without custom plugins.
- Formatter behavior remains compatible by default unless a separate issue explicitly changes it.
- Snapshot work must use collected Erlang terms/maps before TUI rendering; do not parse terminal text as a machine-readable API.
- Raw single-key terminal shortcut work stays out of this issue; `rebar3 shell` is not proof for any future `-noshell` raw-input path.
- Keep terminal rendering changes within existing layout helpers to avoid auto-wrap and stale redraw artifacts.

## Explicit non-goals for issue #133 and this slice

- Do not implement the full AI skill system.
- Do not add a JSON/term snapshot CLI in this issue; only preserve the seam for a later issue.
- Do not rewrite the TUI, add curses-style dependencies, or introduce a generic view framework.
- Do not change normal users' core CLI workflow.
- Do not restart raw single-key shortcut work.
- Do not redesign pages visually.
- Do not delete broad historical code without behavior tests proving equivalence.
- Do not broaden this slice beyond recording the baseline.
