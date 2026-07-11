# observer_cli 2.0 diagnostics release validation

Date: 2026-07-11
Design: `docs/observer-cli-2.0-diagnostics-design.md`, sections 17-23
Result: **release gates passed**

This is current machine evidence. All runtime targets were disposable local
nodes, loaded modules from their own OTP-specific build, and received no remote
BEAM injection.

## Supported combinations

The 138-test focused parser, snapshot, diagnostic, trace, and escript suite plus
compile and escriptize passed on OTP 26.2.5.2, 27.3.4.2, 28.5, and 29.0.3.
OTP 26/27 exercised the list inventory path; OTP 28/29 exercised iterators.
Term output passed on all four. JSON passed on OTP 27-29 and intentionally
returns a capability error on OTP 26, which has no stdlib `json` module.

Every controller/target cell ran a real `snapshot --format term`, asserted exit
0, `status=ok`, and the target OTP release:

| Controller / Target | 26 | 27 | 28 | 29 |
| --- | ---: | ---: | ---: | ---: |
| 26 | 238 ms | 180 ms | 195 ms | 174 ms |
| 27 | 152 ms | 134 ms | 134 ms | 135 ms |
| 28 | 156 ms | 149 ms | 142 ms | 147 ms |
| 29 | 171 ms | 155 ms | 144 ms | 143 ms |

All 16 cells passed. This proves protocol compatibility, not BEAM compatibility:
the target must install `observer_cli` built for its own OTP major.

End-to-end OTP 29 escript checks also passed for `memory`, `processes`,
`snapshot --deep`, `diagnose`, and `trace call`; every result used
`observer_cli.cli/v1`. The trace used a disposable node and cleaned the exact
PID flag and MFA pattern.

## Section 21 proof map

- **Parser/context:** `observer_cli_cli_test` covers all reserved verbs, legacy
  and `tui` forms, misplaced/unknown/duplicate/mutually exclusive flags, four
  diagnose modes, duration/timeout coupling, target/name modes, cookie
  sources, hostile terminal text, bounded safe-ETF context files, permissions,
  symlink/type/corruption rejection, and atomic replacement.
- **Transport/schema:** `observer_cli_escriptize_test`, `observer_cli_cli_test`,
  and `observer_cli_snapshot_test` cover unique outbound-only controllers,
  cookie ordering, pre-distributed refusal, legacy startup once, exits 0-4,
  JSON-safe normalization, term round trips, redaction, UTF-8 tagging, caps,
  evidence pointers, and timeout/crash/heap/controller cleanup.
- **Inspection:** snapshot and resource-specific tests cover exact allowlists,
  stable raw generations, atom-safe lookup, born/dead/reset handling,
  deterministic Top N, staged admission, observer exclusions, Mnesia units and
  ownership, socket registry coverage, no endpoint acquisition, counter-shape
  compatibility, and disappearing resources.
- **Advanced inspection:** snapshot/application tests cover full-state
  acquisition followed by target-side value-free shaping, late execution,
  secret/exception redaction, public application supervisor results, local-live
  roots, restarting/remote/dynamic children, identity caps, and one-level-only
  traversal.
- **Diagnostics:** diagnostic tests cover exact 85/95 percent thresholds,
  evidence paths, required/optional mode sets, complete/partial precedence,
  stable reductions share, signed gauges, resets/gaps, context-only growth,
  online normal/dirty scheduler topology, run-queue wording, and paired
  scheduler wall-time cleanup.
- **Trace:** trace tests cover exact MFA/PID admission, recon 2.5.6 gating,
  external global-call coverage, replacement consent, count/rate/duration,
  busy/collision/zero-match paths, ACK-based natural drain, response caps,
  forced loss, controller/tracee/owner/helper failure, silent IO, legal stop,
  emergency clear, fixed names, PID call flag, and MFA pattern cleanup.
- **TUI parity:** `tui_resource_counts_match_snapshot_window_test` compares the
  existing System collector and the diagnostics resource snapshot in one
  sampling window with bounded observer drift.

Full EUnit is the machine check for the detailed fixtures enumerated in design
section 21; this summary does not replace those assertions.

## Disposable budgets

OTP 29.0.3 ran with `+P 300000 +Q 300000`. Production target dispatch measured:

| Resource | Created | Outcome | Capture | Wall | Peak worker heap |
| --- | ---: | --- | ---: | ---: | ---: |
| Processes | 10,000 | 10,051 scanned | 14 ms | 19 ms | 53,874 words |
| ETS tables | 10,000 | 10,019 scanned | 11 ms | 11 ms | 139,267 words |
| Ports (`ram_file_drv`) | 10,000 | 10,001 scanned | 13 ms | 14 ms | 1,029,676 words |
| Processes | 100,000 | refused at 100,051 | 0 ms | 4 ms | below sample interval |
| ETS tables | 100,000 | refused at 100,019 | 0 ms | 1 ms | 1,597 words |
| Ports | 100,000 | refused before enumeration | 0 ms | 1 ms | 1,597 words |

All dispatch responses reported cleanup confirmation. Afterwards zero fixture
processes remained, ETS returned to 19 tables, ports returned to one, and the
scheduler wall-time flag was false.

With 100,000 disposable processes, `recon_trace:calls/3` matched one exact MFA;
implicit-clear setup took 9 ms and final `clear/0` took 5 ms. The wrapper became
active in 12 ms and naturally drained one event in 18 ms total. Cleanup left no
fixture worker, owner, tracer, formatter, PID call flag, or MFA pattern. Count
and rate bound captured events, not recon's node-global setup/cleanup cost.

On a warmed target, the first command added 364 one-time module/runtime atoms;
commands 2 through 100 all reported 12,776 atoms. Twenty concurrent controllers
produced a sampled peak of 12,777. There was no command-count-linear growth.

## Operator boundaries

- Use trusted targets and networks only. Erlang distribution is bidirectional
  and normally unencrypted; the outbound-only controller is not a sandbox.
- New commands require compatible target modules and never use legacy
  `remote_load/1`.
- Snapshot and diagnose do not acquire process messages/dictionaries, table
  rows, application env, cookies, arbitrary state, trace values, or stacks.
- `gen-server-state` copies full state inside a bounded target worker before
  returning only a value-free shape. Supervision inspection is one public
  application root and its direct children.
- Socket results are registry-known coverage. Distribution queue data and
  uncalibrated growth remain context, not health/root-cause claims.
- Trace is one exact MFA and local PID, call-only and node-global at setup and
  cleanup. No provider, daemon, cluster fan-out, eval, auto-fix, trace session
  registry, scoped clear, or remote loader is supported.

## Broad gates

The final OTP 29.0.3 closeout runs:

```text
rebar3 fmt
rebar3 as test eunit --module=observer_cli_cli_test,observer_cli_snapshot_test,observer_cli_diagnostic_test,observer_cli_trace_test,observer_cli_escriptize_test
rebar3 eunit
rebar3 compile
rebar3 as ci compile
rebar3 xref
rebar3 dialyzer
rebar3 check
git diff --check
```

Exact final test counts are recorded in the Goal 17 commit message.
