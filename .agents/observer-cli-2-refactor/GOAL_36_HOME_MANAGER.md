# Goal 36: Home manager refactor

Checklist item: Refactor the Home manager without behavior changes, preserving scheduler wall-time restoration, store cleanup, and process drill-down behavior.

## Scope

This slice only touched the Home manager in `src/observer_cli.erl` plus one focused core test. No Application, ETS, Mnesia, Network, Process, Port, Plugin, rendering, raw input, or plugin API behavior was changed.

## Changes

- Split Home manager input handling into local action clauses while keeping `observer_cli_lib:parse_cmd/3` as the command source.
- Kept the Home cleanup resource as `[RenderPid, StorePid, LastSchWallFlag, SchUsage]` so scheduler wall-time restoration still uses the pre-action scheduler setting.
- Kept Home quit behavior special: unlink/send `quit` to the render worker, restore scheduler wall time, and exit only the store process.
- Kept process drill-down cleanup and launch behavior behind the same store lookup rules, now isolated in `select_home_process/3` and `open_process_view/3`.
- Added a focused test for exact row selection, numeric non-auto no-op selection, and auto-jump fallback selection.

## Non-goals kept

- No generic manager/view framework.
- No rendering changes.
- No terminal/raw-input behavior changes.
- No lifecycle changes for non-Home managers.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,body:(.body|split("\\n")|.[80:92])}'` -> confirmed item 36 at updatedAt `2026-07-05T12:39:22Z`.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_start_test` -> 43 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 check` -> passed.
- `rebar3 eunit` -> 347 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 as ci compile` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only reorganizes Home manager action dispatch and adds deterministic store selection coverage; rendering and raw `-noshell` input behavior were not changed.
