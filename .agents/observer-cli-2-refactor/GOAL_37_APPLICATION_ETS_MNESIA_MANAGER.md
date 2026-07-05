# Goal 37: Application / ETS / Mnesia manager refactor

Checklist item: Refactor Application / ETS / Mnesia managers without behavior changes, consolidating interval and pagination handling.

## Scope

This slice only touched the Application, ETS, and Mnesia manager loops. It did not change rendering, data collection, command parsing, plugin APIs, or neighboring Home/Network/Process/Port managers.

## Changes

- Reused local restart helpers in Application, ETS, and Mnesia managers so sort/interval/page actions keep one cleanup-and-start path per module.
- Moved page up/down updates in those managers behind local `restart_page/4` helpers while preserving `observer_cli_lib:next_page/2` lower-bound behavior.
- Kept Mnesia live interval and hide-system-table messages live instead of converting them to worker restarts.

## Non-goals kept

- No generic manager/view framework.
- No CLI behavior changes.
- No pagination, interval cadence, rendering, or data collection changes.
- No issue #38 Network manager work.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,line87: (.body | split("\\n")[86]), phase4: (.body | split("\\n") | to_entries | map(select(.value|test("Phase 4|Application / ETS / Mnesia|Home manager|Network manager"))) )}'` -> confirmed item 37 at updatedAt `2026-07-05T12:39:22Z`.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_application_test,observer_cli_ets_test,observer_cli_mnesia_test` -> 30 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 eunit` -> 347 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 as ci compile` -> passed.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only removes duplicated manager cleanup/page-update plumbing and leaves rendering, command parsing, worker cadence, and data collection unchanged.
