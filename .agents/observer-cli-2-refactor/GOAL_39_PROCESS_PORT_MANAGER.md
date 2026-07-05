# Goal 39: Process / Port manager refactor

Checklist item: Refactor Process / Port managers without behavior changes, preserving detail subview switching and back/home behavior.

## Scope

This slice only touched the Process and Port manager loops in `observer_cli_process` and `observer_cli_port`. It did not change rendering, data collection, command return atoms, less/state formatting, Network row selection, or neighboring lifecycle cleanup work.

## Changes

- Kept Process detail subview switching inline through the existing `state_view` wait path and worker view messages.
- Moved Process Home/Back stop-and-open actions behind small local helpers, preserving Home back behavior and Plugin back behavior.
- Split Port command parsing from manager side effects: the manager now handles Home and Network navigation after `parse_cmd/0` returns an action.
- Kept Port interval updates, Info refresh, and unknown/jump input forwarding behavior unchanged.

## Non-goals kept

- No generic manager/view framework.
- No CLI behavior changes.
- No Process/Port rendering, data collection, detail output, or command parsing return-value changes.
- No issue #40 cleanup/deletion work.

## Validation

- `gh issue view 133 --repo zhongwencool/observer_cli --json number,title,updatedAt,body --jq ...` -> confirmed issue #133 updatedAt `2026-07-05T12:39:22Z` and checklist item 39.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_process_test,observer_cli_port_test` -> 87 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 eunit` -> 347 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves local manager dispatch/stop-start code and leaves Process/Port rendering, collection, command action names, less state navigation, and Network-to-Port drill-down unchanged; focused start/navigation EUnit plus full EUnit covered the touched paths.
