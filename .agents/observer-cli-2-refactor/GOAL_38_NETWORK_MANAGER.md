# Goal 38: Network manager refactor

Checklist item: Refactor the Network manager without behavior changes, preserving store-based row selection and port drill-down behavior.

## Scope

This slice only touched the Network manager loop in `observer_cli_inet`. It did not change rendering, data collection, command parsing, Network row storage, Port detail rendering, or neighboring Process/Port managers.

## Changes

- Moved Network mode/type restarts and page restarts behind small local helpers.
- Kept quit, live interval updates, numeric jump, and automatic `jump` port drill-down branches inline so their existing behavior stays obvious.
- Kept `observer_cli_store` as the source of row selection for numeric jumps and automatic `jump` port drill-down.
- Kept page restarts using `observer_cli_lib:update_page_pos/3` with the store pid so selected row positions survive page changes.

## Non-goals kept

- No generic manager/view framework.
- No CLI behavior changes.
- No Network rendering, data collection, command parsing, IO delta, or Port detail behavior changes.
- No issue #39 Process / Port manager work.

## Validation

- `gh issue view 133 --json title,body,updatedAt,url --jq '{title,updatedAt,url,body}'` -> confirmed issue #133 updatedAt `2026-07-05T12:39:22Z` and checklist item 38.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_inet_test` -> 24 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 eunit` -> 347 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 as ci compile` -> passed.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only extracts local manager helper calls and leaves Network rendering, collection, store updates, command parsing, and Port detail behavior unchanged; focused Network manager and port-view EUnit covered the touched paths.
