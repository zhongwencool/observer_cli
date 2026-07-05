# Goal 22: Process detail rendering boundaries

Checklist item: Refactor Process detail rendering without behavior changes: separate meta, memory, GC, links/monitors, and state-less view rendering boundaries.

Changed files:

- `src/observer_cli_process.erl`
- `test/observer_cli_process_test.erl`

Result:

- Added `render_process_sections/3` as the Process Info page rendering boundary used by the worker.
- Split Process Info rendering into local meta, memory, and garbage-collection field helpers while preserving the existing table labels, order, widths, colors, and section output.
- Kept links/monitors and reductions/memory chart rendering as separate Process detail sections behind the new boundary.
- Added `render_stateless_view/4` for the non-State detail subviews so Messages, Dictionary, and Current Stack share the menu/body/footer rendering boundary without changing the State less-client path.
- Added focused regressions for the new Process detail section boundary and stateless view wrapper.

Validation:

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_process_test,observer_cli_golden_test` -> 60 tests, 0 failures
- First broad validation attempt reached `rebar3 check` and failed Dialyzer on the new section helper inheriting the existing queue-opaque warning from `render_reduction_memory/4`; the helper was added to the module-local Dialyzer nowarn list alongside the existing queue helpers.
- `rebar3 check` -> passed
- `git diff --check` -> passed
- `rebar3 compile` -> passed
- `rebar3 eunit` -> 333 tests, 0 failures
- `rebar3 as ci compile` -> passed

Skipped:

- Manual terminal QA was not rerun because this slice only moves Process detail rendering composition behind named helpers; focused Process/golden tests plus full EUnit/compile/check covered the behavior-preservation surface.
