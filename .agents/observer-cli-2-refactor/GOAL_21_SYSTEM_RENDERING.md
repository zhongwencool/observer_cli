# Goal 21: System rendering sections

Checklist item: Refactor System rendering without behavior changes: separate system info, allocator, cache hit, and distribution node sections.

Changed files:

- `src/observer_cli_system.erl`
- `test/observer_cli_system_test.erl`

Result:

- Added `render_system_sections/1` as the single System-page rendering boundary used by the worker.
- Split that boundary into named section renderers for system info, allocator block sizes, distribution nodes, and cache-hit rates.
- Preserved the existing section renderers, section order, menu/footer behavior, and user-visible output fragments.
- Added a focused regression that proves the new section boundary returns the same section output as the existing direct renderers.

Validation:

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_system_test` -> 25 tests, 0 failures
- `rebar3 as test eunit --module=observer_cli_system_test,observer_cli_golden_test` -> 28 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 compile` -> passed
- `rebar3 eunit` -> 331 tests, 0 failures
- `rebar3 check` -> passed
- `rebar3 as ci compile` -> passed

Skipped:

- Manual terminal QA was not rerun because this slice only moves System rendering composition behind named section helpers; focused System/golden tests plus full EUnit/compile/check covered the behavior-preservation surface.
