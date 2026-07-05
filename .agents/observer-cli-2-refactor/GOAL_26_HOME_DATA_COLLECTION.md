# Goal 26: Home data collection refactor

Checklist item: Refactor Home data collection without behavior changes: separate runtime sampling, process ranking, scheduler usage diff, and rendering.

Result:

- `redraw_running/8` now collects a Home snapshot before rendering instead of interleaving sampling, process ranking, scheduler line sizing, and TUI output assembly inline.
- Runtime summary sampling stays in existing Erlang row terms: `system_summary`, `memory_summary`, and `scheduler_usage`.
- Process ranking is collected as `top_processes` plus the existing refresh prompt before `render_top_n_view/5` renders rows.
- Scheduler usage diff is split from IO/GC diff inside `node_stats/2`, and scheduler row count is derived without rendering the scheduler lines first.
- Test-only rendering wrappers remain available under `-ifdef(TEST)` for golden/core tests; production rendering consumes the snapshot map directly.
- No user-visible CLI behavior or plugin API behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_golden_test` -> 33 tests, 0 failures.
- `rebar3 eunit` -> 335 tests, 0 failures.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not run for this slice because the rendered Home fragments remain covered by focused golden output tests and no terminal input/raw-mode behavior changed.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during full EUnit and remains unrelated to this Home data collection change.
