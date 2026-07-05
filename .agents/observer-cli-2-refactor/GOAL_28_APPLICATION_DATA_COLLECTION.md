# Goal 28: Application data collection refactor

Checklist item: Refactor Application data collection without behavior changes: preserve sorting behavior while producing a stable list for rendering.

Changed files:

- `src/observer_cli_application.erl`
- `test/observer_cli_application_test.erl`

Result:

- Application worker now calls `collect_app_render_info/3` before rendering, so collection builds the existing sorted/paginated `{StartPos, Rows}` input consumed by the renderer.
- The row list keeps the existing `observer_cli_lib:sublist/3` / `recon_lib:sublist_top_n_attrs/2` sorting path and the same sort keys derived from the selected Application column.
- `render_app_info/2` now formats already-collected rows; the old test-facing `render_app_info/3` wrapper remains TEST-only for existing layout coverage.
- No user-visible Application page output, sort shortcuts, pagination, interval handling, or plugin API behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_application_test` -> 10 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 eunit` -> 336 tests, 0 failures.
- `rebar3 check` -> passed.

Adjustment:

- First broad `rebar3 check` attempt failed because Dialyzer reported the compatibility `render_app_info/3` wrapper as production-unused after the worker started using collected rows directly. The wrapper was moved under `-ifdef(TEST)` instead of adding a suppression or public export.

Skipped:

- Manual terminal QA was not rerun because this slice only moves Application collection ahead of the existing renderer; focused Application tests, full EUnit, compile, CI compile, diff check, and `rebar3 check` covered the behavior-preservation surface.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during full EUnit and remains unrelated to this Application data collection change.
