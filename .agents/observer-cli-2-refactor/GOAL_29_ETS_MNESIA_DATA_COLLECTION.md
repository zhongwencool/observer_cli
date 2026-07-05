# Goal 29: ETS / Mnesia data collection refactor

Checklist item: Refactor ETS / Mnesia data collection without behavior changes: preserve sorting, hidden-system-table behavior, and exceptional table handling while producing stable lists for rendering.

Changed files:

- `src/observer_cli_ets.erl`
- `src/observer_cli_mnesia.erl`
- `test/observer_cli_ets_test.erl`
- `test/observer_cli_mnesia_test.erl`

Result:

- ETS worker now collects `{StartPos, Rows}` with `collect_ets_render_info/3` before rendering, using the existing `collect_ets_info/1` and `observer_cli_lib:sublist/3` sort/pagination path.
- Mnesia worker now collects `{StartPos, Rows}` with `collect_mnesia_render_info/4` before rendering, preserving the existing `{error, Reason}` branch when Mnesia is not running.
- Mnesia hidden-system-table filtering remains in `collect_mnesia_info/2`; the new render collection wrapper only applies the existing sort/pagination step after that filter.
- ETS unread/dead-table handling stays in `get_ets_info/2` and `unread/0`; Mnesia unknown storage formatting stays in `with_storage_type/3`.
- Test-only compatibility wrappers keep existing layout tests able to pass explicit fixture lists through the same sort/pagination seam.
- No user-visible ETS/Mnesia output, command handling, interval handling, pagination, or plugin API behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_ets_test,observer_cli_mnesia_test` -> 19 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 eunit` -> 341 tests, 0 failures.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves ETS/Mnesia sort/pagination collection ahead of existing renderers; focused ETS/Mnesia tests, full EUnit, compile, CI compile, diff check, and `rebar3 check` covered the behavior-preservation surface.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during full EUnit and remains unrelated to this ETS/Mnesia data collection change.
