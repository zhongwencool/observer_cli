# Goal 30: Network data collection refactor

Checklist item: Refactor Network data collection without behavior changes: preserve count/window mode, IO summary, and port drill-down behavior while producing stable lists for rendering.

Changed files:

- `src/observer_cli_inet.erl`
- `test/observer_cli_inet_test.erl`

Result:

- Network worker still calls `collect_inet_info/5`, preserving the existing `recon:inet_count/2`, first-window-count, and later `recon:inet_window/3` behavior.
- IO sampling is now split into `collect_io_info/1` and `render_io_info/1`; the Byte Input/Output and Total Input/Output summary values use the same `erlang:statistics(io)` deltas as before.
- Per-port memory, IO, queue size, peer, and secondary packet counters are collected into stable row maps with position/selection metadata before `render_inet_rows/3` formats output.
- `render_inet_rows/3` now consumes collected row maps and still returns the reversed `{Pos, Port}` store list used by existing port drill-down and auto-jump fallback behavior.
- Empty `inet_count` / `inet_window` messages, selected-row coloring, count/octet special handling, and the existing non-count input/output column ordering are intentionally preserved.
- No user-visible Network page output, count/window mode switching, interval handling, pagination commands, port drill-down behavior, or plugin API behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_inet_test` -> 24 tests, 0 failures.
- `rebar3 compile` -> passed.
- `git diff --check` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 eunit` -> 343 tests, 0 failures.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves Network IO/per-port data collection ahead of the existing renderer; focused Network tests, full EUnit, compile, CI compile, diff check, and `rebar3 check` covered the behavior-preservation surface.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during full EUnit and remains unrelated to this Network data collection change.
