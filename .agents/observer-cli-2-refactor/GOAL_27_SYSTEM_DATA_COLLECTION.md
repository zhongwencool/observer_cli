# Goal 27: System data collection refactor

Checklist item: Refactor System data collection without behavior changes: separate OS process info, allocator info, and distribution info from rendering.

Changed files:

- `src/observer_cli_system.erl`
- `test/observer_cli_system_test.erl`

Result:

- `collect_system_info/1` now splits collected System data into `os_process_info`, runtime `sys_info`, nested `allocator_info`, and collected distribution-node rows before rendering.
- OS `ps` parsing lives in `collect_os_process_info/1`; rendering consumes the collected values instead of owning command parsing.
- Allocator/cache-hit collection lives in `collect_allocator_info/0`; rendering consumes the nested allocator map.
- Distribution queue size, queue limit, address, in/out, type, and state are collected before `render_dist_node_info/1` formats rows.
- No user-visible System page output, menu/footer behavior, interval handling, or plugin API behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 as test eunit --module=observer_cli_system_test` -> 25 tests, 0 failures.
- `rebar3 eunit` -> 335 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves System-page data collection ahead of existing renderers; focused System tests, full EUnit, compile, CI compile, and check covered the behavior-preservation surface.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during full EUnit and remains unrelated to this System data collection change.
