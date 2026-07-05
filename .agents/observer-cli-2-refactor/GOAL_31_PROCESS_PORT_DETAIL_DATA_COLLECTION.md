# Goal 31: Process / Port detail data collection

Checklist item: Refactor Process / Port detail data collection without behavior changes: preserve recon behavior and error messages while producing detail terms for rendering.

Changed files:

- `src/observer_cli_process.erl`
- `src/observer_cli_port.erl`
- `test/observer_cli_process_test.erl`
- `test/observer_cli_port_test.erl`

Result:

- Process detail collection now normalizes the `recon:info/1` garbage-collection proplist into a stable map before `render_process_info/1` formats the Process Info section.
- Port detail collection now normalizes the `recon:port_info/1` type proplist into a stable map containing `peername`, `sockname`, `statistics`, and `options` before `render_type_line/1` renders socket peer, stats, and options sections.
- Missing port peer/stat/options fields still render as the existing `undefined` peer/socket text or omitted stats/options sections.
- Process `recon:get_state/2` behavior, timeout logging, and the existing user-facing error message remain inside `render_state/3`; this slice did not change the less-client State subview flow.
- Dead Process and Port branches still return `dead` from collection and use the existing `Process(...) has already died.` / `Port(...) has already died.` render paths.
- No user-visible CLI output, navigation, recon call choice, plugin API behavior, or interval behavior is intentionally changed.

Validation:

- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_process_test,observer_cli_port_test,observer_cli_golden_test` -> 90 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 as ci compile` -> passed.
- `rebar3 eunit` -> 344 tests, 0 failures.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only moves Process/Port detail data normalization before the existing renderers; focused detail tests, golden tests, full EUnit, compile, CI compile, diff check, and `rebar3 check` covered the behavior-preservation surface.
- The existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during EUnit and remains the preserved recon error path for processes that do not answer `sys:get_state/2`.
