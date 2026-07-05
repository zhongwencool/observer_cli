# Goal 16 rendering helper naming

Issue #133 Phase 2 item 16: consolidate naming for ANSI colors, selected/unselected state, footer rendering, and menu rendering helpers so call sites express intent.

Changed files:

- `include/observer_cli.hrl`
- `src/observer_cli_lib.erl`
- `src/less_client.erl`
- `src/observer_cli.erl`
- `src/observer_cli_application.erl`
- `src/observer_cli_ets.erl`
- `src/observer_cli_help.erl`
- `src/observer_cli_inet.erl`
- `src/observer_cli_mnesia.erl`
- `src/observer_cli_plugin.erl`
- `src/observer_cli_port.erl`
- `src/observer_cli_process.erl`
- `src/observer_cli_system.erl`
- focused menu/footer/color tests

Result:

- Added explicit ANSI macro aliases and named helpers: `ansi_green/1`, `ansi_red/1`, `selected_menu_item/1`, `unselected_menu_item/1`, `menu_item/3`, `menu_items/2`, `render_top_menu/2`, `render_menu_header/3`, and `render_footer/1,2`.
- Kept old shared helper wrappers (`green/1`, `select/1`, `unselect/1`, `render_menu/2`, `render_last_line/1`) for compatibility while moving repo call sites to the intent-revealing names.
- Reused the new menu/footer helpers in Home, list pages, Process, Port, Plugin, Help, and less footer rendering without changing rendered text or command behavior.
- Consolidated direct red/green ANSI message construction behind `ansi_red/1` and `ansi_green/1`.
- No prerequisite checklist item was touched.

Validation:

- `rebar3 fmt` -> passed
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_golden_test,observer_cli_system_test,observer_cli_process_test,observer_cli_port_test,observer_cli_plugin_test,less_client_test` -> 183 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 check` -> passed
- `rebar3 eunit` -> 328 tests, 0 failures

Notes:

- A first `rebar3 check` attempt failed because temporary local `render_last_line` wrappers were unused after call sites moved to `render_footer`; the wrappers were removed and tests were pointed at the new footer names.
- The known non-fatal `observer_cli_process:render_state/3` timeout warning still appears during EUnit.
