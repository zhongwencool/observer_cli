# Goal 34: view manager lifecycle inventory

Checklist item: Identify common lifecycle behavior across view manager loops: start render worker, handle quit, handle interval, handle pagination, and clean up workers.

## Scope checked

Current manager loops checked in `src/observer_cli*.erl`:

- `observer_cli` Home
- `observer_cli_system`
- `observer_cli_application`
- `observer_cli_ets`
- `observer_cli_mnesia`
- `observer_cli_inet` Network
- `observer_cli_process` Process detail
- `observer_cli_port` Port detail
- `observer_cli_plugin`
- `observer_cli_help`

No Erlang source changed in this slice. This is the inventory step before goal 35's small-helper extraction.

## Common lifecycle shape

Most views follow the same small loop:

1. `start/1` or `start/3` creates any per-view state, then starts a linked render worker with `spawn_link/1`.
2. `manager(...)` reads one command, usually through `observer_cli_lib:parse_cmd/3` or a page-local parser.
3. `quit` tells the worker to stop, or calls `observer_cli_lib:exit_processes/1` when extra linked state must be cleaned.
4. interval changes either send `{new_interval, NewInterval}` to a live worker or clean and restart the page with updated opts.
5. pagination changes current page state, clamps the lower bound with `max(..., 1)`, then restarts render state where needed.
6. render workers redraw on a timer; most timer-driven workers use `observer_cli_lib:next_redraw/2`.

## Behavior by view

| View | Start render worker | Quit / cleanup | Interval handling | Pagination / row state |
| --- | --- | --- | --- | --- |
| Home (`observer_cli`) | Starts store, scheduler wall-time flag, render worker. | Unlinks/sends `quit`, restores scheduler wall time, exits store. `clean/1` keeps the same scheduler restore path for navigation/restarts. | Cleans current resources and restarts Home with updated `#home.interval`. | Updates `#home.pages` through `observer_cli_lib:update_page_pos/3` or `/2`; row jump starts Process detail from store. |
| System | Starts one render worker. | Sends `quit` to worker. `clean/1` delegates to `observer_cli_lib:exit_processes/1` for top-menu navigation. | Sends `{new_interval, NewInterval}` to the live worker and updates `#system.interval`. | None. |
| Application | Starts one render worker with app opts and `auto_row`. | Unlinks/sends `quit`; `clean/1` uses `exit_processes/1` for restarts/navigation. | Cleans and restarts with updated `#app.interval`. | Page up/down clamps at page 1, cleans, restarts with updated `#app.cur_page`. |
| ETS | Starts one render worker with interval, attr, page, and `auto_row`. | Sends `quit`; `clean/1` uses `exit_processes/1`. | Cleans and restarts with updated `#ets.interval`. | Page up/down clamps at page 1, cleans, restarts with updated `#ets.cur_page`. |
| Mnesia | Starts one render worker with interval, hide flag, attr, page, and `auto_row`. | Sends `quit`; `clean/1` uses `exit_processes/1`. | Sends `{new_interval, NewMs}` to the live worker and updates `#db.interval`. | Page up/down clamps at page 1, cleans, restarts with updated `#db.cur_page`. |
| Network (`observer_cli_inet`) | Starts store plus render worker with initial IO counters. | Exits store and sends `quit`; `clean/1` exits store and render worker for restarts/navigation. | Sends `{new_interval, NewInterval}` to the live worker and updates `#inet.interval`. | Updates `#inet.pages` with `observer_cli_lib:update_page_pos/3` or `/2`; row jump starts Port detail from store. |
| Process detail | Starts one render worker for the selected process. | Exits worker with `stop` for quit/back/home. | Sends `{new_interval, NewInterval}` to live worker and updates `#process.interval`. | No list pagination; manager routes detail subview switches and Process State follow-up actions. |
| Port detail | Starts one render worker for the selected port. | Sends `quit` to worker. | Sends `{new_interval, NewInterval}` to live worker and updates `#view_opts.port`. | No list pagination; manager routes detail subview switches. |
| Plugin | Initializes plugin config, creates ETS sheet cache, starts render worker. | `exit_processes([ChildPid])`, deletes sheet cache, returns `quit` or starts Home. | Deletes worker/cache and restarts Plugin with updated plugin interval. | Page up/down and numeric jump are plugin-map specific; helpers keep empty-plugin inputs as no-ops. |
| Help | Starts a worker that renders help/doc output. | Sends `quit`; `clean/1` uses `exit_processes/1` for top-menu navigation. | No manager interval update branch; worker redraws using the initial interval. | None. |

## Shared pieces already present

- Cleanup: `observer_cli_lib:exit_processes/1` is the shared kill/unlink helper, with Home and Plugin keeping necessary local cleanup for scheduler state and ETS sheet cache.
- Timer refresh: `observer_cli_lib:next_redraw/2` cancels the old timer and schedules `redraw`. Home intentionally keeps its own `send_after/3` cadence because `proc_window` redraws at 10 ms while displaying the configured interval.
- Command routing: top-level pages that use `observer_cli_lib:parse_cmd/3` already get shared navigation cleanup through each module's `clean/1`; Process, Port, and Plugin keep local parsers because their command sets are page-specific.
- Page math: the repeated page lower-bound rule is `max(CurPage +/- 1, 1)`. Store-backed pages also update row positions through `observer_cli_lib:update_page_pos/2,3`.

## Extraction boundary for the next slice

Goal 35 should stay below a generic framework. The stable helper candidates are only:

- a tiny cleanup/restart helper where a module already repeats `clean([Pid]), start(NewOpts)`;
- a tiny page-delta helper around `max(CurPage + Delta, 1)` if it removes duplication without hiding store-backed row behavior;
- a tiny live interval update helper only if the caller-specific state update remains explicit.

Do not hide Home scheduler restoration, Network store-backed drill-down, Plugin ETS cache/plugin-map behavior, or Process State navigation behind a generic manager abstraction.

## Validation

- `gh issue view 133 --repo zhongwencool/observer_cli --json number,title,updatedAt,body --jq '{number,title,updatedAt,matchingLine: (.body | split("\n") | to_entries[] | select(.value | test("Identify common lifecycle behavior")))}'` -> confirmed checklist item 34 is issue body line 84 (zero-based key 83) and issue updated at `2026-07-05T12:39:22Z`.
- `grep -R "-spec manager\|manager(" -n src` -> listed current manager loops before documenting them.
- `grep -R "next_redraw\|send_after\|receive$\|{new_interval\|quit ->\|page_down_top_n\|page_up_top_n\|exit_processes\|spawn_link" -n src/observer_cli*.erl` -> checked the documented start, quit, interval, pagination, timer, and cleanup paths against source.
- `for f in src/observer_cli.erl src/observer_cli_system.erl src/observer_cli_application.erl src/observer_cli_ets.erl src/observer_cli_mnesia.erl src/observer_cli_inet.erl src/observer_cli_process.erl src/observer_cli_port.erl src/observer_cli_plugin.erl src/observer_cli_help.erl; do grep -n "^start\|^manager\|^clean" "$f"; done` -> cross-checked every documented loop entry.
- `git diff --no-index --check /dev/null .agents/observer-cli-2-refactor/GOAL_34_LIFECYCLE_BEHAVIOR.md` -> no whitespace errors; command exits 1 for a new file diff.

Skipped:

- `rebar3` validation is not required for this docs-only inventory because no Erlang source, tests, terminal rendering, command handling, plugin API, or runtime behavior changed.
