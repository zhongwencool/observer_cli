# Goal 33: snapshot seam documentation

Checklist item: Document the snapshot seam: future JSON / term output should consume collected data, not parse TUI text.

## Seam contract

Future machine-readable output belongs on the collection side of the current
collection -> render split:

1. collect BEAM / OS / recon data into Erlang terms or maps;
2. optionally sort, page, and normalize those terms;
3. render TUI text from those terms only for the interactive terminal;
4. encode JSON / Erlang term output from the collected terms, never by parsing
   rendered rows, ANSI text, borders, footers, or padded terminal lines.

Current collector shapes are internal implementation details, not a public
snapshot API. A later JSON / term CLI slice may stabilize names and schemas, but
it should still start from these collected terms.

## Current collection seams

| Page / area | Collected data source | Current renderer boundary |
| --- | --- | --- |
| Home | `observer_cli:collect_home_snapshot/6`, `collect_top_n/5` | `observer_cli:render_home_snapshot/2` |
| System | `observer_cli_system:collect_system_info/1`, `collect_sys_info/1` | `observer_cli_system:render_system_sections/1`, `render_sys_info/1` |
| Application | `observer_cli_application:collect_app_info/0`, `collect_app_render_info/3` | `observer_cli_application:render_app_info/2` |
| ETS | `observer_cli_ets:collect_ets_info/1`, `collect_ets_render_info/3` | `observer_cli_ets:render_ets_info/2` |
| Mnesia | `observer_cli_mnesia:collect_mnesia_info/2`, `collect_mnesia_render_info/4` | `observer_cli_mnesia:render_mnesia/4` |
| Network | `observer_cli_inet:collect_io_info/1`, `collect_inet_info/5`, `collect_inet_render_info/3` | `observer_cli_inet:render_io_info/1`, `render_inet_rows/3` |
| Process detail | `observer_cli_process:collect_process_info/1`, `collect_process_messages/1`, `collect_process_dictionary/1`, `collect_process_stack/1`, `collect_process_state/1` | `observer_cli_process:render_process_info/1`, `render_process_messages/1`, `render_process_dictionary/1`, `render_process_stack/1`, `render_process_state/2` |
| Port detail | `observer_cli_port:collect_port_info/1` | `observer_cli_port:render_port_sections/1`, `render_port_info/1` |

## Non-goals for this slice

- No JSON / term CLI flag.
- No public snapshot API.
- No AI skill system.
- No parsing of current terminal output as an intermediate compatibility path.
- No change to normal TUI rendering, commands, plugin behavior, or formatter behavior.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,line80:(.body|split("\n")[79])}'` -> confirmed line 80 is checklist item 33 and issue updated at `2026-07-05T12:39:22Z`.
- `rg -n "collect_.*\(|render_.*snapshot|render_.*info|render_.*rows" src/observer_cli*.erl test/observer_cli*_test.erl` -> checked the documented collector/render names against the current source.
- `git diff --no-index --check /dev/null .agents/observer-cli-2-refactor/GOAL_33_SNAPSHOT_SEAM.md` -> passed.
- `git diff --cached --check` -> passed after staging this ignored evidence file with `git add -f`.

Skipped:

- `rebar3` tests are not required for this docs-only slice because no Erlang
  source, tests, terminal rendering, command handling, raw input, plugin API, or
  runtime behavior changed.
