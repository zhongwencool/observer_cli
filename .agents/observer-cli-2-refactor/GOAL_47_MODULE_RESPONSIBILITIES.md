# Goal 47: 2.0 module responsibility documentation

Checklist item: Update developer documentation describing the 2.0 module
responsibilities: entry points, commands, collection, rendering, pagination,
plugin, formatter, and snapshot seam.

## Changes

- Added `docs/development.md` as the developer-facing 2.0 responsibility map.
- Added the new page to `docs.exs` extras so generated docs include it.
- No Erlang source, tests, tracked generated `doc/` output, README, changelog,
  plugin migration guide, formatter guide, or runtime behavior changed.

## Covered responsibilities

- Entry points: `observer_cli`, `observer_cli_escriptize`, and page `start/*`
  functions.
- Commands: `observer_cli_command`, `observer_cli_lib:parse_cmd/3`, and page
  managers.
- Collection and rendering: current page-specific collect/render seams.
- Pagination: `observer_cli_lib` helpers, `observer_cli_store`, and page records.
- Plugin: `observer_cli_plugin` plus `observer_cli_plugin_compat`.
- Formatter: `observer_cli_formatter`, `observer_cli_formatter_default`, and
  Process State fallback behavior.
- Snapshot seam: future machine-readable output must consume collected terms,
  not rendered terminal text.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,bodyLine103:(.body | split("\n")[102])}'` -> confirmed checklist item 47 and issue updated at `2026-07-05T12:39:22Z`.
- `rg -n "^(collect|render)_[a-zA-Z0-9_]+\(" src/observer_cli_*.erl src/observer_cli.erl` -> checked documented collect/render seam names against current source.
- `rg -n "start_plugin|parse_shared|parse_cmd\(|next_page|update_page_pos|get_pos|sublist|observer_cli_formatter|observer_cli_plugin" src/*.erl` -> checked documented entry, command, pagination, formatter, and plugin owner names.
- `git diff --check` -> passed.
- `rebar3 ex_doc` -> passed and generated the new developer page under ignored `doc/`.
- `rg -n "Developer notes: 2.0 module responsibilities|development.html" doc/development.html doc/dist/sidebar_items-*.js docs/development.md` -> confirmed the generated docs include `development.html`.
- `git diff --cached --check` -> passed after staging the ignored evidence file with `git add -f`.

Skipped:

- EUnit and terminal QA are not required for this docs-only slice because no
  Erlang source, terminal rendering, command handling, plugin behavior,
  formatter behavior, raw input, or runtime startup behavior changed. Goal 49
  owns the next full static/test bundle.
