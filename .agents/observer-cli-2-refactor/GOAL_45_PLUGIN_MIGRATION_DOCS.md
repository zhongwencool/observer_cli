# Goal 45: plugin 1.x-to-2.0 migration documentation

Checklist item: Update plugin documentation with a 1.x-to-2.0 migration guide.

## Scope

Changed only plugin author documentation and this evidence file:

- `docs/plugin.md`
- `.agents/observer-cli-2-refactor/GOAL_45_PLUGIN_MIGRATION_DOCS.md`

No Erlang source, tests, README, generated `doc/` output, or normal built-in CLI page behavior changed. Goal 46 still owns plugin-focused tests and full EUnit confirmation for the breaking change.

No prerequisite checklist item was touched.

## Documentation decisions

The plugin guide now describes the 2.0 map-based plugin API:

- `attributes/1` returns `#{rows => Rows, state => State}`.
- `sheet_header/0` returns `#{columns => Columns, default_sort => ColumnId}` with stable column ids.
- `sheet_body/1` returns `#{rows => Rows, state => State}` where each row is `#{cells => #{ColumnId => Value}}` with optional `handle`.
- Plugin config uses `sort => ColumnId` and `handler => HandlerModule` for new code.
- Default process drill-down is explicit through `handle => Pid`.

The migration guide maps the old 1.x tuple/list shapes to 2.0, includes a minimal before/after code snippet, and records the current compatibility boundary: `sort_column` and handler tuples are migrated at startup, but legacy callback return shapes fail early with `plugin_api_error`.

## Validation

- `gh issue view 133 --json body,updatedAt,title` -> confirmed issue #133 checklist item 45.
- `git diff --check` -> passed.
- `rebar3 ex_doc` -> passed, including Markdown/ExDoc generation.
- `rg -n 'href="#4-1-x-to-2-0-migration"|id="4-1-x-to-2-0-migration"' doc/plugin.html` -> confirmed the generated migration link target.
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test,observer_cli_plugin_compat_test` -> 38 tests, 0 failures.

Skipped:

- Full EUnit and manual terminal QA; this slice changes plugin author docs only and goal 46 owns the plugin-focused/full-EUnit confirmation for the breaking API boundary.
