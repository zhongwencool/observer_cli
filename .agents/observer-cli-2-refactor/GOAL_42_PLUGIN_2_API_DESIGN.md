# Goal 42: plugin 2.0 API design

Checklist item: Design the smallest plugin 2.0 breaking change that makes callback return values clearer and reduces implicit column-width and handler guessing.

## Scope

This slice is design-only. It does not change Erlang runtime code, public `docs/plugin.md`, tests, or normal CLI behavior. Goals 43-45 should turn this contract into compatibility tests, renderer changes, and migration docs.

Inputs checked:

- issue #133 Phase 5 item 42 via `gh issue view 133`
- `GOAL_41_PLUGIN_CALLBACK_USAGE.md`
- `src/observer_cli_plugin.erl`
- `docs/plugin.md`
- `test/observer_cli_plugin_test.erl`
- `test/observer_cli_plugin_render_test.erl`
- `test/observer_cli_test_plugin.erl`

No prerequisite checklist item was touched.

## Chosen smallest breaking change

Keep the existing plugin entry points and callback names, but make the returned data self-describing:

1. Keep `observer_cli:start_plugin/0`, the `plugins` app env, menu `title` / `shortcut`, callback names `attributes/1`, `sheet_header/0`, and `sheet_body/1`.
2. Replace untagged `{Rows, State}` callback results with maps using `rows` and `state` keys.
3. Replace position-only sheet rows with named column cells keyed by header `id`.
4. Replace `sort_column` integer indexes with `sort` column ids.
5. Replace predicate-based row handler guessing with an explicit row `handle` and an optional handler module.

That is the smallest break because it leaves startup, menu routing, refresh intervals, pagination, shortcut sorting, callback count, and the built-in page behavior alone.

## 2.0 callback contract

```erlang
-type plugin_cell() :: string() | binary() | integer() | float() | pid() |
                       atom() | {byte, non_neg_integer()} | {percent, float()}.
-type column_id() :: atom().
-type attr_cell() :: #{content := plugin_cell(),
                       width := pos_integer(),
                       color => iodata()}.
-type sheet_column() :: #{id := column_id(),
                          title := iodata(),
                          width := pos_integer(),
                          shortcut => string()}.
-type sheet_row() :: #{cells := #{column_id() => plugin_cell()},
                       handle => term()}.

-callback attributes(State0) -> #{rows := [[attr_cell()]], state := State1} when
    State0 :: any(),
    State1 :: any().

-callback sheet_header() -> #{columns := [sheet_column()],
                              default_sort := column_id()}.

-callback sheet_body(State0) -> #{rows := [sheet_row()], state := State1} when
    State0 :: any(),
    State1 :: any().
```

Rendering rules:

- Attribute cells keep their current per-cell `width` and optional `color`; only the return wrapper changes.
- Sheet column order and width come only from `sheet_header/0` `columns`.
- Each body row renders by reading `cells[ColumnId]` for each declared column. Missing display cells render as empty; extra cell ids are ignored.
- The sheet width remains an internal sum of declared column widths.
- Header shortcuts still sort the sheet, but the selected sort value is a column id instead of a 1-based index.
- If a plugin config omits `sort`, `sheet_header/0` `default_sort` is used.

## 2.0 config changes

Retained config keys:

- `module`
- `title`
- `shortcut`
- `interval`

Changed config keys:

| 1.x key | 2.0 key | Reason |
| --- | --- | --- |
| `sort_column => 2` | `sort => memory` | Removes `lists:nth/2` column guessing. |
| `handler => {PredicateFun, HandlerModule}` | `handler => HandlerModule` plus row `handle` | Removes scanning a row for the first predicate match. |

Selection rules:

- Numeric row selection and Enter still use the current rendered row and preserve `cur_row` behavior.
- If the row has no `handle`, selection is a no-op.
- If the row has `handle` and the plugin has `handler => Module`, call `Module:start(plugin, Handle, ViewOpts)`.
- If the row has `handle` and no custom handler, call `observer_cli_process:start(plugin, Handle, ViewOpts)` only when `Handle` is a pid; otherwise report a clear plugin API error.

## Migration sketch

1.x:

```erlang
#{module => my_plugin,
  title => "Proc",
  shortcut => "P",
  sort_column => 3,
  handler => {fun is_pid/1, observer_cli_process}}

sheet_header() ->
    [#{title => "Pid", width => 15},
     #{title => "Name", width => 20},
     #{title => "Memory", width => 16, shortcut => "M"}].

sheet_body(State) ->
    {[[Pid, Name, {byte, Memory}]], State}.
```

2.0:

```erlang
#{module => my_plugin,
  title => "Proc",
  shortcut => "P",
  sort => memory}

sheet_header() ->
    #{default_sort => memory,
      columns => [#{id => pid, title => "Pid", width => 15},
                  #{id => name, title => "Name", width => 20},
                  #{id => memory, title => "Memory", width => 16, shortcut => "M"}]}.

sheet_body(State) ->
    #{rows => [#{handle => Pid,
                cells => #{pid => Pid,
                           name => Name,
                           memory => {byte, Memory}}}],
      state => State}.
```

## Compatibility decisions for goal 43

Goal 43 should test these explicitly:

- Retained: plugin menu shortcuts, header shortcuts, refresh interval, pagination, current-row memory, default pid drill-down, custom handler module dispatch, byte/percent formatting, and missing-callback handling policy.
- Migrated: `sort_column` index becomes `sort` column id; `handler` tuple becomes handler module plus row `handle`.
- Rejected with a clear error: 1.x `{Rows, State}` callback returns, list-only `sheet_header/0`, list rows from `sheet_body/1`, duplicate column ids, invalid `sort` / `default_sort` ids, and default handler selection for a non-pid `handle`.

## Non-goals

- No version negotiation or dual 1.x/2.0 adapter in this design.
- No new behaviour module, dependency, TUI framework, or snapshot CLI.
- No release-facing documentation change in this slice; goal 45 owns `docs/plugin.md` migration text.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body` confirmed issue #133 item 42 and updated time `2026-07-05T12:39:22Z`.
- Source and test inspection confirmed the current implicit points: `render_sheet_body/8` uses `lists:nth(SortRow, Row)`, `mix_content_width/3` zips row cells with header widths by position, and row selection scans row items with a predicate.
- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test` -> 33 tests, 0 failures.
- `git diff --no-index --check -- /dev/null .agents/observer-cli-2-refactor/GOAL_42_PLUGIN_2_API_DESIGN.md` -> no whitespace errors; command exits 1 for a new file diff.

Skipped:

- Runtime terminal QA was not run because this slice is a design-only `.agents` evidence file; no Erlang runtime, renderer, or CLI behavior changed.
