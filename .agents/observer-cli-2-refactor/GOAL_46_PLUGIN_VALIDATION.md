# Goal 46: plugin-focused and full EUnit validation

Checklist item: Run plugin-focused tests and full EUnit to confirm the breaking change affects plugin authors only, not normal built-in CLI pages.

## Scope

This slice changed only this evidence file:

- `.agents/observer-cli-2-refactor/GOAL_46_PLUGIN_VALIDATION.md`

No Erlang source, tests, docs, generated docs, or normal built-in CLI page behavior changed. No prerequisite checklist item was touched.

## Issue check

- `gh issue view 133 --json number,title,updatedAt,url,body` confirmed issue #133, updated at `2026-07-05T12:39:22Z`, and line 99 / checklist item 46.

## Validation results

Plugin-focused validation:

- `rebar3 as test eunit --module=observer_cli_plugin_test,observer_cli_plugin_render_test,observer_cli_plugin_compat_test`
- Result: 38 tests, 0 failures.
- Coverage intent: reruns the plugin manager/render path plus the plugin 2.0 compatibility contract that retains plugin navigation and rejects legacy callback return shapes for plugin authors.

Full EUnit validation:

- `rebar3 eunit`
- Result: 352 tests, 0 failures.
- Coverage intent: reruns the normal built-in CLI page test suite after the plugin 2.0 API break. No built-in page regression surfaced, so the observed break remains isolated to plugin author callback/config migration.

## Notes

- Full EUnit still logs the known non-fatal `observer_cli_process:render_state/3` timeout warning; the suite completed with 0 failures.
- Mnesia stop info reports also appeared during full EUnit and did not fail the suite.

Skipped:

- Manual terminal QA; this validation slice does not change rendering code, and goal 50 owns broad real-terminal page inspection.
