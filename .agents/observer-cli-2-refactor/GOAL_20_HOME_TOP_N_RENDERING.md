# Goal 20: Home top-n rendering

Checklist item: Refactor Home top-n rendering without behavior changes: remove duplicated column definitions while preserving per-sort column width and format differences.

Changed files:

- `src/observer_cli.erl`
- `test/observer_cli_core_test.erl`

Result:

- Replaced the five duplicated `render_top_n_view/6` sort clauses with one shared renderer driven by a local `top_n_spec/1`.
- Kept each sort's existing metric titles, base text widths, row formats, value conversion, and secondary `recon:info/2` keys.
- Added a focused regression that renders all Home top-n sorts and checks their stable title fragments plus existing row/title visible widths, including the existing reductions-title +1 width.

Validation:

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_golden_test` -> 32 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 compile` -> passed
- `rebar3 eunit` -> 330 tests, 0 failures
- `rebar3 check` -> passed
- `rebar3 as ci compile` -> passed

Skipped:

- Manual terminal QA was not rerun because this slice only removes duplicated Home top-n rendering definitions and focused/golden/full EUnit plus compile/check covered the behavior-preservation surface.
