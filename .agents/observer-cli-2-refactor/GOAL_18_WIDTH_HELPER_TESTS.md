# Goal 18 width helper boundary tests

Issue #133 Phase 2 item 18: add boundary tests for width helpers: narrow terminals, base width, wide terminals, Unicode, ANSI escapes, and already-bordered lines.

Changed files:

- `test/observer_cli_lib_test.erl`

Result:

- Expanded `layout_width` coverage to include very narrow terminals, below-base terminals, exact base width, and the `base + 1` anti-auto-wrap boundary.
- Added direct wide-terminal checks for `layout_extra_width/2` and weighted extra distribution.
- Added `pad_rendered/1` boundary coverage for full-width already-bordered lines, Unicode bordered lines, and ANSI-colored bordered lines.
- No prerequisite checklist item was touched.

Validation:

- `rebar3 fmt` -> passed
- `rebar3 as test eunit --module=observer_cli_lib_test` -> 31 tests, 0 failures
- `git diff --check` -> passed
- `rebar3 check` -> passed
- `rebar3 eunit` -> 329 tests, 0 failures

Notes:

- The known non-fatal `observer_cli_process:render_state/3` timeout warning still appears during full EUnit.
- Manual terminal QA was not rerun because this slice adds tests only and does not change rendering code.
