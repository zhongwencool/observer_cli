# Goal 49 static validation

Checklist item: `49. Run formatting and static checks: format, compile, CI compile, xref, EUnit, and covertool.`

Scope:

- No source or user-visible CLI behavior changes were needed for this slice.
- `rebar3 fmt` made no tracked file changes.
- Summary evidence for each requested command is recorded below.

Validation run on 2026-07-06 Asia/Shanghai:

| Command | Result |
| --- | --- |
| `rebar3 fmt` | Passed; no tracked formatting diff |
| `rebar3 compile` | Passed |
| `rebar3 as ci compile` | Passed with CI `warn_as_error` profile |
| `rebar3 xref` | Passed |
| `rebar3 eunit` | Passed: 352 tests, 0 failures |
| `epmd -daemon && rebar3 as test do eunit, covertool generate` | Passed: 352 tests, 0 failures; generated `_build/test/covertool/observer_cli.covertool.xml` |

Covertool summary from `_build/test/covertool/observer_cli.covertool.xml`:

- line-rate: `0.966`
- lines-covered: `2079`
- lines-valid: `2153`

Known non-fatal output:

- EUnit still emits the existing `observer_cli_process:render_state/3` timeout warning while completing successfully.

Skipped validation:

- None from checklist item 49.
- `rebar3 check`, manual terminal QA, and startup smoke checks are covered by neighboring/final slices and were not rerun here to keep this goal scoped to item 49.

Final scope review:

- Pre-check `git status --short --branch`: `## v2.x`
- Post-check source tree remained clean before adding this evidence file.
