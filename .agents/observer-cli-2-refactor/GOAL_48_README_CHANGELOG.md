# Goal 48: README and changelog draft

Checklist item: Update README / changelog draft with only user-visible
behavior preservation, plugin breaking changes, and the future AI-friendly
snapshot direction.

## Changes

- Added a short 2.0 draft note to `README.md` covering preserved normal CLI
  behavior, plugin 2.0 breaking API expectations, and the future
  AI-friendly snapshot seam.
- Added a `2.0.0 (draft)` entry to `docs/CHANGELOG.md` with the same three
  release-facing points.
- No Erlang source, tests, generated `doc/` output, runtime behavior, or
  public snapshot CLI changed.

## Validation

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,line48:(.body | split("\n") | to_entries[] | select(.value | test("^48\\. ")) | "\(.key + 1): \(.value)")}'` -> confirmed checklist item 48 at line 104 and issue updated at `2026-07-05T12:39:22Z`.
- `git diff --check` -> passed.
- `rebar3 ex_doc` -> passed; generated ignored `doc/readme.html` and
  `doc/changelog.html` include the new README note and changelog draft entry.
- `rg -n "2\\.0 draft note|2\\.0\\.0 \\(draft\\)|AI-friendly machine-readable snapshots|plugin API breaking" README.md docs/CHANGELOG.md doc/readme.html doc/changelog.html` -> confirmed the source and generated docs contain the intended text.

Skipped:

- EUnit, compile, xref, CI compile, covertool, and manual terminal QA were not
  run because this slice changed release-facing Markdown only. Goal 49 owns the
  next full static/test bundle.
