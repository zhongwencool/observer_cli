# Goal 51 remote and escript startup verification

Checklist item: `51. Verify remote startup and escript startup on a real or simulated remote node.`

## Finding

The first escript smoke run against a simulated short-name target node returned `quit`, but the remote render worker emitted:

```text
{undef,[{recon_lib,scheduler_usage_diff,[undefined,undefined],[]}, ...]}
```

Root cause: `observer_cli_escriptize:required_modules/1` found dependency application names from `observer_cli.app`, but did not load dependency application metadata before reading their `modules` keys. A target node without local observer_cli/recon code therefore missed `recon_lib` during escript `remote_load/1`.

## Fix

- `observer_cli_escriptize` now loads application metadata before reading `applications`, `included_applications`, or `modules` keys.
- `observer_cli_escriptize_test` now covers `required_modules([observer_cli])` when `recon` metadata starts unloaded.

## Verification

- `rebar3 fmt`
- `rebar3 as test eunit --module=observer_cli_escriptize_test,observer_cli_start_test` -> 49 tests, 0 failures. Existing deprecated `catch` warnings remain in the old tests.
- Direct remote startup smoke with a simulated `peer` node and test IO returned `remote_start_result=quit` for target `observer_cli_goal51_remote-1158-52450@dengzhongwendeMacBook-Air`.
- `rebar3 escriptize` built `_build/default/bin/observer_cli`. It emitted non-fatal `Found timestamp before 1980` archive warnings.
- Escript startup smoke used a short-name target Erlang node without observer_cli code paths: `observer_cli_goal51_escript_52435@dengzhongwendeMacBook-Air`.
- `printf 'q\n' | _build/default/bin/observer_cli observer_cli_goal51_escript_52435 observer_cli_goal51_cookie 1000` rendered the remote Home screen and ended with `quit`, with no `undef` or `ERROR REPORT` in the escript output after the fix.

## Scope notes

- This slice only changed escript remote module-loading and its focused tests.
- No CLI command syntax, page navigation, plugin API, or terminal layout behavior was intentionally changed.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 check` -> passed.
