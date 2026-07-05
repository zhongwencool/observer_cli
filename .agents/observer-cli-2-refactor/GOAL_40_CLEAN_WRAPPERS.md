# Goal 40: clean wrapper deletion

Checklist item: Delete no-longer-needed clean wrappers or pure forwarding functions only when tests prove cleanup behavior is unchanged.

## Scope

This slice only removed pure `clean/1` forwarding wrappers that delegated directly to `observer_cli_lib:exit_processes/1`.

Kept the Home `observer_cli:clean/1` callback because it still restores scheduler wall-time state in addition to process cleanup.

## Changes

- Deleted `clean/1` exports and forwarding implementations from Application, ETS, Help, Network, Mnesia, and System modules.
- Replaced their local restart/drill-down cleanup calls with direct `observer_cli_lib:exit_processes/1` calls.
- Changed shared top-menu routing cleanup to call `observer_cli:clean/1` only for Home resources and use `observer_cli_lib:exit_processes/1` for plain worker pid lists.
- Moved cleanup proof from removed module-level `clean_test` cases into `observer_cli_lib_test` top-menu cleanup tests for plain pid cleanup and Home scheduler-resource cleanup.

## Non-goals kept

- No generic view framework.
- No user-visible CLI behavior changes.
- No cleanup changes for Plugin ETS cache handling, Home scheduler restoration, Process detail stop behavior, or Port detail stop behavior.
- No Phase 5 plugin callback/API work.

## Validation

- `gh issue view 133 --json title,body,updatedAt,url --jq '{title,updatedAt,url,body}'` -> confirmed issue #133 updatedAt `2026-07-05T12:39:22Z` and checklist item 40.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_help_test,observer_cli_system_test` -> first run exposed that `exit_processes/1` flushes monitor `DOWN` messages; tests were corrected to assert target process liveness instead of monitor receipt.
- `rebar3 as test eunit --module=observer_cli_lib_test,observer_cli_help_test,observer_cli_system_test` -> 64 tests, 0 failures.
- `rebar3 as test eunit --module=observer_cli_application_test,observer_cli_ets_test,observer_cli_mnesia_test,observer_cli_inet_test,observer_cli_lib_test,observer_cli_help_test,observer_cli_system_test` -> 118 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 compile` -> passed.
- `rebar3 eunit` -> 347 tests, 0 failures. Existing non-fatal `observer_cli_process:render_state/3` timeout warning appeared during the run.
- `rebar3 as ci compile` -> passed.
- `rebar3 check` -> passed.

Skipped:

- Manual terminal QA was not rerun because this slice only deletes pure cleanup forwarding wrappers and preserves the same worker/store exit helper; focused top-menu cleanup tests plus full EUnit covered the cleanup behavior.
