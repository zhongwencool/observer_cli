# Goal 32: collected data structure tests

Checklist item: Add tests for collected data structures. Assert structure and important fields, not volatile runtime values.

Changed files:

- `test/observer_cli_core_test.erl`
- `test/observer_cli_system_test.erl`
- `test/observer_cli_application_test.erl`
- `test/observer_cli_ets_test.erl`
- `test/observer_cli_mnesia_test.erl`
- `test/observer_cli_inet_test.erl`
- `test/observer_cli_process_test.erl`
- `test/observer_cli_port_test.erl`

Result:

- Home snapshot tests now assert the collected snapshot key set and summary block shape while avoiding volatile runtime values.
- System tests now assert the collected top-level map keys, allocator map keys, OS-process fields, selected runtime-info fields, and distribution-node row shape.
- Application tests now assert live collected application entries keep the `{Count, Memory, Reductions, MsgQueueLen, Status, Version}` tuple shape without fixed runtime counters.
- ETS and Mnesia tests now assert collected row tuple/proplist structure plus stable table identity, sort field, and important table fields.
- Network tests now assert the collected per-port render row map has the expected seam keys.
- Process and Port detail tests now assert detail map key sets and important nested fields without pinning volatile counters, names, or runtime values.
- No production code, user-visible CLI behavior, rendering text, command handling, recon call choice, or plugin API behavior was changed.

Validation:

- `gh issue view 133 --json number,title,updatedAt,body --jq '{number,title,updatedAt,line79:(.body|split("\\n")[78])}'` -> confirmed line 79 is checklist item 32 and issue updated at `2026-07-05T12:39:22Z`.
- `rebar3 fmt` -> passed.
- `rebar3 as test eunit --module=observer_cli_core_test,observer_cli_system_test,observer_cli_application_test,observer_cli_ets_test,observer_cli_mnesia_test,observer_cli_inet_test,observer_cli_process_test,observer_cli_port_test` -> 196 tests, 0 failures.
- `git diff --check` -> passed.
- `rebar3 eunit` -> 345 tests, 0 failures.
- `rebar3 as ci compile` -> passed.

Skipped / known noise:

- Manual terminal QA was not run because this slice only adds/strengthens tests and does not change production rendering, terminal input, or raw-mode behavior.
- The focused EUnit run printed existing non-fatal warnings from `test/observer_cli_mnesia_test.erl` deprecated `catch` usage and the OTP `net_address` include record; these were not introduced by this slice.
- The focused and full EUnit runs printed the existing preserved `observer_cli_process:render_state/3` timeout warning path; the tests still passed.
