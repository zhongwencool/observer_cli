# Contribute a change

Use this workflow to make a small, reviewable change and run the same core
checks used by the repository.

## 1. Prepare the checkout

The project uses `rebar3`. The current local tool declaration is Erlang/OTP
29.0, while CI covers OTP 26, 27, 28, and 29.

Fetch dependencies and compile:

```sh
rebar3 compile
```

Do not use `mix test` for normal repository validation.

## 2. Keep the change page-scoped

Before editing, find the existing page module, collector, renderer, parser, or
test helper that owns the behavior. Reuse shared layout and pagination helpers
instead of introducing a second path.

Preserve existing CLI behavior unless the change explicitly alters it. In
particular:

- avoid terminal auto-wrap and stale redraw artifacts;
- keep diagnostics probes bounded and structured;
- preserve unrelated working-tree changes;
- add no dependency for behavior already covered by Erlang/OTP or current
  helpers.

## 3. Run the smallest relevant test first

Run one or more EUnit modules while iterating:

```sh
rebar3 as test eunit --module=observer_cli_core_test,observer_cli_lib_test
```

Choose modules that exercise the changed behavior. Then run the full suite:

```sh
rebar3 eunit
```

Tests that use Erlang distribution need `epmd` and permission to open local
distribution sockets. A sandbox that blocks those operations can produce false
connection failures.

## 4. Format and compile strictly

Format Erlang sources:

```sh
rebar3 fmt
```

Compile with warnings treated as errors:

```sh
rebar3 as ci compile
```

For the full configured quality pipeline, run:

```sh
rebar3 check
```

The `check` alias runs compile, lint, formatting, xref, Dialyzer, and ExDoc.

## 5. Test the generated escript

Build the command users execute:

```sh
rebar3 escriptize
_build/default/bin/observer_cli --help
_build/default/bin/observer_cli --version
```

Run the repository smoke test after command routing, help, output, or escript
startup changes:

```sh
scripts/escript-smoke.sh
```

For raw terminal input or redraw work, run the generated escript in a real
terminal against a disposable node. `rebar3 shell` does not prove the
`-noshell` raw-input path.

## 6. Build the documentation

Generate the ExDoc site:

```sh
rebar3 docs
```

Open `doc/index.html` and check navigation, code blocks, relative links, and any
changed terminal images. Also check `doc/llms.txt` and a changed page's
copy-ready `.md` file. ExDoc warnings are errors in this repository.

## 7. Generate coverage when needed

CI generates the coverage report with:

```sh
epmd -daemon
rebar3 as test do eunit, covertool generate
```

The machine-readable report is written under
`_build/test/covertool/*.covertool.xml`.

## 8. Review the final diff

Before handing off the change:

```sh
git status --short
git diff --check
git diff --stat
git diff
```

Confirm that the diff contains only the intended files and report every command
you actually ran. If a relevant check was skipped, state why.

## 9. Capture the current TUI Home screenshot

Use this runbook only when replacing the documentation Home image.

1. On macOS or Linux, create the destination and use an OTP 29 terminal at
   least 150 columns wide and 30 rows high:

   ```sh
   mkdir -p docs/images
   ```

2. From the repository root, paste this complete block:

```sh
rebar3 escriptize
sh -eu <<'SH'
COOKIE=observer_cli_docs
NAME=observer_cli_docs_$$

cleanup() {
  kill "$TARGET_PID" 2>/dev/null || true
  wait "$TARGET_PID" 2>/dev/null || true
}
trap cleanup EXIT HUP INT TERM

ERL_CRASH_DUMP=/tmp/observer_cli_docs_erl_crash.dump \
erl -noshell -sname "$NAME" -setcookie "$COOKIE" -eval '
Parent = self(),
MemoryHog = spawn(fun() ->
    Data = lists:duplicate(150000, {docs_payload, <<"observer_cli documentation">>}),
    Parent ! memory_ready,
    receive stop -> length(Data) end
end),
register(docs_memory_hog, MemoryHog),
receive memory_ready -> ok end,
Mailbox = spawn(fun() -> receive stop -> ok end end),
register(docs_busy_mailbox, Mailbox),
[Mailbox ! {docs_event, N, <<"queued">>} || N <- lists:seq(1, 2000)],
Cpu = spawn(fun F() -> lists:sum(lists:seq(1, 20000)), timer:sleep(10), F() end),
register(docs_cpu_hotspot, Cpu),
_EtsOwner = spawn(fun() ->
    T = ets:new(docs_sessions, [named_table, public, set, {read_concurrency, true}]),
    ets:insert(T, [{N, active, <<"docs">>} || N <- lists:seq(1, 20000)]),
    Parent ! ets_ready,
    receive stop -> ok end
end),
receive ets_ready -> ok end,
{ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
{ok, {_, TcpPort}} = inet:sockname(Listen),
_Acceptor = spawn(fun() ->
    {ok, Accepted} = gen_tcp:accept(Listen),
    Parent ! tcp_ready,
    receive stop -> gen_tcp:close(Accepted) end
end),
{ok, Client} = gen_tcp:connect({127,0,0,1}, TcpPort, [binary, {active, false}]),
receive tcp_ready -> ok end,
ok = gen_tcp:send(Client, binary:copy(<<"observer_cli_docs">>, 4096)),
{ok, Socket} = socket:open(inet, stream, tcp),
ok = socket:bind(Socket, #{family => inet, addr => loopback, port => 0}),
ok = socket:listen(Socket),
io:format("observer_cli docs target ready: ~p~n", [node()]),
receive stop -> ok end.
' &
TARGET_PID=$!
sleep 1
./_build/default/bin/observer_cli tui "$NAME" "$COOKIE" 1500 </dev/tty
SH
```

3. Wait for two refreshes. Capture the full TUI from the selected Home tab
   through the footer. The process table must include `docs_memory_hog`,
   `docs_busy_mailbox` with a nonzero message queue, and `docs_cpu_hotspot`.

4. Save the image as `docs/images/tui-home.png`.

5. Type `q` and press **Enter**. The script exits and removes the disposable
   target. The cookie in the block is documentation data, not a production
   secret.

6. Replace the screenshot TODO blocks in `README.md` and
   `docs/tutorials/first-tui-session.md` with the image where it helps the
   reader. Run `rebar3 docs` and inspect `doc/readme.html` and
   `doc/first-tui-session.html` before committing the asset.
