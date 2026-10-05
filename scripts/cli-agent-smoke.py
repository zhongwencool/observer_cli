#!/usr/bin/env python3
"""Exercise the shipped CLI as an agent against an owned, temporary BEAM node.

No third-party Python packages, existing node, saved user context, or production
credentials are used. --fixtures saves actual JSON envelopes for schema checks.
OTP 26 runs a separately reported compatibility check, not the JSON workflow.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import secrets
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
OUTPUT_LIMIT = 2 * 1024 * 1024
TIMEOUT = 45
COOKIE_ENV = "OBSERVER_CLI_AGENT_SMOKE_COOKIE"
MISSING_ENV = "OBSERVER_CLI_AGENT_SMOKE_ABSENT"


class Failure(Exception):
    pass


def require(condition, message):
    if not condition:
        raise Failure(message)


def terminate(process):
    if process.poll() is None:
        try:
            os.killpg(process.pid, signal.SIGTERM)
            process.wait(timeout=5)
        except (ProcessLookupError, subprocess.TimeoutExpired):
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=5)


def interrupted(signum, _frame):
    raise Failure(f"interrupted by signal {signum}")


class Harness:
    def __init__(self, directory, binary, fixtures):
        self.directory = directory
        self.binary = binary
        self.fixtures = fixtures
        self.cookie = secrets.token_hex(24)
        self.env = os.environ.copy()
        for key in ("ERL_FLAGS", "ERL_AFLAGS", "ERL_ZFLAGS", "ERL_LIBS", "OBSERVER_CLI_NODE", "OBSERVER_CLI_COOKIE", "OBSERVER_CLI_COOKIE_FILE", "OBSERVER_CLI_NAME_MODE", MISSING_ENV):
            self.env.pop(key, None)
        self.env.update(
            HOME=str(directory / "home"),
            XDG_CONFIG_HOME=str(directory / "config"),
            ERL_CRASH_DUMP=str(directory / "erl_crash.dump"),
            TERM="dumb",
            COLUMNS="80",
        )
        self.env[COOKIE_ENV] = self.cookie
        Path(self.env["HOME"]).mkdir()
        Path(self.env["XDG_CONFIG_HOME"]).mkdir()
        self.processes = []
        self.handles = []
        self.fixture_index = 0

    def close(self):
        for process in reversed(self.processes):
            terminate(process)
        for handle in self.handles:
            handle.close()

    def start(self, command, label):
        output = open(self.directory / f"{label}.stdout", "w+b")
        errors = open(self.directory / f"{label}.stderr", "w+b")
        self.handles.extend((output, errors))
        process = subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=output,
            stderr=errors,
            env=self.env,
            cwd=self.directory,
            start_new_session=True,
        )
        self.processes.append(process)
        return process, output, errors

    def run(self, label, args, expected=(0,), json_output=False, env=None):
        old_env = self.env
        if env is not None:
            self.env = env
        try:
            process, output, errors = self.start([str(self.binary), *args], label)
        finally:
            self.env = old_env
        deadline = time.monotonic() + TIMEOUT
        while process.poll() is None:
            if time.monotonic() >= deadline:
                terminate(process)
                raise Failure(f"{label}: command exceeded {TIMEOUT}s")
            if max(os.fstat(output.fileno()).st_size, os.fstat(errors.fileno()).st_size) > OUTPUT_LIMIT:
                terminate(process)
                raise Failure(f"{label}: output exceeded {OUTPUT_LIMIT} bytes")
            time.sleep(0.05)
        output.seek(0)
        errors.seek(0)
        stdout = output.read(OUTPUT_LIMIT + 1)
        stderr = errors.read(OUTPUT_LIMIT + 1)
        require(max(len(stdout), len(stderr)) <= OUTPUT_LIMIT, f"{label}: oversized output")
        require(self.cookie.encode() not in stdout + stderr, f"{label}: cookie value leaked")
        require(process.returncode in expected, f"{label}: expected exits {expected}, got {process.returncode}")
        if not json_output:
            return stdout.decode("utf-8"), stderr.decode("utf-8")
        require(not stderr, f"{label}: JSON success/error envelope mixed with stderr")
        try:
            envelope = json.loads(stdout)
        except (UnicodeDecodeError, json.JSONDecodeError) as exc:
            raise Failure(f"{label}: invalid JSON response") from exc
        require(isinstance(envelope, dict), f"{label}: expected JSON object")
        require(
            set(envelope) == {"schema", "command", "outcome", "summary", "assessment", "meta", "data", "issues", "next_actions"},
            f"{label}: unexpected envelope fields",
        )
        require(envelope["schema"] == "observer_cli.cli/v2", f"{label}: unexpected schema")
        if self.fixtures:
            self.fixture_index += 1
            path = self.fixtures / f"{self.fixture_index:02d}-{label}.json"
            path.write_text(json.dumps(envelope, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        print(f"ok - {label}")
        return envelope

    def start_target(self):
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            epmd_port = listener.getsockname()[1]
        self.env["ERL_EPMD_PORT"] = str(epmd_port)
        epmd = shutil.which("epmd")
        require(epmd is not None, "epmd not found")
        process, _, _ = self.start([epmd, "-port", str(epmd_port), "-address", "127.0.0.1"], "epmd")
        deadline = time.monotonic() + 10
        while True:
            require(process.poll() is None, "owned epmd failed to start")
            try:
                with socket.create_connection(("127.0.0.1", epmd_port), timeout=0.2):
                    break
            except OSError:
                require(time.monotonic() < deadline, "owned epmd startup timed out")
                time.sleep(0.05)
        self.node = f"observer_cli_agent_{os.getpid()}_{secrets.token_hex(3)}@127.0.0.1"
        log = self.directory / "capture.log"
        log.write_bytes((b"owned smoke log " + b"x" * 700 + b"\n") * 400)
        self.env["OBSERVER_SMOKE_LOG"] = str(log)
        self.env["OBSERVER_SMOKE_READY"] = str(self.directory / "ready")
        ebin = ROOT / "_build/default/lib/observer_cli/ebin"
        recon = ROOT / "_build/default/lib/recon/ebin"
        require((ebin / "observer_cli_snapshot.beam").is_file(), "build target modules before running this script")
        require(recon.is_dir(), "recon build directory is missing")
        fixture = self.directory / "observer_cli_smoke_state.erl"
        fixture.write_text("""-module(observer_cli_smoke_state).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
init(_) -> {ok,#{payload => <<"smoke-state-values-must-not-appear">>}}.
handle_call(_,_,S) -> {reply,ok,S}.
handle_cast(_,S) -> {noreply,S}.
handle_info(_,S) -> {noreply,S}.
terminate(_,_) -> ok.
code_change(_,S,_) -> {ok,S}.
""", encoding="ascii")
        subprocess.run(["erlc", "-o", str(self.directory), str(fixture)], env=self.env, check=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20)
        evaluation = (
            'erlang:set_cookie(node(), list_to_atom(os:getenv("' + COOKIE_ENV + '"))), '
            'gen_server:start_link({local,observer_cli_smoke_state_server},observer_cli_smoke_state,[],[]), '
            'Parent=self(), Worker=spawn(fun() -> '
            'register(observer_cli_agent_smoke_worker,self()), '
            'put(smoke_payload,lists:seq(1,200000)), Parent!{ready,self()}, '
            'receive after infinity -> ok end end), '
            'receive {ready,Worker} -> ok after 5000 -> error(worker_timeout) end, '
            'ok=logger:add_handler(agent_smoke,logger_std_h,#{config=>#{type=>file,'
            'file=>os:getenv("OBSERVER_SMOKE_LOG"),modes=>[append,raw]}}), '
            'ok=file:write_file(os:getenv("OBSERVER_SMOKE_READY"),pid_to_list(Worker)), '
            'receive after infinity -> ok end.'
        )
        process, stdout, stderr = self.start(
            ["erl", "+S", "2:2", "-pa", str(ebin), str(recon), str(self.directory), "-name", self.node, "-noshell", "-eval", evaluation],
            "target",
        )
        deadline = time.monotonic() + 15
        ready = self.directory / "ready"
        while not ready.exists():
            require(process.poll() is None, "owned target failed to start (no user node was used)")
            require(time.monotonic() < deadline, "owned target startup timed out")
            require(max(os.fstat(stdout.fileno()).st_size, os.fstat(stderr.fileno()).st_size) <= OUTPUT_LIMIT,
                    "owned target startup output exceeded limit")
            time.sleep(0.05)
        self.pid = ready.read_text(encoding="ascii")
        self.target = ["--node", self.node, "--cookie-env", COOKIE_ENV]

    def compatibility(self):
        before = list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*"))
        for command in (["inspect", "vm"], ["inspect", "scheduler", "--window", "2s"],
                        ["trace", "call", "erlang:node/0", "--pid", "<0.1.0>", "--replace-existing-trace"]):
            stdout, stderr = self.run(
                "otp26-" + command[0],
                [*command, "--node", "never_started@127.0.0.1", "--cookie-env", MISSING_ENV, "--json"],
                expected=(2,),
            )
            require(not stdout and "JSON output requires OTP 27" in stderr,
                    "OTP 26: output preflight did not take precedence over missing credentials")
        self.run("otp26-describe-term", ["describe", "--format", "term"])
        require(before == list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*")), "OTP 26 checks modified context")
        self.start_target()
        text, stderr = self.run("otp26-vm-text", ["inspect", "vm", *self.target])
        require(not stderr and "not host RSS" in text, "OTP 26 remote text inspection failed")
        self.run("otp26-memory-term", ["inspect", "memory", *self.target, "--format", "term"])
        print("ok - OTP 26 remote text/term and local JSON preflight; JSON workflow NOT RUN (requires OTP 27+)")

    def workflow(self):
        # A deliberately invalid legacy selector must remain byte-for-byte intact.
        legacy = Path(self.env["HOME"]) / "Library/Application Support/observer_cli/context.etf"
        legacy.parent.mkdir(parents=True)
        sentinel = b"legacy-selector-must-not-be-read-or-written"
        legacy.write_bytes(sentinel)
        before = list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*"))
        index = self.run("describe-offline", ["describe", "--json"], json_output=True)
        require(len(index["data"]["entries"]) == 5, "discovery did not return the incremental index")
        self.run("describe-trace", ["describe", "trace", "call", "--json"], json_output=True)
        self.run("describe-full", ["describe", "--full", "--json"], json_output=True)
        stdout, stderr = self.run("describe-schema", ["describe", "--schema", "--json"])
        require(not stderr, "schema export emitted stderr")
        source = json.loads((ROOT / "priv/schema/observer_cli.cli.v2.schema.json").read_text())
        require(json.loads(stdout) == source, "bundled schema differs from source schema")
        for old in ["connect", "status", "disconnect", "diagnose", "snapshot", "processes", "memory"]:
            self.run("removed-" + old, [old, "--json"], expected=(2,), json_output=True)
        for name, args in [
            ("invalid-trace", ["trace", "bogus"]),
            ("missing-mfa", ["trace", "call", "--pid", "<0.1.0>", "--replace-existing-trace"]),
            ("missing-consent", ["trace", "call", "erlang:node/0", "--pid", "<0.1.0>"]),
            ("redacted-selector", ["inspect", "process", "--pid", "pid-1"]),
            ("rate-without-window", ["inspect", "process", "--sort", "reductions-rate"]),
            ("state-without-consent", ["inspect", "state", "--name", "init", "--behavior", "gen_server"]),
            ("verbose-json-conflict", ["inspect", "memory", "--verbose"]),
            ("orphan-cookie", ["inspect", "memory", "--cookie-env", MISSING_ENV]),
            ("logs-redaction", ["inspect", "logs", "--redact"]),
        ]:
            self.run(name, [*args, "--json"], expected=(2,), json_output=True)
        self.start_target()
        included = self.run("vm-included", ["inspect", "vm", *self.target, "--json"], json_output=True)
        require(included["meta"]["target"]["node"] == self.node, "explicit target was not used")
        redacted = self.run("vm-redacted", ["inspect", "vm", *self.target, "--redact", "--json"], json_output=True)
        require(self.node not in json.dumps(redacted), "redacted VM capture exposed target identity")
        started = time.monotonic()
        check = self.run("check-default", ["check", *self.target, "--json"], expected=(0, 3), json_output=True)
        require(check["meta"]["capture"]["requested_window_ms"] == 15000, "default check was not 15 seconds")
        require(time.monotonic() - started >= 15, "default observation did not span its promised window")
        for focus in ["cpu", "memory", "mailbox", "connections"]:
            focused = self.run("check-" + focus, ["check", focus, "--window", "5s", *self.target, "--json"], expected=(0, 3), json_output=True)
            require(focused["data"]["context"], "focused check omitted evidence")
        processes = self.run("process-list", ["inspect", "process", "--sort", "memory", "--limit", "200", *self.target, "--json"], json_output=True)
        item = next((row for row in processes["data"]["items"] if row.get("pid") == self.pid), None)
        require(item is not None, "owned worker missing from inventory")
        selector = item["selector"]
        require(selector == {"kind": "pid", "value": self.pid}, "inventory did not provide the typed selector")
        detail = self.run("process-detail", ["inspect", "process", "--pid", selector["value"], *self.target, "--json"], json_output=True)
        require(detail["data"]["pid"] == self.pid, "follow-up did not preserve the selected identity")
        self.run("process-name", ["inspect", "process", "--name", "observer_cli_agent_smoke_worker", *self.target, "--json"], json_output=True)
        for metric in ["memory", "memory-change", "reductions-rate"]:
            ranked = self.run("process-window-" + metric, ["inspect", "process", "--sort", metric, "--window", "250ms", *self.target, "--json"], json_output=True)
            require(ranked["data"]["sort"] == metric, "metric meaning changed with the window")
        for resource, metric in [("network", "oct"), ("network", "oct-change"), ("socket", "io-rate")]:
            self.run(resource + "-window-" + metric, ["inspect", resource, "--sort", metric, "--window", "250ms", *self.target, "--json"], json_output=True)
        redacted = self.run("process-redacted", ["inspect", "process", "--limit", "200", *self.target, "--redact", "--json"], json_output=True)
        require(self.pid not in json.dumps(redacted), "redacted inventory exposed a raw PID")
        require(all(row["selector"] is None for row in redacted["data"]["items"]), "redacted rows expose executable selectors")
        compact, stderr = self.run("memory-compact", ["inspect", "memory", *self.target])
        verbose, verbose_stderr = self.run("memory-verbose", ["inspect", "memory", *self.target, "--verbose"])
        require(not stderr and not verbose_stderr and "not host RSS" in compact, "compact memory omitted semantics")
        require(len(compact.splitlines()) < len(verbose.splitlines()), "verbose did not expose more evidence")
        logs = self.run("logs-partial", ["inspect", "logs", "--handler", "agent_smoke", "--tail", "200", *self.target, "--json"], expected=(3,), json_output=True)
        require(logs["outcome"] == "partial" and "byte_cap" in json.dumps(logs), "byte-capped logs hide partial coverage")
        require(not logs["next_actions"], "log content generated executable recommendations")
        text, stderr = self.run("logs-partial-text", ["inspect", "logs", "--handler", "agent_smoke", "--tail", "200", *self.target], expected=(3,))
        require(not stderr and text.index("byte_cap") < text.index("UNTRUSTED LOG CONTENT"), "log warning appears after untrusted content")
        state = self.run("state-with-consent", ["inspect", "state", "--name", "observer_cli_smoke_state_server",
                         "--behavior", "gen_server", "--allow-state-read", *self.target, "--json"], json_output=True)
        require("smoke-state-values-must-not-appear" not in json.dumps(state), "state shape leaked a business value")
        self.run("supervision", ["inspect", "supervision", "--app", "kernel", *self.target, "--json"], json_output=True)
        ports = self.run("port-list", ["inspect", "port", *self.target, "--json"], json_output=True)
        if ports["data"]["items"]:
            port = ports["data"]["items"][0]["selector"]
            require(port["kind"] == "port", "port selector kind missing")
            self.run("port-detail", ["inspect", "port", "--id", port["value"], *self.target, "--json"], json_output=True)
        self.run("trace-owned", ["trace", "call", "erlang:node/0", "--pid", self.pid, "--duration", "100ms",
                                  "--limit", "1", "--replace-existing-trace", *self.target, "--json"], json_output=True)
        cookie_file = self.directory / "中文.cookie"
        cookie_file.write_text(self.cookie + "\n", encoding="ascii")
        cookie_file.chmod(0o600)
        self.run("unicode-cookie-file", ["inspect", "vm", "--node", self.node, "--cookie-file", str(cookie_file), "--json"], json_output=True)
        shell_env = self.env.copy()
        shell_env.update(OBSERVER_CLI_NODE=self.node, OBSERVER_CLI_COOKIE=self.cookie)
        self.run("shell-environment", ["inspect", "vm", "--json"], json_output=True, env=shell_env)
        wrong = shell_env.copy()
        wrong["OBSERVER_CLI_NODE"] = "wrong@host"
        explicit = self.run("explicit-wins", ["inspect", "vm", *self.target, "--json"], json_output=True, env=wrong)
        require(explicit["meta"]["target"]["node"] == self.node, "explicit target inherited environment state")
        self.run("no-implicit-cookie", ["inspect", "vm", "--node", self.node, "--json"], expected=(2,), json_output=True, env=shell_env)
        missing = self.env.copy()
        del missing[COOKIE_ENV]
        error = self.run("missing-cookie", ["inspect", "vm", *self.target, "--json"], expected=(3,), json_output=True, env=missing)
        require(COOKIE_ENV in json.dumps(error), "credential error omitted safe source-name context")
        require(legacy.read_bytes() == sentinel, "v3 touched a legacy selector")
        require(before == list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*")), "v3 wrote user-global state")
        print("ok - complete isolated v3 workflow, focused evidence, typed follow-up, fixed metrics, redaction and stateless targets")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fixtures", type=Path, help="write emitted JSON envelopes to this directory")
    parser.add_argument("--require-json", action="store_true", help="fail instead of running only OTP 26 compatibility checks")
    arguments = parser.parse_args()
    binary = Path(os.environ.get("OBSERVER_CLI_BIN", ROOT / "_build/default/bin/observer_cli")).resolve()
    require(binary.is_file(), "build the escript before running this script")
    require(shutil.which("erl") is not None, "erl not found")
    fixtures = arguments.fixtures.resolve() if arguments.fixtures else None
    if fixtures:
        fixtures.mkdir(parents=True, exist_ok=True)
        require(not list(fixtures.glob("*.json")), "fixture directory already contains JSON; use a fresh directory")
    for signum in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(signum, interrupted)
    with tempfile.TemporaryDirectory(prefix="observer-cli-agent-smoke-") as directory:
        harness = Harness(Path(directory), binary, fixtures)
        try:
            stdout, stderr = harness.run("json-capability", ["inspect", "vm", "--node", "never_started@127.0.0.1",
                                                           "--cookie-env", MISSING_ENV, "--json"], expected=(2, 3))
            if not stdout and "JSON output requires OTP 27" in stderr:
                require(not arguments.require_json, "JSON workflow requires OTP 27+; OTP 26 detected")
                harness.compatibility()
            else:
                harness.workflow()
        finally:
            harness.close()


if __name__ == "__main__":
    try:
        main()
    except (Failure, OSError, UnicodeError, ValueError) as error:
        print(f"not ok - {error}", file=sys.stderr)
        sys.exit(1)
