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
        for key in ("ERL_FLAGS", "ERL_AFLAGS", "ERL_ZFLAGS", "ERL_LIBS", MISSING_ENV):
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
            set(envelope) == {"schema", "command", "outcome", "meta", "data", "issues"},
            f"{label}: unexpected envelope fields",
        )
        require(envelope["schema"] == "observer_cli.cli/v1", f"{label}: unexpected schema")
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
        evaluation = (
            'erlang:set_cookie(node(), list_to_atom(os:getenv("' + COOKIE_ENV + '"))), '
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
            ["erl", "+S", "2:2", "-pa", str(ebin), str(recon), "-name", self.node, "-noshell", "-eval", evaluation],
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
        for command in (["snapshot"], ["schedulers", "--duration", "2s"],
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
        print("ok - OTP 26 compatibility only; full JSON workflow NOT RUN (requires OTP 27+)")

    def workflow(self):
        before = list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*"))
        self.run("describe-offline", ["describe", "--json"], json_output=True)
        self.run("describe-trace", ["describe", "trace", "call", "--json"], json_output=True)
        stdout, stderr = self.run("describe-schema", ["describe", "--schema", "--json"])
        require(not stderr, "schema export emitted stderr")
        source = json.loads((ROOT / "priv/schema/observer_cli.cli.v1.schema.json").read_text())
        require(json.loads(stdout) == source, "bundled schema differs from source schema")
        require(before == list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*")), "offline discovery modified context")
        for arguments in (["trace"], ["trace", "bogus"]):
            envelope = self.run("invalid-" + "-".join(arguments), [*arguments, "--json"], expected=(2,), json_output=True)
            require(envelope["command"] is None, "invalid trace action must have command:null")
        self.run("verbose-json-conflict", ["memory", "--verbose", "--json"], expected=(2,), json_output=True)
        self.run("orphan-cookie-option", ["memory", "--cookie-env", MISSING_ENV, "--json"], expected=(2,), json_output=True)
        self.start_target()
        included = self.run("snapshot-included", ["snapshot", *self.target, "--include-identifiers", "--json"], json_output=True)
        require(included["meta"]["target"]["node"] == self.node, "explicit target was not used")
        redacted = self.run("snapshot-redacted", ["snapshot", *self.target, "--json"], json_output=True)
        require(self.node not in json.dumps(redacted), "default snapshot exposed target identifier")
        self.run("diagnose", ["diagnose", *self.target, "--json"], expected=(0, 1, 3), json_output=True)
        processes = self.run("processes-included", ["processes", "--sort", "memory", "--limit", "200", *self.target,
                                                  "--include-identifiers", "--json"], json_output=True)
        require(any(row.get("pid") == self.pid for row in processes["data"]["items"]), "stable owned worker missing from inventory")
        detail = self.run("process-detail", ["process", self.pid, *self.target, "--include-identifiers", "--json"], json_output=True)
        require(self.pid in json.dumps(detail), "follow-up process did not resolve the selected worker")
        redacted = self.run("processes-redacted", ["processes", "--limit", "200", *self.target, "--redact", "--json"], json_output=True)
        require(self.pid not in json.dumps(redacted), "redacted inventory exposed a raw PID")
        compact, stderr = self.run("memory-compact", ["memory", *self.target])
        verbose, verbose_stderr = self.run("memory-verbose", ["memory", *self.target, "--verbose"])
        require(not stderr and not verbose_stderr, "successful text command wrote stderr")
        require("not host RSS" in compact and "outcome=complete" in compact, "compact memory omitted semantics")
        require(len(compact.splitlines()) < len(verbose.splitlines()), "verbose did not expose more evidence")
        logs = self.run("logs-partial", ["logs", "--handler", "agent_smoke", "--tail", "200", *self.target, "--json"],
                        expected=(3,), json_output=True)
        require(logs["outcome"] == "partial", "byte-capped capture did not report partial")
        require("byte_cap" in json.dumps(logs), "byte cap reason missing from JSON")
        text, stderr = self.run("logs-partial-text", ["logs", "--handler", "agent_smoke", "--tail", "200", *self.target], expected=(3,))
        require(not stderr and "outcome=partial" in text and "byte_cap" in text, "partial logs text hides outcome/cap")
        require(text.index("byte_cap") < text.index("UNTRUSTED LOG CONTENT"), "log warning appears after untrusted content")
        require(before == list(Path(self.env["XDG_CONFIG_HOME"]).rglob("*")), "explicit stateless commands changed saved context")
        cookie_file = self.directory / "中文.cookie"
        cookie_file.write_text(self.cookie + "\n", encoding="ascii")
        cookie_file.chmod(0o600)
        self.run("unicode-cookie-connect", ["connect", "--node", self.node, "--cookie-file", str(cookie_file), "--json"], json_output=True)
        self.run("unicode-cookie-status", ["status", "--json"], json_output=True)
        self.run("env-cookie-connect", ["connect", *self.target, "--json"], json_output=True)
        missing_env = self.env.copy()
        del missing_env[COOKIE_ENV]
        status = self.run("missing-cookie-status", ["status", "--json"], expected=(3,), json_output=True, env=missing_env)
        require(COOKIE_ENV in json.dumps(status), "missing credential status did not identify safe cookie source")
        self.run("disconnect", ["disconnect", "--json"], json_output=True)
        print("ok - complete isolated agent workflow, text safety, redaction and saved-context checks")


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
            stdout, stderr = harness.run("json-capability", ["snapshot", "--node", "never_started@127.0.0.1",
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
