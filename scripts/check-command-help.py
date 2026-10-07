#!/usr/bin/env python3
"""Check every help page and shell example using an OTP 27+ JSON controller."""

import json
from pathlib import Path
import subprocess
import sys
import tempfile


BIN = str(Path(sys.argv[1]).resolve())


def run(*args):
    result = subprocess.run([BIN, *args], capture_output=True, text=True, check=True)
    assert not result.stderr, (args, result.stderr)
    return result.stdout


commands = json.loads(run("describe", "--full", "--json"))["data"]["commands"]
assert len(commands) == 23
examples_checked = 0
max_width = 0

with tempfile.TemporaryDirectory(prefix="observer-cli-help-") as directory:
    for path in [["inspect"], ["trace"]] + [command["argv"] for command in commands]:
        help_text = run(*path, "--help")
        assert help_text == run("help", *path), path
        width = max(map(len, help_text.splitlines()))
        assert width <= 80, (path, width)
        max_width = max(max_width, width)
        assert "\nUsage:\n" in help_text, path
        assert "\nNotes:" not in help_text, path
        if len(path) == 1 and path[0] in ("inspect", "trace"):
            continue

        descriptor = next(command for command in commands if command["argv"] == path)
        example_text = help_text.split("\nExamples:\n", 1)[1]
        example_text = example_text.split("\nCommand metadata:", 1)[0]
        shell_examples = example_text.replace("\\\n", "").strip().splitlines()
        assert len(shell_examples) == len(descriptor["examples"]), path
        for example, expected in zip(shell_examples, descriptor["examples"]):
            # A shell function captures argv instead of executing the real CLI.
            # Thus even trace/consent examples can be checked without side effects.
            stub = 'observer_cli() { printf "%s\\0" "$@"; };\n'
            result = subprocess.run(
                ["/bin/sh", "-c", stub + example],
                cwd=directory, capture_output=True, check=True,
            )
            assert not result.stderr, (path, result.stderr)
            actual = result.stdout.decode().split("\0")[:-1]
            assert actual == expected, (path, actual, expected)
            examples_checked += 1

print(f"ok - 25 help pages, maximum {max_width} columns; "
      f"{examples_checked} shell examples preserve argv")
