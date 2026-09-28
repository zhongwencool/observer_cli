#!/usr/bin/env python3
"""Validate emitted CLI responses, schema packaging, and deliberate negative cases.

Test tooling only. Install scripts/schema-requirements.txt in a virtual environment.
This does not connect to targets or invoke commands; fixtures must already exist.
"""
from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path
import sys
import zipfile

from jsonschema import Draft202012Validator, FormatChecker


def load(path: Path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def pointer(root, path):
    value = root
    for part in path.split("/")[1:]:
        token = part.replace("~1", "/").replace("~0", "~")
        if isinstance(value, list):
            if not token.isdigit() or (token.startswith("0") and token != "0"):
                raise ValueError(f"noncanonical array index in evidence pointer: {path}")
            value = value[int(token)]
        else:
            value = value[token]
    return value


def semantic_errors(response):
    """Mirror portable relationships, not the target/controller safety validator."""
    data = response.get("data")
    if not isinstance(data, dict):
        return []
    errors = []
    for finding in data.get("findings", []):
        for evidence in finding.get("evidence", []):
            try:
                observed = pointer(response, evidence["path"])
            except (KeyError, IndexError, TypeError, ValueError):
                errors.append("evidence pointer does not resolve in this response")
                continue
            # Scheduler evidence intentionally references the complete window list.
            if isinstance(observed, (int, float)) and observed != evidence.get("observed"):
                errors.append("numeric evidence observed value differs from pointer target")
    trace = data.get("trace")
    if isinstance(trace, dict):
        if data.get("reason") != trace.get("reason"):
            errors.append("trace reason differs from data reason")
        for event in trace.get("events", []):
            if event.get("tracee") != trace.get("tracee") or event.get("mfa") != trace.get("mfa"):
                errors.append("trace event selector differs from capture selector")
    return errors


def check_response(validator, response):
    errors = [f"/{'/'.join(map(str, error.absolute_path))}: {error.message[:250]}"
              for error in validator.iter_errors(response)]
    if not errors:
        errors.extend(semantic_errors(response))
    return errors


def paths(value, prefix=()):
    if isinstance(value, dict):
        for key, child in value.items():
            yield from paths(child, prefix + (key,))
    elif isinstance(value, list):
        for index, child in enumerate(value[:2]):
            yield from paths(child, prefix + (index,))
    else:
        yield prefix, value


def replace(root, path, value):
    item = root
    for key in path[:-1]:
        item = item[key]
    item[path[-1]] = value


def negative_cases(response):
    """Exercise envelope, domain types, enums, evidence, and cleanup guarantees."""
    mutations = [(("outcome",), "invented_outcome"), (("command",), "unregistered_command")]
    if response.get("meta", {}).get("capture") is not None:
        mutations.append((("meta", "capture", "started_at"), "not-a-timestamp"))
    keys = {"memory_bytes", "total_bytes", "interval_ms", "status", "truncated",
            "reason_code", "scanned_count", "returned_count", "ruleset_version",
            "sample_index", "threshold", "arity", "trace_complete", "cleanup_confirmed",
            "size_bytes", "size_bits", "visited_node_count", "returned_lines",
            "content_truncated", "requested_lines", "port_limit", "utilization_ratio"}
    for path, _ in paths(response.get("data"), ("data",)):
        if path[-1] in keys:
            mutations.append((path, {"invalid": "scalar replaced with object"}))
    for path, value in paths(response.get("data"), ("data",)):
        if path[-1] == "path" and isinstance(value, str) and value.startswith("/data/"):
            mutations.append((path, "/data/absent_evidence_target"))
    for path, value in mutations:
        altered = copy.deepcopy(response)
        replace(altered, path, value)
        yield path, altered
    missing = copy.deepcopy(response)
    del missing["issues"]
    yield ("issues",), missing


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schema", type=Path, default=Path("priv/schema/observer_cli.cli.v1.schema.json"))
    parser.add_argument("--fixtures", type=Path, help="Directory of emitted .json response fixtures")
    parser.add_argument("--escript", type=Path, action="append", default=[], help="Verify embedded schema equals the normative source (repeatable)")
    parser.add_argument("--self-test", action="store_true", help="Require deliberate corrupted response fixtures to fail")
    args = parser.parse_args()
    if not args.fixtures and not args.escript:
        parser.error("provide --fixtures or --escript; checking the schema alone is insufficient")
    schema = load(args.schema)
    Draft202012Validator.check_schema(schema)
    validator = Draft202012Validator(schema, format_checker=FormatChecker(formats=["date-time"]))
    failures = []
    for executable in args.escript:
        with zipfile.ZipFile(executable) as archive:
            names = [name for name in archive.namelist() if name.endswith("/priv/schema/observer_cli.cli.v1.schema.json")]
            if len(names) != 1:
                failures.append(f"{executable}: expected exactly one packaged CLI schema")
            elif json.loads(archive.read(names[0])) != schema:
                failures.append(f"{executable}: packaged CLI schema differs from source")
    count = negative_count = 0
    if args.fixtures:
        files = sorted(args.fixtures.glob("*.json"))
        if not files:
            failures.append(f"{args.fixtures}: no response fixtures found")
        for file in files:
            response = load(file)
            count += 1
            errors = check_response(validator, response)
            failures.extend(f"{file.name}: {error}" for error in errors)
            if args.self_test and not errors:
                for path, malformed in negative_cases(response):
                    negative_count += 1
                    if not check_response(validator, malformed):
                        failures.append(f"{file.name}: corrupted /{'/'.join(map(str, path))} was accepted")
    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print(f"Validated {count} emitted responses, rejected {negative_count} negative cases, verified {len(args.escript)} packaged schemas.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
