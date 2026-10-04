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


def schema_unit_errors(schema):
    """Check unit annotations separately: valid numeric types do not imply valid units."""
    errors = []
    definitions = schema.get("$defs", {})
    opaque_counter = "Opaque scheduler wall-time counter units; compare only within the same sampling window, not elapsed milliseconds."

    def walk(value, path=(), field=None):
        if isinstance(value, dict):
            description = value.get("description", "")
            lower = description.lower()
            location = "/" + "/".join(map(str, path))
            is_counter = path == ("$defs", "schedulerCounter", "properties", "value")
            if "millisecond" in lower and not (field or "").endswith("_ms"):
                if not (is_counter and description == opaque_counter):
                    errors.append(f"{location}: time unit on a non-millisecond field")
            if "monotonic" in lower and "monotonic" not in (field or ""):
                errors.append(f"{location}: monotonic time annotation on a non-time field")
            if "garbage_collection_info" in path and description:
                errors.append(f"{location}: raw OTP GC fields must not inherit guessed units")
            if description and any(unit in lower for unit in ("bytes", "milliseconds", "count", "words", "bits")):
                if path and (len(path) < 2 or path[-2] != "properties"):
                    errors.append(f"{location}: numeric unit annotation belongs on its named property, not a shared subschema")
            for key, child in value.items():
                if key == "properties":
                    for name, prop in child.items():
                        walk(prop, path + (key, name), name)
                elif key != "description":
                    walk(child, path + (key,), field)
        elif isinstance(value, list):
            for index, child in enumerate(value):
                walk(child, path + (index,), field)

    walk(schema)

    def require(path, expected):
        value = schema
        try:
            for part in path:
                value = value[part]
            actual = value.get("description")
        except (KeyError, IndexError, TypeError):
            actual = None
        if actual != expected:
            location = "/" + "/".join(map(str, path))
            errors.append(f"{location}: expected unit annotation {expected!r}, got {actual!r}")

    def prop(name, field, expected):
        require(("$defs", name, "properties", field), expected)

    prop("schedulerCounter", "value", opaque_counter)
    require(("$defs", "portOption", "properties", "value", "properties", "seconds"), "Seconds.")
    for name in ("mnesiaItem", "etsItem"):
        prop(name, "size", "Table object count.")
    for field in ("memory_bytes", "disk_bytes"):
        prop("mnesiaItem", field, "Bytes.")
    require(("$defs", "stateShape", "oneOf", 1, "properties", "size_bytes"), "Bytes.")
    require(("$defs", "stateShape", "oneOf", 2, "properties", "size_bits"), "Bits.")
    require(("$defs", "stateShape", "oneOf", 5, "properties", "size"),
            "Container element count; null when the list size is unavailable.")
    for field in ("memory", "binary_memory", "total_heap_size"):
        prop("processItem", field + "_delta", "Signed byte change over the measured sample interval.")
        prop("processItem", field + "_per_second", "Bytes per second over the measured sample interval.")
    for field, unit in (("message_queue_len", "message count"), ("reductions", "BEAM reduction count")):
        delta_description = ("BEAM reduction count increase over the measured sample interval; reset counters are excluded."
                             if field == "reductions" else f"Signed change in {unit} over the measured sample interval.")
        prop("processItem", field + "_delta", delta_description)
        prop("processItem", field + "_per_second", f"{(unit[0].upper() + unit[1:])} per second over the measured sample interval.")
    for name in ("trendMetrics", "trendRates"):
        for field in definitions.get(name, {}).get("properties", {}):
            if field.endswith("_state"):
                continue
            units = {"memory_words": "machine words", "message_queue_len": "messages", "size": "table objects",
                     **{name: "bytes" for name in ("memory_bytes", "queue_size", "memory", "input", "output",
                         "total_bytes", "processes_bytes", "processes_used_bytes", "system_bytes",
                         "atom_bytes", "atom_used_bytes", "binary_bytes", "code_bytes", "ets_bytes")}}
            if field not in units:
                errors.append(f"{name}/{field}: declare this metric's producer unit explicitly")
                continue
            unit = units[field]
            expected = (f"Signed change in {unit} over the measured sample interval." if name == "trendMetrics" else
                        f"{(unit[0].upper() + unit[1:])} per second over the measured sample interval.")
            prop(name, field, expected)
    return errors


def schema_unit_negative_cases(schema):
    """Mutate valid schema annotations without changing any accepted data types."""
    locations = [
        ("resourceCounts", ("properties", "process", "properties", "observed_count_including_observer")),
        ("mnesiaItem", ("properties", "memory_bytes", "anyOf", 0)),
        ("findingEvidence", ("properties", "sample_index")),
        ("allocator", ("properties", "cache_hit_rates", "items", "properties", "instance")),
        ("processItem", ("properties", "garbage_collection_info", "properties", "heap_size")),
        ("schedulerCounter", ("properties", "value")),
        ("portOption", ("properties", "value", "properties", "seconds")),
        ("stateShape", ("oneOf", 2, "properties", "size_bits")),
    ]
    for name, tail in locations:
        altered = copy.deepcopy(schema)
        value = altered["$defs"][name]
        for part in tail:
            value = value[part]
        value["description"] = "Milliseconds; measured intervals are not assumed equal to requested durations."
        yield name + "/" + "/".join(map(str, tail)), altered
    for name, field, description in [
        ("processItem", "memory_delta", "VM-local monotonic milliseconds; may be negative and cannot be compared across VM instances."),
        ("processItem", "reductions_delta", "VM-local monotonic milliseconds; may be negative and cannot be compared across VM instances."),
        ("trendRates", "memory_bytes", "Bytes; not a percentage of host physical memory."),
        ("trendRates", "memory_words", "Bytes per second over the measured sample interval."),
        ("trendMetrics", "size", "Signed change in bytes over the measured sample interval."),
        ("trendRates", "message_queue_len", "Messages."),
    ]:
        altered = copy.deepcopy(schema)
        altered["$defs"][name]["properties"][field]["description"] = description
        yield name + "/" + field, altered


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
    for finding in (response.get("assessment") or {}).get("findings", data.get("findings", [])):
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
    for path, value in paths(response.get("assessment"), ("assessment",)):
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
    parser.add_argument("--schema", type=Path, default=Path("priv/schema/observer_cli.cli.v2.schema.json"))
    parser.add_argument("--fixtures", type=Path, help="Directory of emitted .json response fixtures")
    parser.add_argument("--escript", type=Path, action="append", default=[], help="Verify embedded schema equals the normative source (repeatable)")
    parser.add_argument("--self-test", action="store_true", help="Require deliberate corrupted response fixtures to fail")
    args = parser.parse_args()
    if not args.fixtures and not args.escript:
        parser.error("provide --fixtures or --escript; checking the schema alone is insufficient")
    schema = load(args.schema)
    Draft202012Validator.check_schema(schema)
    validator = Draft202012Validator(schema, format_checker=FormatChecker(formats=["date-time"]))
    failures = schema_unit_errors(schema)
    unit_negative_count = 0
    if args.self_test and not failures:
        for path, malformed_schema in schema_unit_negative_cases(schema):
            unit_negative_count += 1
            if not schema_unit_errors(malformed_schema):
                failures.append(f"schema: corrupted unit annotation at {path} was accepted")
    for executable in args.escript:
        with zipfile.ZipFile(executable) as archive:
            names = [name for name in archive.namelist() if name.endswith("/priv/schema/observer_cli.cli.v2.schema.json")]
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
    print(f"Validated {count} emitted responses, rejected {negative_count} negative cases, verified {len(args.escript)} packaged schemas; rejected {unit_negative_count} schema-unit mutations.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
