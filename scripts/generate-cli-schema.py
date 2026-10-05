#!/usr/bin/env python3
"""Generate the standalone public schema from capture facts and the live registry.

Run after rebar3 escriptize. --check never writes source. No target is contacted.
"""
from __future__ import annotations
import argparse
import copy
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
PRIVATE = ROOT / 'priv/schema/observer_cli.capture.v1.schema.json'
PUBLIC = ROOT / 'priv/schema/observer_cli.cli.v2.schema.json'


def nullable_number(description, integer=False, nonnegative=False):
    number = {'type': 'integer' if integer else 'number'}
    if nonnegative:
        number['minimum'] = 0
    return {'anyOf': [number, {'type': 'null'}], 'description': description}


def enrich_capture(schema):
    """Add the new facts to the private wire contract without changing its shape."""
    d = schema['$defs']
    d['diagnosticCurrent'] = {
        'type': 'object', 'required': ['memory', 'processes'], 'additionalProperties': False,
        'properties': {
            'memory': {'anyOf': [{'$ref': '#/$defs/beamMemory'}, {'type': 'object', 'maxProperties': 0}]},
            'mailbox_peak': nullable_number('Maximum observed mailbox message count, excluding observer processes.', True, True),
            'processes': {'type': 'object', 'required': ['status', 'items'], 'additionalProperties': False,
                'properties': {
                    'status': {'enum': ['ok', 'unavailable', 'error', 'timeout']},
                    'sort_metric': {'enum': ['memory_bytes', 'message_queue_len']},
                    'items': {'type': 'array', 'maxItems': 20, 'items': {'$ref': '#/$defs/currentProcessItem'}}}}
        }}
    d['currentProcessItem'] = {
        'type': 'object', 'required': ['pid', 'memory_bytes', 'message_queue_len', 'reductions'],
        'additionalProperties': False, 'properties': {
            k: copy.deepcopy(d['processItem']['properties'][k])
            for k in ['pid', 'memory_bytes', 'message_queue_len', 'reductions']}}
    d['diagnosticContext']['properties']['current'] = {'$ref': '#/$defs/diagnosticCurrent'}
    for name in ['processesData', 'networkData', 'socketsData']:
        prop = d[name]['properties']['sort_semantics']
        prop['enum'] = list(dict.fromkeys(prop['enum'] + ['current', 'rate']))
    for name, keys in [('networkItem', ['recv_oct', 'send_oct', 'oct', 'recv_cnt', 'send_cnt', 'cnt']),
                       ('socketItem', ['io', 'read_bytes', 'write_bytes', 'packets', 'waits', 'fails', 'accepts'])]:
        props = d[name]['properties']
        for key in keys:
            unit = 'bytes' if key.endswith('oct') or key in ['io', 'read_bytes', 'write_bytes'] else 'events'
            props[key + '_delta'] = nullable_number(f'{unit.capitalize()} increase over the measured sample interval; reset counters are unavailable.', True, True)
            props[key + '_per_second'] = nullable_number(f'{unit.capitalize()} per second over the measured sample interval.', False, True)
        props['window_state'] = copy.deepcopy(props['state'])
        props['sample_metric_states'] = copy.deepcopy(props['metric_states'])
    return schema


def selector(kind, pattern):
    return {'oneOf': [{'type': 'null'}, {'type': 'object', 'required': ['kind', 'value'],
        'additionalProperties': False, 'properties': {'kind': {'const': kind},
            'value': {'type': 'string', 'pattern': pattern}}}]}


def build(private, commands):
    schema = copy.deepcopy(private)
    d = schema['$defs']
    paths = [command['name'] for command in commands]
    schema['$id'] = 'https://raw.githubusercontent.com/zhongwencool/observer_cli/v2.1.0/priv/schema/observer_cli.cli.v2.schema.json'
    schema['title'] = 'Observer CLI v2 task-first response'
    schema['description'] = 'Standalone public response contract. The controller also enforces target binding, evidence pointers, cleanup, redaction and cross-field relationships.'
    schema['required'] = ['schema', 'command', 'outcome', 'summary', 'assessment', 'data', 'meta', 'issues', 'next_actions']
    props = schema['properties']
    props['schema'] = {'const': 'observer_cli.cli/v2'}
    props['command'] = {'enum': [None, *paths]}
    props['summary'] = {'type': 'string'}
    props['assessment'] = {'oneOf': [{'type': 'null'}, {'$ref': '#/$defs/assessment'}]}
    props['next_actions'] = {'type': 'array', 'maxItems': 3, 'items': {'$ref': '#/$defs/nextAction'}}
    d['assessment'] = {'type': 'object', 'required': ['status', 'findings'], 'additionalProperties': False,
        'properties': {'status': {'enum': ['findings', 'no_findings', 'not_evaluated']},
            'findings': {'type': 'array', 'items': {'$ref': '#/$defs/finding'}}},
        'allOf': [{'if': {'properties': {'status': {'enum': ['no_findings', 'not_evaluated']}}},
                   'then': {'properties': {'findings': {'maxItems': 0}}}},
                  {'if': {'properties': {'status': {'const': 'findings'}}},
                   'then': {'properties': {'findings': {'minItems': 1}}}}]}
    d['checkData'] = copy.deepcopy(d['diagnoseData'])
    for key in ['findings', 'summary', 'next_actions']:
        d['checkData']['properties'].pop(key, None)
        if key in d['checkData']['required']:
            d['checkData']['required'].remove(key)
    d['capture']['properties']['requested_window_ms'] = {'type': 'integer', 'minimum': 1, 'description': 'Requested observation window in milliseconds; actual capture duration remains separate.'}
    d['pidSelector'] = selector('pid', r'^<0\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)>$')
    d['portSelector'] = selector('port', r'^#Port<0\.(0|[1-9][0-9]*)>$')
    for name in ['processItem', 'currentProcessItem', 'hotProcess', 'binaryHolderItem']:
        d[name]['properties']['selector'] = {'$ref': '#/$defs/pidSelector'}
    d['portItem']['properties']['selector'] = {'$ref': '#/$defs/portSelector'}
    # Not-found variants still receive an explicitly null selector.
    for name, ref in [('processData', 'pidSelector'), ('portData', 'portSelector')]:
        for branch in d[name].get('oneOf', []):
            if branch.get('type') == 'object':
                branch.setdefault('properties', {})['selector'] = {'$ref': '#/$defs/' + ref}
    for name in ['processesData', 'networkData', 'socketsData']:
        values = next(c['options'] for c in commands if name in c['output']['data_schemas'])
        sorts = next(o['enum'] for o in values if o['name'] == 'sort')
        d[name]['properties']['sort']['enum'] = sorts
    # The public catalog is incremental; full descriptor properties are unchanged.
    output = d['commandOutput']
    output['properties']['formats']['items']['enum'] = ['text', 'term', 'json', 'interactive']
    output['properties']['default'].pop('const', None)
    output['properties']['default']['enum'] = ['text', 'interactive']
    output['properties']['verbose_format'] = {'enum': ['text', None]}
    output['properties']['data_schemas'] = {'type': 'array', 'items': {'type': 'string'}}
    output['required'].append('data_schemas')
    d['commandConstraint'] = {'type': 'object', 'required': ['kind'],
        'properties': {'kind': {'type': 'string'}}, 'additionalProperties': True}
    def offline_policy(value):
        if isinstance(value, dict):
            if value.get('const') == 'context_or_catalog_metadata':
                value['const'] = 'offline_metadata'
            for child in value.values():
                offline_policy(child)
        elif isinstance(value, list):
            for child in value:
                offline_policy(child)
    offline_policy(d)
    d['describeData'] = {'oneOf': [
        {'type': 'object', 'required': ['entries', 'schema'], 'additionalProperties': False,
         'properties': {'schema': {'const': 'observer_cli.cli/v2'}, 'entries': {'type': 'array', 'minItems': 5, 'maxItems': 5,
            'items': {'type': 'object', 'required': ['name', 'summary'], 'additionalProperties': False,
                'properties': {'name': {'type': 'string'}, 'summary': {'type': 'string'}}}}}},
        {'type': 'object', 'required': ['commands'], 'additionalProperties': False,
         'properties': {'schema': {'const': 'observer_cli.cli/v2'}, 'commands': {'type': 'array', 'items': {'$ref': '#/$defs/commandDescriptor'}}}},
        {'$ref': '#/$defs/commandDescriptor'}]}
    # Replace the old public command union with paths from the registry.
    schema['oneOf'] = []
    for command in commands:
        is_check = command['argv'][0] == 'check'
        data_refs = [{'$ref': '#/$defs/' + name} for name in command['output']['data_schemas']]
        schema['oneOf'].append({'properties': {
            'command': {'const': command['name']},
            'assessment': {'$ref': '#/$defs/assessment'} if is_check else {'type': 'null'},
            'data': {'anyOf': [*data_refs, {'type': 'null'}]}}, 'required': ['command']})
    schema['oneOf'].append({'properties': {'command': {'type': 'null'}, 'outcome': {'const': 'error'}, 'data': {'type': 'null'}, 'assessment': {'type': 'null'}}, 'required': ['command']})
    schema['allOf'] = schema['allOf'][:5]
    # Offline descriptions are the only completed envelopes with no capture.
    remote = [path for path in paths if path not in ['describe', 'tui']]
    schema['allOf'].append({'if': {'properties': {'command': {'enum': remote}, 'data': {'type': 'object'}}, 'required': ['command', 'data']},
        'then': {'properties': {'meta': {'properties': {'target': {'$ref': '#/$defs/target'}, 'capture': {'$ref': '#/$defs/capture'}}}}}})
    schema['allOf'].append({'if': {'properties': {'outcome': {'const': 'error'}}},
        'then': {'properties': {'next_actions': {'maxItems': 0}}}})
    # Unreachable v1 command branches must not appear as advertised capabilities.
    for key in ['contextCommand', 'snapshotDiagnosticCommand', 'vmHealthCommand', 'resourceListCommand',
                'resourceDetailCommand', 'traceCommand', 'logsCommand', 'preCommandError', 'describeCommand',
                'contextData', 'disconnectData', 'diagnoseData']:
        d.pop(key, None)
    return schema


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    parser.add_argument('--escript', type=Path, default=Path(os.environ.get('OBSERVER_CLI_BIN', ROOT / '_build/default/bin/observer_cli')))
    args = parser.parse_args()
    proc = subprocess.run([str(args.escript.resolve()), 'describe', '--full', '--json'], capture_output=True, text=True, check=True, timeout=30)
    if proc.stderr:
        raise RuntimeError('offline registry emitted stderr')
    commands = json.loads(proc.stdout)['data']['commands']
    private = enrich_capture(json.loads(PRIVATE.read_text()))
    public = build(private, commands)
    expected = {PRIVATE: json.dumps(private, indent=2) + '\n', PUBLIC: json.dumps(public, indent=2) + '\n'}
    stale = []
    for path, content in expected.items():
        if args.check:
            if not path.exists() or path.read_text() != content:
                stale.append(str(path.relative_to(ROOT)))
        else:
            path.write_text(content)
    if stale:
        print('Schema drift: ' + ', '.join(stale), file=sys.stderr)
        return 1
    print('Schema generation check passed.' if args.check else 'Generated private facts and standalone public v2 schema.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
