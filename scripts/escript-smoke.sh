#!/bin/sh

set -eu

ROOT=$(CDPATH= cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

rebar3 escriptize
BIN=${OBSERVER_CLI_BIN:-"$ROOT/_build/default/bin/observer_cli"}
TMP=$(mktemp -d "${TMPDIR:-/tmp}/observer-cli-smoke.XXXXXX")
trap 'rm -rf "$TMP"' 0 HUP INT TERM

fail() {
    printf 'not ok - %s\n' "$1" >&2
    exit 1
}

check_stream() {
    stream_name=$1
    stream_file=$2
    stream_expected=$3
    case "$stream_expected" in
        empty)
            [ ! -s "$stream_file" ] || fail "$stream_name: expected an empty stream"
            ;;
        nonempty)
            [ -s "$stream_file" ] || fail "$stream_name: expected output"
            ;;
        *)
            grep -F -e "$stream_expected" "$stream_file" >/dev/null ||
                fail "$stream_name: missing '$stream_expected'"
            ;;
    esac
}

check() {
    case_name=$1
    expected_status=$2
    expected_stdout=$3
    expected_stderr=$4
    shift 4

    stdout="$TMP/stdout"
    stderr="$TMP/stderr"
    if "$BIN" "$@" >"$stdout" 2>"$stderr"; then
        status=0
    else
        status=$?
    fi

    [ "$status" -eq "$expected_status" ] || {
        cat "$stdout" "$stderr" >&2
        fail "$case_name: expected exit $expected_status, got $status"
    }
    check_stream "$case_name stdout" "$stdout" "$expected_stdout"
    check_stream "$case_name stderr" "$stderr" "$expected_stderr"
    printf 'ok - %s\n' "$case_name"
}

check "top-level help" 0 "Usage:" empty --help
check "no-argument help" 0 "Usage:" empty
check "short help" 0 "Usage:" empty -h
check "otp-state help" 0 "observer_cli otp-state" empty otp-state --help
check "logs help" 0 "observer_cli logs" empty logs --help
check "trace call help" 0 "observer_cli trace call" empty trace call --help
check "version" 0 "observer_cli 2.0.0" empty --version
check "unknown option" 2 empty "observer_cli --help" --bogus
check "removed positional TUI shorthand" 2 empty "unknown command: target@host" target@host
check "removed gen-server-state command" 2 empty "unknown command: gen-server-state" gen-server-state init
check "malformed process command" 2 empty "observer_cli process:" process --bogus
check "logs reject redaction" 2 empty "observer_cli logs:" logs --redact
check "logs reject invalid tail" 2 empty "observer_cli logs:" logs --tail 0
check "logs reject arbitrary path" 2 empty "unknown option: --file" logs --file /tmp/app.log
check \
    "malformed process term" \
    2 \
    '<<"command">> => <<"process">>' \
    empty \
    process --bogus --format term
check \
    "unsafe trace command" \
    2 \
    empty \
    --replace-existing-trace \
    trace call erlang:node/0 --pid '<0.1.0>'
