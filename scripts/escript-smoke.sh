#!/bin/sh

set -eu

ROOT=$(CDPATH= cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

# Do not let caller VM flags or target selection escape the owned test harness.
unset ERL_FLAGS ERL_AFLAGS ERL_ZFLAGS ERL_LIBS
unset OBSERVER_CLI_NODE OBSERVER_CLI_COOKIE OBSERVER_CLI_COOKIE_FILE OBSERVER_CLI_NAME_MODE

rebar3 escriptize
BIN=${OBSERVER_CLI_BIN:-"$ROOT/_build/default/bin/observer_cli"}
TMP=$(mktemp -d "${TMPDIR:-/tmp}/observer-cli-smoke.XXXXXX")
TARGET_PID=
TUI_PID=
EPMD_PID=

cleanup() {
    for pid in "$TUI_PID" "$TARGET_PID" "$EPMD_PID"; do
        [ -z "$pid" ] || {
            kill "$pid" 2>/dev/null || true
            wait "$pid" 2>/dev/null || true
        }
    done
    rm -rf "$TMP"
}

trap cleanup 0
trap 'exit 1' HUP INT TERM

mkdir -p "$TMP/home" "$TMP/config"
HOME="$TMP/home"
XDG_CONFIG_HOME="$TMP/config"
ERL_CRASH_DUMP="$TMP/erl_crash.dump"
export HOME XDG_CONFIG_HOME ERL_CRASH_DUMP

ERL_EPMD_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
export ERL_EPMD_PORT
epmd -port "$ERL_EPMD_PORT" -address 127.0.0.1 >"$TMP/epmd.log" 2>&1 &
EPMD_PID=$!

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

check_tui_eof() {
    target="observer_cli_smoke_$$@127.0.0.1"
    cookie="observer_cli_smoke_$$"
    OBSERVER_CLI_SMOKE_COOKIE="$cookie"
    export OBSERVER_CLI_SMOKE_COOKIE
    target_output="$TMP/tui-target"
    erl -pa \
        "$ROOT/_build/default/lib/observer_cli/ebin" \
        "$ROOT/_build/default/lib/recon/ebin" \
        -name "$target" -noshell \
        -eval 'erlang:set_cookie(node(), list_to_atom(os:getenv("OBSERVER_CLI_SMOKE_COOKIE"))), io:put_chars("ready\n"), receive after infinity -> ok end.' \
        >"$target_output" 2>&1 &
    TARGET_PID=$!

    attempts=0
    until grep -F ready "$target_output" >/dev/null; do
        kill -0 "$TARGET_PID" 2>/dev/null || {
            cat "$target_output" >&2
            fail "TUI EOF target failed to start"
        }
        attempts=$((attempts + 1))
        [ "$attempts" -lt 50 ] || fail "TUI EOF target startup timed out"
        sleep 0.1
    done

    stdout="$TMP/tui-eof.stdout"
    stderr="$TMP/tui-eof.stderr"
    TERM=xterm-256color "$BIN" tui --node "$target" --cookie-env OBSERVER_CLI_SMOKE_COOKIE --interval 1000ms \
        </dev/null >"$stdout" 2>"$stderr" &
    TUI_PID=$!

    attempts=0
    while kill -0 "$TUI_PID" 2>/dev/null; do
        attempts=$((attempts + 1))
        [ "$attempts" -lt 50 ] || {
            cat "$stdout" "$stderr" >&2
            fail "TUI did not exit on stdin EOF"
        }
        sleep 0.1
    done
    if wait "$TUI_PID"; then
        status=0
    else
        status=$?
    fi
    TUI_PID=

    [ "$status" -eq 0 ] || {
        cat "$stdout" "$stderr" >&2
        fail "TUI EOF expected exit 0, got $status"
    }
    printf 'ok - TUI exits on stdin EOF\n'
}

check "top-level help" 0 "Usage:" empty --help
check "no-argument help" 0 "15-second overview" empty
check "short help" 0 "Usage:" empty -h
check "state help" 0 "--allow-state-read" empty inspect state --help
check "logs help" 0 "inspect logs" empty inspect logs --help
check "trace call help" 0 "trace call" empty trace call --help
check "version" 0 "observer_cli 3.0.0" empty --version
check "unknown option" 2 empty "Unknown option" --bogus
check "removed session" 2 empty "connect was removed" connect
check "removed resource shorthand" 2 empty "inspect process" processes
check "logs reject redaction" 2 empty "not supported" inspect logs --redact
check "logs invalid tail" 2 empty "Invalid --tail" inspect logs --tail 0
check "rate needs a window" 2 empty "requires --window" inspect process --sort reductions-rate
check "redacted PID is not executable" 2 empty "redacted pid-N" inspect process --pid pid-1
check "state consent" 2 empty "--allow-state-read" inspect state --name init --behavior gen_server
check "trace consent" 2 empty "--replace-existing-trace" trace call erlang:node/0 --pid '<0.1.0>'
check_tui_eof
