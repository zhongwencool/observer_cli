#!/bin/sh

set -eu

ROOT=$(CDPATH= cd "$(dirname "$0")/.." && pwd)
VIEW=${1:-all}
TMP=

usage() {
    cat <<'EOF'
Usage: ./scripts/docs-screenshots.sh [all|tui-home|tui-sockets|tui-process|tui-plugin]

Run from a 180x50 or larger terminal. Wait for the view to settle, take the
screenshot, then press q. The all mode opens every documentation screenshot in
sequence.
EOF
}

case "$VIEW" in
    all | tui-home | tui-sockets | tui-process | tui-plugin) ;;
    -h | --help)
        usage
        exit 0
        ;;
    *)
        usage >&2
        exit 64
        ;;
esac

for command in erl erlc expect rebar3; do
    command -v "$command" >/dev/null 2>&1 || {
        printf 'Missing required command: %s\n' "$command" >&2
        exit 1
    }
done

cleanup() {
    if [ -n "$TMP" ]; then
        rm -rf "$TMP"
    fi
}
trap cleanup 0 HUP INT TERM

cd "$ROOT"
mkdir -p docs/assets
rebar3 compile

TMP=$(mktemp -d "${TMPDIR:-/tmp}/observer-cli-docs.XXXXXX")
cat >"$TMP/observer_cli_docs_fixture.erl" <<'EOF'
-module(observer_cli_docs_fixture).

-export([prepare/0]).
-export([attributes/1, sheet_header/0, sheet_body/1]).

prepare() ->
    application:load(observer_cli),
    start_worker(),
    start_socket_pair(),
    ok.

start_worker() ->
    Pid = spawn(fun() ->
        put(role, documentation_worker),
        self() ! {sample_request, 1},
        self() ! {sample_request, 2},
        worker(<<0:(4 * 1024 * 1024 * 8)>>)
    end),
    true = register(docs_worker, Pid).

worker(State) ->
    receive
        stop -> byte_size(State)
    after 60000 ->
        worker(State)
    end.

start_socket_pair() ->
    {ok, Listen} = socket:open(inet, stream, tcp),
    ok = socket:bind(Listen, #{family => inet, addr => {127, 0, 0, 1}, port => 0}),
    ok = socket:listen(Listen),
    {ok, Address} = socket:sockname(Listen),
    Parent = self(),
    ClientPid = spawn(fun() ->
        {ok, Client} = socket:open(inet, stream, tcp),
        ok = socket:connect(Client, Address, 2000),
        ok = socket:send(Client, <<"observer_cli socket documentation traffic">>),
        Parent ! {socket_client_ready, self()},
        receive
            stop -> socket:close(Client)
        end
    end),
    {ok, Server} = socket:accept(Listen, 2000),
    {ok, _} = socket:recv(Server, 0, 2000),
    receive
        {socket_client_ready, ClientPid} -> ok
    end,
    put(docs_sockets, {Listen, Server, ClientPid}).

attributes(State) ->
    #{
        rows => [[
            #{content => "BEAM memory", width => 16},
            #{content => {byte, erlang:memory(total)}, width => 16}
        ]],
        state => State
    }.

sheet_header() ->
    #{
        columns => [
            #{id => metric, title => "Metric", width => 24},
            #{id => value, title => "Value", width => 16, shortcut => "V"},
            #{id => messages, title => "Messages", width => 12, shortcut => "M"}
        ],
        default_sort => value
    }.

sheet_body(State) ->
    Worker = whereis(docs_worker),
    {memory, Memory} = process_info(Worker, memory),
    {message_queue_len, Messages} = process_info(Worker, message_queue_len),
    #{
        rows => [
            #{
                cells => #{metric => "docs_worker", value => Memory, messages => Messages},
                handle => Worker
            },
            #{
                cells => #{
                    metric => "Processes",
                    value => erlang:system_info(process_count),
                    messages => 0
                }
            },
            #{
                cells => #{
                    metric => "Ports",
                    value => erlang:system_info(port_count),
                    messages => 0
                }
            }
        ],
        state => State
    }.
EOF

erlc -o "$TMP" "$TMP/observer_cli_docs_fixture.erl"

OBSERVER_EBIN="$ROOT/_build/default/lib/observer_cli/ebin"
RECON_EBIN="$ROOT/_build/default/lib/recon/ebin"

show_tui() {
    screen=$1
    runner="$TMP/run-tui.sh"
    cat >"$runner" <<EOF
#!/bin/sh
exec erl -pa "$TMP" "$OBSERVER_EBIN" "$RECON_EBIN" \
    -sname observer_docs -noshell -eval '
observer_cli_docs_fixture:prepare(),
{ok, _} = application:ensure_all_started(observer_cli),
observer_cli:start(),
init:stop().'
EOF
    chmod +x "$runner"
    DOCS_SCREEN="$screen" \
        DOCS_RUNNER="$runner" \
        expect -c '
        set timeout -1
        set screen $env(DOCS_SCREEN)
        spawn -noecho $env(DOCS_RUNNER)
        after 2500
        if {$screen eq "tui-sockets"} {
            send -- "K\r"
            after 2500
        } elseif {$screen eq "tui-process"} {
            send -- "b\r"
            expect -re "docs_worker"
            after 500
            send -- "1\r"
            after 2500
        }
        interact
    '
}

show_plugin() {
    runner="$TMP/run-plugin.sh"
    cat >"$runner" <<EOF
#!/bin/sh
exec erl -pa "$TMP" "$OBSERVER_EBIN" "$RECON_EBIN" -noshell -eval '
observer_cli_docs_fixture:prepare(),
application:set_env(observer_cli, plugins, [#{
    module => observer_cli_docs_fixture,
    title => "Runtime",
    shortcut => "R",
    interval => 1500,
    sort => value
}]),
observer_cli:start_plugin(),
init:stop().'
EOF
    chmod +x "$runner"
    DOCS_PLUGIN_RUNNER="$runner" expect -c '
        set timeout -1
        spawn -noecho $env(DOCS_PLUGIN_RUNNER)
        after 2500
        interact
    '
}

show() {
    screen=$1
    printf '\nOpening %s. Take the screenshot after the redraw settles; press q when done.\n' "$screen"
    case "$screen" in
        tui-plugin) show_plugin ;;
        *) show_tui "$screen" ;;
    esac
}

case "$VIEW" in
    all)
        show tui-home
        show tui-sockets
        show tui-process
        show tui-plugin
        ;;
    *) show "$VIEW" ;;
esac
