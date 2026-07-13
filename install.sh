#!/bin/sh

set -eu

VERSION=2.0.0
RELEASE_URL="https://github.com/zhongwencool/observer_cli/releases/download/v$VERSION"
INSTALL_DIR="$HOME/.local/bin"

for cmd in curl erl escript shasum awk install; do
    command -v "$cmd" >/dev/null 2>&1 || {
        printf 'observer_cli installer: missing command: %s\n' "$cmd" >&2
        exit 1
    }
done

OTP=$(erl -noshell -eval 'io:put_chars(erlang:system_info(otp_release)), halt().')
case "$OTP" in
    26 | 27 | 28 | 29) ;;
    *)
        printf 'observer_cli installer: unsupported controller OTP: %s\n' "$OTP" >&2
        exit 1
        ;;
esac

ASSET="observer_cli-$VERSION-otp$OTP"
TMP=$(mktemp -d "${TMPDIR:-/tmp}/observer-cli-install.XXXXXX")
STAGED=

cleanup() {
    rm -rf "$TMP"
    [ -z "$STAGED" ] || rm -f "$STAGED"
}

trap cleanup 0 HUP INT TERM

curl -fsSL "$RELEASE_URL/$ASSET" -o "$TMP/$ASSET"
curl -fsSL "$RELEASE_URL/SHA256SUMS" -o "$TMP/SHA256SUMS"
awk -v asset="$ASSET" '$2 == asset {print}' "$TMP/SHA256SUMS" > "$TMP/$ASSET.sha256"
test "$(wc -l < "$TMP/$ASSET.sha256")" -eq 1
(cd "$TMP" && shasum -a 256 -c "$ASSET.sha256")

mkdir -p "$INSTALL_DIR"
STAGED=$(mktemp "$INSTALL_DIR/.observer_cli.XXXXXX")
install -m 0755 "$TMP/$ASSET" "$STAGED"
VERSION_OUTPUT=$("$STAGED" --version)
printf '%s\n' "$VERSION_OUTPUT" | grep -Fx "observer_cli $VERSION" >/dev/null
printf '%s\n' "$VERSION_OUTPUT" | grep -Fx "controller OTP $OTP" >/dev/null
mv -f "$STAGED" "$INSTALL_DIR/observer_cli"
STAGED=

printf '%s\nInstalled observer_cli at %s\n' "$VERSION_OUTPUT" "$INSTALL_DIR/observer_cli"
case ":$PATH:" in
    *":$INSTALL_DIR:"*) ;;
    *) printf 'Add %s to PATH.\n' "$INSTALL_DIR" ;;
esac
