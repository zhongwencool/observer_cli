# Releasing

1. Update the version in `src/observer_cli.app.src`,
   `src/observer_cli_snapshot.erl`, `mix.exs`, `install.sh`, README, docs, and
   the matching version assertions, then add the release notes to
   `docs/CHANGELOG.md`.
2. Verify the release:

   ```sh
   rebar3 fmt --check
   rebar3 eunit
   rebar3 compile
   rebar3 as ci compile
   sh -n install.sh
   rebar3 escriptize
   scripts/escript-smoke.sh
   MIX_ENV=prod mix deps.get --check-locked
   MIX_ENV=prod mix escript.build
   OBSERVER_CLI_BIN="$PWD/observer_cli" scripts/escript-smoke.sh
   rebar3 docs
   rebar3 hex build
   ```

3. Commit the release, then create and push its `vX.Y.Z` tag:

   ```sh
   VERSION=2.0.0
   git tag "v$VERSION"
   git push origin main "v$VERSION"
   ```

4. Wait for the OTP 26–29 and Mix jobs to pass. The tag workflow creates a
   GitHub Release containing four escripts and `SHA256SUMS`. Download and verify
   it before publishing Hex:

   ```sh
   set -eu
   VERSION=2.0.0
   TMP=$(mktemp -d)
   gh release download "v$VERSION" --dir "$TMP"
   test "$(find "$TMP" -type f | wc -l)" -eq 5
   test "$(find "$TMP" -type f -name "observer_cli-$VERSION-otp*" | wc -l)" -eq 4
   test "$(wc -l < "$TMP/SHA256SUMS")" -eq 4
   (cd "$TMP" && shasum -a 256 -c SHA256SUMS)
   rm -rf "$TMP"

   rebar3 docs
   test -f doc/llms.txt
   test -f doc/cli.md
   test -f doc/tui.md
   rebar3 hex publish --doc-dir doc
   ```
