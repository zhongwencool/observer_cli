# Releasing

1. Update the version in `src/observer_cli.app.src`, `mix.exs`, README and docs,
   then add the release notes to `docs/CHANGELOG.md`.
2. Verify the release:

   ```sh
   rebar3 fmt --check
   rebar3 eunit
   rebar3 compile
   rebar3 as ci compile
   rebar3 docs
   rebar3 hex build
   ```

3. Commit the release, then tag, push, and publish it:

   ```sh
   VERSION=2.0.0
   git tag "v$VERSION"
   git push origin main "v$VERSION"
   rebar3 hex publish
   ```
