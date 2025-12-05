# How use your own formatter?

Observer CLI before rendering does formatting for:

* Proccess State;
* Process Messages;
* Process Dictionary.

By default it uses `io:format("~p", [])`.

User can change this. Observer CLI exposes behaviour `observer_cli_formatter`
with callback:

```erlang
-callback format(Pid :: pid(), Term :: term()) ->
    string().
```
So, for changing `Term` formatting we need:

* Write our own implementation of `observer_cli_formatter`;
* Add implementation to Observer CLI configuration.

Default formatter config:

```erlang
{formatter, #{application => observer_cli, mod => observer_cli_formatter_default}}
```

**Option reference**

- `application` - Formatter application. Observer CLI loads all it's modules to
  remote node.
- `mod` - Implementation of observer_cli_formatter behaviour.

