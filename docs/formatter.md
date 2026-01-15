# How to use your own formatter?

Observer CLI before rendering does formatting for:

* Process State;
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

Example: build an erlfmt formatter plugin (external)

Goal: keep observer_cli clean, but let users plug in erlfmt.

Step 1: create your own app

Create a new Erlang app, for example `my_formatter_app`.

Step 2: add erlfmt dependency in your app

In your app's `rebar.config`:

```erlang
{deps, [
    observer_cli,
    erlfmt
]}.
```

`observer_cli` is only for the behaviour check at compile time.

Step 3: implement the behaviour

Create a module in your app, for example `my_formatter_erlfmt`:

```erlang
-module(my_formatter_erlfmt).

-behaviour(observer_cli_formatter).

-export([format/2]).

format(Pid, Term) ->
    Header = io_lib:format("Process: ~p~n~n", [Pid]),
    TermString = unicode:characters_to_list(io_lib:format("~p.~n", [Term])),
    Formatted =
        case erlfmt:format_string(TermString, []) of
            {ok, Output, _Warnings} -> Output;
            {skip, Output} -> Output;
            {error, Error} -> erlang:error({erlfmt_format_failed, Error})
        end,
    [_ | _] = unicode:characters_to_list([Header, Formatted]).
```

Step 4: make sure the formatter app is on the target node

The target node must have `my_formatter_app` available in its release,
otherwise the formatter module cannot be loaded remotely.

Step 5: tell observer_cli to use your formatter

In the target node's `sys.config` (or runtime env):

```erlang
{observer_cli, [
    {formatter, #{application => my_formatter_app, mod => my_formatter_erlfmt}}
]}.
```

If your formatter crashes, observer_cli will fall back to the default formatter.

Runtime env example:

```erlang
application:set_env(observer_cli, formatter, #{
    application => my_formatter_app,
    mod => my_formatter_erlfmt
}).
```

**Option reference**

- `application` - Formatter application. Observer CLI loads all its modules to
  remote node.
- `mod` - Implementation of observer_cli_formatter behaviour.
