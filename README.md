

-----------------
# observer_cli
[![Build Status](https://travis-ci.org/zhongwencool/observer_cli.svg?branch=master)](https://travis-ci.org/zhongwencool/observer_cli)
[![GitHub tag](https://img.shields.io/github/tag/zhongwencool/observer_cli.svg)](https://github.com/zhongwencool/observer_cli)
[![MIT License](https://img.shields.io/hexpm/l/observer_cli.svg)](https://hex.pm/packages/observer_cli)
[![Hex.pm Version](https://img.shields.io/hexpm/v/observer_cli.svg)](https://hex.pm/packages/observer_cli)
[![Hex.pm Downloads](https://img.shields.io/hexpm/dt/observer_cli.svg)](https://hex.pm/packages/observer_cli)

Visualize Erlang/Elixir Nodes On The Command Line base on [recon](https://github.com/ferd/recon).
[Document in detail](https://hexdocs.pm/observer_cli/).

## Goal
- Provide a high-performance tool usable both in development and production settings.
- Focus on important and detailed information about real-time running system.
- Keep minimal consumption.

------------------
### Installation

**Erlang**
```erlang
%% rebar.config
{deps, [observer_cli]}
%% erlang.mk
dep_observer_cli = hex 1.7.2
```
**Elixir**
```elixir
# mix.exs
   def deps do
     [{:observer_cli, "~> 1.7"}]
   end
```
------------------
### How-To
#### Try in local shell.

```erlang
%% rebar3 project
rebar3 shell
1> observer_cli:start().
%% mix project
iex -S mix
iex(1)> :observer_cli.start
```
####  Monitor remote node
```erlang
%% rebar3 project
rebar3 shell --name 'observer_cli@127.0.0.1'
1> observer_cli:start('target@host', 'magic_cookie').
%% mix project
iex --name "observer_cli@127.0.0.1" -S mix
iex(1)> :observer_cli.start(:'target@host', :'magic_cookie')
```
:exclamation: **ensure observer_cli application been loaded on target node.**

#### Try in Elixir 1.9.x release
```erlang
%% create elixir release
mix release
%% rpc current node
_build/dev/rel/example/bin/example rpc ":observer_cli.start"
```
:exclamation: **ensure observer_cli application been loaded on current node.**

#### Escriptize
1. cd path/to/observer_cli/
2. `rebar3 escriptize` to generate an escript executable containing the project's and its dependencies' BEAM files.
    Place script(`_build/default/bin/observer_cli`) anywhere in your path and use `observer_cli` command.
3. `observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]` to monitor remote node.

----------------
### DEMO
<img src="https://user-images.githubusercontent.com/3116225/39091211-55554414-4622-11e8-8b28-bd3b5c7e17a6.jpg" width="100%" alt="Home"> </img>

### How to write your own plugin?
If you need to customize some of your internal metrics and integrate it into observer_ci,
you only need to write a `observer_cli_plugin` behaviour in a few simple steps to get a nice presentation.
1. Configure observer_cli，tell observer_cli how to find your plugin.
```erlang
%% module       - Specific module implements plugin behavior. It's mandatory.
%% title        - Menu title. It's mandatory.
%% shortcut     - Switch plugin by shortcut. It's mandatory.
%% interval     - Refresh interval ms. It's optional. default is 1500ms.
%% sort_column  - Sort the sheet by this index. It's optional default is 2.

{plugins,
  [
    #{module => observer_cli_plug_behaviour_x, title => "XPlug",
      interval => 1600, shortcut => "X", sort_column => 3},
    #{module => observer_cli_plug_behaviour_y, title => "YPlug",
      interval =>2000, shortcut => "Y", sort_column => 3}
  ]
}
```
The main view is `HOME` by default(`observer_cli:start()`).
If you want to plugin view as main view, DO:`your_cli:start().`
```erlang
% your_cli.erl
start() -> observer_cli:start_plugin().
```
2. Write observer_cli_plugin behaviour.
observer_cli_plugin has 3 callbacks.

2.1 attributes.
```erlang
-callback attributes(PrevState) -> {[Rows], NewState} when
    Rows :: #{content => string()|integer()|{byte, pos_integer()},
              width => pos_integer(), color => binary()}.
```
for example:
```erlang
attributes(PrevState) ->
  Attrs = [
    [
      #{content => "XXX Ets Size", width => 15},
      #{content => 122, width => 10},
      #{content => "Memory Capcity", width => 15},
      #{content => {percent, 0.12}, width => 16},
      #{content => "XYZ1 Process Mem", width => 19},
      #{content => {byte, 1023 * 1203}, width => 19}
    ],
    [
      #{content => "YYY Ets Size", width => 15},
      #{content => 43, width => 10},
      #{content => "Disk Capcity", width => 15},
      #{content => {percent, 0.23}, width => 16},
      #{content => "XYZ2 Process Mem", width => 19},
      #{content => {byte, 2034 * 220}, width => 19}
    ],
    [
      #{content => "ZZZ Ets Size", width => 15},
      #{content => 108, width => 10},
      #{content => "Volume Capcity", width => 15},
      #{content => {percent, 0.101}, width => 16},
      #{content => "XYZ3 Process Mem", width => 19},
      #{content => {byte, 12823}, width => 19}
    ]
  ],
  NewState = PrevState,
  {Attrs, NewState}.
```

```markdown
|Home(H)|XPlug(X)|YPlug(Y)|                                                                 | 0Days 3:34:50    |
|XXX Ets Size    | 122        | Memory Capcity  | 12.00%           | XYZ1 Process Mem    | 1.1737 MB           |
|YYY Ets Size    | 43         | Disk Capcity    | 23.00%           | XYZ2 Process Mem    | 436.9922 KB         |
|ZZZ Ets Size    | 108        | Volume Capcity  | 10.10%           | XYZ3 Process Mem    | 12.5225 KB          |
```

```erlang
-callback sheet_header() -> [SheetHeader] when
    SheetHeader :: #{title => string(), width => pos_integer(), shortcut => string()}.
```
for example:
```erlang
sheet_header() ->
  [
    #{title => "Pid", width => 15},
    #{title => "Register", width => 20},
    #{title => "Memory", width => 20, shortcut => "S"},
    #{title => "Reductions", width => 23, shortcut => "R"},
    #{title => "Message Queue Len", width => 23, shortcut => "Q"}
  ].
```
```markdown
|No |Pid            |Register            |Memory(S)           |Reductions(R)          |Message Queue Len(Q)    |
```

```erlang
-callback sheet_body(PrevState) -> {[SheetBody], NewState} when
    PrevState :: any(),
    SheetBody :: list(),
    NewState :: any().

```

for example:
```erlang
sheet_body(PrevState) ->
  Body = [
    begin
      Register =
        case erlang:process_info(Pid, registered_name) of
          [] -> [];
          {_, Name} -> Name
        end,
      [
        Pid,
        Register,
        {byte, element(2, erlang:process_info(Pid, memory))},
        element(2, erlang:process_info(Pid, reductions)),
        element(2, erlang:process_info(Pid, message_queue_len))
      ]
    end
    || Pid <- erlang:processes()
  ],
  NewState = PrevState,
  {Body, NewState}.
```
Support `{byte, 1024*10}` to ` 10.0000 KB`; `{percent, 0.12}` to `12.00%`.

```markdown
|No |Pid            |Register            |Memory(S)           |Reductions(R)          |Message Queue Len(Q)    |
|1   |<0.242.0>     |                    | 4.5020 MB          | 26544288              | 0                      |
|2  | <0.206.0>     |                    | 1.2824 MB          | 13357885              | 0                      |
|3  | <0.10.0>      | erl_prim_loader    | 1.0634 MB          | 10046775              | 0                      |
|4  | <0.434.0>     |                    | 419.1719 KB        | 10503690              | 0                      |
|5  | <0.44.0>      | application_contro | 416.6250 KB        | 153598                | 0                      |
|6  | <0.50.0>      | code_server        | 416.4219 KB        | 301045                | 0                      |
|7  | <0.9.0>       | rebar_agent        | 136.7031 KB        | 1337603               | 0                      |
|8  | <0.207.0>     |                    | 99.3125 KB         | 9629                  | 0                      |
|9  | <0.58.0>      | file_server_2      | 41.3359 KB         | 34303                 | 0                      |
|10 | <0.209.0>     |                    | 27.3438 KB         | 31210                 | 0                      |
|11 | <0.0.0>       | init               | 25.8516 KB         | 8485                  | 0                      |
|refresh: 1600ms q(quit) Positive Number(set refresh interval time ms) F/B(forward/back) Current pages is 1    |
```
Support F/B to page up/down.

[A more specific plugin](https://github.com/zhongwencool/os_stats) can collect linux system information such as kernel vsn, loadavg, disk, memory usage, cpu utilization, IO statistics.

----------------
### Changelog
- 1.7.2
  - Fix error when inspecting process that monitors via {RegName, Node}.
- 1.7.1
  - application view show starting/loading/startPfalse/loaded/started application.
  - fixed badarg when staring by rpc and stop by `ctrl+c`.
  - fixed mix.exe version error
- 1.7.0
  - application view support reductions/memory/process_count sort
  - plugin support `{byte, 1024}` to `10.0000 KB`
  - plugin support `{percent, 0.1234` to `12.34%`
  - plugin support dig deep process view.
- 1.6.2
  - fixed crash when ps command not found on windows.
- 1.6.1
  - remove precise opt version
- 1.6.0
   - hidden schedule usage default
   - format by erlformat
   - add `ps -o pcpu,pmem,rss,vsz` information
   - remove recon_alloc:memory/1 from `HOME`(too much cpu usage)
- 1.5.4
  - Bump Recon to 2.5.1 for otp23 alloc compat.
- 1.5.2
  - Use erlang:system_info(otp_release) when can't find `OTP_VERSION` file for the full version.
- 1.5.1
  - Hide mnesia tab when it's not started
  - Show specific erl version such as '22.0.5'
- 1.5.0
  - Bump Recon to 2.5.0
- 1.4.5
  - Include a minimal mix.exs build file
  - Make sure EXIT message has been clear
- 1.4.4
  - Make sure connection errors can be handled
- 1.4.3
  - Bump Recon to 2.4.0
- 1.4.2
  - Hidden schedule process bar when core > 100.
  - Allow to compile escript w/ inet6 based distribution.
  - Rewrite plugin callback, rename kv_label/0 to attributes/1.
- 1.4.1
  - Fixed ets view memory usage wrong.
  - mnesia view memory usage According to bytes.
- 1.4.0
  - Support write your own plugin.
- 1.3.4
  - View(ets mnesia) support page down/up; support sort by memory or size.
  - Fixed pause crash.
  - Make refresh interval configurable.
- 1.3.3
  - fixed io:format(Format,Args) Format not support iolist OTP R21
- 1.3.2
  - Make sure all observer_cli process exit when quit.
  - Upgrade recon to 2.3.6
- 1.3.1
  - Add atom limit/count in home.
  - Escript support short name and long name.
  - Fixed store process not exit.
  - [Upgrade recon to 2.3.5](https://github.com/ferd/recon/commit/e0c3614334589e375f8b1492f404e4b764fe35e7)
- 1.3.0
  - Rewrite Network/Process view.
  - Support PageDown/PageUp for top n list.
  - Escript auto load observer_cli when it's not load on target node.
- 1.2.2
  - fix schedule number >= 32 display wrong.
  - improve memory(byte/kilobyte/megabyte/gigabyte) unit.
- 1.2.1
  - fixed autosize not work.
  - try best to make color adjust all platform.
- 1.2.0
  - add application GUI.
  - Rearrange GUI and optimize render.
  - Always automatically adapt to the window size.

- 1.1.0
  - Support escript, `observer_cli <TARGETNODE> <COOKIE>`

- 1.0.9
  - Upgrade rebar3 to 3.3.3 for publish hex repo.

----------------
### Contributors
| [<img src="https://avatars2.githubusercontent.com/u/3116225?v=4" width="50px;"/><br /><sub>zhongwencool</sub>](https://tried.cc)<br />[💻](https://github.com/zhongwencool/observer_cli/commits?author=zhongwencool) | [<img src="https://avatars2.githubusercontent.com/u/645514?v=4" width="50px;"/><br /><sub>Dimitrios Zorbas</sub>](https://github.com/Zorbash)<br />[💻](https://github.com/zhongwencool/observer_cli/commits?author=Zorbash) | [<img src="https://avatars1.githubusercontent.com/u/3191073?v=4" width="50px;"/><br /><sub>taotao</sub>](https://github.com/redink)<br />[💻](https://github.com/zhongwencool/observer_cli/commits?author=redink) | [<img src="https://avatars1.githubusercontent.com/u/1520926?v=4" width="50px;"/><br /><sub>Trevor Brown</sub>](https://github.com/Stratus3D)<br />[💻](https://github.com/zhongwencool/observer_cli/commits?author=Stratus3D) | [<img src="https://avatars3.githubusercontent.com/u/164324?s=400&v=4" width="50px;"/><br /><sub>Zaiming Shi</sub>](https://github.com/zmstone)<br />[💻](https://github.com/zhongwencool/observer_cli/commits?author=zmstone) |
| :---: | :---: | :---: | :---: | :---: |

--------------------
### License
See the [LICENSE](https://github.com/zhongwencool/observer_cli/blob/master/LICENSE) file for license rights and limitations (MIT).
