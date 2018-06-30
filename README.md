
-----------------
# observer_cli
[![Github Tag](https://img.shields.io/github/tag/zhongwencool/observer_cli.svg)](https://github.com/zhongwencool/observer_cli)
[![Build Status](https://travis-ci.org/zhongwencool/observer_cli.svg?branch=master)](https://travis-ci.org/zhongwencool/observer_cli)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](https://github.com/zhongwencool/observer_cli)
[![Hex.pm](https://img.shields.io/hexpm/v/observer_cli.svg)](http://hex.pm/packages/observer_cli)
[![Hex.pm Downloads](https://img.shields.io/hexpm/dt/observer_cli.svg?style=flat-square)](https://hex.pm/packages/observer_cli)

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
dep_observer_cli = hex 1.3.1
```
**Elixir**
```elixir
# mix.exs                                                                                                   
   def deps do                                                          
     [{:observer_cli, "~> 1.3"}]
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

#### Escriptize
1. cd path/to/observer_cli/
2. `rebar3 escriptize` to generate an escript executable containing the project's and its dependencies' BEAM files.
    Place script(`_build/default/bin/observer_cli`) anywhere in your path and use `observer_cli` command.
3. `observer_cli TARGETNODE [TARGETCOOKIE]` to monitor remote node.

----------------
### GUI
<img src="https://user-images.githubusercontent.com/3116225/39091211-55554414-4622-11e8-8b28-bd3b5c7e17a6.jpg" width="90%" alt="Home"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091212-55870e22-4622-11e8-99e7-8e8c56223765.jpg" width="90%" alt="Network"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091213-55b9aaf8-4622-11e8-91ed-b37c04e20173.jpg" width="90%" alt="System"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091214-55eae91a-4622-11e8-95c2-bc514219b5d9.jpg" width="90%" alt="Ets"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091215-5637b4fc-4622-11e8-9639-99405318fc09.jpg" width="90%" alt="Mnesia"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091216-567ddab8-4622-11e8-8b32-db0f621d6b90.jpg" width="90%" alt="Application"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091217-57258844-4622-11e8-9b21-2a7d661bc623.jpg" width="90%" alt="Document"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091219-66ba0398-4622-11e8-81b1-f489251f111a.jpg" width="90%" alt="Process"></img>
<img src="https://user-images.githubusercontent.com/3116225/39091218-6687caf4-4622-11e8-86c7-190c2106d41e.jpg" width="90%" alt="Port"></img>

-------------------
### TODO
- [x] Processes Memory, Binary, Total Heap Size, Reductions Top.
- [x] include System and Architecture, CPU's and Threads metrics  in observer's system
- [x] Memory Allocators: std, ll, eheap, ets,fix, binary, driver.
- [x] ets include all metrics ets in observer's Table Viewer.
- [x] doc (keep simple)
- [x] remote node support
- [x] mneisa: table info by using mnesia:info, mnesia:system_info/1,
- [ ] ~~Draw all application’s relations.~~
- [ ] ~~Trace Overview.~~ You should use recon_trace.

----------------
### Changelog
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
