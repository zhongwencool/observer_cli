
-----------------
# observer_cli
[![Github Tag](https://img.shields.io/github/tag/zhongwencool/observer_cli.svg)](https://github.com/zhongwencool/observer_cli)
[![Build Status](https://travis-ci.org/zhongwencool/observer_cli.svg?branch=master)](https://travis-ci.org/zhongwencool/observer_cli)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg?style=flat-square)](https://github.com/zhongwencool/observer_cli)
[![Hex.pm](https://img.shields.io/hexpm/v/observer_cli.svg)](http://hex.pm/packages/observer_cli)
[![Hex.pm Downloads](https://img.shields.io/hexpm/dt/observer_cli.svg?style=flat-square)](https://hex.pm/packages/observer_cli)

Visualize Erlang/Elixir Nodes On The Command Line base on [recon](https://github.com/ferd/recon).

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
dep_observer_cli = hex 1.2.0
```
**Elixir**
```elixir
# mix.exs                                                                                                   
   def deps do                                                          
     [{:observer_cli, "~> 1.2"}]                                    
   end
   def application do
     [applications: [:observer_cli]]
  end
```  
------------------
### How-To
#### Try in local shell.

```erlang
$ rebar3 shell
1> observer_cli:start().
```
####  Monitor remote node
```erlang
$ rebar3 shell --name 'observer_cli@127.0.0.1'
1> observer_cli:start('target@host', 'magic_cookie').
```
#### Escriptize
1. `rebar3 escriptize` to generate an escript executable containing the project's and its dependencies' BEAM files.

    Place script(`_build/default/bin/observer_cli`) anywhere in your path and use `observer_cli` command.

2. `observer_cli <TARGETNODE> <TARGETCOOKIE>` to monitor remote node.

   :exclamation: **ensure observer_cli application start on target node.**

----------------
### GUI
<img src="https://user-images.githubusercontent.com/3116225/33328258-78ec3fa4-d494-11e7-9309-40aa8f252463.jpg" width="90%"></img>
<img src="https://user-images.githubusercontent.com/3116225/33328260-792c1b38-d494-11e7-8b2f-02428ac97820.jpg" width="90%"></img>
<img src="https://user-images.githubusercontent.com/3116225/33328263-799b1b14-d494-11e7-85de-1919bfd3ea3d.jpg" width="90%"></img>
<img src="https://user-images.githubusercontent.com/3116225/33328264-79e4f374-d494-11e7-9f18-5d83f7171ff2.jpg" width="90%"></img>
<img src="https://user-images.githubusercontent.com/3116225/33328265-7a37144c-d494-11e7-8f25-d0b85f843047.jpg" width="90%"></img> 
<img src="https://user-images.githubusercontent.com/3116225/33328266-7a6dc064-d494-11e7-962d-add9359cc4e9.jpg" width="90%"></img>
<img src="https://user-images.githubusercontent.com/3116225/33328267-7aa6a398-d494-11e7-8057-d307b1afcdce.jpg" width="90%"></img> 

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

--------------------
### License
See the [LICENSE](https://github.com/zhongwencool/observer_cli/blob/master/LICENSE) file for license rights and limitations (MIT).
