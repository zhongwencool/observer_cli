
-----------------
##observer_cli
[![Build Status](https://travis-ci.org/zhongwencool/observer_cli.svg?branch=master)](https://travis-ci.org/zhongwencool/observer_cli)

Visualize Erlang/Elixir Nodes On The Command Line By Using [recon](https://github.com/ferd/recon).

##Goal
Minimal consumption.

Just look like [observer](http://www.erlang.org/doc/apps/observer/observer_ug.html), but in shell, 

you might think observer_cli would be more convenient than observer.

------------------
###Install
Elixir

```ex
# mix.exs
   def deps do
     [{:observer_cli, "~> 1.0.6", manager: :rebar3}]
   end
   def application do
     [applications: [:observer_cli]]
  end
```
Erlang 

```erlang
%% rebar.config
{deps, [{observer_cli, ".*", {git, "git://github.com/zhongwencool/observer_cli.git",{tag,"1.0.5"}}}
```
------------------
###Try ...

```bash
$ rebar3 shell
1> observer_cli:start().
```
### Process And System Information
 
![Top](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_home_2015_12_26.jpg)

![Process](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_process_20151226.jpg)

### ETS And System Information
![System](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_system_20151226.jpg)

### Allocator Information
![Allocator](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_allocate_20151226.jpg)

### Mnesia Information
![Mnesia](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_db_20151226.jpg)

### Help Information
![Help](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_help_20151226.jpg)


----------------
###Command

```erlang
> observer_cli:start().%% default refresh interval is 2000
> observer_cli:start(Node).
> observer_cli:start(Node, Cookie).
```

-------------------
###TODO
- [x] Processes Memory, Binary, Total Heap Size, Reductions Top.     
- [x] include System and Architecture, CPU's and Threads metrics  in observer's system 
- [x] Memory Allocators: std, ll, eheap, ets,fix, binary, driver.
- [x] ets include all metrics ets in observer's Table Viewer.
- [x] doc (keep simple)
- [x] remote node support
- [x] mneisa: table info by using mnesia:info, mnesia:system_info/1, 
- [ ] ~~Draw all application’s relations.~~
- [ ] ~~Trace Overview.~~ You should use recon_trace.

--------------------
###License
See the [LICENSE](https://github.com/zhongwencool/observer_cli/blob/master/LICENSE) file for license rights and limitations (MIT).
