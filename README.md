
-----------------
##observer_cli
[![Build Status](https://travis-ci.org/zhongwencool/observer_cli.svg?branch=master)](https://travis-ci.org/zhongwencool/observer_cli)

Visualize Erlang Nodes On The Command Line By Using [recon](https://github.com/ferd/recon).

##Goal
Minimal consumption.

Just look like [observer](http://www.erlang.org/doc/apps/observer/observer_ug.html) in shell, 

but you might think that one day observer_cli more convenient than observer.

------------------
###Try Try Try...

```bash
$ make && make shell   
1> observer_cli:start().
```
### Process And System Information
 
![Top](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_home_11_18.jpg)

![Process](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_process_11_18.jpg)

### ETS And System Information
![System](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_ets_11_18.jpg)

### Allocator Information
![Allocator](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_allocator_11_18.jpg)

### Mnesia Information
![Mnesia](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_mnesia_11_18.jpg)

### Help Information
![Help](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_help_11_18.jpg)


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
