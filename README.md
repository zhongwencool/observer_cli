
-----------------
##observer_cli
[![Build Status](https://travis-ci.org/zhongwencool/ovserver_cli.png)](https://travis-ci.org/zhongwencool/observer_cli)

A sharp tool using [recon](https://github.com/ferd/recon) to see erlang node.

##Target
Minimal consumption.

Just look like [observer](http://www.erlang.org/doc/apps/observer/observer_ug.html) in shell, 

but you might think that one day observer_cli more convenient than observer.

------------------
###Try Try Try...

```bash
$ make && make shell   
1> observer_cli:start().

```
![Top](http://7q5a9k.com1.z0.glb.clouddn.com/observer_cli_start.jpg)

----------------
###Command

```erlang
> observer_cli:start().
> observer_cli:start(4000).
> observer_cli:system().
> observer_cli:allocator().
> observer_cli:allocator(4000).
> observer_cli:table().
```

-------------------
###TODO
- [x] observer_cli:start(). Processes Memory, Binary, Total Heap Size, Reductions Top.     
- [ ] observer_cli:system(). include System and Architecture, CPU's and Threads metrics  in observer's system 
- [ ] observer_cli:allocator(). Memory Allocators: std, ll, eheap, ets,fix, binary, driver.
- [ ] observer_cli:table(). include all metrics in observer's Table Viewer.
- [ ] ~~Draw all appication’s relations.~~
- [ ] ~~Trace Overview.~~ You should use recon_trace.

--------------------
###License
See the LICENSE file for license rights and limitations (MIT).
