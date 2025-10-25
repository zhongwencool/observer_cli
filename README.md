---

# observer_cli

[![Build Status](https://github.com/zhongwencool/observer_cli/workflows/ci/badge.svg)](https://github.com/zhongwencool/observer_cli/actions)
[![GitHub tag](https://img.shields.io/github/tag/zhongwencool/observer_cli.svg)](https://github.com/zhongwencool/observer_cli)
[![MIT License](https://img.shields.io/hexpm/l/observer_cli.svg)](https://hex.pm/packages/observer_cli)
[![Hex.pm Version](https://img.shields.io/hexpm/v/observer_cli.svg)](https://hex.pm/packages/observer_cli)
[![Hex.pm Downloads](https://img.shields.io/hexpm/dt/observer_cli.svg)](https://hex.pm/packages/observer_cli)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/observer_cli/)

Observer CLI is a library to be dropped into any beam nodes, to be used to assist DevOps  people diagnose problems in production nodes. Based on [recon](https://github.com/ferd/recon).

<img src="https://github.com/user-attachments/assets/ce797033-732a-4178-a9c5-df4de559ed0c" width="100%" alt="Home"> </img>

- Provide a high-performance tool usable both in development and production settings.
- Focus on important and detailed information about real-time running system.
- Keep minimal consumption.

---

## Installation

<!-- tabs-open -->
### Erlang

```erlang
%% rebar.config
{deps, [observer_cli]}
%% erlang.mk
dep_observer_cli = hex 1.8.5
```

### Elixir

```elixir
# mix.exs
   def deps do
     [{:observer_cli, "~> 1.8"}]
   end
```
<!-- tabs-close -->

## How-To

### Try in local shell.

<!-- tabs-open -->
### Erlang

```erlang
%% rebar3 project
rebar3 shell
1> observer_cli:start().
```
### Elixir
```elixir
%% mix project
iex -S mix
iex(1)> :observer_cli.start
```
<!-- tabs-close -->

### Monitor remote node

<!-- tabs-open -->
### Erlang
```erlang
%% rebar3 project
rebar3 shell --name 'observer_cli@127.0.0.1'
1> observer_cli:start('target@host', 'magic_cookie').
```
### Elixir
```elixir
%% mix project
iex --name "observer_cli@127.0.0.1" -S mix
iex(1)> :observer_cli.start(:'target@host', :'magic_cookie')
```
<!-- tabs-close -->

> #### exclamation {: .info}
> **ensure observer_cli application been loaded on target node.**

> #### tip {: .tip}
> Pass `{interval, 3000}` (Erlang) or `interval: 3000` (Elixir) to sample every 3 seconds. The minimum refresh interval is 1000 ms.

### Escriptize

1. cd path/to/observer_cli/
2. `rebar3 escriptize` to generate an escript executable containing the project's and its dependencies' BEAM files.
   Place script(`_build/default/bin/observer_cli`) anywhere in your path and use `observer_cli` command.
3. `observer_cli TARGETNODE [TARGETCOOKIE REFRESHMS]` to monitor remote node.


## Features

### Home Panel

![Home](https://user-images.githubusercontent.com/3116225/96714573-ede2f280-13d4-11eb-999c-f2aedb0d6ab1.jpg)

The Home panel provides a comprehensive overview of your Erlang node:

[`erlang:system_info/1`](http://erlang.org/doc/man/erlang.html#system_info-1) returns specified information about the current system by below item. When the ratio is greater than 85%, it becomes red.

| Metric             | Source/Limit              |
|--------------------|---------------------------|
| Proc Count         | process_count/process_limit |
| Port Count         | port_count/port_limit   |
| Atom Count         | atom_count/atom_limit     |

* **process_limit**: `erl +P Number` sets the maximum number of simultaneously existing processes for this system if a Number is passed as value. Valid range for Number is [1024-134217727]. The default value is 262144.
* **port_limit**: `erl +Q Number` sets the maximum number of simultaneously existing ports for this system if a Number is passed as value. Valid range for Number is [1024-134217727]. The default value used is normally 65536. However, if the runtime system is able to determine maximum amount of file descriptors that it is allowed to open and this value is larger than 65536, the chosen value will increased to a value larger or equal to the maximum amount of file descriptors that can be opened.
* **atom_limit**: `erl +t size` sets the maximum number of atoms the virtual machine can handle. Defaults to 1,048,576.

[`ps`](https://man7.org/linux/man-pages/man1/ps.1.html) reports a snapshot of the BEAM OS process and feeds the system panel. Observer CLI samples four columns:

| Command/Flag | Description |
|--------------|-------------|
| `ps -o pcpu` | CPU utilization of the BEAM OS process expressed as percentage of a single core. Calculated from cumulative scheduler time / wall clock time, so it may exceed what top reports on multi-core systems. |
| `ps -o pmem` | Percentage of physical memory used by the BEAM OS process (resident set size / total RAM). |
| `ps -o rss`  | Resident set size in kilobytes, useful for spotting long-lived memory growth. |
| `ps -o vsz`  | Virtual memory size in kilobytes, highlighting total address space reservations (code, heap, and mapped binaries). |

[`erlang:memory/0`](http://erlang.org/doc/man/erlang.html#memory-0) Returns a list with information about memory dynamically allocated by the Erlang emulator.

[`erlang:statistics/1`](http://erlang.org/doc/man/erlang.html#statistics-1)

| Statistic                     | Description                                                                                                                                                                                |
|-------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| active task                   | returns the same as `statistics(active_tasks_all)` with the exception that no information about the dirty IO run queue and its associated schedulers is part of the result. That is, only tasks that are expected to be CPU bound are part of the result. |
| context switches              | returns the total number of context switches since the system started.                                                                                                                      |
| reductions(total/sinceLastCall) | total reductions/reductions since last call.                                                                                                                                               |
| io                            | The total number of bytes received/send through ports and the receive/send bytes through ports of growth during the refresh interval.                                                       |
| garbage_collection            | `erlang:statistics(garbage_collection)` which returns the total value and the `{Number_of_GCs, Words_Reclaimed}` of growth during the refresh interval.                                       |
| run_queue                     | The total length of all normal run-queues. That is, the number of processes and ports that are ready to run on all available normal run-queues. Dirty run queues are not part of the result. |

Increments are values that are mostly useful when compared to a previous
one to have an idea what they're doing, because otherwise they'd never
stop increasing: bytes in and out of the node, number of garbage collector
runs, words of memory that were garbage collected, and the global reductions
count for the node.

Scheduler utilization by [`erlang:statistics(scheduler_wall_time)`](http://erlang.org/doc/man/erlang.html#statistics_scheduler_wall_time):

* Total scheduler utilization will equal 1.0 when all schedulers have been active all the time between the two refresh intervals.
* The result being that there is a decent chunk of CPU usage that would be mostly free for scheduling actual Erlang work (assuming the schedulers are busy waiting more than trying to select tasks to run), but is being reported as busy by the OS.
* The scheduler usage may show a higher rate (1.0) than what the OS will report. Schedulers waiting for OS resources are considered utilized as they cannot handle more work. If the OS itself is holding up on non-CPU tasks it is still possible for Erlang’s schedulers not to be able to do more work and report a full ratio.


### Process

When looking for high memory usage, for example it's interesting to be able to list all of a node's processes and find the top N consumers. Enter `m` then press `Enter` will use the `recon:proc_count(memory, N)` function, and you will get output like the following. On OTP 27+ nodes, process rows also display any label set through [`proc_lib:set_label/1`](https://www.erlang.org/doc/apps/stdlib/proc_lib.html#set_label/1), which helps correlate supervised jobs with their metrics.

![Top](https://user-images.githubusercontent.com/3116225/96717499-25539e00-13d9-11eb-85af-fdde633da098.jpg)

[`recon:proc_count/2`](http://ferd.github.io/recon/recon.html#proc_count-2) and [`recon:proc_window/3`](http://ferd.github.io/recon/recon.html#proc_window-3) are to be used when you require information about processes in a larger sense: biggest consumers of given process `memory`, `reductions`, `binary`, `total_heap_size`, `message_queue_len`, either absolutely or over a sliding time window, respectively.

More detail about sliding time windows see [`recon:proc_window/3`](http://ferd.github.io/recon/recon.html#proc_window-3)

When an abnormal process is found, enter the suspected process sequence(Integer) then press `Enter` will use [`erlang:process_info/2`](http://erlang.org/doc/man/erlang.html#process_info-2) to show a lot of information available (which is safe to use in production) about processes.

![Process](https://user-images.githubusercontent.com/3116225/39091219-66ba0398-4622-11e8-81b1-f489251f111a.jpg)

* **registered_name**: if the process has a name (as registered with `erlang:register/2`), it is given here.
* **trap_exit**: set `trap_exit` to true, exit signals arriving to a process are converted to `{EXIT,From,Reason}` messages, which can be received as ordinary messages. If `trap_exit` is set to false, the process exits if it receives an exit signal other than normal and the exit signal is propagated to its linked processes. Application processes are normally not to trap exits.
* **group_leader**: the group leader of a process defines where IO (files, output of `io:format/1-3`) goes.
* **initial_call**: is the initial function call with which the process was spawned.
* **links**: is a list of process identifiers and port identifiers, with processes or ports to which the process has a link.
* **monitored_by**: A list of process identifiers monitoring the process (with `monitor/2`).
* **monitors**: A list of monitors (started by `monitor/2`) that are active for the process. For a local process monitor or a remote process monitor by a process identifier.
* **status**: the nature of the process as seen by the scheduler. The possible values are:
    * `exiting` the process is done, but not fully cleared yet;
    * `waiting` the process is waiting in a `receive ... end`;
    * `running` self-descriptive;
    * `runnable` ready to run, but not scheduled yet because another process is running;
    * `garbage_collecting` self-descriptive;
    * `suspended` whenever it is suspended by a BIF, or as a back-pressure mechanism because a socket or port buffer is full. The process only becomes runnable again once the port is no longer busy
* **reductions**: The VM does scheduling based on reductions, an arbitrary unit of work that allows rather portable implementations of scheduling (time-based scheduling is usually hard to make work efficiently on as many OSes as Erlang runs on). The higher the reductions, the more work, in terms of CPU and function calls, a process is doing.
* **memory**: Includes call stack, heap, and internal structures. `total_heap_size`, `min_bin_vheap_size`, `min_heap_size`, `fullsweep_after`, `heap_size`.
* **messages**: A list of the messages to the process, which have not yet been processed, it is truncated when the term is too big.
* **dictionary**: Dictionary is the process dictionary, it is truncated when the term is too big.
* **current stack**: The current call stack back-trace (**stacktrace**) of the process. The stack has the same format as returned by `erlang:get_stacktrace/0`. The depth of the stacktrace is truncated according to `backtrace_depth` system flag setting.
* **state**: Using [`sys:get_state(Pid, 2500)`](http://erlang.org/doc/man/sys.html#get_state-2) Gets the state of the process.

### Network

![Network](https://user-images.githubusercontent.com/3116225/96717492-2389da80-13d9-11eb-9795-0f77ee441329.jpg)

* **Byte input/output**: The byte of growth input/output during the refresh interval.
* **Total input/output**: [`erlang:statistics(io)`](http://erlang.org/doc/man/erlang.html#statistics-1) returns `Input`, which is the total number of bytes received through ports, and `Output`, which is the total number of bytes output to ports.

Fetches a given attribute from all inet ports (`TCP, UDP, SCTP`) and returns the biggest Num consumers by [`recon:inet_count/2`](http://ferd.github.io/recon/recon.html#inet_count-2) and [`recon:inet_windows/3`](http://ferd.github.io/recon/recon.html#inet_window-3). Attribute name refer to [`inet:getstat/1`](http://erlang.org/doc/man/inet.html#getstat-1).

* `recv_oct`: Number of bytes received by the socket.
* `recv_cnt`: Number of packets received by the socket.
* `send_cnt`: Number of packets sent from the socket.
* `send_oct`: Number of bytes sent from the socket.
* `cnt`: `recv_cnt` + `send_cnt`.
* `oct`: `recv_oct` + `send_oct`.

When find out who is slowly but surely eating up all your bandwidth, enter the suspected port sequence(Integer) then press `Enter` will use [`recon:port_info/2`](http://ferd.github.io/recon/recon.html#port_info-2) to show a lot of information available about port.

![Port](https://user-images.githubusercontent.com/3116225/39091218-6687caf4-4622-11e8-86c7-190c2106d41e.jpg)

* **id**: internal index of a port. Of no particular use except to differentiate ports.
* **name**: type of the port — with names such as `"tcp_inet"`, `"udp_inet"`, or `"efile"`.
* **os_pid**: If the port is not an inet socket, but rather represents an external process or program, this value contains the os pid related to the said external program.
* **connected**: Each port has a controlling process in charge of it, and this process’ pid is the connected one.
* **links**: Ports can be linked with processes, much like other processes can be. The list of linked processes is contained here. Unless the process has been owned by or manually linked to a lot of processes, this should be safe to use.
* **monitors**: Ports that represent external programs can have these programs end up monitoring Erlang processes. These processes are listed here.
* **IO**: `input` the number of bytes read from the port. `output` the number of bytes written to the port.
* **queue_size**: Port programs have a specific queue, called the driver queue. This returns the size of this queue.
* **memory**: this is the memory allocated by the runtime system for the port. This number tends to be small-ish and excludes space allocated by the port itself.
* **sockname/peername**: [`inet:sockname/1`](http://erlang.org/doc/man/inet.html#sockname-1) a list of all local address/port number pairs for a socket.
* **statistics**: show port statistics by [`inet:getstat/2`](http://erlang.org/doc/man/inet.html#getstat-2).
* **options**: show port options by [`inet:getopts/2`](http://erlang.org/doc/man/inet.html#getopts-2).

### System

![System](https://user-images.githubusercontent.com/3116225/39091213-55b9aaf8-4622-11e8-91ed-b37c04e20173.jpg)

* **System Info**: [`erlang:system_info/1`](http://erlang.org/doc/man/erlang.html#system_info-1) returns various information about the allocators of the current system (emulator).
* **Allocator Info**: [`recon_alloc:average_block_sizes(current|max)`](https://ferd.github.io/recon/recon_alloc.html#average_block_sizes-1) check all allocators in `allocator` and returns the average block sizes being used for mbcs and sbcs. This value is interesting to use because it will tell us how large most blocks are. This can be related to the VM's largest multiblock carrier size (lmbcs) and smallest multiblock carrier size (smbcs) to specify allocation strategies regarding the carrier sizes to be used.
* **Cache Hit Rate**: [`recon_alloc:cache_hit_rates()`](https://ferd.github.io/recon/recon_alloc.html#cache_hit_rates-0) Cache can be tweaked using three VM flags: `+MMmcs`, `+MMrmcbf`, and `+MMamcbf`.
* **Distribution buffers**: Uses [`erlang:dist_get_stat/1`](https://www.erlang.org/doc/man/erlang.html#dist_get_stat-1) on OTP 24+ to track per-node distribution queue sizes and the configured `dist_buf_busy_limit`.

### ETS

![Ets](https://user-images.githubusercontent.com/3116225/39091214-55eae91a-4622-11e8-95c2-bc514219b5d9.jpg)

ETS tables are never garbage collected, and will maintain their memory usage as long as records will be left undeleted in a table. Only removing records manually (or deleting the table) will reclaim memory.

Top N list sort by memory size, all items defined in [`ets:info/2`](http://erlang.org/doc/man/ets.html#info-2)

### Mnesia

![Mnesia](https://user-images.githubusercontent.com/3116225/39091215-5637b4fc-4622-11e8-9639-99405318fc09.jpg)

Top N list sort by memory size, all items defined in [`mnesia:table_info/2`](http://erlang.org/doc/man/mnesia.html#table_info-2)

### Application

![Application](https://user-images.githubusercontent.com/3116225/39091216-567ddab8-4622-11e8-8b32-db0f621d6b90.jpg)

Find application debug information by [`application_controller:info()`](https://github.com/erlang/otp/blob/master/lib/kernel/src/application_controller.erl#L280).
