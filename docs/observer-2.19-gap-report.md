# observer 2.19 与 observer_cli 指标差异报告

日期：2026-07-06
对比对象：

- `observer_cli` 当前源码：`/Users/zw/github/observer_cli/src`
- OTP observer 源码：`/Users/zw/.local/share/mise/installs/erlang/29.0/lib/observer-2.19/src`
- observer 实际数据后端：`/Users/zw/.local/share/mise/installs/erlang/29.0/lib/runtime_tools-2.4/src/observer_backend.erl`

本报告只统计 observer/observer_backend 源码中能直接看到的运行时指标、详情字段和页面能力。`Trace`、`Kill`、`GC`、`Send message`、表内容浏览等不是“指标参数”，单独放在“不建议并入核心”的部分。

## 结论先行

`observer_cli` 已经覆盖了 live 生产诊断最常用的一组低成本指标：系统概要、内存、IO/GC 增量、进程 Top N、进程详情、inet socket 流量、port 详情、allocator 摘要、分布式队列、ETS/Mnesia 表概要、应用聚合、插件扩展。

值得补进当前 `observer_cli` 的缺口主要是这些小字段：

1. **Process detail**：`current_function`、`priority`、`stack_size`、`binary` refs 摘要、`last_calls`、`catchlevel`、`trace`、`suspending`、`sequential_trace_token`、`error_handler`。
2. **Ports**：增加“所有 ports”列表；port 详情补 `slot`、`controls`、`parallelism`、`locking`、`monitored_by` 和 observer_backend 的完整 inet options。
3. **ETS/Mnesia metadata**：ETS 补 `id`、`reg_name`、`heir`、`compressed`、`fixed`；Mnesia 补 `keypos`、`fixed`、`compressed`。
4. **System/Allocator**：System 页补 `ETS count / ets_limit` 和全局 `dist_buf_busy_limit`；Allocator 页可补 observer 的 `block size / carrier size / max carrier size`。
5. **Sockets**：OTP `socket` API 有独立 Sockets 页；如果目标系统使用 `socket` 而不是 `gen_tcp`/`inet` ports，应做一个单独页面，否则先不加。

不建议直接加入当前核心：Trace UI、Crashdump Viewer、表内容浏览、进程/port 破坏性操作、完整 wx 风格历史图表。这些要么侵入性高，要么会拖慢 live 节点，要么超出 `observer_cli` 的终端诊断定位。

## observer_cli 当前功能与指标清单

| observer_cli 功能 | 当前覆盖指标/参数 | 源码位置 |
|---|---|---|
| 全局菜单/启动 | Home、Network、System、ETS、Mnesia、App、Doc、Plugin；本地/远程启动；刷新间隔；escript | `src/observer_cli_lib.erl:82`, `src/observer_cli_lib.erl:159`, `src/observer_cli.erl:41` |
| Home/System summary | `process_count/process_limit`、`port_count/port_limit`、`atom_count/atom_limit`、OTP/ERTS version、`ps -o pcpu`、`ps -o pmem`、`total_active_tasks`、`context_switches`、`reductions` | `src/observer_cli.erl:381` |
| Home/Memory + IO/GC | `erlang:memory()` 的 `total`、`processes_used`、`code`、`atom_used`、`binary`、`ets`；ETS 表数量；`run_queue`/`error_logger` queue；`statistics(io)` 增量；`statistics(garbage_collection)` 增量；`port_parallelism` | `src/observer_cli.erl:494` |
| Home/Scheduler usage | `statistics(scheduler_wall_time)` diff 后的每 scheduler 利用率，按终端宽度渲染 | `src/observer_cli.erl:559` |
| Home/Process Top N | `recon:proc_count/2` 和 `recon:proc_window/3`；排序维度：`memory`、`binary_memory`、`reductions`、`total_heap_size`、`message_queue_len`；行内展示 pid、name/label/initial call、current function、memory/reductions/msgq | `src/observer_cli.erl:827` |
| Process detail | registered name、initial call、group leader、status、message queue len、heap size、total heap size、trap exit、GC min bin vheap/min heap/fullsweep/minor_gcs、links、monitors、monitored_by、reductions/memory 趋势、messages、dictionary、current stack、state | `src/observer_cli_process.erl:400`, `src/observer_cli_process.erl:461` |
| Network | `statistics(io)` total/delta；`recon:inet_count/2` / `recon:inet_window/3`；`recv_cnt`、`recv_oct`、`send_cnt`、`send_oct`、`cnt`、`oct`；port input/output、queue_size、memory、peername | `src/observer_cli_inet.erl:108`, `src/observer_cli_inet.erl:410` |
| Port detail | id、name、os_pid、connected、input/output、memory、queue_size、links、monitors、sockname/peername、inet stats、inet options 子集 | `src/observer_cli_port.erl:146`, `src/observer_cli_port.erl:297`, `src/observer_cli_port.erl:345` |
| System | System/architecture、CPU/scheduler/thread、memory、OS `ps -o pcpu/pmem/rss/vsz`、IO total、allocator average block sizes、SBCS/MBCS、cache hit rates、distribution node queue size/limit/address/in/out/type/state | `src/observer_cli_system.erl:75`, `src/observer_cli_system.erl:143`, `src/observer_cli_system.erl:599` |
| ETS | table name、size、memory、type、protection、keypos、write/read concurrency、owner；按 size/memory 排序 | `src/observer_cli_ets.erl:89` |
| Mnesia | name、memory、size、type、storage、owner、index、registered name；隐藏/显示系统表；按 size/memory 排序 | `src/observer_cli_mnesia.erl:113` |
| App | app/no_group、process count、memory、reductions、msgq、status、version；按 process/memory/reductions/msgq 排序 | `src/observer_cli_application.erl:106` |
| Plugin | 插件 attributes、可排序 sheet columns、row handle drill-down | `src/observer_cli_plugin.erl:411` |

## observer 2.19 页面与 observer_cli 对应关系

observer wx notebook 里有 `System`、`Load Charts`、`Memory Allocators`、`Applications`、`Processes`、`Ports`、`Sockets`、`Table Viewer`、`Trace Overview`。来源：`observer_wx.erl:156-214`。

| observer 页面 | observer_cli 对应 | 结论 |
|---|---|---|
| System | Home + System | 大部分已覆盖；缺 `ETS count/limit` 独立行和全局 `dist_buf_busy_limit`。 |
| Load Charts | Home scheduler usage + System | 只有当前值，没有历史曲线；不建议照搬 wx 图表。 |
| Memory Allocators | System | `observer_cli` 有 recon_alloc 视角；缺 observer 的 block/carrier/max carrier 汇总。 |
| Applications | App | `observer_cli` 反而有聚合指标；缺 supervision tree 和交互操作，不是指标缺口。 |
| Processes | Home Top N + Process detail | Top N 更适合终端；detail 少一些 `process_info/2` 字段。 |
| Ports | Network + Port detail | 缺“所有 ports”列表；detail 少一些 port_info 字段。 |
| Sockets | 无 | 这是最大独立缺口；只在使用 OTP `socket` API 时值得加。 |
| Table Viewer | ETS + Mnesia | 表概要有；缺 metadata detail 和表内容浏览。 |
| Trace Overview | 无 | 不是指标，侵入性强，不建议进核心。 |
| Crashdump Viewer | 无 | 离线 dump 工具，不适合塞进 live `observer_cli` 核心。 |

## 缺失指标参数与适配建议

### 1. System / Load / Allocator

observer System 显示字段来自 `observer_sys_wx:info_fields/0` 和 `observer_backend:sys_info/0`：System/ERTS/architecture、CPU/scheduler/thread、memory、uptime、run queue、IO、atoms/processes/ports/ETS count-limit、distribution buffer busy limit。来源：`observer_sys_wx.erl:132-173`、`observer_backend.erl:75-119`。

| 缺失或部分缺失参数 | observer_cli 现状 | 是否适合加入 | 建议 |
|---|---|---|---|
| `ets_count / ets_limit / % used` | Home 显示 ETS 表数量和 ETS 内存；`ets_limit` 只在老 OTP atom_limit 不支持时兜底显示 | 适合 | 加到 System 页或 Home limit 行；成本低。 |
| `dist_buf_busy_limit` 全局值 | System 的分布式节点表会把它作为每节点 queue limit；没有单独全局行，且无 dist 节点时不可见 | 适合 | System 页补一行即可。 |
| Dirty CPU scheduler 标识 | Home 可显示 scheduler wall time，但不标普通/dirty CPU；observer Load Charts 标出 dirty cpu | 可选 | 若用户常看 dirty scheduler，可在 scheduler 行增加 `dirty` 标记；否则先不加。 |

### 2. Processes

observer Processes 列表字段：`Pid`、`Description`、`Reds`、`Memory`、`MsgQ`、`Current Function`。来源：`observer_pro_wx.erl:170-176`。这些核心列 `observer_cli` 已覆盖，并额外支持 binary memory、total heap size、proc_window。

observer Process Information 详情字段来自 `observer_procinfo:process_info_fields/2` 和 `item_list/0`。来源：`observer_procinfo.erl:373-424`。

| 缺失参数 | observer_cli 现状 | 是否适合加入 | 建议 |
|---|---|---|---|
| `current_function` | Top N 行显示；Process detail 不显示 | 适合 | detail meta 增一格。 |
| `priority` | 未显示 | 适合 | 低成本，直接 `process_info/2`。 |
| `binary` refs | Home 可按 binary memory 排序；detail 不显示 binary refs 列表/摘要 | 可选 | 默认显示 `count/bytes` 摘要；完整 refs 用独立子页，避免刷屏。 |
| `last_calls` | 未显示 | 可选 | 只有开启 last calls 才有用；放 detail 子页。 |
| `catchlevel` | 未显示 | 可选 | 低成本但少用；放低优先级。 |
| `trace` | 未显示 | 不建议默认强调 | 只读显示可以加；trace 控制不加。 |
| `suspending` | 未显示 | 适合 | 对 backpressure/port busy 有诊断价值。 |
| `sequential_trace_token` | 未显示 | 不建议默认 | 太专门；需要 trace 相关工作时再加。 |
| `error_handler` | 未显示 | 可选 | 低成本但少用。 |
| `stack_size` | 只显示 heap/total heap | 适合 | 与 observer 对齐，直接补。 |
| `GC FullSweep After` | 已显示 `fullsweep_after` | 已覆盖 | 不需要。 |
| `GC Min Heap Size` | 已显示 `min_heap_size` | 已覆盖 | 不需要。 |
| `Memory` | detail 图表和 Top N 有，但 detail 表内没有当前 memory 数字 | 适合 | 在 detail 表里补当前 memory，避免只看趋势。 |

Crashdump Viewer 里的 process-only 字段不适合直接作为 live 指标：`internal state`、`started`、`parent`、`run queue`、`program counter`、`continuation pointer`、`arity`、old heap/bin vheap/heap fragment/address 类字段。来源：`cdv_proc_cb.erl:127-166`。这些来自 dump 内部结构，live 节点上不是稳定、低成本的日常接口。

### 3. Ports / Network

observer Ports 页面列所有 ports：`Id`、`Connected`、`Name`、`Controls`、`Slot`。来源：`observer_port_wx.erl:104-108`。observer_backend 对每个 port 额外取 `monitors`、`monitored_by`、`parallelism`、`locking`、`queue_size`、`memory`。来源：`observer_backend.erl:173-188`。

| 缺失参数/能力 | observer_cli 现状 | 是否适合加入 | 建议 |
|---|---|---|---|
| 所有 ports 列表 | 只有 Network 的 inet top；非 inet ports 只能通过别的路径间接看到 | 适合 | 增一个极简 `Ports` 页，按 `queue_size`/`memory` 排序即可。 |
| `controls` | 未显示 | 适合 | 对 port 类型定位有用。 |
| `slot` | detail `id` 类似但不等价；未单独显示 | 可选 | 从完整 `erlang:port_info(P)` 读取，补上。 |
| `parallelism` | 未显示 | 可选 | 低成本。 |
| `locking` | 未显示 | 可选 | 低成本，主要调度/driver 排查。 |
| `monitored_by` | Process detail 有；Port detail 没有 | 适合 | 与 observer 对齐。 |
| `local_address/local_port/remote_address/remote_port` 拆分 | observer_cli 显示 sockname/peername 字符串 | 可选 | 当前够用；拆分只为排序/对齐。 |
| port close / trace selected ports | 无 | 不建议 | 破坏性或侵入性操作，违背生产低风险定位。 |

observer port detail 还显示 `Registered Name`、`Connected`、`Slot`、`Controls`、`Parallelism`、`Locking`、`Queue Size`、`Memory`、links/monitors/monitored_by、inet 地址、stats、options。来源：`observer_port_wx.erl:499-565`。`observer_cli` 已覆盖 queue/memory/links/monitors/stats 和常见 options。

#### inet options 缺口

observer_backend 的 inet options 全量列表来源：`observer_backend.erl:226-232`。`observer_cli_port` 当前 options 子集来源：`src/observer_cli_port.erl:345-430`。

`observer_cli` 已显示：`active`、`broadcast`、`buffer`、`delay_send`、`dontroute`、`exit_on_close`、`header`、`high_watermark`、`keepalive`、`linger`、`low_watermark`、`mode`、`nodelay`、`packet`、`packet_size`、`priority`、`recbuf`、`reuseaddr`、`send_timeout`、`sndbuf`。

缺失但 observer 会尝试显示：`bind_to_device`、`deliver`、`high_msgq_watermark`、`ipv6_v6only`、`low_msgq_watermark`、`netns`、`read_packets`、`send_timeout_close`、`show_econnreset`、`tos`、`tclass`。

适配建议：适合补齐，但不要手写 30 个固定格子；最懒方案是复用现有 `render_opts/1` 思路，把 options 作为可分页 key/value 表渲染。这样未来 OTP 增减 option 也不用改布局。

### 4. Sockets

observer 2.19 有独立 Sockets 页，对应 OTP `socket` API，不等同于 inet ports。来源：`observer_wx.erl:206`、`observer_sock_wx.erl:99-114`、`observer_backend.erl:66-73`、`observer_backend.erl:269-440`。

| 缺失参数 | 是否适合加入 | 建议 |
|---|---|---|
| General socket info：`iov_max`、counter bit size、socket 总数、socket monitor 总数、按 domain/type/protocol 计数 | 条件适合 | 如果用户系统用 `socket` API，做独立 `Sockets` 页；否则先不加。 |
| Socket list：`id`、`owner`、`fd`、`domain`、`type`、`protocol`、`read state`、`write state` | 条件适合 | 单独页，按 owner/domain/type 过滤或排序。 |
| Socket detail：owner、fd、domain/type/protocol、read/write state、monitored_by、local/remote address | 条件适合 | 只读安全。 |
| Socket counters：accept/read/write tries/waits/fails/success、read/write bytes、packets、max packet | 条件适合 | 对 socket API 排查很有价值。 |
| Socket options：socket/ip/ipv6/tcp/udp/sctp options | 条件适合 | 做动态 key/value，不硬编码全部列。 |

### 5. ETS / Mnesia / Table Viewer

observer Table Viewer 列表字段：`Table Name`、`Objects`、`Size (kB)`、`Owner Pid`、`Owner Name`、`Table Id`。来源：`observer_tv_wx.erl:94-99`。详情字段包括 Identification/Owner、Settings、Memory Usage。来源：`observer_tv_wx.erl:363-396`。observer_backend 表字段来源：`observer_backend.erl:464-540`。

| 缺失参数/能力 | observer_cli 现状 | 是否适合加入 | 建议 |
|---|---|---|---|
| ETS `id` / table id | 未显示 | 适合 | 加列或详情页。 |
| ETS owner registered name / `reg_name` | owner pid 可替换注册名；未单独显示 owner name | 适合 | 加 `OwnerName`，避免丢 pid。 |
| ETS `heir` | 未显示 | 可选 | 低成本，少用。 |
| ETS `compressed` | 未显示 | 适合 | 排查内存时有用。 |
| ETS `fixed` | 未显示 | 适合 | 删除/并发排查有用。 |
| ETS `named_table` | 未显示 | 可选 | 从 id/name 可推断，优先级低。 |
| ETS table content | 不支持 | 不建议核心 | 可能扫描大表/私有表，和生产低消耗目标冲突。 |
| Mnesia `keypos` | 未显示 | 适合 | observer_backend 有固定字段。 |
| Mnesia `fixed` | 收集了部分 storage 类型 fixed，但未渲染 | 适合 | 既然已取到，补渲染即可。 |
| Mnesia `compressed` | ram/disc copies 收集了但未渲染 | 适合 | 同上。 |
| Mnesia table content | 不支持 | 不建议核心 | 同 ETS，表内容浏览适合独立工具/显式命令。 |

Crashdump Viewer 的 ETS 额外字段包括 slot、data structure、bucket 数、chain min/avg/max/stddev/expected stddev。来源：`cdv_ets_cb.erl:90-115`。这些是 dump 诊断字段，不建议并入 live 核心。

### 6. Applications

observer Applications 是 supervision tree / appmon 视图，支持 process info、send message、kill、trace 等交互。来源：`observer_app_wx.erl:215-240`。它不是以资源指标为中心。

`observer_cli` App 页已经提供 observer 原生 Applications 页没有的聚合指标：process count、memory、reductions、msgq、status、version。

| 缺失能力 | 是否适合加入 | 建议 |
|---|---|---|
| supervision tree/links | 不建议当前加入 | 终端渲染复杂，且和当前聚合指标页目标不同。 |
| send message / kill / trace process tree | 不建议 | 侵入/破坏性操作，不属于低风险诊断指标。 |

### 7. Trace

observer Trace Overview 与 Trace Options 覆盖：trace function call、arity、send/receive message、process events、process scheduling、exiting scheduling、GC、port events、port scheduling、match specs、ttb output。来源：`observer_traceoptions_wx.erl:37-54`、`observer_traceoptions_wx.erl:119-127`。

结论：不建议并入当前 `observer_cli` 核心。理由很简单：trace 会改变被观测节点负载，配置面很大，而且 `observer_cli` 现在定位是低消耗只读诊断。需要 trace 时应做显式独立命令或外部工具，不要藏在普通刷新 UI 里。

### 8. Crashdump Viewer / CDV

`observer` 应用还包含 Crashdump Viewer。这是离线 crashdump 分析，不是 live observer 页面。`observer_cli` 当前没有对应功能。

| CDV 指标域 | 缺失参数 | 是否适合加入当前 observer_cli |
|---|---|---|
| General | slogan、node name、crashdump created on、system version、compiled、taints、memory allocated、memory maximum、atoms、processes、ETS tables、timers、funs、calling thread | 不适合核心；可做独立 `observer_cli_cdv`。来源：`cdv_gen_cb.erl:35-50`。 |
| Atoms | creation order、atom | 不适合 live 核心。来源：`cdv_atom_cb.erl:39-41`。 |
| Processes | internal state、started、parent、run queue、program counter、continuation pointer、arity、old heap、heap unused、binary vheap、heap fragments、heap addresses | 不适合 live 核心；部分字段是 dump 内部结构。来源：`cdv_proc_cb.erl:127-166`。 |
| Ports | state、task flags、input/output bytes、queue bytes、port data、suspended | 不适合 live 核心；live port 页可只补 `controls/slot/parallelism/locking/monitored_by`。来源：`cdv_port_cb.erl:102-115`。 |
| Distribution | name、connection type、controller、channel、creation、remote links/monitors/monitored_by | 不适合 core；live System 已有 dist queue/address/in/out/type/state。来源：`cdv_dist_cb.erl:50-96`。 |
| Schedulers | current process/port、run queue length、port queue length、sleep flags/aux、priority queue lengths、current process stack/PC/CP | 不适合 live core；当前只保留 scheduler utilization 即可。来源：`cdv_sched_cb.erl:52-113`。 |
| Timers | owner、owner name、message、time left | 可独立考虑，但不建议默认加入；遍历 timers 对生产排查价值有限。来源：`cdv_timer_cb.erl:45-49`。 |
| Funs | module、uniq、index、address、native address、refc | 不适合 live core。来源：`cdv_fun_cb.erl:49-55`。 |
| Modules | module、current size、old size、attributes、compile info | 可选但低优先；live code memory 已覆盖总量。来源：`cdv_mod_cb.erl:46-73`。 |
| Memory / allocated areas | dump memory key/value、allocated/used areas、allocator tables | 不适合 live core；System allocator 已覆盖常用 live 视角。来源：`cdv_mem_cb.erl:45-90`。 |
| Persistent terms | persistent terms dump | 不建议默认；可能很大，适合显式插件/命令。来源：`cdv_persistent_cb.erl:32`。 |

## 推荐实现顺序

按“最少代码、最大诊断收益”排序：

1. **Process detail 小字段补齐**：`current_function`、`priority`、`stack_size`、`suspending`、`error_handler`；其它字段先折叠或子页。
2. **ETS/Mnesia metadata 补渲染**：利用现有收集结果，少写新逻辑。
3. **Port detail 补字段 + options 动态表**：先不做新 Ports 页，也能提升 Network drill-down。
4. **System 小字段补齐**：`ets_count/ets_limit`、`dist_buf_busy_limit`。
5. **独立 Sockets 页**：只有在确认目标用户用 OTP `socket` API 后再做。
6. **所有 Ports 页**：如果非 inet ports 的 queue/memory 排查是常见诉求，再做。

## 不建议加入核心的清单

- Trace Overview / Trace Options。
- Kill process、GC process、close port、send message。
- ETS/Mnesia 表内容浏览。
- Crashdump Viewer 全套功能。
- wx Load Charts 等价历史图表。
- 为未来 JSON/AI snapshot 预先设计新抽象；如果要做，应走当前 2.0 “渲染前 term/map seam”，不是在这个指标差异里顺手加。
