# observer_cli CLI 与 TUI 指标对比

日期：2026-07-11

## 1. 范围与判定方法

本文对比的是当前仓库中的两条入口：

- TUI：`observer_cli tui NODE [COOKIE REFRESH_MS]`
- 命令式 CLI：`observer_cli COMMAND [ARGUMENTS] [OPTIONS]`

只比较 TUI 已经展示或可以进入详情页查看的运行时数值。快捷键、分页、暂停、
颜色、终端布局不算指标。`snapshot`、`diagnose`、`trace` 等 CLI 独有能力单独列出，
不反向算作 TUI 缺口。

状态定义：

- **已覆盖**：CLI 有同一事实，单位和语义可以直接对应。
- **部分覆盖**：CLI 有相关数据，但字段、采样窗口或语义不同。
- **未实现**：TUI 可见，命令式 CLI 当前没有返回。
- **CLI 独有**：命令式 CLI 新增的能力，不是 TUI 参数。

审计依据：

- TUI collector/render：`observer_cli.erl`、`observer_cli_system.erl`、
  `observer_cli_inet.erl`、`observer_cli_process.erl`、`observer_cli_port.erl`、
  `observer_cli_socket.erl`、`observer_cli_ets.erl`、
  `observer_cli_mnesia.erl`、`observer_cli_application.erl`
- CLI collector：`observer_cli_snapshot.erl`
- CLI 路由和参数：`observer_cli_cli.erl`、`observer_cli_escriptize.erl`
- 实测目标：OTP 29.0.3，`observer_parity@127.0.0.1`，刷新间隔 1500 ms

动态值不是同一原子时刻采样。CLI 每次会增加 controller/worker，TUI 也有 redraw、
store 和 collector 进程，因此 process、port、atom、memory、reductions、IO、GC 等值
允许有观察者漂移。可以严格比较的是字段来源、单位、排序语义，以及同一稳定资源的
身份和静态值。

## 2. 总览

| TUI 页面/详情 | 对应 CLI | 结论 | 主要缺口 |
|---|---|---|---|
| Home 系统概要 | `snapshot`、`memory`、`schedulers` | 部分覆盖 | 主机 CPU/内存占比、active tasks、context switches、总 reductions/增量、逐 scheduler 利用率 |
| Home 进程 Top N | `processes` | 部分覆盖 | CLI 每行不总是返回 TUI 全部列；只有 reductions 支持窗口排序 |
| Process Info | `process TARGET`、`gen-server-state TARGET` | 部分覆盖 | `messages`、`dictionary` 有意排除；state 只返回有界 shape |
| Network | `network` | 部分覆盖 | 字段已覆盖；collector 未直接复用 recon 的两种视角 |
| Ports | `ports`、`port TARGET` | 已覆盖 | 列表字段及有界详情均覆盖；`locking` 为实现相关信息 |
| Sockets | `sockets` | 部分覆盖 | general counters、endpoint/state/owner/fd、accept/max packet、详情 counters/options |
| System | `snapshot`、`memory`、`schedulers`、`distribution`、`network` | 部分覆盖 | 主机 RSS/VSZ、CPU 拓扑细项、distribution 连接细节 |
| ETS | `ets` | 已覆盖 | 包含 write/read concurrency |
| Mnesia | `mnesia` | 部分覆盖 | type、owner、index、registered name |
| App | `applications` | 部分覆盖 | version；`no_group` 只有数量，没有完整资源聚合行 |
| Doc | 无 | 不属于指标 | TUI 内置帮助，不需要做成指标命令 |
| Plugin | 无 | 未实现扩展协议 | CLI 没有执行 TUI plugin sheet 的通用命令 |

当前没有一个 TUI 数据页达到“所有可见字段逐项完全等价”。CLI 已覆盖主要资源清单和
自动化所需的稳定 envelope，但它不是 TUI 页面的无损文本导出。

## 3. 实际数值对比

### 3.1 同一目标的采样结果

下面的 TUI 值来自实际 `-noshell` escript TUI，不是 `rebar3 shell`；CLI 值来自紧邻的
`snapshot --deep --include-identifiers --format json`。差异包含正常采样漂移和 CLI
明确标记的 observer contamination。

| 指标 | TUI 实测 | CLI 实测 | 结果 |
|---|---:|---:|---|
| OTP release | `29` | `29` | 已覆盖 |
| ERTS/runtime | `17.0.3` | `17.0.3` | 已覆盖 |
| Architecture | `aarch64-apple-darwin` | `aarch64-apple-darwin` | 已覆盖 |
| Process count/limit | `59 / 1048576` | `62 / 1048576` | 同源；CLI 包含 controller/worker |
| Port count/limit | `4 / 1048576` | `5 / 1048576` | 同源；CLI distribution controller 增加一个 port |
| Atom count/limit | `13833 / 1048576` | `13209 / 1048576` | 同源；CLI 在采样后继续加载模块，不能按时序严格相等 |
| ETS count/limit | `21 / 8192` | `21 / 8192` | 已覆盖，实测相等 |
| Persistent terms | `26 / 32.0000 KiB` | `26 / 32768 bytes` | 已覆盖，单位换算后相等 |
| Total BEAM memory | `49.1206 MiB` | `51075056 bytes`（约 `48.71 MiB`） | 同源，动态漂移 |
| Process memory | `13.9937 MiB` | `14766080 bytes`（约 `14.08 MiB`） | 同源，动态漂移 |
| Binary memory | `225.1875 KiB` | `232480 bytes`（约 `227.03 KiB`） | 同源，动态漂移 |
| Code memory | `6.7815 MiB` | `6657615 bytes`（约 `6.35 MiB`） | 同源；模块加载时点不同 |
| ETS memory | `583.4922 KiB` | `593968 bytes`（约 `580.05 KiB`） | 同源，动态漂移 |
| Run queue | `1` | normal/dirty observed runnable 均为 `0`，queue lengths 全 `0` | 部分覆盖；采样定义和时点不同 |
| IO input total | `99.8438 KiB` | `97746 bytes`（约 `95.46 KiB`） | 同源，动态累计值 |
| IO output total | `582.2119 KiB` | `550229 bytes`（约 `537.33 KiB`） | 同源，动态累计值 |
| GC collections total | `1308` | `1208` | 同源，动态累计值 |
| GC reclaimed words | `4796373` | `4554626` | 已覆盖；CLI 另给 `36437008 bytes` |

### 3.2 稳定资源行

`code_server` 在 TUI Home 和 CLI `process code_server` 中可以直接对应：

| 字段 | TUI | CLI | 结果 |
|---|---|---|---|
| PID | `<0.50.0>` | `<0.50.0>` | 相等 |
| registered name | `code_server` | `code_server` | 相等 |
| current function | `code_server:loop/1` | `{code_server, loop, 1}` | 结构化表达相等 |
| memory | `950.4141 KiB` | `973224 bytes` | 单位换算后相等 |
| message queue len | `0` | `0` | 相等 |
| reductions | `204045` | `202879`（另一采样时刻） | 累计值随时间增长 |

CLI 深度快照还实测得到：

- distribution：一个 TUI controller hidden peer，queue `0 / 1048576 bytes`；CLI 自己的
  controller 被排除。
- ETS：扫描 21 张表；Top 1 为 `code_server`，`size=413`、`memory=201832 bytes`。
- Mnesia：`status=not_running`，没有表行；TUI 同样不显示 Mnesia 菜单项。
- Ports：扫描 5 个 port，排除 CLI controller 后只有非 inet `forker` 进入列表。
- Sockets：OTP socket registry 为空，`status=empty`；这不代表没有 legacy inet ports。

## 4. Home 页面逐项对比

### 4.1 系统概要

| TUI 字段 | CLI 字段/命令 | 状态 | 说明 |
|---|---|---|---|
| process count / limit | `snapshot.data.resources.process` | 已覆盖 | CLI 标明 `observer_contaminated` |
| port count / limit | `snapshot.data.resources.port` | 已覆盖 | CLI 标明 `observer_contaminated` |
| atom count / limit | `snapshot.data.resources.atom` | 已覆盖 | CLI 标明 `observer_contaminated` |
| OTP version | `memory.data.runtime.otp_release` | 已覆盖 | |
| 完整 ERTS build flags | 无 | 未实现 | TUI header 含 source、64-bit、smp、dirty scheduler、async、jit |
| host `ps -o pcpu` | 无 | 未实现 | CLI memory 是 BEAM memory，不是 host RSS/CPU |
| host `ps -o pmem` | 无 | 未实现 | |
| active tasks | 无 | 未实现 | TUI 使用 `statistics(total_active_tasks)` |
| context switches total | 无 | 未实现 | |
| reductions total / since last | 无全局字段 | 未实现 | CLI 只提供进程 reductions 和诊断采样 |

### 4.2 Memory、IO、GC

| TUI 字段 | CLI 字段/命令 | 状态 | 说明 |
|---|---|---|---|
| total memory | `memory.beam.total_bytes` | 已覆盖 | |
| processes used | `memory.beam.processes_used_bytes` | 已覆盖 | |
| code | `memory.beam.code_bytes` | 已覆盖 | |
| atom used | `memory.beam.atom_used_bytes` | 已覆盖 | |
| binary | `memory.beam.binary_bytes` | 已覆盖 | |
| ETS memory/count | `memory.beam.ets_bytes` + `resources.ets` | 已覆盖 | count 需要 `snapshot`，不在单独 `memory` 中 |
| persistent term count/memory | `memory.persistent_term` | 已覆盖 | |
| run queue | `snapshot.schedulers.run_queue_lengths` 或 `schedulers.run_queues` | 部分覆盖 | CLI 明确区分 normal/dirty，并声明非原子采样 |
| error_logger/logger queue | 无 | 未实现 | TUI 在 run queue 位置旁显示 logger queue |
| IO input/output total | `snapshot.memory.io`、`network.vm_port_driver_io` | 已覆盖 | |
| IO input/output interval delta | `network --duration DURATION` | 已覆盖 | TUI 固定使用 refresh interval；CLI 显式指定窗口 |
| GC count total | `snapshot.memory.garbage_collection.collections_total` | 已覆盖 | |
| GC count interval delta | 无 | 未实现 | |
| GC reclaimed words total | `reclaimed_words_total` | 已覆盖 | |
| GC reclaimed words interval delta | 无 | 未实现 | |
| port parallelism | 无 | 未实现 | Home 中的全局 port parallelism 状态没有 CLI 字段 |

### 4.3 Scheduler usage

| TUI 字段 | CLI 字段/命令 | 状态 | 说明 |
|---|---|---|---|
| 每个 normal scheduler 利用率 | 无逐 scheduler items | 未实现 | CLI 只有 normal aggregate ratio |
| normal scheduler aggregate | `schedulers.data.normal.utilization_ratio` | 部分覆盖 | TUI 主要渲染逐 scheduler bar |
| dirty CPU aggregate | `schedulers.data.dirty_cpu.utilization_ratio` | CLI 独有/补充 | TUI Home 不单独给 aggregate |
| scheduler configured/online | `schedulers.data.topology` | 已覆盖 | |
| normal/dirty run queue start/end | `schedulers.data.run_queues` | CLI 独有/补充 | 带非原子和 observer contamination 声明 |

### 4.4 Process Top N

| TUI 能力/字段 | CLI | 状态 | 说明 |
|---|---|---|---|
| sort by memory | `processes --sort memory` | 已覆盖 | |
| sort by binary memory | `--sort binary_memory` | 已覆盖 | |
| sort by reductions total | `--sort reductions` | 已覆盖 | |
| sort by total heap size | `--sort total_heap_size` | 已覆盖 | |
| sort by message queue len | `--sort message_queue_len` | 已覆盖 | |
| memory/reductions/binary/heap/msgq 窗口排序 | memory、binary、heap、msgq、reductions 均支持 `--duration` | 已覆盖 | 统一使用窗口 delta 排名与 `*_delta`/`*_per_second` |
| PID | `pid` | 已覆盖 | |
| name/label/initial call | name + initial call | 部分覆盖 | CLI 没有 proc label；name 和 initial call 已有 |
| current function | `current_function` | 已覆盖 | |
| memory、reductions、msgq 同行展示 | 只保证返回当前 sort 所需字段 | 部分覆盖 | 例如 `--sort memory` 行没有 reductions/msgq |
| 分页浏览全部 Top N | `--limit 1..200` | 等价替代 | CLI 无交互分页，使用 limit |

## 5. Process 详情逐项对比

### 5.1 基本与内存字段

| TUI 字段 | `process TARGET` | 状态 |
|---|---|---|
| pid、registered name、initial call、group leader、status | 同名结构化字段 | 已覆盖 |
| current function | `current_function` | CLI 补充；TUI 在 Home 行显示，详情 meta 不显示 |
| memory、reductions、message queue len | 同名/bytes 字段 | 已覆盖 |
| heap size、total heap size、stack size | `*_bytes` | 已覆盖 |
| priority | `priority` | 已覆盖 |
| binary refs count/bytes | `binary_refs_count`、`binary_refs_bytes` | 已覆盖 |
| catchlevel | `catchlevel` | 已覆盖 |
| suspending | `suspending`、`suspending_total_count` | 已覆盖；明细最多 30 项，另标记是否截断 |
| error_handler | `error_handler` | 已覆盖 |
| trap_exit | `trap_exit` | 已覆盖 |

### 5.2 GC、signals 与子视图

| TUI 字段/子视图 | CLI | 状态 | 说明 |
|---|---|---|---|
| GC min_bin_vheap_size | `garbage_collection_info.min_bin_vheap_size` | 已覆盖 | TUI 与 `garbage_collection_info` 同步 |
| GC min_heap_size | `garbage_collection_info.min_heap_size` | 已覆盖 | TUI 与 `garbage_collection_info` 同步 |
| GC fullsweep_after | `garbage_collection_info.fullsweep_after` | 已覆盖 | TUI 与 `garbage_collection_info` 同步 |
| GC minor_gcs | `garbage_collection_info.minor_gcs` | 已覆盖 | TUI 与 `garbage_collection_info` 同步 |
| links | `links`、`links_total_count` | 已覆盖 | 明细最多 30 项，另标记是否截断 |
| monitors | `monitors`、`monitors_total_count` | 已覆盖 | 明细最多 30 项，另标记是否截断 |
| monitored_by | `monitored_by`、`monitored_by_total_count` | 已覆盖 | 明细最多 30 项，另标记是否截断 |
| reductions/memory 趋势图 | 无 | 未实现 | CLI 是单次事实或显式 bounded window，不返回历史图 |
| messages | 无 | 未实现 | 有意避免复制消息内容 |
| dictionary | 无 | 未实现 | 有意避免复制进程字典 |
| current stacktrace | `current_stacktrace` | 部分覆盖 | 最多返回 30 帧；参数只返回 arity，源码路径不返回 |
| raw state | `gen-server-state TARGET` | 部分覆盖 | CLI 在目标端复制后只返回有界、去值的 shape，不返回 TUI `recon:get_state/2` 原值 |

因此，`process TARGET` 目前是安全的 metadata 详情，不是 TUI Process Info 的完整导出。

## 6. Network 页面逐项对比

| TUI 字段/排序 | `network` | 状态 | 说明 |
|---|---|---|---|
| VM IO input/output total | `vm_port_driver_io.*_bytes_total` | 已覆盖 | |
| VM IO input/output delta | `--duration` 的 `*_bytes_delta` | 已覆盖 | |
| recv_oct、send_oct、oct | 同名字段和 sort | 已覆盖 | legacy inet ports only |
| recv_cnt、send_cnt、cnt | 同名字段和 sort | 已覆盖 | lifetime total 或 duration delta |
| port id | `resource` | 已覆盖 | |
| protocol | `protocol` | 已覆盖 | |
| port input/output | `input`、`output` | 已覆盖 | duration 使用第二次采样的当前值 |
| queue_size | `queue_size` | 已覆盖 | 不扩展 sort allowlist |
| memory | `memory` | 已覆盖 | 不扩展 sort allowlist |
| peername | `peername` | 已覆盖 | 默认显示；`--redact` 和 deep snapshot 使用稳定 endpoint ID；listener 为 `null` |
| recon `inet_count`/`inet_window` 两种视角 | point-in-time 或 `--duration` | 部分覆盖 | CLI 使用自己的有界 counter collector |

`network --sort oct --limit 10` 和带 `--duration` 的窗口路径均已通过 OTP 29 live
验证。枚举后、读取 counters 前消失的 inet port 会计入 `disappeared_count`，不会再使
整个 command 返回 `controller_failed`。

## 7. Ports 页面逐项对比

TUI 与 CLI 都排除 `tcp_inet`、`udp_inet`、`sctp_inet`，只在 Ports 页列非 inet port。

| TUI 字段/能力 | `ports` | 状态 | 说明 |
|---|---|---|---|
| port/id | `resource`、`display_id` | 已覆盖 | |
| connected | `connected_pid` | 已覆盖 | |
| name | `name`、`controls` | 已覆盖 | `controls` 是同值同类型 alias |
| queue_size | `queue_size` | 已覆盖 | 可排序 |
| memory | `memory` | 已覆盖 | 可排序 |
| input/output/io | 同名字段 | CLI 补充 | TUI list 不展示，detail 可见部分 stats |
| controls | `controls` | 已覆盖 | `name` alias，不重复采集 |
| slot | `slot` | 已覆盖 | `display_id` alias，不作为跨采样 identity |
| parallelism | `parallelism` | 已覆盖 | 缺失为 `null` 并进入 `field_errors` |
| locking | `locking` | 已覆盖 | 实现相关信息；缺失为 `null` |
| os_pid | `port TARGET.os_pid` | 已覆盖 | 缺失为 `null` |
| monitors/monitored_by | `port TARGET` 同名字段 | 已覆盖 | 各自最多 30 项，另有 total/truncated |
| port detail：links/monitor | `port TARGET` | 已覆盖 | 只接受目标节点本地 raw `#Port<0.N>` 文本 |
| port detail：sockname/peername | `port TARGET.inet` | 已覆盖 | 默认显示，`--redact` 使用报告内稳定 endpoint ID |
| port detail：inet statistics/options | `port TARGET.inet` | 已覆盖 | 固定 10 项 stats；TUI allowlist options 逐项标状态 |

## 8. Sockets 页面逐项对比

| TUI 字段/能力 | `sockets` | 状态 | 说明 |
|---|---|---|---|
| socket identity | `resource` | 已覆盖 | opaque registry identity |
| domain/type/protocol | 同名字段 | 已覆盖 | |
| read/write/io bytes | 同名 metrics | 已覆盖 | point-in-time total 或显式 delta |
| packets | `packets` | 已覆盖 | required/optional counter 状态另行返回 |
| waits/fails | 同名 metrics | 已覆盖 | |
| owner | 无 | 未实现 | |
| fd | 无 | 未实现 | |
| local/remote endpoint | 无 | 未实现 | CLI 明确不额外获取 endpoint |
| read/write state | 无 | 未实现 | |
| accept success/tries | 无 | 未实现 | TUI 可显示并按 accepts 排序 |
| max packet | 无 | 未实现 | TUI 可显示并排序 |
| sort by id/fd/owner/domain/type/protocol | 无 | 未实现 | CLI 只按六个 counter metric 排序 |
| general：socket/monitor/domain/type/protocol counts、iov_max | 无 | 未实现 | |
| detail：monitored_by | 无 | 未实现 | |
| detail：完整 counters | 无 | 未实现 | CLI 只公开聚合后的六个 metrics |
| detail：socket options | 无 | 未实现 | |

CLI 的 `registry_known_count`、`use_registry`、born/gone/reset/shape-change lifecycle 是
自动化用的补充字段，TUI 没有同名输出。

## 9. System 页面逐项对比

### 9.1 Runtime、CPU、memory、statistics

| TUI 字段组 | CLI | 状态 |
|---|---|---|
| OTP/ERTS version、architecture、word size | `memory.runtime` | 已覆盖 |
| SMP/thread support、async thread pool size | 无独立字段 | 未实现 |
| logical/online/available CPUs | 无 | 未实现 |
| configured/online schedulers | `schedulers.topology` | 已覆盖 |
| available schedulers | 无 | 未实现 |
| dirty CPU configured/online | `schedulers.topology` | 已覆盖 |
| host ps CPU、memory、RSS、VSZ | 无 | 未实现 |
| BEAM total/process/atom/binary/code/ETS memory | `memory.beam` | 已覆盖 |
| total IO input/output | `snapshot.memory.io` 或 `network` | 已覆盖 |
| module count | 无 | 未实现 |
| process/port/atom/ETS count-limit | `snapshot.resources` | 已覆盖 |
| global dist_buf_busy_limit | 只在每个 `distribution.controller_queues[].busy_limit_bytes` | 部分覆盖 |

### 9.2 Allocator

`memory.data.memory.allocator` 已结构化覆盖 System 页的 allocator 指标：

- 各 util allocator 的 current/max MBCS average block size；
- current/max SBCS average block size；
- current/max SBCS-to-MBCS ratio；
- 每个 allocator instance 的 hits、calls、cache hit rate。

block size 使用 bytes，ratio 保留原始数值；cache hit rows 按 instance ID 排序。默认
`snapshot` 和 `diagnose` 仍不采 allocator，只有显式 `memory` 执行现有 `recon_alloc` collector。

### 9.3 Distribution

| TUI 字段 | `distribution` | 状态 | 说明 |
|---|---|---|---|
| visible/hidden connected nodes | `visible_peers`、`hidden_peers` | 已覆盖 | CLI 还排除自身 controller |
| dist queue size | `observed_queue_size_bytes` | 已覆盖 | |
| busy limit | `busy_limit_bytes` | 已覆盖 | |
| queue percent | 可由 size/limit 计算，但无字段 | 部分覆盖 | |
| health ok/warn/down/unknown | `health_inference=unavailable` | 未实现同等判断 | CLI 有意把 queue 当 context，不判 root cause/health |
| address | 无 | 未实现 | |
| in/out | 无 | 未实现 | |
| connection type | 无 | 未实现 | |
| connection state | 无 | 未实现 | |

## 10. ETS 页面逐项对比

| TUI 字段 | `ets` | 状态 |
|---|---|---|
| name、size、memory、type、protection、keypos、owner | 同名结构化字段 | 已覆盖 |
| write_concurrency | 同名字段 | 已覆盖 |
| read_concurrency | 同名字段 | 已覆盖 |

CLI 另有 `table_id`、`management`、扫描/消失/截断审计字段；TUI 没有对应列。

## 11. Mnesia 页面逐项对比

| TUI 字段 | `mnesia` | 状态 |
|---|---|---|
| name/table、size、memory、storage | `table`、`size`、`memory_bytes`/`disk_bytes`、`storage_type` | 已覆盖 |
| type | 无 | 未实现 |
| owner | 无 | 未实现 |
| index | 无 | 未实现 |
| registered name | 无 | 未实现 |
| hide/show system tables | 无选项 | 未实现 |

CLI 只列 local tables，并明确区分 RAM/disc memory bytes 与 disc-only disk bytes；这个单位
语义比 TUI 的单个 `Memory` 列更适合自动化。

## 12. App 页面逐项对比

| TUI 字段 | `applications` | 状态 | 说明 |
|---|---|---|---|
| application | `application` | 已覆盖 | |
| process count | `process_count` | 已覆盖 | |
| memory | `memory_bytes` | 已覆盖 | |
| reductions | `reductions` | 已覆盖 | |
| message queue len | `message_queue_len` | 已覆盖 | |
| status | `loaded` + `running` | 已覆盖 | 表达形式不同 |
| version | 无 | 未实现 | |
| `no_group` 完整聚合行 | 只有 `unattributed_process_count` | 部分覆盖 | 未返回 unattributed memory/reductions/msgq |

两边都按 group leader 推断 application 归属；CLI 明确标记
`attribution_semantics=approximation`。

## 13. TUI 中尚未在 CLI 实现的参数清单

按优先级合并去重后：

### P0：直接影响常见排障

1. Ports：controls、slot、parallelism、locking、monitors、monitored_by。
2. Distribution：address、in/out、type、state。
3. Applications：version 和 `no_group` 的 memory/reductions/msgq 聚合。

### P1：有诊断价值，但可按需增加

1. Home 全局 active tasks、context switches、reductions total/delta、GC interval delta、
   logger queue、port parallelism。
2. 逐 scheduler 利用率，而不是只有 normal/dirty aggregate。
3. 非 reductions 的 process window Top N。
4. Socket owner/fd/endpoint/state、accepts、max packet 和 identity/metadata sorts。
5. Mnesia type、owner、index、registered name。
6. System module count、logical CPU 细项、thread/async 信息。

### P2：成本或暴露面较高

1. Socket 的完整详情、完整 counters/options。
2. Process messages、dictionary（保留有意不返回）。
3. TUI plugin sheet 的通用 CLI 执行协议。

不建议为了“字段完全相等”直接把 process state、messages、dictionary、完整 stacktrace 或完整
socket options 塞进 `snapshot`。这些数据复制成本和敏感信息风险高；若要补，应保持独立、
显式、有界的独立命令。

## 14. CLI 独有能力

以下不是 TUI 缺失参数，但说明 CLI 已经超出“导出 TUI”范围：

- `connect`、`status`、`disconnect` 的持久目标上下文；
- `snapshot` 的稳定 schema、probe coverage、observer effects、扫描审计和 redaction；
- `diagnose` 的 findings/suspects/context；
- `port TARGET` 的有界 signals、endpoint、statistics 和固定 allowlist options；
- `gen-server-state` 的 value-free bounded shape；
- `supervision-tree --app APP` 的一层 public supervision 结构；
- `trace call MFA` / `trace stop --all` 的 bounded instrumentation；
- text、consultable term、JSON 三种输出；
- limit、timeout、resource budget、born/gone/reset/shape-change 语义。

结论：命令式 CLI 已实现 TUI 的主要“资源是什么、当前多大、Top N 是谁”，但尚未实现
TUI 的全部“详情和辅助上下文”。最明显的缺口集中在 Process signals/GC、
Ports detail、Sockets detail，以及各页面只为人读而存在的扩展字段。
