:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region Erlang 查询客户端

### 简介

该 bingding 以 `Erlang` 语言实现 xdb 查询客户端，基于 `Erlang OTP Application`，查询逻辑由 `ip2region_worker` 工作进程实现，支持配多个工作进程来进行负载均衡。

### 应用配置

该应用可配置的参数在 `ip2region.app.src` 中, 如下：

```erlang
  {env,[
    {poolargs, [
        {size, 1},  %% 工作进程默认数量
        {max_overflow, 5}  %% 工作进程最大数量
    ]},
    {db, [
        {ipv4, "ip2region.xdb"}  %% 默认 IPv4 xdb 文件
    ]}
  ]}
```

### 双栈配置（IPv4 + IPv6）

如需启用 IPv6 查询，在 `db` 列表中加入 `ipv6` 项，并将两个 xdb 文件放到 `priv/` 目录下：

```erlang
  {env,[
    {poolargs, [
        {size, 1},
        {max_overflow, 5}
    ]},
    {db, [
        {ipv4, "ip2region.xdb"},
        {ipv6, "ip2region_v6.xdb"}
    ]}
  ]}
```

`xdb:search/1` 接口会自动识别 IPv4 与 IPv6 输入，并将其路由到对应的工作进程池。

### 编译

```bash
$ rebar3 compile
```

### 运行

将 xdb 文件放到 `priv` 目录下，然后启动 Erlang 节点：

```bash
$ rebar3 shell
```

在 Erlang shell 中调用 `xdb:search/1` 接口查询 IP 地址信息。该接口支持 list 字符串、binary 字符串、tuple 和整数表示的 IP 地址：

```erlang
1> xdb:search("1.0.8.0").
[20013,22269,124,24191,19996,30465,124,24191,24030,24066,
 124,20013,22269,30005,20449,124,67,78]
2>
3> io:format("~ts~n", [xdb:search("1.0.8.0")]).
中国|广东省|广州市|中国电信|CN
4> io:format("~ts~n", [xdb:search(<<"1.0.8.0">>)]).
中国|广东省|广州市|中国电信|CN
5> io:format("~ts~n", [xdb:search({1,0,8,0})]).
中国|广东省|广州市|中国电信|CN
6> io:format("~ts~n", [xdb:search(16779264)]).
中国|广东省|广州市|中国电信|CN
```

启用双栈后，IPv6 地址也按同样方式支持：

```erlang
1> io:format("~ts~n", [xdb:search("2001:4860:4860::8888")]).
United States|Florida|Miami|Google LLC|US
2> io:format("~ts~n", [xdb:search(<<"2001:4860:4860::8888">>)]).
United States|Florida|Miami|Google LLC|US
3> io:format("~ts~n", [xdb:search({8193,18528,18528,0,0,0,0,34952})]).
United States|Florida|Miami|Google LLC|US
```

### 使用方法

- 在 `rebar.config` 中引入依赖

```erlang
{deps, [
  ip2region
]}.
```

- 启动 ip2region Application

```erlang
{ok, _} = application:ensure_all_started(ip2region).
```

- 调用 `xdb:search/1` 接口查询 IP 信息

```erlang
xdb:search("1.0.8.0").
```

### 单元测试

```bash
$ rebar3 eunit
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ip2region
===> Performing EUnit tests...
=INFO REPORT==== 28-Jun-2026::04:53:28 ===
XdbFile:/Users/nana/Documents/code/ip2region/.worktrees/erlang-ipv6/binding/erlang/_build/test/lib/ip2region/priv/ip2region.xdb

....
Finished in 0.192 seconds
38 tests, 0 failures
```

### 基准测试

IPv4 与 IPv6 共用一个脚本，通过参数指定版本：

> `cold` = 第一次遍历源文件：每个 IP 都会触发真实查询，并把结果写入 ETS 缓存。
> `warm` = 第二次遍历同一列表，所有查询都直接命中 ETS 缓存。

```bash
$ cd benchmarks/
$ sh xdb-benchmark.sh ipv4
```

IPv6：

```bash
$ sh xdb-benchmark.sh ipv6
```

也可以在 `binding/erlang` 目录直接用 Makefile：

```bash
$ make bench-v4
$ make bench-v6
```

#### IPv4 基准测试示例

```bash
System:
  CPU    : Apple M4
  Cores  : 10 cores / 10 threads
  Erlang : Erlang/OTP 29 [erts-17.0.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]
  Loaded : 487169 IPs in 1.335 s

Benchmarks:
  cold      total=  9.601s  count= 487169  qps=    50740.66  avg= 0.019708 ms/op (19.708 us/op)
  warm      total=  0.160s  count= 487169  qps=  3053164.29  avg= 0.000328 ms/op ( 0.328 us/op)

Done.
```

#### IPv6 基准测试示例

```bash
System:
  CPU    : Apple M4
  Cores  : 10 cores / 10 threads
  Erlang : Erlang/OTP 29 [erts-17.0.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]
  Loaded : 638953 IPs in 2.949 s

Benchmarks:
  cold      total= 20.504s  count= 638953  qps=    31162.52  avg= 0.032090 ms/op (32.090 us/op)
  warm      total=  0.444s  count= 638953  qps=  1437781.56  avg= 0.000696 ms/op ( 0.696 us/op)

Done.
```
