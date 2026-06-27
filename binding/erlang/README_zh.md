:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region erlang 查询客户端

### 简介
该bingding以erlang语言实现xdb查询客户端，基于Erlang OTP Application，查询逻辑由ip2region_worker工作进程实现，支持配多个工作进程来进行负载均衡。

### 应用配置
该应用可配置的参数在ip2region.app.src中,如下：
``` erlang
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

``` erlang
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

```
$ rebar3 compile
```

### 运行
将xdb文件放到priv目录下，然后启动erlang节点：
```
$ rebar3 shell
```
在erlang shell中调用xdb:search/1接口查询Ip地址信息, 该接口支持以list格式字符串、binary格式字符串、tuple和整数表示的IP地址，如下：
```
1> xdb:search("1.0.8.0").
[20013,22269,124,48,124,24191,19996,30465,124,24191,24030,
 24066,124,30005,20449]
2>
3> io:format("~ts~n", [xdb:search("1.0.8.0")]).
中国|0|广东省|广州市|电信
io:format("~ts~n", [xdb:search(<<"1.0.8.0">>)]).
中国|0|广东省|广州市|电信
4> io:format("~ts~n", [xdb:search({1,0,8,0})]).
中国|0|广东省|广州市|电信
6> io:format("~ts~n", [xdb:search(16779264)]).
中国|0|广东省|广州市|电信
```

### 使用方法
* 在rebar.config中引入依赖
```
{deps, [
  ip2region
]}.
```
* 启动ip2region Application
```
......

application:ensure_started(ip2region),

......
```

* 调用xdb:search/1接口查询IP信息
```
......

ip2region:search("1.0.8.0"),

......
```

### 单元测试

```
$ rebar3 eunit
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ip2region
===> Performing EUnit tests...
=INFO REPORT==== 17-Jan-2023::11:52:59.920155 ===
XdbFile:/home/admin/erl-workspace/ip2region/binding/erlang/_build/test/lib/ip2region/priv/ip2region.xdb

....
Finished in 0.074 seconds
4 tests, 0 failures
```

### 基准测试

IPv4 与 IPv6 共用一个脚本，通过参数指定版本：

```
$ cd benchmarks/
$ sh xdb-benchmark.sh ipv4
```

IPv6：

```
$ sh xdb-benchmark.sh ipv6
```

也可以在 `binding/erlang` 目录直接用 Makefile：

```
$ make bench-v4
$ make bench-v6
```

#### IPv4 基准测试示例

```
CPU info:
model name      : Apple M4
cores/threads   : 10/10

Erlang info:
system_version:Erlang/OTP 29 [erts-17.0.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]
load test data use 1.34496s

start run benchmark tests

search from file:
ip count:487169,
total time: 11.564375s,
search 42126.70377776577 times per second,
use 23.737912305585947 micro second per search

search from cache:
ip count:487169,
total time: 0.213552s,
search 2281266.389450813 times per second,
use 0.43835301507279817 micro second per search

benchmark test finish
```

#### IPv6 基准测试示例

```
CPU info:
model name      : Apple M4
cores/threads   : 10/10

Erlang info:
system_version:Erlang/OTP 29 [erts-17.0.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]
load test data use 2.472659s

start run benchmark tests

search from file:
ip count:638953,
total time: 18.463896s,
search 34605.535039842085 times per second,
use 28.897111368128797 micro second per search

search from cache:
ip count:638953,
total time: 0.541899s,
search 1179099.7953493178 times per second,
use 0.8481046336741513 micro second per search

benchmark test finish
```
