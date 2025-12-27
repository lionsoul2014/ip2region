# ip2region xdb erlang 查询客户端

### 简介
该bingding以erlang语言实现xdb查询客户端，基于Erlang OTP Application，查询逻辑由ip2region_worker工作进程实现，支持配多个工作进程来进行负载均衡。

### 应用配置
该应用可配置的参数在ip2region.app.src中,如下：
``` erlang
  {env,[
    {poolargs, [
        {size, 1},  %% 工作进程默认数量
        {max_overflow, 5}  %% 工作进程最大数量
    ]}
  ]}
```

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
```
$ cd benchmarks/
$ sh xdb-benchmark.sh
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ip2region
Erlang/OTP 24 [erts-12.3.2.2] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

Eshell V12.3.2.2  (abort with ^G)
1> =INFO REPORT==== 17-Jan-2023::11:37:35.631095 ===
XdbFile:/home/admin/erl-workspace/ip2region/binding/erlang/_build/default/lib/ip2region/priv/ip2region.xdb

===> Booted ip2region
===> Evaluating: "xdb_benchmark:main(\"../../data/ip.merge.txt\"), init:stop()."
CPU info:
model name      : AMD EPYC 7K62 48-Core Processor
cache size      : 512 KB
cpu MHz         : 2595.124
bogomips        : 5190.24
cores/threads   : 2

Erlang info:
system_version:Erlang/OTP 24 [erts-12.3.2.2] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]
load test data use 4.835593s

start run benchmark tests

search from file:
ip count:683844,
total time: 28.201699s,
search 24248.326315375536 times per second,
use 41.23995969841075 micro second per search

search from cache:
ip count:683844,
total time: 0.671801s,
search 1017926.4395259906 times per second,
use 0.9823892583688677 micro second per search

benchmark test finish

```
