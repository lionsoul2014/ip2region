# ip2region xdb erlang 查询客户端


### 编译

```
$ rebar3 compile
```
### 运行
将xdb文件放到priv目录下，然后启动erlang节点：
```
$ rebar3 shell

```
在erlang节点中执行以下指令查询Ip地址信息：
```
1> ip2region:search("1.0.8.0").
#{city => <<229,185,191,229,183,158,229,184,130>>,
  country => <<228,184,173,229,155,189>>,
  isp => <<231,148,181,228,191,161>>,
  province => <<229,185,191,228,184,156,231,156,129>>,
  region => <<>>}
2>
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

* 调用ip2region:search/1接口查询IP信息
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
=INFO REPORT==== 13-Jan-2023::21:26:15.137021 ===
XdbFile:/home/admin/erl-workspace/ip2region/binding/erlang/_build/test/lib/ip2region/priv/ip2region.xdb
...
Finished in 0.085 seconds
3 tests, 0 failures

```

### 基准测试
```
$ cd benchmarks/
$ sh ip2region-benchmark.sh
CPU info:
model name      : AMD EPYC 7K62 48-Core Processor
cache size      : 512 KB
cpu MHz         : 2595.124
bogomips        : 5190.24
cores/threads   : 2

Erlang info:
system_version:Erlang/OTP 24 [erts-12.3.2.2] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]
load test data use 2.724344s

start run benchmark tests

search from file:
ip count:683844,
total time: 25.56304s,
search 26751.27840820184 times per second,
use 37.381391077497206 micro second per search

search from cache:
ip count:683844,
total time: 0.670307s,
search 1020195.2239794602 times per second,
use 0.9802045495756342 micro second per search

benchmark test finish

```
