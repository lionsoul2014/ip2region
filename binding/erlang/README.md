:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region erlang query client

### Introduction

This binding implements the xdb query client in Erlang, based on the Erlang OTP Application. The query logic is implemented by the `ip2region_worker` worker process, supporting multiple worker processes for load balancing.

### Application Configuration

The configurable parameters for this application are in `ip2region.app.src`, as follows:

```erlang
  {env,[
    {poolargs, [
        {size, 1},  %% Default number of worker processes
        {max_overflow, 5}  %% Maximum number of worker processes
    ]}
  ]}
```

### Compile

```
$ rebar3 compile
```

### Run

Place the xdb file in the `priv` directory, then start the Erlang node:

```
$ rebar3 shell
```

Call the `xdb:search/1` interface in the Erlang shell to query IP address information. This interface supports IP addresses represented as list strings, binary strings, tuples, and integers, as follows:

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

### Usage

* Add the dependency in `rebar.config`

```
{deps, [
  ip2region
]}.
```

* Start the ip2region Application

```
......

application:ensure_started(ip2region),

......
```

* Call the `xdb:search/1` interface to query IP information

```
......

ip2region:search("1.0.8.0"),

......
```

### Unit Test

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

### Benchmark

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
