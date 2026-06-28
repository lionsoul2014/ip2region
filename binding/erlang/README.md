:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region Erlang query client

### Introduction

This binding implements the xdb query client in `Erlang`, based on the `Erlang OTP Application`. The query logic is implemented by the `ip2region_worker` worker process, supporting multiple worker processes for load balancing.

### Application Configuration

The configurable parameters for this application are in `ip2region.app.src`, as follows:

```erlang
  {env,[
    {poolargs, [
        {size, 1},  %% Default number of worker processes
        {max_overflow, 5}  %% Maximum number of worker processes
    ]},
    {db, [
        {ipv4, "ip2region.xdb"}  %% Default IPv4 xdb file
    ]}
  ]}
```

### Dual-stack configuration (IPv4 + IPv6)

To enable IPv6 queries, add the `ipv6` entry to the `db` list and place both xdb files under `priv/`:

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

The `xdb:search/1` interface automatically detects IPv4 and IPv6 inputs and routes them to the correct worker pool.

### Compile

```bash
$ rebar3 compile
```

### Run

Place the xdb file in the `priv` directory, then start the Erlang node:

```bash
$ rebar3 shell
```

Call the `xdb:search/1` interface in the Erlang shell to query IP address information. This interface supports IP addresses represented as list strings, binary strings, tuples, and integers:

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

With dual-stack enabled, IPv6 addresses are supported in the same way:

```erlang
1> io:format("~ts~n", [xdb:search("2001:4860:4860::8888")]).
United States|Florida|Miami|Google LLC|US
2> io:format("~ts~n", [xdb:search(<<"2001:4860:4860::8888">>)]).
United States|Florida|Miami|Google LLC|US
3> io:format("~ts~n", [xdb:search({8193,18528,18528,0,0,0,0,34952})]).
United States|Florida|Miami|Google LLC|US
```

### Usage

- Add the dependency in `rebar.config`

```erlang
{deps, [
  ip2region
]}.
```

- Start the ip2region Application

```erlang
{ok, _} = application:ensure_all_started(ip2region).
```

- Call the `xdb:search/1` interface to query IP information

```erlang
xdb:search("1.0.8.0").
```

### Unit Test

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
63 tests, 0 failures
```

### Benchmark

Both IPv4 and IPv6 benchmarks share the same script. Run it with the desired IP version:

> `cold` = first pass over the source file: each IP triggers a real search and the
> result is written into the ETS cache. `warm` = second pass over the same list,
> where every lookup is served directly from the ETS cache.

```bash
$ cd benchmarks/
$ sh xdb-benchmark.sh ipv4
```

For IPv6:

```bash
$ sh xdb-benchmark.sh ipv6
```

Or use the Makefile targets from the `binding/erlang` directory:

```bash
$ make bench-v4
$ make bench-v6
```

#### IPv4 benchmark example

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

#### IPv6 benchmark example

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
