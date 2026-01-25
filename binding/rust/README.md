## `ip2region xdb rust` 查询客户端实现

## Features
- 支持`ip`字符串和`u32`/`u28` 数字两种类型的查询
- 支持 IPv4 和 IPv6
- 支持无缓存，Vector 索引缓存，全部数据缓存三种模式

## 缓存策略对比与说明
| 缓存模式     | IPv4 数据内存占用 | IPv6 数据内存占用 | IPv4 benchmark 查询耗时 | IPv6 benchmark 查询耗时 |
| ------------ | ----------- | ----------- | ------------------- |---------------------|
| 无缓存       | 1-2MB       | 1-2MB       | 54 us               | 122us               |
| vector index | 1-2MB       | 1-2MB       | 27 us               | 100us               |
| 全部缓存     | 20 MB       | 200 MB      | 120 ns              | 178 ns              |

- 在 `ip2region::Searcher` 初始化的时候会产生一次 IO, 读取 `xdb` 的 header 信息以初始化 `Searcher`，header 信息主要包含了 `xdb` 的 IP 版本，该操作对后续 IP 的查询不产生性能，耗时影响，多占用约 20 Byte 的内存
- 在无缓存模式与 `vector index` 缓存模式下，所有 `xdb` 的 IO 读取都是按需（按照 bytes offset, bytes length）读取少量信息, 都是线程安全的，可以 benchmark 测试验证
- 在全部缓存模式下，`xdb` 文件会一次读取，加载到内存中，测试 `IPv6 xdb` 文件大约占用内存 200MB 左右，查询不频繁的话，占用内存会逐渐降低
- 所有缓存模式下，包括初始化 `ip2region::Searcher` 过程当中，程序都是线程安全的，不存在某个全局可修改的中间变量，`ip2region::Searcher` 初始化完成以后，调用函数`search`都是使用不可变引用，同时 `ip2region::Searcher` 也可以通过 `Arc` 方式传递给不同线程使用

## 使用方式

使用`cargo`新建一个项目，比如`cargo new ip-test`

配置`Cargo.toml`的`[dependencies]`如下

```toml
[dependencies]
ip2region = { git = "https://github.com/lionsoul2014/ip2region.git", branch = "master" }
```

### 基本使用示例

编写`main.rs`

```rust
use ip2region::{CachePolicy, Searcher};

fn main() {
    for cache_policy in [
        CachePolicy::NoCache,
        CachePolicy::FullMemory,
        CachePolicy::VectorIndex,
    ] {
        let ipv4_seacher = Searcher::new("../ip2region/data/ip2region_v4.xdb".to_owned(), cache_policy).unwrap();
        for ip in [1_u32, 2, 3] {
            let result = ipv4_seacher.search(ip).unwrap();
            println!("CachePolicy: {cache_policy:?}, IP: {ip}, Result: {result}");
        }

        for ip in ["1.1.1.1", "2.2.2.2"] {
            let result = ipv4_seacher.search(ip).unwrap();
            println!("CachePolicy: {cache_policy:?}, IP: {ip}, Result: {result}");
        }

        let ipv6_seacher = Searcher::new("../ip2region/data/ip2region_v6.xdb".to_owned(), cache_policy).unwrap();
        for ip in ["2001::", "2001:4:112::"] {
            let result = ipv6_seacher.search(ip).unwrap();
            println!("CachePolicy: {cache_policy:?}, IP: {ip}, Result: {result}");
        }

        for ip in [1_u128, 2, 3<<125] {
            let result = ipv6_seacher.search(ip).unwrap();
            println!("CachePolicy: {cache_policy:?}, IP: {ip}, Result: {result}");
        }
    }
}
```

## Cache policy benchmark

```bash
$ git lfs pull
$ cd binding/rust/ip2region
$ cargo test
$ cargo bench

// --snip---
ipv4_no_memory_bench    time:   [54.699 µs 57.401 µs 61.062 µs]
Found 16 outliers among 100 measurements (16.00%)
  10 (10.00%) high mild
  6 (6.00%) high severe

ipv4_vector_index_cache_bench
                        time:   [25.972 µs 26.151 µs 26.360 µs]
Found 9 outliers among 100 measurements (9.00%)
  1 (1.00%) low severe
  6 (6.00%) high mild
  2 (2.00%) high severe

ipv4_full_memory_cache_bench
                        time:   [132.04 ns 139.48 ns 149.20 ns]
Found 10 outliers among 100 measurements (10.00%)
  4 (4.00%) high mild
  6 (6.00%) high severe

ipv6_no_memory_bench    time:   [121.00 µs 122.14 µs 123.40 µs]
Found 5 outliers among 100 measurements (5.00%)
  2 (2.00%) high mild
  3 (3.00%) high severe

ipv6_vector_index_cache_bench
                        time:   [96.830 µs 100.23 µs 104.81 µs]
Found 8 outliers among 100 measurements (8.00%)
  2 (2.00%) high mild
  6 (6.00%) high severe

ipv6_full_memory_cache_bench
                        time:   [175.29 ns 178.82 ns 183.77 ns]
Found 6 outliers among 100 measurements (6.00%)
  2 (2.00%) high mild
  4 (4.00%) high severe
// --snip--
```

## 测试与结果验证，benchmark
```bash
$ git lfs pull
$ cd binding/rust/example
$ cargo build -r
```
构建的执行程序位置 `binding/rust/target/release/searcher`

测试 IPv6 以及 IPv4 需要结合 ipv6_source.txt 以及 ipv4_source.txt 的内容进行查询结果校验

**此处展示的查询结果只表示当前时间数据的查询，后续查询结果可能会由于 ip_source.txt 以及 xdb 二进制数据的 IP region 段更新修正导致不同**

#### 测试 IPv6

```bash
$ cd binding/rust
$ cargo build -r
$ ./target/release/searcher --xdb='../../data/ip2region_v6.xdb' query
ip2region xdb searcher test program, type `quit` or `Ctrl + c` to exit
ip2region>> ::
region: Ok(""), took: 79.651412ms
ip2region>> 240e:3b7:3273:51d0:cd38:8ae1:e3c0:b708
region: Ok("中国|广东省|深圳市|电信|CN"), took: 7.575µs
ip2region>> 2001::
region: Ok("0|0|Reserved|Reserved|Reserved"), took: 7.256µs
ip2region>> 2001:268:9a02:8888::
region: Ok("Japan|Aichi|Nagoya|KDDI CORPORATION|JP"), took: 7.921µs
ip2region>> 2a02:26f7:b408:a6c2::
region: Ok("United States|Virginia|Emporia|Akamai Technologies, Inc.|US"), took: 8.461µs
ip2region>> 2c99::
region: Ok("0|0|Reserved|Reserved|Reserved"), took: 5.33µs
```

#### 测试 IPv4
```bash
$ cd binding/rust
$ cargo build -r
$  ./target/release/searcher --xdb='../../data/ip2region_v4.xdb' query
ip2region xdb searcher test program, type `quit` or `Ctrl + c` to exit
ip2region>> 1.2.3.4
region: Ok("Australia|Queensland|Brisbane|0|AU"), took: 6.07µs
ip2region>> 1.1.2.1
region: Ok("中国|福建省|福州市|0|CN"), took: 5.653µs
ip2region>> 2.2.21.1
region: Ok("United States|Texas|0|Oracle Svenska AB|US"), took: 4.556µs
```

#### Benchmark 与验证结果

通过 searcher 程序来测试性能，同时依据 ip sources 文件对比查询结果，检测是否存在错误

```bash
$ cd binding/rust/example
$ cargo build -r
## 通过 data/ip2region_v4.xdb 和 data/ipv4_source.txt 进行 ipv4 的 bench 测试：
$ RUST_LOG=debug ../target/release/searcher --xdb='../../../data/ip2region_v4.xdb' bench '../../../data/ipv4_source.txt'
2025-09-24T07:02:07.840535Z DEBUG ip2region::searcher: Load xdb file with header header=Header { version: 3, index_policy: VectorIndex, create_time: 1757125456, start_index_ptr: 955933, end_index_ptr: 11042415, ip_version: V4, runtime_ptr_bytes: 4 }
2025-09-24T07:02:07.840894Z DEBUG ip2region::searcher: Load vector index cache
2025-09-24T07:02:07.840905Z DEBUG ip2region::searcher: Load full cache filepath="../../../data/ip2region_v4.xdb"
2025-09-24T07:02:08.409990Z  INFO searcher: Benchmark finished count=3404406 took=569.388667ms avg_took=167ns

## 通过 data/ip2region_v6.xdb 和 data/ipv6_source.txt 进行 ipv6 的 bench 测试：
$ RUST_LOG=debug ../target/release/searcher --xdb='../../../data/ip2region_v6.xdb' bench '../../../data/ipv6_source.txt'
2025-09-24T07:01:48.991835Z DEBUG ip2region::searcher: Load xdb file with header header=Header { version: 3, index_policy: VectorIndex, create_time: 1756970508, start_index_ptr: 6585371, end_index_ptr: 647078145, ip_version: V6, runtime_ptr_bytes: 4 }
2025-09-24T07:01:48.992557Z DEBUG ip2region::searcher: Load vector index cache
2025-09-24T07:01:48.992563Z DEBUG ip2region::searcher: Load full cache filepath="../../../data/ip2region_v6.xdb"
2025-09-24T07:01:59.775879Z  INFO searcher: Benchmark finished count=38335905 took=10.784124584s avg_took=281ns

```
