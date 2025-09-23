## `ip2region xdb rust` 查询客户端实现

## Features
- 支持`ip`字符串和`u32`/`u28` 数字两种类型的查询
- 支持 IPv4 和 IPv6
- 支持无缓存，Vector 索引缓存，全部数据缓存三种模式

## 缓存策略对比
| 缓存模式         | IPv4 数据内存占用 | IPv6 数据内存占用 | IPv4 benchmark 查询耗时 | IPv6 benchmark 查询耗时 |
| ------------ | ----------- | ----------- | ------------------- | ------------------- |
| 无缓存          | 1-2MB       | 1-2MB       | 54 us               | 47us                |
| vector index | 1-2MB       | 1-2MB       | 27 us               | 19us                |
| 全部缓存         | 20 MB       | 200 MB      | 120 ns              | 638 ns              |


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

## Benchmark 测试

```bash
$ git lfs pull
$ cd binding/rust/ip2region
$ cargo test
$ cargo bench

// --snip---
ipv4_no_memory_bench    time:   [53.020 µs 54.810 µs 57.837 µs]
Found 14 outliers among 100 measurements (14.00%)
  1 (1.00%) low mild
  3 (3.00%) high mild
  10 (10.00%) high severe

ipv4_vector_index_cache_bench
                        time:   [26.411 µs 27.070 µs 28.078 µs]
Found 8 outliers among 100 measurements (8.00%)
  1 (1.00%) low mild
  2 (2.00%) high mild
  5 (5.00%) high severe

ipv4_full_memory_cache_bench
                        time:   [124.26 ns 126.01 ns 128.21 ns]
Found 8 outliers among 100 measurements (8.00%)
  5 (5.00%) high mild
  3 (3.00%) high severe

ipv6_no_memory_bench    time:   [46.541 µs 47.365 µs 48.518 µs]
Found 9 outliers among 100 measurements (9.00%)
  4 (4.00%) high mild
  5 (5.00%) high severe

ipv6_vector_index_cache_bench
                        time:   [19.596 µs 19.777 µs 19.967 µs]
Found 3 outliers among 100 measurements (3.00%)
  3 (3.00%) high mild

ipv6_full_memory_cache_bench
                        time:   [603.73 ns 638.19 ns 683.22 ns]
Found 12 outliers among 100 measurements (12.00%)
  4 (4.00%) high mild
  8 (8.00%) high severe
// --snip--
```
