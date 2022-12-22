# `ip2region xdb rust` 查询客户端实现

# 使用方式

配置`Cargo.toml`如下

```toml
[dependencies]
search = { git = "https://github.com/lionsoul2014/ip2region.git", branch = "master" }
# 如果要在异步环境下使用，需要加上如下依赖
tokio = { version = "1", features = ["full"]}
```

程序启动的时候是没加载文件，这个程序占用内存`1M`左右

一旦开始执行查询，`ip2region.xdb`文件会直接加载到内存，程序占用内存`12M`左右

预先加载整个` ip2region.xdb` 到内存，完全基于内存查询，该方式线程安全，采用`once_cell::sync::OnceCell`，只会加载一次数据，多线程安全，可以自由使用`tokio`异步运行时或者标准库的多线程`std::thread`

### 缓存整个 `xdb` 数据

编写`main.rs`

**需要使用`XDB_FILEPATH`指定`ip2region.xdb`文件的路径**，该参数可以使用相对路径或者绝对路径，如果使用相对路径报错，请修改为绝对路径

```rust
use std::env;

#[tokio::main]
async fn main() {
    env::set_var(
        "XDB_FILEPATH",
        "../data/ip2region.xdb",
    );
    //可以调用如下直接加载文件
    // search::global_searcher();
    
    
    // search_by_ip的参数可以是u32类型，字符串IP类型，字符串数字类型
    for i in 1..5 {
        tokio::spawn(async move {
            // u32
            println!("{:?}", search::search_by_ip(i));
        });
    }
    // ip str
    println!("{:?}", search::search_by_ip("1.0.1.0"));

    // u32 str
    let ip_u32 = 1 << 24 | 1 << 8;
    println!("{:?}", search::search_by_ip(ip_u32.to_string().as_str()));
}
```

进行测试

```shell
$ cargo run
init xdb searcher at ../data/ip2region.xdb
Ok("中国|0|福建省|福州市|电信")
Ok("中国|0|福建省|福州市|电信")
Ok("0|0|0|内网IP|内网IP")
Ok("0|0|0|内网IP|内网IP")
Ok("0|0|0|内网IP|内网IP")
Ok("0|0|0|内网IP|内网IP")
```

# 编译程序

通过如下方式编译得到 `ip2region` 可执行程序

切换到 `rust binding` 根目录，执行如下命令

```bash
➜ cargo build -r
```

生成的二进制文件会在`./target/release/ip2region`位置

# 查询测试

通过 `./target/release/ip2region` 命令来测试查询
```
➜ ./target/release/ip2region --help
you can set environment XDB_FILEPATH=../data/ip2region or just use --xdb in command
Usage: ip2region [OPTIONS]
Options:
      --xdb <xdb>
          the xdb filepath, you can set this field like ../data/ip2region.xdb
  -h, --help
          Print help information (use `-h` for a summary)
  -V, --version
          Print version information
```

命令行指定参数进行查询测试，输入 `ip` 地址或者一个`u32`类型的数字进行查询即可，输入 `quit` 退出测试程序
```bash
➜ ./target/release/ip2region --xdb=../../data/ip2region.xdb
init xdb searcher at ../../data/ip2region.xdb
ip2region xdb searcher test program, type `quit` to exit
ip2region>> 1.1.1.1
region: Ok("澳大利亚|0|0|0|0"), took: 4.227µs
ip2region>> 2.2.2.2
region: Ok("法国|0|0|0|橘子电信"), took: 4.495µs
ip2region>> 222222222
region: Ok("美国|0|康涅狄格|0|0"), took: 4.048µs
```

或者使用环境变量

```shell
➜ XDB_FILEPATH=../../data/ip2region.xdb ./target/release/ip2region
init xdb searcher at ../../data/ip2region.xdb
ip2region xdb searcher test program, type `quit` to exit
ip2region>> 2.2.2.2
region: Ok("法国|0|0|0|橘子电信"), took: 4.458µs
ip2region>> 4.4.4.5
region: Ok("美国|0|0|0|Level3"), took: 4.847µs
```

# 单元测试

```shell
➜ XDB_FILEPATH=../../../data/ip2region.xdb cargo test
```

# `bench` 测试

通过 `cargo bench` 命令来进行自动 `bench` 测试，一方面确保程序和 `xdb` 文件都没有错误，另一方面通过大量的查询得到平均查询性能

在不同机器上面的测试性能时间是不一样的，如下是在机器`CPU`是`Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz`，内存`DDR4 32G`下面的测试结果 

```shell
➜ XDB_FILEPATH=../../../data/ip2region.xdb cargo bench 
// --snip--
     Running benches/search.rs (target/release/deps/search-9614305a566885c4)
Benchmarking ip_search_bench: Warming up for 3.0000 sinit xdb searcher at ../../../data/ip2region.xdb
ip_search_bench         time:   [120.84 ns 122.91 ns 125.20 ns]
                        change: [-3.3346% -1.2786% +0.8027%] (p = 0.23 > 0.05)
                        No change in performance detected.
Found 6 outliers among 100 measurements (6.00%)
  6 (6.00%) high mild
```

可以看到上面的`ip_search_bench time`一行的参数表示是左右值分别显示置信区间的下限和上限，中间值显示 `Criterion.rs` 对基准程序每次迭代所用时间的最佳估计
