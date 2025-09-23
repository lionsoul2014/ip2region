# `ip2region xdb rust` 查询客户端实现

# 实现效果

得益于`xdb`数据存储格式设计以及`rust`编译器的高度代码优化

- 实现单核`CPU`下接近每秒千万级别的查询，如果是4核8线这样的`CPU`，采用`tokio`异步运行时，可以达到接近每秒4千万查询速度，查询速度取决于`CPU`物理核睿频频率
- 达到查询稳定在`100-150ns/op`
- `ip2region.xdb`文件会直接加载到内存，整个程序运行时候占用内存`13M`左右，即使是多线程或者异步运行时下面的高并发查询也是稳定在这个内存大小
- 只会加载一次数据，多线程安全，可以自由使用`tokio`异步运行时或者标准库的多线程`std::thread`

# 缓存方式说明

由于基于文件的查询以及缓存`VectorIndex`索引在并发较高(比如每秒上百并发)的情况下，每次查询都会从磁盘加载`ip2region.xdb`文件进入内存，由此会产生很高的磁盘`IO`以及极大的内存占用，所以决定做一次减法，不对这两种缓存进行开发，只提供缓存整个`xdb`文件的方式，以此实现最小的并发查询内存开销以及极限`CPU`性能压榨

# 使用方式

使用`cargo`新建一个项目，比如`cargo new ip-test`

同时把`ip2region.xdb`文件也移动到该项目根路径下，或者不移动，下面示例编译的时候注意调整`xdb_filepath`的参数值

配置`Cargo.toml`的`[dependencies]`如下

```toml
[dependencies]
xdb = { git = "https://github.com/lionsoul2014/ip2region.git", branch = "master" }
# 用于生成随机数
rand = "0.8"
# 用于初始化日志打印
tracing = "0.1"
tracing-subscriber = "0.3.14"
# 异步运行时
tokio = { version = "1", features = ["full"]}
```

### 基本使用示例

编写`main.rs`

```rust
use std::net::Ipv4Addr;
use std::thread;
use std::time::{Duration, Instant};

use xdb::{search_by_ip, searcher_init};

fn main() {
    // 配置输出日志信息
    tracing_subscriber::fmt::init();

    // 初始化加载xdb文件
    let xdb_filepath = "./ip2region.xdb";
    searcher_init(Some(xdb_filepath.to_owned()));
    // 如果../data或者../../data或者../../../data下面有对应的ip2region.xdb文件
    // 初始化函数可以直接调用如下
    // searcher_init(None);

    println!("\n测试多类型查询");
    println!("{}", search_by_ip("9999999").unwrap());
    println!("{}", search_by_ip("1.0.1.0").unwrap());
    println!("{}", search_by_ip(9999999).unwrap());
    println!("{:?}", search_by_ip(Ipv4Addr::from(3333333)));

    println!("\n测试多线程初始化以及多线程查询");
    for i in 1..5 {
        thread::spawn(move || {
            // 再次初始化是没什么效果的
            searcher_init(Some(xdb_filepath.to_owned()));
            println!("in thread {i} {:?}", search_by_ip(rand::random::<u32>()));
        });
    }
    // 等待多线程执行结束
    thread::sleep(Duration::from_secs(1));

    let count = 10_000_000;
    print!("\n计算千万数据总共耗时: ");
    let now = Instant::now();
    for _ in 0..count {
        search_by_ip(rand::random::<u32>()).unwrap();
    }
    println!("{:?}, ave: {:?}", now.elapsed(), now.elapsed()/count);

    print!("\n计算千万数据，每次迭代都统计耗时的总共耗时: ");
    let mut total = Duration::from_secs(0);
    for _ in 0..count {
        let current = Instant::now();
        search_by_ip(rand::random::<u32>()).unwrap();
        total += current.elapsed();
    }
    println!("{:?}, ave: {:?}", total, total/count);

    print!("\n计算空迭代耗时: ");
    let now = Instant::now();
    for _ in 0..count {}
    println!("{:?}", now.elapsed());
}
```

进行测试如下，需要指定`RUST_LOG`参数打印日志

```shell
➜ RUST_LOG=debug cargo run -r
   Compiling ip-test v0.1.0 (/home/gong/rust-work/ip-test)
    Finished release [optimized] target(s) in 0.27s
     Running `target/release/ip-test`
2022-12-24T03:31:22.921958Z DEBUG xdb::searcher: load xdb searcher file at ./ip2region.xdb 

测试多类型查询
0|0|0|内网IP|内网IP
中国|0|福建省|福州市|电信
0|0|0|内网IP|内网IP
Ok("0|0|0|内网IP|内网IP")

测试多线程初始化以及多线程查询
in thread 1 Ok("美国|0|新墨西哥|0|康卡斯特")
in thread 3 Ok("0|0|0|内网IP|内网IP")
in thread 2 Ok("土耳其|0|Ankara|0|0")
in thread 4 Ok("爱尔兰|0|Cork|0|0")

计算千万数据总共耗时: 1.176241972s, ave: 117ns

计算千万数据，每次迭代都统计耗时的总共耗时: 1.407956755s, ave: 140ns

计算空迭代耗时: 40ns
```

### `tokio`使用示例

```rust
use std::time::Instant;

use tokio::sync::mpsc;

use xdb::{search_by_ip, searcher_init};

#[tokio::main]
async fn main() {
    // 配置输出debug 信息
    tracing_subscriber::fmt::init();
    searcher_init(Some("./ip2region.xdb".to_owned()));
    let main_now = Instant::now();
    let (tx, mut rx) = mpsc::channel(10);
    for i in 0..6 {
        let tx = tx.clone();
        tokio::spawn(async move {
            let now = Instant::now();
            let count = 10_000_000;
            for _ in 0..count {
                search_by_ip(rand::random::<u32>()).unwrap();
            }
            let message = format!(
                "tokio spawn {i} over cost: {:?}, ave: {:?}",
                now.elapsed(),
                now.elapsed() / count
            );
            tx.send(message).await.unwrap();
        });
    }
    drop(tx);
    while let Some(message) = rx.recv().await {
        println!("{}", message);
    }
    println!("总共耗时: {:?}", main_now.elapsed());
}
```

开始执行测试

```shell
$ RUST_LOG=debug cargo run -r
   Compiling ip-test v0.1.0 (/home/gong/rust-work/ip-test)
    Finished release [optimized] target(s) in 0.51s
     Running `target/release/ip-test`
2022-12-24T04:05:32.876664Z DEBUG xdb::searcher: load xdb searcher file at ./ip2region.xdb 
tokio spawn 4 over cost: 1.133448675s, ave: 113ns
tokio spawn 2 over cost: 1.133938619s, ave: 113ns
tokio spawn 1 over cost: 1.136872027s, ave: 113ns
tokio spawn 5 over cost: 1.173464286s, ave: 117ns
tokio spawn 0 over cost: 1.197527014s, ave: 119ns
tokio spawn 3 over cost: 1.26446099s, ave: 126ns
总共耗时: 1.264631935s
```

# `binding/rust`路径下面的结构说明

`xdb`

- 封装了`ip`到`region`的函数
- 里面包含单元测试和`benchmark`测试

`example`

- 包含了命令行可执行文件生成的源码程序
- 作为一个用于`rust`的开发集成例子

开始编译之后会生成如下

`target`

- 文件夹存放编译之后的文件以及编译产生的临时文件与缓存

`Cargo.lock`

- 固定`rust`第三方库的版本

编译生成的文件全部在`.gitignore`中有标识，不会被提交

# 编译程序

切换到 `ip2region/binding/rust` 路径，执行如下命令

```bash
$ cargo build -r
```

生成的二进制文件会在`./target/release/rust-example`位置

# 查询测试

切换到 `ip2region/binding/rust` 路径，执行如下命令

`help`输出如下

```shell
$ ./target/release/rust-example query --help
query test

Usage: rust-example query [OPTIONS]

Options:
      --db <db>  the xdb filepath, you can set this field like ../data/ip2region.xdb,if you dont set,if will detect xdb file on ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb if exists
  -h, --help     Print help information
```

执行测试，使用默认`data/ip2region.txt`

```shell
$ ./target/release/rust-example query --db=../../data/ip2region.xdb
ip2region xdb searcher test program, type `quit` or `Ctrl + c` to exit
ip2region>> 123123123
region: Ok("美国|0|0|0|0"), took: 4.94µs
ip2region>> 1.1.1.1
region: Ok("澳大利亚|0|0|0|0"), took: 2.057µs
ip2region>> 2.2.2.2
region: Ok("法国|0|0|0|橘子电信"), took: 4.294µs
ip2region>> 
```

这边发现每次查询的消耗时间都超过`1µs`，和开头所说的纳秒级查询不一致啊，这个是由于`rust`的标准库封装的`use std::time::Instant`对象是调用系统底层函数实现的，在打印过程中会存在时间误差

可以试着找一个新的项目

在`main.rs`写入如下

```rust
use std::thread;
use std::time::{Instant, Duration};

fn main() {
    let now = Instant::now();
    thread::sleep(Duration::from_secs(3));
    println!("{:?}", now.elapsed());
}
```

执行命令如下，发现毫秒级别是没什么问题，微秒和纳秒上面是存在误差的，详情可以参考[`rust`标准库文档的`time::Instant`章](https://rustwiki.org/zh-CN/std/time/struct.Instant.html)

```shell
$ cargo run -r
    Finished release [optimized] target(s) in 0.03s
     Running `target/release/ip-test`
3.000197389s
```

# `bench`测试

测试平均性能

切换到 `ip2region/binding/rust` 路径，执行如下命令

`help`输出如下

```shell
$ ./target/release/rust-example bench --help                       
bench test

Usage: rust-example bench [OPTIONS] --src <src>

Options:
      --src <src>  set this to specific source bench file
      --db <db>    the xdb filepath, you can set this field like ../data/ip2region.xdb,if you dont set,if will detect xdb file on ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb if exists
  -h, --help       Print help information
```

使用默认的`ip2region`和`ip.merge.txt`

```shell
$ ./target/release/rust-example bench --src=../../data/ip.merge.txt --db=../../data/ip2region.xdb
Bench finished, total: 3419220,took: 519.820535ms ,cost: 152ns/op
```

# `binding/rust`后续维护须知

`bingd/rust`编写了单元测试，后续开发需要保证单元测试正常

切换到 `ip2region/binding/rust` 路径，执行如下命令

```shell
$ cargo test
```

需要保证查询速度不会有大幅降低，希望有朝一日，远方的朋友可以再深度优化一下，实现几十纳秒级别的查询速度

下面是`rust/xdb`库的第一版`benchmark`结果

重点关注如下

`search_by_ip_bench  `

- 查询`ip`的实际调用函数`search_by_ip`

`get_block_by_size_bench`

- 获取并且计算偏移值，对应函数`get_block_by_size_bench`，和其他`binding`下的实现的`getLong`和`getShort`相似
- 该函数会被`search_by_ip`多次调用，所以被标注为`#[inline]`使用内联优化，以此来消除函数调用产生的压栈开销

```shell
$ cargo bench 
// --snip---
search_by_ip_bench      time:   [116.99 ns 119.52 ns 122.31 ns]
                        change: [-8.1930% -5.8295% -3.3029%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 4 outliers among 100 measurements (4.00%)
  4 (4.00%) high mild

get_block_by_size_bench time:   [5.2388 ns 5.2784 ns 5.3229 ns]
                        change: [-6.2649% -4.3559% -2.5539%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 7 outliers among 100 measurements (7.00%)
  3 (3.00%) high mild
  4 (4.00%) high severe

get_full_cache_bench    time:   [1.4800 ns 1.5034 ns 1.5325 ns]
                        change: [-17.984% -13.664% -9.1681%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 15 outliers among 100 measurements (15.00%)
  3 (3.00%) high mild
  12 (12.00%) high severe

get_vec_index_cache_bench
                        time:   [1.7578 ns 1.7757 ns 1.7961 ns]
                        change: [-7.5169% -4.5088% -1.7147%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 5 outliers among 100 measurements (5.00%)
  3 (3.00%) high mild
  2 (2.00%) high severe
// --snip--
```

