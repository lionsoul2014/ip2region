## Rust 客户端

## 用法

Demo： 在 `example` 目录里

API文档： `cargo doc --features lazy --open` 可以看到所有。 

运行测试： `cargo test --features lazy` 

### 添加依赖

注意 `#` 是 toml格式的注释，文件前三行的 `path` 和 `git` 是引用包的方式，`path` 是按路径引用包，`git`  是按git项目地址，按实际情况选一个。

```toml
[dependencies.ip2region]
git = "https://github.com/lionsoul2014/ip2region"
# git = "https://github.com/biluohc/ip2region"
# path = "../"
version = "*"
# features = ["lazy"]
```

### 代码
查看 `example/src/main.rs` 

### `lazy` feature 把 DB 直接打包进二进制

取消上面 toml 的 `# features = ["lazy"]` 行的注释即可使用，其 api 是 `memory_search` 和  `memory_search_ip`。

只是目前 DB 足有3.2M，还是有些感人的。

关键的一行是 `features = ["lazy"]` ，不需要则可以注释或者删掉。



