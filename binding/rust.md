## Rust 客户端

## 用法

基本都在 `src/main.rs` 的 `overview` 函数里,

 另外 `cargo doc --features lazy`可以看到所有`API`。

## `lazy` feature 直接把DB打包到二进制

复制下面的 toml 添加依赖即可使用，其 api 是 `memory_search`。

只是目前 DB足有3.2M，还是有些感人的。

```toml
[dependencies.ip2region]
git = "https://github.com/lionsoul2014/ip2region"
# git = "https://github.com/biluohc/ip2region"
version = "*"
features = ["lazy"]
```


