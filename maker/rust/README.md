# ip2region xdb rust 生成实现

## 程序编译
```bash
$ cd maker/rust/maker
$ cargo build -r
```
编译成功以后，执行文件位置 `./target/release/maker`

## `xdb` 数据生成
```bash
# CWD ip2region/maker/rust/maker
$ ./target/release/maker --help
Usage: maker [OPTIONS] --src <SRC> --dst <DST> --ip-version <IP_VERSION>

Options:
      --src <SRC>
          ip source region txt filepath

      --dst <DST>
          generated xdb filepath

      --ip-version <IP_VERSION>
          Possible values:
          - v4: IPv4
          - v6: Ipv6

      --index-policy <INDEX_POLICY>
          index cache policy

          [default: vector-index]
          [possible values: vector-index, b-tree-index]

      --filter-fields <FILTER_FIELDS>
          region filter fields, the index of the fields, e.g. `1,2,3,5`

  -h, --help
          Print help (see a summary with '-h')
```

例如，使用默认的仓库 data/ 下默认的原始数据生成生成 xdb 文件到当前目录(`ip2region/maker/rust/maker`)：
```bash
# ipv6
./target/release/maker --src=../../../data/ipv6_source.txt --dst=./target/ipv6.xdb --ip-version v6
# ipv4
./target/release/maker --src=../../../data/ipv4_source.txt --dst=./target/ipv4.xdb --ip-version v4
```

## `xdb` 数据查询 和 bench 测试

基于xdb 格式的查询功能和测试见 [ip2region binding](../../binding)

## 对比其他 maker 生成的 xdb 文件 
推荐使用 `vbindiff`, 与其他文件的差异只有 create time 信息上有差异，其他数据都需要是一样的

golang 版本 maker 构建 xdb
```bash
$ cd maker golang
$ make
$ ./xdb_maker gen --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb --version=ipv4
$ ./xdb_maker gen --src=../../data/ipv6_source.txt --dst=./ip2region_v6.xdb --version=ipv6
```

对比 xdb 差异
```bash
$ cd maker/rust/maker
$ ./target/release/maker --src=../../../data/ipv4_source.txt --dst=./target/ipv4.xdb --ip-version v4
$ ./target/release/maker --src=../../../data/ipv6_source.txt --dst=./target/ipv6.xdb --ip-version v6
$ vbindiff ./ipv4.xdb ../../golang/ip2region_v4.xdb
$ vbindiff ./ipv6.xdb ../../golang/ip2region_v6.xdb
```
