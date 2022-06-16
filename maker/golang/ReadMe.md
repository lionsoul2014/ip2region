# ip2region xdb golang 生成实现

# 程序编译
通过如下方式编译得到 dbmaker 可执行程序:
```
# 切换到golang maker 根目录
cd ./
go build
```
编译成功后会在当前目录生成一个 dbmaker 的可执行文件

# 数据生成

通过 `dbmaker gen` 命令生成 ip2region.xdb 二进制文件:
```bash
➜  golang git:(v2.0_xdb) ✗ ./dbmaker gen
dbmaker gen [command options]
options:
 --src string    source ip text file path
 --dst string    destination binary db file path
```

例如，使用默认的 data/ip.merge.txt 作为源数据，生成一个 ip2region.xdb 到当前目录：
```
./dbmaker gen --src=../../data/ip.merge.txt --dst=./ip2region.xdb
# 会看到一堆输出，最终会看到类似如下输出表示运行结束
...
2022/06/16 16:38:48 maker.go:317: write done, with 13804 data blocks and (683591, 720221) index blocks
2022/06/16 16:38:48 main.go:89: Done, elapsed: 33.615278847s
```

# 数据查询

# bench 测试
