# ip2region xdb python 生成实现


# 脚本执行

```
# 切换到python maker 根目录
> python main.py
ip2region xdb maker
main.py [command] [command options]
Command:
  gen      generate the binary db file
```

# `xdb` 数据生成

通过 `python main.py gen` 命令生成 ip2region.xdb 二进制文件:
```
➜  python git:(v2.0_xdb) ✗ python main.py gen
main.py gen [command options]
options:
 --src string    source ip text file path
 --dst string    destination binary xdb file path
```

例如，使用默认的 data/ip.merge.txt 作为源数据，生成一个 ip2region.xdb 到当前目录：
```
➜  python git:(v2.0_xdb) ✗ python main.py gen --src=../../data/ip.merge.txt --dst=./ip2region.xdb
# 会看到一堆输出，最终会看到类似如下输出表示运行结束
...
2022-07-13 19:58:00,540-root-238-INFO - write done, dataBlocks: 13804, indexBlocks: (683591, 720221), indexPtr: (982904, 11065984)
2022-07-13 19:58:00,540-root-63-INFO - Done, elapsed: 3m3s
```


# `xdb` 数据查询 和 bench 测试

基于xdb 格式的查询功能和测试见 [ip2region binding](../../binding)
