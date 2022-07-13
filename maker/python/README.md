# ip2region xdb python 生成实现


# 脚本执行

```
# 切换到python maker 根目录
> python main.py
ip2region xdb maker
main.py [command] [command options]
Command:
  gen      generate the binary db file
  search   binary xdb search test
  bench    binary xdb bench test
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


# `xdb` 数据查询

通过 `python main.py search` 命令来测试查询输入的 ip：
```
➜  python git:(v2.0_xdb) ✗ python main.py search
main.py search [command options]
options:
 --db string    ip2region binary xdb file path
```

例如，使用自带的 xdb 文件来运行查询测试：
```
➜  python git:(v2.0_xdb) ✗ python main.py search --db=./ip2region.xdb
ip2region xdb search test program, commands:
loadIndex : load the vector index for search speedup.
clearIndex: clear the vector index.
quit      : exit the test program
ip2region>> 117.148.181.111
[region:中国|0|浙江省|嘉兴市|移动, took:0s]
ip2region>> 120.196.20.28
[region:中国|0|广东省|茂名市|移动, took:0s]
ip2region>> 81.33.22.150
[region:西班牙|0|马德里|马德里|西班牙电信, took:0s]
ip2region>> 
```

# bench 测试

如果你自主生成了 `xdb` 文件，请确保运行如下的 `python main.py bench` 命令来确保生成的的 `xdb` 文件的正确性：
```
➜  python git:(v2.0_xdb) ✗ python main.py bench
main.py bench [command options]
options:
 --db string            ip2region binary xdb file path
 --src string           source ip text file path
 --ignore-error bool    keep going if bench failed
```

例如：使用 data/ip.merge.txt 源文件来 bench 测试 data/ip2region.xdb 这个 xdb 文件：
```
➜  python git:(v2.0_xdb) ✗ python main.py bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt
# 会看到一堆输出，看到类似如下的数据表示 bench 测试通过了，否则就会报错
...
try to bench segment: `{}` 224.0.0.0|255.255.255.255|0|0|0|内网IP|内网IP
|-try to bench ip '224.0.0.0' ... --[Ok]
|-try to bench ip '231.255.255.255' ... --[Ok]
|-try to bench ip '239.255.255.255' ... --[Ok]
|-try to bench ip '247.255.255.255' ... --[Ok]
|-try to bench ip '255.255.255.255' ... --[Ok]
Bench finished, [count: 3417955, failed: 0, took: 88.061s]
```
*请注意 bench 测试使用的 `src` 文件需要是对应的生成 ip2region.xdb 的源文件相同*。
如果运行过程中有错误会立马停止运行，也可以执行 --ignore-error=true 参数来忽略错误，在最后看 failed 的统计结果。
