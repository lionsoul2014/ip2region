# ip2region xdb golang 生成实现


# 程序编译

通过如下方式编译得到 xdb_maker 可执行程序:
```
# 切换到golang maker 根目录
make
```
编译成功后会在当前目录生成一个 xdb_maker 的可执行文件


# `xdb` 数据生成

通过 `xdb_maker gen` 命令生成 ip2region.xdb 二进制文件:
```
./xdb_maker gen [command options]
options:
 --src string           source ip text file path
 --dst string           destination binary xdb file path
 --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused 
 --field-list string    field index list imploded with ',' eg: 0,1,2,3-6,7
 --log-level string     set the log level, options: debug/info/warn/error
```

例如，使用默认的仓库 data/ 下默认的原始数据生成生成 xdb 文件到当前目录：
```bash
# ipv4 
./xdb_maker gen --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb --version=ipv4
# ipv6
./xdb_maker gen --src=../../data/ipv6_source.txt --dst=./ip2region_v6.xdb --version=ipv6
```

生成过程中数据字段自定义请参考 [xdb-v4文件生成#自定义数据字段](https://ip2region.net/doc/data/ipv4_xdb_make#field-list)


# `xdb` 数据查询

通过 `xdb_maker search` 命令来测试查询输入的 ip：
```
➜  golang git:(v2.0_xdb) ✗ ./xdb_maker search
./xdb_maker search [command options]
options:
 --db string    ip2region binary xdb file path
```

例如，使用自带的 xdb 文件来运行查询测试：
```bash
# ipv4
./xdb_maker search --db=../../data/ip2region_v4.xdb
ip2region xdb search test program,
source xdb: ../../data/ip2region_v4.xdb (IPv4)
commands:
  loadIndex : load the vector index for search speedup.
  clearIndex: clear the vector index.
  quit      : exit the test program
ip2region>> 58.251.30.115
{region:中国|广东省|深圳市|联通, iocount:3, took:37.043µs}
ip2region>> 

# ipv6
./xdb_maker search --db=../../data/ip2region_v6.xdb
ip2region xdb search test program,
source xdb: ../../data/ip2region_v6.xdb (IPv6)
commands:
  loadIndex : load the vector index for search speedup.
  clearIndex: clear the vector index.
  quit      : exit the test program
ip2region>> 2604:bc80:8001:11a4:ffff:ffff:ffff:ffff
{region:中国|广东省|深圳市|数据中心, iocount:14, took:138.68µs}
ip2region>>
```

# `xdb` 数据编辑

通过 `xdb_maker edit` 命令来编辑原始的　IP 数据：
```
./xdb_maker edit [command options]
options:
 --src string        source ip text file path
 --version string    IP version, options: ipv4/ipv6, specify this flag so you don't get confused
```

例如，使用编辑器打开 `./data/ipv4_source.txt` 会看到如下的操作面板：
```bash
./xdb_maker edit --src=../../data/ipv4_source.txt --version=ipv4
init the editor from source @ `../../data/ipv4_source.txt` ... 
all segments loaded, length: 683591, elapsed: 479.73743ms
command list: 
  put [segment]        : put the specifield $segment
  put_file [file]      : put all the segments from the specified $file
  list [offset] [size] : list the first $size segments start from $offset
  save                 : save all the changes to the destination source file
  quit                 : exit the program
  help                 : print this help menu
editor>>
```

通过 `put` 命令修改指定 IP 段的定位信息，例如：
```bash
editor>> put 36.132.128.0|36.132.147.255|中国|黑龙江省|哈尔滨市|移动
Put(36.132.128.0|36.132.147.255|中国|黑龙江省|哈尔滨市|移动): Ok, with 1 deletes and 2 additions
*editor>> 
```

通过 `put_file` 命令从文件中批量载入修改，文件中的 IP 段不需要像　./data/ip.merge.txt 中的数据那么严格，不需要前后连续，不同 IP　段有重叠也没关系，编辑器会自动分析处理，例如：
```bash
*editor>> put_file ../../data/ip.test.txt
PutFile(../../data/ip.test.txt): Ok, with 25 deletes and 25 additions
*editor>> 
```

通过 `save` 命令保存修改，保存成功后，再通过上面的命令从修改后的原始 IP 文件重新生成 xdb 即可：
```bash
*editor>> save
all segments saved to ../../data/ip.merge.txt
editor>> 
```

# bench 测试

如果你自主生成了 `xdb` 文件，请确保运行如下的 `xdb_maker bench` 命令来确保生成的的 `xdb` 文件的正确性：
```
./xdb_maker bench [command options]
options:
 --db string            ip2region binary xdb file path
 --src string           source ip text file path
 --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused 
 --log-level string     set the log level, options: debug/info/warn/error
 --ignore-error bool    keep going if bench failed
```

例如：使用 data 下的源文件来 bench 测试 data 的 xdb 文件：
```bash
# ipv4
./xdb_maker bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt --version=ipv4

#ipv6
./xdb_maker bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt --version=ipv6
```
*请注意 bench 测试使用的 `src` 文件需要是对应的生成 xdb 的源文件相同*。
如果运行过程中有错误会立马停止运行，也可以执行 --ignore-error=true 参数来忽略错误，在最后看 failed 的统计结果。
