# ip2region xdb c 查询客户端实现

# 使用方式

### 完全基于文件的查询

### 缓存 `VectorIndex` 索引

### 缓存整个 `xdb` 数据


# 测试程序编译

通过如下方式编译得到 xdb_searcher 可执行程序：
```bash
# cd 到 c binding 根目录
make
```

# 查询测试

通过 `xdb_searcher search` 命令来测试对 ip2region.xdb 的查询：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher search
./xdb_searcher search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher search --db=../../data/ip2region.xdb --cache-policy=vectorIndex
ip2region xdb searcher test program, cache_policy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, io_count: 7, took: 13 μs}
ip2region>> 
```

输入 ip 即可进行查询，输入 quit 即可退出测试程序。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同的缓存实现的效率。

# bench 测试

通过 `xdb_searcher bench` 命令来进行 bench 测试，一方面确保查询程序和 `xdb` 文件没有错误，另一方面可以通过大量的查询得到评价的查询性能：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 来进行 bench 测试：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=vectorIndex
Bench finished, {cache_policy: vectorIndex, total: 3417955, took: 4.233s, cost: 1 μs/op}
```

可以设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 不同缓存实现机制的效率。 @Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件相同的源文件。
