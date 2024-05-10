# ip2region xdb golang 查询客户端实现

# 使用方式

### package 获取
```bash
go get github.com/lionsoul2014/ip2region/binding/golang
```

### 完全基于文件的查询

```golang
import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
    "time"
)

func main() {
    var dbPath = "ip2region.xdb file path"
    searcher, err := xdb.NewWithFileOnly(dbPath)
    if err != nil {
        fmt.Printf("failed to create searcher: %s\n", err.Error())
        return
    }

    defer searcher.Close()

    // do the search
    var ip = "1.2.3.4"
    var tStart = time.Now()
    region, err := searcher.SearchByStr(ip)
    if err != nil {
        fmt.Printf("failed to SearchIP(%s): %s\n", ip, err)
        return
    }

    fmt.Printf("{region: %s, took: %s}\n", region, time.Since(tStart))

    // 备注：并发使用，每个 goroutine 需要创建一个独立的 searcher 对象。
}
```

### 缓存 `VectorIndex` 索引

可以预先加载 `vectorIndex` 缓存，然后做成全局变量，每次创建 searcher 的时候使用全局的 `vectorIndex`，可以减少一次固定的 IO 操作从而加速查询，减少系统 io 压力。
```golang
// 1、从 dbPath 加载 VectorIndex 缓存，把下述 vIndex 变量全局到内存里面。
vIndex, err := LoadVectorIndexFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load vector index from `%s`: %s\n", dbPath, err)
    return
}

// 2、用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
searcher, err := xdb.NewWithVectorIndex(dbPath, vIndex)
if err != nil {
    fmt.Printf("failed to create searcher with vector index: %s\n", err)
    return
}

// 备注：并发使用，全部 goroutine 共享全局的只读 vIndex 缓存，每个 goroutine 创建一个独立的 searcher 对象
```

### 缓存整个 `xdb` 数据

可以预先加载整个 ip2region.xdb 到内存，完全基于内存查询，类似于之前的 memory search 查询。
```golang
// 1、从 dbPath 加载整个 xdb 到内存
cBuff, err := LoadContentFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load content from `%s`: %s\n", dbPath, err)
    return
}

// 2、用全局的 cBuff 创建完全基于内存的查询对象。
searcher, err := xdb.NewWithBuffer(cBuff)
if err != nil {
    fmt.Printf("failed to create searcher with content: %s\n", err)
    return
}

// 备注：并发使用，用整个 xdb 缓存创建的 searcher 对象可以安全用于并发。
```



# 编译测试程序

通过如下方式编译得到 xdb_searcher 可执行程序：
```bash
# 切换到 golang binding 根目录
make
```


# 查询测试

通过 `xdb_searcher search` 命令来测试 ip2region.xdb 的查询：
```
➜  golang git:(v2.0_xdb) ./xdb_searcher search
./xdb_searcher search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试
```bash
➜  golang git:(v2.0_xdb) ✗ ./xdb_searcher search --db=../../data/ip2region.xdb
ip2region xdb searcher test program, type `quit` to exit
ip2region>> 1.2.3.4
{region:美国|0|华盛顿|0|谷歌, took:101.57µs}
```

输入 ip 地址进行查询即可，输入 quit 退出测试程序。可以设置 `cache-policy` 为 file/vectorIndex/content 来测试不同的查询缓存机制。


# bench 测试

通过 `xdb_searcher bench` 命令来进行自动 bench 测试，一方面确保程序和 `xdb` 文件都没有错误，另一方面通过大量的查询得到平均查询性能：
```bash
➜  golang git:(v2.0_xdb) ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过 data/ip2region.xdb 和 data/ip.merge.txt 进行 bench 测试：
```bash
➜  golang git:(v2.0_xdb) ✗ ./xdb_searcher bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt
Bench finished, {total: 3417955, took: 28.211578339s, cost: 8253 ns/op}
```

可以设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 不同缓存实现机制的效率。

*请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件*。

bench 程序会逐行读取 `src` 指定的源IP文件，然后每个 IP 段选取 5 个固定位置的 IP 进行测试，以确保查询的 region 信息和原始的 region 信息是相同。测试途中没有调试信息的输出，有错误会打印错误信息并且终止运行，所以看到 `Bench finished` 就表示 bench 成功了，cost 是表示每次查询操作的平均时间(ns)。
