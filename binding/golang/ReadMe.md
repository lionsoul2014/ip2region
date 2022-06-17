# ip2region xdb golang 查询客户端实现

# 使用方式

### package 获取
```bash
go get github.com/lionsoul2014/ip2region/binding/golang
```

### API 使用 Demo
```golang
import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
    "time"
)

func main() {
    var dbPath = "ip2region.xdb file path"
    searcher, err := xdb.New(dbPath)
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
}
```

# 编译测试程序

通过如下方式编译得到 xdb_searcher 可执行程序：
```bash
# 切换到 golang binding 根目录
go build -o xdb_searcher
```


# 查询测试

通过 `xdb_searcher search` 命令来测试 ip2region.xdb 的查询：
```
➜  golang git:(v2.0_xdb) ✗ ./searcher search
./xdb_searcher search [command options]
options:
 --db string    ip2region binary xdb file path
```

例如：使用默认的 data/ip2region.xdb 进行查询测试
```bash
➜  golang git:(v2.0_xdb) ✗ ./xdb_searcher search --db=../../data/ip2region.xdb
ip2region xdb searcher test program, type `quit` to exit
ip2region>> 1.2.3.4
{region:美国|0|华盛顿|0|谷歌, took:101.57µs}
```

输入 ip 地址进行查询即可，输入 quit 退出测试程序。


# bench 测试

通过 `xdb_searcher bench` 命令来进行自动 bench 测试，一方面确保程序和xdb文件都没有错误，一方面通过大量的查询得到平均查询性能：
```bash
➜  golang git:(v2.0_xdb) ✗ ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string     ip2region binary xdb file path
 --src string    source ip text file path
```

例如：通过 data/ip2region.xdb 和 data/ip.merge.txt 进行 bench 测试：
```bash
➜  golang git:(v2.0_xdb) ✗ ./xdb_searcher bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt
Bench finished, {total: 3417955, took: 28.211578339s, cost: 8253 ns/op}
```

*请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件*。bench 程序会逐行读取 `src` 指定的源IP文件，然后每个 IP 段选取 5 个固定位置的 IP 进行测试，以确保查询的 region 信息和原始的 region 信息是相同。测试途中没有调试信息的输出，有错误会打印错误信息并且终止运行，所以看到 `Bench finished` 就表示 bench 成功了，cost 是表示每次查询操作的平均时间(ns)。
