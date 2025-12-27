# ip2region xdb golang 查询客户端实现

# 使用方式

### package 获取
```bash
go get github.com/lionsoul2014/ip2region/binding/golang
```

### 关于查询服务
从 `3.11.0` 版本开始提供了一个双协议兼容且并发安全的 `Ip2Region` 查询服务，**建议优先使用该方式来进行查询调用**，具体使用方式如下：
```go
import "github.com/lionsoul2014/ip2region/binding/golang/service"

// 1, 创建 v4 的配置：指定缓存策略和 v4 的 xdb 文件路径
// 参数1： 缓存策略, options: service.NoCache / service.VIndexCache / service.BufferCache
// 参数2: xdb 文件路径
// 参数3: 初始化的查询器数量
v4Config, err := service.NewV4Config(service.VIndexCache, "ip2region v4 xdb path", 20)
if err != nil {
    return fmt.Errorf("failed to create v4 config: %s", err)
}

// 2, 创建 v6 的配置：指定缓存策略和 v6 的 xdb 文件路径
v6Config, err := service.NewV6Config(service.VIndexCache, "ip2region v6 xdb path", 20)
if err != nil {
    return fmt.Errorf("failed to create v6 config: %s", err)
}

// 3，通过上述配置创建 Ip2Region 查询服务
ip2region, err := service.NewIp2Region(v4Config, v6Config)
if err != nil {
    return fmt.Errorf("failed to create ip2region service: %s", err)
}

// 4，导出 ip2region 服务进行双版本的IP地址的并发查询，例如：
v4Region, err := ip2region.SearchByStr("113.92.157.29")                          // 进行 IPv4 查询
v6Region, err := ip2region.SearchByStr("240e:3b7:3272:d8d0:db09:c067:8d59:539e") // 进行 IPv6 查询


// 5，在服务需要关闭的时候，同时关闭 ip2region 查询服务
ip2region.Close()
```

##### `Ip2Region` 查询备注:
1. 该查询服务的 API 并发安全且同时支持 IPv4 和 Ipv6 的地址，内部实现会自动判断。
2. v4 和 v6 的配置需要单独创建，可以给 v4 和 v6 设置使用不同的缓存策略，也可以指定其中一个为 `nil` 则该版本的 IP 地址查询都会返回 `""`。
3. 请结合您的项目的并发数设置一个合适的查询器数量，这个值在运行过程中是固定的，每次查询会从池子里租借一个查询器来完成查询操作，查询完成后再归还回去，如果租借的时候池子已经空了则等待直到有可用的查询器来完成查询服务。
4. 如果配置设置的缓存策略为 `service.BufferCache` 即 `全内存缓存` 则默认会使用单实例的内存查询器，该实现天生并发安全，此时指定的查询器数量无效。
5. 如果 `Ip2Region` 查询器在提供服务期间，调用 Close 默认会最大等待 10 秒钟来等待尽量多的查询器归还，也可以调用 `CloseTimeout` 来自定义最长等待时间。


### 关于查询 API
定位信息查询 API 原型为：
```go
SearchByStr(string) (string, error)
Search([]byte) (string, error)
```
查询出错则 error 会包含具体的错误信息，查询成功会返回字符串的 `region` 信息，如果指定的 IP 查询不到则会返回空字符串 `""`。

### 关于 IPv4 / IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```go
// 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 xdb.IPv4
dbPath := "../../data/ip2region_v4.xdb"  // 或者你的 ipv4 xdb 的路径
version := xdb.IPv4

// 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 xdb.IPv6
dbPath = "../../data/ip2region_v6.xdb"  // 或者你的 ipv6 xdb 路径
version = xdb.IPv6

// dbPath 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
// 备注：以下演示直接使用 dbPath 和 version 变量
```

### 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。
你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```go
err := xdb.VerifyFromFile(dbPath)
if err != nil {
	// err 包含的验证的错误
	return fmt.Errorf("xdb file verify: %w", err)
}

// 当前使用的 Searcher 可以安全的用于对 dbPath 指向的 xdb 的查询操作
```

### 完全基于文件的查询

```go
import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
    "time"
)

func main() {
	// 通过 version 和 dbPath 创建完全基于文件的查询对象
    searcher, err := xdb.NewWithFileOnly(version, dbPath)
    if err != nil {
        fmt.Printf("failed to create searcher: %s\n", err.Error())
        return
    }

    defer searcher.Close()

    // 定位信息查询：IPv4 或者 IPv6 的地址都支持
    var ip = "1.2.3.4"  // IPv4
	// ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff" // IPv6
    var tStart = time.Now()
    region, err := searcher.SearchByStr(ip)
    if err != nil {
        fmt.Printf("failed to SearchIP(%s): %s\n", ip, err)
        return
    }

	// IPv4 或者 IPv6 的定位信息 
    fmt.Printf("{region: %s, took: %s}\n", region, time.Since(tStart))

    // 备注：并发使用，每个 goroutine 需要创建一个独立的 searcher 对象。
}
```

### 缓存 `VectorIndex` 索引

可以预先加载 `vectorIndex` 缓存，然后做成全局变量，每次创建 searcher 的时候使用全局的 `vectorIndex`，可以减少一次固定的 IO 操作从而加速查询，减少系统 io 压力。
```go
// 1、从 dbPath 加载 VectorIndex 缓存，把下述 vIndex 变量全局到内存里面。
vIndex, err := xdb.LoadVectorIndexFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load vector index from `%s`: %s\n", dbPath, err)
    return
}

// 2、用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
searcher, err := xdb.NewWithVectorIndex(version, dbPath, vIndex)
if err != nil {
    fmt.Printf("failed to create searcher with vector index: %s\n", err)
    return
}

// 备注：并发使用，全部 goroutine 共享全局的只读 vIndex 缓存，每个 goroutine 创建一个独立的 searcher 对象
```

### 缓存整个 `xdb` 数据

可以预先加载整个 ip2region.xdb 到内存，完全基于内存查询，类似于之前的 memory search 查询。
```go
// 1、从 dbPath 加载整个 xdb 到内存
cBuff, err := xdb.LoadContentFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load content from `%s`: %s\n", dbPath, err)
    return
}

// 2、用全局的 cBuff 创建完全基于内存的查询对象。
searcher, err := xdb.NewWithBuffer(version, cBuff)
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

### 查询命令
通过 `./xdb_searcher search` 命令来测试 xdb 的查询：
```bash
➜  golang git:(master) ✗ ./xdb_searcher search --help
./xdb_searcher search [command options]
options:
 --v4-db string            ip2region v4 binary xdb file path
 --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content
 --v6-db string            ip2region v6 binary xdb file path
 --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content
 --help                    print this help menu
```

### 参数解析
1. `v4-xdb`: IPv4 的 xdb 文件路径，默认为仓库中的 data/ip2region_v4.xdb
2. `v6-xdb`: IPv6 的 xdb 文件路径，默认为仓库中的 data/ip2region_v6.xdb
3. `v4-cache-policy`: v4 查询使用的缓存策略，默认为 `vectorIndex`，可选：file/vectorIndex/content
4. `v6-cache-policy`: v6 查询使用的缓存策略，默认为 `vectorIndex`，可选：file/vectorIndex/content

### 测试 Demo
例如：使用默认的 data/ip2region_v4.xdb 和 data/ip2region_v6.xdb 进行查询测试：
```bash
➜  golang git:(master) ✗ ./xdb_searcher search       
ip2region search service test program
+-v4 db: /data01/code/c/ip2region/data/ip2region_v4.xdb (vectorIndex)
+-v6 db: /data01/code/c/ip2region/data/ip2region_v6.xdb (vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|华盛顿|0|谷歌, took: 69.088µs}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|家庭宽带, took: 67.756µs}
ip2region>>
```
输入 v4 或者 v6 的 IP 地址即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。


# bench 测试

### 测试命令
通过 `xdb_searcher bench` 命令来进行自动 bench 测试，一方面确保程序和 `xdb` 文件都没有错误，另一方面通过大量的查询得到平均查询性能：
```bash
➜  golang git:(fr_xdb_ipv6) ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

### v4 bench
例如：通过 data/ip2region_v4.xdb 和 data/ipv4_source.txt 进行 ipv4 的 bench 测试：
```bash
./xdb_searcher bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt 
```

### v6 bench
例如：通过 data/ip2region_v6.xdb 和 data/ipv6_source.txt 进行 ipv6 的 bench 测试：
```bash
./xdb_searcher bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt 
```

可以设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 不同缓存实现机制的效率。

*请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件*。

bench 程序会逐行读取 `src` 指定的源IP文件，然后每个 IP 段选取 5 个固定位置的 IP 进行测试，以确保查询的 region 信息和原始的 region 信息是相同。测试途中没有调试信息的输出，有错误会打印错误信息并且终止运行，所以看到 `Bench finished` 就表示 bench 成功了，cost 是表示每次查询操作的平均时间(ns)。
