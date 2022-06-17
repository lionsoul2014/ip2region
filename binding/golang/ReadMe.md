# ip2region golang 查询客户端实现

# 使用方式

### package 获取
```bash
go get github.com/lionsoul2014/ip2region/binding/golang
```

### API 使用
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

# 查询测试

# bench 测试
