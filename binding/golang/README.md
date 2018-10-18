### golang 实现ip地址查询

#### 获取

```
go get github.com/lionsoul2014/ip2region/binding/golang
```




#### 使用

```golang

package main

import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/ip2region"
)

func main() {
	fmt.Println("err")
	region, err := ip2region.New("ip2region.db")
	defer region.Close()
	if err != nil {
		fmt.Println(err)
		return
	}
	ip, err := region.MemorySearch("127.0.0.1")
	fmt.Println(ip, err)
	ip, err = region.BinarySearch("127.0.0.1")
	fmt.Println(ip, err)
	ip, err = region.BtreeSearch("127.0.0.1")
	fmt.Println(ip, err)
}

```

#### 返回对象
```golang
type IpInfo struct {
	CityId   int64
	Country  string
	Region   string
	Province string
	City     string
	ISP      string
}
```

#### 性能

|名称|次数|平均耗时|
|---|---|------|
BenchmarkBtreeSearch-4|    200000 |             7715 ns/op
BenchmarkMemorySearch-4|  2000000  |             840 ns/op
BenchmarkBinarySearch-4|    30000   |          42680 ns/op


#### 测试程序

```
cd /binging/golang

go run main.go ../../data/ip2region.db

Or

go build -o ip2region main.go
./ip2region ../../data/ip2region.db

```

会看到如下界面

```
initializing
+-------------------------------------------------------+
| ip2region test script                                 |
| format 'ip type'                                      |
| type option 'b-tree','binary','memory' default b-tree |
| Type 'quit' to exit program                           |
+-------------------------------------------------------+
ip2reginon >> 127.0.0.1 memory
0|未分配或者内网IP|0|0|0|0  960.5µs

```
