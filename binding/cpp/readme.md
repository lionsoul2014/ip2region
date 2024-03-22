# ip2region xdb C++ 查询客户端实现

## 使用方式
### 完全基于文件的查询
```
#include <iostream>

#include "xdb_search.h"

int main(int argc, char* argv[]) {
    char file_name[] = "../../data/ip2region.xdb";
    char ip[]        = "1.2.3.4";

    xdb_search_t xdb(file_name);
    xdb.init_file();

    std::cout << xdb.search(ip) << std::endl;
    return 0;
}
```

### 缓存 `vector_index` 索引
```
#include <iostream>

#include "xdb_search.h"

int main(int argc, char* argv[]) {
    char file_name[] = "../../data/ip2region.xdb";
    char ip[]        = "1.2.3.4";

    xdb_search_t xdb(file_name);
    xdb.init_vector_index();

    std::cout << xdb.search(ip) << std::endl;
    return 0;
}
```

### 缓存整个 `xdb` 数据
```
#include <iostream>

#include "xdb_search.h"

int main(int argc, char* argv[]) {
    char file_name[] = "../../data/ip2region.xdb";
    char ip[]        = "1.2.3.4";

    xdb_search_t xdb(file_name);
    xdb.init_content();

    std::cout << xdb.search(ip) << std::endl;
    return 0;
}
```

## 测试程序编译
1. 切换到当前目录
2. 编译

```
$ make
g++ -std=c++11 -O2 xdb_search.cc xdb_search_test.cc -o xdb_search
g++ -std=c++11 -O2 xdb_search.cc xdb_bench.cc xdb_bench_test.cc -o xdb_bench
```

## 测试查询
### 说明
```
$ ./xdb_search --help
./xdb_search  [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vector_index/content
 --help                   print help
```

### 测试
```
$ ./xdb_search --db ../../data/ip2region.xdb --cache-policy vector_index
cache policy : vector_index
ip2region>> 1.2.3.4
美国|0|华盛顿|0|谷歌
```

## bench 测试
### 说明
```
$ ./xdb_bench --help
./xdb_bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vector_index/content
 --help                   print help
```

### 测试
```
$ ./xdb_bench --db ../../data/ip2region.xdb --src ../../data/ip.merge.txt --cache-policy content
total: 3419220, took: 3.44 s, cost: 0.27 μs/op, io count: 0
$ ./xdb_bench --db ../../data/ip2region.xdb --src ../../data/ip.merge.txt --cache-policy vector_index
total: 3419220, took: 45.99 s, cost: 12.24 μs/op, io count: 21739300
$ ./xdb_bench --db ../../data/ip2region.xdb --src ../../data/ip.merge.txt --cache-policy file
total: 3419220, took: 60.39 s, cost: 16.32 μs/op, io count: 25158520
```

