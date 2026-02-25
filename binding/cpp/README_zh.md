:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region C++ 查询客户端

## 0. 文件说明
```
Makefile --------- 构建

src ------------------ 源文件目录
src/base.* ----------- 常量及工具函数
src/ip.* ------------- 实现 IP 处理
src/header.* --------- 实现 xdb 头部解析
src/search.* --------- 实现 xdb 查找
src/bench.* ---------- 实现 查找 测速
src/make.* ----------- 实现 生成 xdb 文件
src/edit.* ----------- 实现 原始数据编辑

test ---------------- 测试目录
test/header.cc ------ 测试 头部
test/search.cc ------ 测试 查找
test/bench.cc ------- 测速
test/make.cc -------- 生成 xdb 文件
test/edit_v4.cc ----- 测试 原始数据编辑(ipv4)
test/edit_v6.cc ----- 测试 原始数据编辑(ipv6)


bin --------------- 可执行文件目录(通过 make 生成)
bin/header -------- 测试 头部
bin/search -------- 测试 查找
bin/bench --------- 测速
bin/make ---------- 生成 xdb 文件
bin/edit_v4 ------- 测试 原始数据编辑(ipv4)
bin/edit_v6 ------- 测试 原始数据编辑(ipv6)

readme.md --------- readme
```

## 1. 编译
```
$ make
```

## 2. 查找
### 2.1 示例
```cpp
#include "src/search.h"

// IP 版本: xdb::ipv4 xdb::ipv6
//    策略: xdb::policy_file xdb::policy_vector xdb::policy_content
//               不缓存           部分缓存           全部缓存
int main() {
    std::string xdb_name = "../../data/ip2region_v6.xdb";
    int         version  = xdb::ipv6;
    int         policy   = xdb::policy_content;
    std::string ip       = "2001:200:124::";

    xdb::search_t s(xdb_name, version, policy);
    std::cout << s.search(ip) << std::endl;
    return 0;
}

// $ g++ src/*.cc 1.cc --- 编译
// $ ./a.out ------------- 测试
// Japan|Tokyo|Asagaya-minami|WIDE Project|JP
```

### 2.2 测试 xdb 头部
```
$ ./bin/header
测试 IPv4
版本号: 3
缓存策略: 1
文件生成时间: 2025-09-06 02:24:16
索引起始地址: 955933
索引结束地址: 11042415
IP版本: 4
指针字节数: 4

测试 IPv6
版本号: 3
缓存策略: 1
文件生成时间: 2025-10-17 04:41:04
索引起始地址: 3094259
索引结束地址: 36258303
IP版本: 6
指针字节数: 4
```

### 2.3 测试查找
```
$ ./bin/search
测试 IPv4   不缓存: 成功
测试 IPv4 部分缓存: 成功
测试 IPv4 全部缓存: 成功
测试 IPv6   不缓存: 成功
测试 IPv6 部分缓存: 成功
测试 IPv6 全部缓存: 成功
```

## 3. 测速以及检验正确性
```
./bin/bench
测试 IPv4,   不缓存, total: 3910284, took:    27.60s, cost:   6.59μs/op, io count: 28227147
测试 IPv4, 部分缓存, total: 3910284, took:    21.85s, cost:   5.15μs/op, io count: 24316863
测试 IPv4, 全部缓存, total: 3910284, took:     2.26s, cost:   0.25μs/op, io count: 0
测试 IPv6,   不缓存, total: 4792520, took:   100.40s, cost:  20.22μs/op, io count: 80758866
测试 IPv6, 部分缓存, total: 4792520, took:    93.06s, cost:  18.71μs/op, io count: 75966346
测试 IPv6, 全部缓存, total: 4792520, took:     6.24s, cost:   0.81μs/op, io count: 0
```

## 4. 生成 xdb 文件
### 4.1 生成 xdb 文件
```
$ ./bin/make
生成 ipv4 的 xdb 文件, took: 0.57s
生成 ipv6 的 xdb 文件, took: 1.24s
```

## 5. 原始数据编辑
### 5.1. 使用说明
* 新的IP归属地文件可以包含空行
* 新的IP归属地文件顺序可以乱序, 程序会自动排序
* 新的IP归属地文件顺序可以重叠, 只要无二义性, 程序会自动合并
* 最终的结果会将相邻的且归属地相同的行自动合并
* 以下测试, 原文件使用仓库自带的数据文件, 新文件使用当前目录下的 1.txt
