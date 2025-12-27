# ip2region xdb C++ 实现

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
```
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
// 日本|东京都|千代田区|专线用户
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

### 4.2 测试正确性
```
# ipv4 --- 只有时间不同
$ diff <(xxd ./ip2region_v4.xdb) <(xxd ../../data/ip2region_v4.xdb)
1c1
< 00000000: 0300 0100 9f2f 2969 1d96 0e00 6f7e a800  ...../)i....o~..
---
> 00000000: 0300 0100 509b bb68 1d96 0e00 6f7e a800  ....P..h....o~..

# ipv6 --- 只有时间不同
$ diff <(xxd ./ip2region_v6.xdb) <(xxd ../../data/ip2region_v6.xdb)
1c1
< 00000000: 0300 0100 a02f 2969 f336 2f00 ff41 2902  ...../)i.6/..A).
---
> 00000000: 0300 0100 e0c8 f168 f336 2f00 ff41 2902  .......h.6/..A).
```

## 5. 原始数据编辑
### 5.1. 使用说明
* 新的IP归属地文件可以包含空行
* 新的IP归属地文件顺序可以乱序, 程序会自动排序
* 新的IP归属地文件顺序可以重叠, 只要无二义性, 程序会自动合并
* 最终的结果会将相邻的且归属地相同的行自动合并
* 以下测试, 原文件使用仓库自带的数据文件, 新文件使用当前目录下的 1.txt

### 5.2. 数据正确性测试 -- ipv4
#### 测试一: 测试数据文件包含空行以及重复的情况
```
$ cat -n 1.txt
     1
     2	1.0.128.0|1.0.128.255|测试归属地
     3
     4	1.0.128.0|1.0.128.255|测试归属地
     5
$ ./bin/edit_v4
took: 0.80s
$ git diff ../../data/
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..5d1fdfa 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,7 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试二: 测试数据文件乱序以及数据有交叉, 归属地相同的情况
```
$ cat 1.txt
1.0.128.5|1.0.128.255|测试归属地
1.0.128.0|1.0.128.9|测试归属地
$ ./bin/edit_v4
took: 0.88s
$ git diff ../../data/
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..5d1fdfa 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,7 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试三: 测试数据文件重叠, 归属地相同的情况
```
$ cat 1.txt
1.0.128.0|1.0.128.8|测试归属地
1.0.128.7|1.0.128.255|测试归属地
$ ./bin/edit_v4
took: 0.91s
$ git diff ../../data/
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..5d1fdfa 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,7 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试四: 测试数据文件重叠, 归属地相同的情况
```
$ cat 1.txt
1.0.128.0|1.0.128.8|测试归属地
1.0.128.8|1.0.128.255|测试归属地
$ ./bin/edit_v4
took: 0.81s
git diff ../../data
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..5d1fdfa 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,7 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试五: 测试数据文件连接, 归属地相同的情况
```
$ cat 1.txt
1.0.128.0|1.0.128.8|测试归属地
1.0.128.9|1.0.128.255|测试归属地
$ ./bin/edit_v4
took: 0.71s
git diff ../../data
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..5d1fdfa 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,7 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试六: 测试数据文件重叠, 归属地不同情况
```
$ cat 1.txt
1.0.128.0|1.0.128.8|测试归属地123
1.0.128.8|1.0.128.255|测试归属地
$ ./bin/edit_v4
数据有二义性: 1.0.128.0|1.0.128.8|测试归属地123, 1.0.128.8|1.0.128.255|测试归属地
```

#### 测试七: 测试数据文件连接, 归属地不同情况
```
$ cat 1.txt
1.0.128.0|1.0.128.8|测试归属地123
1.0.128.9|1.0.128.255|测试归属地
$ ./bin/edit_v4
took: 0.75s
git diff ../../data
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..976e6bf 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -7,7 +7,8 @@
 1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
 1.0.64.0|1.0.79.255|日本|广岛县|0|0
 1.0.80.0|1.0.127.255|日本|冈山县|0|0
-1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
+1.0.128.0|1.0.128.8|测试归属地123
+1.0.128.9|1.0.128.255|测试归属地
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
 1.0.133.0|1.0.133.255|泰国|素攀武里府|0|TOT
```

#### 测试八: 测试将一个IP数据拆成多个IP
```
$ cat 1.txt
36.136.1.0|36.136.7.255|中国|0|广西|来宾市|移动
36.136.8.0|36.136.15.255|中国|0|广西|玉林市|移动
36.136.16.0|36.136.23.255|中国|0|广西|河池市|移动
$ ./bin/edit_v4
took: 0.80s
git diff ../../data
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..f895c2f 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -54778,7 +54778,11 @@
 36.134.84.0|36.134.85.255|中国|安徽省|合肥市|移动
 36.134.86.0|36.134.87.255|中国|广西|南宁市|移动
 36.134.88.0|36.134.89.255|中国|内蒙古|呼和浩特市|移动
-36.134.90.0|36.141.255.255|中国|0|0|移动
+36.134.90.0|36.136.0.255|中国|0|0|移动
+36.136.1.0|36.136.7.255|中国|0|广西|来宾市|移动
+36.136.8.0|36.136.15.255|中国|0|广西|玉林市|移动
+36.136.16.0|36.136.23.255|中国|0|广西|河池市|移动
+36.136.24.0|36.141.255.255|中国|0|0|移动
 36.142.0.0|36.142.1.255|中国|四川省|成都市|移动
 36.142.2.0|36.142.31.255|中国|甘肃省|兰州市|移动
 36.142.32.0|36.142.127.255|中国|甘肃省|0|移动
```

#### 测试九: 测试将多个IP数据并成一个IP数据
```
$ cat 1.txt
1.0.16.0|1.0.127.255|测试归属地
$ ./bin/edit_v4
took: 0.76s
git diff ../../data
diff --git a/data/ipv4_source.txt b/data/ipv4_source.txt
index 00dacc3..756354c 100644
--- a/data/ipv4_source.txt
+++ b/data/ipv4_source.txt
@@ -3,10 +3,7 @@
 1.0.1.0|1.0.3.255|中国|福建省|福州市|电信
 1.0.4.0|1.0.7.255|澳大利亚|维多利亚|墨尔本|0
 1.0.8.0|1.0.15.255|中国|广东省|广州市|电信
-1.0.16.0|1.0.31.255|日本|0|0|0
-1.0.32.0|1.0.63.255|中国|广东省|广州市|电信
-1.0.64.0|1.0.79.255|日本|广岛县|0|0
-1.0.80.0|1.0.127.255|日本|冈山县|0|0
+1.0.16.0|1.0.127.255|测试归属地
 1.0.128.0|1.0.128.255|泰国|清莱府|0|TOT
 1.0.129.0|1.0.132.191|泰国|曼谷|曼谷|TOT
 1.0.132.192|1.0.132.255|泰国|Nakhon-Ratchasima|0|TOT
```

### 5.3 数据正确性测试 -- ipv6
#### 测试一: 测试数据文件包含空行以及重复的情况
```
$ cat -n 1.txt
     1
     2	2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|测试归属地
     3
     4	2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|测试归属地
     5
$ ./bin/edit_v6
took: 1.74s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..29617c4 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,7 +2,7 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|测试归属地
 2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试二: 测试数据文件乱序以及数据有交叉, 归属地相同的情况
```
$ cat 1.txt
2001:200:121::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地
2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.68s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..9e83b03 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,9 +2,8 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地
+2001:200:126::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试三: 测试数据文件重叠, 归属地相同的情况
```
$ cat 1.txt
2001:200:120::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地
2001:200:125::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.75s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..7a23ba2 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,9 +2,8 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
+2001:200:127::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试四: 测试数据文件重叠, 归属地相同的情况
```
$ cat 1.txt
2001:200:120::|2001:200:125::|测试归属地
2001:200:125::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.46s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..7a23ba2 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,9 +2,8 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
+2001:200:127::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试五: 测试数据文件连接, 归属地相同的情况
```
$ cat 1.txt
2001:200:120::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地
2001:200:126::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.79s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..7a23ba2 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,9 +2,8 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
+2001:200:127::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试六: 测试数据文件重叠, 归属地不同情况
```
$ cat 1.txt
2001:200:120::|2001:200:126::|测试归属地123
2001:200:126::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
数据有二义性: 2001:200:120::|2001:200:126::|测试归属地123, 2001:200:126::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
```

#### 测试七: 测试数据文件连接, 归属地不同情况
```
$ cat 1.txt
2001:200:120::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地123
2001:200:126::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.78s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..142f7cc 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -2,9 +2,9 @@
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
+2001:200:120::|2001:200:125:ffff:ffff:ffff:ffff:ffff|测试归属地123
+2001:200:126::|2001:200:126:ffff:ffff:ffff:ffff:ffff|测试归属地
+2001:200:127::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
```

#### 测试八: 测试将一个IP数据拆成多个IP
```
$ cat 1.txt
2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ff11|测试归属地1
2001:200:11f:ffff:ffff:ffff:ffff:ff12|2001:200:11f:ffff:ffff:ffff:ffff:ff33|测试归属地2
2001:200:11f:ffff:ffff:ffff:ffff:ff34|2001:200:11f:ffff:ffff:ffff:ffff:ffff|测试归属地3
$ ./bin/edit_v6
took: 1.52s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..e450e27 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -1,7 +1,9 @@
 1:1::|2001:1ff:ffff:ffff:ffff:ffff:ffff:ffff|0|0|内网IP|内网IP
 2001:200::|2001:200:101:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
+2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ff11|测试归属地1
+2001:200:11f:ffff:ffff:ffff:ffff:ff12|2001:200:11f:ffff:ffff:ffff:ffff:ff33|测试归属地2
+2001:200:11f:ffff:ffff:ffff:ffff:ff34|2001:200:11f:ffff:ffff:ffff:ffff:ffff|测试归属地3
 2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
```

#### 测试九: 测试将多个IP数据并成一个IP数据
```
$ cat 1.txt
2001:200:123::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|测试归属地
$ ./bin/edit_v6
took: 1.99s
git diff ../../data
diff --git a/data/ipv6_source.txt b/data/ipv6_source.txt
index 4dee31b..ecd29c3 100644
--- a/data/ipv6_source.txt
+++ b/data/ipv6_source.txt
@@ -3,9 +3,7 @@
 2001:200:102::|2001:200:104:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:105::|2001:200:11f:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:120::|2001:200:122:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:123::|2001:200:123:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
-2001:200:124::|2001:200:129:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
-2001:200:12a::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
+2001:200:123::|2001:200:12a:ffff:ffff:ffff:ffff:ffff|测试归属地
 2001:200:12b::|2001:200:130:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
 2001:200:131::|2001:200:132:ffff:ffff:ffff:ffff:ffff|日本|神奈川县|藤泽市|专线用户
 2001:200:133::|2001:200:135:ffff:ffff:ffff:ffff:ffff|日本|东京都|千代田区|专线用户
```

