# Ip2region 是什么

[ip2region](https://ip2region.net) - 是一个离线IP地址定位库和IP定位数据管理框架，同时支持`IPv4`和`IPv6`，10微秒级别的查询效率，提供了众多主流编程语言的 `xdb` 数据生成和查询客户端实现。



# Ip2region 特性

### 1、离线定位库

项目本身同时了提供了一份 IPv4(`data/ipv4_source.txt`) 和 IPv6(`data/ipv6_source.txt`) 的原始数据和对应的 xdb 文件(`data/ip2region_v4.xdb` 和 `data/ip2region_v6.xdb`) 用于实现精确到城市的的查询定位功能。

### 2、数据管理框架

`xdb` 支持亿级别的 IP 数据段行数，region 信息支持完全自定义，自带数据的 region 信息固定了格式为：`国家|省份|城市|ISP`，你可以在 region 中追加特定业务需求的数据，例如：GPS信息/国际统一地域信息编码/邮编等。也就是你完全可以使用 ip2region 来管理你自己的 IP 定位数据。

### 3、数据去重和压缩

`xdb` 格式生成程序会自动处理输入的原始数据，检查并且完成相连 IP 段的的合并以及相同地域信息的去重和压缩。

### 4、极速查询响应

即使是完全基于 `xdb` 文件的查询，单次查询响应时间在十微秒级别，可通过如下两种方式开启内存加速查询：

1. `vIndex` 索引缓存 ：使用固定的 `512KiB` 的内存空间缓存 vector index 数据，减少一次 IO 磁盘操作，保持平均查询效率稳定在100微秒之内。
2. `xdb` 整个文件缓存：将整个 `xdb` 文件全部加载到内存，内存占用等同于 `xdb` 文件大小，无磁盘 IO 操作，保持10微秒级别的查询效率。

### 5、统一的查询接口

`xdb` 提供了版本兼容的查询实现，一个统一的 API 可以同时提供对 IPv4 和 IPv6 数据的查询并且返回统一的数据。

# `xdb` 数据查询

API 介绍，使用文档和测试程序请参考对应 `searcher` 查询客户端下的 ReadMe 介绍，全部查询 binding 实现情况如下：
| 编程语言                         | 描述                        | IPv4 支持          | IPv6 支持          | 贡献者                                            |
|:---------------------------------|:-----------------------------------------------|:-------------------|:------------------ |:--------------------------------------------------|
| [Golang](binding/golang)         | golang xdb 查询客户端                          | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [PHP](binding/php)               | php xdb 查询客户端                             | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Java](binding/java)             | java xdb 查询客户端                            | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [C](binding/c)                   | POSIX C xdb 查询客户端                         | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Lua_c](binding/lua_c)           | lua c 扩展 xdb 查询客户端                      | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Lua](binding/lua)               | lua xdb 查询客户端                             | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Rust](binding/rust)             | rust xdb 查询客户端                            | :white_check_mark: | :white_check_mark: | [gongzhengyang](https://github.com/gongzhengyang) |
| [Python](binding/python)         | python xdb 查询客户端                          | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Javascript](binding/javascript) | javascript (nodejs, typescript) xdb 查询客户端 | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Csharp](binding/csharp)         | csharp xdb 查询客户端                          | :white_check_mark: | :x:                | [Alen Lee](https://github.com/malus2077)          |
| [Erlang](binding/erlang)         | erlang xdb 查询客户端                          | :white_check_mark: | :x:                | [malou](https://github.com/malou996)              |
| [Nginx](binding/nginx)           | nginx 扩展 xdb 查询客户端                      | :white_check_mark: | :x:                | [Wu Jian Ping](https://github.com/wujjpp)         |
| [C++](binding/cpp)               | C++ xdb 查询客户端                             | :white_check_mark: | :x:                | [Yunbin Liu](https://github.com/liuyunbin)        |


以下工具链实现由社区开发者通过第三方仓库贡献：
| 编程语言                                                        | 描述                    | 贡献者                                         |
|:--------------------------------------------------------------- |:------------------------|:-----------------------------------------------|
| [ip2region-composer](https://github.com/zoujingli/ip2region)    | php composer 管理客户端 | [邹景立](https://github.com/zoujingli)         |
| [ip2region-ts](https://github.com/Steven-Qiang/ts-ip2region2)   | node.js addon 管理客户端| [Steven Qiang](https://github.com/Steven-Qiang)|
| [ruby-ip2region](https://github.com/jicheng1014/ruby-ip2region) | ruby xdb 查询客户端实现 | [jicheng1014](https://github.com/jicheng1014)  |
| [Ip2regionTool](https://github.com/orestonce/Ip2regionTool)     | ip2region 数据转换工具  | [orestonce](https://github.com/orestonce)      |


# `xdb` 数据生成

API 介绍，使用文档和测试程序请参考如下 `maker` 生成程序下的 ReadMe 文档：

| 编程语言                | 描述                 | IPv4 支持          | IPv6 支持          | 贡献者                                                                                      |
| :---------------------- | :------------------- |:-------------------|:-------------------| :------------------------------------------------------------------------------------------ |
| [Golang](maker/golang)  | golang xdb 生成程序  | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)                                                     |
| [Java](maker/java)      | java xdb 生成程序    | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)                                                     |
| [Python](maker/python)  | python xdb 生成程序  | :white_check_mark: | :x:                | [leolin49](https://github.com/leolin49)                                                     |
| [Csharp](maker/csharp)  | csharp xdb 生成程序  | :white_check_mark: | :x:                | [Alan Lee](https://github.com/malus2077)                                                    |
| [Rust](maker/rust)      | rust xdb 生成程序    | :white_check_mark: | :white_check_mark: | [KevinWang](https://github.com/KevinWL) & [gongzhengyang](https://github.com/gongzhengyang) |
| [C++](maker/cpp)        | C++ xdb 生成程序     | :white_check_mark: | :x:                | [Yunbin Liu](https://github.com/liuyunbin)                                                  |
                                                 
                                                
# `xdb` 数据更新
                                                 
ip2region 项目的核心在于 <b>研究 IP 数据的存储和快速查询的设计和实现</b>， 项目自带的 `./data/ipv4_source.txt` 和 `./data/ipv6_source.txt` 原始数据不会再提供更新，对于数据精度和更新频率要求很高的使用场景建议到 [Ip2Region社区](https://ip2region.net/products/offline) 或者第三方购买商用离线数据，你可以使用如下几种方式来尝试自己更新数据：
                                                     
### 手动编辑更新
你可以基于 ip2region 自带的 `./data/ipv4_source.txt` 和 `./data/ipv6_source.txt` 原始 IP 数据用 ip2region 提供的编辑工具来自己修改，目前数据源有如下几种方式：
1. ip2region 社区提供的数据（请参考地底部的公众号关注社区通知）
2. ip2region Github/Gitee 中带有 `[数据源补充]` 标签的 Issue
3. 其他自定义数据：例如客户提供的数据，或者通过 GPS 和 WIFI 定位得到的数据，或者来自其他平台的合法合规的数据

原始 IP 数据编辑工具使用方法请参考如下的 `maker` 生成程序下的 ReadMe 文档：
| 编程语言                            | 描述                     | IPv4 支持          | IPv6 支持          | 贡献者                                     |
|:------------------------------------|:-------------------------|:-------------------|:-------------------|:-------------------------------------------|
| [Golang](maker/golang#xdb-数据编辑) | golang IP 原始数据编辑器 | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)    |
| [Java](maker/java#xdb-数据编辑)     | java IP 原始数据编辑器   | :white_check_mark: | :soon:             | [Lion](https://github.com/lionsoul2014)    |
| [C++](maker/cpp#xdb-数据编辑)       | C++ IP 原始数据编辑器    | :white_check_mark: | :x:                | [Yunbin Liu](https://github.com/liuyunbin) |


### 检测自动更新
如果你想通过你自己的 API 或数据源来更新数据，你可以参考以下视频分享的 `基于检测算法` 的更新算法来自己编写一个更新程序：
1. [数据更新实现视频分享 - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
2. [数据更新实现视频分享 - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)

# 官方社区
Ip2Region 官方社区正式上线于 `2025/06/12` 日，一方面提供了稳定的 [商用离线数据](https://ip2region.net/products/offline) 服务，另一方面便于在核心代码外强化 IP 工具链和数据服务，例如 [使用文档](https://ip2region.net/doc/)，[查询测试](https://ip2region.net/search/demo)，数据纠错等，更多关于社区的信息和服务请访问 [Ip2Region 官方社区](https://ip2region.net/)。

# 相关备注

### 1、并发查询必读
xdb 整个缓存的查询都 <b>是</b> 并发安全的，基于文件的查询都 <b>不是</b> 并发安全的实现，不同进程/线程/协程需要通过创建不同的查询对象来安全使用，并发量很大的情况下，基于文件查询的方式可能会导致打开文件数过多的错误，请修改内核的最大允许打开文件数(fs.file-max=一个更高的值)，或者将整个xdb加载到内存进行安全并发使用。

### 2、核心 xdb 技术：
1. xdb 数据结构分析：[“ip2region xdb-数据结构描述“](https://ip2region.net/doc/xdb/structure)
2. xdb 查询过程分析：[“ip2region xdb-查询过程描述”](https://ip2region.net/doc/xdb/search)
3. xdb 生成过程分析：[“ip2region xdb-生成过程描述”](https://ip2region.net/doc/xdb/generate)
4. xdb 文件生成教程：[“ip2region xdb-文件生成教程”](https://ip2region.net/doc/data/xdb_make)
5. xdb 数据更新方法：[“ip2region 数据更新和 xdb 数据编辑器的使用”](https://mp.weixin.qq.com/s/cZH5qIn4E5rQFy6N32RCzA)

### 3、技术信息博客
1. 微信公众号 - lionsoul-org，作者活跃的技术分享渠道
2. [Ip2Region 官方社区](https://ip2region.net)
