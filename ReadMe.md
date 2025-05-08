# Ip2region 是什么

ip2region - 是一个离线IP地址定位库和IP定位数据管理框架，10微秒级别的查询效率，提供了众多主流编程语言的 `xdb` 数据生成和查询客户端实现。



# Ip2region 特性

### 1、IP 数据管理框架

`xdb` 支持亿级别的 IP 数据段行数，默认的 region 信息都固定了格式：`国家|区域|省份|城市|ISP`，缺省的地域信息默认是0。
region 信息支持完全自定义，例如：你可以在 region 中追加特定业务需求的数据，例如：GPS信息/国际统一地域信息编码/邮编等。也就是你完全可以使用 ip2region 来管理你自己的 IP 定位数据。

### 2、数据去重和压缩

`xdb` 格式生成程序会自动去重和压缩部分数据，默认的全部 IP 数据，生成的 ip2region.xdb 数据库是 11MiB，随着数据的详细度增加数据库的大小也慢慢增大。

### 3、极速查询响应

即使是完全基于 `xdb` 文件的查询，单次查询响应时间在十微秒级别，可通过如下两种方式开启内存加速查询：

1. `vIndex` 索引缓存 ：使用固定的 `512KiB` 的内存空间缓存 vector index 数据，减少一次 IO 磁盘操作，保持平均查询效率稳定在10-20微秒之间。
2. `xdb` 整个文件缓存：将整个 `xdb` 文件全部加载到内存，内存占用等同于 `xdb` 文件大小，无磁盘 IO 操作，保持微秒级别的查询效率。


# `xdb` 数据查询

API 介绍，使用文档和测试程序请参考对应 `searcher` 查询客户端下的 ReadMe 介绍，全部查询 binding 实现情况如下：

| Ok?                | 状态 | 编程语言 | 描述                    | 贡献者                |
|:-------------------|:-----| :--- |:----------------------|:--------------------|
| :white_check_mark: | 已完成  | [golang](binding/golang) | golang xdb 查询客户端实现    | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [php](binding/php) | php xdb 查询客户端实现       | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [java](binding/java) | java xdb 查询客户端实现      | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [lua](binding/lua) | 纯 lua xdb 查询客户端实现     | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [c](binding/c) | ANSC c xdb 查询客户端实现    | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [lua_c](binding/lua_c) | lua c 扩展 xdb 查询客户端实现  | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [rust](binding/rust) | rust xdb 查询客户端实现      | [gongzhengyang](https://github.com/gongzhengyang) |
| :white_check_mark: | 已完成  | [python](binding/python) | python xdb 查询客户端实现    | [厉害的花花](https://github.com/luckydog6132) |
| :white_check_mark: | 已完成  | [nodejs](binding/nodejs) | nodejs xdb 查询客户端实现    | [Wu Jian Ping](https://github.com/wujjpp) |
| :white_check_mark: | 已完成  | [csharp](binding/csharp) | csharp xdb 查询客户端实现   | [Alen Lee](https://github.com/malus2077) |
| :white_check_mark: | 已完成  | [erlang](binding/erlang) | erlang xdb 查询客户端实现   | [malou](https://github.com/malou996) |
| &nbsp;&nbsp;&nbsp; | 待开始  | [php_ext](binding/php7_ext) | php c 扩展 xdb 查询客户端实现 | 待确定 |
| :white_check_mark: | 已完成  | [nginx](binding/nginx) | nginx 扩展 xdb 查询客户端实现  | [Wu Jian Ping](https://github.com/wujjpp) |
| :white_check_mark: | 已完成  | [C++](binding/cpp) | C++ xdb 查询客户端实现    | [Yunbin Liu](https://github.com/liuyunbin) |
| :white_check_mark: | 已完成  | [Typescript](binding/typescript) | Typescript xdb 查询客户端实现    | [Alan Lee](https://github.com/malus2077) |


以下工具链实现由社区开发者通过第三方仓库贡献：
| Ok?                | 状态 | 编程语言 | 描述                    | 贡献者            |
|:-------------------|:-----| :--- |:----------------------|:--------------------|
| :white_check_mark: | 已完成  | [ruby-ip2region](https://github.com/jicheng1014/ruby-ip2region) | ruby xdb 查询客户端实现    | [jicheng1014](https://github.com/jicheng1014) |
| :white_check_mark: | 已完成  | [Ip2regionTool](https://github.com/orestonce/Ip2regionTool) | ip2region数据转换工具     | [orestonce](https://github.com/orestonce) |


# `xdb` 数据生成

API 介绍，使用文档和测试程序请参考如下 `maker` 生成程序下的 ReadMe 文档：

| Ok?                | 状态 | 编程语言 | 描述 | 贡献者 |
|:-------------------|:-----| :--- | :--- | :--- |
| :white_check_mark: | 已完成  | [golang](maker/golang) | golang xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [java](maker/java) | java xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| &nbsp;&nbsp;&nbsp; | 待开始  | [c](maker/c) | ANSC c xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [python](maker/python) | python xdb 生成程序实现 | [leolin49](https://github.com/leolin49) |
| :white_check_mark: | 已完成  | [csharp](maker/csharp) | csharp xdb 生成程序实现 | [Alan Lee](https://github.com/malus2077) |
| :white_check_mark: | 已完成  | [rust](maker/rust) | rust xdb 生成程序实现 | [KevinWang](https://github.com/KevinWL) |
| :white_check_mark: | 已完成  | [C++](maker/cpp) | C++ xdb 生成程序实现 | [Yunbin Liu](https://github.com/liuyunbin) |


# `xdb` 数据更新

ip2region 旨在于 <b>研究 IP 数据的存储和快速查询的设计和实现</b>，并没有原始 IP 数据的支撑，也不会有商用版本。
本项目的自带的 `./data/ip.merge.txt` 原始数据已经很久没有更新，也不会再更新，对于数据精度和更新频率要求很高的使用场景建议购买第三方商用数据，你可以使用如下几种方式来尝试自己更新数据：

### 手动编辑更新
你可以基于 ip2region 自带的 `./data/ip.merge.txt` 原始 IP 数据用 ip2region 提供的编辑工具来自己修改，目前数据源有如下几种方式：
1. ip2region 社区提供的数据（请参考地底部的公众号关注社区通知）
2. ip2region Github/Gitee 中带有 `[数据源补充]` 标签的 Issue
3. 其他自定义数据：例如客户提供的数据，或者通过 GPS 和 WIFI 定位得到的数据，或者来自其他平台的合法合规的数据

原始 IP 数据编辑工具使用方法请参考如下的 `maker` 生成程序下的 ReadMe 文档：

| Ok?                | 状态  | 编程语言 | 描述                 | 贡献者 |
|:-------------------|:----| :--- |:-------------------| :--- |
| :white_check_mark: | 已完成 | [golang](maker/golang#xdb-数据编辑) | golang 原始 IP 数据编辑器 | [Lion](https://github.com/lionsoul2014) |
| &nbsp;&nbsp;&nbsp; | 待开始 | [java](maker/java#xdb-数据编辑) | java 原始 IP 数据编辑器   | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成 | [C++](maker/cpp#xdb-数据编辑) | C++ 原始 IP 数据编辑器 | [Yunbin Liu](https://github.com/liuyunbin) |


### 检测自动更新
如果你想通过你自己的 API 或数据源来更新数据，你可以参考以下视频分享的 `基于检测算法` 的更新算法来自己编写一个更新程序：
1. [数据更新实现视频分享 - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
2. [数据更新实现视频分享 - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)


# 相关备注

### 1、并发查询必读
xdb 整个缓存的查询都 <b>是</b> 并发安全的，基于文件的查询都 <b>不是</b> 并发安全的实现，不同进程/线程/协程需要通过创建不同的查询对象来安全使用，并发量很大的情况下，基于文件查询的方式可能会导致打开文件数过多的错误，请修改内核的最大允许打开文件数(fs.file-max=一个更高的值)，或者将整个xdb加载到内存进行安全并发使用。

### 2、技术资源分享
1. xdb 数据结构分析：[“ip2region xdb 数据结构和查询过程详解“](https://mp.weixin.qq.com/s/ndjzu0BgaeBmDOCw5aqHUg)
2. xdb 查询过程分析：[“ip2region xdb 数据结构和查询过程详解”](https://mp.weixin.qq.com/s/ndjzu0BgaeBmDOCw5aqHUg)
3. xdb 生成过程分析：[“ip2region xdb 二进制数据生成过程详解”](https://mp.weixin.qq.com/s/HEAc7WKzAjH5oTwgxojPUg)
4. xdb 数据更新方法：[“ip2region 数据更新和 xdb 数据编辑器的使用”](https://mp.weixin.qq.com/s/cZH5qIn4E5rQFy6N32RCzA)

### 3、技术信息博客
请先关注微信公众号 lionsoul-org (狮子的魂)
