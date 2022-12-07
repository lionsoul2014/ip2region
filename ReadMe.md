# Ip2region 是什么

ip2region v2.0 - 是一个离线IP地址定位库和IP定位数据管理框架，10微秒级别的查询效率，提供了众多主流编程语言的 `xdb` 数据生成和查询客户端实现。v1.0 旧版本: [v1.0版本入口](v1.0)



# Ip2region 特性

### 1、标准化的数据格式

每个 ip 数据段的 region 信息都固定了格式：`国家|区域|省份|城市|ISP`，只有中国的数据绝大部分精确到了城市，其他国家部分数据只能定位到国家，后前的选项全部是0。


### 2、数据去重和压缩

`xdb` 格式生成程序会自动去重和压缩部分数据，默认的全部 IP 数据，生成的 ip2region.xdb 数据库是 11MiB，随着数据的详细度增加数据库的大小也慢慢增大。

### 3、极速查询响应

即使是完全基于 `xdb` 文件的查询，单次查询响应时间在十微秒级别，可通过如下两种方式开启内存加速查询：

1. `vIndex` 索引缓存 ：使用固定的 `512KiB` 的内存空间缓存 vector index 数据，减少一次 IO 磁盘操作，保持平均查询效率稳定在10-20微秒之间。
2. `xdb` 整个文件缓存：将整个 `xdb` 文件全部加载到内存，内存占用等同于 `xdb` 文件大小，无磁盘 IO 操作，保持微秒级别的查询效率。

### 4、IP 数据管理框架

v2.0 格式的 `xdb` 支持亿级别的 IP 数据段行数，region 信息也可以完全自定义，例如：你可以在 region 中追加特定业务需求的数据，例如：GPS信息/国际统一地域信息编码/邮编等。也就是你完全可以使用 ip2region 来管理你自己的 IP 定位数据。



# `xdb` 数据查询

API 介绍，使用文档和测试程序请参考对应 `searcher` 查询客户端下的 ReadMe 介绍，全部查询 binding 实现情况如下：

| Ok?                 | 状态 | 编程语言 | 描述                    | 贡献者                 |
|:-------------------|:-----| :--- |:----------------------|:--------------------|
| :white_check_mark: | 已完成  | [golang](binding/golang) | golang xdb 查询客户端实现    | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [php](binding/php) | php xdb 查询客户端实现       | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [java](binding/java) | java xdb 查询客户端实现      | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [lua](binding/lua) | 纯 lua xdb 查询客户端实现     | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [c](binding/c) | ANSC c xdb 查询客户端实现    | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [lua_c](binding/lua_c) | lua c 扩展 xdb 查询客户端实现  | [Lion](https://github.com/lionsoul2014) |
| &nbsp;&nbsp;&nbsp; | 待开始  | [rust](binding/rust) | rust xdb 查询客户端实现      | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [python](binding/python) | python xdb 查询客户端实现    | [厉害的花花](https://github.com/luckydog6132) |
| :white_check_mark: | 已完成  | [nodejs](binding/nodejs) | nodejs xdb 查询客户端实现    | [Wu Jian Ping](https://github.com/wujjpp) |
| :white_check_mark: | 已完成  | [csharp](binding/csharp) | csharp xdb 查询客户端实现   | [Alen Lee](https://github.com/malus2077) |
| &nbsp;&nbsp;&nbsp; | 待开始  | [php_ext](binding/php7_ext) | php c 扩展 xdb 查询客户端实现 | 待确定 |
| &nbsp;&nbsp;&nbsp; | 待开始  | [nginx](binding/nginx) | nginx 扩展 xdb 查询客户端实现  | 待确定 |



# `xdb` 数据生成

API 介绍，使用文档和测试程序请参考如下 `maker` 生成程序下的 ReadMe 文档：

| Ok?                | 状态 | 编程语言 | 描述 | 贡献者 |
|:-------------------|:-----| :--- | :--- | :--- |
| :white_check_mark: | 已完成  | [golang](maker/golang) | golang xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [java](maker/java) | java xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| &nbsp;&nbsp;&nbsp; | 待开始  | [c](maker/c) | ANSC c xdb 生成程序实现 | [Lion](https://github.com/lionsoul2014) |
| :white_check_mark: | 已完成  | [python](maker/python) | python xdb 生成程序实现 | [leolin49](https://github.com/leolin49) |
| :white_check_mark: | 已完成  | [csharp](maker/csharp) | csharp xdb 生成程序实现 | [Alan Lee](https://github.com/malus2077) |


# `xdb` 数据更新

ip2region 重点在于 <b>研究 IP 数据的存储和快速查询的实现</b>，并没有原始 IP 数据的支撑，本项目不保证及时的数据更新，暂时也不会有商用版本，你可以使用如下几种方式来更新数据：

### 手动编辑更新
你可以基于 ip2region 自带的 `./data/ip.merge.txt` 原始 IP 数据用 ip2region 提供的编辑工具来自己修改，目前数据源有如下几种方式：
1. ip2region 社区群提供的数据（请参考地底部的备注信息加入 ip2region 微信群）
2. ip2region Github/Gitee 中带有 `[数据源补充]` 标签的 Issue
3. 其他自定义数据：例如客户提供的数据，或者通过 GPS 和 WIFI 定位得到的数据，或者你信任的他平台的数据
4. ip2region 官方不定时提供的数据

原始 IP 数据编辑工具使用方法请参考如下的 `maker` 生成程序下的 ReadMe 文档：

| Ok?                | 状态  | 编程语言 | 描述                 | 贡献者 |
|:-------------------|:----| :--- |:-------------------| :--- |
| :white_check_mark: | 已完成 | [golang](maker/golang#xdb-数据编辑) | golang 原始 IP 数据编辑器 | [Lion](https://github.com/lionsoul2014) |
| &nbsp;&nbsp;&nbsp; | 待开始 | [java](maker/java#xdb-数据编辑) | java 原始 IP 数据编辑器   | [Lion](https://github.com/lionsoul2014) |


### 爬虫自动更新
如果你想通过第三方的 API 来更新数据，你可以参考以下视频分享的 `基于检测算法` 的更新算法来自己编写一个更新程序：
1. [数据更新实现视频分享 - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
2. [数据更新实现视频分享 - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)


# 相关备注

### 1、并发查询必读
全部查询客户端的 search 接口都 <b>不是</b> 并发安全的实现，不同进程/线程/协程需要通过创建不同的查询对象来安全使用，并发量很大的情况下，基于文件查询的方式可能会导致打开文件数过多的错误，请修改内核的最大允许打开文件数(fs.file-max=一个更高的值)，或者将整个xdb加载到内存进行安全并发使用。

### 2、技术交流
ip2region 微信交流群，请先加微信：lionsoul2014 (请备注 ip2region)

### 3、数据结构
1. xdb 数据结构分析：[“ip2region xdb 数据结构和查询过程详解“](https://mp.weixin.qq.com/s?__biz=MzU4MDc2MzQ5OA==&mid=2247483696&idx=1&sn=6e9e138e86cf18245656c54ff4be3129&chksm=fd50ab35ca2722239ae7c0bb08efa44f499110c810227cbad3a16f36ebc1c2afc58eb464a57c#rd)
2. xdb 查询过程分析：[“ip2region xdb 数据结构和查询过程详解”](https://mp.weixin.qq.com/s?__biz=MzU4MDc2MzQ5OA==&mid=2247483696&idx=1&sn=6e9e138e86cf18245656c54ff4be3129&chksm=fd50ab35ca2722239ae7c0bb08efa44f499110c810227cbad3a16f36ebc1c2afc58eb464a57c#rd)
3. xdb 生成过程分析：[“ip2region xdb 二进制数据生成过程详解”](https://mp.weixin.qq.com/s?__biz=MzU4MDc2MzQ5OA==&mid=2247483718&idx=1&sn=92e552f3bba44a97ca661da244f35574&chksm=fd50ab43ca2722559733ed4e1082f239f381aaa881f9dbeb479174936145522696d9d200531e#rd)
