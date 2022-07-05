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

| 编程语言 | 描述 | 开发状态   | 贡献者 |
| :--- | :--- |:-------| :--- |
| [golang](binding/golang) | golang 查询客户端实现 | 已完成    | [Lion](https://github.com/lionsoul2014) |
| [php](binding/php) | php 查询客户端实现 | 已完成    | [Lion](https://github.com/lionsoul2014) |
| [java](binding/java) | java 查询客户端实现 | 已完成 | [Lion](https://github.com/lionsoul2014) |
| [lua](binding/lua) | lua 查询客户端实现 | 开发中... | [Lion](https://github.com/lionsoul2014) |
| [c](binding/c) | ANSC c 查询客户端实现 | 已完成 | [Lion](https://github.com/lionsoul2014) |
| [lua_c](binding/lua_c) | lua 查询客户端实现 | 已完成 | [Lion](https://github.com/lionsoul2014) |
| [rust](binding/rust) | rust 查询客户端实现 | 开发中... | [Lion](https://github.com/lionsoul2014) |
| [python](binding/python) | python 查询客户端实现 | 待开始    | 待确定 |
| [nodejs](binding/nodejs) | nodejs 查询客户端实现 | 待开始    | 待确定 |
| [php5_ext](binding/php5_ext) | php5 c 扩展查询客户端实现 | 待开始    | 待确定 |
| [php7_ext](binding/php7_ext) | php7 c 扩展查询客户端实现 | 待开始    | 待确定 |
| [nginx](binding/nginx) | nginx的扩展查询客户端实现 | 待开始    | 待确定 |
| [csharp](binding/csharp) | c# 查询客户端实现 | 待开始    | 待确定 |



# `xdb` 数据生成

API 介绍，使用文档和测试程序请参考对应 `maker` 生成程序下的 ReadMe 介绍，全部生成 maker 实现情况如下：

| 编程语言 | 描述 | 开发状态 | 贡献者 |
| :--- | :--- | :--- | :--- |
| [golang](maker/golang) | golang xdb 生成程序实现 | 已完成 | [Lion](https://github.com/lionsoul2014) |
| [java](maker/java) | java xdb 生成程序实现 | 开发中... | [Lion](https://github.com/lionsoul2014) |
| [c](maker/c) | ANSC c xdb 生成客户端实现 | 开发中... | [Lion](https://github.com/lionsoul2014) |



# 并发查询必读

全部查询客户端的 search 接口都 <b>不是</b> 并发安全的实现，不同进程/线程/协程需要通过创建不同的查询对象来安全使用，并发量很大的情况下，基于文件查询的方式可能会导致打开文件数过多的错误，请修改内核的最大允许打开文件数(fs.file-max=一个更高的值)，或者将整个xdb加载到内存进行安全并发使用。


# 相关备注

### 1、使用声明 
ip2region 重点在于<b>研究 IP 定位数据的存储设计和各种语言的查询实现</b>，并没有原始 IP 数据的支撑，本项目不保证及时的数据更新，没有也不会有商用版本，你可以使用自定义的数据导入 ip2region 进行管理。

### 2、技术交流
ip2region 微信交流群，请先加微信：lionsoul2014 (请备注 ip2region)

### 3、数据更新
基于检测算法的数据更新方式视频分享：[数据更新实现视频分享 - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)，[数据更新实现视频分享 - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)

### 4、数据结构
1. v1.0数据库文件的结构和原理 blog：[“ip2region 数据库文件的结构和原理”](https://github.com/dongyado/dongyado.github.io/blob/master/_posts/2016-08-18-structure-of-ip2region-database-file.md)
2. v1.0素据库文件的结构和原理 视频：[ip2region 数据结构设计和实现视频分享](https://www.bilibili.com/video/BV1wv4y1N7SD)
