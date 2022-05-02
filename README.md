# Ip2region是什么？ 

ip2region - 准确率99.9%的离线IP地址定位库，0.0x毫秒级查询，ip2region.db数据库只有数MB，提供了java,php,c,python,nodejs,golang,c#等查询绑定和Binary,B树,内存三种查询算法。




# Ip2region特性

### 99.9%准确率

数据聚合了一些知名ip到地名查询提供商的数据，这些是他们官方的的准确率，经测试着实比经典的纯真IP定位准确一些。<br />
ip2region的数据聚合自以下服务商的开放API或者数据(升级程序每秒请求次数2到4次): <br />
01, &gt;80%, 淘宝IP地址库, [http://ip.taobao.com/](http://ip.taobao.com/) <br />
02, ≈10%, GeoIP, [https://geoip.com/](https://geoip.com/) <br />
03, ≈2%, 纯真IP库, [http://www.cz88.net/](http://www.cz88.net/) <br />
<b>备注：</b>如果上述开放API或者数据都不给开放数据时ip2region将停止数据的更新服务。


### 标准化的数据格式

每条ip数据段都固定了格式：
```
_城市Id|国家|区域|省份|城市|ISP_
```

只有中国的数据精确到了城市，其他国家有部分数据只能定位到国家，后前的选项全部是0，已经包含了全部你能查到的大大小小的国家（请忽略前面的城市Id，个人项目需求）。


### 体积小

包含了全部的IP，生成的数据库文件ip2region.db只有几MB，最小的版本只有1.5MB，随着数据的详细度增加数据库的大小也慢慢增大，目前还没超过8MB。


### 查询速度快

全部的查询客户端单次查询都在0.x毫秒级别，内置了三种查询算法

1. memory算法：整个数据库全部载入内存，单次查询都在0.1x毫秒内，C语言的客户端单次查询在0.00x毫秒级别。
2. binary算法：基于二分查找，基于ip2region.db文件，不需要载入内存，单次查询在0.x毫秒级别。
3. b-tree算法：基于btree算法，基于ip2region.db文件，不需要载入内存，单词查询在0.x毫秒级别，比binary算法更快。

任何客户端b-tree都比binary算法快，当然memory算法固然是最快的！


### 多查询客户端的支持

已经集成的客户端有：java、C#、php、c、python、nodejs、php扩展(php5和php7)、golang、rust、lua、lua_c, nginx。

binding | 描述 | 开发状态 | binary查询耗时 | b-tree查询耗时 | memory查询耗时
:-: | :-: | :-: | :-: | :-: | :-:
[c](binding/c) | ANSC c binding | 已完成 | 0.0x毫秒 | 0.0x毫秒 | 0.00x毫秒
[c#](binding/c#) | c# binding | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.1x毫秒
[golang](binding/golang) | golang binding | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.1x毫秒
[java](binding/java) | java binding | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.1x毫秒
[lua](binding/lua) | lua实现的binding | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.x毫秒
[lua_c](binding/lua_c) | lua的c扩展      | 已完成 | 0.0x毫秒 | 0.0x毫秒 | 0.00x毫秒
[nginx](binding/nginx) | nginx的c扩展 | 已完成 | 0.0x毫秒 | 0.0x毫秒 | 0.00x毫秒
[nodejs](binding/nodejs) | nodejs | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.1x毫秒
[php](binding/php) | php实现的binding | 已完成 | 0.x毫秒 | 0.1x毫秒 | 0.1x毫秒
[php5_ext](binding/php5_ext) | php5的c扩展 | 已完成 | 0.0x毫秒 | 0.0x毫秒 | 0.00x毫秒
[php7_ext](binding/php7_ext) | php7的c扩展 | 已完成 | 0.0毫秒 | 0.0x毫秒 | 0.00x毫秒
[python](binding/python) | python bindng | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.x毫秒
[rust](binding/rust) | rust binding | 已完成 | 0.x毫秒 | 0.x毫秒 | 0.x毫秒




# ip2region快速测试

请参考每个binding下的README说明去运行cli测试程序，例如C语言的demo运行如下：
```shell
cd binding/c/
gcc -g -O2 testSearcher.c ip2region.c
./a.out ../../data/ip2region.db
```

会看到如下cli界面：
```shell
initializing  B-tree ... 
+----------------------------------+
| ip2region test script            |
| Author: chenxin619315@gmail.com  |
| Type 'quit' to exit program      |
+----------------------------------+
p2region>> 101.105.35.57
2163|中国|华南|广东省|深圳市|鹏博士 in 0.02295 millseconds
```

输入IP地址开始测试，第一次会稍微有点慢，在运行命令后面接入binary,memory来尝试其他算法，建议使用b-tree算法，速度和并发需求的可以使用memory算法，具体集成请参考不同binding下的测试源码。




# ip2region安装

具体请参考每个binding下的README文档和测试demo，以下是一些可用的快捷安装方式：

### maven仓库地址
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>1.7.2</version>
</dependency>
```

### nodejs
```
npm install node-ip2region --save
```

### nuget安装
```shell
Install-Package IP2Region
```

### php composer
```shell
# 插件来自：https://github.com/zoujingli/ip2region
composer require zoujingli/ip2region
```



# ip2region 并发使用

1. 全部binding的各个search接口都<b>不是</b>线程安全的实现，不同线程可以通过创建不同的查询对象来使用，并发量很大的情况下，binary和b-tree算法可能会打开文件数过多的错误，请修改内核的最大允许打开文件数(fs.file-max=一个更高的值)，或者使用持久化的memory算法。

2. memorySearch接口，在发布对象前进行一次预查询(本质上是把ip2region.db文件加载到内存)，可以安全用于多线程环境。




# ip2region.db的生成

从1.8版本开始，ip2region开源了ip2region.db生成程序的java实现，提供了ant编译支持，编译后会得到以下提到的dbMaker-{version}.jar，对于需要研究生成程序的或者更改自定义生成配置的请参考${ip2region_root}/maker/java内的java源码。


从ip2region 1.2.2版本开始里面提交了一个dbMaker-{version}.jar的可以执行jar文件，用它来完成这个工作：

1. 确保你安装好了java环境（不玩Java的童鞋就自己谷歌找找拉，临时用一用，几分钟的事情）
2. cd到${ip2region_root}/maker/java，然后运行如下命令：

```shell
java -jar dbMaker-{version}.jar -src 文本数据文件 -region 地域csv文件 [-dst 生成的ip2region.db文件的目录]

# 文本数据文件：db文件的原始文本数据文件路径，自带的ip2region.db文件就是/data/ip.merge.txt生成而来的，你可以换成自己的或者更改/data/ip.merge.txt重新生成
# 地域csv文件：该文件目的是方便配置ip2region进行数据关系的存储，得到的数据包含一个city_id，这个直接使用/data/origin/global_region.csv文件即可
# ip2region.db文件的目录：是可选参数，没有指定的话会在当前目录生成一份./data/ip2region.db文件
```

3. 获取生成的ip2region.db文件覆盖原来的ip2region.db文件即可
4. 默认的ip2region.db文件生成命令:

```shell
cd ${ip2region_root}/java/
java -jar dbMaker-1.2.2.jar -src ./data/ip.merge.txt -region ./data/global_region.csv

# 会看到一大片的输出
```




# 相关备注

### 声明 
ip2region重点在于<b>研究IP数据的存储设计和各种语言的查询实现</b>，并没有原始IP数据的支撑，数据来源请参考上面的描述，升级数据需要很多IP的支撑并且会对原始平台造成一定量的请求压力，本项目不保证及时的数据更新，没有也不会有商用版本，你可以使用自定义的数据导入ip2region进行自定义查询的实现。

### 技术交流
1. 数据库文件的结构和原理请阅读 @冬芽 的blog：[“ip2region数据库文件的结构和原理”](https://github.com/dongyado/dongyado.github.io/blob/master/_posts/2016-08-18-structure-of-ip2region-database-file.md)
2. ip2region交流分享，微信：lionsoul2014(请备注ip2region)，QQ：1187582057(很少关注)
