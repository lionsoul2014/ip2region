ip2region - 最自由的ip地址查询库，ip到地区的映射库，提供Binary,B树和纯内存三种查询算法，妈妈再也不用担心我的ip地址定位。

### 1. 99.9%准确率，定时更新：

数据聚合了一些知名ip到地名查询提供商的数据，这些是他们官方的的准确率，经测试着实比纯真啥的准确多了。<br />
每次聚合一下数据需要1-2天，会不定时更新。<br /><br />
ip2region的数据聚合自以下服务商的开放API或者数据(升级程序每秒请求次数2到4次): <br />
01, &gt;80%, 淘宝IP地址库, [http://ip.taobao.com/](http://ip.taobao.com/) <br />
02, ≈10%, GeoIP, [https://geoip.com/](https://geoip.com/) <br />
03, ≈2%, 纯真IP库, [http://www.cz88.net/](http://www.cz88.net/) <br />
<b>备注：</b>如果上述开放API或者数据都不给开发时ip2region将停止数据的更新服务，目前是2~3个月更新一次。


### 2. 标准化的数据格式：

每条ip数据段都固定了格式：_城市Id|国家|区域|省份|城市|ISP_

只有中国的数据精确到了城市，其他国家只能定位到国家，后前的选项全部是0，已经包含了全部你能查到的大大小小的国家。
（请忽略前面的城市Id，个人项目需求）

### 3. 体积小：

生成的数据库文件ip2region.db只有1.5M（1.2版本前是3.5M）

### 4. 多查询客户端的支持，0.0x毫秒级别的查询

已经集成的客户端有：java、C#、php、c、python、nodejs、php扩展(php5和php7)、golang、rust、lua、lua_c。

    提供了两种查询算法，响应时间如下：
    客户端/binary算法/b-tree算法/Memory算法：
    c#/0.x毫秒/0.x毫秒/0.x毫秒
    java/0.x毫秒/0.x毫秒/0.1x毫秒 (使用RandomAccessFile)
    php/0.x毫秒/0.1x毫秒/0.1x毫秒
    c/0.0x毫秒/0.0x毫秒/0.00x毫秒(b-tree算法基本稳定在0.02x毫秒级别)
    python/0.x毫秒/0.1x毫秒/未知
    lua/0.x毫秒/0.x毫秒/0.x毫秒
    lua_c/0.0x毫秒/0.0x毫秒/0.00x毫秒

任何客户端b-tree都比binary算法快，当然Memory算法固然是最快的！

maven仓库地址：
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>1.7.2</version>
</dependency>
```
nuget安装命令
```shell
Install-Package IP2Region
```

### 5. 测试程序：
C#、Node.js、lua_c的测试请具体参考文件夹中README.md 说明。

java: 
```shell
cd binding/java
ant all
java -jar ip2region-{version}.jar ./data/ip2region.db
```

php: 
```shell
php binding/php/testSearcher.php ./data/ip2region.db
```

c: 
```shell
cd binding/c/
gcc -g -O2 testSearcher.c ip2region.c
./a.out ../../data/ip2region.db
```

python: 
```shell
python binding/python/testSearcher.py ./data/ip2region.db
```

lua: 
```shell
cd binding/lua/
lua testSearcher.lua ../../data/ip2region.db
```

均会看到如下界面：
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

输入ip地址开始测试，第一次会稍微有点慢，在运行命令后面接入binary,memory来尝试其他算法，建议使用b-tree算法，速度和并发需求的可以使用memory算法。

具体集成请参考不同客户端的测试源码。


### 6. 如何生成ip2region.db文件

从1.8版本开始，ip2region开源了ip2region.db生成程序的java实现，提供了ant编译支持，编译后会得到以下提到的dbMaker-{version}.jar，对于需要研究生成程序的或者更改自定义生成配置的请参考${ip2region_root}/maker/java内的java源码。


从ip2region 1.2.2版本开始里面提交了一个dbMaker-{version}.jar的可以执行jar文件，用它来完成这个工作：
* 1, 确保你安装好了java环境（不玩Java的童鞋就自己谷歌找找拉，临时用一用，几分钟的事情）
* 2, cd到${ip2region_root}/maker/java，然后运行如下命令：

```shell
java -jar dbMaker-{version}.jar -src 文本数据文件 -region 地域csv文件 [-dst 生成的ip2region.db文件的目录]

# 文本数据文件：db文件的原始文本数据文件路径，自带的ip2region.db文件就是/data/ip.merge.txt生成而来的，你可以换成自己的或者更改/data/ip.merge.txt重新生成
# 地域csv文件：该文件目的是方便配置ip2region进行数据关系的存储，得到的数据包含一个city_id，这个直接使用/data/origin/global_region.csv文件即可
# ip2region.db文件的目录：是可选参数，没有指定的话会在当前目录生成一份./data/ip2region.db文件
```

* 3, 获取生成的ip2region.db文件覆盖原来的ip2region.db文件即可
* 4, 默认的ip2region.db文件生成命令:

```shell
cd ${ip2region_root}/java/
java -jar dbMaker-1.2.2.jar -src ./data/ip.merge.txt -region ./data/global_region.csv

# 会看到一大片的输出
```

* 5, 数据库文件的结构和原理请阅读 @冬芽 的blog：[“ip2region数据库文件的结构和原理”](http://dongyado.com/tool/2016/08/18/structure-of-ip2region-database-file/)


### 7. 其他备注
* 1, 全部binding的各个search接口都<b>不是</b>线程安全的实现，不同线程可以通过创建不同的查询对象来使用。
* 2, memorySearch接口，在发布对象前进行一次预查询，可以安全用于多线程环境。
* 3, ip2region交流分享，微信：lionsoul2014，QQ：1187582057
