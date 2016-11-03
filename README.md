ip2region - 最自由的ip地址查询库，ip到地区的映射库，提供Binary,B树和纯内存三种查询算法，妈妈再也不用担心我的ip地址定位。

**1. 99.9%准确率，定时更新：**

数据聚合了一些知名ip到地名查询提供商的数据，这些是他们官方的的准确率，经测试着实比纯真啥的准确多了。<br />
每次聚合一下数据需要1-2天，会不定时更新。

**2. 标准化的数据格式：**

每条ip数据段都固定了格式：_城市Id|国家|区域|省份|城市|ISP_

只有中国的数据精确到了城市，其他国家只能定位到国家，后前的选项全部是0，已经包含了全部你能查到的大大小小的国家。
（请忽略前面的城市Id，个人项目需求）

**3. 体积小：**

生成的数据库文件ip2region.db只有1.5M（1.2版本前是3.5M）

**4. 多查询客户端的支持，0.0x毫秒级别的查询**

已经集成的客户端有：java, php, c, python，nodejs，php扩展(支持linux, php5, php7版本已支持)，golang。

    提供了两种查询算法，响应时间如下：
    客户端/binary算法/b-tree算法/Memory算法：
    java/0.x毫秒/0.x毫秒/0.1x毫秒 (使用RandomAccessFile)
    php/0.x毫秒/0.1x毫秒/0.1x毫秒
    c/0.0x毫秒/0.0x毫秒/0.00x毫秒(b-tree算法基本稳定在0.02x毫秒级别)
    python/0.x毫秒/0.1x毫秒/未知

任何客户端b-tree都比binary算法快，当然Memory算法固然是最快的！

**5. 测试程序：**
java: 

	cd binding/java
	ant all
	java -jar ip2region-{version}.jar ./data/ip2region.db

php: 

    php binding/php/testSearcher ./data/ip2region.db

c: 

    cd binding/c/
    gcc -g -O2 testSearcher.c ip2region.c
    ./a.out ../../data/ip2region.db

python: 

    python binding/python/testSearcher ./data/ip2region.db

均会看到如下界面：

    initializing  B-tree ... 
    +----------------------------------+
    | ip2region test script            |
    | Author: chenxin619315@gmail.com  |
    | Type 'quit' to exit program      |
    +----------------------------------+
    p2region>> 101.105.35.57
    2163|中国|华南|广东省|深圳市|鹏博士 in 0.02295 millseconds

输入ip地址开始测试，第一次会稍微有点慢，在运行命令后面接入binary,memory来尝试其他算法，建议使用b-tree算法，速度和并发需求的可以使用memory算法。

具体集成请参考不同客户端的测试源码。

**6. 如何生成ip2region.db文件**

从ip2region 1.2.2版本开始里面提交了一个dbMaker-{version}.jar的可以执行jar文件，用它来完成这个工作：
* 1, 确保你安装好了java环境（不玩Java的童鞋就自己谷歌找找拉，临时用一用，几分钟的事情）
* 2, cd到ip2region的根目录，然后运行如下命令：

```shell
java -jar dbMaker-{version}.jar -src 文本数据文件 -region 地域csv文件 [-dst 生成的ip2region.db文件的目录]

# 文本数据文件：db文件的原始文本数据文件路径，自带的ip2region.db文件就是/data/ip.merge.txt生成而来的，你可以换成自己的或者更改/data/ip.merge.txt重新生成
# 地域csv文件：该文件目的是方便配置ip2region进行数据关系的存储，得到的数据包含一个city_id，这个直接使用/data/origin/global_region.csv文件即可
# ip2region.db文件的目录：是可选参数，没有指定的话会在当前目录生成一份./data/ip2region.db文件
```

* 3, 获取生成的ip2region.db文件覆盖原来的ip2region.db文件即可
* 4, 默认的ip2region.db文件生成命令:

```shell
cd ip2region项目根目录
java -jar dbMaker-1.2.2.jar -src ./data/ip.merge.txt ./data/global_region.csv

# 会看到一大片的输出
```

* 5, 数据库文件的结构和原理请阅读 @冬芽 的blog：[“ip2region数据库文件的结构和原理”](http://dongyado.com/tools/2016/08/18/structure-of-ip2region-database-file/)
