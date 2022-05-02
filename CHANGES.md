### 1.8
1. 数据升级至2018/12/31的版本，国外的也增加了大量的城市数据。
2. Java的binding的Searcher类增加了如下的接口和实现：
```java
DbSearcher(DbConfig dbConfig, string dbBinStr);
```

maven坐标：
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>1.7.2</version>
</dependency>
```

3. Javascript的pacakge上传到了npm。
4. 增加纯lua实现的lua binding (lua 5.2版本依赖bit模块，对5.3的支持将使用lua自带的bit操作)。
5. 增加C实现的lua_c binding (和c的性能等同，建议使用)。
6. 优化部分binding的demo运行和使用文档。
7. 修复python binding的benchmark的部分错误。


### 1.2.4
1, 花了近两周的时间重写了数据的升级算法，再次提高准确率，升级过程如下（此处应该有掌声）：

```shell
1. ip预分段，利于分布式同步升级
2. 分段同步升级，目前使用四个机器同时升级
3，自动验证数据查缺补全
4，数据合并
5，数据格式标准化，目前2个工作：
    1)，ipip.net的数据和淘宝数据保持区域名称统一，ipip.net的数据不带“省”和“市”关键字。
    2)，香港澳门台湾等国家信息修复（淘宝原始数据有问题），reported at http://git.oschina.net/lionsoul/ip2region/issues/21
6，重复ip段的合并得到data目录下的ip.merge.txt
7，生产data目录下的ip2region.db二进制数据库文件
```

2，修复原始数据关于“香港，台湾，澳门”国家信息错误的bug，reported at http://git.oschina.net/lionsoul/ip2region/issues/21
3，将数据升级至：2017/03/15同步版本
4，增加c_mmap查询客户端，C客户端查询文件读取使用内存映射加速，感谢[Leo Ma](http://git.oschina.net/begeekmyfriend)的贡献

备注：部分阿里云数据有问题，缺失的部分使用免费开放的ipip.net的数据代替了。

### 1.2.3
1. 优化升级算法，进行更深度的分裂，可以使准确率再度提高（同时数据的更新也花费了2天多的时间）。
2. 数据升级至2016/12/14版本的数据（包括原始数据和binary数据库的数据）。

### 1.2.2

1. ip数据文件更新至2016/11/02版本
2. 增加ip2region.db数据库文件的java生成工具，请参考README获取使用方式
3. 新增golang查询客户端的实现，感谢 [@mohong122](https://github.com/mohong122) 的关注和贡献

### 1.2

1. 新增分列式升级算法，大大的增加了数据的准确率，基本避免之前各大网友反馈的些许ip定位错误！
2. 数据升级是2016/06/30版！
3. 优化数据文件生成算法，ip2region.db文件由原来的3.5M降为1.5M，42亿个IP地址，皆大欢喜啊！
4. C/PHP/JAVA客户端增加纯内存搜索模式（python, php扩展，nodejs会在后续版本加上！）
说明：因为数据文件只有1.5M，对于PHP,java这类IO优化类语言提升速度不大，C有一个数量级的提升！
