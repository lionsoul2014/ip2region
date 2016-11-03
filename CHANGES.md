### 1.2.2

1. ip数据文件更新至2016/11/02版本
2. 增加ip2region.db数据库文件的java生成工具，用法如下：

```shell
cd 项目根目录
java -jar dbMaker-{version}.jar -src 文本数据文件 -region 地域csv文件

# 文件数据文件：是db文件的原始数据，自带的ip2region.db文件就是/data/ip.merge.txt生成而来
# 地域csv文件：该文件目的是用于方便配置ip2region进行数据关系的存储，可以获取里面的city_id，这个直接使用/data/origin/global_region.csv文件即可
```

3. 新增golang查询客户端的实现，感谢 [@mohong122](https://github.com/mohong122) 的关注和贡献

### 1.2

1. 新增分列式升级算法，大大的增加了数据的准确率，基本避免之前各大网友反馈的些许ip定位错误！
2. 数据升级是2016/06/30版！
3. 优化数据文件生成算法，ip2region.db文件由原来的3.5M降为1.5M，42亿个IP地址，皆大欢喜啊！
4. C/PHP/JAVA客户端增加纯内存搜索模式（python, php扩展，nodejs会在后续版本加上！）
说明：因为数据文件只有1.5M，对于PHP,java这类IO优化类语言提升速度不大，C有一个数量级的提升！
