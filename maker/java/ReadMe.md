# ip2region xdb java 生成实现

# 编译安装
通过 maven 来编译可运行 jar 程序：
```bash
# cd 到 maker/java 根目录
mvn clean compile package
```

然会会在当前目录的 target 目录下得到一个 ip2region-maker-{version}.jar 的打包文件。

# 数据生成

通过 `java -jar ip2region-maker-{version}.jar` 来生成 ip2region.xdb 二进制文件：
```bash
➜  java git:(java_xdb_maker) ✗ java -jar ./target/ip2region-maker-1.0.0.jar 
ip2region xdb maker
java -jar ip2region-maker-{version}.jar [command options]
options:
 --src string    source ip text file path
 --dst string    destination binary xdb file path
```

例如，通过默认的 data/ip.merge.txt 原数据，在当前目录生成一个 ip2region.xdb 二进制文件：
```bash
➜  java git:(java_xdb_maker) ✗ java -jar ./target/ip2region-maker-1.0.0.jar --src=../../data/ip.merge.txt --dst=./ip2region.xdb
# 会看到一堆输出，最终会看到如下输出表示运行成功
...
2022-07-15 20:21:29 INFO  org.lionsoul.ip2region.xdb.Maker try to write the vector index block ...
2022-07-15 20:21:29 INFO  org.lionsoul.ip2region.xdb.Maker try to write the segment index ptr ...
2022-07-15 20:21:29 INFO  org.lionsoul.ip2region.xdb.Maker write done, dataBlocks: 13804, indexBlocks: (683591, 720221), indexPtr: (982904, 11065984)
2022-07-15 20:21:29 INFO  org.lionsoul.ip2region.MakerTest Done, elapsed: 50 s
```

# 数据 查询/bench 测试

已经完成开发的 [binding](../../binding/) 都有查询和 bench 测试程序以及使用文档，你可以使用你熟悉的语言的 searcher 进行查询测试或者bench测试，来确认数据的正确性和完整性。
