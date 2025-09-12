# ip2region xdb java 生成实现

# 编译安装
通过 maven 来编译可运行 jar 程序：
```bash
# cd 到 maker/java 根目录
mvn clean compile package
```

然会会在当前目录的 target 目录下得到一个 ip2region-maker-{version}.jar 的打包文件。

# 数据生成

通过 `java -jar ip2region-maker-{version}.jar` 来生成 xdb 二进制文件：
```bash
➜  java git:(master) java -jar target/ip2region-maker-3.0.0.jar 
ip2region xdb maker
java -jar ip2region-maker-{version}.jar [command options]
options:
 --src string           source ip text file path
 --dst string           destination binary xdb file path
 --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused
 --field-list string    field index list imploded with ',' eg: 0,1,2,3-6,7
 --log-level string     set the log level, options: debug/info/warn/error
```

例如，通过默认的 data/ipv4_source.txt 原始数据，在当前目录生成一个 IPv4 的 ip2region_v4.xdb 二进制文件：
```bash
java -jar target/ip2region-maker-3.0.0.jar --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb --version=ipv4
...
2025-09-13 00:33:06 INFO  org.lionsoul.ip2region.xdb.Maker write done, dataBlocks: 13827, indexBlocks: (683843, 720464), indexPtr: (955933, 11042415)
2025-09-13 00:33:06 INFO  org.lionsoul.ip2region.MakerApp Done, elapsed: 2 s
```

例如，通过默认的 data/ipv6_source.txt 有原始据，在当前目录生成一个 IPv6 的 ip2region_v6.xdb 二进制文件：
```bash
java -jar target/ip2region-maker-3.0.0.jar --src=../../data/ipv6_source.txt --dst=./ip2region_v6.xdb --version=ipv6
...
2025-09-13 00:35:34 INFO  org.lionsoul.ip2region.xdb.Maker write done, dataBlocks: 120446, indexBlocks: (16789611, 16855074), indexPtr: (6585371, 647078145)
2025-09-13 00:35:34 INFO  org.lionsoul.ip2region.MakerApp Done, elapsed: 67 s
```

生成过程中数据字段自定义请参考 [xdb-v4文件生成#自定义数据字段](https://ip2region.net/doc/data/ipv4_xdb_make#field-list)

# 数据 查询/bench 测试

已经完成开发的 [binding](../../binding/) 都有查询和 bench 测试程序以及使用文档，你可以使用你熟悉的语言的 searcher 进行查询测试或者bench测试，来确认数据的正确性和完整性。
