:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region xdb java generation implementation

# Compilation and Installation

Compile the executable jar program via maven:

```bash
# cd to the maker/java root directory
mvn clean compile package
```

Then you will get an ip2region-maker-{version}.jar package file in the target directory of the current directory.

# Data Generation

Generate the xdb binary file via `java -jar ip2region-maker-{version}.jar`:

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

For example, generate an IPv4 ip2region_v4.xdb binary file in the current directory using the default data/ipv4_source.txt raw data:

```bash
java -jar target/ip2region-maker-3.0.0.jar --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb --version=ipv4
...
2025-09-13 00:33:06 INFO  org.lionsoul.ip2region.xdb.Maker write done, dataBlocks: 13827, indexBlocks: (683843, 720464), indexPtr: (955933, 11042415)
2025-09-13 00:33:06 INFO  org.lionsoul.ip2region.MakerApp Done, elapsed: 2 s
```

For example, generate an IPv6 ip2region_v6.xdb binary file in the current directory using the default data/ipv6_source.txt raw data:

```bash
java -jar target/ip2region-maker-3.0.0.jar --src=../../data/ipv6_source.txt --dst=./ip2region_v6.xdb --version=ipv6
...
2025-09-13 00:35:34 INFO  org.lionsoul.ip2region.xdb.Maker write done, dataBlocks: 120446, indexBlocks: (16789611, 16855074), indexPtr: (6585371, 647078145)
2025-09-13 00:35:34 INFO  org.lionsoul.ip2region.MakerApp Done, elapsed: 67 s
```

For custom data fields during the generation process, please refer to [xdb-文件生成#自定义数据字段](https://ip2region.net/doc/data/xdb_make#field-list)

# Data Search/bench Test

All [bindings](../../binding/) come with search and bench test programs as well as usage documentation. You can use the searcher of your familiar language for query testing or bench testing to confirm the correctness and integrity of the data.
