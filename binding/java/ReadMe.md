# ip2region xdb java 查询客户端实现

# 使用方式

### maven 仓库：
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>2.6.5</version>
</dependency>
```

### Example

```java
package org.lionsoul.ip2region.example;

import org.lionsoul.ip2region.xdb.Searcher;

import java.util.concurrent.TimeUnit;

/**
 * @see org.lionsoul.ip2region.xdb.Searcher
 */
public class SearcherExample {

    public static void main(String[] args) throws Exception {
        String dbPath = "ip2region.xdb file path";

        // 基于文件查询，单次 search 需要文件 IO，非线程安全，性能最差
        try (Searcher fileSearcher = Searcher.newWithFileOnly(dbPath)) {
            searchExample(fileSearcher);
        }

        // 基于 VectorIndex 查询，单次 search 需要内存查询 + 文件 IO，非线程安全，性能次之
        try (Searcher indexSearcher = Searcher.newWithVectorIndex(dbPath)) {
            searchExample(indexSearcher);
        }

        // 推荐：基于 Buffer 查询，单次 search 全使用内存查询，线程安全，支持序列化，性能最佳
        // 可重写 loadFile 方法扩展从其他文件系统(hdfs/s3/oss...)装载 buffer 数据
        try (Searcher bufferSearcher = Searcher.newWithBuffer(dbPath)) {
            searchExample(bufferSearcher);
        }

        // 备注：并发查询时，对于非线程安全实现推荐为每个线程创建一个独立的 searcher 对象单独使用。
    }

    private static void searchExample(Searcher searcher) {
        String ip = "1.2.3.4";
        try {
            long sTime = System.nanoTime();
            Searcher.Region region = searcher.searchRegion(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros(System.nanoTime() - sTime);
            System.out.printf("{region: %s, searcher: %s, ioCount: %d, took: %d μs}\n",
                    region, searcher.getClass().getSimpleName(), region.getIoCount(), cost);
            System.out.println(region.toRegionMsg());
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }
    }
}

```


# 编译测试程序

通过 maven 来编译测试程序。
```bash
# cd 到 java binding 的根目录
cd binding/java/
mvn compile package
```

然后会在当前目录的 target 目录下得到一个 ip2region-{version}.jar 的打包文件。



# 查询测试

可以通过 `java -jar ip2region-{version}.jar search` 命令来测试查询：
```bash
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar search
java -jar ip2region-{version}.jar search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 文件进行查询测试：
```bash
$ java -jar target/ip2region-2.6.5.jar search --db=../../data/ip2region.xdb
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, ioCount: 7, took: 82 μs}
ip2region>>
```

输入 ip 即可进行查询测试，也可以分别设置 `--cache-policy` 来测试三种不同缓存实现的查询效果。


# bench 测试

可以通过 `java -jar ip2region-{version}.jar bench` 命令来进行 bench 测试，一方面确保 `xdb` 文件没有错误，一方面可以评估查询性能：
```bash
java -jar target/ip2region-2.6.5.jar bench

java -jar ip2region-{version}.jar bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/buffer
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 文件进行 bench 测试：
```bash
$ java -jar target/ip2region-2.6.5.jar bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=buffer

-- 笔者笔记本[mac 2.6 GHz 六核Intel Core i7]测试结果案例
Bench finished, {cachePolicy: buffer, total: 3417955, took: 2s, ioCount: 0, cost: 0 μs/op}
Bench finished, {cachePolicy: vectorIndex, total: 3417955, took: 28s, ioCount: 43465554, cost: 8 μs/op}
Bench finished, {cachePolicy: file, total: 3417955, took: 34s, ioCount: 43465554, cost: 9 μs/op}
```

可以通过分别设置 `--cache-policy` 来测试三种不同缓存实现的效果。
@Note: 注意 bench 使用的 src 文件要是生成对应 xdb 文件相同的源文件。
