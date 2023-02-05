# ip2region xdb java 查询客户端实现

# 使用方式

### maven 仓库：
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>2.7.0</version>
</dependency>
```

### 完全基于文件的查询

```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // 1、创建 searcher 对象
        String dbPath = "ip2region.xdb file path";
        Searcher searcher = null;
        try {
            searcher = Searcher.newWithFileOnly(dbPath);
        } catch (IOException e) {
            System.out.printf("failed to create searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 2、查询
        try {
            String ip = "1.2.3.4";
            long sTime = System.nanoTime();
            String region = searcher.search(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
            System.out.printf("{region: %s, ioCount: %d, took: %d μs}\n", region, searcher.getIOCount(), cost);
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }

        // 3、关闭资源
        searcher.close();
        
        // 备注：并发使用，每个线程需要创建一个独立的 searcher 对象单独使用。
    }
}
```

### 缓存 `VectorIndex` 索引

我们可以提前从 `xdb` 文件中加载出来 `VectorIndex` 数据，然后全局缓存，每次创建 Searcher 对象的时候使用全局的 VectorIndex 缓存可以减少一次固定的 IO 操作，从而加速查询，减少 IO 压力。
```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        String dbPath = "ip2region.xdb file path";

        // 1、从 dbPath 中预先加载 VectorIndex 缓存，并且把这个得到的数据作为全局变量，后续反复使用。
        byte[] vIndex;
        try {
            vIndex = Searcher.loadVectorIndexFromFile(dbPath);
        } catch (Exception e) {
            System.out.printf("failed to load vector index from `%s`: %s\n", dbPath, e);
            return;
        }

        // 2、使用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
        Searcher searcher;
        try {
            searcher = Searcher.newWithVectorIndex(dbPath, vIndex);
        } catch (Exception e) {
            System.out.printf("failed to create vectorIndex cached searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 3、查询
        try {
            String ip = "1.2.3.4";
            long sTime = System.nanoTime();
            String region = searcher.search(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
            System.out.printf("{region: %s, ioCount: %d, took: %d μs}\n", region, searcher.getIOCount(), cost);
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }
        
        // 4、关闭资源
        searcher.close();

        // 备注：每个线程需要单独创建一个独立的 Searcher 对象，但是都共享全局的制度 vIndex 缓存。
    }
}
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 ip2region.xdb 的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。
```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        String dbPath = "ip2region.xdb file path";

        // 1、从 dbPath 加载整个 xdb 到内存。
        byte[] cBuff;
        try {
            cBuff = Searcher.loadContentFromFile(dbPath);
        } catch (Exception e) {
            System.out.printf("failed to load content from `%s`: %s\n", dbPath, e);
            return;
        }

        // 2、使用上述的 cBuff 创建一个完全基于内存的查询对象。
        Searcher searcher;
        try {
            searcher = Searcher.newWithBuffer(cBuff);
        } catch (Exception e) {
            System.out.printf("failed to create content cached searcher: %s\n", e);
            return;
        }

        // 3、查询
        try {
            String ip = "1.2.3.4";
            long sTime = System.nanoTime();
            String region = searcher.search(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
            System.out.printf("{region: %s, ioCount: %d, took: %d μs}\n", region, searcher.getIOCount(), cost);
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }
        
        // 4、关闭资源 - 该 searcher 对象可以安全用于并发，等整个服务关闭的时候再关闭 searcher
        // searcher.close();

        // 备注：并发使用，用整个 xdb 数据缓存创建的查询对象可以安全的用于并发，也就是你可以把这个 searcher 对象做成全局对象去跨线程访问。
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
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar search --db=../../data/ip2region.xdb
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, ioCount: 7, took: 82 μs}
ip2region>>
```

输入 ip 即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。


# bench 测试

可以通过 `java -jar ip2region-{version}.jar bench` 命令来进行 bench 测试，一方面确保 `xdb` 文件没有错误，一方面可以评估查询性能：
```bash
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar bench
java -jar ip2region-{version}.jar bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 文件进行 bench 测试：
```bash
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt
Bench finished, {cachePolicy: vectorIndex, total: 3417955, took: 8s, cost: 2 μs/op}
```

可以通过分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效果。
@Note: 注意 bench 使用的 src 文件要是生成对应 xdb 文件相同的源文件。
