# ip2region xdb java 查询客户端实现

# 使用方式

### maven 仓库：
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>3.3.4</version>
</dependency>
```

### 关于查询服务
从 `3.2.0` 版本开始提供了一个双协议兼容且并发安全的 `Ip2Region` 查询服务，**建议优先使用该方式来进行查询调用**，具体使用方式如下：
```java
import org.lionsoul.ip2region.service.Config;
import org.lionsoul.ip2region.service.Ip2Region;

// 1, 创建 v4 的配置：指定缓存策略和 v4 的 xdb 文件路径
final Config v4Config = Config.custom()
    .setCachePolicy(Config.VIndexCache)     // 指定缓存策略:  NoCache / VIndexCache / BufferCache
    .setSearchers(15)                       // 设置初始化的查询器数量
    // .setCacheSliceBytes(int)             // 设置缓存的分片字节数，默认为 50MiB
    // .setXdbInputStream(InputStream)      // 设置 v4 xdb 文件的 inputstream 对象
    // .setXdbFile(File)                    // 设置 v4 xdb File 对象
    .setXdbPath("ip2region v4 xdb path")    // 设置 v4 xdb 文件的路径
    .asV4();    // 指定为 v4 配置

// 2, 创建 v6 的配置：指定缓存策略和 v6 的 xdb 文件路径
final Config v6Config = Config.custom()
    .setCachePolicy(Config.VIndexCache)     // 指定缓存策略: NoCache / VIndexCache / BufferCache
    .setSearchers(15)                       // 设置初始化的查询器数量
    // .setCacheSliceBytes(int)             // 设置缓存的分片字节数，默认为 50MiB
    // .setXdbInputStream(InputStream)      // 设置 v6 xdb 文件的 inputstream 对象
    // .setXdbFile(File)                    // 设置 v6 xdb File 对象
    .setXdbPath("ip2region v6 xdb path")    // 设置 v6 xdb 文件的路径
    .asV6();    // 指定为 v6 配置

// 备注：Xdb 三种初始化输入的优先级：XdbInputStream -> XdbFile -> XdbPath
// setXdbInputStream 仅方便使用者从 jar 包中加载 xdb 文件内容，这时 cachePolicy 只能设置为 Config.BufferCache

// 3，通过上述配置创建 Ip2Region 查询服务
final Ip2Region ip2Region = Ip2Region.create(v4Config, v6Config);

// 4，导出 ip2region 服务作为全局变量，进行双版本的IP地址的并发查询，例如：
final String v4Region = ip2Region.search("113.92.157.29");                          // 进行 IPv4 查询
final String v6Region = ip2Region.search("240e:3b7:3272:d8d0:db09:c067:8d59:539e"); // 进行 IPv6 查询

// 5，在服务需要关闭的时候，同时关闭 ip2region 查询服务
// 备注：close 方法只需要在整个服务关闭的时候关闭，查询途中不需要操作
ip2Region.close();
```
##### `Ip2Region` 查询备注：
1. 该查询服务的 API 并发安全且同时支持 `IPv4` 和 `IPv6` 的地址，内部实现会自动判断。
2. v4 和 v6 的配置需要单独创建，可以给 v4 和 v6 设置使用不同的缓存策略，也可以指定其中一个为 `null` 则该版本的 IP 地址查询都会返回 `null`。
3. 请结合您项目的并发数给 `setSearchers` 一个合适的查询器数量，默认为 20 个，这个值在运行过程中是固定的，每次查询会从池子里租借一个查询器来完成查询操作，查询完成后再归还回去，如果租借的时候池子已经空了则等待直到有可用的查询器来完成查询服务，租借的锁是使用的 `ReentrantLock` 来管理，也可以通过如下方式来设置 `Ip2Region` 查询服务使用公平锁：
```java
final Ip2Region ip2region = Ip2Region.create(v4Config, v6Config, true);
```
4. 如果配置设置的缓存策略为 `Config.BufferCache` 即 `全内存缓存` 则默认会使用单实例的内存查询器，该实现天生并发安全，此时通过 `setSearchers` 指定的查询器数量无效。
5. 如果 `ip2region` 查询器在提供服务期间，调用 close 默认会最大等待 10 秒钟来等待尽量多的查询器归还。


### 关于查询 API
定位信息查询 API 的原型为：
```java
String search(String ipStr) throw Exception;
String search(byte[] ip) throw Exception;
```
查询出错会抛出异常，如果查询成功会返回字符串的 `region` 信息，如果指定的 ip 查询不到会返回空字符串 `""`，这对于自定义数据或者数据不完整的情况会出现。

### 关于 IPv4 和 IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```java
import org.lionsoul.ip2region.xdb.Version;

// 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 Version.IPv4
final String dbPath = "../../data/ip2region_v4.xdb";  // 或者你的 ipv4 xdb 的路径
final Version version = Version.IPv4;

// 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 Version.IPv6
final String dbPath = "../../data/ip2region_v6.xdb";  // 或者你的 ipv6 xdb 路径
final Version version = Version.IPv6;

// dbPath 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
// 备注：以下演示直接使用 dbPath 和 version 变量
```

### 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。 你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```java
try {
    Searcher.verifyFromFile(dbPath);
} catch (Exception e) {
    // 适用性验证失败！！！
    // 当前查询客户端实现不适用于 dbPath 指定的 xdb 文件的查询.
    // 应该停止启动服务，使用合适的 xdb 文件或者升级到适合 dbPath 的 Searcher 实现。
    return;
}

// 验证通过，当前使用的 Searcher 可以安全的用于对 dbPath 指向的 xdb 的查询操作
```

### 完全基于文件的查询

```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // 1、使用上述的 version 和 dbPath 创建 searcher 对象
        Searcher searcher = null;
        try {
            searcher = Searcher.newWithFileOnly(version, dbPath);
        } catch (IOException e) {
            System.out.printf("failed to create searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 2、查询，IPv4 或者 IPv6 的地址都支持
        try {
            String ip = "1.2.3.4";
            // ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
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
        // 备注：version 和 dbPath 来源，请看上面的版本描述

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
            searcher = Searcher.newWithVectorIndex(version, dbPath, vIndex);
        } catch (Exception e) {
            System.out.printf("failed to create vectorIndex cached searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 3、查询，IPv4 或者 IPv6 地址都支持
        try {
            String ip = "1.2.3.4";
            // ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
            long sTime = System.nanoTime();
            String region = searcher.search(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
            System.out.printf("{region: %s, ioCount: %d, took: %d μs}\n", region, searcher.getIOCount(), cost);
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }
        
        // 4、关闭资源
        searcher.close();

        // 备注：每个线程需要单独创建一个独立的 Searcher 对象，但是都共享全局的只读 vIndex 缓存。
    }
}
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 xdb 文件的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。
```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // 备注：version 和 dbPath 来源，请看上面的版本描述

        // 1、从 dbPath 加载整个 xdb 到内存。
        // 从这个 release 版本开始，xdb 的 buffer 使用 LongByteArray 来存储，避免 xdb 文件过大的时候 int 类型的溢出
        LongByteArray cBuff;
        try {
            cBuff = Searcher.loadContentFromFile(dbPath);
        } catch (Exception e) {
            System.out.printf("failed to load content from `%s`: %s\n", dbPath, e);
            return;
        }

        // 2、使用上述的 cBuff 创建一个完全基于内存的查询对象。
        Searcher searcher;
        try {
            searcher = Searcher.newWithBuffer(version, cBuff);
        } catch (Exception e) {
            System.out.printf("failed to create content cached searcher: %s\n", e);
            return;
        }

        // 3、查询，IPv4 和 IPv6 都支持
        try {
            String ip = "1.2.3.4";
            // ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
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

如果调用 `loadContentXXX` 方法来加载 xdb buffer 的过程中出现了 OOM 错误，请参考以下的 [sliceBytes 设置](#slicebytes)，选择使用带 sliceBytes 参数的 `loadContentXXX` 方法来加载 。

### sliceBytes

sliceBytes 表示 xdb 全内存缓存时 `LongByteArray` 类内部维护的 `List<byte[]> buffs` 集合的分片内存的大小，默认值为 `Searcher.DEFAULT_SLICE_BYTES` = `50MiB`，这个值的最大允许值为 `Searcher.MAX_WRITE_BYTES` = `0x7ffff000`，关于该取值的来源可以参考作者博客文章：[https://mp.weixin.qq.com/s/4xHRcnQbIcjtMGdXEGrxsA](https://mp.weixin.qq.com/s/4xHRcnQbIcjtMGdXEGrxsA)。
1. 从 `3.3.3` 版本开始 `LongByteArray` 实现了固定分片尺寸支持，可以通过简单的计算快速的完成 `offset` 定位的从而实现 `slice` 或者 `copy` 操作。
2. 从计算速度来说 sliceBytes 越大 buffs 的长度越小，计算耗时越小，不过有了固定 sliceBytes 实现这个差距完全可以忽略，所以建议保持默认值为 `50MiB` 即可，也不会出现之前弹性分片尺寸可能导致的 OOM 问题。



# 编译测试程序

通过 maven 来编译测试程序。
```bash
# cd 到 java binding 的根目录
cd binding/java/
mvn compile package
```

然后会在当前目录的 target 目录下得到一个 ip2region-{version}.jar 的打包文件。



# 查询测试

### 测试命令
可以通过 `java -jar target/ip2region-{version}.jar search` 命令来测试查询：
```bash
➜  java git:(master) ✗ java -jar target/ip2region-3.3.0.jar search --help
java -jar ip2region-{version}.jar search [command options]
options:
 --v4-db string            ip2region ipv4 binary xdb file path
 --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content
 --v6-db string            ip2region ipv6 binary xdb file path
 --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content
 --help                    print this help menu
```

### 参数解析
1. `v4-xdb`: IPv4 的 xdb 文件路径，默认为仓库中的 data/ip2region_v4.xdb
2. `v6-xdb`: IPv6 的 xdb 文件路径，默认为仓库中的 data/ip2region_v6.xdb
3. `v4-cache-policy`: v4 查询使用的缓存策略，默认为 `vectorIndex`，可选：file/vectorIndex/content
4. `v6-cache-policy`: v6 查询使用的缓存策略，默认为 `vectorIndex`，可选：file/vectorIndex/content

### 测试 Demo
例如：使用默认的 data/ip2region_v4.xdb 和 data/ip2region_v6.xdb 进行查询测试：
```bash
➜  java git:(java_app_with_ip2region_service) ✗ java -jar target/ip2region-3.3.0.jar search       
ip2region search service test program
+-v4 xdb: /data01/code/c/ip2region/data/ip2region_v4.xdb (vectorIndex)
+-v6 xdb: /data01/code/c/ip2region/data/ip2region_v6.xdb (vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, took: 140 μs}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, took: 391 μs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, took: 503 μs}
```
输入 v4 或者 v6 的 IP 地址即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。


# bench 测试

### 测试命令
可以通过 `java -jar ip2region-{version}.jar bench` 命令来进行 bench 测试，一方面确保 `xdb` 文件没有错误，一方面可以评估查询性能：
```bash
➜  java git:(fr_java_ipv6) ✗ java -jar target/ip2region-3.1.0.jar bench                                  
java -jar ip2region-{version}.jar bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

### v4 bench
例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 文件进行 IPv4 的 bench 测试：
```bash
java -jar target/ip2region-3.1.0.jar bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

### v6 bench
例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 文件进行 IPv6 的 bench 测试：
```bash
java -jar target/ip2region-3.1.0.jar bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

可以通过分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效果。
@Note: 注意 bench 使用的 src 文件要是生成对应 xdb 文件相同的源文件。
