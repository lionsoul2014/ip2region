:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region java Query Client

# Usage

### Maven Repository:

```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>3.3.5</version>
</dependency>
```

### About Query Service

Starting from version `3.2.0`, a dual-protocol compatible and concurrency-safe `Ip2Region` query service is provided. **It is recommended to prioritize this method for query calls.** The specific usage is as follows:

```java
import org.lionsoul.ip2region.service.Config;
import org.lionsoul.ip2region.service.Ip2Region;

// 1. Create v4 configuration: specify cache policy and v4 xdb file path
final Config v4Config = Config.custom()
    .setCachePolicy(Config.VIndexCache)     // Specify cache policy: NoCache / VIndexCache / BufferCache
    .setSearchers(15)                       // Set the number of initialized searchers
    // .setCacheSliceBytes(int)             // Set cache slice bytes, default is 50MiB
    // .setXdbInputStream(InputStream)      // Set v4 xdb file inputstream object
    // .setXdbFile(File)                    // Set v4 xdb File object
    .setXdbPath("ip2region v4 xdb path")    // Set the path of v4 xdb file
    .asV4();    // Specify as v4 configuration

// 2. Create v6 configuration: specify cache policy and v6 xdb file path
final Config v6Config = Config.custom()
    .setCachePolicy(Config.VIndexCache)     // Specify cache policy: NoCache / VIndexCache / BufferCache
    .setSearchers(15)                       // Set the number of initialized searchers
    // .setCacheSliceBytes(int)             // Set cache slice bytes, default is 50MiB
    // .setXdbInputStream(InputStream)      // Set v6 xdb file inputstream object
    // .setXdbFile(File)                    // Set v6 xdb File object
    .setXdbPath("ip2region v6 xdb path")    // Set the path of v6 xdb file
    .asV6();    // Specify as v6 configuration

// Note: Priority for the three types of Xdb initialization inputs: XdbInputStream -> XdbFile -> XdbPath
// setXdbInputStream is only for the convenience of users to load xdb file content from jar packages, in which case cachePolicy can only be set to Config.BufferCache

// 3. Create Ip2Region query service through the above configurations
final Ip2Region ip2Region = Ip2Region.create(v4Config, v6Config);

// 4. Export the ip2region service as a global variable to perform concurrent queries for both versions of IP addresses, for example:
final String v4Region = ip2Region.search("113.92.157.29");                          // Perform IPv4 query
final String v6Region = ip2Region.search("240e:3b7:3272:d8d0:db09:c067:8d59:539e"); // Perform IPv6 query

// 5. When the service needs to be shut down, close the ip2region query service at the same time
// Note: The close method only needs to be called when the entire service is shut down; no operation is needed during queries
ip2Region.close();
```

##### `Ip2Region` Query Notes:

1. The API of this query service is concurrency-safe and supports both `IPv4` and `IPv6` addresses; the internal implementation will automatically distinguish them.
2. v4 and v6 configurations need to be created separately. You can set different cache policies for v4 and v6, or specify one of them as `null`, in which case IP address queries for that version will return `null`.
3. Please set a suitable number of searchers for `setSearchers` based on your project's concurrency. The default is 20. This value is fixed during runtime. Each query will borrow a searcher from the pool and return it after the query is completed. If the pool is empty when borrowing, it will wait until a searcher becomes available. The borrow lock is managed using `ReentrantLock`. You can also set the `Ip2Region` query service to use a fair lock as follows:

```java
final Ip2Region ip2region = Ip2Region.create(v4Config, v6Config, true);
```

4. If the cache policy in the configuration is set to `Config.BufferCache` (i.e., `Full Memory Cache`), a single-instance memory searcher will be used by default. This implementation is natively concurrency-safe, and the number of searchers specified via `setSearchers` will be ignored.
5. If `close` is called while the `ip2region` searcher is providing service, it will wait for a maximum of 10 seconds by default to allow as many searchers as possible to be returned.

### About Query API

The prototype of the location information query API is:

```java
String search(String ipStr) throw Exception;
String search(byte[] ip) throw Exception;
```

An exception will be thrown if the query fails. If the query is successful, the `region` information string will be returned. If the specified IP cannot be found, an empty string `""` will be returned, which may occur for custom data or incomplete data.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. The usage is as follows:

```java
import org.lionsoul.ip2region.xdb.Version;

// For IPv4: Set xdb path to the v4 xdb file, specify IP version as Version.IPv4
final String dbPath = "../../data/ip2region_v4.xdb";  // or your ipv4 xdb path
final Version version = Version.IPv4;

// For IPv6: Set xdb path to the v6 xdb file, specify IP version as Version.IPv6
final String dbPath = "../../data/ip2region_v6.xdb";  // or your ipv6 xdb path
final Version version = Version.IPv6;

// The IP version of the xdb specified by dbPath must be consistent with version, otherwise an error will occur during query execution
// Note: The following demonstration directly uses the dbPath and version variables
```

### File Verification

It is recommended that you proactively verify the applicability of the xdb file, as some future new features may cause the current Searcher version to be incompatible with the xdb file you are using. Verification can avoid unpredictable errors during runtime. You do not need to verify every time; for example, verify when the service starts or manually call a command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```java
try {
    Searcher.verifyFromFile(dbPath);
} catch (Exception e) {
    // Applicability verification failed!!!
    // The current query client implementation is not suitable for querying the xdb file specified by dbPath.
    // You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation compatible with dbPath.
    return;
}

// Verification passed, the current Searcher can be safely used for query operations on the xdb pointed to by dbPath
```

### File-Based Query

```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // 1. Create a searcher object using the version and dbPath mentioned above
        Searcher searcher = null;
        try {
            searcher = Searcher.newWithFileOnly(version, dbPath);
        } catch (IOException e) {
            System.out.printf("failed to create searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 2. Query, both IPv4 and IPv6 addresses are supported
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

        // 3. Close resources
        searcher.close();
        
        // Note: For concurrent use, each thread needs to create an independent searcher object for separate use.
    }
}
```

### Caching `VectorIndex`

We can pre-load `VectorIndex` data from the `xdb` file and cache it globally. Using the global VectorIndex cache every time a Searcher object is created can reduce a fixed IO operation, thereby accelerating queries and reducing IO pressure.

```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // Note: For version and dbPath sources, please see the version description above

        // 1. Pre-load VectorIndex cache from dbPath and use the obtained data as a global variable for subsequent repeated use.
        byte[] vIndex;
        try {
            vIndex = Searcher.loadVectorIndexFromFile(dbPath);
        } catch (Exception e) {
            System.out.printf("failed to load vector index from `%s`: %s\n", dbPath, e);
            return;
        }

        // 2. Use the global vIndex to create a query object with VectorIndex cache.
        Searcher searcher;
        try {
            searcher = Searcher.newWithVectorIndex(version, dbPath, vIndex);
        } catch (Exception e) {
            System.out.printf("failed to create vectorIndex cached searcher with `%s`: %s\n", dbPath, e);
            return;
        }

        // 3. Query, both IPv4 and IPv6 addresses are supported
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
        
        // 4. Close resources
        searcher.close();

        // Note: Each thread needs to create an independent Searcher object, but they all share the same read-only global vIndex cache.
    }
}
```

### Caching the Entire `xdb` File

We can also pre-load the entire xdb file data into memory and then create a query object based on this data to achieve a fully memory-based query, similar to the previous memory search.

```java
import org.lionsoul.ip2region.xdb.Searcher;
import java.io.*;
import java.util.concurrent.TimeUnit;

public class SearcherTest {
    public static void main(String[] args) {
        // Note: For version and dbPath sources, please see the version description above

        // 1. Load the entire xdb from dbPath into memory.
        // Starting from this release version, the xdb buffer uses LongByteArray for storage to avoid int type overflow when the xdb file is too large
        LongByteArray cBuff;
        try {
            cBuff = Searcher.loadContentFromFile(dbPath);
        } catch (Exception e) {
            System.out.printf("failed to load content from `%s`: %s\n", dbPath, e);
            return;
        }

        // 2. Use the above cBuff to create a fully memory-based query object.
        Searcher searcher;
        try {
            searcher = Searcher.newWithBuffer(version, cBuff);
        } catch (Exception e) {
            System.out.printf("failed to create content cached searcher: %s\n", e);
            return;
        }

        // 3. Query, both IPv4 and IPv6 are supported
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
        
        // 4. Close resources - this searcher object can be safely used for concurrency; close it when the entire service is shut down
        // searcher.close();

        // Note: For concurrent use, the query object created with the entire xdb data cache can be safely used for concurrency, 
        // meaning you can make this searcher object a global object for cross-thread access.
    }
}
```

If an OOM error occurs while calling the `loadContentXXX` method to load the xdb buffer, please refer to the [sliceBytes setting](#slicebytes) below and choose the `loadContentXXX` method with the sliceBytes parameter.

### sliceBytes

sliceBytes represents the size of the partitioned memory for the `List<byte[]> buffs` collection maintained inside the `LongByteArray` class during full memory caching. The default value is `Searcher.DEFAULT_SLICE_BYTES` = `50MiB`. The maximum allowed value is `Searcher.MAX_WRITE_BYTES` = `0x7ffff000`. For the source of this value, please refer to the author's blog post: [https://mp.weixin.qq.com/s/4xHRcnQbIcjtMGdXEGrxsA](https://mp.weixin.qq.com/s/4xHRcnQbIcjtMGdXEGrxsA).

1. Starting from version `3.3.3`, `LongByteArray` implements fixed partition size support, which allows for fast `offset` positioning through simple calculation to perform `slice` or `copy` operations.
2. In terms of calculation speed, the larger the sliceBytes, the smaller the length of buffs and the lower the calculation time. However, with the fixed sliceBytes implementation, this gap is completely negligible. Therefore, it is recommended to keep the default value of `50MiB`, which also avoids the OOM issues that could be caused by elastic partition sizes previously.

# Compiling the Test Program

Compile the test program via Maven.

```bash
# cd to the root directory of java binding
cd binding/java/
mvn compile package
```

Then a packaged file named ip2region-{version}.jar will be generated in the target directory under the current folder.

# Query Testing

### Test Command

You can test queries via the `java -jar target/ip2region-{version}.jar search` command:

```bash
➜  java git:(master) ✗ java -jar target/ip2region-3.3.4.jar search --help
java -jar ip2region-{version}.jar search [command options]
options:
 --v4-db string            ip2region ipv4 binary xdb file path
 --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content
 --v6-db string            ip2region ipv6 binary xdb file path
 --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content
 --help                    print this help menu
```

### Parameter Parsing

1. `v4-xdb`: IPv4 xdb file path, defaults to data/ip2region_v4.xdb in the repository.
2. `v6-xdb`: IPv6 xdb file path, defaults to data/ip2region_v6.xdb in the repository.
3. `v4-cache-policy`: Cache policy used for v4 queries, default is `vectorIndex`, options: file/vectorIndex/content.
4. `v6-cache-policy`: Cache policy used for v6 queries, default is `vectorIndex`, options: file/vectorIndex/content.

### Test Demo

Example: performing query testing using default data/ip2region_v4.xdb and data/ip2region_v6.xdb:

```bash
➜  java git:(java_app_with_ip2region_service) ✗ java -jar target/ip2region-3.3.4.jar search
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

Enter a v4 or v6 IP address to perform a query test. You can also set `cache-policy` to file/vectorIndex/content respectively to test the effects of the three different cache implementations.

# bench Testing

### Test Command

You can perform bench testing via the `java -jar ip2region-{version}.jar bench` command to ensure the `xdb` file is error-free and to evaluate query performance:

```bash
➜  java git:(fr_java_ipv6) ✗ java -jar target/ip2region-3.3.4.jar bench
java -jar ip2region-{version}.jar bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

### v4 bench

Example: IPv4 bench testing using default data/ip2region_v4.xdb and data/ipv4_source.txt files:

```bash
java -jar target/ip2region-3.3.4.jar bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

### v6 bench

Example: IPv6 bench testing using default data/ip2region_v6.xdb and data/ipv6_source.txt files:

```bash
java -jar target/ip2region-3.3.4.jar bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

You can test the effects of the three different cache implementations by setting `cache-policy` to file/vectorIndex/content.
@Note: Please ensure that the src file used for benching is the same source file used to generate the corresponding xdb file.
