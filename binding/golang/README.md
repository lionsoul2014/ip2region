[中文](README_zh.md) | [English](README.md)

# ip2region golang query client implementation

# Usage

### package get

```bash
go get github.com/lionsoul2014/ip2region/binding/golang

```

### About the query service

Starting from version `3.11.0`, a dual-protocol compatible and concurrency-safe `Ip2Region` query service is provided. **It is recommended to prioritize this method for query calls.** The specific usage is as follows:

```go
import "github.com/lionsoul2014/ip2region/binding/golang/service"

// 1. Create v4 configuration: specify the cache policy and the v4 xdb file path
// Parameter 1: Cache policy, options: service.NoCache / service.VIndexCache / service.BufferCache
// Parameter 2: xdb file path
// Parameter 3: Number of initialized searchers
v4Config, err := service.NewV4Config(service.VIndexCache, "ip2region v4 xdb path", 20)
if err != nil {
    return fmt.Errorf("failed to create v4 config: %s", err)
}

// 2. Create v6 configuration: specify the cache policy and the v6 xdb file path
v6Config, err := service.NewV6Config(service.VIndexCache, "ip2region v6 xdb path", 20)
if err != nil {
    return fmt.Errorf("failed to create v6 config: %s", err)
}

// 3. Create the Ip2Region query service using the above configurations
ip2region, err := service.NewIp2Region(v4Config, v6Config)
if err != nil {
    return fmt.Errorf("failed to create ip2region service: %s", err)
}

// 4. Export the ip2region service for concurrent dual-version IP address queries, for example:
v4Region, err := ip2region.SearchByStr("113.92.157.29")                          // Perform IPv4 query
v6Region, err := ip2region.SearchByStr("240e:3b7:3272:d8d0:db09:c067:8d59:539e") // Perform IPv6 query


// 5. When the parent service needs to be closed, close the ip2region query service as well
ip2region.Close()
```

##### `Ip2Region` Query Notes:

1. The API of this query service is concurrency-safe and supports both IPv4 and IPv6 addresses; the internal implementation handles identification automatically.
2. v4 and v6 configurations need to be created separately. You can set different cache policies for v4 and v6, or specify one as `nil`, which will cause queries for that IP version to return `""`.
3. Please set an appropriate number of searchers based on your project's concurrency. This value is fixed during runtime; each query borrows a searcher from the pool to complete the operation and returns it afterward. If the pool is empty during borrowing, it will wait until a searcher becomes available.
4. If the cache policy is set to `service.BufferCache` (Full Memory Cache), a single-instance memory searcher is used by default. This implementation is natively concurrency-safe, and the specified number of searchers will be ignored.
5. If `Close` is called while the `Ip2Region` service is running, it will wait up to 10 seconds by default for searchers to be returned. You can also call `CloseTimeout` to define a custom maximum wait time.

### About the Query API

The location information query API prototypes are:

```go
SearchByStr(string) (string, error)
Search([]byte) (string, error)
```

If a query fails, the `error` will contain specific error details. If successful, it returns the `region` string. If the specified IP cannot be found, it returns an empty string `""`.

### About IPv4 / IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. Usage is as follows:

```go
// For IPv4: Set the xdb path to the v4 xdb file and specify the IP version as xdb.IPv4
dbPath := "../../data/ip2region_v4.xdb"  // Or your ipv4 xdb path
version := xdb.IPv4

// For IPv6: Set the xdb path to the v6 xdb file and specify the IP version as xdb.IPv6
dbPath = "../../data/ip2region_v6.xdb"  // Or your ipv6 xdb path
version = xdb.IPv6

// The IP version of the xdb specified by dbPath must match the version specified, otherwise the query will throw an error.
// Note: The following demonstrations directly use the dbPath and version variables.
```

### File Verification

It is recommended to actively verify the applicability of the xdb file. New features in later versions may make the current Searcher version incompatible with your xdb file. Verification helps avoid unpredictable errors during runtime.
You do not need to verify every time; for example, run it during service startup or manually via command line to confirm version matching. Do not run verification every time a Searcher is created, as this will impact query response speed, especially in high-concurrency scenarios.

```go
err := xdb.VerifyFromFile(dbPath)
if err != nil {
	// err contains the verification error
	return fmt.Errorf("xdb file verify: %w", err)
}

// The current Searcher can safely be used for query operations on the xdb specified by dbPath.
```

### Pure File-Based Query

```go
import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
    "time"
)

func main() {
	// Create a pure file-based query object using version and dbPath
    searcher, err := xdb.NewWithFileOnly(version, dbPath)
    if err != nil {
        fmt.Printf("failed to create searcher: %s\n", err.Error())
        return
    }

    defer searcher.Close()

    // Location info query: both IPv4 and IPv6 addresses are supported
    var ip = "1.2.3.4"  // IPv4
	// ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" // IPv6
    var tStart = time.Now()
    region, err := searcher.SearchByStr(ip)
    if err != nil {
        fmt.Printf("failed to SearchIP(%s): %s\n", ip, err)
        return
    }

	// IPv4 or IPv6 location information 
    fmt.Printf("{region: %s, took: %s}\n", region, time.Since(tStart))

    // Note: For concurrent use, each goroutine needs to create an independent searcher object.
}
```

### Caching `VectorIndex`

You can pre-load the `vectorIndex` cache and store it as a global variable. Using the global `vectorIndex` whenever creating a searcher reduces a fixed IO operation, accelerating queries and reducing system IO pressure.

```go
// 1. Load VectorIndex cache from dbPath and store the vIndex variable globally in memory.
vIndex, err := xdb.LoadVectorIndexFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load vector index from `%s`: %s\n", dbPath, err)
    return
}

// 2. Create a query object with VectorIndex cache using the global vIndex.
searcher, err := xdb.NewWithVectorIndex(version, dbPath, vIndex)
if err != nil {
    fmt.Printf("failed to create searcher with vector index: %s\n", err)
    return
}

// Note: For concurrent use, all goroutines share the global read-only vIndex cache, while each goroutine creates an independent searcher object.
```

### Caching the entire `xdb` file

You can pre-load the entire xdb file into memory for full memory-based queries, similar to the previous memory search.

```go
// 1. Load the entire xdb from dbPath into memory
cBuff, err := xdb.LoadContentFromFile(dbPath)
if err != nil {
    fmt.Printf("failed to load content from `%s`: %s\n", dbPath, err)
    return
}

// 2. Create a fully memory-based query object using the global cBuff.
searcher, err := xdb.NewWithBuffer(version, cBuff)
if err != nil {
    fmt.Printf("failed to create searcher with content: %s\n", err)
    return
}

// Note: For concurrent use, searcher objects created with the entire xdb cache can be safely used for concurrency.
```

# Compile the test program

Compile to get the xdb_searcher executable through the following method:

```bash
# cd to the golang binding root directory first
make
```

# Query test

### Query command

Test xdb queries using the `./xdb_searcher search` command:

```bash
➜  golang git:(master) ✗ ./xdb_searcher search --help
./xdb_searcher search [command options]
options:
 --v4-db string            ip2region v4 binary xdb file path
 --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content
 --v6-db string            ip2region v6 binary xdb file path
 --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content
 --help                    print this help menu
```

### Parameter parsing

1. `v4-xdb`: IPv4 xdb file path, defaults to data/ip2region_v4.xdb in the repository
2. `v6-xdb`: IPv6 xdb file path, defaults to data/ip2region_v6.xdb in the repository
3. `v4-cache-policy`: Cache policy used for v4 queries, defaults to `vectorIndex`, options: file/vectorIndex/content
4. `v6-cache-policy`: Cache policy used for v6 queries, defaults to `vectorIndex`, options: file/vectorIndex/content

### Test Demo

Example: Perform query tests using the default data/ip2region_v4.xdb and data/ip2region_v6.xdb:

```bash
➜  golang git:(master) ✗ ./xdb_searcher search       
ip2region search service test program
+-v4 db: /data01/code/c/ip2region/data/ip2region_v4.xdb (vectorIndex)
+-v6 db: /data01/code/c/ip2region/data/ip2region_v6.xdb (vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, took: 50.216µs}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, took: 100.606µs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, took: 99.078µs}
```

Enter an IPv4 or IPv6 address to perform query tests. You can also set `cache-policy` to file/vectorIndex/content respectively to test the performance of the three different cache implementations.

# bench test

### Test command

Perform automatic bench testing using the `xdb_searcher bench` command. This ensures that both the program and the `xdb` file are free of errors, and provides average query performance through a large number of queries:

```bash
➜  golang git:(fr_xdb_ipv6) ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

### v4 bench

Example: Perform ipv4 bench testing using data/ip2region_v4.xdb and data/ipv4_source.txt:

```bash
./xdb_searcher bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt 
```

### v6 bench

Example: Perform ipv6 bench testing using data/ip2region_v6.xdb and data/ipv6_source.txt:

```bash
./xdb_searcher bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt 
```

You can set the `cache-policy` parameter to test the efficiency of file/vectorIndex/content cache mechanisms respectively.

*Please note that the src file used for bench needs to be the same source file used to generate the corresponding xdb file*.

The bench program will read the source IP file specified by `src` line by line, then select the start and end IPs from each IP segment for testing to ensure that the queried region information matches the original region information. There is no debug information output during the test; if an error occurs, the error message will be printed and execution will terminate. Seeing `Bench finished` indicates the bench was successful. Cost represents the average time for each query operation (ns).
