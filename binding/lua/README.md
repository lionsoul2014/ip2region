:globe_with_meridians: Supported Languages: [中文简体](README_zh.md) | [English](README.md)

# ip2region lua query client

#### Note: Please prioritize the use of the lua_c extension query client, as its performance is much faster than the pure lua implementation!!!

# Version Compatibility

This implementation is compatible with lua `5.3` and `5.4`, and no longer provides compatibility for lower versions. If you need to use it under lower versions of Lua, please consider using the `[lua_c](../lua_c/)` extension.

# Usage

### About Query API

The prototype of the query API is as follows:

```lua
-- Query via IP string
search_by_string(ip_string) (region, error)
-- Query via bytes IP returned by parse_ip
search(ip_bytes) (region, error)
```

If the query fails, it returns a non-`nil` error string. If successful, it returns the `region` information as a string. If the IP address cannot be found, it returns an empty string `""`.

### About IPv4 and IPv6

```lua
local xdb = require("xdb_searcher")

-- For IPv4: Set xdb path to the v4 xdb file, specify IP version as Version.IPv4
local dbPath  = "../../data/ip2region_v4.xdb"  -- or your ipv4 xdb path
local version = xdb.IPv4

-- For IPv6: Set xdb path to the v6 xdb file, specify IP version as Version.IPv6
local dbPath  = "../../data/ip2region_v6.xdb"  -- or your ipv6 xdb path
local version = xdb.IPv6

-- The IP version of the xdb specified by dbPath must match the version specified, otherwise an error will occur during query execution
-- Note: The following demonstration directly uses the dbPath and version variables
```

### File Verification

It is recommended that you actively verify the suitability of the xdb file, as some new features in the future may cause the current Searcher version to be incompatible with the xdb file you are using. Verification can avoid unpredictable errors during runtime. You don't need to verify every time; for example, verify when the service starts or by manually calling the verification command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```lua
local xdb = require('xdb_searcher')

local err = xdb.verify(dbPath);
if err ~= nil then
    -- Suitability verification failed!!!
    -- The current query client implementation is not suitable for the xdb file specified by dbPath.
    -- You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation compatible with dbPath.
    print(string.format("binding is not applicable for xdb file '%s': %s", dbPath, err))
    return
end

-- Verification passed, the current Searcher can safely be used for query operations on the xdb pointed to by dbPath
```

### Entirely File-Based Query

```lua
local xdb = require("xdb_searcher")

-- 1. Create an entirely file-based query object using the version and dbPath mentioned above
local searcher, err = xdb.new_with_file_only(version, db_path)
if err ~= nil then
    print(string.format("failed to create searcher: %s", err))
    return
end

-- 2. Call the query API; with both IPv4 and IPv6 addresses supported
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 3. Close resources
searcher:close()

-- Note: For concurrent use, each coroutine needs to create a separate xdb query object
```

### Caching `VectorIndex`

If supported by your `lua` environment, you can pre-load the vectorIndex cache and make it a global variable. Using the global vectorIndex every time a Searcher is created can reduce one fixed IO operation, thereby accelerating queries and reducing IO pressure.

```lua
local xdb = require("xdb_searcher")

-- 1. Load vectorIndex cache from the specified db_path and make the v_index object below a global variable.
-- vectorIndex only needs to be loaded once; it is recommended to load it as a global object when the service starts.
v_index, err = xdb.load_vector_index(dbPath)
if err ~= nil then
    print(string.format("failed to load vector index from '%s'", db_path))
    return
end

-- 2. Use the global v_index to create a query object with vectorIndex cache.
searcher, err = xdb.new_with_vector_index(version, dbPath, v_index)
if err ~= nil then
    print(string.format("failed to create vector index searcher: %s", err))
    return
end

-- 3. Call the query API; the same interface is used for both IPv4 and IPv6
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 4. Close resources
searcher:close()

-- Note: For concurrent use, each coroutine needs to create a separate xdb query object, but they share the global v_index object
```

### Caching the Entire `xdb` File

If supported by your `lua` environment, you can pre-load the entire xdb data into memory to achieve completely memory-based queries, similar to the previous memory search.

```lua
local xdb = require("xdb_searcher")

-- 1. Load the entire xdb into memory from the specified dbPath.
-- xdb content only needs to be loaded once; it is recommended to load it as a global object when the service starts.
content, err = xdb.load_content(dbPath)
if err ~= nil then
    print(string.format("failed to load xdb content: %s", err))
    return
end

-- 2. Use the global content to create an entirely memory-based query object.
searcher, err = xdb.new_with_buffer(version, content)
if err ~= nil then
    print(string.format("failed to create content buffer searcher: %s", err))
    return
end

-- 3. Call the query API; the same interface is used for both IPv4 and IPv6
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 4. Close resources - This searcher object can be safely used for concurrency; close the searcher only when the entire service is shut down
-- searcher:close()

-- Note: For concurrent use, query objects created with the entire xdb cache can be safely used concurrently.
-- It is recommended to create a global searcher object when the service starts and then use it globally and concurrently.
```

# Query Testing

Perform query tests via the `lua search_test.lua` script:

```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua   
lua search_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: using the default data/ip2region_v4.xdb file for IPv4 query testing:

```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua  --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, io_count: 5, took: 0μs}
ip2region>> 113.118.113.77
{region: 中国|广东省|深圳市|电信|CN, io_count: 2, took: 0μs}
```

For example: using the default data/ip2region_v6.xdb file for IPv6 query testing:

```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua  --db=../../data/ip2region_v6.xdb                                                           
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, io_count: 8, took: 0μs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, io_count: 13, took: 0μs}
```

Enter an IP to perform a query test. You can also set `cache-policy` to file/vectorIndex/content respectively to test the efficiency of the three different cache implementations.

# Bench Testing

Perform automatic bench testing via the `lua bench_test.lua` script. This ensures that the `xdb` file has no errors and tests average query performance through a large number of queries:

```bash
➜  lua git:(fr_lua_ipv6) ✗ lua bench_test.lua                                                                                              
lua bench_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: perform IPv4 bench testing using default data/ip2region_v4.xdb and data/ipv4_source.txt files:

```bash
lua bench_test.lua --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

For example: perform IPv6 bench testing using default data/ip2region_v6.xdb and data/ipv6_source.txt files:

```bash
lua bench_test.lua --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

You can test the performance of the three different cache implementations (file/vectorIndex/content) by setting the `cache-policy` parameter.
@Note: Please note that the src file used for the bench must be the same source file used to generate the corresponding xdb file.
