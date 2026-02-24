[中文简体](README_zh.md) | [English](README.md)

# ip2region lua c extension query client

# Version Compatibility

This implementation is compatible with lua `5.1`, `5.2`, `5.3`, and `5.4`.

# Compilation and Installation

### Default Compilation

Use the following commands to compile and install the default `Lua5.4` version of the extension:

```bash
# cd to the root directory of lua_c binding
make
sudo make install
```

### Specify Lua Version

Specify the Lua version for compilation using the `LuaVersion` parameter, for example: `5.1` / `5.2` / `5.3` / `5.4`

```bash
# cd to the root directory of lua_c binding
# For example, compile the extension compatible with version 5.1
make LuaVersion=5.1
sudo make install
```

Note: Please use the same version of `lua` to run the following tests as the one used to compile the extension. For example:

```bash
# Compile extension using lua 5.1
make LuaVersion=5.1

# Run query test using lua5.1
lua5.1 search_test.py --db=../../data/ip2region_v4.xdb
```

# Usage

### About Query API

The prototype of the query API is as follows:

```lua
-- Query via IP string or binary IP parsed by xdb.parse_ip
search(ip_string | ip_bytes) (region, error)
```

If the query fails, `error` will be a non-`nil` error description string. If successful, it returns the `region` information as a string. If the IP address is not found, it returns an empty string `""`.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. Usage is as follows:

```lua
-- Import xdb searcher extension
local xdb = require("xdb_searcher")

-- For IPv4: Set xdb path to v4 xdb file, specify IP version as IPv4
local db_path  = "../../data/ip2region_v4.xdb"  -- or your ipv4 xdb path
local version = xdb.IPv4

-- For IPv6: Set xdb path to v6 xdb file, specify IP version as IPv6
local db_path  = "../../data/ip2region_v6.xdb";  -- or your ipv6 xdb path
local version = xdb.IPv6

-- The IP version of the xdb specified by db_path must match the version specified, otherwise an error will occur during query execution
-- Note: The following demonstration directly uses the db_path and version variables
```

### XDB File Verification

It is recommended to actively verify the suitability of the xdb file. New features in the future may cause the current Searcher version to be incompatible with the xdb file you are using. Verification helps avoid unpredictable errors during runtime. You don't need to verify every time; for example, verify when the service starts or by manually calling the verification command. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```lua
local xdb = require("xdb_searcher")

-- verify the xdb
if xdb.verify(db_path) == false then
    -- Suitability verification failed!!!
    -- The current query client implementation is not suitable for the xdb file specified by db_path.
    -- You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation compatible with db_path.
    print(string.format("failed to verify the xdb file: %s", db_path))
    return
end

-- Verification passed, the current Searcher can safely be used for query operations on the xdb pointed to by db_path
```

### Entirely File-Based Query

```lua
local xdb = require("xdb_searcher")

-- 1. Create a file-based xdb query object from db_path using version
local searcher, err = xdb.new_with_file_only(version, db_path)
if err ~= nil then
    print(string.format("failed to create searcher: %s", err))
    return
end

-- 2. Call the query API; both IPv4 and IPv6 are supported
local ip_str = "1.2.3.4"
-- ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() - s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- Note: For concurrent use, each coroutine needs to create a separate xdb query object

-- 3. Close the xdb searcher
searcher:close()

--
-- 4. Module resource cleanup, only call before the entire service is completely shut down
xdb.cleanup()
```

### Caching `VectorIndex`

If supported by your `lua` environment, you can pre-load the `vectorIndex` cache and make it a global variable. Using the global `vectorIndex` every time a Searcher is created can reduce one fixed IO operation, thereby accelerating queries and reducing IO pressure.

```lua
local xdb = require("xdb_searcher")

-- 1. Load VectorIndex cache from the specified db_path and make the v_index object below a global variable.
-- vectorIndex only needs to be loaded once; it is recommended to load it as a global object when the service starts.
v_index, err = xdb.load_vector_index(db_path)
if err ~= nil then
    print(string.format("failed to load vector index from '%s'", db_path))
    return
end

-- 2. Use the global v_index to create a query object with VectorIndex cache.
searcher, err = xdb.new_with_vector_index(version, db_path, v_index)
if err ~= nil then
    print(string.format("failed to create vector index searcher: %s", err))
    return
end

-- 3. Call the query API; both IPv4 and IPv6 are supported
local ip_str = "1.2.3.4"
-- ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() = s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- Note: For concurrent use, each coroutine needs to create a separate xdb query object, but they share the global v_index object

-- 4. Close the xdb searcher
searcher:close()

--
-- 5. Module resource cleanup, only call before the entire service is completely shut down
xdb.cleanup()
```

### Caching the Entire `xdb` File

If supported by your `lua` environment, you can pre-load the entire xdb data into memory to achieve completely memory-based queries, similar to the previous memory search.

```lua
local xdb = require("xdb_searcher")

-- 1. Load the entire xdb into memory from the specified db_path.
-- xdb content only needs to be loaded once; it is recommended to load it as a global object when the service starts.
local content = xdb.load_content(db_path)
if content == nil then
    print(string.format("failed to load xdb content from '%s'", db_path))
    return
end

-- 2. Use the global content to create a query object based entirely on memory.
searcher, err = xdb.new_with_buffer(version, content)
if err ~= nil then
    print(string.format("failed to create content buffer searcher: %s", err))
    return
end

-- 3. Call the query API; both IPv4 and IPv6 are supported
local ip_str = "1.2.3.4"
-- ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() - s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- Note: For concurrent use, query objects created with the entire xdb cache can be safely used concurrently.
-- It is recommended to create a global searcher object when the service starts and then use it globally and concurrently.

-- 4. Close the xdb searcher
searcher:close()

--
-- 5. Module resource cleanup, only call before the entire service is completely shut down
xdb.cleanup()
```

# Query Testing

Perform query tests via the `search_test.lua` script:

```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./search_test.lua 
lua search_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: using the default `data/ip2region_v4.xdb` for IPv4 query testing:

```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./search_test.lua --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, io_count: 5, took: 17μs}
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动|CN, io_count: 3, took: 40μs}
```

For example: using the default `data/ip2region_v6.xdb` for IPv6 query testing:

```bash
➜  lua_c git:(master) lua ./search_test.lua --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: , io_count: 1, took: 48μs}
ip2region>> 240e:3b7:3276:33b0:958f:f34c:d04f:f6a
{region: 中国|广东省|深圳市|电信|CN, io_count: 8, took: 52μs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, io_count: 13, took: 35μs}
```

Enter an IP to perform a query test. You can also set `cache-policy` to `file`/`vectorIndex`/`content` respectively to test the efficiency of the three different cache implementations.

# Bench Testing

Perform automatic bench testing via the `bench_test.lua` script. This ensures that the `xdb` file has no errors and tests average query performance through a large number of queries:

```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua 
lua bench_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: perform IPv4 bench testing using default `data/ip2region_v4.xdb` and `data/ipv4_source.txt`:

```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
Bench finished, {cachePolicy: vectorIndex, total: 1367686, took: 8.593 s, cost: 5.433 μs/op}
```

For example: perform IPv6 bench testing using default `data/ip2region_v6.xdb` and `data/ipv6_source.txt`:

```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt                       
Bench finished, {cachePolicy: vectorIndex, total: 34159862, took: 829.008 s, cost: 23.176 μs/op}
```

You can test the performance of the three different cache implementations (`file`/`vectorIndex`/`content`) by setting the `cache-policy` parameter.
@Note: Please ensure that the `src` file used for the bench is the same source file used to generate the corresponding `xdb` file.
