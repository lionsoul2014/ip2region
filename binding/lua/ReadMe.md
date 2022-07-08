# ip2region xdb lua 查询客户端实现

#### 备注：请优先使用 lua_c 扩展 xdb 查询客户端，性能比纯 lua 实现的要快很多！！！


# 使用方式

### 完全基于文件的查询
```lua
local xdb = require("xdb_searcher")

-- 1、从 db_path 创建基于文件的 xdb 查询对象
local db_path = "ip2region.xdb file path"
local searcher, err = xdb.new_with_file_only(db_path)
if err ~= nil then
    print(string.format("failed to create searcher: %s", err))
    return
end

-- 2、调用查询 API 进行查询
local ip_str = "1.2.3.4"
local s_time = xdb.now()
region, err = searcher:search(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象

print(string.format("{region: %s, took: %.5f μs}", region, xdb.now() - s_time))
```

### 缓存 `VectorIndex` 索引

如果你的 `lua` 母环境支持，可以预先加载 vectorIndex 缓存，然后做成全局变量，每次创建 Searcher 的时候使用全局的 vectorIndex，可以减少一次固定的 IO 操作从而加速查询，减少 io 压力。
```lua
local xdb = require("xdb_searcher")

local db_path = "ip2region.xdb file path"

-- 1、从指定的 db_path 加载 VectorIndex 缓存，把下述的 v_index 对象做成全局变量。
-- vectorIndex 加载一次即可，建议在服务启动的时候加载为全局对象。
v_index, err = xdb.load_vector_index(db_path)
if err ~= nil then
    print(string.format("failed to load vector index from '%s'", db_path))
    return
end

-- 2、使用全局的 v_index 创建带 VectorIndex 缓存的查询对象。
searcher, err = xdb.new_with_vector_index(db_path, v_index)
if err ~= nil then
    print(string.format("failed to create vector index searcher: %s", err))
    return
end

-- 3、调用查询 API 
local ip_str = "1.2.3.4"
local s_time = xdb.now()
region, err = searcher:search(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象，但是共享全局的 v_index 对象

print(string.format("{region: %s, took: %.5f μs}", region, xdb.now() - s_time))
```

### 缓存整个 `xdb` 数据

如果你的 `lua` 母环境支持，可以预先加载整个 xdb 的数据到内存，这样可以实现完全基于内存的查询，类似之前的 memory search 查询。
```lua
local xdb = require("xdb_searcher")

local db_path = "ip2region.xdb file path"

-- 1、从指定的 db_path 加载整个 xdb 到内存。
-- xdb内容加载一次即可，建议在服务启动的时候加载为全局对象。
content = xdb.load_content(db_path)
if content == nil then
    print(string.format("failed to load xdb content from '%s'", db_path))
    return
end

-- 2、使用全局的 content 创建带完全基于内存的查询对象。
searcher, err = xdb.new_with_buffer(content)
if err ~= nil then
    print(string.format("failed to create content buffer searcher: %s", err))
    return
end

-- 3、调用查询 API 
local ip_str = "1.2.3.4"
local s_time = xdb.now()
region, err = searcher:search(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

-- 备注：并发使用，用 xdb 整个缓存创建的查询对象可以安全的用于并发。
-- 建议在服务启动的时候创建好全局的 searcher 对象，然后全局并发使用。

print(string.format("{region: %s, took: %.5f μs}", region, xdb.now() - s_time))
```


# 查询测试

通过 `search_test.lua` 脚本来进行查询测试：
```bash
➜  lua git:(lua_binding) ✗ lua search_test.lua
lua search_test.lua [command options]
options:
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试：
```bash
➜  lua git:(lua_binding) ✗ lua search_test.lua --db=../../data/ip2region.xdb --cache-policy=vectorIndex
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, io_count: 7, took: 0μs}
ip2region>> 
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。


# bench 测试

通过 `bench_test.lua` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  lua git:(lua_binding) ✗ lua bench_test.lua 
lua bench_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 来进行 bench 测试：
```bash
➜  lua git:(lua_binding) ✗ lua bench_test.lua --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=vectorIndex
Bench finished, {cachePolicy: vectorIndex, total: 3417955, took: 29.000 s, cost: 7.899 μs/op}
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。
