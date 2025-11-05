# ip2region xdb lua 查询客户端实现

#### 备注：请优先使用 lua_c 扩展 xdb 查询客户端，性能比纯 lua 实现的要快很多！！！

# 版本兼容
该实现兼容 lua `5.3`, `5.4`，且不再提供更低版本的兼容，如果需要在更低版本的 Lua 下使用请考虑使用 `[lua_c](../lua_c/)` 扩展。

# 使用方式

### 关于查询 API
查询 API 的原型如下：
```lua
-- 通过字符串 IP 查询
search_by_string(ip_string) (region, error)
-- 通过 parse_ip 返回的 bytes IP 查询
search(ip_bytes) (region, error)
```
如果查询出错则会返回非 `nil` 的 error 字符串信息，如果查询成功则会返回字符串的 `region` 信息，如果查询的 IP 地址找不到则会返回空字符串 `""`。

### 关于 IPv4 和 IPv6
```lua
local xdb = require("xdb_searcher")

-- 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 Version.IPv4
local dbPath  = "../../data/ip2region_v4.xdb"  -- 或者你的 ipv4 xdb 的路径
local version = xdb.IPv4

-- 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 Version.IPv6
local dbPath  = "../../data/ip2region_v6.xdb"  -- 或者你的 ipv6 xdb 路径
local version = xdb.IPv6

-- dbPath 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
-- 备注：以下演示直接使用 dbPath 和 version 变量
```

### 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。 你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```lua
local xdb = require('xdb_searcher')

local err = xdb.verify(dbPath);
if err ~= nil then
    -- 适用性验证失败！！！
    -- 当前查询客户端实现不适用于 dbPath 指定的 xdb 文件的查询.
    -- 应该停止启动服务，使用合适的 xdb 文件或者升级到适合 dbPath 的 Searcher 实现。
    print(string.format("binding is not applicable for xdb file '%s': %s", dbPath, err))
    return
end

-- 验证通过，当前使用的 Searcher 可以安全的用于对 dbPath 指向的 xdb 的查询操作
```

### 完全基于文件的查询
```lua
local xdb = require("xdb_searcher")

-- 1，使用上述的 version 和 dbPath 创建完全基于文件的查询对象
local searcher, err = xdb.new_with_file_only(version, db_path)
if err ~= nil then
    print(string.format("failed to create searcher: %s", err))
    return
end

-- 2、调用查询 API 进行查询，IPv4 或者 IPv6 的地址都是同一个接口
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 3、关闭资源
searcher:close()

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象
```

### 缓存 `VectorIndex` 索引

如果你的 `lua` 母环境支持，可以预先加载 vectorIndex 缓存，然后做成全局变量，每次创建 Searcher 的时候使用全局的 vectorIndex，可以减少一次固定的 IO 操作从而加速查询，减少 io 压力。
```lua
local xdb = require("xdb_searcher")

-- 1、从指定的 db_path 加载 vectorIndex 缓存，把下述的 v_index 对象做成全局变量。
-- vectorIndex 加载一次即可，建议在服务启动的时候加载为全局对象。
v_index, err = xdb.load_vector_index(dbPath)
if err ~= nil then
    print(string.format("failed to load vector index from '%s'", db_path))
    return
end

-- 2、使用全局的 v_index 创建带 vectorIndex 缓存的查询对象。
searcher, err = xdb.new_with_vector_index(version, dbPath, v_index)
if err ~= nil then
    print(string.format("failed to create vector index searcher: %s", err))
    return
end

-- 3、调用查询 API，IPv4 或者 IPv6 都是同一个接口
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 4、关闭资源
searcher:close()

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象，但是共享全局的 v_index 对象
```

### 缓存整个 `xdb` 数据

如果你的 `lua` 母环境支持，可以预先加载整个 xdb 的数据到内存，这样可以实现完全基于内存的查询，类似之前的 memory search 查询。
```lua
local xdb = require("xdb_searcher")

-- 1、从上述的 dbPath 加载整个 xdb 到内存。
-- xdb内容加载一次即可，建议在服务启动的时候加载为全局对象。
content, err = xdb.load_content(dbPath)
if err ~= nil then
    print(string.format("failed to load xdb content: %s", err))
    return
end

-- 2、使用全局的 content 创建带完全基于内存的查询对象。
searcher, err = xdb.new_with_buffer(version, content)
if err ~= nil then
    print(string.format("failed to create content buffer searcher: %s", err))
    return
end

-- 3、调用查询 API，IPv4 或者 IPv6 都是同一个接口
local ip_str = "1.2.3.4"
-- local ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e" -- IPv6
local s_time = xdb.now()
region, err = searcher:search_by_string(ip_str)
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, io_count: %d, took: %.5f μs}", region, searcher:get_io_count(), xdb.now() - s_time))

-- 4、关闭资源 - 该 searcher 对象可以安全用于并发，等整个服务关闭的时候再关闭 searcher
-- searcher:close()

-- 备注：并发使用，用 xdb 整个缓存创建的查询对象可以安全的用于并发。
-- 建议在服务启动的时候创建好全局的 searcher 对象，然后全局并发使用。
```


# 查询测试

通过 `lua search_test.lua` 脚本来进行查询测试：
```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua   
lua search_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region_v4.xdb 文件进行 IPv4 的查询测试：
```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua  --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|华盛顿|0|谷歌, io_count: 7, took: 0μs}
```

例如：使用默认的 data/ip2region_v6.xdb 文件进行 IPv6 的查询测试：
```bash
➜  lua git:(fr_lua_ipv6) ✗ lua search_test.lua  --db=../../data/ip2region_v6.xdb                                                           
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|家庭宽带, io_count: 8, took: 0μs}
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。


# bench 测试

通过 `lua bench_test.lua` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  lua git:(fr_lua_ipv6) ✗ lua bench_test.lua                                                                                              
lua bench_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 文件进行 IPv4 的 bench 测试：
```bash
lua bench_test.lua --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 文件进行 IPv6 的 bench 测试：
```bash
lua bench_test.lua --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。
