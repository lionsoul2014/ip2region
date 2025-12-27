# ip2region xdb lua c 扩展查询客户端实现

# 版本兼容
该实现兼容 lua `5.1`，`5.2`，`5.3`, `5.4`

# 编译安装

### 默认编译
通过如下方式来编译安装默认的 `Lua5.4` 版本的扩展：
```bash
# cd 到 lua_c binding 的根目录
make
sudo make install
```

### 指定 Lua 版本
通过如下的 `LuaVersion` 参数指定 Lua 版本编译，例如：`5.1` / `5.2` / `5.3` / `5.4`
```bash
# cd 到 lua_c binding 的根目录
# 例如，编译 5.1 版本兼容的扩展
make LuaVersion=5.1
sudo make install
```

备注：使用了指定的版本的 lua 编译的扩展就请使用相同版本的`lua`去运行以下的测试，例如：
```bash
# 使用 lua 5.1 编译扩展
make LuaVersion=5.1

# 使用 lua5.1 运行查询测试
lua5.1 search_test.py --db=../../data/ip2region_v4.xdb
```



# 使用方式

### 关于查询 API
查询 API 的原型如下：
```lua
-- 通过字符串 IP 或者 xdb.parse_ip 解析得到的二进制 IP 进行查询
search(ip_string | ip_bytes) (region, error)
```
如果查询失败则 error 将会为一个非 `nil` 的错误描述字符串，查询成功将会返回字符串的 `region` 信息，如果查询的 IP 地址没找到则会返回一个空字符 `""`。

### 关于 IPv4 和 IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```lua
-- 引入 xdb searcher 扩展
local xdb = require("xdb_searcher")

-- 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 IPv4
local db_path  = "../../data/ip2region_v4.xdb"  -- 或者你的 ipv4 xdb 的路径
local version = xdb.IPv4

-- 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 IPv6
local db_path  = "../../data/ip2region_v6.xdb";  -- 或者你的 ipv6 xdb 路径
local version = xdb.IPv6

-- db_path 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
-- 备注：以下演示直接使用 db_path 和 version 变量
```

### XDB 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。 你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```lua
local xdb = require("xdb_searcher")

-- verify the xdb
if xdb.verify(db_path) == false then
    -- 适用性验证失败！！！
    -- 当前查询客户端实现不适用于 db_path 指定的 xdb 文件的查询.
    -- 应该停止启动服务，使用合适的 xdb 文件或者升级到适合 db_path 的 Searcher 实现。
    print(string.format("failed to verify the xdb file: %s", db_path))
    return
end

-- 验证通过，当前使用的 Searcher 可以安全的用于对 db_path 指向的 xdb 的查询操作
```

### 完全基于文件的查询
```lua
local xdb = require("xdb_searcher")

-- 1、使用 version 从 db_path 创建基于文件的 xdb 查询对象
local searcher, err = xdb.new_with_file_only(version, db_path)
if err ~= nil then
    print(string.format("failed to create searcher: %s", err))
    return
end

-- 2、调用查询 API 进行查询，IPv4 和 IPv6 都支持
local ip_str = "1.2.3.4"
-- ip_str = "2001:4:112:ffff:ffff:ffff:ffff:ffff" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() - s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象

-- 3，关闭 xdb 查询器
searcher:close()

--
-- 4，模块资源清理，仅在需要将整个服务完全关闭前调用
xdb.cleanup()
```

### 缓存 `VectorIndex` 索引

如果你的 `lua` 母环境支持，可以预先加载 vectorIndex 缓存，然后做成全局变量，每次创建 Searcher 的时候使用全局的 vectorIndex，可以减少一次固定的 IO 操作从而加速查询，减少 io 压力。
```lua
local xdb = require("xdb_searcher")

-- 1、从指定的 db_path 加载 VectorIndex 缓存，把下述的 v_index 对象做成全局变量。
-- vectorIndex 加载一次即可，建议在服务启动的时候加载为全局对象。
v_index, err = xdb.load_vector_index(db_path)
if err ~= nil then
    print(string.format("failed to load vector index from '%s'", db_path))
    return
end

-- 2、使用全局的 v_index 创建带 VectorIndex 缓存的查询对象。
searcher, err = xdb.new_with_vector_index(version, db_path, v_index)
if err ~= nil then
    print(string.format("failed to create vector index searcher: %s", err))
    return
end

-- 3、调用查询 API ，IPv4 和 IPv6 都支持
local ip_str = "1.2.3.4"
-- ip_str = "2001:4:112:ffff:ffff:ffff:ffff:ffff" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() = s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- 备注：并发使用，每个协程需要创建单独的 xdb 查询对象，但是共享全局的 v_index 对象

-- 4，关闭 xdb 查询器
searcher:close()

--
-- 5，模块资源清理，仅在需要将整个服务完全关闭前调用
xdb.cleanup()
```

### 缓存整个 `xdb` 数据

如果你的 `lua` 母环境支持，可以预先加载整个 xdb 的数据到内存，这样可以实现完全基于内存的查询，类似之前的 memory search 查询。
```lua
local xdb = require("xdb_searcher")

-- 1、从指定的 db_path 加载整个 xdb 到内存。
-- xdb内容加载一次即可，建议在服务启动的时候加载为全局对象。
local content = xdb.load_content(db_path)
if content == nil then
    print(string.format("failed to load xdb content from '%s'", db_path))
    return
end

-- 2、使用全局的 content 创建带完全基于内存的查询对象。
searcher, err = xdb.new_with_buffer(version, content)
if err ~= nil then
    print(string.format("failed to create content buffer searcher: %s", err))
    return
end

-- 3、调用查询 API ，IPv4 和 IPv6 都支持
local ip_str = "1.2.3.4"
-- ip_str = "2001:4:112:ffff:ffff:ffff:ffff:ffff" // IPv6
local s_time = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() - s_time
if err ~= nil then
    print(string.format("failed to search(%s): %s", ip_str, err))
    return
end

print(string.format("{region: %s, took: %.5f μs}", region, c_time))

-- 备注：并发使用，用 xdb 整个缓存创建的查询对象可以安全的用于并发。
-- 建议在服务启动的时候创建好全局的 searcher 对象，然后全局并发使用。

-- 4，关闭 xdb 查询器
searcher:close()

--
-- 5，模块资源清理，仅在需要将整个服务完全关闭前调用
xdb.cleanup()
```


# 查询测试

通过 `search_test.lua` 脚本来进行查询测试：
```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./search_test.lua 
lua search_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region_v4.xdb 进行 IPv4 查询测试：
```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./search_test.lua --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动, io_count: 3, took: 34μs}
```

例如：使用默认的 data/ip2region_v6.xdb 进行 IPv6 查询测试：
```bash
➜  lua_c git:(master) lua ./search_test.lua --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: , io_count: 1, took: 48μs}
ip2region>> 240e:3b7:3276:33b0:958f:f34c:d04f:f6a 
{region: 中国|广东省|深圳市|家庭宽带, io_count: 8, took: 51μs}
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。


# bench 测试

通过 `bench_test.lua` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua 
lua bench_test.lua [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 来进行 IPv4 的 bench 测试：
```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
Bench finished, {cachePolicy: vectorIndex, total: 1367686, took: 8.593 s, cost: 5.433 μs/op}
```

例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 来进行 IPv6 的 bench 测试：
```bash
➜  lua_c git:(fr_lua_c_ipv6) ✗ lua ./bench_test.lua --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt                       
Bench finished, {cachePolicy: vectorIndex, total: 34159862, took: 829.008 s, cost: 23.176 μs/op}
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。
