-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/06/30

-- set the package to load the current xdb_searcher.so
package.path = "./?.lua;" .. package.path
package.cpath = "./?.so;" .. package.cpath
local xdb = require("xdb_searcher")

local function printHelp()
    print("lua bench_test.lua [command options]")
    print("options: ")
    print(" --db string             ip2region binary xdb file path")
    print(" --src string            source ip text file path")
    print(" --cache-policy string   cache policy: file/vectorIndex/content")
end

if #arg < 2 then
    printHelp()
    return
end

-- parser the command line args
local dbFile, srcFile = "", ""
local cachePolicy = "vectorIndex"
for _, r in ipairs(arg) do
    if string.len(r) >= 5 and string.sub(r, 1, 2) == "--" then
        local k, v = string.match(string.sub(r, 3), "([^=]+)=([^%s]+)")
        if k == "db" then
            dbFile = v
        elseif k == "src" then
            srcFile = v
        elseif k == "cache-policy" then
            cachePolicy = v
        else
            print(string.format("undefined option `%s`", r))
            return
        end
    end
end

-- print(string.format("dbFile=%s, srcFile=%s, cachePolicy=%s", dbFile, srcFile, cachePolicy))
if string.len(dbFile) < 2 or string.len(srcFile) < 2 then
    printHelp()
    return
end

-- verify the xdb from header
if xdb.verify(dbFile) == false then
    error(string.format("failed to verify the xdb file: %s", dbFile))
end

-- detect the version from the xdb header
local header, err = xdb.load_header(dbFile)
if err ~= nil then
    error(string.format("failed to load header: %s", err))
end

local version
version, err = xdb.version_from_header(header);
if err ~= nil then
    error(string.format("failed to detect version from header: %s", err))
end

-- create the searcher based on the cache-policy
local searcher, v_index, content
if cachePolicy == "file" then
    searcher, err = xdb.new_with_file_only(version, dbFile)
    if err ~= nil then
        error(string.format("failed to create searcher: %s", err))
    end
elseif cachePolicy == "vectorIndex" then
    v_index, err = xdb.load_vector_index(dbFile)
    if err ~= nil then
        error(string.format("failed to load vector index: %s", err))
    end

    searcher, err = xdb.new_with_vector_index(version, dbFile, v_index)
    if err ~= nil then
        error(string.format("failed to create vector index searcher: %s", err))
    end
elseif cachePolicy == "content" then
    content, err = xdb.load_content(dbFile)
    if err ~= nil then
        error(string.format("failed to load xdb content from '%s'", dbFile))
    end

    searcher, err = xdb.new_with_buffer(version, content)
    if err ~= nil then
        error(string.format("failed to create content buffer searcher: %s", err))
    end
else
    error(string.format("undefined cache-policy `%s`", cachePolicy))
end

-- do the bench test
local handle = io.open(srcFile, "r")
if handle == nil then
    error(string.format("failed to open src text file `%s`", srcFile))
end

local lines = handle:lines()
local count, c_time = 0, 0
local s_time = xdb.now()
for l in lines do
    if string.len(l) > 0 then
        local sip_str, eip_str, s_region = string.match(l, "([^|]+)|([^|]+)|([^\n]+)")

        local t_time = xdb.now()
        local sip_bytes, eip_bytes
        sip_bytes, err = xdb.parse_ip(sip_str)
        if err ~= nil then
            error(string.format("invalid start ip `%s`", sip_str))
        end

        eip_bytes, err = xdb.parse_ip(eip_str)
        if err ~= nil then
            error(string.format("invalid end ip `%s`", eip_str))
        end

        if xdb.ip_compare(sip_bytes, eip_bytes) > 0 then
            error(string.format("start ip(%s) should not be greater than end ip(%s)", sip_str, eip_str))
        end

        for _, ip_bytes in ipairs({sip_bytes, eip_bytes}) do
            local region
            region, err = searcher:search(ip_bytes)
            if err ~= nil then
                error(string.format("failed to search ip `%s`: %s", xdb.ip_to_string(ip_bytes), err))
            end

            -- check the region
            if region ~= s_region then
                error(string.format("failed search(%s) with (%s != %s)", xdb.ip_to_string(ip_bytes), region, s_region))
            end

            count = count + 1
        end

        -- increase the time costs
        c_time = c_time + xdb.now() - t_time
    end
end

-- resource cleanup
searcher:close()
if v_index ~= nil then
    v_index:close()
end
if content ~= nil then
    content:close()
end

xdb.cleanup()

-- print the stats
local avg_costs = 0
if count > 0 then
    avg_costs = c_time / count
end
print(string.format("Bench finished, {cachePolicy: %s, total: %d, took: %.3f s, cost: %.3f μs/op}",
         cachePolicy, count, (xdb.now() - s_time)/1e6, avg_costs))