-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/06/30

-- set the package to load the current xdb_searcher.so
package.path = "./?.lua" .. package.path
package.cpath = "./?.so" .. package.cpath
local xdb = require("xdb_searcher")

function printHelp()
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
    if string.len(r) < 5 then
        goto continue
    end

    if string.sub(r, 1, 2) ~= "--" then
        goto continue
    end

    for k, v in string.gmatch(string.sub(r, 3), "([^=]+)=([^%s]+)") do
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

        -- break the match iterate
        break
    end

    -- continue this loop
    ::continue::
end

-- print(string.format("dbFile=%s, srcFile=%s, cachePolicy=%s", dbFile, srcFile, cachePolicy))
if string.len(dbFile) < 2 or string.len(srcFile) < 2 then
    printHelp()
    return
end

-- open the dbFile
local handle, closer, err = xdb.open_file(dbFile, "rb")
if err ~= nil then
    print(string.format("failed to open %s: %s", dbFile, err))
    return
end

-- verify the xdb
err = xdb.verify(handle)
if err ~= nil then
    closer()
    print(string.format("verify(%s): %s", dbFile, err))
    return
end

-- load the header and define the ip version
local header, err = xdb.load_header(handle)
if err ~= nil then
    closer()
    print(string.format("failed to load the header: %s", err))
    return
end

local version, err = xdb.version_from_header(header)
if err ~= nil then
    closer()
    print(string.format("failed to detect version from header: %s", err))
    return
end

-- file close
closer("bench_test")


-- create the searcher based on the cache-policy
local searcher, v_index, content
if cachePolicy == "file" then
    searcher, err = xdb.new_with_file_only(version, dbFile)
    if err ~= nil then
        print(string.format("failed to create searcher: %s", err))
        return
    end
elseif cachePolicy == "vectorIndex" then
    v_index, err = xdb.load_vector_index(dbFile)
    if err ~= nil then
        print(string.format("failed to load vector index: %s", err))
        return
    end

    searcher, err = xdb.new_with_vector_index(version, dbFile, v_index)
    if err ~= nil then
        print(string.format("failed to create vector index searcher: %s", err))
        return
    end
elseif cachePolicy == "content" then
    content, err = xdb.load_content(dbFile)
    if err ~= nil then
        print(string.format("failed to load xdb content: %s", err))
        return
    end

    searcher, err = xdb.new_with_buffer(version, content)
    if err ~= nil then
        print(string.format("failed to create content buffer searcher: %s", err))
        return
    end
else
    print(string.format("undefined cache-policy `%s`", cachePolicy))
    return
end

-- do the bench test
local handle = io.open(srcFile, "r")
if handle == nil then
    print(string.format("failed to open src text file `%s`", handle))
    return
end

local lines = handle:lines()
local sip_str, eip_str, s_region, region = "", "", "", nil
local sip, eip, err = nil, nil, nil
local count, c_time = 0, 0
local s_time = xdb.now()
for l in lines do
    if string.len(l) < 1 then
        goto continue
    end

    for v1, v2, v3 in string.gmatch(l, "([^|]+)|([^|]+)|([^\n]+)") do
        sip_str = v1
        eip_str = v2
        s_region = v3
        break
    end
    -- print('sip', sip_str, 'eip', eip_str, 'region', s_region)

    sip, err = xdb.parse_ip(sip_str)
    if err ~= nil then
        print(string.format("invalid start ip `%s`", sip_str))
        return
    end

    eip, err = xdb.parse_ip(eip_str)
    if err ~= nil then
        print(string.format("invalid end ip `%s`", sip_str))
        return
    end

    if xdb.ip_compare(sip, eip) > 0 then
        print(string.format("start ip(%s) should not be greater than end ip(%s)\n", sip_str, eip_str))
        return
    end

    local t_time = xdb.now()
    for _, ip in ipairs({sip, eip}) do
        region, err = searcher:search(ip)
        if err ~= nil then
            print(string.format("failed to search ip `%s`", xdb.ip_to_string(ip)))
            return
        end

        -- check the region
        if region ~= s_region then
            print(string.format("failed search(%s) with (%s != %s)\n", xdb.ip_to_string(ip), region, s_region))
            return
        end

        count = count + 1
    end
    c_time = c_time + xdb.now() - t_time

    ::continue::
end

-- resource cleanup
searcher:close()

-- print the stats
local total_costs = (xdb.now() - s_time) / 1e6
local avg_costs = 0
if count > 0 then
    avg_costs = c_time / count
end
print(string.format(
    "Bench finished, {cachePolicy: %s, total: %d, took: {total: %0.3fs, search: %0.3fs}, cost: %.3f μs/op}", 
    cachePolicy, count, total_costs, c_time/1e6, avg_costs
))
