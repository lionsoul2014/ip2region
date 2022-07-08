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
    print("lua search_test.lua [command options]")
    print("options: ")
    print(" --db string             ip2region binary xdb file path")
    print(" --cache-policy string   cache policy: file/vectorIndex/content")
end

if #arg < 2 then
    printHelp(arg)
    return
end

-- parser the command line args
local dbFile = ""
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

-- print(string.format("dbFile=%s, cachePolicy=%s", dbFile, cachePolicy))
if string.len(dbFile) < 2 then
    printHelp()
    return
end

-- create the searcher based on the cache-policy
local searcher, v_index, content
if cachePolicy == "file" then
    searcher, err = xdb.new_with_file_only(dbFile)
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

    searcher, err = xdb.new_with_vector_index(dbFile, v_index)
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

    searcher, err = xdb.new_with_buffer(content)
    if err ~= nil then
        print(string.format("failed to create content buffer searcher: %s", err))
        return
    end
else
    print(string.format("undefined cache-policy `%s`", cachePolicy))
    return
end

-- do the search
print(string.format([[
ip2region xdb searcher test program, cachePolicy: %s
type 'quit' to exit]], cachePolicy))
local region, err = "", nil
local ip_int, s_time, c_time =  0, 0, 0
while ( true ) do
    io.write("ip2region>> ");
    io.input(io.stdin);
    local line = io.read();
    if (line == nil) then
        break
    end

    if ( line == "quit" ) then
        break
    end

    ip_int, err = xdb.check_ip(line)
    if err ~= nil then
        print(string.format("invalid ip address `%s`", line))
        goto continue
    end

    -- do the search
    s_time = xdb.now()
    region, err = searcher:search(line)
    if err ~= nil then
        print(string.format("{err: %s, io_count: %d}", err, searcher:get_io_count()))
    else
        c_time = xdb.now() - s_time
        print(string.format("{region: %s, io_count: %d, took: %dμs}", region, searcher:get_io_count(), c_time))
    end

    ::continue::
end

-- resource cleanup
searcher:close()