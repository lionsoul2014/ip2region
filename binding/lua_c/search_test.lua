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

function printHelp()
    print("lua search_test.lua [command options]")
    print("options: ")
    print(" --db string             ip2region binary xdb file path")
    print(" --cache-policy string   cache policy: file/vectorIndex/content")
end

if #arg < 1 then
    printHelp()
    return
end

-- parser the command line args
local dbFile = ""
local cachePolicy = "vectorIndex"
for _, r in ipairs(arg) do
    if string.len(r) < 5 then
        -- continue and do nothing here
    elseif string.sub(r, 1, 2) ~= "--" then
        -- continue and do nothing here
    else
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
    end
end

-- print(string.format("dbFile=%s, cachePolicy=%s", dbFile, cachePolicy))
if string.len(dbFile) < 2 then
    printHelp()
    return
end

-- verify the xdb
if xdb.verify(dbFile) == false then
    print(string.format("failed to verify the xdb file: %s", dbFile))
    return
end

-- detect the version from the xdb header
header, err = xdb.load_header(dbFile)
if err ~= nil then
    print(string.format("failed to load header: %s", err))
    return
end

version, err = xdb.version_from_header(header);
if err ~= nil then
    print(string.format("failed to detect version from header: %s", err))
    return
end

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
        print(string.format("failed to load xdb content from '%s'", dbFile))
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

-- do the search
print(string.format([[
ip2region xdb searcher test program
source xdb: %s (%s, %s)
type 'quit' to exit]], dbFile, xdb.version_info(version).name, cachePolicy))
local region, err = "", nil
local s_time, c_time =  0, 0
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

    -- empty string ignore
    line = line:gsub("^%s*(.-)%s*$", "%1")
    if string.len(line) < 1 then
        -- continue and do nothing here
    else
        s_time = xdb.now()
        ip_bytes, err = xdb.parse_ip(line)
        -- print(string.format("parse(%s): %s, err: %s", line, xdb.ip_to_string(ip_bytes), err))
        if err ~= nil then
            print(string.format("invalid ip address `%s`", line))
        else
            -- do the search
            region, err = searcher:search(ip_bytes)
            c_time = xdb.now() - s_time
            if err ~= nil then
                print(string.format("{err: %s, io_count: %d}", err, searcher:get_io_count()))
            else
                print(string.format("{region: %s, io_count: %d, took: %dμs}", region, searcher:get_io_count(), c_time))
            end
        end
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

xdb.cleanup();