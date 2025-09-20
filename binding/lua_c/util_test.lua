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

---- ip checking testing
function test_parse_ip()
    local ip_list = {
        "1.2.3.4", "192.168.2.3", "120.24.78.129", "255.255.255.0", "invalid-ipv.4",
        "::", "3000::", "240e:3b7:3276:33b0:4844:6f28:f69c:1eee", "2001:4:112::", "invalid-ipv::6"
    }

    local s_time = xdb.now()
    for _, ip_src in ipairs(ip_list) do
        ip_bytes, err = xdb.parse_ip(ip_src)
        if err ~= nil then
            print(string.format("invalid ip address `%s`: %s", ip_src, err))
        else
            print(string.format("parse_ip(%s): %s", ip_src, xdb.ip_to_string(ip_bytes)))
        end
    end
end

function test_print_const()
    print("ipv4: ", xdb.IPv4);
    print("ipv6: ", xdb.IPv6);
    print("header_buffer: ", xdb.header_buffer);
    print("v_index_buffer: ", xdb.v_index_buffer);
    print("content_buffer: ", xdb.content_buffer);
end

---- buffer loading test
function test_load_header() 
    header, err = xdb.load_header("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load header: ", err)
    else
        print(string.format("xdb header buffer `%s` loaded", header))

        local tpl = [[
    header: {
        version: %d
        index_policy: %d
        created_at: %d
        start_index_ptr: %d
        end_index_ptr: %d
        ip_version: %d
        runtime_ptr_bytes: %d
    }]]

        local t = header:to_table()
        print(string.format(tpl,
            t["version"], t["index_policy"], t["created_at"], 
            t["start_index_ptr"], t["end_index_ptr"], t["ip_version"], t["runtime_ptr_bytes"])
        )
    end
end


function test_load_vector_index()
    v_index, err = xdb.load_vector_index("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load vector index: ", err)
    else
        print(string.format("xdb vector index buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
                v_index, v_index:name(), v_index:type(), v_index:length()))
        v_index:close()
    end
end


function test_load_content()
    c_buffer, err = xdb.load_content("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load content: ", err)
    else
        print(string.format("xdb content buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
                c_buffer, c_buffer:name(), c_buffer:type(), c_buffer:length()))
        c_buffer:close();
    end
end


function test_search()
    local ip_str = "1.2.3.4"
    searcher, err = xdb.new_with_file_only(xdb.IPv4, "../../data/ip2region_v4.xdb")
    local t_start = xdb.now()
    region, err = searcher:search(ip_str)
    local c_time = xdb.now() - t_start
    print(string.format("search(%s): {region=%s, io_count: %d, took: %dμs, err=%s}",
            ip_str, region, searcher:get_io_count(), c_time, err))
    print(string.format("searcher.tostring=%s", searcher))
    searcher:close()
end

local func_name = arg[1]
if func_name == nil then
    print("please specified the function to test")
    return
end

if (_G[func_name] == nil) then
    print(string.format("undefined function `%s` to call", func_name))
    return
end

local s_time = xdb.now();
print(string.format("+---calling test function %s ...", func_name))
_G[func_name]();
local cost_time = xdb.now() - s_time
print(string.format("|---done, took: %.3fμs", cost_time))