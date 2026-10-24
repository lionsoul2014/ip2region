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

local tests = {}

---- ip checking testing
function tests.test_parse_ip()
    local ip_list = {
        "1.2.3.4", "192.168.2.3", "120.24.78.129", "255.255.255.0", "invalid-ipv.4",
        "::", "3000::", "240e:3b7:3276:33b0:4844:6f28:f69c:1eee", "2001:4:112::", "invalid-ipv::6"
    }

    for _, ip_src in ipairs(ip_list) do
        local ip_bytes, err = xdb.parse_ip(ip_src)
        if err ~= nil then
            print(string.format("invalid ip address `%s`: %s", ip_src, err))
        else
            local ip_string = xdb.ip_to_string(ip_bytes);
            print(string.format("parse_ip(%s)->%s ? %s", ip_src, ip_string, tostring(ip_src==ip_string)))
        end
    end
end

function tests.test_print_const()
    print("ipv4: ", xdb.IPv4);
    print("ipv6: ", xdb.IPv6);
    print("header_buffer: ", xdb.header_buffer);
    print("v_index_buffer: ", xdb.v_index_buffer);
    print("content_buffer: ", xdb.content_buffer);
end

---- buffer loading test
function tests.test_load_header()
    local header, err = xdb.load_header("../../data/ip2region_v4.xdb")
    if err ~= nil then
        error(string.format("failed to load header: %s", err))
    end

    print(string.format("xdb header buffer `%s` loaded", tostring(header)))

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

function tests.test_version_info()
    for _, id in ipairs({xdb.IPv4, xdb.IPv6}) do
        local v = xdb.version_info(id)
        print(string.format("{id:%d, name: %s, bytes: %d, segment_index_size: %d}",
                v.id, v.name, v.bytes, v.segment_index_size))
    end

    assert(not pcall(xdb.version_info, 3), "version_info(3) should fail")
end

function tests.test_load_vector_index()
    local v_index, err = xdb.load_vector_index("../../data/ip2region_v4.xdb")
    if err ~= nil then
        error(string.format("failed to load vector index: %s", err))
    end

    print(string.format("xdb vector index buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
            tostring(v_index), v_index:name(), v_index:type(), v_index:length()))
    v_index:close()
end


function tests.test_load_content()
    local c_buffer, err = xdb.load_content("../../data/ip2region_v4.xdb")
    if err ~= nil then
        error(string.format("failed to load content: %s", err))
    end

    print(string.format("xdb content buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
            tostring(c_buffer), c_buffer:name(), c_buffer:type(), c_buffer:length()))
    c_buffer:close();
end


function tests.test_search()
    local test_list = {
        {"1.2.3.4", xdb.IPv4, "../../data/ip2region_v4.xdb"},
        {"240e:3b7:3276:33b0:958f:f34c:d04f:f6a", xdb.IPv6, "../../data/ip2region_v6.xdb"}
    }

    for _, test in ipairs(test_list) do
        local ip_str = test[1]
        local searcher, err = xdb.new_with_file_only(test[2], test[3])
        if err ~= nil then
            error(string.format("failed to create searcher: %s", err))
        end

        print(string.format("searcher.tostring=%s", tostring(searcher)))
        local t_start = xdb.now()
        local region
        region, err = searcher:search(ip_str)
        if err ~= nil then
            error(string.format("failed to search(%s): %s", ip_str, err))
        end

        local c_time = xdb.now() - t_start
        print(string.format("search(%s): {region=%s, io_count: %d, took: %dμs}",
                ip_str, region, searcher:get_io_count(), c_time))
        searcher:close()
    end
end

local func_name = arg[1]
if func_name == nil then
    error("please specified the function to test")
end

local test = tests[func_name]
if test == nil then
    error(string.format("undefined function `%s` to call", func_name))
end

local s_time = xdb.now();
print(string.format("+---calling test function %s ...", func_name))
test()
local cost_time = xdb.now() - s_time
xdb.cleanup();
print(string.format("|---done, took: %.3fμs", cost_time))