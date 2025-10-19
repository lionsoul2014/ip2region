-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/07/05

package.path = "./?.lua" .. package.path
package.cpath = "./?.so" .. package.cpath
local xdb = require("xdb_searcher")

function test_parse_ip()
    local ip_list = {
        "1.0.0.0", "58.251.30.115", "192.168.1.100", "126.255.32.255", "219.xx.xx.11", 
        "::", "::1", "fffe::", "2c0f:fff0::", "2c0f:fff0::1", "2a02:26f7:c409:4001::",
        "2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "240e:982:e617:ffff:ffff:ffff:ffff:ffff", "::xx:ffff"
    }

    for _, ip_str in ipairs(ip_list) do
        ip_bytes, err = xdb.parse_ip(ip_str)
        if err ~= nil then
            print(string.format("failed to parse ip address `%s`: %s", ip_str, err))
        else
            print(string.format("`%s`.bytes=%d", ip_str, #ip_bytes))
        end
    end
end

function test_check_ip()
    local ip_list = {
        "1.2.3.4", "192.168.2.3", "120.24.78.129", "255.255.255.0",
        "256.7.12.9", "12.56.78.320", "32.12.45.192", "222.221.220.219",
        "192.168.1.101 ", "132.96.12.98a", "x23.12.2.12"
    }

    local s_time = xdb.now()
    for _, ip_src in ipairs(ip_list) do
        ip, err = xdb.check_ip(ip_src)
        if err ~= nil then
            print(string.format("invalid ip address `%s`: %s", ip_src, err))
        else
            ip_dst = xdb.long2ip(ip)
            io.write(string.format("long(%-15s)=%10d, long2ip(%-10d)=%-15s", ip_src, ip, ip, ip_dst))
            if ip_src ~= ip_dst then
                print(" --[Failed]")
            else
                print(" --[Ok]")
            end
        end
    end
end

---- buffer loading test
function test_load_header()
    header, err = xdb.load_header("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load header: ", err)
    else
        print("xdb header buffer loaded")

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

        print(string.format(tpl,
            header["version"], header["index_policy"],
            header["created_at"], header["start_index_ptr"], header["end_index_ptr"],
            header["ip_version"], header["runtime_ptr_bytes"]
        ))
    end
end

function test_load_vector_index()
    v_index, err = xdb.load_vector_index("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load vector index: ", err)
    else
        print("xdb vector index buffer loaded")
    end
end

function test_load_content()
    c_buffer, err = xdb.load_content("../../data/ip2region_v4.xdb")
    if err ~= nil then
        print("failed to load content: ", err)
    else
        print("xdb content buffer loaded")
    end
end


function test_search()
    local ip_str = "1.2.3.4"
    searcher, err = xdb.new_with_file_only("../../data/ip2region_v4.xdb")
    local t_start = xdb.now()
    region, err = searcher:search(ip_str)
    if err ~= nil then
        print(string.format("search(%s) failed: %s", ip_str, err))
    else
        local c_time = xdb.now() - t_start
        print(string.format("search(%s): {region=%s, io_count: %d, took: %dμs, err=%s}",
                ip_str, region, searcher:get_io_count(), c_time, err))
        print(string.format("searcher.tostring=%s", searcher))
    end
    searcher:close()
end


-- check and call the function

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
print(string.format("|---done, elapsed %.3fμs", cost_time))