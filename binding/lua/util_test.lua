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

function test_version_parse()
    print("IPv4", xdb.IPv4)
    print("IPv6", xdb.IPv6)

    -- version name parse
    local v_list = {"v4", "ipv4", "v4x", "v6", "ipv6", "v6x"}
    for _, name in ipairs(v_list) do
        local version, err = xdb.version_from_name(name)
        if err ~= nil then
            print(string.format("version_from_name(%s): %s", name, err))
        else
            print(string.format("version_from_name(%s): %s", name, version))
        end
    end
end

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
            local ip_to_str = xdb.ip_to_string(ip_bytes)
            print(string.format(
                "parse_ip(`%s`)->{bytes:%d, to_string:%s, equal:%s}", 
                ip_str, #ip_bytes, ip_to_str, tostring(ip_str == ip_to_str)
            ))
        end
    end
end

function test_ip_compare()
    local ip_list = {
        {"1.0.0.0", "1.0.0.1", -1},
        {"192.168.1.101", "192.168.1.90", 1},
        {"219.133.111.87", "114.114.114.114", 1},
        {"2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff", -1},
        {"2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff", -1},
        {"ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff", 1}
    }

    for _,ip_pair in ipairs(ip_list) do
        local ip1 = xdb.parse_ip(ip_pair[1])
        local ip2 = xdb.parse_ip(ip_pair[2])
        local cmp = xdb.ip_compare(ip1, ip2)
        print(string.format("compare(%s, %s): %d ? %s", ip_pair[1], ip_pair[2], cmp, tostring(cmp == ip_pair[3])))
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


function test_ip_search()
    local test_list = {
        -- ipv4
        {"1.2.3.4", xdb.IPv4, "../../data/ip2region_v4.xdb"},
        -- ipv6
        {"240e:3b7:3272:d8d0:db09:c067:8d59:539e", xdb.IPv6, "../../data/ip2region_v6.xdb"}
    }

    for _, test in ipairs(test_list) do
        -- ipv6
        local ip_str = test[1]
        searcher, err = xdb.new_with_file_only(test[2], test[3])
        t_start = xdb.now()
        region, err = searcher:search_by_string(ip_str)
        if err ~= nil then
            print(string.format("failed to search(%s): %s", ip_str, err))
        else
            local c_time = xdb.now() - t_start
            print(string.format(
                "search(%s): {region:%s, io_count:%d, took:%dμs}", 
                ip_str, region, searcher:get_io_count(), c_time
            ))
        end
        searcher:close()
    end
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