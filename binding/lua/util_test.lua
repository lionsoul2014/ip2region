-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/07/05

-- set the package path
package.path = "./?.lua"
package.cpath = "./?.so"

local xdb = require("xdb_searcher")

---- ip checking testing
print("--- testing check_ip and long2ip ... ")
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

---- buffer loading test
print("\n--- testing load header ... ")
header, err = xdb.load_header("../../data/ip2region.xdb")
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
}]]

    print(string.format(tpl,
        header["version"], header["index_policy"],
        header["created_at"], header["start_index_ptr"], header["end_index_ptr"])
    )
end


print("\n--- testing load vector index ... ")
v_index, err = xdb.load_vector_index("../../data/ip2region.xdb")
if err ~= nil then
    print("failed to load vector index: ", err)
else
    print("xdb vector index buffer loaded")
end


print("\n--- testing load content buffer ... ")
c_buffer, err = xdb.load_content("../../data/ip2region.xdb")
if err ~= nil then
    print("failed to load content: ", err)
else
    print("xdb content buffer loaded")
end


print("\n--- testing search ... ")
local ip_str = "1.2.3.4"
searcher, err = xdb.new_with_file_only("../../data/ip2region.xdb")
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


print("")
print(string.format("all tests done, elapsed %d μs", xdb.now() - s_time))