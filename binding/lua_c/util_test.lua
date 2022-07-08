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
    print(string.format("xdb header buffer `%s` loaded", header))

    local tpl = [[
header: {
    version: %d
    index_policy: %d
    created_at: %d
    start_index_ptr: %d
    end_index_ptr: %d
}]]

    local t = header:to_table()
    print(string.format(tpl,
        t["version"], t["index_policy"], t["created_at"], t["start_index_ptr"], t["end_index_ptr"])
    )
end


print("\n--- testing load vector index ... ")
v_index, err = xdb.load_vector_index("../../data/ip2region.xdb")
if err ~= nil then
    print("failed to load vector index: ", err)
else
    print(string.format("xdb vector index buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
            v_index, v_index:name(), v_index:type(), v_index:length()))
    v_index:close()
end


print("\n--- testing load content buffer ... ")
c_buffer, err = xdb.load_content("../../data/ip2region.xdb")
if err ~= nil then
    print("failed to load content: ", err)
else
    print(string.format("xdb content buffer `%s` loaded, info={name=%s, type=%d, length=%d}",
            c_buffer, c_buffer:name(), c_buffer:type(), c_buffer:length()))
    c_buffer:close();
end


print("\n--- testing search ... ")
local ip_str = "1.2.3.4"
searcher, err = xdb.new_with_file_only("../../data/ip2region.xdb")
local t_start = xdb.now()
region, err = searcher:search(ip_str)
local c_time = xdb.now() - t_start
print(string.format("search(%s): {region=%s, io_count: %d, took: %dμs, err=%s}",
        ip_str, region, searcher:get_io_count(), c_time, err))
print(string.format("searcher.tostring=%s", searcher))
searcher:close()


print("")
print(string.format("all tests done, elapsed %d μs", xdb.now() - s_time))