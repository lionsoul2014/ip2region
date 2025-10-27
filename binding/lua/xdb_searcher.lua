-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/07/05

-- constants define
local header_info_length  = 256
local vector_index_rows   = 256
local vector_index_cols   = 256
local vector_index_size   = 8
local vector_index_length = 524288  -- cols x rows * 8

local xdb_structure_20 = 2
local xdb_structure_30 = 3
local xdb_ipv4_id = 4
local xdb_ipv6_id = 6

local xdb = {
    -- ip version
    version = nil,

    -- xdb file handle
    handle = nil,

    -- header info
    header = nil,
    io_count = 0,

    -- vector index
    vector_index = nil,

    -- xdb content buffer
    content_buff = nil
}

-- index and to string attribute set
xdb.__index = xdb
xdb.__tostring = function(self)
    return "xdb searcher object (lua)"
end

-- construct functions
function new_base(version, db_path, v_index, c_buffer)
    local obj = setmetatable({}, xdb)
    obj.version = version
    if c_buffer ~= nil then
        obj.io_count = 0
        obj.vector_index = nil
        obj.content_buff = c_buffer
    else
        obj.io_count = 0
        obj.vector_index = v_index
        obj.handle = io.open(db_path, "r")
        if obj.handle == nil then
            return nil, string.format("failed to open xdb file `%s`", db_path)
        end
    end

    return obj, nil
end

function xdb.new_with_file_only(version, db_path)
    return new_base(version, db_path, nil, nil)
end

function xdb.new_with_vector_index(version, db_path, v_index)
    return new_base(version, db_path, v_index, nil)
end

function xdb.new_with_buffer(version, c_buffer)
    return new_base(version, nil, nil, c_buffer)
end

-- End of constructors

-- object api impl, must call via ':'

function xdb:search_by_string(ip_str)
    local ip_bytes, err = xdb.parse_ip(ip_str)
    if err ~= nil then
        return "", string.format("failed to parse string ip `%s`: %s", ip_str, err)
    end

    return self:search(ip_bytes)
end

function xdb:search(ip_bytes)
    -- check the bytes ip
    if type(ip_bytes) ~= "string" then
        return "", string.format("invalid bytes ip `%s`", ip_bytes)
    end

    -- ip version check
    local version = self.version
    if #ip_bytes ~= version.bytes then
        return "", string.format("invalid ip address `%s` (%s expected)", xdb.ip_to_string(ip_bytes), version.name);
    end

    -- reset the global counter
    -- and global resource local cache
    self.io_count = 0
    local vector_index = self.vector_index
    local content_buff = self.content_buff
    local read_data = self.read

    -- locate the segment index based on the vector index
    local il0 = string.byte(ip_bytes, 1) & 0xFF
    local il1 = string.byte(ip_bytes, 2) & 0xFF
    local idx = il0 * vector_index_cols * vector_index_size + il1 * vector_index_size
    local s_ptr, e_ptr = 0, 0
    if vector_index ~= nil then
        s_ptr = le_get_uint32(vector_index, idx + 1)
        e_ptr = le_get_uint32(vector_index, idx + 5)
    elseif content_buff ~= nil then
        s_ptr = le_get_uint32(content_buff, header_info_length + idx + 1)
        e_ptr = le_get_uint32(content_buff, header_info_length + idx + 5)
    else
        -- load from the file
        buff, err = read_data(self, header_info_length + idx, vector_index_size)
        if err ~= nil then
            return "", string.format("read buffer: %s", err)
        end

        s_ptr = le_get_uint32(buff, 1)
        e_ptr = le_get_uint32(buff, 5)
    end

    -- print(string.format("s_ptr: %d, e_ptr: %d", s_ptr, e_ptr))
    -- binary search to get the data
    local index_size, ip_sub_compare = version.index_size, version.ip_sub_compare
    local bytes, d_bytes = version.bytes, version.bytes << 1
    local data_ptr, data_len, p = 0, 0, 0
    local buff, err = nil, nil
    local l, m, h = 0, 0, (e_ptr - s_ptr) / index_size
    while l <= h do
        m = (l + h) >> 1
        p = s_ptr + m * index_size

        -- read the segment index
        buff, err = read_data(self, p, index_size)
        if err ~= nil then
            return "", string.format("read segment index at %d", p)
        end

        -- check the index
        if ip_sub_compare(ip_bytes, buff, 1) < 0 then
            h = m - 1
        elseif ip_sub_compare(ip_bytes, buff, bytes + 1) > 0 then
            l = m + 1
        else
            data_len = le_get_uint16(buff, d_bytes + 1)
            data_ptr = le_get_uint32(buff, d_bytes + 3)
            break
        end
    end

    -- matching nothing interception
    -- print(string.format("data_len=%d, data_ptr=%d", data_len, data_ptr))
    if data_len == 0 then
        return "", nil
    end

    -- load and return the region data
    buff, err = read_data(self, data_ptr, data_len)
    if err ~= nil then
        return "", string.format("read data at %d:%d", data_ptr, data_len)
    end

    return buff, nil
end


-- read specified bytes from the specified index

function xdb:read(offset, length)
    -- local cache
    local content_buff = self.content_buff
    local handle = self.handle

    -- check the in-memory buffer first
    if content_buff ~= nil then
        return string.sub(content_buff, offset + 1, offset + length), nil
    end

    -- read from the file
    local r = handle:seek("set", offset)
    if r == nil then
        return nil, string.format("seek to offset %d", offset)
    end

    self.io_count = self.io_count + 1
    local buff = handle:read(length)
    if buff == nil then
        return nil, string.format("read %d bytes", length)
    end

    return buff, nil
end

function xdb:get_ip_version()
    return self.version
end

function xdb:get_io_count()
    return self.io_count
end

function xdb:close()
    if self.handle ~= nil then
        self.handle:close()
    end
end

---
-- internal function to decode buffer
function le_get_uint32(buff, idx)
    local i1 = (string.byte(buff, idx))
    local i2 = (string.byte(buff, idx+1) << 8)
    local i3 = (string.byte(buff, idx+2) << 16)
    local i4 = (string.byte(buff, idx+3) << 24)
    return (i1 | i2 | i3 | i4)
end

function le_get_uint16(buff, idx)
    local i1 = (string.byte(buff, idx))
    local i2 = (string.byte(buff, idx+1) << 8)
    return (i1 | i2)
end

-- static util functions

function xdb.open_file(db_path, mode)
    local t, handle = type(db_path), nil
    local _closer = nil
    if t == "userdata" then
        handle = db_path   -- file handle
        _closer = function(caller) end
    elseif t == "string" then
        handle = io.open(db_path, mode)
        if handle == nil then
            return nil, nil, string.format("failed to open xdb file `%s`", db_path)
        end

        _closer = function(caller) 
            handle:close()
        end
    end

    return handle, _closer, nil
end

function xdb.load_header(db_path)
    local handle, closer, err = xdb.open_file(db_path, "rb")
    if err ~= nil then
        return nil, err
    end

    local r = handle:seek("set", 0)
    if r == nil then
        closer("load_header")
        return nil, "failed to seek to 0"
    end

    local c = handle:read(header_info_length)
    if c == nil then
        closer("load_header")
        return nil, string.format("failed to read %d bytes", header_info_length)
    end

    closer("load_header")
    return {
        ["version"] = le_get_uint16(c, 1),
        ["index_policy"] = le_get_uint16(c, 3),
        ["created_at"] = le_get_uint32(c, 5),
        ["start_index_ptr"] = le_get_uint32(c, 9),
        ["end_index_ptr"] = le_get_uint32(c, 13),

        -- xdb 3.0 since IPv6 supporting
        ["ip_version"] = le_get_uint16(c, 17),
        ["runtime_ptr_bytes"] = le_get_uint16(c, 19),

        ["raw_data"] = c
    }, nil
end

function xdb.load_vector_index(db_path)
    local handle, closer, err = xdb.open_file(db_path, "rb")
    if err ~= nil then
        return nil, err
    end

    local r = handle:seek("set", header_info_length)
    if r == nil then
        closer("load_vector_index")
        return nil, string.format("failed to seek to %d", header_info_length)
    end

    local c = handle:read(vector_index_length)
    if c == nil then
        closer("load_vector_index")
        return nil, string.format("failed to read %d bytes", vector_index_length)
    end

    closer("load_vector_index")
    return c, nil
end

function xdb.load_content(db_path)
    local handle, closer, err = xdb.open_file(db_path, "rb")
    local c = handle:read("*a")
    if c == nil then
        closer("load_content")
        return nil, string.format("failed to read xdb content")
    end

    closer("load_content")
    return c, nil
end

-- Verify if the current Searcher could be used to search the specified xdb file.
-- Why do we need this check ?
-- The future features of the xdb impl may cause the current searcher not able to work properly.
--
-- @Note: You Just need to check this ONCE when the service starts
-- Or use another process (eg, A command) to check once Just to confirm the suitability.
function xdb.verify(db_path)
    local handle, closer, err = xdb.open_file(db_path, "rb")
    if err ~= nil then
        return err
    end

    -- load the header from handle
    local header, err = xdb.load_header(handle)
    if err ~= nil then
        closer()
        return string.format("failed to load header: %s", err)
    end

    -- get the runtime ptr bytes
    local runtime_ptr_bytes = 0
    if header.version == xdb_structure_20 then
        runtime_ptr_bytes = 4
    elseif header.version == xdb_structure_30 then
        runtime_ptr_bytes = header.runtime_ptr_bytes
    else
        closer()
        return string.format("invalid structure version %d", header.version);
    end

    -- 1, confirm the xdb file size
    -- to ensure that the maximum file pointer does not overflow
    local max_file_ptr = ((1 << (runtime_ptr_bytes * 8)) & 0xFFFFFFFFFFFFFFFF) - 1
    local _file_bytes  = handle:seek("end", 0)
    -- print("max_file_ptr=", max_file_ptr, "_file_bytes", _file_bytes)
    if _file_bytes > max_file_ptr then
        closer()
        return string.format("xdb file exceeds the maximum supported bytes: %d", max_file_ptr);
    end

    closer()
    return nil
end

-- 
-- parse ip string
function split(str, sep)
    local ps, sIndex, length = {}, 1, #str
    -- loop to find all parts
    while true do
        local mi = string.find(str, sep, sIndex, true)
        if mi == nil then
            table.insert(ps, string.sub(str, sIndex))
            break
        end

        if sIndex == mi then
            table.insert(ps, "")
        else 
            table.insert(ps, string.sub(str, sIndex, mi - 1))
        end

        -- reset the start index
        sIndex = mi + 1
    end

    return ps
end

function _parse_ipv4_addr(v4_str)
    local ps = split(v4_str, ".")
    if #ps ~= 4 then
        return nil, string.format("invalid ipv4 address `%s`", v4_str)
    end

    local bytes = {0x00, 0x00, 0x00, 0x00}
    for i, s in ipairs(ps) do
        local v = tonumber(s)
        if v == nil then
            return nil, string.format("invalid ipv4 part `%s`, a valid number expected", s)
        end

        if v < 0 or v > 255 then
            return nil, string.format("invalid ipv4 part `%s`, should <=0 and <= 255", s)
        end

        bytes[i] = v
    end

    return string.char(table.unpack(bytes)), nil
end

function _parse_ipv6_addr(v6_str)
    local ps = split(v6_str, ':')
    if #ps < 3 or #ps > 8 then
        return nil, string.format("invalid ipv6 address `%s`", v6_str)
    end

    local bytes = {
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00
    }

    local i, v, dc_num, offset, length = 1, 0, 0, 1, #ps

    -- process the v6 parts
    while i <= length do
        local s = ps[i]:match("^%s*(.-)%s*$")
        -- Double colon check and auto padding
        if #s == 0 then
            -- ONLY one double colon allow
            if dc_num > 0 then
                return nil, "invalid ipv6 address: multi double colon detected"
            end

            -- clear all the consecutive spaces
            local start = i
            i = i + 1
            while true do
                s = ps[i]:match("^%s*(.-)%s*$")
                if #s > 0 then
                    i = i - 1
                    break
                end

                if i >= length then
                    break
                end

                i = i + 1
            end

            dc_num = 1
            -- padding = 9 - start - left
            local padding = 9 - start - (length - i)
            offset = offset + 2 * padding
            -- print("-> i ", i, "start", start, "padding: ", padding, "offset", offset)
            goto continue
        end

        v = tonumber(s, 16);
        if v == nil then
            return nil, string.format("invalid ipv6 part `%s`, a valid hex number expected", ps[i])
        end

        if v < 0 or v > 0xFFFF then
            return nil, string.format("invalid ipv6 part `%s` should >= 0 and <= 65534", ps[i])
        end

        bytes[offset] = (v >> 8) & 0xFF
        bytes[offset+1] = (v  & 0xFF)
        offset = offset + 2

        ::continue::
        i = i + 1
    end

    return string.char(table.unpack(bytes))
end

function xdb.parse_ip(ip_str)
    local s_dot = string.find(ip_str, ".", 1, true)
    local c_dot = string.find(ip_str, ":", 1, true)
    if s_dot ~= nil and c_dot == nil then
        return _parse_ipv4_addr(ip_str)
    elseif c_dot ~= nil then
        return _parse_ipv6_addr(ip_str)
    else
        return nil, string.format("invalid ip address `%s`", ip_str)
    end
end

--
-- ip to string
function _ipv4_to_string(ip_bytes)
    return string.format(
        "%d.%d.%d.%d", 
        string.byte(ip_bytes, 1), 
        string.byte(ip_bytes, 2),
        string.byte(ip_bytes, 3),
        string.byte(ip_bytes, 4)
    ), nil
end

function _ipv6_to_string(ip_bytes, compress)
    local ps, i, hex = {}, 0, 0
    local last_hex, need_compress = -1, false
    for i = 1, #ip_bytes, 2 do
        hex = (string.byte(ip_bytes, i) << 8) | string.byte(ip_bytes, i + 1)
        table.insert(ps, string.format("%x", hex))

        -- check the necessity for compress
        if last_hex > -1 
            and hex == 0 
            and last_hex == 0 then
            need_compress = true
        end

        -- reset the last hex
        last_hex = hex
    end

    -- print('need_compress', need_compress)
    if need_compress == false
        or (compress ~= nil and compress == false) then
        return table.concat(ps, ':'), nil
    end

    -- auto compression of consecutive zero
    local i, j, length, mi = 1, 0, #ps, #ps + 1
    local _ = {}
    while i <= length do
        if i >= length or j > 0 then
            table.insert(_, ps[i])
            goto continue
        end

        if ps[i] ~= '0' or ps[i+1] ~= '0' then
            table.insert(_, ps[i])
            goto continue
        end

        -- find the first two zero part
        -- and keep find all the zero part
        i = i + 2
        while i <= length do
            if ps[i] ~= '0' then
                i = i - 1
                break
            end
            i = i + 1
        end

        -- make sure there is an empty head.
        if #_ == 0 then
            table.insert(_, '')
        end

        table.insert(_, '') -- empty for double colon

        -- make sure there is an empty tail
        if i == mi and #_ < length then
            table.insert(_, '')
        end
        
        --continue
        ::continue::
        i = i + 1
    end

    return table.concat(_, ':')
end

function xdb.ip_to_string(ip_bytes, compress)
    local l = #ip_bytes
    if l == 4 then
        return _ipv4_to_string(ip_bytes)
    elseif l == 16 then
        return _ipv6_to_string(ip_bytes, compress)
    else
        return nil, string.format("invalid bytes ip with length not 4 or 6")
    end
end

-- 
-- ip bytes compare
function xdb.ip_sub_compare(ip1, buff, offset)
    local ip2 = string.sub(buff, offset, offset + #ip1 - 1)
    if ip1 > ip2 then
        return 1
    elseif ip1 < ip2 then
        return -1
    else
        return 0
    end
end

function xdb.ip_compare(ip1, ip2)
    return xdb.ip_sub_compare(ip1, ip2, 1)
end

-- this is a bit weird
-- but we have no better choice for now
function xdb.now()
    return os.time() * 1e6
end

---
-- ip version

local Version = {
    __tostring = function(t)
        return string.format(
            '{id:%d, name:%s, bytes:%d, index_size:%d}',
            t.id, t.name, t.bytes, t.index_size
        )
    end
}

local IPv4 = {
    id = xdb_ipv4_id,
    name = "IPv4",
    bytes = 4,
    index_size = 14,  -- 14 = 4 + 4 + 2 + 4
    ip_sub_compare = function(ip1, buff, offset)
        -- ip1: Big endian byte order parsed from input
        -- ip2: Little endian byte order read from xdb index.
        -- @Note: to compatible with the old Litten endian index encode implementation.
        local l = #ip1
        local j = offset + l - 1
        for i = 1, l, 1 do
            local i1 = string.byte(ip1, i)
            local i2 = string.byte(buff, j)
            if i1 > i2 then
                return 1
            end

            if i1 < i2 then
                return -1
            end
            
            j = j - 1
        end

        return 0
    end
}

local IPv6 = {
    id = xdb_ipv6_id,
    name = "IPv6",
    bytes = 16,
    index_size = 38,  -- 38 = 16 + 16 + 2 + 4
    ip_sub_compare = xdb.ip_sub_compare
}

setmetatable(IPv4, Version)
setmetatable(IPv6, Version)

xdb.version_from_name = function(name)
    local n = string.upper(name)
    if n == "V4" or n == "IPV4" then
        return IPv4, nil
    elseif n == "V6" or n == "IPV6" then
        return IPv6, nil
    else
        return nil, string.format("invalid version name `%s`", name)
    end
end

xdb.version_from_header = function(header)
    -- old structure with ONLY IPv4 supporting
    if header.version == xdb_structure_20 then
        return IPv4, nil
    end

    -- structure 3.0 with IPv6 supporting
    if header.version ~= xdb_structure_30 then
        return nil, string.format("unsupported structure version `%d`", header.version)
    end

    local ip_ver = header.ip_version
    if ip_ver == xdb_ipv4_id then
        return IPv4, nil
    elseif ip_ver == xdb_ipv6_id then
        return IPv6, nil
    else
        return nil, string.format("unkown ip version id `%d`", ip_ver)
    end
end

-- constants register
xdb.ipv4_id = xdb_ipv4_id
xdb.ipv6_id = xdb_ipv6_id
xdb.structure_20 = xdb_structure_20
xdb.structure_30 = xdb_structure_30
xdb.IPv4 = IPv4
xdb.IPv6 = IPv6

return xdb