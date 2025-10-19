-- Copyright 2022 The Ip2Region Authors. All rights reserved.
-- Use of this source code is governed by a Apache2.0-style
-- license that can be found in the LICENSE file.
--
-- ---
-- @Author Lion <chenxin619315@gmail.com>
-- @Date   2022/07/05

-- constants define
local HeaderInfoLength = 256
local VectorIndexRows  = 256
local VectorIndexCols  = 256
local VectorIndexSize  = 8
local SegmentIndexSize = 14
local VectorIndexLength = 524288


local _xdb = {
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
_xdb.__index = _xdb
_xdb.__tostring = function(self)
    return "xdb searcher object (lua)"
end


-- construct functions

function newBase(dbPath, vIndex, cBuffer)
    local obj = setmetatable({}, _xdb)
    if cBuffer ~= nil then
        obj.io_count = 0
        obj.vector_index = nil
        obj.content_buff = cBuffer
    else
        obj.io_count = 0
        obj.vector_index = vIndex
        obj.handle = io.open(dbPath, "r")
        if obj.handle == nil then
            return nil, string.format("failed to open xdb file `%s`", dbPath)
        end
    end

    return obj, nil
end

function _xdb.new_with_file_only(dbPath)
    return newBase(dbPath, nil, nil)
end

function _xdb.new_with_vector_index(dbPath, vIndex)
    return newBase(dbPath, vIndex, nil)
end

function _xdb.new_with_buffer(cBuffer)
    return newBase(nil, nil, cBuffer)
end

-- End of constructors

-- object api impl, must call via ':'

function _xdb:search(ip_src)
    -- check and convert string ip to long ip
    local t, ip = type(ip_src), 0
    if t == nil then
        return "", string.format("invalid ip address `%s`", ip_src)
    elseif t == "string" then
        ip, err = self.check_ip(ip_src)
        if err ~= nil then
            return "", string.format("check ip `%s`: %s", ip_src, err)
        end
    elseif t ~= "number" then
        return "", "invalid number or string ip"
    else
        -- use the original value
        ip = ip_src
    end

    -- reset the global counter
    -- and global resource local cache
    self.io_count = 0
    local vector_index = self.vector_index
    local content_buff = self.content_buff
    local read_data = self.read

    -- locate the segment index based on the vector index
    local il0 = (ip >> 24) & 0xFF
    local il1 = (ip >> 16) & 0xFF
    local idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize
    local s_ptr, e_ptr = 0, 0
    if vector_index ~= nil then
        s_ptr = le_get_uint32(vector_index, idx + 1)
        e_ptr = le_get_uint32(vector_index, idx + 5)
    elseif content_buff ~= nil then
        s_ptr = le_get_uint32(content_buff, HeaderInfoLength + idx + 1)
        e_ptr = le_get_uint32(content_buff, HeaderInfoLength + idx + 5)
    else
        -- load from the file
        buff, err = read_data(self, HeaderInfoLength + idx, SegmentIndexSize)
        if err ~= nil then
            return "", string.format("read buffer: %s", err)
        end

        s_ptr = le_get_uint32(buff, 1)
        e_ptr = le_get_uint32(buff, 5)
    end

    -- print(string.format("s_ptr: %d, e_ptr: %d", s_ptr, e_ptr))
    -- binary search to get the data
    local data_ptr, data_len, p = 0, 0, 0
    local sip, eip, err, buff = 0, 0, ""
    local l, m, h = 0, 0, (e_ptr - s_ptr) / SegmentIndexSize
    while l <= h do
        m = (l + h) >> 1
        p = s_ptr + m * SegmentIndexSize

        -- read the segment index
        buff, err = read_data(self, p, SegmentIndexSize)
        if err ~= nil then
            return "", string.format("read segment index at %d", p)
        end

        sip = le_get_uint32(buff, 1)
        if ip < sip then
            h = m - 1
        else
            eip = le_get_uint32(buff, 5)
            if ip > eip then
                l = m + 1
            else
                data_len = le_get_uint16(buff, 9)
                data_ptr = le_get_uint32(buff, 11)
                break
            end
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

function _xdb:read(offset, length)
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

function _xdb:get_io_count()
    return self.io_count
end

function _xdb:close()
    if self.handle ~= nil then
        self.handle:close()
    end
end

-- End of search api


-- static util functions

function _xdb.load_header(dbPath)
    local handle = io.open(dbPath, "r")
    if handle == nil then
        return nil, string.format("failed to open xdb file `%s`", dbPath)
    end

    local r = handle:seek("set", 0)
    if r == nil then
        handle:close()
        return nil, "failed to seek to 0"
    end

    local c = handle:read(HeaderInfoLength)
    if c == nil then
        handle:close()
        return nil, string.format("failed to read %d bytes", HeaderInfoLength)
    end

    handle:close()
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

function _xdb.load_vector_index(dbPath)
    local handle = io.open(dbPath, "r")
    if handle == nil then
        return nil, string.format("failed to open xdb file `%s`", dbPath)
    end

    local r = handle:seek("set", HeaderInfoLength)
    if r == nil then
        handle:close()
        return nil, string.format("failed to seek to %d", HeaderInfoLength)
    end

    local c = handle:read(VectorIndexLength)
    if c == nil then
        handle:close()
        return nil, string.format("failed to read %d bytes", VectorIndexLength)
    end

    handle:close()
    return c, nil
end

function _xdb.load_content(dbPath)
    local handle = io.open(dbPath, "r")
    if handle == nil then
        return nil, string.format("failed to open xdb file `%s`", dbPath)
    end

    local c = handle:read("*a")
    if c == nil then
        handle:close()
        return nil, string.format("failed to read xdb content")
    end

    handle:close()
    return c, nil
end

--- ip parse and compare

function _xdb.check_ip(ip_str)
    local ip, id, v = 0, 1, 0
    local offset_arr = {24, 16, 8, 0}
    for p in string.gmatch(ip_str..".", "([%d]+)%.") do
        -- match pattern checking
        if p == nil then
            return 0, "err=1"
        end

        -- count checking
        if id > 4 then
            return 0, "err=1"
        end

        -- value checking
        v = tonumber(p)
        if v > 255 then
            return 0, "err=2"
        end

        ip = ip | (v << offset_arr[id])
        id = id + 1
    end

    if id ~= 5 then
        return 0, "err=1"
    end

    return ip, nil
end

function _xdb.long2ip(ip)
    return string.format("%d.%d.%d.%d", (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, (ip >> 8 ) & 0xFF, ip & 0xFF)
end

-- 
-- parse ip string
--
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

    local i, dc_num, offset, length = 1, 0, 1, #ps

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
            i = i + 1
        else
            local v = tonumber(s, 16);
            if v == nil then
                return nil, string.format("invalid ipv6 part `%s`, a valid hex number expected", ps[i])
            end

            if v < 0 or v > 0xFFFF then
                return nil, string.format("invalid ipv6 part `%s` should >= 0 and <= 65534", ps[i])
            end

            bytes[offset    ] = (v >> 8) & 0xFF
            bytes[offset + 1] = (v  & 0xFF)
            offset = offset + 2
            i = i + 1
        end
    end

    return string.char(table.unpack(bytes))
end

function _xdb.parse_ip(ip_str)
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
--
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

function _xdb.ip_to_string(ip_bytes, compress)
    local len = #ip_bytes
    if len == 4 then
        return _ipv4_to_string(ip_bytes)
    elseif len == 16 then
        return _ipv6_to_string(ip_bytes, compress)
    else
        return nil, string.format("invalid bytes ip with length not 4 or 6")
    end
end

-- 
-- ip bytes compare

function _xdb.ip_sub_compare(ip1, buff, offset)
    local ip2 = string.sub(buff, offset, offset + #ip1)
    if ip1 > ip2 then
        return 1
    elseif ip1 < ip2 then
        return -1
    else
        return 0
    end
end

function _xdb.ip_compare(ip1, ip2)
    return _xdb.ip_sub_compare(ip1, ip2, 1)
end

-- End of util functions

--internal function to get a integer from a binary string

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

-- this is a bit weird, but we have no better choice for now
function _xdb.now()
    return os.time() * 1e6
end

return _xdb