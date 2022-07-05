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
local _M = {
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
_M.__index = _M
_M.__tostring = function(self)
    return "xdb searcher object"
end


-- --- construct functions
function newBase(dbPath, vIndex, cBuffer)
    local obj = setmetatable({}, _M)
    if cBuffer ~= nil then
        obj.io_count = 0
        obj.vector_index = nil
        obj.content_buff = cBuffer
    else
        obj.io_count = 0
        obj.vector_index = vIndex
        obj.handle = io.open(dbPath, "r")
        if obj.handle == nil then
            error(string.format("failed to open xdb file `%s`", dbPath), 2)
        end
    end

    return obj
end

function _M.new_with_file_only(dbPath)
    return newBase(dbPath, nil, nil)
end

function _M.new_with_vector_index(dbPath, vIndex)
    return newBase(dbPath, vIndex, nil)
end

function _M.new_with_buffer(cBuffer)
    return newBase(nil, nil, cBuffer)
end

-- End of constructors

-- object api impl
-- must call via ':'
function _M:search(ip_src)
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
    end

    -- reset the global counter
    self.io_count = 0

    -- locate the segment index based on the vector index
    local il0 = (ip >> 24) & 0xFF
    local il1 = (ip >> 16) & 0xFF
    local idx = il0 * VectorIndexCols * SegmentIndexSize + il1 * SegmentIndexSize
    local s_ptr, e_ptr = 0, 0
    if self.vector_index ~= nil then
        s_ptr = getLong(self.vector_index, idx)
        e_ptr = getLong(self.vector_index, idx + 4)
    elseif self.content_buff ~= nil then
        s_ptr = getLong(self.content_buff, HeaderInfoLength + idx)
        e_ptr = getLong(self.content_buff, HeaderInfoLength + idx + 4)
    else
        -- load from the file
    end

    return "", "not implemented yet"
end

function _M:get_io_count()
    return self.io_count
end

function _M:close()
    if self.handle ~= nil then
        self.handle:close()
    end
end
-- End of search api


-- static util functions
function _M.load_header(dbPath)
    return nil, "not implemented yet"
end

function _M.load_vector_index(dbPath)
    return nil, "not implemented yet"
end

function _M.load_content(dbPath)
    return nil, "not implemented yet"
end

function _M.check_ip(ip_str)
    local ip, id = 0, 1
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

function _M.long2ip(ip)
    return string.format("%d.%d.%d.%d", (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, (ip >> 8 ) & 0xFF, ip & 0xFF)
end

function _M.now()
    return 0
end
-- End of util functions

return _M