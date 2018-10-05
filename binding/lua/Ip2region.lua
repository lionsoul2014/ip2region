--[[
Ip2region lua binding

@author chenxin<chenxin619315@gmail.com>
@date   2018/10/02
]]--

require("bit32");

local INDEX_BLOCK_LENGTH  = 12;
local TOTAL_HEADER_LENGTH = 8192;
local _M = {
    dbFile = "",
    dbFileHandler = "",
    dbBinStr = "",
    HeaderSip = "", 
    HeaderPtr = "",
    headerLen = 0,
    firstIndexPtr = 0,
    lastIndexPtr = 0, 
    totalBlocks = 0
};

_G["Ip2region"] = _M;

-- set the __index to itself
_M.__index = _M;

-- set the print meta-method
_M.__tostring = function(table)
    local t = {
        "dbFile=" .. table.dbFile,
        -- "dbFileHandler=" .. table.dbFileHandler,
        "headerLen=" .. table.headerLen,
        "firstIndexPtr" .. table.firstIndexPtr,
        "lastIndexPtr" .. table.lastIndexPtr,
        "totalBlocks" .. table.totalBlocks
    };

    return table.concat(t, ",");
end

--[[
construct method

@param  obj
@return Ip2region object
--]]
function _M.new(dbFile)
    obj = {};
    setmetatable(obj, _M);
    obj.dbFile = dbFile;
    return obj;
end


--[[
internal function to get a integer from a binary string

@param  dbBinStr
@param  idx
@return Integer
]]--
function getLong(bs, idx)
    local a1 = string.byte(string.sub(bs, idx, idx));
    local a2 = bit32.lshift(string.byte(string.sub(bs, idx+1, idx+1)),  8);
    local a3 = bit32.lshift(string.byte(string.sub(bs, idx+2, idx+2)), 16);
    local a4 = bit32.lshift(string.byte(string.sub(bs, idx+3, idx+3)), 24);

    local val = bit32.bor(a1, a2);
    val = bit32.bor(val, a3);
    val = bit32.bor(val, a4);

    return val;
end


--[[
internal function to convert the string ip to a long value

@param  ip
@return Integer
]]--
function _M.ip2long(self, ip)
    -- dynamic arguments checking
    -- to support object.ip2long and object:ip2long access
    if ( type(self) == "string") then
        ip = self;
    end

    local ini = 1;
    local iip = 0;
    local off = 24;
    while true do
        local pos = string.find(ip, '.', ini, true);
        if ( not pos ) then
            break;
        end

        local sub = string.sub(ip, ini, pos - 1);
        if ( string.len(sub) < 1 ) then
            return nil;
        end

        iip = bit32.bor(iip, bit32.lshift(tonumber(sub), off));
        ini = pos + 1;
        off = off - 8;
    end

    -- check if it is a valid ip address
    if ( off ~= 0 or ini > string.len(ip) ) then
        return nil;
    end

    local sub = string.sub(ip, ini);
    if ( string.len(sub) < 1 ) then
        return nil;
    end

    return bit32.bor(iip, bit32.lshift(tonumber(sub), off));
end


--[[
internal function to get the whole content of a file

@param  file
@return String
]]--
function get_file_contents(file)
    local fi = io.input(file);
    if ( not fi ) then
        return nil;
    end

    local str = io.read("*a");
    io.close();
    return str;
end


--[[
set the current db file path

@param  dbFile
]]--
function _M:setDbFile(dbFile)
    self.dbFile = dbFile;
end


--[[
all the db binary string will be loaded into memory
then search the memory only and this will a lot faster than disk base search
@Note: invoke it once before put it to public invoke could make it thread safe

@param  ip
@return table or nil for failed
]]--
function _M:memorySearch(ip)
    -- string ip conversion
    if ( type(ip) == "string" ) then
        ip = self:ip2long(ip);
        if ( ip == nil ) then
            return nil;
        end
    end;

    -- check and load the binary string for the first time
    if ( self.dbBinStr == "" ) then
        self.dbBinStr = get_file_contents(self.dbFile);
        if ( not self.dbBinStr ) then
            return nil;
        end

        self.firstIndexPtr = getLong(self.dbBinStr, 1);
        self.lastIndexPtr  = getLong(self.dbBinStr, 5);
        self.totalBlocks   = (self.lastIndexPtr - self.firstIndexPtr)/INDEX_BLOCK_LENGTH + 1;
    end


    -- binary search to define the data
    local l = 0;
    local h = self.totalBlocks;
    local dataPtr = 0;
    while ( l <= h ) do
        local m = math.floor((l + h) / 2);
        local p = self.firstIndexPtr + m * INDEX_BLOCK_LENGTH;
        local sip = getLong(self.dbBinStr, p + 1);
        if ( ip < sip ) then
            h = m - 1;
        else
            local eip = getLong(self.dbBinStr, p + 5);  -- 4 + 1
            if ( ip > eip ) then
                l = m + 1;
            else
                dataPtr = getLong(self.dbBinStr, p + 9); -- 8 + 1
                break;
            end
        end
    end

    -- not matched just stop it here
    if ( dataPtr == 0 ) then return nil end

    -- get the data
    local dataLen = bit32.band(bit32.rshift(dataPtr, 24), 0xFF);
    dataPtr = bit32.band(dataPtr, 0x00FFFFFF);
    local dptr = dataPtr + 5;   -- 4 + 1

    return {
        city_id = getLong(self.dbBinStr, dataPtr), 
        region  = string.sub(self.dbBinStr, dptr, dptr + dataLen - 5)
    };
end


--[[
get the data block through the specified ip address 
or long ip numeric with binary search algorithm

@param  ip
@return table or nil for failed
]]--
function _M:binarySearch(ip)
    -- check and conver the ip address
    if ( type(ip) == "string" ) then
        ip = self:ip2long(ip);
        if ( ip == nil ) then
            return nil;
        end
    end

    if ( self.totalBlocks == 0 ) then
        -- check and open the original db file
        self.dbFileHandler = io.open(self.dbFile, "r");
        if ( not self.dbFileHandler ) then
            return nil;
        end

        self.dbFileHandler:seek("set", 0);
        local superBlock = self.dbFileHandler:read(8);

        self.firstIndexPtr = getLong(superBlock, 1);    -- 0 + 1
        self.lastIndexPtr  = getLong(superBlock, 5);    -- 4 + 1
        self.totalBlocks   = (self.lastIndexPtr-self.firstIndexPtr)/INDEX_BLOCK_LENGTH + 1;
    end


    -- binary search to define the data
    local l = 0;
    local h = self.totalBlocks;
    local dataPtr = 0;
    while ( l <= h ) do
        local m = math.floor((l + h) / 2);
        local p = m * INDEX_BLOCK_LENGTH;
        self.dbFileHandler:seek("set", self.firstIndexPtr + p);
        local buffer = self.dbFileHandler:read(INDEX_BLOCK_LENGTH);
        local sip = getLong(buffer, 1); -- 0 + 1
        if ( ip < sip ) then
            h = m - 1;
        else
            local eip = getLong(buffer, 5);   -- 4 + 1
            if ( ip > eip ) then
                l = m + 1;
            else
                dataPtr = getLong(buffer, 9);   -- 8 + 1
                break;
            end
        end
    end

    -- not matched just stop it here
    if ( dataPtr == 0 ) then return nil; end


    -- get the data
    local dataLen = bit32.band(bit32.rshift(dataPtr, 24), 0xFF);
    dataPtr = bit32.band(dataPtr, 0x00FFFFFF);

    self.dbFileHandler:seek("set", dataPtr);
    local data = self.dbFileHandler:read(dataLen);

    return {
        city_id = getLong(data, 1),    -- 0 + 1
        region  = string.sub(data, 5)  -- 4 + 1
    };
end


--[[
get the data block associated with the specified ip with b-tree search algorithm

@param  ip
@return table or nil for failed
]]--
function _M:btreeSearch(ip)
    -- string ip to integer conversion
    if ( type(ip) == "string" ) then
        ip = self:ip2long(ip);
        if ( ip == nil ) then
            return nil;
        end
    end

    -- check and load the header
    if ( self.headerLen == 0 ) then
        -- check and open the original db file
        self.dbFileHandler = io.open(self.dbFile, 'r');
        if ( not self.dbFileHandler ) then
            return nil;
        end

        self.dbFileHandler:seek("set", 8);
        local buffer = self.dbFileHandler:read(TOTAL_HEADER_LENGTH);
        
        -- fill the header
        local i = 0;
        local idx = 0;
        self.HeaderSip = {};
        self.HeaderPtr = {};
        for i=0, TOTAL_HEADER_LENGTH, 8 do
            local startIp = getLong(buffer, i + 1); -- 0 + 1
            local dataPtr = getLong(buffer, i + 5); -- 4 + 1
            if ( dataPtr == 0 ) then
                break;
            end

            table.insert(self.HeaderSip, startIp);
            table.insert(self.HeaderPtr, dataPtr);
            idx = idx + 1;
        end

        self.headerLen = idx;
    end
    

    -- 1. define the index block with the binary search
    local l = 0; 
    local h = self.headerLen;
    local sptr = 0;
    local eptr = 0;
    while ( l <= h ) do
        local m = math.floor((l + h) / 2);
        -- perfetc matched, just return it
        if ( ip == self.HeaderSip[m] ) then
            if ( m > 0 ) then
                sptr = self.HeaderPtr[m-1];
                eptr = self.HeaderPtr[m  ];
            else
                sptr = self.HeaderPtr[m  ];
                eptr = self.HeaderPtr[m+1];
            end
            
            break;
        end
        
        -- less then the middle value
        if ( ip < self.HeaderSip[m] ) then
            if ( m == 0 ) then
                sptr = self.HeaderPtr[m  ];
                eptr = self.HeaderPtr[m+1];
                break;
            elseif ( ip > self.HeaderSip[m-1] ) then
                sptr = self.HeaderPtr[m-1];
                eptr = self.HeaderPtr[m  ];
                break;
            end
            h = m - 1;
        else
            if ( m == self.headerLen - 1 ) then
                sptr = self.HeaderPtr[m-1];
                eptr = self.HeaderPtr[m  ];
                break;
            elseif ( ip <= self.HeaderSip[m+1] ) then
                sptr = self.HeaderPtr[m  ];
                eptr = self.HeaderPtr[m+1];
                break;
            end
            l = m + 1;
        end
    end
    
    -- match nothing just stop it
    if ( sptr == 0 ) then return nil; end
    
    -- 2. search the index blocks to define the data
    self.dbFileHandler:seek("set", sptr);
    local blockLen = eptr - sptr;
    local index = self.dbFileHandler:read(blockLen + INDEX_BLOCK_LENGTH);
    local dataPtr = 0;

    l = 0;
    h = blockLen / INDEX_BLOCK_LENGTH;
    while ( l <= h ) do
        local m = math.floor((l + h) / 2);
        local p = m * INDEX_BLOCK_LENGTH;
        local sip = getLong(index, p + 1);       -- 0 + 1
        if ( ip < sip ) then
            h = m - 1;
        else
            local eip = getLong(index, p + 5);   -- 4 + 1
            if ( ip > eip ) then
                l = m + 1;
            else
                dataPtr = getLong(index, p + 9); -- 8 + 1
                break;
            end
        end
    end
    
    -- not matched
    if ( dataPtr == 0 ) then return nil; end
    
    -- 3. get the data
    local dataLen = bit32.band(bit32.rshift(dataPtr, 24), 0xFF);
    dataPtr = bit32.band(dataPtr, 0x00FFFFFF);
    
    self.dbFileHandler:seek("set", dataPtr);
    local data = self.dbFileHandler:read(dataLen);

    return {
        city_id = getLong(data, 1),     -- 0 + 1
        region  = string.sub(data, 5)   -- 4 + 1
    };
end


--[[
close the object and do the basic gc
]]--
function _M.close(self)
    if ( self.dbFileHandler ~= "" ) then
        self.dbFileHandler:close();
    end

    if ( self.dbBinStr ~= "" ) then
        self.dbBinStr = nil;
    end
end


return _M;
