--[[
ip2region lua client test script

@author chenxin<chenxin619315@gmail.com>
]]--

-- check the command line arguments
if ( not arg[1] ) then
    print([[
Usage: lua testSearcher.lua [ip2region db file] [algorithm]
+-Optional Algorithm: binary, b-tree, memory]]);
    os.exit();
end

local Ip2region = require "Ip2region";
-- local cjson = require "cjson";
-- local socket = require "socket";


-- check and parse the dbFile and the method algorithm
-- Create a new ip2region object by the new interface
local ip2region = Ip2region.new(arg[1]);

-- reset the dbFile by the follow two ways:
-- ip2region.dbFile = arg[1];
-- ip2region:setDbFile(arg[1]);


local algorithm = "btree";
if ( arg[2] ~= nil ) then
    local arg_2 = string.lower(arg[2]);
    if ( arg_2 ~= "binary" and arg_2 ~= "memory" ) then
        algorithm = "binary";
    elseif ( arg_2 == "memory" ) then
        algorithm = "memory";
    end
end



-- local data = searcher:memorySearch("120.79.17.142");
-- local data = searcher:binarySearch("120.79.17.142");
-- local data = searcher:btreeSearch("120.79.17.142");
print("initializing " .. algorithm ..[[ 
+----------------------------------+
| ip2region test script            |
| Author: chenxin619315@gmail.com  |
| Type 'quit' to exit program      |
+----------------------------------+]]
);

while ( true ) do
    io.write("ip2region>> ");
    io.input(io.stdin);
    local line = io.read();
    if ( line == nil ) then
        -- do nothing
        break;
    elseif ( line == "quit" ) then
        break;
    elseif ( ip2region.ip2long(line) == nil ) then
        print("Invalid ip address=", line);
    else
        local data;
        local s_time = os.clock();
        if ( algorithm == "btree" ) then
            data = ip2region:btreeSearch(line);
        elseif ( algorithm == "binary" ) then
            data = ip2region:binarySearch(line);
        elseif ( algorithm == "memory" ) then
            data = ip2region:memorySearch(line);
        end

        local cost_time = (os.clock() - s_time) * 1000; -- to millseconds
        if ( data == nil ) then
            io.write("Failed for ip=", line, " is it a valid ip address ?");
        else
            io.write(string.format("%u|%s in %5f millseconds\n", data.city_id, data.region, cost_time));
        end
    end
end

-- close the object
-- print(ip2region);
ip2region:close();
