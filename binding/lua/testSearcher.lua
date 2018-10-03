-- ip2region test lua script
local cjson = require "cjson";
local Ip2region = require "Ip2region";

local searcher = Ip2region:new({dbFile = "../../data/ip2region.db"});
-- local data = searcher:memorySearch("120.79.17.142");
-- local data = searcher:binarySearch("120.79.17.142");
local data = searcher:btreeSearch("120.79.17.142");
print(cjson.encode(data));
