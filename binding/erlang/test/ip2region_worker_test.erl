-module(ip2region_worker_test).
-include_lib("eunit/include/eunit.hrl").
-include("ip2region.hrl").

worker_search_v4_binary_test() ->
    ip2region_sup:create_table(),
    {ok, Pid} = ip2region_worker:start_link([{xdb_file, "ip2region.xdb"}]),
    try
        Region = ip2region_worker:search(Pid, <<1,0,8,0>>),
        ?assert(is_list(Region))
    after
        ip2region_worker:stop(Pid)
    end.
