-module(ip2region_sup_test).
-include_lib("eunit/include/eunit.hrl").
-include("ip2region.hrl").

pools_started_test() ->
    {ok, _} = ip2region_test_util:start_app(),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V4))),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V6))).
