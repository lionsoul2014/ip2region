-module(ip2region_sup_test).
-include_lib("eunit/include/eunit.hrl").
-include("ip2region.hrl").

pools_started_test() ->
    application:stop(ip2region),
    application:unload(ip2region),
    ok = application:load(ip2region),
    {ok, Cwd} = file:get_cwd(),
    V6File = filename:join([Cwd, "..", "..", "data", "ip2region_v6.xdb"]),
    ok = application:set_env(ip2region, db, [
        {ipv4, "ip2region.xdb"},
        {ipv6, V6File}
    ]),
    application:ensure_started(ip2region),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V4))),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V6))).
