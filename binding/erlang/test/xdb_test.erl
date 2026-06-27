-module(xdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(IPV6_RESULT, "United States|Florida|Miami|Google LLC|US").

search_test_() ->
    {ok, _} = ip2region_test_util:start_app(),
    A = "中国|广东省|广州市|中国电信|CN",
    Region0 = xdb:search("1.0.8.0"),
    Region1 = xdb:search(<<"1.0.8.0">>),
    Region2 = xdb:search({1,0,8,0}),
    Region3 = xdb:search("xxx.0.8.0"),
    [
        ?_assert(A =:= Region0),
        ?_assert(A =:= Region1),
        ?_assert(A =:= Region2),
        ?_assert({error, bad_ip_format} =:= Region3)
    ].

ipv6_search_test_() ->
    {ok, _} = ip2region_test_util:start_app(),
    [
        ?_assert(?IPV6_RESULT =:= xdb:search("2001:4860:4860::8888")),
        ?_assert(?IPV6_RESULT =:= xdb:search(<<"2001:4860:4860::8888">>)),
        ?_assert(?IPV6_RESULT =:= xdb:search({8193, 18528, 18528, 0, 0, 0, 0, 34952}))
    ].

invalid_search_test_() ->
    {ok, _} = ip2region_test_util:start_app(),
    [
        ?_assertEqual({error, bad_ip_format}, xdb:search("xxx.0.8.0")),
        ?_assertEqual({error, bad_ip_format}, xdb:search("::ggg")),
        ?_assertEqual({error, bad_ip_format}, xdb:search({1,2,3}))
    ].

ipv6_pool_not_configured_test() ->
    {ok, _} = ip2region_test_util:start_app([{ipv4, "ip2region.xdb"}]),
    ?assertEqual({error, pool_not_configured}, xdb:search("2001:4860:4860::8888")).

xdb_version_mismatch_test() ->
    {ok, _} = ip2region_test_util:start_app(),
    V4File = filename:join([
        ip2region_test_util:repo_root(), "binding", "erlang", "priv", "ip2region.xdb"
    ]),
    error_logger:tty(false),
    try
        ?assertEqual(
            {error, {xdb_version_mismatch, ipv6, ipv4}},
            ip2region_worker:start([{xdb_file, V4File}, {expected_version, ipv6}])
        )
    after
        error_logger:tty(true)
    end.
