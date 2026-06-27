-module(xdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(IPV6_RESULT, "United States|Florida|Miami|Google LLC|US").

search_test_() ->
    {ok, _} = application:ensure_all_started(ip2region),
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
    setup_ipv6(),
    [
        ?_assert(?IPV6_RESULT =:= xdb:search("2001:4860:4860::8888")),
        ?_assert(?IPV6_RESULT =:= xdb:search(<<"2001:4860:4860::8888">>)),
        ?_assert(?IPV6_RESULT =:= xdb:search({8193, 18528, 18528, 0, 0, 0, 0, 34952}))
    ].

invalid_search_test_() ->
    {ok, _} = application:ensure_all_started(ip2region),
    [
        ?_assertEqual({error, bad_ip_format}, xdb:search("xxx.0.8.0")),
        ?_assertEqual({error, bad_ip_format}, xdb:search("::ggg")),
        ?_assertEqual({error, bad_ip_format}, xdb:search({1,2,3}))
    ].

ipv6_pool_not_configured_test() ->
    try application:stop(ip2region) catch _:_ -> ok end,
    try application:unload(ip2region) catch _:_ -> ok end,
    ok = application:load(ip2region),
    ok = application:set_env(ip2region, db, [{ipv4, "ip2region.xdb"}]),
    {ok, _} = application:ensure_all_started(ip2region),
    ?assertEqual({error, pool_not_configured}, xdb:search("2001:4860:4860::8888")).

setup_ipv6() ->
    try application:stop(ip2region) catch _:_ -> ok end,
    try application:unload(ip2region) catch _:_ -> ok end,
    ok = application:load(ip2region),
    RepoRoot = repo_root(),
    V6File = filename:join([RepoRoot, "data", "ip2region_v6.xdb"]),
    ok = application:set_env(ip2region, db, [
        {ipv4, "ip2region.xdb"},
        {ipv6, V6File}
    ]),
    {ok, _} = application:ensure_all_started(ip2region).

repo_root() ->
    TestDir = filename:dirname(?FILE),
    ErlangDir = filename:dirname(TestDir),
    BindingDir = filename:dirname(ErlangDir),
    filename:dirname(BindingDir).
