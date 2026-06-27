-module(xdb_test).

-include_lib("eunit/include/eunit.hrl").

search_test_() ->
    application:ensure_started(ip2region),
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
    application:ensure_started(ip2region),
    [
        ?_assert(is_list(xdb:search("2001:4860:4860::8888"))),
        ?_assert(is_list(xdb:search(<<"2001:4860:4860::8888">>))),
        ?_assert(is_list(xdb:search({8193, 10304, 10304, 0, 0, 0, 0, 34952})))
    ].

invalid_search_test_() ->
    application:ensure_started(ip2region),
    [
        ?_assertEqual({error, bad_ip_format}, xdb:search("xxx.0.8.0")),
        ?_assertEqual({error, bad_ip_format}, xdb:search("::ggg")),
        ?_assertEqual({error, bad_ip_format}, xdb:search({1,2,3}))
    ].