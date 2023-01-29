-module(xdb_test).

-include_lib("eunit/include/eunit.hrl").

search_test_() ->
    application:ensure_started(ip2region),
    A = "中国|0|广东省|广州市|电信",
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