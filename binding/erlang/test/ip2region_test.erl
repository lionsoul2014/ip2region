-module(ip2region_test).

-include_lib("eunit/include/eunit.hrl").

search_test_() ->
    application:ensure_started(ip2region),
    A = #{
        city => <<"广州市"/utf8>>,
        country => <<"中国"/utf8>>,
        isp => <<"电信"/utf8>>,
        province => <<"广东省"/utf8>>,
        region => <<>>
    },
    Region1 = ip2region:search("1.0.8.0"),
    Region2 = ip2region:search({1,0,8,0}),
    Region3 = ip2region:search("xxx.0.8.0"),
    [
        ?_assert(A =:= Region1),
        ?_assert(A =:= Region2),
        ?_assert({error, bad_ip_format} =:= Region3)

    ].