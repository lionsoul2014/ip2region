-module(ip2region_util_test).
-include_lib("eunit/include/eunit.hrl").

ipv4_inputs_test_() ->
    [
        ?_assertEqual({ok, ipv4, <<1,0,8,0>>}, ip2region_util:ip_to_bytes("1.0.8.0")),
        ?_assertEqual({ok, ipv4, <<1,0,8,0>>}, ip2region_util:ip_to_bytes(<<"1.0.8.0">>)),
        ?_assertEqual({ok, ipv4, <<1,0,8,0>>}, ip2region_util:ip_to_bytes({1,0,8,0})),
        ?_assertEqual({ok, ipv4, <<1,0,8,0>>}, ip2region_util:ip_to_bytes(16779264)),
        ?_assertEqual(ipv4, ip2region_util:ip_version("1.0.8.0")),
        ?_assertEqual(ipv4, ip2region_util:ip_version({1,0,8,0}))
    ].

ipv6_inputs_test_() ->
    [
        ?_assertEqual({ok, ipv6, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1>>},
                      ip2region_util:ip_to_bytes("::1")),
        ?_assertEqual({ok, ipv6, <<0,0,0,0,0,0,0,0,0,0,255,255,1,0,8,0>>},
                      ip2region_util:ip_to_bytes("::ffff:1.0.8.0")),
        ?_assertEqual({ok, ipv6, <<0,0,0,0,0,0,0,0,0,0,255,255,1,0,8,0>>},
                      ip2region_util:ip_to_bytes(<<"::ffff:1.0.8.0">>)),
        ?_assertEqual({ok, ipv6, <<0,0,0,0,0,0,0,0,0,0,255,255,1,0,8,0>>},
                      ip2region_util:ip_to_bytes({0,0,0,0,0,65535,256,2048})),
        ?_assertEqual(ipv6, ip2region_util:ip_version("::1")),
        ?_assertEqual(ipv6, ip2region_util:ip_version({0,0,0,0,0,0,0,1}))
    ].

invalid_inputs_test_() ->
    [
        ?_assertEqual({error, bad_ip_format}, ip2region_util:ip_to_bytes("xxx.0.8.0")),
        ?_assertEqual({error, bad_ip_format}, ip2region_util:ip_to_bytes("::ggg")),
        ?_assertEqual({error, bad_ip_format}, ip2region_util:ip_to_bytes({1,2,3})),
        ?_assertEqual({error, bad_ip_format}, ip2region_util:ip_to_bytes(atom))
    ].

legacy_ipv4_to_n_test_() ->
    [
        ?_assertEqual(16779264, ip2region_util:ipv4_to_n("1.0.8.0")),
        ?_assertEqual(16779264, ip2region_util:ipv4_to_n(<<"1.0.8.0">>)),
        ?_assertEqual(16779264, ip2region_util:ipv4_to_n({1,0,8,0})),
        ?_assertEqual(16779264, ip2region_util:ipv4_to_n(16779264))
    ].
