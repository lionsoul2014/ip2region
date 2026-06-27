-module(ip2region_xdb_test).
-include_lib("eunit/include/eunit.hrl").

header_parse_test() ->
    HeaderBin = <<3:16/little, 1:16/little, 123456:32/little,
                  1000:32/little, 2000:32/little, 6:16/little, 4:16/little,
                  0:((256-20)*8)>>,
    {ok, Header} = ip2region_xdb:parse_header(HeaderBin),
    ?assertEqual(3, ip2region_xdb:header_version(Header)),
    ?assertEqual(6, ip2region_xdb:header_ip_version(Header)),
    ?assertEqual(1000, ip2region_xdb:header_start_index_ptr(Header)),
    ?assertEqual(2000, ip2region_xdb:header_end_index_ptr(Header)).

version_constants_test() ->
    ?assertEqual(14, ip2region_xdb:segment_index_size(ipv4)),
    ?assertEqual(38, ip2region_xdb:segment_index_size(ipv6)).
