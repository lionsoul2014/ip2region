-module(ip2region_xdb_test).

-include_lib("eunit/include/eunit.hrl").
-include("ip2region.hrl").

valid_v4_header_test() ->
    HeaderBin = <<2:16/little, 1:16/little, 0:32/little,
                  256:32/little, 1024:32/little,
                  ?IP_VERSION_4:16/little, 4:16/little>>,
    {ok, Header} = ip2region_xdb:parse_header(HeaderBin),
    ?assertEqual(2, ip2region_xdb:header_version(Header)),
    ?assertEqual(1, ip2region_xdb:header_index_policy(Header)),
    ?assertEqual(256, ip2region_xdb:header_start_index_ptr(Header)),
    ?assertEqual(1024, ip2region_xdb:header_end_index_ptr(Header)),
    ?assertEqual(?IP_VERSION_4, ip2region_xdb:header_ip_version(Header)),
    ?assertEqual(4, ip2region_xdb:header_runtime_ptr_bytes(Header)).

valid_v6_header_test() ->
    HeaderBin = <<3:16/little, 1:16/little, 0:32/little,
                  256:32/little, 1024:32/little,
                  ?IP_VERSION_6:16/little, 4:16/little>>,
    {ok, Header} = ip2region_xdb:parse_header(HeaderBin),
    ?assertEqual(3, ip2region_xdb:header_version(Header)),
    ?assertEqual(?IP_VERSION_6, ip2region_xdb:header_ip_version(Header)).

invalid_header_test() ->
    ?assertEqual({error, invalid_header}, ip2region_xdb:parse_header(<<0:128>>)).

table_helpers_test_() ->
    [
        ?_assertEqual(?XDB_VECTOR_INDEX_V4, ip2region_xdb:vector_index_table(ipv4)),
        ?_assertEqual(?XDB_VECTOR_INDEX_V6, ip2region_xdb:vector_index_table(ipv6)),
        ?_assertEqual(?XDB_SEGMENT_INDEX_V4, ip2region_xdb:segment_index_table(ipv4)),
        ?_assertEqual(?XDB_SEGMENT_INDEX_V6, ip2region_xdb:segment_index_table(ipv6)),
        ?_assertEqual(?IP2REGION_CACHE_V4, ip2region_xdb:cache_table(ipv4)),
        ?_assertEqual(?IP2REGION_CACHE_V6, ip2region_xdb:cache_table(ipv6))
    ].

segment_index_size_test_() ->
    [
        ?_assertEqual(14, ip2region_xdb:segment_index_size(ipv4)),
        ?_assertEqual(38, ip2region_xdb:segment_index_size(ipv6))
    ].
