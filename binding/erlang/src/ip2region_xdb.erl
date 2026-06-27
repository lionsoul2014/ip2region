%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%%
%% @doc
%% ip2region xdb binary format helpers.
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_xdb).

-export([parse_header/1]).
-export([header_version/1, header_index_policy/1, header_created_at/1,
         header_start_index_ptr/1, header_end_index_ptr/1,
         header_ip_version/1, header_runtime_ptr_bytes/1]).
-export([segment_index_size/1]).

-include("ip2region.hrl").

-record(xdb_header, {
    version :: non_neg_integer(),
    index_policy :: non_neg_integer(),
    created_at :: non_neg_integer(),
    start_index_ptr :: non_neg_integer(),
    end_index_ptr :: non_neg_integer(),
    ip_version :: non_neg_integer(),
    runtime_ptr_bytes :: non_neg_integer()
}).

-spec parse_header(binary()) -> {ok, #xdb_header{}} | {error, invalid_header}.
parse_header(Bin) when is_binary(Bin), byte_size(Bin) >= 20 ->
    <<Version:16/little, IndexPolicy:16/little, CreatedAt:32/little,
      StartIndexPtr:32/little, EndIndexPtr:32/little,
      IpVersion:16/little, RuntimePtrBytes:16/little, _/binary>> = Bin,
    {ok, #xdb_header{
        version = Version,
        index_policy = IndexPolicy,
        created_at = CreatedAt,
        start_index_ptr = StartIndexPtr,
        end_index_ptr = EndIndexPtr,
        ip_version = IpVersion,
        runtime_ptr_bytes = RuntimePtrBytes
    }};
parse_header(_) ->
    {error, invalid_header}.

header_version(#xdb_header{version = V}) -> V.
header_index_policy(#xdb_header{index_policy = P}) -> P.
header_created_at(#xdb_header{created_at = T}) -> T.
header_start_index_ptr(#xdb_header{start_index_ptr = P}) -> P.
header_end_index_ptr(#xdb_header{end_index_ptr = P}) -> P.
header_ip_version(#xdb_header{ip_version = V}) -> V.
header_runtime_ptr_bytes(#xdb_header{runtime_ptr_bytes = B}) -> B.

-spec segment_index_size(ipv4 | ipv6) -> pos_integer().
segment_index_size(ipv4) -> ?XDB_SEGMENT_INDEX_SIZE_V4;
segment_index_size(ipv6) -> ?XDB_SEGMENT_INDEX_SIZE_V6.
