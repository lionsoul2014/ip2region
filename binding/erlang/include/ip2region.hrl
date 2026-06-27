-ifndef(IP2REGION_HRL).
-define(IP2REGION_HRL, true).

-define(NONE, none).
-define(APP_NAME, ip2region).

-define(XDB_VECTOR_INDEX, ets_xdb_vector_index).
-define(XDB_SEGMENT_INDEX, ets_xdb_segement_index).
-define(IP2REGION_CACHE, ets_ip2region_cache).


-define(XDB_HEADER_SIZE, 256).
-define(XDB_VECTOR_COLS, 256).
-define(XDB_VECTOR_INDEX_SIZE, 8). 
-define(XDB_VECTOR_INDEX_COUNT, (16#10000)). %% 256*256

-define(XDB_SEGMENT_INDEX_SIZE, 14).

-define(IP2REGION_POOL, ip2region_pool).

%% xdb header field offsets (bytes)
-define(XDB_HEADER_VERSION_OFFSET, 0).
-define(XDB_HEADER_INDEX_POLICY_OFFSET, 2).
-define(XDB_HEADER_CREATED_AT_OFFSET, 4).
-define(XDB_HEADER_START_INDEX_PTR_OFFSET, 8).
-define(XDB_HEADER_END_INDEX_PTR_OFFSET, 12).
-define(XDB_HEADER_IP_VERSION_OFFSET, 16).
-define(XDB_HEADER_RUNTIME_PTR_BYTES_OFFSET, 18).
-define(XDB_HEADER_INFO_LENGTH, 256).

%% IP versions as stored in xdb header
-define(IP_VERSION_4, 4).
-define(IP_VERSION_6, 6).

%% Segment index sizes (bytes): start_ip + end_ip + data_len + data_ptr
-define(XDB_SEGMENT_INDEX_SIZE_V4, 14).  %% 4 + 4 + 2 + 4
-define(XDB_SEGMENT_INDEX_SIZE_V6, 38).  %% 16 + 16 + 2 + 4

%% Per-version ETS tables
-define(XDB_VECTOR_INDEX_V4, ets_xdb_vector_index_v4).
-define(XDB_VECTOR_INDEX_V6, ets_xdb_vector_index_v6).
-define(XDB_SEGMENT_INDEX_V4, ets_xdb_segment_index_v4).
-define(XDB_SEGMENT_INDEX_V6, ets_xdb_segment_index_v6).
-define(IP2REGION_CACHE_V4, ets_ip2region_cache_v4).
-define(IP2REGION_CACHE_V6, ets_ip2region_cache_v6).

%% Per-version pool names. ?IP2REGION_POOL is kept as the legacy v4 pool name.
-define(IP2REGION_POOL_V4, ip2region_pool_v4).
-define(IP2REGION_POOL_V6, ip2region_pool_v6).

-ifndef(IF).
-define(IF(C, T, F), case (C) of true -> (T); false -> (F) end).
-define(IF(C, T), ?IF(C, T, skip)).
-endif.

-endif.