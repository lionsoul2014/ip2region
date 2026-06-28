-ifndef(IP2REGION_HRL).
-define(IP2REGION_HRL, true).

-define(NONE, none).
-define(APP_NAME, ip2region).

-define(XDB_HEADER_SIZE, 256).
-define(XDB_VECTOR_COLS, 256).
-define(XDB_VECTOR_INDEX_SIZE, 8).
-define(XDB_VECTOR_INDEX_COUNT, (16#10000)). %% 256*256

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

%% Per-version pool names.
-define(IP2REGION_POOL_V4, ip2region_pool_v4).
-define(IP2REGION_POOL_V6, ip2region_pool_v6).

-record(xdb_header, {
    version :: non_neg_integer(),
    index_policy :: non_neg_integer(),
    created_at :: non_neg_integer(),
    start_index_ptr :: non_neg_integer(),
    end_index_ptr :: non_neg_integer(),
    ip_version :: non_neg_integer(),
    runtime_ptr_bytes :: non_neg_integer()
}).

-endif.
