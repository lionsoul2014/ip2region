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

-ifndef(IF).
-define(IF(C, T, F), case (C) of true -> (T); false -> (F) end).
-define(IF(C, T), ?IF(C, T, skip)).
-endif.

-endif.