#ifndef __NGX_HTTP_IP2REGION_MODULE_INCLUDED__
#define __NGX_HTTP_IP2REGION_MODULE_INCLUDED__

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

typedef struct {
    // xdb_searcher_t searcher;
    // xdb_vector_index_t *v_index;
    // xdb_content_t *c_buffer;
} ip2region_searcher_t;

typedef struct { 
    ip2region_searcher_t *ip2region_searcher;
} ngx_http_ip2region_conf_t;

#endif
