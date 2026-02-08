/*
 * Created by Wu Jian Ping on - 2023/03/30.
 */

#ifndef __NGX_HTTP_IP2REGION_MODULE_H_INCLUDED__
#define __NGX_HTTP_IP2REGION_MODULE_H_INCLUDED__

#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include <xdb_api.h>

#if (NGX_HAVE_INET6)
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

typedef struct {
    xdb_searcher_t       searcher;
    xdb_vector_index_t  *v_index;
    xdb_content_t       *c_buffer;
} ip2region_searcher_t;

typedef struct {
    ip2region_searcher_t *v4_searcher;  // IPv4 searcher for ip2region_db
    ip2region_searcher_t *v6_searcher;  // IPv6 searcher for ip2region_db6
} ngx_http_ip2region_conf_t;

#endif
