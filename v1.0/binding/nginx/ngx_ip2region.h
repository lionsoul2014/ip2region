#ifndef __NGX_IP2REGION_H__
#define __NGX_IP2REGION_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "../c/ip2region.h"

typedef struct ngx_ip2region_conf_s {
    ngx_str_t   db_file;
    ngx_uint_t  algo;
} ngx_ip2region_conf_t;

typedef uint_t (*search_func_ptr)(ip2region_t, uint_t, datablock_t);

extern ngx_int_t ngx_ip2region_search(ngx_str_t *addr_text, ngx_str_t *isp, ngx_str_t *city);

#ifdef __cplusplus
}
#endif

#endif
