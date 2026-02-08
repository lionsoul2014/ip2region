/*
 * Created by Wu Jian Ping on - 2023/03/30.
 */

#include "ngx_http_ip2region_module.h"

static ngx_int_t ngx_http_ip2region_is_absolute_path(char *name);
static char *ngx_http_ip2region_init_searcher(ngx_conf_t *cf, char *db_name,
    char *cache_policy, xdb_version_t *expected_version, const char *directive_name);

static ngx_int_t ngx_http_ip2region_add_variables(ngx_conf_t *cf);

static void *ngx_http_ip2region_create_conf(ngx_conf_t *cf);
static void ngx_http_ip2region_cleanup(void *data);
static char *ngx_http_ip2region_init(ngx_conf_t *cf,
    ngx_command_t *cmd, void *conf);

static ngx_int_t ngx_http_ip2region_variable(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data);

static ngx_http_module_t ngx_http_ip2region_ctx = {
    ngx_http_ip2region_add_variables,                                          /* pre configuration */
    NULL,                                                                      /* post configuration */
    ngx_http_ip2region_create_conf,                                            /* create main configuration */
    NULL,                                                                      /* init main configuration */
    NULL,                                                                      /* create server configuration */
    NULL,                                                                      /* merge server configuration */
    NULL,                                                                      /* create location configuration */
    NULL                                                                       /* merge location configuration */
};


static char *ngx_http_ip2region_init(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static char *ngx_http_ip2region_init_v6(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);

static ngx_command_t ngx_http_ip2region_commands[] = {
    { ngx_string("ip2region_db"),
      NGX_HTTP_MAIN_CONF | NGX_CONF_TAKE12,
      ngx_http_ip2region_init,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },

    { ngx_string("ip2region_db6"),
      NGX_HTTP_MAIN_CONF | NGX_CONF_TAKE12,
      ngx_http_ip2region_init_v6,
      NGX_HTTP_MAIN_CONF_OFFSET,
      0,
      NULL },

    ngx_null_command
};


/* ngx_module_t is required, otherwise failed at complie time */
ngx_module_t ngx_http_ip2region_module = {
    NGX_MODULE_V1,
    &ngx_http_ip2region_ctx,                                                   /* module context */
    ngx_http_ip2region_commands,                                               /* module directives */
    NGX_HTTP_MODULE,                                                           /* module type */
    NULL,                                                                      /* init master */
    NULL,                                                                      /* init module */
    NULL,                                                                      /* init process */
    NULL,                                                                      /* init thread */
    NULL,                                                                      /* exit thread */
    NULL,                                                                      /* exit process */
    NULL,                                                                      /* exit master */
    NGX_MODULE_V1_PADDING
};


static ngx_http_variable_t ngx_http_ip2region_vars[] = {
    { ngx_string("ip2region"), NULL,
      ngx_http_ip2region_variable,
      0, 0, 0 },

    ngx_http_null_variable
};


// 用于初始化带版本校验的搜索器的辅助函数
static char *
ngx_http_ip2region_init_searcher(ngx_conf_t *cf, char *db_name,
    char *cache_policy, xdb_version_t *expected_version, const char *directive_name)
{
    ip2region_searcher_t       *ip2region_searcher;
    int                         err;
    char                       *db_path;
    size_t                      len;

    if(ngx_http_ip2region_is_absolute_path(db_name) == NGX_OK) {
        db_path = db_name;
    } else { // relative path to conf directory
        len = ngx_cycle->conf_prefix.len + strlen(db_name) + 1;
        db_path = malloc(len);
        if (db_path == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to allocate memory for db_path");
            return NGX_CONF_ERROR;
        }
        memset(db_path, '\0', len);
        memcpy(db_path, ngx_cycle->conf_prefix.data, ngx_cycle->conf_prefix.len);
        strcat(db_path, db_name);
    }

    ip2region_searcher = ngx_palloc(cf->pool, sizeof(ip2region_searcher_t));

    if(ip2region_searcher == NULL) {
        if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
            free(db_path);
        }
        return NGX_CONF_ERROR;
    }

    ip2region_searcher->v_index = NULL;
    ip2region_searcher->c_buffer = NULL;

    // 检查XDB文件的版本信息以确定IP类型
    xdb_header_t *header = xdb_load_header_from_file(db_path);
    if (header == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to load xdb header from: %s", db_path);
        if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
            free(db_path);
        }
        return NGX_CONF_ERROR;
    }

    xdb_version_t *xdb_version = xdb_version_from_header(header);
    if (xdb_version == NULL) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "failed to determine xdb version from header: %s", db_path);
        xdb_free_header((void *)header);
        if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
            free(db_path);
        }
        return NGX_CONF_ERROR;
    }

    // 验证XDB文件版本是否匹配
    if (xdb_version->id != expected_version->id) {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "%s expects %s xdb file, but got %s: %s",
                           directive_name, expected_version->name, xdb_version->name, db_path);
        xdb_free_header((void *)header);
        if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
            free(db_path);
        }
        return NGX_CONF_ERROR;
    }

    if (strcmp(cache_policy, "file") == 0) {
        err = xdb_new_with_file_only(xdb_version, &ip2region_searcher->searcher, db_path);
        xdb_free_header((void *)header);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create searcher: %s", db_path);
            if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
                free(db_path);
            }
            return NGX_CONF_ERROR;
        }
    } else if (strcmp(cache_policy, "vectorIndex") == 0) {
        ip2region_searcher->v_index = xdb_load_vector_index_from_file(db_path);
        if (ip2region_searcher->v_index == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to load vector index from: %s", db_path);
            xdb_free_header((void *)header);
            if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
                free(db_path);
            }
            return NGX_CONF_ERROR;
        }

        err = xdb_new_with_vector_index(xdb_version, &ip2region_searcher->searcher, db_path, ip2region_searcher->v_index);
        xdb_free_header((void *)header);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create vector index cached searcher: %s", db_path);
            xdb_free_vector_index((void *)ip2region_searcher->v_index);
            if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
                free(db_path);
            }
            return NGX_CONF_ERROR;
        }
    } else if (strcmp(cache_policy, "content") == 0) {
        ip2region_searcher->c_buffer = xdb_load_content_from_file(db_path);
        if (ip2region_searcher->c_buffer == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to load xdb content: %s", db_path);
            xdb_free_header((void *)header);
            if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
                free(db_path);
            }
            return NGX_CONF_ERROR;
        }

        err = xdb_new_with_buffer(xdb_version, &ip2region_searcher->searcher, ip2region_searcher->c_buffer);
        xdb_free_header((void *)header);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create content cached searcher: %s", db_path);
            xdb_free_content((void *)ip2region_searcher->c_buffer);
            if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
                free(db_path);
            }
            return NGX_CONF_ERROR;
        }
    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid cache policy, options: file/vectorIndex/content");
        xdb_free_header((void *)header);
        if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
            free(db_path);
        }
        return NGX_CONF_ERROR;
    }

    if(ngx_http_ip2region_is_absolute_path(db_name) != NGX_OK) {
        free(db_path);
    }

    return (char *)ip2region_searcher;
}


static char *
ngx_http_ip2region_init(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf)
{
    ngx_http_ip2region_conf_t  *ip2region_cf;
    char                       *db_name, *cache_policy;
    ngx_str_t                  *value;
    char                       *result;

    ip2region_cf = conf;

    if (ip2region_cf->v4_searcher) {
        return "ip2region_db is duplicate";
    }

    value = cf->args->elts;
    db_name = (char *)value[1].data;

    // default cache_policy: content
    if(cf->args->nelts == 2) {
        cache_policy = "content";
    } else {
        cache_policy = (char *)value[2].data;
    }

    result = ngx_http_ip2region_init_searcher(cf, db_name, cache_policy,
                                               xdb_version_v4(), "ip2region_db");
    if (result == NGX_CONF_ERROR) {
        return NGX_CONF_ERROR;
    }

    ip2region_cf->v4_searcher = (ip2region_searcher_t *)result;

    return NGX_CONF_OK;
}

static char *
ngx_http_ip2region_init_v6(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf)
{
    ngx_http_ip2region_conf_t  *ip2region_cf;
    char                       *db_name, *cache_policy;
    ngx_str_t                  *value;
    char                       *result;

    ip2region_cf = conf;

    if (ip2region_cf->v6_searcher) {
        return "ip2region_db6 is duplicate";
    }

    value = cf->args->elts;
    db_name = (char *)value[1].data;

    // default cache_policy: content
    if(cf->args->nelts == 2) {
        cache_policy = "content";
    } else {
        cache_policy = (char *)value[2].data;
    }

    result = ngx_http_ip2region_init_searcher(cf, db_name, cache_policy,
                                               xdb_version_v6(), "ip2region_db6");
    if (result == NGX_CONF_ERROR) {
        return NGX_CONF_ERROR;
    }

    ip2region_cf->v6_searcher = (ip2region_searcher_t *)result;

    return NGX_CONF_OK;
}


static void *
ngx_http_ip2region_create_conf(ngx_conf_t *cf)
{
    ngx_pool_cleanup_t         *cln;
    ngx_http_ip2region_conf_t  *conf;

    conf = ngx_pcalloc(cf->pool, sizeof(ngx_http_ip2region_conf_t));
    if (conf == NULL) {
        return NULL;
    }

    cln = ngx_pool_cleanup_add(cf->pool, 0);
    if (cln == NULL) {
        return NULL;
    }

    cln->handler = ngx_http_ip2region_cleanup;
    cln->data = conf;

    return conf;
}


static ngx_int_t
ngx_http_ip2region_add_variables(ngx_conf_t *cf)
{
    ngx_http_variable_t  *var;
    ngx_http_variable_t  *v;

    for (v = ngx_http_ip2region_vars; v->name.len; v++) {
        var = ngx_http_add_variable(cf, &v->name, v->flags);
        if (var == NULL) {
            return NGX_ERROR;
        }

        var->get_handler = v->get_handler;
        var->data = v->data;
    }
    return NGX_OK;
}


static ngx_int_t
ngx_http_ip2region_variable(ngx_http_request_t *r,
    ngx_http_variable_value_t *v, uintptr_t data)
{
    ngx_http_ip2region_conf_t  *ip2region_conf;
    struct sockaddr_in         *sin;
    char                        region_buffer[512] = {'\0'};
    xdb_region_buffer_t         region;
    int                         err = 1;
    unsigned int                ip;
    xdb_searcher_t             *searcher_ptr = NULL;

#if (NGX_HAVE_INET6)
    u_char                      *p;
    in_addr_t                    addr;
    struct sockaddr_in6         *sin6;
#endif

    ip2region_conf = ngx_http_get_module_main_conf(r, ngx_http_ip2region_module);

    if (ip2region_conf->v4_searcher == NULL && ip2region_conf->v6_searcher == NULL) {
        v->not_found = 1;
        return NGX_OK;
    }

    // 初始化 region buffer
    err = xdb_region_buffer_init(&region, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        v->not_found = 1;
        return NGX_OK;
    }

    switch (r->connection->sockaddr->sa_family) {
        case AF_INET:
            sin = (struct sockaddr_in *) r->connection->sockaddr;

            // 检查是否有IPv4 searcher
            if (ip2region_conf->v4_searcher == NULL) {
                ngx_log_error(NGX_LOG_WARN, r->connection->log, 0,
                              "no IPv4 searcher available for IPv4 address");
                break;
            }

            searcher_ptr = &ip2region_conf->v4_searcher->searcher;

            // 正确转换IP地址字节序，按ip2region期望的格式
            ip = ntohl(sin->sin_addr.s_addr); // 将网络字节序转换为主机字节序
            // 按照xdb_parse_v4_ip中的格式重新组织字节
            {
                bytes_ip_t ip_bytes[4];
                ip_bytes[0] = (ip >> 24) & 0xFF;
                ip_bytes[1] = (ip >> 16) & 0xFF;
                ip_bytes[2] = (ip >> 8) & 0xFF;
                ip_bytes[3] = ip & 0xFF;
                err = xdb_search(searcher_ptr, ip_bytes, 4, &region);
            }
            if (err == 0) {
                v->data = (unsigned char *)region.value;
                v->len = strlen(region.value);
                xdb_region_buffer_free(&region);
                return NGX_OK;
            } else {
                ngx_log_error(NGX_LOG_WARN, r->connection->log, 0,
                              "ip2region search failed for IPv4 address");
            }
            break;

#if (NGX_HAVE_INET6)

        case AF_INET6:
            sin6 = (struct sockaddr_in6 *) r->connection->sockaddr;
            p = sin6->sin6_addr.s6_addr;

            if (IN6_IS_ADDR_V4MAPPED(&sin6->sin6_addr)) {
                // 处理IPv4映射的IPv6地址
                if (ip2region_conf->v4_searcher != NULL) {
                    searcher_ptr = &ip2region_conf->v4_searcher->searcher;
                    addr = p[12] << 24;
                    addr += p[13] << 16;
                    addr += p[14] << 8;
                    addr += p[15];
                    // 按照xdb_parse_v4_ip中的格式重新组织字节
                    {
                        bytes_ip_t ip_bytes[4];
                        ip_bytes[0] = (addr >> 24) & 0xFF;
                        ip_bytes[1] = (addr >> 16) & 0xFF;
                        ip_bytes[2] = (addr >> 8) & 0xFF;
                        ip_bytes[3] = addr & 0xFF;
                        err = xdb_search(searcher_ptr, ip_bytes, 4, &region);
                    }
                    if (err == 0) {
                        v->data = (unsigned char *)region.value;
                        v->len = strlen(region.value);
                        xdb_region_buffer_free(&region);
                        return NGX_OK;
                    } else {
                        ngx_log_error(NGX_LOG_WARN, r->connection->log, 0,
                                      "ip2region search failed for IPv4-mapped IPv6 address");
                    }
                }
            } else {
                // 处理纯IPv6地址
                if (ip2region_conf->v6_searcher != NULL) {
                    searcher_ptr = &ip2region_conf->v6_searcher->searcher;
                    bytes_ip_t ip6_bytes[16];
                    if (p != NULL) {
                        memcpy(ip6_bytes, p, 16);
                        err = xdb_search(searcher_ptr, ip6_bytes, 16, &region);
                        if (err == 0) {
                            v->data = (unsigned char *)region.value;
                            v->len = strlen(region.value);
                            xdb_region_buffer_free(&region);
                            return NGX_OK;
                        } else {
                            ngx_log_error(NGX_LOG_WARN, r->connection->log, 0,
                                          "ip2region search failed for IPv6 address");
                        }
                    }
                } else {
                    ngx_log_error(NGX_LOG_WARN, r->connection->log, 0,
                                  "no IPv6 searcher available for IPv6 address");
                }
            }
            break;

#endif

    }
    // 如果搜索失败，释放 region buffer
    xdb_region_buffer_free(&region);

    ngx_log_error(NGX_LOG_INFO, r->connection->log, 0,
                  "ip2region: no region found for IP address");
    v->not_found = 1;
    return NGX_OK;
}


static void
ngx_http_ip2region_cleanup(void *data)
{
    ngx_http_ip2region_conf_t *ip2region_conf = data;

    // 清理 IPv4 searcher
    if(ip2region_conf->v4_searcher != NULL) {
        xdb_close(&ip2region_conf->v4_searcher->searcher);

        // check and free the vector index
        if (ip2region_conf->v4_searcher->v_index != NULL) {
            xdb_free_vector_index((void *)ip2region_conf->v4_searcher->v_index);
            ip2region_conf->v4_searcher->v_index = NULL;
        }

        // check and free the content buffer
        if (ip2region_conf->v4_searcher->c_buffer != NULL) {
            xdb_free_content((void *)ip2region_conf->v4_searcher->c_buffer);
            ip2region_conf->v4_searcher->c_buffer = NULL;
        }

        ip2region_conf->v4_searcher = NULL;
    }

    // 清理 IPv6 searcher
    if(ip2region_conf->v6_searcher != NULL) {
        xdb_close(&ip2region_conf->v6_searcher->searcher);

        // check and free the vector index
        if (ip2region_conf->v6_searcher->v_index != NULL) {
            xdb_free_vector_index((void *)ip2region_conf->v6_searcher->v_index);
            ip2region_conf->v6_searcher->v_index = NULL;
        }

        // check and free the content buffer
        if (ip2region_conf->v6_searcher->c_buffer != NULL) {
            xdb_free_content((void *)ip2region_conf->v6_searcher->c_buffer);
            ip2region_conf->v6_searcher->c_buffer = NULL;
        }

        ip2region_conf->v6_searcher = NULL;
    }
}


static ngx_int_t
ngx_http_ip2region_is_absolute_path(char *name)
{
#if (NGX_WIN32)
    u_char  c0, c1;

    c0 = name[0];

    if (strlen(name) < 2) {
        if (c0 == '/') {
            return 2;
        }

        return NGX_DECLINED;
    }

    c1 = name[1];

    if (c1 == ':') {
        c0 |= 0x20;

        if ((c0 >= 'a' && c0 <= 'z')) {
            return NGX_OK;
        }

        return NGX_DECLINED;
    }

    if (c1 == '/') {
        return NGX_OK;
    }

    if (c0 == '/') {
        return 2;
    }

    return NGX_DECLINED;

#else

    if (name[0] == '/') {
        return NGX_OK;
    }

    return NGX_DECLINED;

#endif
}
