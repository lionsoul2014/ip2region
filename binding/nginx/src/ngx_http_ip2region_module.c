/*
 * Created by Wu Jian Ping on - 2023/03/30.
 */

#include "ngx_http_ip2region_module.h"

static ngx_int_t ngx_http_ua_parser_test_full_name(char *name);

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


static ngx_command_t ngx_http_ip2region_commands[] = {
    { ngx_string("ip2region_db"),
      NGX_HTTP_MAIN_CONF | NGX_CONF_TAKE12,
      ngx_http_ip2region_init,
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


static char *
ngx_http_ip2region_init(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf)
{
    ngx_http_ip2region_conf_t  *ip2region_cf;
    ip2region_searcher_t       *ip2region_searcher;
    char                       *db_name, *cache_policy;
    ngx_str_t                  *value;
    int                         err;
    char                       *db_path;
    size_t                      len;
    ip2region_cf = conf;

    if (ip2region_cf->ip2region_searcher) {
        return "is duplicate";
    }

    value = cf->args->elts;

    db_name = (char *)value[1].data;

    // default cache_policy: content
    if(cf->args->nelts == 2) {
        cache_policy = "content";
    } else {
        cache_policy = (char *)value[2].data;
    }

    if(ngx_http_ua_parser_test_full_name(db_name) == NGX_OK) {
        db_path = db_name;
    } else { // relative path to conf directory
        len = ngx_cycle->conf_prefix.len + strlen(db_name) + 1;
        db_path = malloc(len);
        memset(db_path, '\0', len);
        memcpy(db_path, ngx_cycle->conf_prefix.data, ngx_cycle->conf_prefix.len);
        strcat(db_path, db_name);
    }

    ip2region_searcher = ngx_palloc(cf->pool, sizeof(ip2region_searcher_t));

    if(ip2region_searcher == NULL) {
        return NGX_CONF_ERROR;
    }

    ip2region_searcher->v_index = NULL;
    ip2region_searcher->c_buffer = NULL;

    if (strcmp(cache_policy, "file") == 0) {
        err = xdb_new_with_file_only(&ip2region_searcher->searcher, db_path);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create searcher: %s", db_path);
            return NGX_CONF_ERROR;
        }
    } else if (strcmp(cache_policy, "vectorIndex") == 0) {
        ip2region_searcher->v_index = xdb_load_vector_index_from_file(db_path);
        if (ip2region_searcher->v_index == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to load vector index from: %s", db_path);
            return NGX_CONF_ERROR;
        }

        err = xdb_new_with_vector_index(&ip2region_searcher->searcher, db_path, ip2region_searcher->v_index);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create vector index cached searcher: %s", db_path);
            return NGX_CONF_ERROR;
        }
    } else if (strcmp(cache_policy, "content") == 0) {
        ip2region_searcher->c_buffer = xdb_load_content_from_file(db_path);
        if (ip2region_searcher->c_buffer == NULL) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to load xdb content: %s", db_path);
            return NGX_CONF_ERROR;
        }

        err = xdb_new_with_buffer(&ip2region_searcher->searcher, ip2region_searcher->c_buffer);
        if (err != 0) {
            ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                               "failed to create content cached searcher: %s", db_path);
            return NGX_CONF_ERROR;
        }
    } else {
        ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
                           "invalid cache policy `%V`, options: file/vectorIndex/content", &value[2]);
        return NGX_CONF_ERROR;
    }

    ip2region_cf->ip2region_searcher = ip2region_searcher;

    if(ngx_http_ua_parser_test_full_name(db_name) != NGX_OK) {
        free(db_path);
    }

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
    char                        region[512] = {'\0'};
    int                         err;
    unsigned int                ip;

#if (NGX_HAVE_INET6)
    u_char                      *p;
    in_addr_t                    addr;
    struct sockaddr_in6         *sin6;
#endif

    ip2region_conf = ngx_http_get_module_main_conf(r, ngx_http_ip2region_module);

    if (ip2region_conf->ip2region_searcher != NULL) {

        switch (r->connection->sockaddr->sa_family) {
            case AF_INET:
                sin = (struct sockaddr_in *) r->connection->sockaddr;
                ip = htonl(sin->sin_addr.s_addr);
                err = xdb_search(&ip2region_conf->ip2region_searcher->searcher, ip, region, sizeof(region));
                if (err == 0) {
                    v->data = (unsigned char *)region;
                    v->len = strlen(region);
                    return NGX_OK;
                }
                break;

#if (NGX_HAVE_INET6)

            case AF_INET6:
                sin6 = (struct sockaddr_in6 *) r->connection->sockaddr;
                p = sin6->sin6_addr.s6_addr;

                if (IN6_IS_ADDR_V4MAPPED(&sin6->sin6_addr)) {
                    addr = p[12] << 24;
                    addr += p[13] << 16;
                    addr += p[14] << 8;
                    addr += p[15];

                    ip = htonl(addr);
                    err = xdb_search(&ip2region_conf->ip2region_searcher->searcher, ip, region, sizeof(region));
                    if (err == 0) {
                        v->data = (unsigned char *)region;
                        v->len = strlen(region);
                        return NGX_OK;
                    }
                }
                break;

#endif

        }
    }

    v->not_found = 1;
    return NGX_OK;
}


static void
ngx_http_ip2region_cleanup(void *data)
{
    ngx_http_ip2region_conf_t *ip2region_conf = data;

    if(ip2region_conf->ip2region_searcher != NULL) {
        xdb_close(&ip2region_conf->ip2region_searcher->searcher);

        // check and free the vector index
        if (ip2region_conf->ip2region_searcher->v_index != NULL) {
            xdb_close_vector_index(ip2region_conf->ip2region_searcher->v_index);
            ip2region_conf->ip2region_searcher->v_index = NULL;
        }

        // check and free the content buffer
        if (ip2region_conf->ip2region_searcher->c_buffer != NULL) {
            xdb_close_content(ip2region_conf->ip2region_searcher->c_buffer);
            ip2region_conf->ip2region_searcher->c_buffer = NULL;
        }

        ip2region_conf->ip2region_searcher = NULL;
    }
}


static ngx_int_t
ngx_http_ua_parser_test_full_name(char *name)
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
