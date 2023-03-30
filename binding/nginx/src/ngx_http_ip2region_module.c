/*
 * Created by Wu Jian Ping on - 2023/03/20.
 */

#include "ngx_http_ip2region_module.h"

static ngx_int_t ngx_http_ip2region_add_variables(ngx_conf_t *cf);

static void *ngx_http_ip2region_create_conf(ngx_conf_t *cf);
static void ngx_http_ip2region_cleanup(void *data);
static char *ngx_http_ip2region_processor(ngx_conf_t *cf,
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
    { ngx_string("ip2region"),
      NGX_HTTP_MAIN_CONF | NGX_CONF_TAKE1,
      ngx_http_ip2region_processor,
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
ngx_http_ip2region_processor(ngx_conf_t *cf, ngx_command_t *cmd,
    void *conf)
{
    // ngx_http_ip2region_conf_t  *uacf = conf;
    // ngx_str_t                  *value;

    // if (uacf->ip2region) {
    //     return "is duplicate";
    // }

    // value = cf->args->elts;

    // uacf->ip2region = open_ip2region_regexes_yaml((char *)value[1].data);

    // if(uacf->ip2region == NULL) {
    //     ngx_conf_log_error(NGX_LOG_EMERG, cf, 0,
    //         "open_ip2region_regexes_yaml(\"%V\") failed", &value[1]);
    //     return NGX_CONF_ERROR;
    // }

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
    
    /* the last ngx_http_variable_t's name is ngx_null_string and it's name's len is 0*/
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
    v->not_found = 1;
    return NGX_OK;
}


static void
ngx_http_ip2region_cleanup(void *data)
{
    // ngx_http_ip2region_conf_t *uacf = data;
}