// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "stdio.h"
#include "xdb_api.h"

// test functions return 0 for pass or 1 for failure
typedef int (* test_func_ptr) (void);
struct test_func_entry {
    char *name;
    test_func_ptr func;
};
typedef struct test_func_entry test_func_t;

int test_load_header(void) {
    xdb_header_t *header = xdb_load_header_from_file("../../data/ip2region_v4.xdb");
    if (header == NULL) {
        printf("failed to load header\n");
        return 1;
    }

    printf("header loaded: {\n"
       "    version: %d, \n"
       "    index_policy: %d, \n"
       "    created_at: %u, \n"
       "    start_index_ptr: %d, \n"
       "    end_index_ptr: %d\n"
       "    ip_version: %d\n"
       "    runtime_ptr_bytes: %d\n"
       "    length: %d\n"
       "}\n",
       header->version, header->index_policy, header->created_at,
       header->start_index_ptr, header->end_index_ptr, 
       header->ip_version, header->runtime_ptr_bytes, header->length
    );

    xdb_free_header(header);
    return 0;
}

int test_load_vector_index(void) {
    xdb_vector_index_t *v_index = xdb_load_vector_index_from_file("../../data/ip2region_v4.xdb");
    if (v_index == NULL) {
        printf("failed to load vector index from file\n");
        return 1;
    }

    printf("vector index loaded from file, length=%d\n", v_index->length);
    xdb_free_vector_index(v_index);
    return 0;
}

int test_load_content(void) {
    xdb_content_t *content = xdb_load_content_from_file("../../data/ip2region_v4.xdb");
    if (content == NULL) {
        printf("failed to load content from file\n");
        return 1;
    }

    printf("content loaded from file, length=%d\n", content->length);
    xdb_free_content(content);
    return 0;
}

struct ip_entry {
    const char *ip;
    int version;    // 0 for the invalid ip
};
int test_parse_ip(void) {
    struct ip_entry ip_list[] = {
        {"1.0.0.0", 4}, {"58.251.30.115", 4}, {"192.168.1.100", 4},
        {"::", 6}, {"2c0f:fff0::", 6}, {"2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", 6}, {"240e:982:e617:ffff:ffff:ffff:ffff:ffff", 6},
        {"219.xx.xx.11", 0}, {"::xx:ffff", 0},
        {NULL, 0}
    };

    int errcode = 0;
    struct ip_entry *entry;
    xdb_version_t *version;
    bytes_ip_t ip_bytes[16] = {'\0'};
    string_ip_t ip_string[INET6_ADDRSTRLEN] = {'\0'};

    // init the sock env (for windows)
    if (xdb_init_winsock() != 0) {
        printf("failed to init the winsock\n");
        return 1;
    }

    for (entry = ip_list; entry->ip != NULL; entry++) {
        version = xdb_parse_ip(entry->ip, ip_bytes, sizeof(ip_bytes));
        if (version == NULL) {
            printf("failed to parse ip `%s`\n", entry->ip);
            if (entry->version != 0) {
                errcode = 1;
            }
            continue;
        }

        if (version->id != entry->version) {
            printf("ip `%s` parsed as v%d, v%d expected\n", entry->ip, version->id, entry->version);
            errcode = 1;
            continue;
        }

        if (xdb_ip_to_string(ip_bytes, version->bytes, ip_string, sizeof(ip_string)) != 0
            || strcmp(ip_string, entry->ip) != 0) {
            printf("ip `%s` converted back to `%s`\n", entry->ip, ip_string);
            errcode = 1;
            continue;
        }

        printf("ip: %s (version=v%d), toString: %s\n", entry->ip, version->id, ip_string);
    }

    // clean up the winsock
    xdb_clean_winsock();
    return errcode;
}

struct ip_pair {
    const char *sip;
    const char *eip;
    int expected;
};
int test_ip_compare(void) {
    struct ip_pair ip_pair_list[] = {
        {"1.0.0.0", "1.0.0.1", -1},
        {"192.168.1.101", "192.168.1.90", 1},
        {"219.133.111.87", "114.114.114.114", 1},
        {"1.0.4.0", "1.0.1.0", 1},
        {"1.0.4.0", "1.0.3.255", 1},
        {"2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff", -1},
        {"2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff", -1},
        {"ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff", 1},
        {NULL, NULL, 0}
    };

    struct ip_pair *pair_ptr = NULL;
    bytes_ip_t sip_bytes[16] = {'\0'};
    bytes_ip_t eip_bytes[16] = {'\0'};
    xdb_version_t *s_version, *e_version;
    int errcode = 0, cmp;

    // init the sock env (for windows)
    if (xdb_init_winsock() != 0) {
        printf("failed to init the winsock\n");
        return 1;
    }

    for (pair_ptr = ip_pair_list; pair_ptr->sip != NULL; pair_ptr++) {
        s_version = xdb_parse_ip(pair_ptr->sip, sip_bytes, sizeof(sip_bytes));
        if (s_version == NULL) {
            printf("failed to parse sip `%s`\n", pair_ptr->sip);
            errcode = 1;
            continue;
        }

        e_version = xdb_parse_ip(pair_ptr->eip, eip_bytes, sizeof(eip_bytes));
        if (e_version == NULL) {
            printf("failed to parse eip `%s`\n", pair_ptr->eip);
            errcode = 1;
            continue;
        }

        if (s_version->id != e_version->id) {
            printf("sip and eip version not match `%s` != `%s`\n", s_version->name, e_version->name);
            errcode = 1;
            continue;
        }

        cmp = xdb_ip_sub_compare(sip_bytes, s_version->bytes, (string_ip_t *) eip_bytes, 0);
        printf("ip_sub_compare(%s, %s): %d\n", pair_ptr->sip, pair_ptr->eip, cmp);
        if (cmp != pair_ptr->expected) {
            printf("%d expected\n", pair_ptr->expected);
            errcode = 1;
        }
    }

    // clean up the winsock
    xdb_clean_winsock();
    return errcode;
}

// please register your function heare
static test_func_t _test_function_list[] = {
    // xdb buffer
    {"test_load_header", test_load_header},
    {"test_load_vector_index", test_load_vector_index},
    {"test_load_content", test_load_content},

    // ip utils
    {"test_parse_ip", test_parse_ip},
    {"test_ip_compare", test_ip_compare},

    {NULL, NULL}
};

// valgrind --tool=memcheck --leak-check=full ./a.out
int main(int argc, char *argv[]) {
    int i;
    char *name;

    // check and call the function
    if (argc < 2) {
        printf("please specified the function name to call\n");
        return 1;
    }

    name = argv[1];
    test_func_ptr func = NULL;
    for (i = 0; ; i++) {
        if (_test_function_list[i].name == NULL) {
            break;
        }

        if (strcmp(name, _test_function_list[i].name) == 0) {
            func = _test_function_list[i].func;
            break;
        }
    }

    if (func == NULL) {
        printf("can't find test function `%s`\n", name);
        return 1;
    }
    
    // call the function
    return func();
}
