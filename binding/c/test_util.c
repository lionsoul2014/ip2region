// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "stdio.h"
#include "xdb_api.h"

typedef void (* test_func_ptr) ();
struct test_func_entry {
    char *name;
    test_func_ptr func;
};
typedef struct test_func_entry test_func_t;

void test_load_header() {
    xdb_header_t *header = xdb_load_header_from_file("../../data/ip2region_v4.xdb");
    if (header == NULL) {
        printf("failed to load header");
    } else {
        printf("header loaded: {\n"
           "    version: %d, \n"
           "    index_policy: %d, \n"
           "    created_at: %u, \n"
           "    start_index_ptr: %d, \n"
           "    end_index_ptr: %d\n"
           "    length: %d\n"
           "}\n",
           header->version, header->index_policy, header->created_at,
           header->start_index_ptr, header->end_index_ptr, header->length
       );
    }

    xdb_free_header(header);
}

void test_load_vector_index() {
    xdb_vector_index_t *v_index = xdb_load_vector_index_from_file("../../data/ip2region_v4.xdb");
    if (v_index == NULL) {
        printf("failed to load vector index from file\n");
    } else {
        printf("vector index loaded from file, length=%d\n", v_index->length);
    }

    xdb_free_vector_index(v_index);
}

void test_load_content() {
    xdb_content_t *content = xdb_load_content_from_file("../../data/ip2region_v4.xdb");
    if (content == NULL) {
        printf("failed to load content from file\n");
    } else {
        printf("content loaded from file, length=%d\n", content->length);
    }

    xdb_free_content(content);
}

void test_parse_ip() {
    const char *ip_list[] = {
        "1.0.0.0", "58.251.30.115", "192.168.1.100",
        "::", "2c0f:fff0::", "2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "240e:982:e617:ffff:ffff:ffff:ffff:ffff", 
        "219.xx.xx.11", "::xx:ffff",
        NULL
    };

    int errcode;
    bytes_ip_t ip_bytes[17] = {'\0'};
    string_ip_t ip_string[INET6_ADDRSTRLEN + 1] = {'\0'};

    // init the sock env (for windows)
    if ((errcode = xdb_init_winsock()) != 0) {
        printf("failed to init the winsock");
        return;
    }

    for (int i = 0;; i++) {
        if (ip_list[i] == NULL) {
            break;
        }

        errcode = xdb_parse_ip(ip_list[i], ip_bytes, sizeof(ip_bytes));
        if (errcode == -1) {
            printf("failed to parse ip `%s`\n", ip_list[i]);
            continue;
        }

        xdb_ip_to_string(ip_bytes, errcode, ip_string, sizeof(ip_string));
        printf("ip: %s (version=v%d), toString: %s\n", ip_list[i], errcode, ip_string);
    }

    // clean up the winsock
    xdb_clean_winsock();
}

struct ip_pair {
    const char *sip;
    const char *eip;
};
void test_ip_compare() {
    struct ip_pair ip_pair_list[] = {
        {"1.0.0.0", "1.0.0.1"},
        {"192.168.1.101", "192.168.1.90"},
        {"219.133.111.87", "114.114.114.114"},
        {"1.0.4.0", "1.0.1.0"},
        {"1.0.4.0", "1.0.3.255"},
        {"2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff"},
        {"2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff"},
        {"ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff"},
        {NULL, NULL}
    };

    struct ip_pair *pair_ptr = NULL;
    bytes_ip_t sip_bytes[16] = {'\0'};
    bytes_ip_t eip_bytes[16] = {'\0'};
    int sip_version, eip_version, bytes, errcode;

    // init the sock env (for windows)
    if ((errcode = xdb_init_winsock()) != 0) {
        printf("failed to init the winsock");
        return;
    }

    for (int i = 0; ;i++) {
        pair_ptr = &ip_pair_list[i];
        if (pair_ptr->sip == NULL) {
            break;
        }

        sip_version = xdb_parse_ip(pair_ptr->sip, sip_bytes, sizeof(sip_bytes));
        if (sip_version == -1) {
            printf("failed to parse sip `%s`", pair_ptr->sip);
            continue;
        }

        eip_version = xdb_parse_ip(pair_ptr->eip, eip_bytes, sizeof(eip_bytes));
        if (eip_version == -1) {
            printf("failed to parse eip `%s`", pair_ptr->eip);
            continue;
        }

        bytes = sip_version == xdb_ipv4_version_no ? xdb_ipv4_bytes : xdb_ipv6_bytes;
        printf(
            "ip_sub_compare(%s, %s): %d\n", 
            pair_ptr->sip, pair_ptr->eip, 
            xdb_ip_sub_compare(sip_bytes, bytes, eip_bytes, 0)
        );
    }

    // clean up the winsock
    xdb_clean_winsock();
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
    func();

    return 0;
}
