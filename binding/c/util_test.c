// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "stdio.h"
#include "xdb_searcher.h"

void test_check_ip() {
    char *ip_list[] = {
            "1.2.3.4", "192.168.2.3", "120.24.78.129", "255.255.255.0",
            "256.7.12.9", "12.56.78.320", "32.12.45.192", "222.221.220.219",
            "192.168.1.101 ", "132.96.12.98a", "x23.12.2.12"
    };

    int errcode, i;
    unsigned int ip;
    char ip_buff[16] = {'\0'};
    for (i = 0; i < 11; i++) {
        errcode = check_ip(ip_list[i], &ip);
        if (errcode != 0) {
            printf("invalid ip address `%s`\n", ip_list[i]);
            continue;
        }

        long2ip(ip, ip_buff);
        printf("long(%-15s)=%-10u, long2ip(%-10u)=%-15s", ip_list[i], ip, ip, ip_buff);
        if (strcmp(ip_list[i], ip_buff) != 0) {
            printf(" --[Failed]\n");
        } else {
            printf(" --[Ok]\n");
        }
    }
}

void test_load_header() {
    xdb_header_t header;
    int err = xdb_load_header_from_file("../../data/ip2region.xdb", &header);
    if (err != 0) {
        printf("failed to load header with errcode=%d\n", err);
    } else {
        printf("header loaded: {\n"
           "    version: %d, \n"
           "    index_policy: %d, \n"
           "    created_at: %u, \n"
           "    start_index_ptr: %d, \n"
           "    end_index_ptr: %d\n"
           "}\n",
           header.version, header.index_policy, header.created_at,
           header.start_index_ptr, header.end_index_ptr
       );
    }
}

void test_load_vector_index() {
    char *ptr = xdb_load_vector_index_from_file("../../data/ip2region.xdb");
    if (ptr == NULL) {
        printf("failed to load vector index from file\n");
    } else {
        printf("vector index loaded from file\n");
    }

    xdb_free(ptr);
}

void test_load_content() {
    char *ptr = xdb_load_content_from_file("../../data/ip2region.xdb");
    if (ptr == NULL) {
        printf("failed to load content from file\n");
    } else {
        printf("content loaded from file\n");
    }

    xdb_free(ptr);
}

// valgrind --tool=memcheck --leak-check=full ./a.out
int main(int argc, char *argv[]) {
    printf("test check ip ... \n");
    test_check_ip();
    printf("|--done\n\n");

    printf("test load header ... \n");
    test_load_header();
    printf("|--done\n\n");

    printf("test load vector index ... \n");
    test_load_vector_index();
    printf("|--done\n\n");

    printf("test load content ... \n");
    test_load_content();
    printf("|--done\n\n");

    return 0;
}