// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/28

#include "stdio.h"
#include "stdlib.h"
#include "xdb_searcher.h"


void print_help(char *argv[]) {
    printf("ip2region xdb searcher\n");
    printf("%s [command] [command options]\n", argv[0]);
    printf("Command: \n");
    printf("  search    search input test\n");
    printf("  bench     search bench test\n");
}

void test_search(int argc, char *argv[]) {
    int i, n;
    char *r, key[33] = {'\0'}, val[256] = {'\0'};
    char db_file[256] = {'\0'}, cache_policy[16] = {"vectorIndex"};
    for (i = 2; i < argc; i++) {
        r = argv[i];
        if (strlen(r) < 5) {
            continue;
        }

        if (r[0] != '-' || r[1] != '-') {
            continue;
        }

        if (strchr(r, '=') == NULL) {
            printf("missing = for args pair '%s'\n", r);
            return;
        }

        n = sscanf(r+2, "%32[^=]=%255[^\n]", key, val);
        if (n != 2) {
            printf("invalid option flag `%s`\n", r);
            return;
        }

        // printf("key=%s, val=%s\n", key, val);
        if (strcmp(key, "db") == 0) {
            snprintf(db_file, sizeof(db_file), "%s", val);
        } else if (strcmp(key, "cache-policy") == 0) {
            memcpy(cache_policy, val, sizeof(cache_policy) - 1);
            // snprintf(cache_policy, sizeof(cache_policy), "%s", val);
        } else {
            printf("undefined option `%s`\n", r);
            return;
        }
    }

    if (strlen(db_file) < 1) {
        printf("%s search [command options]\n", argv[0]);
        printf("options:\n");
        printf(" --db string              ip2region binary xdb file path\n");
        printf(" --cache-policy string    cache policy: file/vectorIndex/content\n");
        return;
    }

    printf("db_file=%s, cache_policy=%s\n", db_file, cache_policy);
}

void test_bench(int argc, char *argv[]) {
    int i, n;
    char *r, key[33] = {'\0'}, val[256] = {'\0'};
    char db_file[256] = {'\0'}, src_file[256] = {'\0'}, cache_policy[16] = {"vectorIndex"};
    for (i = 2; i < argc; i++) {
        r = argv[i];
        if (strlen(r) < 5) {
            continue;
        }

        if (r[0] != '-' || r[1] != '-') {
            continue;
        }

        if (strchr(r, '=') == NULL) {
            printf("missing = for args pair '%s'\n", r);
            return;
        }

        n = sscanf(r+2, "%32[^=]=%255[^\n]", key, val);
        if (n != 2) {
            printf("invalid option flag `%s`\n", r);
            return;
        }

        if (strcmp(key, "db") == 0) {
            snprintf(db_file, sizeof(db_file), "%s", val);
        } else if (strcmp(key, "src") == 0) {
            snprintf(src_file, sizeof(src_file), "%s", val);
        } else if (strcmp(key, "cache-policy") == 0) {
            memcpy(cache_policy, val, sizeof(cache_policy) - 1);
        } else {
            printf("undefined option `%s`\n", r);
            return;
        }
    }

    if (strlen(db_file) < 1 || strlen(src_file) < 1) {
        printf("%s bench [command options]\n", argv[0]);
        printf("options:\n");
        printf(" --db string              ip2region binary xdb file path\n");
        printf(" --src string             source ip text file path\n");
        printf(" --cache-policy string    cache policy: file/vectorIndex/content\n");
        return;
    }

    printf("db_file=%s, src_file=%s, cache_policy=%s\n", db_file, src_file, cache_policy);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_help(argv);
        return 0;
    }

    char *opt = argv[1];
    if (strcmp(opt, "search") == 0) {
        test_search(argc, argv);
    } else if (strcmp(opt, "bench") == 0) {
        test_bench(argc, argv);
    } else {
        print_help(argv);
    }

    return 0;
}
