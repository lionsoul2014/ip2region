// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/28

#include "stdio.h"
#include "sys/time.h"
#include "xdb_searcher.h"

//read a line from a command line.
static char *get_line(FILE *fp, char *__dst) {
    register int c;
    register char *cs;

    cs = __dst;
    while ( ( c = getc( fp ) ) != EOF ) {
        if ( c == '\n' ) break;
        *cs++ = c;
    }
    *cs = '\0';

    return ( c == EOF && cs == __dst ) ? NULL : __dst;
}

void print_help(char *argv[]) {
    printf("ip2region xdb searcher\n");
    printf("%s [command] [command options]\n", argv[0]);
    printf("Command: \n");
    printf("  search    search input test\n");
    printf("  bench     search bench test\n");
}

void test_search(int argc, char *argv[]) {
    int i, n, err;

    // for args parse
    char *r, key[33] = {'\0'}, val[256] = {'\0'};
    char db_file[256] = {'\0'}, cache_policy[16] = {"vectorIndex"};

    // for search
    long s_time, c_time;
    unsigned int ip;
    char line[256] = {'\0'}, region[256] = {'\0'};

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

    // printf("db_file=%s, cache_policy=%s\n", db_file, cache_policy);

    xdb_searcher_t searcher;
    char *vIndex = NULL, *cBuffer = NULL;
    if (strcmp(cache_policy, "file") == 0) {
        err = xdb_new_with_file_only(&searcher, db_file);
        if (err != 0) {
            printf("failed to create searcher with errcode=%d", err);
            return;
        }
    } else if (strcmp(cache_policy, "vectorIndex") == 0) {
        vIndex = xdb_load_vector_index_from_file(db_file);
        if (vIndex == NULL) {
            printf("failed to load vector index from `%s`", db_file);
            return;
        }

        err = xdb_new_with_vector_index(&searcher, db_file, vIndex);
        if (err != 0) {
            printf("failed to create vector index cached searcher with errcode=%d", err);
            return;
        }
    } else if (strcmp(cache_policy, "content") == 0) {
        cBuffer = xdb_load_content_from_file(db_file);
        if (cBuffer == NULL) {
            printf("failed to load xdb content from `%s`", db_file);
            return;
        }

        err = xdb_new_with_buffer(&searcher, cBuffer);
        if (err != 0) {
            printf("failed to create content cached searcher with errcode=%d", err);
            return;
        }
    } else {
        printf("invalid cache policy `%s`, options: file/vectorIndex/content", cache_policy);
        return;
    }

    printf("ip2region xdb searcher test program, "
           "cache_policy: %s\ntype 'quit' to exit\n", cache_policy);
    while ( 1 ) {
        printf("ip2region>> ");
        get_line(stdin, line);
        if ( strlen(line) < 2 ) {
            continue;
        }

        if (strcasecmp( line, "quit") == 0 ) {
            break;
        }

        if (check_ip(line, &ip) != 0) {
            printf("invalid ip address `%s`\n", line);
            continue;
        }

        s_time = xdb_now();
        err = xdb_search(&searcher, ip, region, sizeof(region));
        if (err != 0) {
            printf("{err: %d, io_count: %d}\n", err, xdb_get_io_count(&searcher));
        } else {
            c_time = xdb_now() - s_time;
            printf("{region: %s, io_count: %d, took: %ld μs}\n", region, xdb_get_io_count(&searcher), c_time);
        }
    }

    xdb_close(&searcher);
    if (vIndex != NULL) {
        xdb_free(vIndex);
    }
    if(cBuffer != NULL) {
        xdb_free(cBuffer);
    }
    printf("searcher test program exited, thanks for trying\n");
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
