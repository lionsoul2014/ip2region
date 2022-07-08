// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/28

#include "stdio.h"
#include "xdb_searcher.h"

struct searcher_test_entry {
    xdb_searcher_t searcher;
    xdb_vector_index_t *v_index;
    xdb_content_t *c_buffer;
};
typedef struct searcher_test_entry searcher_test_t;

int init_searcher_test(searcher_test_t *test, char *db_path, char *cache_policy) {
    int err;
    test->v_index = NULL;
    test->c_buffer = NULL;

    if (strcmp(cache_policy, "file") == 0) {
        err = xdb_new_with_file_only(&test->searcher, db_path);
        if (err != 0) {
            printf("failed to create searcher with errcode=%d\n", err);
            return 1;
        }
    } else if (strcmp(cache_policy, "vectorIndex") == 0) {
        test->v_index = xdb_load_vector_index_from_file(db_path);
        if (test->v_index == NULL) {
            printf("failed to load vector index from `%s`\n", db_path);
            return 2;
        }

        err = xdb_new_with_vector_index(&test->searcher, db_path, test->v_index);
        if (err != 0) {
            printf("failed to create vector index cached searcher with errcode=%d\n", err);
            return 3;
        }
    } else if (strcmp(cache_policy, "content") == 0) {
        test->c_buffer = xdb_load_content_from_file(db_path);
        if (test->c_buffer == NULL) {
            printf("failed to load xdb content from `%s`\n", db_path);
            return 4;
        }

        err = xdb_new_with_buffer(&test->searcher, test->c_buffer);
        if (err != 0) {
            printf("failed to create content cached searcher with errcode=%d\n", err);
            return 5;
        }
    } else {
        printf("invalid cache policy `%s`, options: file/vectorIndex/content\n", cache_policy);
        return 6;
    }

    return 0;
}

void destroy_searcher_test(searcher_test_t *test) {
    xdb_close(&test->searcher);

    // check and free the vector index
    if (test->v_index != NULL) {
        xdb_close_vector_index(test->v_index);
        test->v_index = NULL;
    }

    // check and free the content buffer
    if (test->c_buffer != NULL) {
        xdb_close_content(test->c_buffer);
        test->c_buffer = NULL;
    }
}

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
    char line[512] = {'\0'}, region[512] = {'\0'};
    searcher_test_t test;

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
    err = init_searcher_test(&test, db_file, cache_policy);
    if (err != 0) {
        // init program will print the error reasons;
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

        if (xdb_check_ip(line, &ip) != 0) {
            printf("invalid ip address `%s`\n", line);
            continue;
        }

        s_time = xdb_now();
        err = xdb_search(&test.searcher, ip, region, sizeof(region));
        if (err != 0) {
            printf("{err: %d, io_count: %d}\n", err, xdb_get_io_count(&test.searcher));
        } else {
            c_time = xdb_now() - s_time;
            printf("{region: %s, io_count: %d, took: %ld μs}\n", region, xdb_get_io_count(&test.searcher), c_time);
        }
    }

    destroy_searcher_test(&test);
    printf("searcher test program exited, thanks for trying\n");
}

void test_bench(int argc, char *argv[]) {
    int i, n, err;
    char *r, key[33] = {'\0'}, val[256] = {'\0'};
    char db_file[256] = {'\0'}, src_file[256] = {'\0'}, cache_policy[16] = {"vectorIndex"};

    FILE *handle;
    char line[1024] = {'\0'}, sip_str[16] = {'\0'}, eip_str[16] = {'\0'};
    char src_region[512] = {'\0'}, region_buffer[512] = {'\0'};
    unsigned int sip, eip, mip, ip_list[5];
    int count = 0, took;
    long s_time, t_time, c_time = 0;
    searcher_test_t test;

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

    // printf("db_file=%s, src_file=%s, cache_policy=%s\n", db_file, src_file, cache_policy);
    s_time = xdb_now();
    err = init_searcher_test(&test, db_file, cache_policy);
    if (err != 0) {
        // the init function will print the details;
        return;
    }

    // open the source file
    handle = fopen(src_file, "r");
    if (handle == NULL) {
        printf("failed to open source text file `%s`\n", src_file);
        return;
    }

    while(fgets(line, sizeof(line), handle) != NULL) {
        n = sscanf(line, "%15[^|]|%15[^|]|%511[^\n]", sip_str, eip_str, src_region);
        if (n != 3) {
            printf("invalid ip segment line `%s`\n", line);
            return;
        }

        if (xdb_check_ip(sip_str, &sip) != 0) {
            printf("invalid start ip `%s`\n", sip_str);
            return;
        }

        if (xdb_check_ip(eip_str, &eip) != 0) {
            printf("invalid end ip `%s`\n", sip_str);
            return;
        }

        if (sip > eip) {
            printf("start ip(%s) should not be greater than end ip(%s)\n", sip_str, eip_str);
            return;
        }

        mip = xdb_mip(sip, eip);
        ip_list[0] = sip;
        ip_list[1] = xdb_mip(sip, mip);
        ip_list[2] = mip;
        ip_list[3] = xdb_mip(mip, eip);
        ip_list[4] = eip;
        for (i = 0; i < 5; i++) {
            t_time = xdb_now();
            err = xdb_search(&test.searcher, ip_list[i], region_buffer, sizeof(region_buffer));
            if (err != 0) {
                xdb_long2ip(ip_list[i], sip_str);
                printf("failed to search ip `%s` with errno=%d\n", sip_str, err);
                return;
            }

            c_time += xdb_now() - t_time;

            // check the region info
            if (strcmp(region_buffer, src_region) != 0) {
                xdb_long2ip(ip_list[i], sip_str);
                printf("failed to search(%s) with (%s != %s)\n", sip_str, region_buffer, src_region);
                return;
            }

            count++;
        }
    };

    took = xdb_now() - s_time;
    destroy_searcher_test(&test);
    fclose(handle);
    printf("Bench finished, {cache_policy: %s, total: %d, took: %.3fs, cost: %d μs/op}\n",
           cache_policy, count, took/1e6, count == 0 ? 0 : (int)(c_time/count));
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
