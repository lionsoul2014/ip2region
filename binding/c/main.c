// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/28

#include "stdio.h"
#include "xdb_api.h"

struct searcher_test_entry {
    xdb_searcher_t searcher;
    xdb_vector_index_t *v_index;
    xdb_content_t *c_buffer;

    // xdb region buffer
    // char region_buffer[256];
    xdb_region_buffer_t region;
};
typedef struct searcher_test_entry searcher_test_t;

int init_searcher_test(searcher_test_t *test, char *db_path, char *cache_policy) {
    int err, errcode = 0;
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return -1;
    }

    // auto detect the version from the xdb header
    xdb_header_t *header = xdb_load_header(handle);
    if (header == NULL) {
        printf("failed to load header from `%s`\n", db_path);
        errcode = 1;
        goto defer;
    }

    // verify the current xdb
    err = xdb_verify_from_header(handle, header);
    if (err != 0) {
        printf("failed to verify xdb file `%s` with errno=%d\n", db_path, err);
        errcode = 2;
        goto defer;
    }

    xdb_version_t *version = xdb_version_from_header(header);
    if (version == NULL) {
        printf("failed to load version from header\n");
        errcode = 3;
        goto defer;
    }

    test->v_index = NULL;
    test->c_buffer = NULL;

    if (strcmp(cache_policy, "file") == 0) {
        err = xdb_new_with_file_only(version, &test->searcher, db_path);
        if (err != 0) {
            printf("failed to create searcher with errcode=%d\n", err);
            errcode = 4;
            goto defer;
        }
    } else if (strcmp(cache_policy, "vectorIndex") == 0) {
        test->v_index = xdb_load_vector_index_from_file(db_path);
        if (test->v_index == NULL) {
            printf("failed to load vector index from `%s`\n", db_path);
            errcode = 4;
            goto defer;
        }

        err = xdb_new_with_vector_index(version, &test->searcher, db_path, test->v_index);
        if (err != 0) {
            printf("failed to create vector index cached searcher with errcode=%d\n", err);
            errcode = 5;
            goto defer;
        }
    } else if (strcmp(cache_policy, "content") == 0) {
        test->c_buffer = xdb_load_content_from_file(db_path);
        if (test->c_buffer == NULL) {
            printf("failed to load xdb content from `%s`\n", db_path);
            errcode = 4;
            goto defer;
        }

        err = xdb_new_with_buffer(version, &test->searcher, test->c_buffer);
        if (err != 0) {
            printf("failed to create content cached searcher with errcode=%d\n", err);
            errcode = 5;
            goto defer;
        }
    } else {
        printf("invalid cache policy `%s`, options: file/vectorIndex/content\n", cache_policy);
        errcode = 6;
        goto defer;
    }

    // init the region buffer
    // err = xdb_region_buffer_init(&test->region, test->region_buffer, sizeof(test->region_buffer));
    err = xdb_region_buffer_init(&test->region, NULL, 0);
    if (err != 0) {
        printf("failed to init the region buffer with err=%d\n", err);
        errcode = 7;
        goto defer;
    }

defer:
    if (header != NULL) {
        xdb_free_header(header);
    }

    if (handle != NULL) {
        fclose(handle);
    }

    return errcode;
}

void destroy_searcher_test(searcher_test_t *test) {
    xdb_close(&test->searcher);

    // check and free the vector index
    if (test->v_index != NULL) {
        xdb_free_vector_index(test->v_index);
        test->v_index = NULL;
    }

    // check and free the content buffer
    if (test->c_buffer != NULL) {
        xdb_free_content(test->c_buffer);
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
    char line[512] = {'\0'};

    // ip parse
    xdb_version_t *version;
    bytes_ip_t ip_bytes[16] = {'\0'};

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

    // init the win sock
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return;
    }

    // printf("db_file=%s, cache_policy=%s\n", db_file, cache_policy);
    err = init_searcher_test(&test, db_file, cache_policy);
    if (err != 0) {
        // init program will print the error reasons;
        return;
    }

    printf("ip2region xdb searcher test program\n"
            "source xdb: %s (%s, %s)\n"
            "type 'quit' to exit\n", db_file, xdb_get_version(&test.searcher)->name, cache_policy);
    while ( 1 ) {
        printf("ip2region>> ");
        get_line(stdin, line);
        if ( strlen(line) < 2 ) {
            continue;
        }

        if (strcasecmp( line, "quit") == 0 ) {
            break;
        }

        version = xdb_parse_ip(line, ip_bytes, sizeof(ip_bytes));
        if (version == NULL) {
            printf("invalid ip address `%s`\n", line);
            continue;
        }

        s_time = xdb_now();
        err = xdb_search(&test.searcher, ip_bytes, version->bytes, &test.region);
        if (err != 0) {
            printf("{err: %d, io_count: %d}\n", err, xdb_get_io_count(&test.searcher));
        } else {
            c_time = xdb_now() - s_time;
            printf("{region: %s, io_count: %d, took: %ld μs}\n", test.region.value, xdb_get_io_count(&test.searcher), c_time);
        }

        // free the region
        xdb_region_buffer_free(&test.region);
    }

    destroy_searcher_test(&test);
    xdb_clean_winsock();
    printf("searcher test program exited, thanks for trying\n");
}

void test_bench(int argc, char *argv[]) {
    int i, n, err;
    char *r, key[33] = {'\0'}, val[256] = {'\0'};
    char db_file[256] = {'\0'}, src_file[256] = {'\0'}, cache_policy[16] = {"vectorIndex"};

    FILE *handle;
    char line[1024] = {'\0'}, sip_str[INET6_ADDRSTRLEN+1] = {'\0'}, eip_str[INET6_ADDRSTRLEN+1] = {'\0'};
    char src_region[512] = {'\0'};
    int count = 0, took;
    long s_time, t_time, c_time = 0;

    // ip parse
    xdb_version_t *s_version, *e_version;
    bytes_ip_t sip_bytes[16] = {'\0'}, eip_bytes[16] = {'\0'};
    string_ip_t ip_string[INET6_ADDRSTRLEN] = {'\0'};
    bytes_ip_t *ip_list[2];

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

    // init the win sock
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
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
        n = sscanf(line, "%46[^|]|%46[^|]|%511[^\n]", sip_str, eip_str, src_region);
        if (n != 3) {
            printf("invalid ip segment line `%s`\n", line);
            return;
        }

        s_version = xdb_parse_ip(sip_str, sip_bytes, sizeof(sip_bytes));
        if (s_version == NULL) {
            printf("invalid start ip `%s`\n", sip_str);
            return;
        }

        e_version = xdb_parse_ip(eip_str, eip_bytes, sizeof(eip_bytes));
        if (e_version == NULL) {
            printf("invalid end ip `%s`\n", sip_str);
            return;
        }

        if (s_version->id != e_version->id) {
            printf("start ip and end ip version not match for line `%s`\n", line);
            return;
        }

        if (xdb_ip_sub_compare(sip_bytes, s_version->bytes, (string_ip_t *) eip_bytes, 0) > 0) {
            printf("start ip(%s) should not be greater than end ip(%s)\n", sip_str, eip_str);
            return;
        }

        ip_list[0] = sip_bytes;
        ip_list[1] = eip_bytes;
        for (i = 0; i < 2; i++) {
            t_time = xdb_now();
            err = xdb_search(&test.searcher, ip_list[i], s_version->bytes, &test.region);
            c_time += xdb_now() - t_time;
            if (err != 0) {
                xdb_ip_to_string(ip_list[i], s_version->bytes, ip_string, sizeof(ip_string));
                printf("failed to search ip `%s` with errno=%d\n", ip_string, err);
                return;
            }

            // check the region info
            if (strcmp(test.region.value, src_region) != 0) {
                xdb_ip_to_string(ip_list[i], s_version->bytes, ip_string, sizeof(ip_string));
                printf("failed to search(%s) with (%s != %s)\n", ip_string, test.region.value, src_region);
                return;
            }

            // free the region buffer
            xdb_region_buffer_free(&test.region);

            count++;
        }
    };

    took = xdb_now() - s_time;
    destroy_searcher_test(&test);
    xdb_clean_winsock();
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
