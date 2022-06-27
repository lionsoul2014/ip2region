// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "xdb_searcher.h"

XDB_PRIVATE(int) xdb_new_base(xdb_searcher_t *xdb, const char *dbPath, const char *vIndex, const char *cBuff) {
    memset(xdb, 0x00, sizeof(xdb_searcher_t));

    // check the content buffer first
    if (cBuff != NULL) {
        xdb->vector_index = NULL;
        xdb->content_buff = cBuff;
        return 0;
    }

    // open the xdb binary file
    FILE *handle = fopen(dbPath, "r");
    if (handle == NULL) {
        return 1;
    }

    xdb->handle = handle;
    xdb->vector_index = vIndex;

    return 0;
}

// xdb searcher new api define
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_searcher_t *xdb, char *dbPath) {
    return xdb_new_base(xdb, dbPath, NULL, NULL);
}

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_searcher_t *xdb, char *dbPath, char *vIndex) {
    return xdb_new_base(xdb, dbPath, vIndex, NULL);
}

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_searcher_t *xdb, char *cBuff) {
    return xdb_new_base(xdb, NULL, NULL, cBuff);
}

XDB_PUBLIC(void) xdb_close(xdb_searcher_t *xdb) {
    if (xdb->handle != NULL) {
        fclose(xdb->handle);
    }
}

// xdb searcher search api define
XDB_PUBLIC(int) xdb_search(unsigned int ip, char *buffer) {
    return 0;
}

XDB_PUBLIC(int) xdb_search_by_string(const char *ip, char *buffer) {
    return 0;
}

// get unsigned long (4bytes) from a specified buffer start from the specified offset
XDB_PUBLIC(unsigned int) get_unsigned_int(const char *buffer, int offset) {
    return (
        ((buffer[offset  ]) & 0x000000FF) |
        ((buffer[offset+1] <<  8) & 0x0000FF00) |
        ((buffer[offset+2] << 16) & 0x00FF0000) |
        ((buffer[offset+3] << 24) & 0xFF000000)
    );
}

// get unsigned short (2bytes) from a specified buffer start from the specified offset
XDB_PUBLIC(unsigned int) get_unsigned_short(const char *buffer, int offset) {
    return (
        ((buffer[offset  ]) & 0x000000FF) |
        ((buffer[offset+1] << 8) & 0x0000FF00)
    );
}

// string ip to unsigned int
static int shiftIndex[4] = {24, 16, 8, 0};
XDB_PUBLIC(int) check_ip(const char *src_ip, unsigned int *dst_ip) {
    char c;
    int i, n, ip = 0;
    const char *ptr = src_ip;
    for (i = 0; i < 4; i++) {
        n = 0;
        while (1) {
            c = *ptr;
            ptr++;
            if (c >= '0' && c <= '9') {
                n *= 10;
                n += c - '0';
            } else if ((i < 3 && c == '.') || i == 3) {
                // stopping at the '.' but ignore the tailing chars
                // after the 3rd one (auto clean the tailing none-integer ?).
                break;
            } else {
                return 1;
            }
        }

        if (n > 0xFF) {
            return 2;
        }

        ip |= (n << shiftIndex[i]);
    }

    *dst_ip = ip;
    return 0;
}

// unsigned int ip to string ip
XDB_PUBLIC(void) long2ip(unsigned int ip, char *buffer) {
    sprintf(buffer, "%d.%d.%d.%d", (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, (ip >> 8) & 0xFF, ip & 0xFF);
}