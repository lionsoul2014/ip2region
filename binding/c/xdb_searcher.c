// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "sys/time.h"
#include "xdb_searcher.h"

// internal function prototype define
XDB_PRIVATE(int) read(xdb_searcher_t *, long offset, char *, size_t length);

XDB_PRIVATE(int) xdb_new_base(xdb_searcher_t *xdb, const char *db_path, const char *vIndex, const char *cBuff) {
    memset(xdb, 0x00, sizeof(xdb_searcher_t));

    // check the content buffer first
    if (cBuff != NULL) {
        xdb->vector_index = NULL;
        xdb->content_buff = cBuff;
        return 0;
    }

    // open the xdb binary file
    FILE *handle = fopen(db_path, "r");
    if (handle == NULL) {
        return 1;
    }

    xdb->handle = handle;
    xdb->vector_index = vIndex;

    return 0;
}

// xdb searcher new api define
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_searcher_t *xdb, const char *db_path) {
    return xdb_new_base(xdb, db_path, NULL, NULL);
}

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_searcher_t *xdb, const char *db_path, const char *vIndex) {
    return xdb_new_base(xdb, db_path, vIndex, NULL);
}

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_searcher_t *xdb, const char *c_buffer) {
    return xdb_new_base(xdb, NULL, NULL, c_buffer);
}

XDB_PUBLIC(void) xdb_close(xdb_searcher_t *xdb) {
    if (xdb->handle != NULL) {
        fclose(xdb->handle);
        xdb->handle = NULL;
    }
}

// --- xdb searcher search api define

XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *xdb, const char *str_ip, char *region_buffer, size_t length) {
    unsigned int ip = 0;
    int errcode = xdb_check_ip(str_ip, &ip);
    if (errcode != 0) {
        return 10 + errcode;
    } else {
        return xdb_search(xdb, ip, region_buffer, length);
    }
}

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *xdb, unsigned int ip, char *region_buffer, size_t length) {
    int il0, il1, idx, err, l, h, m, data_len;
    unsigned int s_ptr, e_ptr, p, sip, eip, data_ptr;
    char vector_buffer[xdb_vector_index_size], segment_buffer[xdb_segment_index_size];

    // reset the io counter
    xdb->io_count = 0;

    // locate the segment index block based on the vector index
    il0 = ((int) (ip >> 24)) & 0xFF;
    il1 = ((int) (ip >> 16)) & 0xFF;
    idx = il0 * xdb_vector_index_cols * xdb_vector_index_size + il1 * xdb_vector_index_size;
    if (xdb->vector_index != NULL) {
        s_ptr = xdb_get_uint(xdb->vector_index, idx);
        e_ptr = xdb_get_uint(xdb->vector_index, idx + 4);
    } else if (xdb->content_buff != NULL) {
        s_ptr = xdb_get_uint(xdb->content_buff, xdb_header_info_length + idx);
        e_ptr = xdb_get_uint(xdb->content_buff, xdb_header_info_length + idx + 4);
    } else {
        err = read(xdb, xdb_header_info_length + idx, vector_buffer, sizeof(vector_buffer));
        if (err != 0) {
            return 10 + err;
        }

        s_ptr = xdb_get_uint(vector_buffer, 0);
        e_ptr = xdb_get_uint(vector_buffer, 4);
    }

    // printf("s_ptr=%u, e_ptr=%u\n", s_ptr, e_ptr);
    // binary search to get the final region info
    data_len = 0, data_ptr = 0;
    l = 0, h = ((int) (e_ptr - s_ptr)) / xdb_segment_index_size;
    while (l <= h) {
        m = (l + h) >> 1;
        p = s_ptr + m * xdb_segment_index_size;

        // read the segment index item
        err = read(xdb, p, segment_buffer, sizeof(segment_buffer));
        if (err != 0) {
            return 20 + err;
        }

        // decode the data fields as needed
        sip = xdb_get_uint(segment_buffer, 0);
        if (ip < sip) {
            h = m - 1;
        } else {
            eip = xdb_get_uint(segment_buffer, 4);
            if (ip > eip) {
                l = m + 1;
            } else {
                data_len = xdb_get_ushort(segment_buffer, 8);
                data_ptr = xdb_get_uint(segment_buffer, 10);
                break;
            }
        }
    }

    // printf("data_len=%u, data_ptr=%u\n", data_len, data_ptr);
    if (data_len == 0) {
        region_buffer[0] = '\0';
        return 0;
    }

    // buffer length checking
    if (data_len >= length) {
        return 1;
    }

    err = read(xdb, data_ptr, region_buffer, data_len);
    if (err != 0) {
        return 30 + err;
    }

    // auto append a NULL-end
    region_buffer[data_len] = '\0';
    return 0;
}

XDB_PRIVATE(int) read(xdb_searcher_t *xdb, long offset, char *buffer, size_t length) {
    // check the xdb content cache first
    if (xdb->content_buff != NULL) {
        memcpy(buffer, xdb->content_buff + offset, length);
        return 0;
    }

    // seek to the offset
    if (fseek(xdb->handle, offset, SEEK_SET) == -1) {
        return 1;
    }

    xdb->io_count++;
    if (fread(buffer, 1, length, xdb->handle) != length) {
        return 2;
    }

    return 0;
}

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *xdb) {
    return xdb->io_count;
}


// --- buffer load util functions

XDB_PUBLIC(int) xdb_load_header(FILE *handle, xdb_header_t *header) {
    char buffer[256];

    if (fseek(handle, 0, SEEK_SET) == -1) {
        return 1;
    }

    if (fread(buffer, 1, 256, handle) != 256) {
        return 2;
    }

    // fill the fields
    header->version = (unsigned short) xdb_get_ushort(buffer, 0);
    header->index_policy = (unsigned short) xdb_get_ushort(buffer, 2);
    header->created_at = xdb_get_uint(buffer, 4);
    header->start_index_ptr = xdb_get_uint(buffer, 8);
    header->end_index_ptr = xdb_get_uint(buffer,12);

    return 0;
}

XDB_PUBLIC(int) xdb_load_header_from_file(const char *db_path, xdb_header_t *header) {
    FILE *handle = fopen(db_path, "r");
    if (handle == NULL) {
        return 10;
    }

    return xdb_load_header(handle, header);
}

XDB_PUBLIC(char *) xdb_load_vector_index(FILE *handle) {
    char *ptr = NULL;
    int size = xdb_vector_index_length;

    // seek to the vector index offset
    if (fseek(handle, xdb_header_info_length, SEEK_SET) == -1) {
        return NULL;
    }

    // do the buffer read
    ptr = (char *) xdb_malloc(size);
    if (ptr == NULL) {
        return NULL;
    }

    if (fread(ptr, 1, size, handle) != size) {
        xdb_free(ptr);
        return NULL;
    }

    return ptr;
}

XDB_PUBLIC(char *) xdb_load_vector_index_from_file(const char *db_path) {
    FILE *handle = fopen(db_path, "r");
    if (handle == NULL) {
        return NULL;
    }

    return xdb_load_vector_index(handle);
}

XDB_PUBLIC(char *) xdb_load_content(FILE *handle) {
    long filesize;
    char *ptr = NULL;

    // determine the file size
    if (fseek(handle, 0, SEEK_END) == -1) {
        return NULL;
    }

    filesize = ftell(handle);
    if (fseek(handle, 0, SEEK_SET) == -1) {
        return NULL;
    }

    // do the file read
    ptr = (char *) xdb_malloc(filesize);
    if (ptr == NULL) {
        return NULL;
    }

    // read the content into the buffer
    if (fread(ptr, 1, filesize, handle) != filesize) {
        xdb_free(ptr);
        return NULL;
    }

    return ptr;
}

XDB_PUBLIC(char *) xdb_load_content_from_file(const char *db_path) {
    FILE *handle = fopen(db_path, "r");
    if (handle == NULL) {
        return NULL;
    }

    return xdb_load_content(handle);
}

// --- End

// get unsigned long (4bytes) from a specified buffer start from the specified offset
XDB_PUBLIC(unsigned int) xdb_get_uint(const char *buffer, int offset) {
    return (
        ((buffer[offset  ]) & 0x000000FF) |
        ((buffer[offset+1] <<  8) & 0x0000FF00) |
        ((buffer[offset+2] << 16) & 0x00FF0000) |
        ((buffer[offset+3] << 24) & 0xFF000000)
    );
}

// get unsigned short (2bytes) from a specified buffer start from the specified offset
XDB_PUBLIC(int) xdb_get_ushort(const char *buffer, int offset) {
    return (
        ((buffer[offset  ]) & 0x000000FF) |
        ((buffer[offset+1] << 8) & 0x0000FF00)
    );
}

// string ip to unsigned int
static int shiftIndex[4] = {24, 16, 8, 0};
XDB_PUBLIC(int) xdb_check_ip(const char *src_ip, unsigned int *dst_ip) {
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
XDB_PUBLIC(void) xdb_long2ip(unsigned int ip, char *buffer) {
    sprintf(buffer, "%d.%d.%d.%d", (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, (ip >> 8) & 0xFF, ip & 0xFF);
}

// get the middle ip of a and b
XDB_PUBLIC(unsigned int) xdb_mip(unsigned long a, unsigned long b) {
    return (unsigned int) ((a + b) >> 1);
}

XDB_PUBLIC(long) xdb_now() {
    struct timeval c_time;
    gettimeofday(&c_time, NULL);
    return c_time.tv_sec * (int)1e6 + c_time.tv_usec;
}
