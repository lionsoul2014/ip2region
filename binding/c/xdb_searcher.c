// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "xdb_api.h"

// --- region buffer
XDB_PUBLIC(int) xdb_region_buffer_init(xdb_region_buffer_t *region, char *buffer, size_t length) {
    if (buffer == NULL) {
        region->type   = xdb_region_buffer_auto;
        region->length = 0;
    } else if (length <= 0) {
        return 1;
    } else {
        region->type   = xdb_region_buffer_wrapper;
        region->length = length;
        memset(buffer, 0x00, length);   // zero-fill the buffer
    }

    region->value = buffer;
    return 0;
}

XDB_PUBLIC(int) xdb_region_buffer_alloc(xdb_region_buffer_t *region, int length) {
    if (length <= 0) {
        return 1;
    }

    // no allocation supports for the buffer wapper
    if (region->type == xdb_region_buffer_wrapper) {
        if (length >= region->length) {
            return 2;
        }

        region->value[length] = '\0';
        return 0;
    }

    // ensure that the value were freed
    // by calling #xdb_region_buffer_free
    if (region->value != NULL) {
        return 3;
    }

    char *ptr = (char *) xdb_malloc(length + 1);
    if (ptr == NULL) {
        return 4;
    }

    ptr[length]    = '\0';  // NULL-end
    region->value  = ptr;
    region->length = length;
    return 0;
}

XDB_PUBLIC(void) xdb_region_buffer_free(xdb_region_buffer_t *region) {
    if (region->type == xdb_region_buffer_auto) {
        xdb_free(region->value);
        region->value = NULL;
    }
}

// --- END region buffer

// internal function prototype define
XDB_PRIVATE(int) read(xdb_searcher_t *, long offset, char *, size_t length);

XDB_PRIVATE(int) xdb_new_base(xdb_version_t *version, xdb_searcher_t *xdb, const char *db_path, const xdb_vector_index_t *v_index, const xdb_content_t *c_buffer) {
    memset(xdb, 0x00, sizeof(xdb_searcher_t));

    // set the version
    xdb->version = version;

    // check the content buffer first
    if (c_buffer != NULL) {
        xdb->v_index = NULL;
        xdb->content = c_buffer;
        return 0;
    }

    // open the xdb binary file
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return 1;
    }

    xdb->handle = handle;
    xdb->v_index = v_index;

    return 0;
}

// xdb searcher new api define
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_version_t *version, xdb_searcher_t *xdb, const char *db_path) {
    return xdb_new_base(version, xdb, db_path, NULL, NULL);
}

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_version_t *version, xdb_searcher_t *xdb, const char *db_path, const xdb_vector_index_t *v_index) {
    return xdb_new_base(version, xdb, db_path, v_index, NULL);
}

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_version_t *version, xdb_searcher_t *xdb, const xdb_content_t *c_buffer) {
    return xdb_new_base(version, xdb, NULL, NULL, c_buffer);
}

XDB_PUBLIC(void) xdb_close(void *ptr) {
    xdb_searcher_t *xdb = (xdb_searcher_t *) ptr;
    if (xdb->handle != NULL) {
        fclose(xdb->handle);
        xdb->handle = NULL;
    }
}

// --- xdb searcher search api define

XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *xdb, const string_ip_t *ip_string, xdb_region_buffer_t *region) {
    bytes_ip_t ip_bytes[16] = {'\0'};
    xdb_version_t *version = xdb_parse_ip(ip_string, ip_bytes, sizeof(ip_bytes));
    if (version == NULL) {
        return 10;
    } else {
        return xdb_search(xdb, ip_bytes, version->bytes, region);
    }
}

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *xdb, const bytes_ip_t *ip_bytes, int ip_len, xdb_region_buffer_t *region) {
    int il0, il1, idx, err, bytes, d_bytes;
    register int seg_index_size, l, h, m, p;
    unsigned int s_ptr, e_ptr, data_ptr, data_len;
    char vector_buffer[xdb_vector_index_size];
    char segment_buffer[xdb_v6_index_size];

    // ip version check
    if (ip_len != xdb->version->bytes) {
        return -1;
    }

    // some resets
    err = 0;
    data_len = 0;
    bytes   = xdb->version->bytes;
    d_bytes = xdb->version->bytes << 1;
    xdb->io_count = 0;

    // locate the segment index block based on the vector index
    il0 = (int) (ip_bytes[0]);
    il1 = (int) (ip_bytes[1]);
    idx = il0 * xdb_vector_index_cols * xdb_vector_index_size + il1 * xdb_vector_index_size;
    if (xdb->v_index != NULL) {
        s_ptr = xdb_le_get_uint32(xdb->v_index->buffer, idx);
        e_ptr = xdb_le_get_uint32(xdb->v_index->buffer, idx + 4);
    } else if (xdb->content != NULL) {
        s_ptr = xdb_le_get_uint32(xdb->content->buffer, xdb_header_info_length + idx);
        e_ptr = xdb_le_get_uint32(xdb->content->buffer, xdb_header_info_length + idx + 4);
    } else {
        err = read(xdb, xdb_header_info_length + idx, vector_buffer, sizeof(vector_buffer));
        if (err != 0) {
            return 10 + err;
        }

        s_ptr = xdb_le_get_uint32(vector_buffer, 0);
        e_ptr = xdb_le_get_uint32(vector_buffer, 4);
    }

    // printf("s_ptr=%u, e_ptr=%u\n", s_ptr, e_ptr);
    // binary search to get the final region info
    // segment_buffer = xdb_malloc(seg_index_size);
    seg_index_size = xdb->version->segment_index_size;
    data_len = 0, data_ptr = 0;
    l = 0, h = ((int) (e_ptr - s_ptr)) / seg_index_size;
    while (l <= h) {
        m = (l + h) >> 1;
        p = s_ptr + m * seg_index_size;

        // read the segment index item
        err = read(xdb, p, segment_buffer, seg_index_size);
        if (err != 0) {
            return 20 + err;
        }

        // decode the data fields as needed
        if (xdb->version->ip_compare(ip_bytes, bytes, segment_buffer, 0) < 0) {
            h = m - 1;
        } else if (xdb->version->ip_compare(ip_bytes, bytes, segment_buffer, bytes) > 0) {
            l = m + 1;
        } else {
            data_len = xdb_le_get_uint16(segment_buffer, d_bytes);
            data_ptr = xdb_le_get_uint32(segment_buffer, d_bytes + 2);
            break;
        }
    }

    // printf("data_len=%u, data_ptr=%u\n", data_len, data_ptr);
    if (data_len == 0) {
        return 100;
    }

    // buffer alloc checking
    err = xdb_region_buffer_alloc(region, data_len);
    if (err != 0) {
        return 100 + err;
    }

    err = read(xdb, data_ptr, region->value, data_len);
    if (err != 0) {
        return 30 + err;
    }

    return err;
}

XDB_PRIVATE(int) read(xdb_searcher_t *xdb, long offset, char *buffer, size_t length) {
    // check the xdb content cache first
    if (xdb->content != NULL) {
        memcpy(buffer, xdb->content->buffer + offset, length);
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

XDB_PUBLIC(xdb_version_t *) xdb_get_version(xdb_searcher_t *xdb) {
    return xdb->version;
}

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *xdb) {
    return xdb->io_count;
}
