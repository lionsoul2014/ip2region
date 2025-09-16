// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "xdb_api.h"

// for Linux
#ifdef XDB_LINUX
#include "sys/time.h"
#endif

// @Note: since 2023/10/13 to compatible with the windows system
#ifdef XDB_WINDOWS
#include <windows.h>
XDB_PRIVATE(int) gettimeofday(struct timeval* tp, void* tzp) {
    time_t clock;
    struct tm tm;
    SYSTEMTIME wtm;
    GetLocalTime(&wtm);
    tm.tm_year = wtm.wYear - 1900;
    tm.tm_mon = wtm.wMonth - 1;
    tm.tm_mday = wtm.wDay;
    tm.tm_hour = wtm.wHour;
    tm.tm_min = wtm.wMinute;
    tm.tm_sec = wtm.wSecond;
    tm.tm_isdst = -1;
    clock = mktime(&tm);
    tp->tv_sec = clock;
    tp->tv_usec = wtm.wMilliseconds * 1000;
    return (0);
}
#endif

XDB_PUBLIC(long) xdb_now() {
    struct timeval c_time;
    gettimeofday(&c_time, NULL);
    return c_time.tv_sec * (int)1e6 + c_time.tv_usec;
}

XDB_PUBLIC(unsigned int) xdb_le_get_uint32(const char *buffer, int offset) {
    return (
        ((buffer[offset  ]) & 0x000000FF) |
        ((buffer[offset+1] <<  8) & 0x0000FF00) |
        ((buffer[offset+2] << 16) & 0x00FF0000) |
        ((buffer[offset+3] << 24) & 0xFF000000)
    );
}

XDB_PUBLIC(int) xdb_le_get_uint16(const char *buffer, int offset) {
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

XDB_PUBLIC(int) xdb_parse_ip(const char *ip, const char *buffer, size_t length) {
    return 0;
}

XDB_PUBLIC(int) xdb_ip_to_string(const char *bytes, size_t length) {
    return 0;
}

XDB_PUBLIC(int) xdb_ip_sub_compare(const char *ip1, const char *buffer, int offset, size_t length) {
    return 0;
}


// --- xdb buffer function implementations

XDB_PUBLIC(xdb_header_t *) xdb_load_header(FILE *handle) {
    xdb_header_t *header;
    unsigned int size = xdb_header_info_length;

    // entry alloc
    header = (xdb_header_t *) xdb_malloc(sizeof(xdb_header_t));
    if (header == NULL) {
        return NULL;
    }

    if (fseek(handle, 0, SEEK_SET) == -1) {
        xdb_free(header);
        return NULL;
    }

    if (fread(header->buffer, 1,size, handle) != size) {
        xdb_free(header);
        return NULL;
    }

    // fill the fields
    header->length = size;
    header->version = (unsigned short) xdb_le_get_uint16(header->buffer, 0);
    header->index_policy = (unsigned short) xdb_le_get_uint16(header->buffer, 2);
    header->created_at = xdb_le_get_uint32(header->buffer, 4);
    header->start_index_ptr = xdb_le_get_uint32(header->buffer, 8);
    header->end_index_ptr = xdb_le_get_uint32(header->buffer,12);

    // since IPv6 supporting
    header->ip_version = xdb_le_get_uint16(header->buffer, 16);
    header->runtime_ptr_bytes = xdb_le_get_uint16(header->buffer, 18);

    return header;
}

XDB_PUBLIC(xdb_header_t *) xdb_load_header_from_file(const char *db_path) {
    xdb_header_t *header;
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return NULL;
    }

    header = xdb_load_header(handle);
    fclose(handle);
    return header;
}

XDB_PUBLIC(void) xdb_free_header(void *ptr) {
    xdb_header_t *header = (xdb_header_t *) ptr;
    if (header->length > 0) {
        header->length = 0;
        xdb_free(header);
    }
}

// --- vector index

XDB_PUBLIC(xdb_vector_index_t *) xdb_load_vector_index(FILE *handle) {
    xdb_vector_index_t *v_index;
    unsigned int size = xdb_vector_index_length;

    // seek to the vector index offset
    if (fseek(handle, xdb_header_info_length, SEEK_SET) == -1) {
        return NULL;
    }

    // do the buffer read
    v_index = (xdb_vector_index_t *) xdb_malloc(sizeof(xdb_vector_index_t));
    if (v_index == NULL) {
        return NULL;
    }

    v_index->length = size;
    if (fread(v_index->buffer, 1, size, handle) != size) {
        xdb_free(v_index);
        return NULL;
    }

    return v_index;
}

XDB_PUBLIC(xdb_vector_index_t *) xdb_load_vector_index_from_file(const char *db_path) {
    xdb_vector_index_t *v_index;
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return NULL;
    }

    v_index = xdb_load_vector_index(handle);
    fclose(handle);
    return v_index;
}

XDB_PUBLIC(void) xdb_free_vector_index(void *ptr) {
    xdb_vector_index_t *v_index = (xdb_vector_index_t *) ptr;
    if (v_index->length > 0) {
        v_index->length = 0;
        xdb_free(v_index);
    }
}

// --- content buffer

XDB_PUBLIC(xdb_content_t *) xdb_load_content(FILE *handle) {
    unsigned int size;
    xdb_content_t *content;

    // determine the file size
    if (fseek(handle, 0, SEEK_END) == -1) {
        return NULL;
    }

    size = (unsigned int) ftell(handle);
    if (fseek(handle, 0, SEEK_SET) == -1) {
        return NULL;
    }

    // do the file read
    content = (xdb_content_t *) xdb_malloc(sizeof(xdb_content_t));
    if (content == NULL) {
        return NULL;
    }

    // do the buffer alloc
    content->buffer = (char *) xdb_malloc(size);
    if (content->buffer == NULL) {
        xdb_free(content);
        return NULL;
    }

    // read the content into the buffer
    content->length = size;
    if (fread(content->buffer, 1, size, handle) != size) {
        xdb_free(content);
        return NULL;
    }

    return content;
}

XDB_PUBLIC(xdb_content_t *) xdb_load_content_from_file(const char *db_path) {
    xdb_content_t *content;
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return NULL;
    }

    content = xdb_load_content(handle);
    fclose(handle);
    return content;
}

XDB_PUBLIC(void) xdb_free_content(void *ptr) {
    xdb_content_t *content = (xdb_content_t *) ptr;
    if (content->length > 0) {
        content->length = 0;
        xdb_free(content->buffer);
        content->buffer = NULL;
        xdb_free(content);
    }
}

// --- End
