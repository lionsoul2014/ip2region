// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "xdb_api.h"
#include <ctype.h>

// for Linux
#ifdef XDB_LINUX
#include "sys/time.h"
#endif

#ifdef XDB_WINDOWS
#include <time.h>
#endif

// @Note: since 2023/10/13 to compatible with the windows system
#ifdef XDB_WINDOWS
static int winsock_initialized = 0;
XDB_PUBLIC(int) xdb_init_winsock() {
    if (winsock_initialized == 1) {
        return 0;
    }

    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2,2), &wsaData) != 0) {
        return -1; 
    }
    winsock_initialized = 1;
    return 0;
}

XDB_PUBLIC(void) xdb_clean_winsock() {
    if (winsock_initialized == 1) {
        WSACleanup();
        winsock_initialized = 0;
    }
}

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
#else
XDB_PUBLIC(int) xdb_init_winsock() {return 0;}
XDB_PUBLIC(void) xdb_clean_winsock() {}
#endif

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

XDB_PUBLIC(int) xdb_verify_from_header(FILE *handle, xdb_header_t *header) {
    int runtime_ptr_bytes = 0;  // runtime ptr bytes
    if (header->version == xdb_structure_20) {
        runtime_ptr_bytes = 4;
    } else if (header->version == xdb_structure_30) {
        runtime_ptr_bytes = header->runtime_ptr_bytes;
    } else {
        return 2;
    }

    // 1, confirm the xdb file size.
    // to ensure that the maximum file pointer does not overflow.
    int err = fseek(handle, 0L, SEEK_END);
    if (err != 0) {
        return 3;
    }

    long int fileBytes = ftell(handle);
    long int maxFilePtr = (1L << (runtime_ptr_bytes * 8)) - 1;
    // printf("fileBytes: %ld, maxFilePtr: %ld\n", fileBytes, maxFilePtr);
    if (fileBytes > maxFilePtr) {
        return 4;
    }

    return 0;
}

XDB_PUBLIC(int) xdb_verify(FILE *handle) {
    xdb_header_t *header = xdb_load_header(handle);
    if (header == NULL) {
        return 1;
    }

    int errcode = xdb_verify_from_header(handle, header);
    if (errcode != 0) {
        goto done;
    }

    // what next ?
done:
    xdb_free_header(header);
    return errcode;
}

XDB_PUBLIC(int) xdb_verify_from_file(const char *db_path) {
    FILE *handle = fopen(db_path, "rb");
    if (handle == NULL) {
        return -1;
    }

    int r = xdb_verify(handle);
    fclose(handle);
    return r;
}

// --- End content buffer


// --- ip version

// ip compare for IPv4
// ip1 - with Big endian byte order parsed from an input
// ip2 - with Little endian byte order read from the xdb index.
// to compatiable with the Little Endian encoded IPv4 on xdb 2.0.
XDB_PRIVATE(int) _ipv4_sub_compare(const bytes_ip_t *ip_bytes, int bytes, const char *buffer, int offset) {
    register int i0, i1;
    for (int i = 0, j = offset + bytes - 1; i < bytes; i++, j--) {
        i0 = ip_bytes[i];
        i1 = buffer[j] & 0xFF;
        if (i0 > i1) {
            return 1;
        } else if (i0 < i1) {
            return -1;
        }
    }
    return 0;
}

static xdb_version_t _ip_version_list[] = {
    // 14 = 4 + 4 + 2 + 4
    {xdb_ipv4_id, "IPv4", xdb_ipv4_bytes, xdb_v4_index_size, _ipv4_sub_compare},

    // 38 = 16 + 16 + 2 + 4
    {xdb_ipv6_id, "IPv6", xdb_ipv6_bytes, xdb_v6_index_size, xdb_ip_sub_compare},

    // END
    {0, NULL, 0, 0, NULL}
};

XDB_PUBLIC(xdb_version_t *) xdb_version_v4() {
    return &_ip_version_list[0];
}

XDB_PUBLIC(xdb_version_t *) xdb_version_v6() {
    return &_ip_version_list[1];
}

XDB_PUBLIC(int) xdb_version_is_v4(const xdb_version_t *version) {
    return version->id == xdb_ipv4_id;
}

XDB_PUBLIC(int) xdb_version_is_v6(const xdb_version_t *version) {
    return version->id == xdb_ipv6_id;
}

XDB_PUBLIC(xdb_version_t *) xdb_version_from_name(char *name) {
    // to upper case the name
    for (int i = 0; name[i] != '\0'; i++) {
        name[i] = toupper((unsigned char) name[i]);
    }

    if (strcmp(name, "V4") == 0 || strcmp(name, "IPV4") == 0) {
        return xdb_version_v4();
    } else if (strcmp(name, "V6") == 0 || strcmp(name, "IPV6") == 0) {
        return xdb_version_v6();
    } else {
        return NULL;
    }
}

XDB_PUBLIC(xdb_version_t *) xdb_version_from_header(xdb_header_t *header) {
    // Old structure with ONLY IPv4 supports
    if (header->version == xdb_structure_20) {
        return xdb_version_v4();
    }

    // structure 3.0 with IPv6 supporting
    if (header->version != xdb_structure_30) {
        return NULL;
    }

    if (header->ip_version == xdb_ipv4_id) {
        return xdb_version_v4();
    } else if (header->ip_version == xdb_ipv6_id) {
        return xdb_version_v6();
    } else {
        return NULL;
    }
}

// --- END ip version

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

XDB_PUBLIC(xdb_version_t *) xdb_parse_ip(const string_ip_t *ip_string, bytes_ip_t *buffer, size_t length) {
    // version check
    if (strchr(ip_string, '.') != NULL && strchr(ip_string, ':') == NULL) {
        return xdb_parse_v4_ip(ip_string, buffer, length);
    } else if (strchr(ip_string, ':') != NULL) {
        return xdb_parse_v6_ip(ip_string, buffer, length);
    }

    return NULL;
}

XDB_PUBLIC(xdb_version_t *) xdb_parse_v4_ip(const string_ip_t *ip_string, bytes_ip_t *buffer, size_t length) {
    struct in_addr addr;

    // buffer length checking
    if (length < xdb_ipv4_bytes) {
        return NULL;
    }

    if (inet_pton(AF_INET, ip_string, &addr) != 1) {
        return NULL;
    }

    // encode the address to buffer with big endian byte bufffer.
    buffer[0] = (addr.s_addr) & 0xFF;
    buffer[1] = (addr.s_addr >> 8) & 0xFF;
    buffer[2] = (addr.s_addr >> 16) & 0xFF;
    buffer[3] = (addr.s_addr >> 24) & 0xFF;
    return XDB_IPv4;
}

XDB_PUBLIC(xdb_version_t *) xdb_parse_v6_ip(const string_ip_t *ip_string, bytes_ip_t *buffer, size_t length) {
    struct  in6_addr addr;

    // buffer length checking
    if (length < xdb_ipv6_bytes) {
        return NULL;
    }

    if (inet_pton(AF_INET6, ip_string, &addr) != 1) {
        return NULL;
    }

    memcpy(buffer, addr.s6_addr, xdb_ipv6_bytes);
    return XDB_IPv6;
}

XDB_PUBLIC(int) xdb_ip_to_string(const bytes_ip_t *ip_bytes, int bytes, char *ip_string, size_t length) {
    if (bytes == xdb_ipv4_bytes) {
        return xdb_v4_ip_to_string(ip_bytes, ip_string, length);
    } else if (bytes == xdb_ipv6_bytes) {
        return xdb_v6_ip_to_string(ip_bytes, ip_string, length);
    }

    return -1;
}

XDB_PUBLIC(int) xdb_v4_ip_to_string(const bytes_ip_t *ip_bytes, char *ip_string, size_t length) {
    if (!ip_bytes || !ip_string || length == 0) {
        return -1;
    }

    // buffer length checking
    if (length < INET_ADDRSTRLEN) {
        return -1;
    }

    if (inet_ntop(AF_INET, ip_bytes, ip_string, length) == NULL) {
        return -1;
    }

    return 0;
}

XDB_PUBLIC(int) xdb_v6_ip_to_string(const bytes_ip_t *ip_bytes, char *ip_string, size_t length) {
    if (!ip_bytes || !ip_string || length == 0) {
        return -1;
    }

    if (length < INET6_ADDRSTRLEN) {
        return -1;
    }

    if (inet_ntop(AF_INET6, ip_bytes, ip_string, length) == NULL) {
        return -1;
    }

    return 0;
}

XDB_PUBLIC(int) xdb_ip_sub_compare(const bytes_ip_t *ip1, int bytes, const char *buffer, int offset) {
    register int i, i1, i2;
    for (i = 0; i < bytes; i++) {
        i1 = ip1[i];
        i2 = buffer[offset + i] & 0xFF;
        if (i1 > i2) {
            return 1;
        } else if (i1 < i2) {
            return -1;
        }
    }
    return 0;
}
