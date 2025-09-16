// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#ifndef C_IP2REGION_XDB_H
#define C_IP2REGION_XDB_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if ( defined(WIN32) || defined(_WIN32) || defined(__WINDOWS_) || defined(WINNT) )
#   define XDB_PUBLIC(type)    extern __declspec(dllexport) type
#   define XDB_PRIVATE(type)   static type
#   define XDB_WINDOWS
#elif ( defined(linux) || defined(_UNIX) || defined(__APPLE__) )
#   define XDB_PUBLIC(type)    extern type
#   define XDB_PRIVATE(type)   static inline type
#   define XDB_LINUX
#endif

#define xdb_calloc( _blocks, _bytes )  calloc( _blocks, _bytes )
#define xdb_malloc( _bytes )           malloc( _bytes )
#define xdb_free( _ptr )               free( _ptr )

// public constants define
#define xdb_header_info_length 256
#define xdb_vector_index_rows  256
#define xdb_vector_index_cols  256
#define xdb_vector_index_size  8
#define xdb_segment_index_size 14

// cache of vector_index_row × vector_index_rows × vector_index_size
#define xdb_vector_index_length 524288

// --- xdb util functions

// get the current time in microseconds
XDB_PUBLIC(long) xdb_now();

// get unsigned long (4bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(unsigned int) xdb_le_get_uint32(const char *, int);

// get unsigned short (2bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(int) xdb_le_get_uint16(const char *, int);

// check the specified string ip and convert it to an unsigned int
XDB_PUBLIC(int) xdb_check_ip(const char *, unsigned int *);

// unsigned int ip to string ip
XDB_PUBLIC(void) xdb_long2ip(unsigned int, char *);


// parse the specified IP address to byte array
XDB_PUBLIC(int) xdb_parse_ip(const char *, const char *, size_t);

// convert a specified ip bytes to humen-readable string
XDB_PUBLIC(int) xdb_ip_to_string(const char *, size_t);

// compare the specified ip bytes with another ip bytes in the specified buff from offset.
// returns: -1 if ip1 < ip2, 1 if ip1 > ip2 or 0
XDB_PUBLIC(int) xdb_ip_sub_compare(const char *, const char *, int, size_t);

// --- END xdb utils


// --- xdb buffer functions

// use the following buffer struct to wrap the binary buffer data
// since the buffer data could not be operated with the string API.
struct xdb_header {
    unsigned short version;
    unsigned short index_policy;
    unsigned int created_at;
    unsigned int start_index_ptr;
    unsigned int end_index_ptr;
    
    // since 3.0+ with IPv6 supporting
    unsigned short ip_version;
    unsigned short runtime_ptr_bytes;

    // the original buffer
    unsigned int length;
    char buffer[xdb_header_info_length];
};
typedef struct xdb_header xdb_header_t;

XDB_PUBLIC(xdb_header_t *) xdb_load_header(FILE *);

XDB_PUBLIC(xdb_header_t *) xdb_load_header_from_file(const char *);

XDB_PUBLIC(void) xdb_free_header(void *);


// --- vector index buffer
struct xdb_vector_index {
    unsigned int length;
    char buffer[xdb_vector_index_length];
};
typedef struct xdb_vector_index xdb_vector_index_t;

XDB_PUBLIC(xdb_vector_index_t *) xdb_load_vector_index(FILE *);

XDB_PUBLIC(xdb_vector_index_t *) xdb_load_vector_index_from_file(const char *);

XDB_PUBLIC(void) xdb_free_vector_index(void *);


// --- content buffer
struct xdb_content {
    unsigned int length;
    char *buffer;
};
typedef struct xdb_content xdb_content_t;

XDB_PUBLIC(xdb_content_t *) xdb_load_content(FILE *);

XDB_PUBLIC(xdb_content_t *) xdb_load_content_from_file(const char *);

XDB_PUBLIC(void) xdb_free_content(void *);

// --- End xdb buffer

// --- xdb searcher api

// xdb searcher structure
struct xdb_searcher_entry {
    FILE *handle;

    // header info
    const char *header;
    int io_count;

    // vector index buffer cache.
    // preload the vector index will reduce the number of IO operations
    // thus speedup the search process.
    const xdb_vector_index_t *v_index;

    // content buffer.
    // cache the whole xdb content.
    const xdb_content_t *content;
};
typedef struct xdb_searcher_entry xdb_searcher_t;

// xdb searcher new api define
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_searcher_t *, const char *);

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_searcher_t *, const char *, const xdb_vector_index_t *);

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_searcher_t *, const xdb_content_t *);

XDB_PUBLIC(void) xdb_close(void *);

// xdb searcher search api define
XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *, const char *, char *, size_t);

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *, unsigned int, char *, size_t);

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *);

// --- END xdb searcher api

#endif // C_IP2REGION_XDB_H
