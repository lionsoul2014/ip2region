// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#ifndef C_XDB_SEARCHER_H
#define C_XDB_SEARCHER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if ( defined(WIN32) || defined(_WIN32) || defined(__WINDOWS_) || defined(WINNT) )
#   define XDB_PUBLIC(type)    extern __declspec(dllexport) type
#   define XDB_PRIVATE(type)   static type
#   define XDB_WINDOWS
#elif ( defined(linux) || defined(_UNIX) )
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


// xdb searcher structure
struct xdb_searcher_entry {
    FILE *handle;

    // header info
    const char *header;
    int io_count;

    // vector index buffer cache.
    // preload the vector index will reduce the number of IO operations
    // thus speedup the search process.
    const char *vector_index;

    // content buffer.
    // cache the whole xdb content.
    const char *content_buff;
};
typedef struct xdb_searcher_entry xdb_searcher_t;

// xdb searcher new api define
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_searcher_t *, char *);

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_searcher_t *, char *, char *);

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_searcher_t *, char *);

XDB_PUBLIC(void) xdb_close(xdb_searcher_t *);

// xdb searcher search api define
XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *, const char *, char *, size_t);

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *, unsigned int, char *, size_t);

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *);


// --- buffer load util functions

struct xdb_header {
    unsigned short version;
    unsigned short index_policy;
    unsigned int created_at;
    unsigned int start_index_ptr;
    unsigned int end_index_ptr;
};
typedef struct xdb_header xdb_header_t;

XDB_PUBLIC(int) xdb_load_header(FILE *, xdb_header_t *);

XDB_PUBLIC(int) xdb_load_header_from_file(char *, xdb_header_t *);

XDB_PUBLIC(char *) xdb_load_vector_index(FILE *);

XDB_PUBLIC(char *) xdb_load_vector_index_from_file(char *);

XDB_PUBLIC(char *) xdb_load_content(FILE *);

XDB_PUBLIC(char *) xdb_load_content_from_file(char *);

// --- End buffer load


// get unsigned long (4bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(unsigned int) get_unsigned_int(const char *, int);

// get unsigned short (2bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(int) get_unsigned_short(const char *, int);

// check the specified string ip and convert it to an unsigned int
XDB_PUBLIC(int) check_ip(const char *, unsigned int *);

// unsigned int ip to string ip
XDB_PUBLIC(void) long2ip(unsigned int, char *);

// get the current time in microseconds
XDB_PUBLIC(long) xdb_now();


#endif //C_XDB_SEARCHER_H
