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

// memory allocation error
#define XDB_ALLOCATE_ERROR(func, bytes)    \
do { \
    printf("<XDB>: Allocate Error In Function <%s> For %lu Bytes.\n", func, (unsigned long int) bytes);    \
    return NULL; \
} while (0);


#define xdb_calloc( _blocks, _bytes )  calloc( _blocks, _bytes )
#define xdb_malloc( _bytes )           malloc( _bytes )
#define xdb_free( _ptr )               free( _ptr )
#define field_size(type, field)        sizeof(((type *)0)->field)
#define sf_field_size(type, field)     (sizeof(((type *)0)->field) - 1)


// public constants define
#define HeaderInfoLength 256
#define VectorIndexRows  256
#define VectorIndexCols  256
#define VectorIndexSize  8
#define SegmentIndexSize 14


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

XDB_PUBLIC(int) xdb_load_header(FILE *, char *, size_t);

XDB_PUBLIC(int) xdb_load_header_from_file(char *, char *, size_t);

XDB_PUBLIC(int) xdb_load_vector_index(FILE *, char *, size_t);

XDB_PUBLIC(int) xdb_load_vector_index_from_file(char *, char *, size_t);

XDB_PUBLIC(int) xdb_load_content(FILE *, char *, size_t);

XDB_PUBLIC(int) xdb_load_content_from_file(char *, char *, size_t);

// --- End buffer load


// get unsigned long (4bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(unsigned int) get_unsigned_int(const char *, int);

// get unsigned short (2bytes) from a specified buffer start from the specified offset with little-endian
XDB_PUBLIC(unsigned int) get_unsigned_short(const char *, int);

// check the specified string ip and convert it to an unsigned int
XDB_PUBLIC(int) check_ip(const char *, unsigned int *);

// unsigned int ip to string ip
XDB_PUBLIC(void) long2ip(unsigned int, char *);


#endif //C_XDB_SEARCHER_H
