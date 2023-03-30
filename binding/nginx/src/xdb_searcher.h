// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

// @Modified By Wu Jian Ping <wujjpp@hotmail.com>
// @Date   2023/03/30

#ifndef C_XDB_SEARCHER_H
#define C_XDB_SEARCHER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct {
    unsigned short version;
    unsigned short index_policy;
    unsigned int created_at;
    unsigned int start_index_ptr;
    unsigned int end_index_ptr;

    // the original buffer
    unsigned int length;
    char buffer[xdb_header_info_length];
} xdb_header_t;

xdb_header_t* xdb_load_header(FILE *);

xdb_header_t *xdb_load_header_from_file(const char *);

void xdb_close_header(void *);

// --- vector index buffer
struct xdb_vector_index {
    unsigned int length;
    char buffer[xdb_vector_index_length];
};
typedef struct xdb_vector_index xdb_vector_index_t;

xdb_vector_index_t* xdb_load_vector_index(FILE *);

xdb_vector_index_t * xdb_load_vector_index_from_file(const char *);

void xdb_close_vector_index(void *);

// --- content buffer
typedef struct {
    unsigned int length;
    char *buffer;
} xdb_content_t;

xdb_content_t* xdb_load_content(FILE *);

xdb_content_t* xdb_load_content_from_file(const char *);

void xdb_close_content(void *);

// --- End buffer load

// xdb searcher structure
typedef struct xdb_searcher_entry {
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
} xdb_searcher_t;

// xdb searcher new api define
int xdb_new_with_file_only(xdb_searcher_t *, const char *);

int xdb_new_with_vector_index(xdb_searcher_t *, const char *, const xdb_vector_index_t *);

int xdb_new_with_buffer(xdb_searcher_t *, const xdb_content_t *);

void xdb_close(void *);

// xdb searcher search api define
int xdb_search_by_string(xdb_searcher_t *, const char *, char *, size_t);

int xdb_search(xdb_searcher_t *, unsigned int, char *, size_t);

int xdb_get_io_count(xdb_searcher_t *);

// get unsigned long (4bytes) from a specified buffer start from the specified offset with little-endian
unsigned int xdb_get_uint(const char *, int);

// get unsigned short (2bytes) from a specified buffer start from the specified offset with little-endian
int xdb_get_ushort(const char *, int);

// check the specified string ip and convert it to an unsigned int
int xdb_check_ip(const char *, unsigned int *);

// unsigned int ip to string ip
void xdb_long2ip(unsigned int, char *);

// get the middle ip of a and b
unsigned int xdb_mip(unsigned long, unsigned long);

// get the current time in microseconds
long xdb_now();

#endif //C_XDB_SEARCHER_H
