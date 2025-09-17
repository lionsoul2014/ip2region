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
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#elif ( defined(linux) || defined(_UNIX) || defined(__APPLE__) )
#   define XDB_PUBLIC(type)    extern type
#   define XDB_PRIVATE(type)   static inline type
#   define XDB_LINUX
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
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

// --- ip version info
#define xdb_ipv4_version_no 4
#define xdb_ipv6_version_no 6
#define xdb_ipv4_bytes 4
#define xdb_ipv6_bytes 16
// cache of vector_index_row × vector_index_rows × vector_index_size
#define xdb_vector_index_length 524288

// types type define
typedef char string_ip_t;
typedef unsigned char bytes_ip_t;

// --- xdb util functions

// to compatiable with the windows
// returns: 0 for ok and -1 for failed
XDB_PUBLIC(int) xdb_init_winsock();
XDB_PUBLIC(void) xdb_clean_winsock();

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


// parse the specified IP address to byte array.
// returns: 4 for valid ipv4, 16 for valid ipv6, or -1 for failed
XDB_PUBLIC(int) xdb_parse_ip(const string_ip_t *, bytes_ip_t *, size_t);

// parse the specified IPv4 address to byte array
// returns: 4 for valid ipv4, or -1 for failed
XDB_PUBLIC(int) xdb_parse_v4_ip(const string_ip_t *, bytes_ip_t *, size_t);

// parse the specified IPv6 address to byte array
// returns: 16 for valid ipv6, or -1 for failed
XDB_PUBLIC(int) xdb_parse_v6_ip(const string_ip_t *, bytes_ip_t *, size_t);

// convert a specified ip bytes to humen-readable string.
// returns: 0 for success or -1 for failed.
XDB_PUBLIC(int) xdb_ip_to_string(const bytes_ip_t *, int, char *, size_t);

// ipv4 bytes to string
XDB_PUBLIC(int) xdb_v4_ip_to_string(const bytes_ip_t *, char *, size_t);

// ipv6 bytes to string
XDB_PUBLIC(int) xdb_v6_ip_to_string(const bytes_ip_t *, char *, size_t);

// compare the specified ip bytes with another ip bytes in the specified buff from offset.
// ip args must be the return value from #xdb_parse_ip.
// returns: -1 if ip1 < ip2, 1 if ip1 > ip2 or 0
XDB_PUBLIC(int) xdb_ip_sub_compare(const bytes_ip_t *, size_t, const char *, int);

// --- END xdb utils

// --- ip version
#define XDB_IPv4 (xdb_version_ipv4())
#define XDB_IPv6 (xdb_version_ipv6())
typedef int (* ip_compare_fn_t) (const bytes_ip_t *, size_t, const char *, int);
struct xdb_ip_version_entry {
    int id;                 // version id
    char *name;             // version name
    int bytes;              // ip bytes number
    int segment_index_size; // segment index size in bytes

    // function to compare two ips
    ip_compare_fn_t ip_compare;
};
typedef struct xdb_ip_version_entry xdb_ip_version_t;

XDB_PUBLIC(xdb_ip_version_t *) xdb_version_ipv4();
XDB_PUBLIC(xdb_ip_version_t *) xdb_version_ipv6();

// --- END ip version


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
    // ip version
    xdb_ip_version_t *version;

    // xdb file handle
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
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_ip_version_t *, xdb_searcher_t *, const char *);

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_ip_version_t *, xdb_searcher_t *, const char *, const xdb_vector_index_t *);

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_ip_version_t *, xdb_searcher_t *, const xdb_content_t *);

XDB_PUBLIC(void) xdb_close(void *);

// xdb searcher search api define
XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *, const char *, char *, size_t);

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *, const bytes_ip_t *, int, char *, size_t);

XDB_PUBLIC(xdb_ip_version_t *) xdb_get_ip_version(xdb_searcher_t *);

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *);

// --- END xdb searcher api

#endif // C_IP2REGION_XDB_H
