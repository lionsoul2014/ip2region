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
#elif (defined(linux) || defined(_UNIX) || defined(__APPLE__) || defined(unix) || defined(__unix) || defined(__unix__) || defined(__linux__) || defined(linux) || defined(__linux))
#   define XDB_PUBLIC(type)    extern type
#   define XDB_PRIVATE(type)   static inline type
#   define XDB_LINUX
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#   define XDB_PUBLIC(type) type
#   define XDB_PRIVATE(type) static type
#endif

#define xdb_calloc( _blocks, _bytes )  calloc( _blocks, _bytes )
#define xdb_malloc( _bytes )           malloc( _bytes )
#define xdb_free( _ptr )               free( _ptr )

// public constants define
#define xdb_structure_20 2
#define xdb_structure_30 3
#define xdb_header_info_length 256
#define xdb_vector_index_rows  256
#define xdb_vector_index_cols  256
#define xdb_vector_index_size  8
#define xdb_v4_index_size 14    // 4 + 4 + 2 + 4
#define xdb_v6_index_size 38    // 16 + 16 + 2 + 4

// --- ip version info
#define xdb_ipv4_id 4
#define xdb_ipv6_id 6
#define xdb_ipv4_bytes 4
#define xdb_ipv6_bytes 16
// cache of vector_index_row × vector_index_rows × vector_index_size
#define xdb_vector_index_length 524288

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

// --- xdb verify

// Verify if the current Searcher could be used to search the specified xdb file.
// Why do we need this check ?
// The future features of the xdb impl may cause the current searcher not able to work properly.
//
// @Note: You Just need to check this ONCE when the service starts
// Or use another process (eg, A command) to check once Just to confirm the suitability.
XDB_PUBLIC(int) xdb_verify(FILE *);

XDB_PUBLIC(int) xdb_verify_from_header(FILE *handle, xdb_header_t *);

XDB_PUBLIC(int) xdb_verify_from_file(const char *);

// --- End xdb buffer


// types type define
typedef char string_ip_t;
typedef unsigned char bytes_ip_t;

// --- ip version
#define XDB_IPv4 (xdb_version_v4())
#define XDB_IPv6 (xdb_version_v6())
typedef int (* ip_compare_fn_t) (const bytes_ip_t *, int, const char *, int);
struct xdb_ip_version_entry {
    int id;                 // version id
    char *name;             // version name
    int bytes;              // ip bytes number
    int segment_index_size; // segment index size in bytes

    // function to compare two ips
    ip_compare_fn_t ip_compare;
};
typedef struct xdb_ip_version_entry xdb_version_t;

XDB_PUBLIC(xdb_version_t *) xdb_version_v4();
XDB_PUBLIC(xdb_version_t *) xdb_version_v6();

XDB_PUBLIC(int) xdb_version_is_v4(const xdb_version_t *);
XDB_PUBLIC(int) xdb_version_is_v6(const xdb_version_t *);

XDB_PUBLIC(xdb_version_t *) xdb_version_from_name(char *);
XDB_PUBLIC(xdb_version_t *) xdb_version_from_header(xdb_header_t *);

// --- END ip version

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


// parse the specified IP address to byte array.
// returns: xdb_version_t for valid ipv4 / ipv6, or NULL for failed
XDB_PUBLIC(xdb_version_t *) xdb_parse_ip(const string_ip_t *, bytes_ip_t *, size_t);

// parse the specified IPv4 address to byte array
// returns: xdb_version_t for valid ipv4, or NULL for failed
XDB_PUBLIC(xdb_version_t *) xdb_parse_v4_ip(const string_ip_t *, bytes_ip_t *, size_t);

// parse the specified IPv6 address to byte array
// returns: xdb_version_t for valid ipv6, or NULL for failed
XDB_PUBLIC(xdb_version_t *) xdb_parse_v6_ip(const string_ip_t *, bytes_ip_t *, size_t);

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
XDB_PUBLIC(int) xdb_ip_sub_compare(const bytes_ip_t *, int, const char *, int);

// --- END xdb utils


// --- xdb searcher api

// xdb region info structure
#define xdb_region_buffer_wrapper 1
#define xdb_region_buffer_auto    2
struct xdb_region_buffer_entry {
    int type;           // buffer type
    char *value;        // region value
    size_t length;      // buffer length
};
typedef struct xdb_region_buffer_entry xdb_region_buffer_t;

// wrapper the region from a local stack buffer.
// returns: 0 for succeed or failed
XDB_PUBLIC(int) xdb_region_buffer_init(xdb_region_buffer_t *, char *, size_t);

// do the buffer alloc.
// returns: 0 for ok or failed
XDB_PUBLIC(int) xdb_region_buffer_alloc(xdb_region_buffer_t *, int);

// empty alloc - empty string
// returns:  0 - always
XDB_PUBLIC(int) xdb_region_buffer_empty(xdb_region_buffer_t *);

XDB_PUBLIC(void) xdb_region_buffer_free(xdb_region_buffer_t *);

// xdb searcher structure
struct xdb_searcher_entry {
    // ip version
    xdb_version_t *version;

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
XDB_PUBLIC(int) xdb_new_with_file_only(xdb_version_t *, xdb_searcher_t *, const char *);

XDB_PUBLIC(int) xdb_new_with_vector_index(xdb_version_t *, xdb_searcher_t *, const char *, const xdb_vector_index_t *);

XDB_PUBLIC(int) xdb_new_with_buffer(xdb_version_t *, xdb_searcher_t *, const xdb_content_t *);

XDB_PUBLIC(void) xdb_close(void *);

// xdb searcher search api define
XDB_PUBLIC(int) xdb_search_by_string(xdb_searcher_t *, const string_ip_t *, xdb_region_buffer_t *);

XDB_PUBLIC(int) xdb_search(xdb_searcher_t *, const bytes_ip_t *, int, xdb_region_buffer_t *);

XDB_PUBLIC(xdb_version_t *) xdb_get_version(xdb_searcher_t *);

XDB_PUBLIC(int) xdb_get_io_count(xdb_searcher_t *);

// --- END xdb searcher api

#endif // C_IP2REGION_XDB_H
