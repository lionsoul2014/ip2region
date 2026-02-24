[中文简体](README_zh.md) | [English](README.md)

# ip2region c Query Client

# Usage

### About Query API

The prototype of the Query API is as follows:

```c
// Query via string IP
int xdb_search_by_string(xdb_searcher_t *, const string_ip_t *, xdb_region_buffer_t *);
// Query via binary IP returned by xdb_parse_ip
int xdb_search(xdb_searcher_t *, const bytes_ip_t *, int, xdb_region_buffer_t *);
```

If the query fails, a non-`0` error code will be returned. If the query is successful, the `region` information string can be obtained from `xdb_region_buffer_t`. If the input IP cannot be found, `xdb_region_buffer_t` will receive an empty string `""`.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. The usage is as follows:

```c
#include "xdb_api.h";

// For IPv4: Set xdb path to the v4 xdb file, specify IP version as IPv4
const char *db_path  = "../../data/ip2region_v4.xdb";  // or your ipv4 xdb path
xdb_version_t *version = XDB_IPv4;

// For IPv6: Set xdb path to the v6 xdb file, specify IP version as IPv6
const char *db_path  = "../../data/ip2region_v6.xdb";  // or your ipv6 xdb path
xdb_version_t *version = XDB_IPv6;

// The IP version of the xdb specified by db_path must be consistent with the version, otherwise an error will occur during query execution
// Note: The following demonstration directly uses db_path and version variables
```

### XDB File Verification

It is recommended that you proactively verify the applicability of the xdb file, as some future new features may cause the current Searcher version to be incompatible with the xdb file you are using. Verification can avoid unpredictable errors during runtime. You do not need to verify every time; for example, verify when the service starts or manually call a command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```c
#include "xdb_api.h";

int errcode = xdb_verify(db_path);
if ($err != 0) {
    // Applicability verification failed!!!
    // The current query client implementation is not suitable for querying the xdb file specified by db_path.
    // You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation compatible with db_path.
    printf("failed to verify xdb file `%s`, errcode: %d\n", db_path, errcode);
    return;
}

// Verification passed, the current Searcher can be safely used for query operations on the xdb pointed to by dbPath
```

### File-Based Query

```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_searcher_t searcher;
    char region_buffer[512] = {'\0'};
    xdb_region_buffer_t region;

    // Initialize region_buffer_t using region_buffer from stack space
    int err = xdb_region_buffer_init(&region, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 1;
    }

    // Initialize winsock when the service starts; no need to call repeatedly, only needed on Windows systems
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1. Initialize xdb query object from db_path.
    // @Note: Use the db_path and version described above to create the searcher
    err = xdb_new_with_file_only(version, &searcher, db_path);
    if (err != 0) {
        printf("failed to create xdb searcher from `%s` with errno=%d\n", db_path, err);
        return 1;
    }

    // 2. Call search API to query, both IPv4 and IPv6 are supported.
    const char *ip_string = "1.2.3.4";
    // ip_string = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // Clean up memory resources for region info; must be called after every search
    xdb_region_buffer_free(&region);

    // Note: For concurrent use, each thread needs to define and initialize its own searcher query object independently.

    // 3. Close xdb searcher
    xdb_close(&searcher);
    xdb_clean_winsock();    // Call on Windows
    return 0;
}
```

### Caching `VectorIndex`

We can pre-load VectorIndex data from the xdb file and cache it globally. Using the global VectorIndex cache every time a Searcher object is created can reduce a fixed IO operation, thereby accelerating queries and reducing IO pressure.

```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_vector_index_t *v_index;
    xdb_searcher_t searcher;
    xdb_region_buffer_t region;

    // Initialize region_buffer with NULL to let it manage memory allocation automatically
    int err = xdb_region_buffer_init(&region, NULL, 0);
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 0;
    }

    // Initialize winsock when the service starts; no need to call repeatedly, only needed on Windows systems
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1. Load VectorIndex from the db_path described above.
    // Obtain v_index to create a global cache for subsequent repeated use.
    // Note: v_index does not need to be loaded every time; it is recommended to load it once at service startup as a global resource.
    v_index = xdb_load_vector_index_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load vector index from `%s`\n", db_path);
        return 1;
    }

    // 2. Use the global VectorIndex variable to create an xdb searcher with VectorIndex cache.
    // @Note: Use the db_path and version described above to create the searcher
    err = xdb_new_with_vector_index(version, &searcher, db_path, v_index);
    if (err != 0) {
        printf("failed to create vector index cached searcher with errcode=%d\n", err);
        return 2;
    }


    // 3. Call search API to query, both IPv4 and IPv6 are supported
    const char *ip_string = "1.2.3.4";
    // ip_string = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // Clean up memory resources for region info; must be called after every search
    xdb_region_buffer_free(&region);


    // Note: For concurrent use, each thread needs to define and initialize its own searcher query object independently.

    // 4. Close xdb searcher; if the service is being shut down, the memory for v_index also needs to be freed.
    xdb_close(&searcher);
    xdb_close_vector_index(v_index);
    xdb_clean_winsock();
    return 0;
}
```

### Caching the Entire `xdb` File

We can also pre-load the entire xdb file into memory and then create a query object based on this data to achieve fully memory-based queries, similar to the previous memory search.

```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_content_t *c_buffer;
    xdb_searcher_t searcher;
    xdb_region_buffer_t region;

    // Initialize region_buffer with NULL to let it manage memory allocation automatically
    int err = xdb_region_buffer_init(&region, NULL, 0);
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 0;
    }


    // Initialize winsock when the service starts; no need to call repeatedly, only needed on Windows systems
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1. Load the entire xdb data from the db_path described above.
    c_buffer = xdb_load_content_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load xdb content from `%s`\n", db_path);
        return 1;
    }

    // 2. Use the global c_buffer variable to create a fully memory-based xdb query object.
    // @Note: Use the version described above to create the searcher.
    err = xdb_new_with_buffer(version, &searcher, c_buffer);
    if (err != 0) {
        printf("failed to create content cached searcher with errcode=%d\n", err);
        return 2;
    }

    // 3. Call search API to query, both IPv4 and IPv6 are supported
    const char *ip_string = "1.2.3.4";
    // ip_string = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // Clean up memory resources for region info; must be called after every search
    xdb_region_buffer_free(&region);


    // Note: For concurrent use, xdb query objects created this way can be safely used for concurrency.
    // It is recommended to create them when the service starts and then use them safely in parallel until the service shuts down.

    // 4. Close xdb searcher; memory for c_buffer needs to be freed when shutting down the service.
    xdb_close(&searcher);
    xdb_close_content(c_buffer);
    xdb_clean_winsock();
    return 0;
}
```

### About Storage of Location Information

In older implementations, search-related functions relied on a specified `region_buffer` memory to store location information, which had significant limitations.
The new implementation provides an `xdb_region_buffer_t` object to manage these memory allocations. You can still specify a fixed `region_buffer` to create memory management for the region; this is suitable when the maximum length of your location information is known, which helps reduce memory fragmentation during runtime. If the length of the location information is uncertain or if your program is not suited for pre-allocating a block of memory, you can initialize `xdb_region_buffer_t` by specifying `NULL`. In this case, the object will automatically manage memory allocation, making it suitable for storing location information of any length, though this approach will certainly increase memory fragmentation over long-term operation.

```c
// 1. Create region_buffer by specifying a memory block
char buffer[512];
xdb_region_buffer_t region;
int err = xdb_region_buffer_init(&region, buffer, sizeof(buffer));
if (err != 0) {
    // Initialization failed
    printf("failed to init region buffer width errcode=%d", err);
    return;
}

// 2. Create region_buffer by specifying NULL to let it allocate memory as needed automatically
xdb_region_buffer_t region;
int err = xdb_region_buffer_init(&region, NULL, 0);
if (err != 0) {
    // Initialization failed
    printf("failed to init region buffer width errcode=%d", err);
    return;
}


// Note: After each query call, you must manually call the function to free memory.
// The search function will report an error if used with uncleaned region info.
xdb_region_buffer_free(&region);
```

# Compiling the Test Program

Compile and obtain the `xdb_searcher` executable as follows:

```bash
# cd to the c binding root directory
➜  c git:(master) ✗ make
gcc -std=c99 -Wall -O2 -I./ xdb_util.c xdb_searcher.c main.c -o xdb_searcher
gcc -std=c99 -Wall -O2 -I./ xdb_util.c test_util.c -o test_util
```

# Query Testing

Test queries against xdb via the `xdb_searcher search` command:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search
./xdb_searcher search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

Example: performing IPv4 query testing using the default data/ip2region_v4.xdb:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, io_count: 5, took: 39 μs}
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动|CN, io_count: 3, took: 13 μs}
```

Example: performing IPv6 query testing using the default data/ip2region_v6.xdb:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: , io_count: 1, took: 38 μs}
ip2region>> 2604:bc80:8001:11a4:ffff:ffff:ffff:ffff
{region: United States|Florida|Miami|velia.net Internetdienste GmbH|US, io_count: 14, took: 76 μs}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, io_count: 8, took: 42 μs}
```

Enter an IP to perform a query; enter `quit` to exit the test program. You can also set `cache-policy` to file/vectorIndex/content respectively to test the efficiency of the three different cache implementations.

# bench Testing

Perform bench testing via the `xdb_searcher bench` command. This ensures there are no errors in the query program and the `xdb` file, while also providing average query performance through a large number of queries:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench                                  
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

Example: performing IPv4 bench testing via the default data/ip2region_v4.xdb and data/ipv4_source.txt:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
Bench finished, {cache_policy: vectorIndex, total: 1367686, took: 7.640s, cost: 5 μs/op}
```

Example: performing IPv6 bench testing via the default data/ip2region_v6.xdb and data/ipv6_source.txt:

```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
Bench finished, {cache_policy: vectorIndex, total: 34159862, took: 857.750s, cost: 24 μs/op}
```

You can set the `cache-policy` parameter to test the efficiency of different cache mechanisms (file/vectorIndex/content). @Note: Please ensure that the `src` file used for benching is the same source file used to generate the corresponding `xdb` file.
