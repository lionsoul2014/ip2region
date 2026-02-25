:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region C++ query client

## 0. File Description

```
Makefile --------- Build

src ------------------ Source directory
src/base.* ----------- Constants and utility functions
src/ip.* ------------- IP processing implementation
src/header.* --------- xdb header parsing implementation
src/search.* --------- xdb search implementation
src/bench.* ---------- Search benchmarking implementation
src/make.* ----------- xdb file generation implementation
src/edit.* ----------- Raw data editing implementation

test ---------------- Test directory
test/header.cc ------ Test header
test/search.cc ------ Test search
test/bench.cc ------- Benchmarking
test/make.cc -------- Generate xdb file
test/edit_v4.cc ----- Test raw data editing (ipv4)
test/edit_v6.cc ----- Test raw data editing (ipv6)


bin --------------- Executable directory (generated via make)
bin/header -------- Test header
bin/search -------- Test search
bin/bench --------- Benchmarking
bin/make ---------- Generate xdb file
bin/edit_v4 ------- Test raw data editing (ipv4)
bin/edit_v6 ------- Test raw data editing (ipv6)

readme.md --------- readme

```

## 1. Compilation

```
$ make
```

## 2. Search

### 2.1 Example

```cpp
#include "src/search.h"

// IP Version: xdb::ipv4 xdb::ipv6
//    Policy: xdb::policy_file xdb::policy_vector xdb::policy_content
//               No cache           Partial cache        Full cache
int main() {
    std::string xdb_name = "../../data/ip2region_v6.xdb";
    int         version  = xdb::ipv6;
    int         policy   = xdb::policy_content;
    std::string ip       = "2001:200:124::";

    xdb::search_t s(xdb_name, version, policy);
    std::cout << s.search(ip) << std::endl;
    return 0;
}

// $ g++ src/*.cc 1.cc --- Compile
// $ ./a.out ------------- Test
// Japan|Tokyo|Asagaya-minami|WIDE Project|JP
```

### 2.2 Test xdb Header

```
$ ./bin/header
Test IPv4
Version: 3
Cache Policy: 1
File Generation Time: 2025-09-06 02:24:16
Index Start Address: 955933
Index End Address: 11042415
IP Version: 4
Pointer Bytes: 4

Test IPv6
Version: 3
Cache Policy: 1
File Generation Time: 2025-10-17 04:41:04
Index Start Address: 3094259
Index End Address: 36258303
IP Version: 6
Pointer Bytes: 4
```

### 2.3 Test Search

```
$ ./bin/search
Test IPv4   No cache: Success
Test IPv4 Partial cache: Success
Test IPv4 Full cache: Success
Test IPv6   No cache: Success
Test IPv6 Partial cache: Success
Test IPv6 Full cache: Success
```

## 3. Benchmarking and Correctness Verification

```
./bin/bench
Test IPv4,   No cache, total: 3910284, took:    27.60s, cost:   6.59μs/op, io count: 28227147
Test IPv4, Partial cache, total: 3910284, took:    21.85s, cost:   5.15μs/op, io count: 24316863
Test IPv4, Full cache, total: 3910284, took:     2.26s, cost:   0.25μs/op, io count: 0
Test IPv6,   No cache, total: 4792520, took:   100.40s, cost:  20.22μs/op, io count: 80758866
Test IPv6, Partial cache, total: 4792520, took:    93.06s, cost:  18.71μs/op, io count: 75966346
Test IPv6, Full cache, total: 4792520, took:     6.24s, cost:   0.81μs/op, io count: 0
```

## 4. Generate xdb File

### 4.1 Generate xdb File

```
$ ./bin/make
Generate ipv4 xdb file, took: 0.57s
Generate ipv6 xdb file, took: 1.24s
```

## 5. Raw Data Editing

### 5.1. Instructions for Use

* New IP attribution files can contain empty lines
* New IP attribution files can be out of order; the program will automatically sort them
* New IP attribution files can overlap; as long as there is no ambiguity, the program will automatically merge them
* The final result will automatically merge adjacent lines with the same attribution
* For the following tests, the original file uses the data file provided in the repository, and the new file uses 1.txt in the current directory
