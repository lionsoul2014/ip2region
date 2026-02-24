:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region python query client

# Version Compatibility

This implementation is compatible with Python `>=` **`3.7`**

# Usage

### Install `py-ip2region`

```bash
pip3 install py-ip2region
```

### About Query API

The prototype of the Query API is:

```python
# Query via string IP or binary IP (bytes type) parsed by util.parse_ip
search(ip: str | bytes)
```

An exception will be thrown if the query fails. If successful, the `region` information in string format will be returned. If the specified IP cannot be found, an empty string `""` will be returned.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. Use it as follows:

```python
import ip2region.util as util

# For IPv4: Set xdb path to the v4 xdb file, and specify the IP version as util.IPv4
db_path = "../../data/ip2region_v4.xdb"  # Or your ipv4 xdb path
version = util.IPv4

# For IPv6: Set xdb path to the v6 xdb file, and specify the IP version as util.IPv6
db_path = "../../data/ip2region_v6.xdb"  # Or your ipv6 xdb path
version = util.IPv6

# The IP version of the xdb specified by db_path must match the version specified by version, otherwise an error will occur during query execution
# Note: The following demonstrations directly use the db_path and version variables
```

### File Verification

It is recommended that you proactively verify the applicability of the xdb file. Future new features may cause the current Searcher version to be incompatible with the xdb file you are using; verification can prevent unpredictable errors during runtime. You do not need to verify every time. For example, perform verification when the service starts or manually call the command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```python
import ip2region.util as util

try:
    util.verify_from_file(db_path)
except Exception e:
    # Applicability verification failed!!!
    # The current query client implementation is not applicable for the xdb file specified by db_path.
    # You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation suitable for db_path.
    print(f"binding is not applicable for xdb file '{db_path}': {str(e)}")
    return

# Verification passed, the current Searcher can be safely used for query operations on the xdb pointed to by db_path
```

### File-Only Query

```python
import ip2region.util as util
import ip2region.searcher as xdb

# 1. Use the version and db_path mentioned above to create a file-only query object
try:
    searcher = xdb.new_with_file_only(version, db_path)
except Exception as e:
    print(f"failed to new_with_file_only: {str(e)}")
    return


# 2. Query, it supports both IPv4 and IPv6 addresses
ip = "1.2.3.4"
# ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"  // IPv6
try:
    region = searcher.search(ip)
    print(f"search({ip}): {{region: {region}, io_count: {searcher.get_io_count()}}}")
except Exception as e:
    print(f"failed to search: {str(e)}")

# 3. Close resources
searcher.close()

# Note: Each thread needs to create an independent Searcher object
```

### Caching `VectorIndex`

We can pre-load the `VectorIndex` data from the `xdb` file and cache it globally. Using the global VectorIndex cache every time a Searcher object is created can reduce a fixed IO operation, thereby accelerating queries and reducing IO pressure.

```python
import ip2region.util as util
import ip2region.searcher as xdb

# 1. Pre-load VectorIndex cache from db_path and use this data as a global variable for subsequent repeated use.
try:
    v_index = util.load_vector_index_from_file(db_path)
except Exception as e:
    print(f"failed to load vector index from {db_path}: {str(e)}")
    return

# 2. Use the global v_index to create a query object with VectorIndex cache.
try:
    searcher = xdb.new_with_vector_index(version, db_path, v_index)
except Exception as e:
    print(f"failed to new_with_vector_index: {str(e)}")
    return


# 3. Query; the interface is the same for both IPv4 and IPv6 addresses
ip = "1.2.3.4"
# ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"  // IPv6
try:
    region = searcher.search(ip)
    print(f"search({ip}): {{region: {region}, io_count: {searcher.get_io_count()}}}")
except Exception as e:
    print(f"failed to search: {str(e)}");

# 4. Close resources
searcher.close()

# Note: Each thread needs to create an independent Searcher object, but they all share the global read-only v_index cache.
```

### Caching the Entire `xdb` File

We can also pre-load the data of the entire xdb file into memory and create a query object based on this data to achieve fully memory-based queries, similar to the previous memory search.

```python
import ip2region.util as util
import ip2region.searcher as xdb

# 1. Load the entire xdb into memory from db_path.
try:
    c_buffer = util.load_content_from_file(db_path)
except Exception as e:
    print(f"failed to load content from {db_path}: {str(e)}")
    return

# 2. Use the c_buffer mentioned above to create a fully memory-based query object.
try:
    searcher = xdb.new_with_buffer(version, c_buffer)
except Exception e:
    print(f"failed to new_with_buffer: {str(e)}")
    return

# 3. Query; the interface is the same for both IPv4 and IPv6 addresses
ip = "1.2.3.4"
# ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"  # IPv6
try:
    region = searcher.search(ip)
    print(f"search({ip}): {{region: {region}, io_count: 0}}")
except Exception as e:
    print(f"failed to search: {str(e)}")
        
# 4. Close resources - This searcher object can be safely used for concurrency; close the searcher only when the entire service is going to shut down
# searcher.close()

# Note: For concurrent use, query objects created with the entire xdb data cache can be safely used for concurrency, meaning you can make this searcher object a global object for cross-thread access.
```

# Query Test

You can test queries via the `python3 search_test.py` command:

```bash
➜  python git:(fr_python_ipv6) ✗ python3 search_test.py 
usage: python search_test.py [command option]

ip2region search test script

options:
  -h, --help            show this help message and exit
  --db DB               ip2region binary xdb file path
  --cache-policy CACHE_POLICY
                        cache policy: file/vectorIndex/content, default: vectorIndex
```

For example: Use the default data/ip2region_v4.xdb file for IPv4 query testing:

```bash
➜  python git:(fr_python_ipv6) ✗ python3 search_test.py --db=../../data/ip2region_v4.xdb                       
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, ioCount: 5, took: 188 μs}
ip2region>> 113.118.113.77
{region: 中国|广东省|深圳市|电信|CN, ioCount: 2, took: 143 μs}
```

For example: Use the default data/ip2region_v6.xdb file for IPv6 query testing:

```bash
➜  python git:(fr_python_ipv6) ✗ python3 search_test.py --db=../../data/ip2region_v6.xdb 
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: , ioCount: 1, took: 166 μs}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, ioCount: 8, took: 197 μs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, ioCount: 13, took: 240 μs}
```

Input an IP to perform a query test. You can also set `cache-policy` to file/vectorIndex/content respectively to test the effects of the three different cache implementations.

# bench Test

Bench testing can be performed via the `python3 bench_test.py` command, which ensures the `xdb` file is error-free and allows for performance evaluation:

```bash
➜  python git:(fr_python_ipv6) ✗ python3 bench_test.py                                                                                         
usage: python bench_test.py [command option]

ip2region bench test script

options:
  -h, --help            show this help message and exit
  --db DB               ip2region binary xdb file path
  --src SRC             source ip text file path
  --cache-policy CACHE_POLICY
                        cache policy: file/vectorIndex/content, default: vectorIndex
```

For example: Perform IPv4 bench testing via the default data/ip2region_v4.xdb and data/ipv4_source.txt files:

```bash
python3 bench_test.py --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

For example: Perform IPv6 bench testing via the default data/ip2region_v6.xdb and data/ipv6_source.txt files:

```bash
python3 bench_test.py --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

You can test the effects of the three different cache implementations by setting `cache-policy` to file/vectorIndex/content respectively.
@Note: Ensure the src file used for bench is the same source file used to generate the corresponding xdb file.
