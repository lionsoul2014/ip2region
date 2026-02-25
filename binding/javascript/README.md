:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region JavaScript Query Client

# Usage

### Install `ip2region.js`

```bash
npm install ip2region.js --save 
```

### About Query API

The prototype of the Query API is:

```javascript
// Query via a string IP or a binary IP (Buffer type) parsed by parseIP
search(ip: string | Buffer): string;
```

If an error occurs during the query, an exception will be thrown. If the query is successful, the `region` information string will be returned. If the specified IP cannot be found, an empty string `""` will be returned.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. The usage is as follows:

```javascript
import {IPv4, IPv6} from 'ip2region.js';

// For IPv4: Set xdb path to the v4 xdb file, and set IP version to Version.IPv4
let dbPath  = "../../data/ip2region_v4.xdb";  // or your ipv4 xdb path
let version = IPv4;

// For IPv6: Set xdb path to the v6 xdb file, and set IP version to Version.IPv6
let dbPath  = "../../data/ip2region_v6.xdb";  // or your ipv6 xdb path
let version = IPv6;

// The IP version of the xdb specified by dbPath must match the version specified; otherwise, an error will occur during execution.
// Note: The following demonstrations directly use the dbPath and version variables.
```

### File Verification

It is recommended that you proactively verify the applicability of the xdb file. Some new features in the future may cause the current Searcher version to be incompatible with the xdb file you are using. Verification helps avoid unpredictable errors during runtime. You do not need to verify every time; for example, verify once when the service starts or manually call the command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```javascript
import {verifyFromFile} from 'ip2region.js';

try {
    verifyFromFile(dbPath);
} catch (e) {
    // Applicability verification failed!!!
    // The current query client implementation is not applicable for queries on the xdb file specified by dbPath.
    // You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation that fits dbPath.
    console.log(`binding is not applicable for xdb file '${dbPath}': ${e.message}`);
    return;
}

// Verification passed. The current Searcher can be safely used for query operations on the xdb pointed to by dbPath.
```

### File-Only Query

```javascript
import {newWithFileOnly} from 'ip2region.js';

// 1. Create a file-only query object using the version and dbPath mentioned above
let searcher;
try {
    searcher = newWithFileOnly(version, dbPath);
} catch(e) {
    console.log(`failed to newWithFileOnly: ${err.message}`);
    return;
}


// 2. Query; the interface is the same for both IPv4 and IPv6 addresses
let ip = "1.2.3.4";
// ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
try {
    let region = await searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
} catch(e) {
    console.log(`${err.message}`);
}

// 3. Close resources
searcher.close();

// Note: Each thread needs to create an independent Searcher object separately
```

### Caching `VectorIndex`

We can pre-load the `VectorIndex` data from the `xdb` file and cache it globally. Using a global VectorIndex cache every time a Searcher object is created can reduce one fixed IO operation, thereby accelerating queries and reducing IO pressure.

```javascript
import {loadVectorIndexFromFile, newWithVectorIndex} from 'ip2region.js';

// 1. Pre-load VectorIndex cache from dbPath and keep this data as a global variable for subsequent repeated use.
let vIndex;
try {
    vIndex = loadVectorIndexFromFile(dbPath);
} catch (e) {
    console.log(`failed to load vector index from ${dbPath}: ${e.message}`);
    return;
}

// 2. Create a query object with VectorIndex cache using the global vIndex.
let searcher;
try {
    searcher = newWithVectorIndex(version, dbPath, vIndex);
} catch(e) {
    console.log(`failed to newWithVectorIndex: ${err.message}`);
    return;
}


// 3. Query; the interface is the same for both IPv4 and IPv6 addresses
let ip = "1.2.3.4";
// ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
try {
    let region = await searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
} catch(e) {
    console.log(`${err.message}`);
}

// 4. Close resources
searcher.close();

// Note: Each thread needs to create a separate independent Searcher object, but they all share the global read-only vIndex cache.
```

### Caching the entire `xdb` file

We can also pre-load the data of the entire xdb file into memory and then create a query object based on this data to achieve a completely memory-based query, similar to the previous memory search.

```javascript
import {loadContentFromFile, newWithBuffer} from 'ip2region.js';

// 1. Load the entire xdb from dbPath into memory.
let cBuffer;
try {
    cBuffer = loadContentFromFile(dbPath);
} catch (e) {
    console.log(`failed to load content from ${dbPath}: ${e.message}`);
    return;
}

// 2. Use the cBuff above to create a completely memory-based query object.
let searcher;
try {
    searcher = newWithBuffer(version, cBuffer);
} catch(e) {
    console.log(`failed to newWithBuffer: ${err.message}`);
    return;
}

// 3. Query; the interface is the same for both IPv4 and IPv6 addresses
let ip = "1.2.3.4";
// ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";  // IPv6
try {
    let region = await searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}`);
} catch(e) {
    console.log(`${err.message}`);
}
        
// 4. Close resources - This searcher object is safe for concurrent use; close the searcher only when the entire service shuts down.
// searcher.close();

// Note: For concurrent use, the query object created with the entire xdb data cache can be safely used concurrently, meaning you can make this searcher object a global object for cross-thread access.
```

# Query Test

You can test queries using the `node tests/search.app.js` command:

```bash
➜  javascript git:(fr_javascript_ipv6) node tests/search.app.js                                                                 
usage: Usage node tests/search.app.js [command options]

ip2region search script

optional arguments:
  -h, --help            show this help message and exit
  --db DB               ip2region binary xdb file path
  --cache-policy CACHE_POLICY
                        cache policy: file/vectorIndex/content, default: vectorIndex
```

Example: Using the default data/ip2region_v4.xdb file for IPv4 query testing:

```bash
➜  javascript git:(fr_javascript_ipv6) ✗ node tests/search.app.js --db=../../data/ip2region_v4.xdb                                
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, ioCount: 5, took: 657.035 μs}
ip2region>> 113.118.113.77
{region: 中国|广东省|深圳市|电信|CN, ioCount: 2, took: 169.927 μs}
```

Example: Using the default data/ip2region_v6.xdb file for IPv6 query testing:

```bash
➜  javascript git:(fr_javascript_ipv6) ✗ node tests/search.app.js --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, ioCount: 8, took: 98.953 μs}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, ioCount: 13, took: 287.703 μs}
```

Enter an IP to perform a query test. You can also set `cache-policy` to file/vectorIndex/content respectively to test the effects of the three different cache implementations.

# bench Test

You can perform a bench test via the `node tests/bench.app.js` command, which ensures the `xdb` file is error-free and evaluates query performance:

```bash
➜  javascript git:(fr_javascript_ipv6) ✗ node tests/bench.app.js 
usage: Usage node tests/bench.app.js [command options]

ip2region bench script

optional arguments:
  -h, --help            show this help message and exit
  --db DB               ip2region binary xdb file path
  --src SRC             source ip text file path
  --cache-policy CACHE_POLICY
                        cache policy: file/vectorIndex/content, default: vectorIndex
```

Example: Perform an IPv4 bench test using the default data/ip2region_v4.xdb and data/ipv4_source.txt files:

```bash
node tests/bench.app.js --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

Example: Perform an IPv6 bench test using the default data/ip2region_v6.xdb and data/ipv6_source.txt files:

```bash
node tests/bench.app.js --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

You can test the effects of the three different cache implementations by setting `cache-policy` to file/vectorIndex/content.
@Note: Ensure the src file used for bench is the same source file used to generate the corresponding xdb file.

### Third-party Library Support:

1. [ts-ip2region2](https://github.com/Steven-Qiang/ts-ip2region2) - Based on the official C extension, providing higher execution efficiency than pure JS.
