[中文简体](README_zh.md) | [English](README.md)

# ip2region PHP Query Client

# Usage

### About Query API

The prototype of the Query API is as follows:

```php
// Query via string IP
// @throw Exception
search($ip_string) string

// Query via binary IP returned by Util.parseIP
// @throw Exception
searchByBytes($ip_bytes) string
```

If the query fails, an exception will be thrown; if the query is successful, the `region` information string will be returned; if the IP being queried cannot be found, an empty string `""` will be returned.

### About IPv4 and IPv6

This xdb query client implementation supports both IPv4 and IPv6 queries. The usage is as follows:

```php
use \ip2region\xdb\{IPv4, IPv6};

// For IPv4: Set xdb path to the v4 xdb file, specify IP version as IPv4
$dbFile  = "../../data/ip2region_v4.xdb";  // or your ipv4 xdb path
$version = IPv4::default();

// For IPv6: Set xdb path to the v6 xdb file, specify IP version as IPv6
$dbFile  = "../../data/ip2region_v6.xdb";  // or your ipv6 xdb path
$version = IPv6::default();

// The IP version of the xdb specified by dbPath must be consistent with the version specified, otherwise an error will occur during query execution
// Note: The following demonstration directly uses $dbFile and $version variables
```

### XDB File Verification

It is recommended that you proactively verify the applicability of the xdb file, as some new features in the future may cause the current Searcher version to be incompatible with the xdb file you are using. Verification can avoid unpredictable errors during runtime. You do not need to verify every time; for example, verify when the service starts or manually call the command to confirm version matching. Do not run verification every time a Searcher is created, as this will affect query response speed, especially in high-concurrency scenarios.

```php
use \ip2region\xdb\Util;

$err = Util::verify($dbFile);
if ($err != null) {
    // Applicability verification failed!!!
    // The current query client implementation is not suitable for querying the xdb file specified by dbPath.
    // You should stop the service and use a suitable xdb file or upgrade to a Searcher implementation compatible with dbPath.
    printf("failed to verify xdb file `%s`: %s\n", $dbFile, $err);
    return;
}

// Verification passed, the current Searcher can be safely used for query operations on the xdb pointed to by dbPath
```

### File-Based Query

```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;

// 1. Create a Searcher object using the $version and $dbFile mentioned above
try {
    $searcher = Searcher::newWithFileOnly($version, $dbFile);
} catch (Exception $e) {
    printf("failed to create searcher with '%s': %s\n", $dbFile, $e->getMessage());
    return;
}

// 2. Query, both IPv4 or IPv6 addresses are supported
try {
    $ip = '1.2.3.4';
    // $ip = ""240e:3b7:3272:d8d0:db09:c067:8d59:539e; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 3. Close resources
$searcher->close();

// Note: For concurrent use, each thread or coroutine needs to create an independent searcher object.
```

### Caching `VectorIndex`

If your PHP environment supports it, you can pre-load the vectorIndex cache and make it a global variable. Using the global vectorIndex every time you create a Searcher can reduce a fixed IO operation, thereby accelerating queries and reducing IO pressure.

```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;


// 1. Load VectorIndex cache from $dbFile and cache the following vIndex variable in memory.
$vIndex = Util::loadVectorIndexFromFile($dbFile);
if ($vIndex === null) {
    printf("failed to load vector index from '%s'\n", $dbFile);
    return;
}

// 2. Use the global vIndex to create a query object with VectorIndex cache.
try {
    $searcher = Searcher::newWithVectorIndex($version, $dbFile, $vIndex);
} catch (Exception $e) {
    printf("failed to create vectorIndex searcher with '%s': %s\n", $dbFile, $e->getMessage());
    return;
}

// 3. Query, both IPv4 or IPv6 are supported
try {
    $ip = '1.2.3.4';
    // $ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 4. Close resources
$searcher->close();

// Note: For concurrent use, each thread or coroutine needs to create an independent searcher object, but they all share the same read-only global vectorIndex.
```

### Caching the Entire `xdb` File

If your PHP environment supports it, you can pre-load the entire `xdb` file into memory. This allows for fully memory-based queries, similar to the previous memory search.

```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;

// 1. Load the entire xdb from $dbFile into memory.
$cBuff = Util::loadContentFromFile($dbFile);
if ($cBuff === null) {
    printf("failed to load content buffer from '%s'\n", $dbFile);
    return;
}

// 2. Use the global cBuff to create a query object that is fully based on memory.
try {
    $searcher = Searcher::newWithBuffer($version, $cBuff);
} catch (Exception $e) {
    printf("failed to create buffer cached searcher: %s\n", $dbFile, $e->getMessage());
    return;
}

// 3. Query, both IPv4 or IPv6 are supported
try {
    $ip = '1.2.3.4';
    // $ip = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 4. Close resources
// This searcher object can be safely used for concurrency; close it when the entire service is shut down
// $searcher->close();

// Note: For concurrent use, the searcher object created with the entire xdb cache can be safely used for concurrency.
```

# Query Testing

Run query tests via the `search_test.php` script:

```bash
➜  php git:(fr_php_ipv6) ✗ php search_test.php 
php search_test.php [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: using the default data/ip2region_v4.xdb for IPv4 query testing:

```bash
➜  php git:(fr_php_ipv6) ✗ php search_test.php --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb file: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: Australia|Queensland|Brisbane|0|AU, ioCount: 5, took: 0.12695 ms}
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动|CN, ioCount: 3, took: 0.07397 ms}
```

For example: using the default data/ip2region_v6.xdb for IPv6 query testing:

```bash
➜  php git:(master) ✗ php ./search_test.php --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb file: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: , ioCount: 1, took: 0.08887 ms}
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|电信|CN, ioCount: 8, took: 0.10303 ms}
ip2region>> 2604:a840:3::a04d
{region: United States|California|San Jose|xTom|US, ioCount: 13, took: 0.04614 ms}
```

Enter an IP to perform a query test. You can also set `cache-policy` to file/vectorIndex/content respectively to test the efficiency of the three different cache implementations.

# Bench Testing

Run automatic bench testing via the `bench_test.php` script. On one hand, this ensures that there are no errors in the `xdb` file; on the other hand, it tests average query performance through a large number of queries:

```bash
➜  php git:(fr_php_ipv6) ✗ php bench_test.php
php bench_test.php [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

For example: perform an IPv4 bench test using the default data/ip2region_v4.xdb and data/ipv4_source.txt files:

```bash
php bench_test.php --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

For example: perform an IPv6 bench test using the default data/ip2region_v6.xdb and data/ipv6_source.txt files:

```bash
php bench_test.php --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

You can test the performance of the three different cache implementations (file/vectorIndex/content) by setting the `cache-policy` parameter.
@Note: Please ensure that the src file used for benching is the same source file used to generate the corresponding xdb file.

### Third-party Repository Support

1. Composer supported [zoujingli/ip2region](https://github.com/zoujingli/ip2region) - IPv6 supported.
