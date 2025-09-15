# ip2region xdb php 查询客户端实现

# 使用方式

第三方 composer 地址 (请自行确认仓库对 IPv6 的支持): 
1. [https://github.com/zoujingli/ip2region](https://github.com/zoujingli/ip2region)
2. [https://github.com/chinayin/ip2region-core-php](https://github.com/chinayin/ip2region-core-php)


### 关于 IPv4 和 IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```php
use \ip2region\xdb\{IPv4, IPv6};

// 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 IPv4
$dbFile  = "../../data/ip2region_v4.xdb";  // 或者你的 ipv4 xdb 的路径
$version = IPv4::default();

// 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 IPv6
$dbFile  = "../../data/ip2region_v6.xdb";  // 或者你的 ipv6 xdb 路径
$version = IPv6::default();

// dbPath 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
// 备注：以下演示直接使用 $dbFile 和 $version 变量
```

### 完全基于文件的查询
```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;

// 1, 使用上述的 $version 和 $dbFile 创建 Searcher 对象
try {
    $searcher = Searcher::newWithFileOnly($version, $dbFile);
} catch (Exception $e) {
    printf("failed to create searcher with '%s': %s\n", $dbFile, $e->getMessage());
    return;
}

// 2, 查询，IPv4 或者 IPv6 的地址都支持
try {
    $ip = '1.2.3.4';
    // $ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 3，关闭资源
$searcher->close();

// 备注：并发使用，每个线程或者协程需要创建一个独立的 searcher 对象。
```

### 缓存 `VectorIndex` 索引

如果你的 php 母环境支持，可以预先加载 vectorIndex 缓存，然后做成全局变量，每次创建 Searcher 的时候使用全局的 vectorIndex，可以减少一次固定的 IO 操作从而加速查询，减少 io 压力。 
```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;


// 1、从 $dbFile 加载 VectorIndex 缓存，把下述的 vIndex 变量缓存到内存里面。
$vIndex = Util::loadVectorIndexFromFile($dbFile);
if ($vIndex === null) {
    printf("failed to load vector index from '%s'\n", $dbFile);
    return;
}

// 2、使用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
try {
    $searcher = Searcher::newWithVectorIndex($version, $dbFile, $vIndex);
} catch (Exception $e) {
    printf("failed to create vectorIndex searcher with '%s': %s\n", $dbFile, $e->getMessage());
    return;
}

// 3、查询，IPv4 或者 IPv6 都支持
try {
    $ip = '1.2.3.4';
    // $ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 4, 关闭资源
$searcher->close();

// 备注：并发使用，每个线程或者协程需要创建一个独立的 searcher 对象，但是都共享统一的只读全局 vectorIndex。。
```

### 缓存整个 `xdb` 数据

如果你的 PHP 母环境支持，可以预先加载整个 `xdb` 的数据到内存，这样可以实现完全基于内存的查询，类似之前的 memory search 查询。
```php
// require or autoload the xdb\Searcher
require 'xdb\Searcher.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;

// 1、从 $dbFile 加载整个 xdb 到内存。
$cBuff = Util::loadContentFromFile($dbFile);
if ($cBuff === null) {
    printf("failed to load content buffer from '%s'\n", $dbFile);
    return;
}

// 2、使用全局的 cBuff 创建带完全基于内存的查询对象。
try {
    $searcher = Searcher::newWithBuffer($version, $cBuff);
} catch (Exception $e) {
    printf("failed to create buffer cached searcher: %s\n", $dbFile, $e->getMessage());
    return;
}

// 3、查询，IPv4 或者 IPv6 都支持
try {
    $ip = '1.2.3.4';
    // $ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6
    $sTime  = Util::now();
    $region = $searcher->search($ip);
    $costMs = Util::now() - $sTime;
    printf("{region: %s, took: %.5f ms}\n", $region, $costMs);
} catch (Exception $e) {
    printf("failed to search(%s): %s", $ip, $e->getMessage());
}

// 4，关闭资源
// 该 searcher 对象可以安全的用于并发，等整个服务都关闭的时候再关闭 searcher
// $searcher->close();

// 备注：并发使用，用整个 xdb 缓存创建的 searcher 对象可以安全用于并发。
```

# 查询测试

通过 `search_test.php` 脚本来进行查询测试：
```bash
➜  php git:(fr_php_ipv6) ✗ php search_test.php 
php search_test.php [command options]
options: 
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region_v4.xdb 进行 IPv4 的查询测试：
```bash
➜  php git:(fr_php_ipv6) ✗ php search_test.php --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb file: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动, ioCount: 3, took: 0.02783 ms}
```

例如：使用默认的 data/ip2region_v6.xdb 进行 IPv6 的查询测试：
```bash
➜  php git:(fr_php_ipv6) ✗ php search_test.php --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb file: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: |||, ioCount: 2, took: 0.06982 ms}
ip2region>> 2604:bc80:8001:11a4:ffff:ffff:ffff:ffff
{region: 中国|广东省|深圳市|数据中心, ioCount: 13, took: 0.19409 ms}
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。

# bench 测试

通过 `bench_test.php` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  php git:(fr_php_ipv6) ✗ php bench_test.php
php bench_test.php [command options]
options: 
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 文件进行 IPv4 的 bench 测试：
```bash
php bench_test.php --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 文件进行 IPv6 的 bench 测试：
```bash
php bench_test.php --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。
