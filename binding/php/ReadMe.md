# ip2region xdb php 查询客户端实现

# 使用方式

第三方 composer 地址: 
1. [https://github.com/zoujingli/ip2region](https://github.com/zoujingli/ip2region)
2. [https://github.com/chinayin/ip2region-core-php](https://github.com/chinayin/ip2region-core-php)

### 完全基于文件的查询
```php
$dbFile = "ip2region.xdb file path";
try {
    $searcher = XdbSearcher::newWithFileOnly($dbFile);
} catch (Exception $e) {
    printf("failed to create searcher with '%s': %s\n", $dbFile, $e);
    return;
}

$ip = '1.2.3.4';
$sTime = XdbSearcher::now();
$region = $searcher->search($ip);
if ($region === null) {
    // something is wrong
    printf("failed search(%s)\n", $ip);
    return;
}

printf("{region: %s, took: %.5f ms}\n", $region, XdbSearcher::now() - $sTime);

// 备注：并发使用，每个线程或者协程需要创建一个独立的 searcher 对象。
```

### 缓存 `VectorIndex` 索引

如果你的 php 母环境支持，可以预先加载 vectorIndex 缓存，然后做成全局变量，每次创建 Searcher 的时候使用全局的 vectorIndex，可以减少一次固定的 IO 操作从而加速查询，减少 io 压力。 
```php
// 1、从 dbPath 加载 VectorIndex 缓存，把下述的 vIndex 变量缓存到内存里面。
$vIndex = XdbSearcher::loadVectorIndexFromFile($dbPath);
if ($vIndex === null) {
    printf("failed to load vector index from '%s'\n", $dbPath);
    return;
}

// 2、使用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
try {
    $searcher = XdbSearcher::newWithVectorIndex($dbFile, $vIndex);
} catch (Exception $e) {
    printf("failed to create vectorIndex cached searcher with '%s': %s\n", $dbFile, $e);
    return;
}

// 3、查询
$sTime = XdbSearcher::now();
$region = $searcher->search('1.2.3.4');
if ($region === null) {
    printf("failed search(1.2.3.4)\n");
    return;
}

printf("{region: %s, took: %.5f ms}\n", $region, XdbSearcher::now() - $sTime);

// 备注：并发使用，每个线程或者协程需要创建一个独立的 searcher 对象，但是都共享统一的只读 vectorIndex。
```

### 缓存整个 `xdb` 数据

如果你的 PHP 母环境支持，可以预先加载整个 `xdb` 的数据到内存，这样可以实现完全基于内存的查询，类似之前的 memory search 查询。
```php
// 1、从 dbPath 加载整个 xdb 到内存。
$cBuff = XdbSearcher::loadContentFromFile($dbPath);
if ($cBuff === null) {
    printf("failed to load content buffer from '%s'\n", $dbPath);
    return;
}

// 2、使用全局的 cBuff 创建带完全基于内存的查询对象。
try {
    $searcher = XdbSearcher::newWithBuffer($cBuff);
} catch (Exception $e) {
    printf("failed to create buffer cached searcher: %s\n", $dbFile, $e);
    return;
}

// 3、查询
$sTime = XdbSearcher::now();
$region = $searcher->search('1.2.3.4');
if ($region === null) {
    printf("failed search(1.2.3.4)\n");
    return;
}

printf("{region: %s, took: %.5f ms}\n", $region, XdbSearcher::now() - $sTime);

// 备注：并发使用，用整个 xdb 缓存创建的 searcher 对象可以安全用于并发。
```

# 查询测试

通过 `search_test.php` 脚本来进行查询测试：
```bash
➜  php git:(v2.0_xdb) ✗ php ./search_test.php
php ./search_test.php [command options]
options:
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试：
```bash
➜  php git:(v2.0_xdb) ✗ php ./search_test.php --db=../../data/ip2region.xdb --cache-policy=vectorIndex
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, ioCount: 7, took: 0.04492 ms}
ip2region>> 
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。

# bench 测试

通过 `bench_test.php` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  php git:(v2.0_xdb) ✗ php ./bench_test.php
php ./bench_test.php [command options]
options:
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 来进行 bench 测试：
```bash
➜  php git:(v2.0_xdb) ✗ php ./bench_test.php --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=vectorIndex
Bench finished, {cachePolicy: vectorIndex, total: 3417955, took: 15s, cost: 0.005 ms/op}
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。
