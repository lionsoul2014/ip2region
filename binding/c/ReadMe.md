# ip2region xdb c 查询客户端实现


# 使用方式

### 关于 IPv4 和 IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```c
#include "xdb_api.h";

// 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 IPv4
const char *db_path  = "../../data/ip2region_v4.xdb";  // 或者你的 ipv4 xdb 的路径
xdb_version_t *version = XDB_IPv4;

// 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 IPv6
const char *db_path  = "../../data/ip2region_v6.xdb";  // 或者你的 ipv6 xdb 路径
xdb_version_t *version = XDB_IPv6;

// db_path 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
// 备注：以下演示直接使用 db_path 和 version 变量
```

### XDB 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。 你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```c
#include "xdb_api.h";

int errcode = xdb_verify(db_path);
if ($err != 0) {
    // 适用性验证失败！！！
    // 当前查询客户端实现不适用于 db_path 指定的 xdb 文件的查询.
    // 应该停止启动服务，使用合适的 xdb 文件或者升级到适合 db_path 的 Searcher 实现。
    printf("failed to verify xdb file `%s`, errcode: %d\n", db_path, errcode);
    return;
}

// 验证通过，当前使用的 Searcher 可以安全的用于对 dbPath 指向的 xdb 的查询操作
```

### 完全基于文件的查询

```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_searcher_t searcher;
    char region_buffer[512] = {'\0'};
    xdb_region_buffer_t region;

    // 使用栈空间的 region_buffer 初始化 region_buffer_t
    int err = xdb_region_buffer_init(&region, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 1;
    }

    // 在服务启动的时候初始化 winsock，不需要重复调用，只需要在 windows 系统下调用
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1、从 db_path 初始化 xdb 查询对象.
    // @Note: 使用顶部描述的 db_path 和 version 来创建 searcher
    err = xdb_new_with_file_only(version, &searcher, db_path);
    if (err != 0) {
        printf("failed to create xdb searcher from `%s` with errno=%d\n", db_path, err);
        return 1;
    }

    // 2、调用 search API 查询，IPv4 和 IPv6 都支持.
    const char *ip_string = "1.2.3.4";
    // ip_string = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // 清理 region 信息的内存资源，每次 search 之后都得调用
    xdb_region_buffer_free(&region);

    // 备注：并发使用，每一个线程需要单独定义并且初始化一个 searcher 查询对象。

    // 3、关闭 xdb 查询器
    xdb_close(&searcher);
    xdb_clean_winsock();    // windows 下调用
    return 0;
}
```

### 缓存 `VectorIndex` 索引

我们可以提前从 xdb 文件中加载出来 VectorIndex 数据，然后全局缓存，每次创建 Searcher 对象的时候使用全局的 VectorIndex 缓存可以减少一次固定的 IO 操作，从而加速查询，减少 IO 压力。
```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_vector_index_t *v_index;
    xdb_searcher_t searcher;
    xdb_region_buffer_t region;

    // 使用 NULL 初始化 region_buffer，让其自动管理内存的分配
    int err = xdb_region_buffer_init(&region, NULL, 0);
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 0;
    }

    // 在服务启动的时候初始化 winsock，不需要重复调用，只需要在 windows 系统下调用
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1、从顶部描述的 db_path 加载 VectorIndex 索引。
    // 得到 v_index 做成全局缓存，便于后续反复使用。
    // 注意：v_index 不需要每次都加载，建议在服务启动的时候加载一次，然后做成全局资源。
    v_index = xdb_load_vector_index_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load vector index from `%s`\n", db_path);
        return 1;
    }

    // 2、使用全局的 VectorIndex 变量创建带 VectorIndex 缓存的 xdb 查询对象.
    // @Note: 使用顶部描述的 db_path 和 version 来创建 searcher
    err = xdb_new_with_vector_index(version, &searcher, db_path, v_index);
    if (err != 0) {
        printf("failed to create vector index cached searcher with errcode=%d\n", err);
        return 2;
    }


    // 3、调用 search API 查询，IPv4 和 IPv6 都支持
    const char *ip_string = "1.2.3.4";
    // ip_string = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // 清理 region 信息的内存资源，每次 search 之后都得调用
    xdb_region_buffer_free(&region);


    // 备注：并发使用，每一个线程需要单独定义并且初始化一个 searcher 查询对象。

    // 4、关闭 xdb 查询器，如果是要关闭服务，也需要释放 v_index 的内存。
    xdb_close(&searcher);
    xdb_close_vector_index(v_index);
    xdb_clean_winsock();
    return 0;
}
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 xdb 文件到内存，然后基于这个数据创建查询对象来实现完全基于内存的查询，类似之前的 memory search。
```c
#include <stdio.h>
#include "xdb_api.h"

int main(int argc, char *argv[]) {
    xdb_content_t *c_buffer;
    xdb_searcher_t searcher;
    xdb_region_buffer_t region;

    // 使用 NULL 初始化 region_buffer，让其自动管理内存的分配
    int err = xdb_region_buffer_init(&region, NULL, 0);
    if (err != 0) {
        printf("failed to init the region buffer with errcode=%d\n", err);
        return 0;
    }


    // 在服务启动的时候初始化 winsock，不需要重复调用，只需要在 windows 系统下调用
    err = xdb_init_winsock();
    if (err != 0) {
        printf("failed to init the winsock with errno=%d\n", err);
        return 1;
    }

    // 1、从 顶部描述的 db_path 加载整个 xdb 的数据。
    c_buffer = xdb_load_content_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load xdb content from `%s`\n", db_path);
        return 1;
    }

    // 2、使用全局的 c_buffer 变量创建一个完全基于内存的 xdb 查询对象.
    // @Note: 使用顶部描述的 version 来创建 searcher.
    err = xdb_new_with_buffer(version, &searcher, c_buffer);
    if (err != 0) {
        printf("failed to create content cached searcher with errcode=%d\n", err);
        return 2;
    }

    // 3、调用 search API 查询，IPv4 和 IPv6 都支持
    const char *ip_string = "1.2.3.4";
    // ip_string = "2001:4:112:ffff:ffff:ffff:ffff:ffff"; // IPv6

    long cost_time = 0, s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip_string, &region);
    cost_time = (int) (xdb_now() - s_time);
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip_string, err);
    } else {
        printf("{region: %s, took: %d μs}", region.value, cost_time);
    }

    // 清理 region 信息的内存资源，每次 search 之后都得调用
    xdb_region_buffer_free(&region);


    // 备注：并发使用，使用这种方式创建的 xdb 查询对象可以安全用于并发。
    // 建议在服务启动的时候创建好，然后一直安全并发使用，直到服务关闭。

    // 4、关闭 xdb 查询器，关闭服务的时候需要释放 c_buffer 的内存。
    xdb_close(&searcher);
    xdb_close_content(c_buffer);
    xdb_clean_winsock();
    return 0;
}
```

### 关于定位信息的存储
在旧版本的实现中，search相关的函数都是依靠指定一个 `region_buffer` 内存来用于存储地域信息，这种方式还是有很大的局限性。
新的实现提供了一个 `xdb_region_buffer_t` 对象来管理这些内存的分配，你依然可以指定一个固定的 `region_buffer` 来创建 region 的内存管理，这个情况适合当你的地域信息的最大长度是可知的，这种方式可以减少运行过程中内存的碎片堆积。如果地域信息的长度不确定或者你的程序不适合提前分配一块内存来管理，你可以通过指定 NULL 的方式来初始化 `xdb_region_buffer_t`，这样对象会自动管理内存的分配，也适合任意长度的地域信息的存储，不过这种方式在长期的运行过程中肯定会增加内存碎片的堆积。
```c
// 1, 通过指定一块内存来创建 region_buffer
char buffer[512];
xdb_region_buffer_t region;
int err = xdb_region_buffer_init(&region, buffer, sizeof(buffer));
if (err != 0) {
    // 初始化失败
    printf("failed to init region buffer width errcode=%d", err);
    return;
}

// 2，通过指定 NULL 来创建 region_buffer，让其自动按需分配内存
xdb_region_buffer_t region;
int err = xdb_region_buffer_init(&region, NULL, 0);
if (err != 0) {
    // 初始化失败
    printf("failed to init region buffer width errcode=%d", err);
    return;
}


// 备注：在每次调用 search 完成 IP 定位信息的查询后，你需要手动调用函数来释放内存 .
// search 函数使用未经清理的 region 信息会报错。
xdb_region_buffer_free(&region);
```


# 测试程序编译

通过如下方式编译得到 xdb_searcher 可执行程序：
```bash
# cd 到 c binding 根目录
➜  c git:(fr_c_ipv6) ✗ make
gcc -O2 -I./ xdb_util.c xdb_searcher.c main.c -o xdb_searcher
gcc -O2 -I./ xdb_util.c test_util.c -o test_util
```


# 查询测试

通过 `xdb_searcher search` 命令来测试对 xdb 的查询：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search
./xdb_searcher search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region_v4.xdb 进行 IPv4 查询测试：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search --db=../../data/ip2region_v4.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 120.229.45.2
{region: 中国|广东省|深圳市|移动, io_count: 3, took: 29 μs}
```

例如：使用默认的 data/ip2region_v6.xdb 进行 IPv6 查询测试：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher search --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> ::
{region: |||, io_count: 2, took: 38 μs}
ip2region>> 2604:bc80:8001:11a4:ffff:ffff:ffff:ffff
{region: 中国|广东省|深圳市|数据中心, io_count: 13, took: 77 μs}
```

输入 ip 即可进行查询，输入 quit 即可退出测试程序。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同的缓存实现的效率。



# bench 测试

通过 `xdb_searcher bench` 命令来进行 bench 测试，一方面确保查询程序和 `xdb` 文件没有错误，另一方面可以通过大量的查询得到评价的查询性能：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench                                  
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 来进行 IPv4 的 bench 测试：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
Bench finished, {cache_policy: vectorIndex, total: 1367686, took: 7.640s, cost: 5 μs/op}
```

例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 来进行 IPv6 的 bench 测试：
```bash
➜  c git:(fr_c_ipv6) ✗ ./xdb_searcher bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
Bench finished, {cache_policy: vectorIndex, total: 34159862, took: 857.750s, cost: 24 μs/op}
```

可以设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 不同缓存实现机制的效率。 @Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件相同的源文件。
