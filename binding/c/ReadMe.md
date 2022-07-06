# ip2region xdb c 查询客户端实现


# 使用方式

### 完全基于文件的查询

```c
#include <stdio.h>
#include "xdb_searcher.h"

int main(int argc, char *argv[]) {
    char *db_path = "ip2region.xdb file path";
    xdb_searcher_t searcher;
    char region_buffer[256], ip_buffer[16], *ip = "1.2.3.4";
    long s_time;

    // 1、从 db_path 初始化 xdb 查询对象
    int err = xdb_new_with_file_only(&searcher, db_path);
    if (err != 0) {
        printf("failed to create xdb searcher from `%s` with errno=%d\n", db_path, err);
        return 1;
    }

    // 2、调用 search API 查询
    // 得到的 region 信息会存储到 region_buffer 里面，如果你自定义了数据，请确保给足 buffer 的空间。
    s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip, err);
    } else {
        printf("{region: %s, took: %d μs}", region_buffer, (int)(xdb_now() - s_time));
    }

    // 备注：并发使用，没一个线程需要单独定义并且初始化一个 searcher 查询对象。

    // 3、关闭 xdb 查询器
    xdb_close(&searcher);
    return 0;
}
```

### 缓存 `VectorIndex` 索引

我们可以提前从 xdb 文件中加载出来 VectorIndex 数据，然后全局缓存，每次创建 Searcher 对象的时候使用全局的 VectorIndex 缓存可以减少一次固定的 IO 操作，从而加速查询，减少 IO 压力。
```c
#include <stdio.h>
#include "xdb_searcher.h"

int main(int argc, char *argv[]) {
    char *db_path = "ip2region.xdb file path";
    xdb_vector_index_t *v_index;
    xdb_searcher_t searcher;
    char region_buffer[256], ip_buffer[16], *ip = "1.2.3.4";
    long s_time;

    // 1、从 db_path 加载 VectorIndex 索引。
    // 得到 v_index 做成全局缓存，便于后续反复使用。
    // 注意：v_index 不需要每次都加载，建议在服务启动的时候加载一次，然后做成全局资源。
    v_index = xdb_load_vector_index_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load vector index from `%s`\n", db_path);
        return 1;
    }

    // 2、使用全局的 VectorIndex 变量创建带 VectorIndex 缓存的 xdb 查询对象
    int err = xdb_new_with_vector_index(&searcher, db_path, v_index);
    if (err != 0) {
        printf("failed to create vector index cached searcher with errcode=%d\n", err);
        return 2;
    }

    // 3、调用 search API 查询
    // 得到的 region 信息会存储到 region_buffer 里面，如果你自定义了数据，请确保给足 buffer 的空间。
    s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip, err);
    } else {
        printf("{region: %s, took: %d μs}", region_buffer, (int)(xdb_now() - s_time));
    }

    // 备注：并发使用，没一个线程需要单独定义并且初始化一个 searcher 查询对象。

    // 4、关闭 xdb 查询器，如果是要关闭服务，也需要释放 v_index 的内存。
    xdb_close(&searcher);
    xdb_close_vector_index(v_index);
    return 0;
}
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 ip2region.xdb 的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。
```c
#include <stdio.h>
#include "xdb_searcher.h"

int main(int argc, char *argv[]) {
    char *db_path = "ip2region.xdb file path";
    xdb_content_t *c_buffer;
    xdb_searcher_t searcher;
    char region_buffer[256], ip_buffer[16], *ip = "1.2.3.4";
    long s_time;

    // 1、从 db_path 加载整个 xdb 的数据。
    c_buffer = xdb_load_content_from_file(db_path);
    if (v_index == NULL) {
        printf("failed to load xdb content from `%s`\n", db_path);
        return 1;
    }

    // 2、使用全局的 c_buffer 变量创建一个完全基于内存的 xdb 查询对象
    err = xdb_new_with_buffer(&searcher, c_buffer);
    if (err != 0) {
        printf("failed to create content cached searcher with errcode=%d\n", err);
        return 2;
    }

    // 3、调用 search API 查询
    // 得到的 region 信息会存储到 region_buffer 里面，如果你自定义了数据，请确保给足 buffer 的空间。
    s_time = xdb_now();
    err = xdb_search_by_string(&searcher, ip, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        printf("failed search(%s) with errno=%d\n", ip, err);
    } else {
        printf("{region: %s, took: %d μs}", region_buffer, (int)(xdb_now() - s_time));
    }

    // 备注：并发使用，使用这种方式创建的 xdb 查询对象可以安全用于并发。
    // 建议在服务启动的时候创建好，然后一直安全并发使用，直到服务关闭。

    // 4、关闭 xdb 查询器，关闭服务的时候需要释放 c_buffer 的内存。
    xdb_close(&searcher);
    xdb_close_content(c_buffer);
    return 0;
}
```


# 测试程序编译

通过如下方式编译得到 xdb_searcher 可执行程序：
```bash
# cd 到 c binding 根目录
make
```


# 查询测试

通过 `xdb_searcher search` 命令来测试对 ip2region.xdb 的查询：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher search
./xdb_searcher search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher search --db=../../data/ip2region.xdb --cache-policy=vectorIndex
ip2region xdb searcher test program, cache_policy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, io_count: 7, took: 13 μs}
ip2region>> 
```

输入 ip 即可进行查询，输入 quit 即可退出测试程序。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同的缓存实现的效率。



# bench 测试

通过 `xdb_searcher bench` 命令来进行 bench 测试，一方面确保查询程序和 `xdb` 文件没有错误，另一方面可以通过大量的查询得到评价的查询性能：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher bench
./xdb_searcher bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 来进行 bench 测试：
```bash
➜  c git:(c_binding) ✗ ./xdb_searcher bench --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=vectorIndex
Bench finished, {cache_policy: vectorIndex, total: 3417955, took: 4.233s, cost: 1 μs/op}
```

可以设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 不同缓存实现机制的效率。 @Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件相同的源文件。
