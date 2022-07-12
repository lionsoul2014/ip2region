# ip2region python 查询客户端实现

# 使用方式

### 完全基于文件的查询

```python
from xdbSearcher import XdbSearcher

def searchWithFile():
    # 1. 创建查询对象
    dbPath = "../../data/ip2region.xdb"
    searcher = XdbSearcher(dbfile=dbPath)
    
    # 2. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.searchByIPStr(ip)
    print(region_str)
    
    # 3. 关闭searcher
    searcher.close()
```

### 缓存 `VectorIndex` 索引

我们可以提前从 `xdb` 文件中加载出来 `VectorIndex` 数据，然后全局缓存，每次创建 Searcher 对象的时候使用全局的 VectorIndex 缓存可以减少一次固定的 IO 操作，从而加速查询，减少 IO 压力。

```python
from xdbSearcher import XdbSearcher

def searchWithVectorIndex():
     # 1. 预先加载整个 xdb
    dbPath = "../../data/ip2region.xdb"
    vi = XdbSearcher.loadVectorIndexFromFile(dbfile=dbPath)

    # 2. 使用上面的缓存创建查询对象, 同时也要加载 xdb 文件
    searcher = XdbSearcher(dbfile=dbPath, vectorIndex=vi)
    
    # 3. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.search(ip)
    print(region_str)

    # 4. 关闭searcher
    searcher.close()
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 ip2region.xdb 的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。

```python
from xdbSearcher import XdbSearcher

def searchWithContent():
    # 1. 预先加载整个 xdb
    dbPath = "../../data/ip2region.xdb";
    cb = XdbSearcher.loadContentFromFile(dbfile=dbPath)
    
    # 2. 仅需要使用上面的全文件缓存创建查询对象, 不需要传源 xdb 文件
    searcher = XdbSearcher(contentBuff=cb)
    
    # 3. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.search(ip)
    print(region_str)

    # 4. 关闭searcher
    searcher.close()
```
# 查询测试

通过 `search_test.py` 脚本来进行查询测试：
```bash
➜  python git:(python_dev) ✗ python3 ./search_test.py
python3 search_test.py [command options]
options:
 --db string             ip2region binary xdb file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 进行查询测试：
```bash
➜  python git:(python_dev) ✗ python3 ./search_test.py --db=../../data/ip2region.xdb --cache-policy=content
ip2region xdb searcher test program, cachePolicy: content
type 'quit' to exit
ip2region>> 1.2.3.4
region :美国|0|华盛顿|0|谷歌 , took 0.0689 ms
ip2region>> quit
searcher test program exited, thanks for trying
```

输入 ip 即可进行查询测试。也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效率。

# bench 测试

通过 `bench_test.py` 脚本来进行自动 bench 测试，一方面确保 `xdb` 文件没有错误，另一方面通过大量的查询测试平均查询性能：
```bash
➜  python git:(python_dev) ✗ python3 ./bench_test.py
python bench_test.py [command options]
options:
 --db string             ip2region binary xdb file path
 --src string            source ip text file path
 --cache-policy string   cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 来进行 bench 测试：
```bash
➜  python git:(python_dev) ✗ python3 ./bench_test.py --db=../../data/ip2region.xdb --src=../../data/ip.merge.txt --cache-policy=content
Bench finished, [cachePolicy: content, total: 3417955, took: 34.93 s, cost: 0.0094 ms/op]
```

可以通过设置 `cache-policy` 参数来分别测试 file/vectorIndex/content 三种不同的缓存实现的的性能。
@Note：请注意 bench 使用的 src 文件需要是生成对应的 xdb 文件的相同的源文件。