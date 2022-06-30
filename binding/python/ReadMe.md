# ip2region python 查询客户端实现

# 使用方式

### 完全基于文件的查询

```python
import ip2Region

if __name__ == '__main__':
    # 1. 创建查询对象
    dbPath = "./data/ip2region.xdb";
    searcher = ip2Region.Ip2Region(dbfile=dbPath)
    
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
import ip2Region

if __name__ == '__main__':
    # 1. 预先加载 VectorIndex 缓存
    dbPath = "./data/ip2region.xdb";
    vi = ip2Region.Ip2Region.loadVectorIndexFromFile(dbfile=dbPath)

    # 2. 使用上面的缓存创建查询对象, 同时也要加载 xdb 文件
    searcher = ip2Region.Ip2Region(dbfile=dbPath, vectorIndex=vi)
    
    # 3. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.searchByIPStr(ip)
    print(region_str)
    
    # 4. 关闭searcher
    searcher.close()
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 ip2region.xdb 的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。

```python
import ip2Region

if __name__ == '__main__':
    # 1. 预先加载整个 xdb
    dbPath = "./data/ip2region.xdb";
    cb = ip2Region.Ip2Region.loadContentFromFile(dbfile=dbPath)
    
    # 2. 仅需要使用上面的全文件缓存创建查询对象, 不需要传源 xdb 文件
    searcher = ip2Region.Ip2Region(contentBuff=cb)
    
    # 3. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.searchByIPStr(ip)
    print(region_str)
    
    # 4. 关闭searcher
    searcher.close()

```
# 查询测试

# bench 测试
