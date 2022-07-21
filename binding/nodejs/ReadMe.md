# ip2region nodejs 查询客户端实现

## 使用方式

### 完全基于文件的查询

```javascript
// 导入包
const Searcher = require('.')
// 指定ip2region数据文件路径
const dbPath = 'ip2region.xdb file path'

try {
  // 创建searcher对象
  const searcher = Searcher.newWithFileOnly(dbPath)
  // 查询
  const data = await searcher.search('218.4.167.70')
  // data: '中国|0|江苏省|苏州市|电信'
} catch(e) {
  console.log(e)
}

```

### 缓存 `VectorIndex` 索引

```javascript
// 导入包
const Searcher = require('.')
// 指定ip2region数据文件路径
const dbPath = 'ip2region.xdb file path'

try {
  // 同步读取vectorIndex
  const vectorIndex = Searcher.loadVectorIndexFromFile(dbPath)
  // 创建searcher对象
  const searcher = Searcher.newWithVectorIndex(dbPath, vectorIndex)
  // 查询 await 或 promise均可
  const data = await searcher.search('218.4.167.70')
  // data: '中国|0|江苏省|苏州市|电信'
} catch(e) {
  console.log(e)
}
```

### 缓存整个 `xdb` 数据

```javascript
// 导入包
const Searcher = require('.')
// 指定ip2region数据文件路径
const dbPath = 'ip2region.xdb file path'

try {
  // 同步读取buffer
  const buffer = Searcher.loadContentFromFile(dbPath)
  // 创建searcher对象
  const searcher = Searcher.newWithVectorIndex(buffer)
  // 查询 await 或 promise均可
  const data = await searcher.search('218.4.167.70')
  // data: '中国|0|江苏省|苏州市|电信'
} catch(e) {
  console.log(e)
}
```

## 查询测试

可以通过 `java -jar ip2region-{version}.jar search` 命令来测试查询：

```shell
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar search
java -jar ip2region-{version}.jar search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 文件进行查询测试：

```shell
➜  java git:(v2.0_xdb) ✗ java -jar target/ip2region-2.6.0.jar search --db=../../data/ip2region.xdb
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, ioCount: 7, took: 82 μs}
ip2region>>
```

输入 ip 即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。

## bench 测试

## 单元测试结果

```shell
ip2region
  ✔ #newWithFileOnly
  ✔ #newWithVectorIndex
  ✔ #newWithBuffer

3 passing (10ms)

----------|---------|----------|---------|---------|----------------------------------
File      | % Stmts | % Branch | % Funcs | % Lines | Uncovered Line #s
----------|---------|----------|---------|---------|----------------------------------
All files |   91.17 |    60.71 |     100 |   91.17 |
 index.js |   91.17 |    60.71 |     100 |   91.17 | 56,70,80,137,143,173,179,193,201
----------|---------|----------|---------|---------|----------------------------------
```
