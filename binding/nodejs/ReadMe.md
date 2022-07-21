# ip2region nodejs 查询客户端实现

## 使用方式

### 完全基于文件的查询

### 缓存 `VectorIndex` 索引

### 缓存整个 `xdb` 数据

```js
const Ip2Region = require('./')

const ip2region = new Ip2Region()

// 注意：这边是同步代码，生产环境建议只执行一次初始化, 并且数据是存放在内存中的
ip2region.load('../../data/ip2region.xdb')

const info = ip2region.search('202.97.77.50')

```

## 查询测试

## bench 测试
