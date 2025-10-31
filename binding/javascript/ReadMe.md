# ip2region xdb javascript 查询客户端实现

# 使用方式

### 安装 `ip2region.js`
```bash
npm install ip2region.js --save 
```

### 关于查询 API
查询 API 的原型为：
```javascript 
// 通过字符串 IP 或者 parseIP 解析得到的二进制 IP (Buffer类型) 进行查询
search(ip: string | Buffer): string;
```
如果查询出错会抛异常，查询成功则会返回字符的 `region` 信息，如果指定的 IP 查询不到则会返回空字符串 `""`。

### 关于 IPv4 和 IPv6
该 xdb 查询客户端实现同时支持对 IPv4 和 IPv6 的查询，使用方式如下：
```javascript
import {IPv4, IPv6} from 'ip2region.js';

// 如果是 IPv4: 设置 xdb 路径为 v4 的 xdb 文件，IP版本指定为 Version.IPv4
let dbPath  = "../../data/ip2region_v4.xdb";  // 或者你的 ipv4 xdb 的路径
let version = IPv4;

// 如果是 IPv6: 设置 xdb 路径为 v6 的 xdb 文件，IP版本指定为 Version.IPv6
let dbPath  = "../../data/ip2region_v6.xdb";  // 或者你的 ipv6 xdb 路径
let version = IPv6;

// dbPath 指定的 xdb 的 IP 版本必须和 version 指定的一致，不然查询执行的时候会报错
// 备注：以下演示直接使用 dbPath 和 version 变量
```

### 文件验证
建议您主动去验证 xdb 文件的适用性，因为后期的一些新功能可能会导致目前的 Searcher 版本无法适用你使用的 xdb 文件，验证可以避免运行过程中的一些不可预测的错误。 你不需要每次都去验证，例如在服务启动的时候，或者手动调用命令验证确认版本匹配即可，不要在每次创建的 Searcher 的时候运行验证，这样会影响查询的响应速度，尤其是高并发的使用场景。
```javascript
import {verifyFromFile} from 'ip2region.js';

try {
    verifyFromFile(dbPath);
} catch (e) {
    // 适用性验证失败！！！
    // 当前查询客户端实现不适用于 dbPath 指定的 xdb 文件的查询.
    // 应该停止启动服务，使用合适的 xdb 文件或者升级到适合 dbPath 的 Searcher 实现。
    console.log(`binding is not applicable for xdb file '${dbPath}': ${e.message}`);
    return;
}

// 验证通过，当前使用的 Searcher 可以安全的用于对 dbPath 指向的 xdb 的查询操作
```

### 完全基于文件的查询

```javascript
import {newWithFileOnly} from 'ip2region.js';

// 1，使用上述的 version 和 dbPath 创建完全基于文件的查询对象
let searcher;
try {
    searcher = newWithFileOnly(version, dbPath);
} catch(e) {
    console.log(`failed to newWithFileOnly: ${err.message}`);
    return;
}


// 2、查询，IPv4 或者 IPv6 的地址都是同一个接口
let ip = "1.2.3.4";
// ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff";  // IPv6
try {
    let region = searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
} catch(e) {
    console.log(`${err.message}`);
}

// 3、关闭资源
searcher.close();

// 备注：每个线程需要单独创建一个独立的 Searcher 对象
```

### 缓存 `VectorIndex` 索引

我们可以提前从 `xdb` 文件中加载出来 `VectorIndex` 数据，然后全局缓存，每次创建 Searcher 对象的时候使用全局的 VectorIndex 缓存可以减少一次固定的 IO 操作，从而加速查询，减少 IO 压力。
```javascript
import {loadVectorIndexFromFile, newWithVectorIndex} from 'ip2region.js';

// 1、从 dbPath 中预先加载 VectorIndex 缓存，并且把这个得到的数据作为全局变量，后续反复使用。
let vIndex;
try {
    vIndex = loadVectorIndexFromFile(dbPath);
} catch (e) {
    console.log(`failed to load vector index from ${dbPath}: ${e.message}`);
    return;
}

// 2、使用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
let searcher;
try {
    searcher = newWithVectorIndex(version, vIndex, dbPath);
} catch(e) {
    console.log(`failed to newWithVectorIndex: ${err.message}`);
    return;
}


// 3、查询，IPv4 或者 IPv6 的地址都是同一个接口
let ip = "1.2.3.4";
// ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff";  // IPv6
try {
    let region = searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
} catch(e) {
    console.log(`${err.message}`);
}

// 4、关闭资源
searcher.close();

// 备注：每个线程需要单独创建一个独立的 Searcher 对象，但是都共享全局的只读 vIndex 缓存。
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 xdb 文件的数据到内存，然后基于这个数据创建查询对象来实现完全基于内存的查询，类似之前的 memory search。
```javascript
import {loadContentFromFile, newWithBuffer} from 'ip2region.js';

// 1、从 dbPath 加载整个 xdb 到内存。
let cBuffer;
try {
    cBuffer = loadContentFromFile(dbPath);
} catch (e) {
    console.log(`failed to load content from ${dbPath}: ${e.message}`);
    return;
}

// 2、使用上述的 cBuff 创建一个完全基于内存的查询对象。
let searcher;
try {
    searcher = newWithBuffer(version, cBuffer);
} catch(e) {
    console.log(`failed to newWithBuffer: ${err.message}`);
    return;
}

// 3、查询，IPv4 或者 IPv6 的地址都是同一个接口
let ip = "1.2.3.4";
// ip = "2001:4:112:ffff:ffff:ffff:ffff:ffff";  // IPv6
try {
    let region = searcher.search(ip);
    console.log(`search(${ip}): {region: ${region}`);
} catch(e) {
    console.log(`${err.message}`);
}
        
// 4、关闭资源 - 该 searcher 对象可以安全用于并发，等整个服务关闭的时候再关闭 searcher
// searcher.close();

// 备注：并发使用，用整个 xdb 数据缓存创建的查询对象可以安全的用于并发，也就是你可以把这个 searcher 对象做成全局对象去跨线程访问。
```


# 查询测试

可以通过 `node tests/search.app.js` 命令来测试查询：
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

例如：使用默认的 data/ip2region_v4.xdb 文件进行 IPv4 的查询测试：
```bash
➜  javascript git:(fr_javascript_ipv6) ✗ node tests/search.app.js --db=../../data/ip2region_v4.xdb                                
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v4.xdb (IPv4, vectorIndex)
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|华盛顿|0|谷歌, ioCount: 7, took: 0.430357 μs}
```

例如：使用默认的 data/ip2region_v6.xdb 文件进行 IPv6 的查询测试：
```bash
➜  javascript git:(fr_javascript_ipv6) ✗ node tests/search.app.js --db=../../data/ip2region_v6.xdb
ip2region xdb searcher test program
source xdb: ../../data/ip2region_v6.xdb (IPv6, vectorIndex)
type 'quit' to exit
ip2region>> 240e:3b7:3272:d8d0:db09:c067:8d59:539e
{region: 中国|广东省|深圳市|家庭宽带, ioCount: 14, took: 4.727663 μs}
```

输入 ip 即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。


# bench 测试

可以通过 `node tests/bench.app.js` 命令来进行 bench 测试，一方面确保 `xdb` 文件没有错误，一方面可以评估查询性能：
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

例如：通过默认的 data/ip2region_v4.xdb 和 data/ipv4_source.txt 文件进行 IPv4 的 bench 测试：
```bash
node tests/bench.app.js --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt
```

例如：通过默认的 data/ip2region_v6.xdb 和 data/ipv6_source.txt 文件进行 IPv6 的 bench 测试：
```bash
node tests/bench.app.js --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt
```

可以通过分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效果。
@Note: 注意 bench 使用的 src 文件要是生成对应 xdb 文件相同的源文件。

### 第三方库支持：
1. [ts-ip2region2](https://github.com/Steven-Qiang/ts-ip2region2) - 基于官方的 C 扩展，比纯 JS 有更高的运行效率。
