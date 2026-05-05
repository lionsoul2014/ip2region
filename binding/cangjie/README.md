# ip2region xdb searcher — Cangjie Binding

ip2region (v2.0/v3.0) xdb 数据库的仓颉语言查询库，支持 IPv4 和 IPv6，提供多线程安全的服务层。

## Architecture

```
binding/cangjie/
├── src/
│   ├── xdb/              # Low-level xdb query engine
│   │   ├── searcher.cj   # Searcher (file-only / vector-index / content-buff modes)
│   │   ├── util.cj       # IP parsing/comparison, byte readers
│   │   ├── header.cj     # xdb header + version parsing
│   │   ├── version.cj    # IPv4/IPv6 version definitions
│   │   ├── *_test.cj     # Unit tests (24 cases)
│   ├── service/          # High-level thread-safe service layer
│   │   ├── config.cj     # Config (cache policy, pool size, pre-loaded data)
│   │   ├── searcher_pool.cj  # SearcherPool (Semaphore + ConcurrentLinkedQueue)
│   │   ├── ip2region.cj  # Ip2Region unified API
│   │   ├── *_test.cj     # Unit tests (17 cases)
├── example/              # Executable CLI demo (separate project)
│   ├── cjpm.toml
│   └── src/main.cj       # 5 usage demos + benchmarks
├── cjpm.toml             # Static library project
└── README.md
```

**Two API layers:**

- **`ip2region.xdb`** — Low-level Searcher. Not thread-safe (mutable `ioCount` + `File` seek/read state). Supports three cache modes.
- **`ip2region.service`** — High-level `Ip2Region` class wrapping `SearcherPool`. Thread-safe, handles v4/v6 dispatch automatically.

## Build

```bash
cd binding/cangjie
cjpm build          # static library
cd example
cjpm build          # example executable
```

## Test

41 tests across both packages (24 xdb + 17 service):

```bash
cd binding/cangjie
cjpm test
```

## Cache Policies

| Policy | Memory | Speed | Thread-safe* |
|--------|--------|-------|-------------|
| `FileOnly` (0) | ~0 MB | ~21 µs/op (v4) | Via SearcherPool |
| `VectorIndex` (1) | ~4 MB | ~20 µs/op (v4) | Via SearcherPool |
| `ContentBuff` (2) | ~full xdb | ~5 µs/op (v4) | Via SearcherPool |

\* Low-level `Searcher` is NOT thread-safe. The service layer (`SearcherPool` / `Ip2Region`) provides thread safety by pooling searchers (FileOnly/VectorIndex) or sharing a read-only buffer (ContentBuff).

## Benchmark Results

Benchmarked on Windows 11, AMD Ryzen 7, 487k IPv4 records / 638k IPv6 records:

### IPv4 (`ip2region_v4.xdb`, 487,167 records)

| Cache Policy | Total | Time | Avg |
|-------------|-------|------|-----|
| ContentBuff | 487,167 | 3,059 ms | **5 µs/op** |
| VectorIndex | 487,167 | 10,413 ms | **20 µs/op** |
| FileOnly | 487,167 | 11,271 ms | **21 µs/op** |

### IPv6 (`ip2region_v6.xdb`, 638,953 records)

| Cache Policy | Total | Time | Avg |
|-------------|-------|------|-----|
| ContentBuff | 638,953 | 37,080 ms | **56 µs/op** |
| VectorIndex | 638,953 | 38,443 ms | **58 µs/op** |
| FileOnly | 638,953 | 19,148 ms | **29 µs/op** |

Run your own:

```bash
cd example
cjpm run -- bench --db ../../../data/ip2region_v4.xdb --src ../../../data/ipv4_source.txt --cache-policy content
cjpm run -- bench --db ../../../data/ip2region_v6.xdb --src ../../../data/ipv6_source.txt --cache-policy file
```

## Usage

### Low-level API (`ip2region.xdb`)

```cangjie
import ip2region.xdb.*

// Content buffer mode (fastest)
let content = File.readFrom(Path("ip2region_v4.xdb"))
let header = newHeaderFromBytes(content)
let version = versionFromHeader(header)
let searcher = Searcher(version, content, "ip2region_v4.xdb")
let region = searcher.search(parseIP("220.181.108.183"))
searcher.close()

// Vector index mode
let vIndex = loadVectorIndex(content)
let searcher2 = Searcher(version, "ip2region_v4.xdb", vIndex)
searcher2.close()

// File only mode
let searcher3 = Searcher(version, "ip2region_v4.xdb")
searcher3.close()
```

### High-level API (`ip2region.service`)

```cangjie
import ip2region.service.*

// Via factory (VectorIndex, 20 searchers)
let region = newIp2Region("ip2region_v4.xdb", "ip2region_v6.xdb")
let result = region.search("220.181.108.183")
region.close()

// Custom config
let v4Cfg = Config(ContentBuff, IPv4, "ip2region_v4.xdb", 10)
let v6Cfg = Config(VectorIndex, IPv6, "ip2region_v6.xdb", 20)
let svc = Ip2Region(v4Cfg, v6Cfg)
let result = svc.search("2408:8266:100:1000::")
svc.close()
```

### Single-version API

```cangjie
import ip2region.service.*

// IPv4 only — Ip2Region(config, IPv4)
let v4Cfg = Config(ContentBuff, IPv4, "ip2region_v4.xdb", 10)
let v4 = Ip2Region(v4Cfg, IPv4)
let r4 = v4.search("220.181.108.183")
v4.close()

// IPv6 only — Ip2Region(config, IPv6)
let v6Cfg = Config(ContentBuff, IPv6, "ip2region_v6.xdb", 10)
let v6 = Ip2Region(v6Cfg, IPv6)
let r6 = v6.search("2408:8266:100:1000::")
v6.close()

// Factory functions
let v4Only = newIp2RegionV4(v4Cfg)
let v6Only = newIp2RegionV6(v6Cfg)
v4Only.close(); v6Only.close()
```

### CLI

```bash
cd example
cjpm run
```

## Thread Safety

- **`xdb.Searcher`** — NOT thread-safe. Keep one per thread or use a pool.
- **`service.SearcherPool`** — Thread-safe. Pre-allocates N searchers, uses `Semaphore` for backpressure and `ConcurrentLinkedQueue` for storage. `borrow()` blocks until a searcher is available.
- **`service.Ip2Region`** — Thread-safe. Delegates to pool or shared in-mem searcher based on cache policy. ContentBuff mode shares a single Searcher (Array<Byte> is read-only), FileOnly/VectorIndex use the pool.
- **`service.Config`** — Immutable after construction. Safe to share.

## Requirements

- Cangjie SDK 1.1.0+
- `cjpm` build tool
