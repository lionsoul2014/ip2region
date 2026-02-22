[中文](README.md) | [English](README_en.md)

# ip2region

[ip2region](https://ip2region.net) - is an offline IP address localization library and IP localization data management framework. It supports both `IPv4` and `IPv6` with query efficiency at the 10-microsecond level. It provides `xdb` data generation and query client implementations for many mainstream programming languages.

---

# ip2region Features

### 1. Offline Localization Library

The project itself provides raw data for both IPv4 (`data/ipv4_source.txt`) and IPv6 (`data/ipv6_source.txt`), along with corresponding xdb files (`data/ip2region_v4.xdb` and `data/ip2region_v6.xdb`) to achieve city-level query localization. The field format is: `Country|Province|City|ISP|iso-alpha2-code`. Localization information for China is entirely in Chinese, while regional information for non-China areas is entirely in English.

### 2. Data Management Framework

`xdb` supports hundreds of millions of IP data segment rows. Region information supports full customization. The region information of the built-in data is fixed in the format: `Country|Province|City|ISP|iso-alpha2-Code`. You can append data for specific business needs to the region, such as: GPS information/International Standard Regional Codes/Zip codes, etc. In other words, you can fully use ip2region to manage your own IP localization data.

### 3. Data Deduplication and Compression

The `xdb` format generation program automatically processes the input raw data, checks and completes the merging of adjacent IP segments, and performs deduplication and compression of identical regional information.

### 4. High-Speed Query Response

Even for queries based entirely on the `xdb` file, the single query response time is at the 10-microsecond level. Memory-accelerated queries can be enabled through the following two methods:

1. `vIndex` Index Caching: Uses a fixed `512KiB` of memory to cache vector index data, reducing one disk IO operation and maintaining average query efficiency within 100 microseconds.
2. Entire `xdb` File Caching: Loads the entire `xdb` file into memory. Memory usage is equal to the `xdb` file size. There is no disk IO operation, maintaining 10-microsecond level query efficiency.

### 5. Unified Query Interface

`xdb` provides version-compatible query implementations. A unified API can simultaneously provide queries for both IPv4 and IPv6 data and return unified data.

---

# `xdb` Data Query

For API introductions, usage documentation, and test programs, please refer to the README introduction under the corresponding `searcher` query client. All query binding implementations are as follows:

| Programming Language | Description | IPv4 Support | IPv6 Support | Contributors |
| --- | --- | --- | --- | --- |
| [Golang](binding/golang)         | golang xdb query client          | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [PHP](binding/php)               | php xdb query client             | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Java](binding/java)             | java xdb query client            | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [C](binding/c)                   | POSIX C xdb query client         | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Lua_c](binding/lua_c)           | lua c extension xdb query client | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Lua](binding/lua)               | lua xdb query client             | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Rust](binding/rust)             | rust xdb query client            | :white_check_mark: | :white_check_mark: | [gongzhengyang](https://github.com/gongzhengyang) |
| [Python](binding/python)         | python xdb query client          | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Javascript](binding/javascript) | javascript xdb query client      | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)           |
| [Csharp](binding/csharp)         | csharp xdb query client          | :white_check_mark: | :white_check_mark: | [Alen Lee](https://github.com/malus2077) & [ArgoZhang](https://github.com/ArgoZhang) |
| [Erlang](binding/erlang)         | erlang xdb query client          | :white_check_mark: | :x:                | [malou](https://github.com/malou996)              |
| [Nginx](binding/nginx)           | nginx extension xdb query client | :white_check_mark: | :x:                | [Wu Jian Ping](https://github.com/wujjpp)         |
| [C++](binding/cpp)               | C++ xdb query client             | :white_check_mark: | :white_check_mark: | [Yunbin Liu](https://github.com/liuyunbin)        |

The following toolchain implementations are contributed by community developers via third-party repositories:

| Programming Language | Description | Contributors |
| --- | --- | --- |
| [ip2region-composer](https://github.com/zoujingli/ip2region)    | php composer management client       | [Zou Jingli](https://github.com/zoujingli)      |
| [ip2region-ts](https://github.com/Steven-Qiang/ts-ip2region2)   | node.js addon management client      | [Steven Qiang](https://github.com/Steven-Qiang) |
| [ruby-ip2region](https://github.com/jicheng1014/ruby-ip2region) | ruby xdb query client implementation | [jicheng1014](https://github.com/jicheng1014)   |
| [Ip2regionTool](https://github.com/orestonce/Ip2regionTool)     | ip2region data conversion tool       | [orestonce](https://github.com/orestonce)       |

---

# `xdb` Data Generation

For API introductions, usage documentation, and test programs, please refer to the README documents under the following `maker` generation programs:

| Programming Language | Description | IPv4 Support | IPv6 Support | Contributors |
| --- | --- | --- | --- | --- |
| [Golang](maker/golang) | golang xdb generation program | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)    |
| [Java](maker/java)     | java xdb generation program   | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)    |
| [Python](maker/python) | python xdb generation program | :white_check_mark: | :x:                | [leolin49](https://github.com/leolin49)    |
| [Csharp](maker/csharp) | csharp xdb generation program | :white_check_mark: | :x:                | [Alan Lee](https://github.com/malus2077)   |
| [Rust](maker/rust)     | rust xdb generation program   | :white_check_mark: | :white_check_mark: | [KevinWang](https://github.com/KevinWL) & [gongzhengyang](https://github.com/gongzhengyang) |
| [C++](maker/cpp)       | C++ xdb generation program    | :white_check_mark: | :white_check_mark: | [Yunbin Liu](https://github.com/liuyunbin) |

---

# `xdb` Data Update

The core of the ip2region project lies in **researching the design and implementation of IP data storage and fast querying**. The raw data `./data/ipv4_source.txt` and `./data/ipv6_source.txt` included in the project are updated irregularly. For scenarios with high requirements for data accuracy and update frequency, it is recommended to purchase commercial offline data from the [Ip2Region Community](https://ip2region.net/products/offline) or third-party vendors. You can try to update the data yourself using the following methods:

### Manual Editing and Updating

You can modify the data yourself based on the raw IP data provided by ip2region in `./data/ipv4_source.txt` and `./data/ipv6_source.txt` using the editing tools provided by ip2region. Currently, the data sources include:

1. Data provided by the ip2region community (please refer to the official account at the bottom for community notifications)
2. Project Issues tagged with `[Data_Updates]`
3. Other custom data: e.g., data provided by customers, data obtained through GPS and WIFI positioning, or legal and compliant data from other platforms.

For instructions on using the raw IP data editing tools, please refer to the README documents under the following `maker` generation programs:

| Programming Language | Description | IPv4 Support | IPv6 Support | Contributors |
| --- | --- | --- | --- | --- |
| [Golang](maker/golang%23xdb-%E6%95%B0%E6%8D%AE%E7%BC%96%E8%BE%91) | golang IP raw data editor | :white_check_mark: | :white_check_mark: | [Lion](https://github.com/lionsoul2014)    |
| [Java](maker/java%23xdb-%E6%95%B0%E6%8D%AE%E7%BC%96%E8%BE%91)     | java IP raw data editor   | :white_check_mark: | :x:                | [Lion](https://github.com/lionsoul2014)    |
| [C++](maker/cpp)                                                  | C++ IP raw data editor    | :white_check_mark: | :white_check_mark: | [Yunbin Liu](https://github.com/liuyunbin) |

### Detection Automatic Update

If you want to update data via your own API or data source, you can refer to the update algorithm based on the "Detection Algorithm" shared in the following videos to write your own update program:

1. [Data Update Implementation Video Sharing - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
2. [Data Update Implementation Video Sharing - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)

---

# Official Community

The Ip2Region official community was officially launched on `2025/06/12`. On one hand, it provides stable [commercial offline data](https://ip2region.net/products/offline) services. On the other hand, it facilitates the strengthening of the IP toolchain and data services outside the core code, such as [usage documentation](https://ip2region.net/doc/), [query testing](https://ip2region.net/search/demo), and data correction. For more information and services regarding the community, please visit the [Ip2Region Official Community](https://ip2region.net/).

---

# Related Remarks

### 1. xdb Technical Documents:

1. xdb Data Structure Analysis: ["ip2region xdb - Data Structure Description"](https://ip2region.net/doc/xdb/structure)
2. xdb Query Process Analysis: ["ip2region xdb - Query Process Description"](https://ip2region.net/doc/xdb/search)
3. xdb Generation Process Analysis: ["ip2region xdb - Generation Process Description"](https://ip2region.net/doc/xdb/generate)
4. xdb File Generation Tutorial: ["ip2region xdb - File Generation Tutorial"](https://ip2region.net/doc/data/xdb_make)
5. xdb Concurrent Safety Query: ["ip2region xdb - Concurrent Safety Query"](https://ip2region.net/doc/xdb/concurrent)
6. xdb Data Update Method: ["ip2region Data Update and Use of xdb Data Editor"](https://mp.weixin.qq.com/s/cZH5qIn4E5rQFy6N32RCzA)

### 3. Technical Information Blogs

1. WeChat Official Account - lionsoul-org, the author's active technical sharing channel
2. [Ip2Region Official Community](https://ip2region.net)
