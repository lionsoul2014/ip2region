:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region

[ip2region](https://ip2region.net) - is an offline IP address localization library and IP localization data management framework. It supports both `IPv4` and `IPv6` with query efficiency at the 10-microsecond level. It provides `xdb` data generation and query client implementations for many mainstream programming languages.


# Features

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


# `xdb` Query

For API introductions, usage documentation, and test programs, please refer to the README introduction under the corresponding `searcher` query client. All query binding implementations are as follows:

| Language | Description | IPv4 Support | IPv6 Support |
| --- | --- | --- | --- |
| [Golang](binding/golang/README.md)         | golang query client          | :white_check_mark: | :white_check_mark: |
| [PHP](binding/php/README.md)               | php query client             | :white_check_mark: | :white_check_mark: |
| [Java](binding/java/README.md)             | java query client            | :white_check_mark: | :white_check_mark: |
| [C](binding/c/README.md)                   | C[std=c99] query client      | :white_check_mark: | :white_check_mark: |
| [Lua_c](binding/lua_c/README.md)           | lua c extension query client | :white_check_mark: | :white_check_mark: |
| [Lua](binding/lua/README.md)               | lua query client             | :white_check_mark: | :white_check_mark: |
| [Rust](binding/rust/README.md)             | rust query client            | :white_check_mark: | :white_check_mark: |
| [Python](binding/python/README.md)         | python query client          | :white_check_mark: | :white_check_mark: |
| [Javascript](binding/javascript/README.md) | javascript query client      | :white_check_mark: | :white_check_mark: |
| [Csharp](binding/csharp)                   | csharp query client          | :white_check_mark: | :white_check_mark: |
| [Erlang](binding/erlang/README.md)         | erlang query client          | :white_check_mark: | :x:                |
| [Nginx](binding/nginx)                     | nginx extension query client | :white_check_mark: | :x:                |
| [C++](binding/cpp/README.md)               | C++ query client             | :white_check_mark: | :white_check_mark: |

The following toolchain implementations are contributed by community developers via third-party repositories:

| Language | Description |
| --- | --- |
| [ip2region-composer](https://github.com/zoujingli/ip2region)    | php composer management client       |
| [ip2region-ts](https://github.com/Steven-Qiang/ts-ip2region2)   | node.js addon management client      |
| [ruby-ip2region](https://github.com/jicheng1014/ruby-ip2region) | ruby xdb query client implementation |
| [Ip2regionTool](https://github.com/orestonce/Ip2regionTool)     | ip2region data conversion tool       |


# `xdb` Generation

For API introductions, usage documentation, and test programs, please refer to the README documents under the following `maker` generation programs:

| Language | Description | IPv4 Support | IPv6 Support |
| --- | --- | --- | --- |
| [Golang](maker/golang/README.md) | golang xdb generation program | :white_check_mark: | :white_check_mark: |
| [Java](maker/java/README.md)     | java xdb generation program   | :white_check_mark: | :white_check_mark: |
| [Python](maker/python)           | python xdb generation program | :white_check_mark: | :x:                |
| [Csharp](maker/csharp)           | csharp xdb generation program | :white_check_mark: | :x:                |
| [Rust](maker/rust)               | rust xdb generation program   | :white_check_mark: | :white_check_mark: |
| [C++](maker/cpp)                 | C++ xdb generation program    | :white_check_mark: | :white_check_mark: |


# `xdb` Update

The core of the ip2region project lies in **researching the design and implementation of IP data storage and fast querying**. The raw data `./data/ipv4_source.txt` and `./data/ipv6_source.txt` included in the project are updated irregularly. For scenarios with high requirements for data accuracy and update frequency, it is recommended to purchase commercial offline data from the [Ip2Region Community](https://ip2region.net/products/offline) or third-party vendors. You can try to update the data yourself using the following methods:

### Manual Editing and Updating

You can modify the data yourself based on the raw IP data provided by ip2region in `./data/ipv4_source.txt` and `./data/ipv6_source.txt` using the editing tools provided by ip2region. Currently, the data sources include:

1. Data provided by the ip2region community (please refer to the official account at the bottom for community notifications)
2. Project Issues tagged with `[Data_Updates]`
3. Other custom data: e.g., data provided by customers, data obtained through GPS and WIFI positioning, or legal and compliant data from other platforms.

For instructions on using the raw IP data editing tools, please refer to the README documents under the following `maker` generation programs:

| Language | Description | IPv4 Support | IPv6 Support |
| --- | --- | --- | --- |
| [Golang](maker/golang#xdb-%E6%95%B0%E6%8D%AE%E7%BC%96%E8%BE%91) | golang IP raw data editor | :white_check_mark: | :white_check_mark: |
| [Java](maker/java#xdb-%E6%95%B0%E6%8D%AE%E7%BC%96%E8%BE%91)     | java IP raw data editor   | :white_check_mark: | :x:                |
| [C++](maker/cpp)                                                | C++ IP raw data editor    | :white_check_mark: | :white_check_mark: |

### Detection Automatic Update

If you want to update data via your own API or data source, you can refer to the update algorithm based on the "Detection Algorithm" shared in the following videos to write your own update program:

1. [Data Update Implementation Video Sharing - part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
2. [Data Update Implementation Video Sharing - part2](https://www.bilibili.com/video/BV1pF411j7Aw/)


# Official Community

The Ip2Region official community was officially launched on `2025/06/12`. On one hand, it provides stable [commercial offline data](https://ip2region.net/products/offline) services. On the other hand, it facilitates the strengthening of the IP toolchain and data services outside the core code, such as [usage documentation](https://ip2region.net/doc/), [query testing](https://ip2region.net/search/demo), and data correction. For more information and services regarding the community, please visit the [Ip2Region Official Community](https://ip2region.net/).


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
