package ip2regionExample

import std.fs.*
import std.time.*
import std.collection.*
import ip2region.xdb.*
import ip2region.service.*

// ip2region Cangjie 使用示例
//
// 构建: cjpm build
// 运行: cjpm run
//
// 默认从 ../../../data/ 查找 xdb 文件，
// 也可通过命令行参数指定路径:
//   cjpm run -- --v4-db <path> --v6-db <path>

func printHelp() {
    println("ip2region Cangjie 使用示例")
    println("用法: cjpm run -- [options]")
    println("选项:")
    println("  --v4-db <path>   IPv4 xdb 文件路径 (默认: ../../../data/ip2region_v4.xdb)")
    println("  --v6-db <path>   IPv6 xdb 文件路径 (默认: ../../../data/ip2region_v6.xdb)")
    println("  --help           显示此帮助")
}

// ============================================================
// 方式一: 使用底层 Searcher API (ip2region.xdb)
// 可自行选择缓存策略，单线程使用
// ============================================================
func demoXdbSearcher(v4Path: String, v6Path: String) {
    println("\n=== 方式一: 底层 Searcher API ===")

    // 1. 读取 xdb 文件到内存
    let v4Content = File.readFrom(Path(v4Path))

    // 2. 解析文件头，获取版本信息
    let v4Header = newHeaderFromBytes(v4Content)
    let v4Version = versionFromHeader(v4Header)

    // 3. 选择缓存策略创建 Searcher
    //    支持三种模式: ContentBuff(最快), VectorIndex(均衡), FileOnly(省内存)
    let vIndex = loadVectorIndex(v4Content)
    let searcher = Searcher(v4Version, v4Path, vIndex)  // VectorIndex 模式

    // 4. 查询 IP
    let ip = parseIP("220.181.108.183")
    let region = searcher.search(ip)
    println("  IP: 220.181.108.183")
    println("  结果: ${region}")

    searcher.close()
}

// ============================================================
// 方式二: 使用 Ip2Region 服务 API (ip2region.service)
// 自动管理连接池，支持多线程安全访问
// ============================================================
func demoIp2Region(v4Path: String, v6Path: String) {
    println("\n=== 方式二: Ip2Region 服务 API ===")

    // 1. 创建配置 (缓存策略 + 连接池大小)
    let v4Cfg = Config(VectorIndex, IPv4, v4Path, 10)
    let v6Cfg = Config(VectorIndex, IPv6, v6Path, 10)

    // 2. 创建服务 (同时支持 IPv4 + IPv6)
    let region = Ip2Region(v4Cfg, v6Cfg)

    // 3. 查询 IPv4
    let r1 = region.search("220.181.108.183")
    println("  IPv4: 220.181.108.183 -> ${r1}")

    // 4. 查询 IPv6
    let r2 = region.search("2408:8266:100:1000::")
    println("  IPv6: 2408:8266:100:1000:: -> ${r2}")

    region.close()
}

// ============================================================
// 方式三: 单版本服务 (仅 IPv4 或 仅 IPv6)
// 使用 init(config, ipVersion) 构造函数
// ============================================================
func demoSingleVersion(v4Path: String, v6Path: String) {
    println("\n=== 方式三: 单版本服务 ===")

    // IPv4 only
    let v4Cfg = Config(ContentBuff, IPv4, v4Path, 3)
    let v4Region = Ip2Region(v4Cfg, IPv4)
    let r4 = v4Region.search("220.181.108.183")
    println("  IPv4 only: 220.181.108.183 -> ${r4}")
    v4Region.close()

    // IPv6 only
    let v6Cfg = Config(ContentBuff, IPv6, v6Path, 3)
    let v6Region = Ip2Region(v6Cfg, IPv6)
    let r6 = v6Region.search("2408:8266:100:1000::")
    println("  IPv6 only: 2408:8266:100:1000:: -> ${r6}")
    v6Region.close()
}

// ============================================================
// 方式四: 使用简便工厂函数 (ip2region.service)
// 一行创建，默认 VectorIndex + 20 连接池
// ============================================================
func demoFactory(v4Path: String, v6Path: String) {
    println("\n=== 方式四: 简便工厂函数 ===")

    let region = newIp2Region(v4Path, v6Path)
    let result = region.search("220.181.108.183")
    println("  220.181.108.183 -> ${result}")
    region.close()
}

// ============================================================
// 性能测试: 测试三种缓存策略的查询速度
// ============================================================
func bench(v4Path: String) {
    println("\n=== 性能测试 (IPv4, 487k 条记录) ===")

    let content = File.readFrom(Path(v4Path))
    let header = newHeaderFromBytes(content)
    let version = versionFromHeader(header)
    let vIndex = loadVectorIndex(content)

    // 从数据文件读取测试 IP 列表
    let srcPath = v4Path.replace("ip2region_v4.xdb", "ipv4_source.txt")
    let srcContent = File.readFrom(Path(srcPath))
    let lines = String.fromUtf8(srcContent).split("\n")

    var ipList = ArrayList<Array<Byte>>()
    for (line in lines) {
        let trimmed = line.trimAsciiStart().trimAsciiEnd()
        if (trimmed.size == 0) { continue }
        let parts = trimmed.split("|")
        if (parts.size < 3) { continue }
        try { ipList.add(parseIP(parts[0])) } catch (_) { continue }
    }
    println("  加载了 ${ipList.size} 个 IP")

    // 测试 ContentBuff 模式
    println("\n  [ContentBuff 模式]")
    let s1 = Searcher(version, content, v4Path)
    benchSearcher(s1, ipList)

    // 测试 VectorIndex 模式
    println("  [VectorIndex 模式]")
    let s2 = Searcher(version, v4Path, vIndex)
    benchSearcher(s2, ipList)

    // 测试 FileOnly 模式
    println("  [FileOnly 模式]")
    let s3 = Searcher(version, v4Path)
    benchSearcher(s3, ipList)

    s1.close(); s2.close(); s3.close()
}

func benchSearcher(searcher: Searcher, ipList: ArrayList<Array<Byte>>) {
    let count = ipList.size
    let start = MonoTime.now()
    for (i in 0..count) {
        let _ = searcher.search(ipList[i])
    }
    let elapsed = MonoTime.now() - start
    let avg = elapsed.toMicroseconds() / count
    println("    总量: ${count}, 耗时: ${elapsed.toMilliseconds()} ms, 平均: ${avg} us/op")
}

main(args: Array<String>): Int64 {
    var v4Path = "../../../data/ip2region_v4.xdb"
    var v6Path = "../../../data/ip2region_v6.xdb"

    var i = 0
    while (i < args.size) {
        if (args[i] == "--v4-db" && i + 1 < args.size) {
            v4Path = args[i + 1]; i = i + 2
        } else if (args[i] == "--v6-db" && i + 1 < args.size) {
            v6Path = args[i + 1]; i = i + 2
        } else if (args[i] == "--help") {
            printHelp(); return 0
        } else { i = i + 1 }
    }

    println("ip2region Cangjie 使用示例")
    println("xdb 路径: ${v4Path}")

    demoXdbSearcher(v4Path, v6Path)
    demoIp2Region(v4Path, v6Path)
    demoSingleVersion(v4Path, v6Path)
    demoFactory(v4Path, v6Path)
    bench(v4Path)

    println("\n完成!")
    return 0
}
