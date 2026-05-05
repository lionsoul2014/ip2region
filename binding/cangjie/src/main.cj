package ip2region

import std.fs.*
import std.time.*
import std.env.*
import ip2region.xdb.*

func printHelp() {
    println("ip2region xdb searcher")
    println("Usage: ip2region [command] [command options]")
    println("Command:")
    println("  search    search input test")
    println("  bench     search bench test")
}

func runSearch(args: Array<String>) {
    var v4DbPath = "../../data/ip2region_v4.xdb"
    var v6DbPath = "../../data/ip2region_v6.xdb"
    var v4CachePolicy = "vectorIndex"
    var v6CachePolicy = "vectorIndex"
    var showHelp = false

    var i = 1 // skip command name
    while (i < args.size) {
        let arg = args[i]
        if (arg == "--v4-db" && i + 1 < args.size) {
            v4DbPath = args[i + 1]
            i = i + 2
        } else if (arg == "--v6-db" && i + 1 < args.size) {
            v6DbPath = args[i + 1]
            i = i + 2
        } else if (arg == "--v4-cache-policy" && i + 1 < args.size) {
            v4CachePolicy = args[i + 1]
            i = i + 2
        } else if (arg == "--v6-cache-policy" && i + 1 < args.size) {
            v6CachePolicy = args[i + 1]
            i = i + 2
        } else if (arg == "--help") {
            showHelp = true
            i = i + 1
        } else {
            i = i + 1
        }
    }

    if (showHelp) {
        println("ip2region search [command options]")
        println("options:")
        println(" --v4-db <path>            ip2region v4 binary xdb file path")
        println(" --v4-cache-policy <mode>  v4 cache policy: file/vectorIndex/content")
        println(" --v6-db <path>            ip2region v6 binary xdb file path")
        println(" --v6-cache-policy <mode>  v6 cache policy: file/vectorIndex/content")
        println(" --help                    print this help")
        return
    }

    // Load v4 xdb
    print("Loading v4 xdb ... ")
    let v4Content = File.readFrom(Path(v4DbPath))
    let v4Header = newHeaderFromBytes(v4Content)
    let v4Version = versionFromHeader(v4Header)
    let v4Searcher = createSearcher(v4Version, v4Content, v4DbPath, v4CachePolicy)
    println("done")

    // Load v6 xdb
    print("Loading v6 xdb ... ")
    let v6Content = File.readFrom(Path(v6DbPath))
    let v6Header = newHeaderFromBytes(v6Content)
    let v6Version = versionFromHeader(v6Header)
    let v6Searcher = createSearcher(v6Version, v6Content, v6DbPath, v6CachePolicy)
    println("done")

    println("ip2region search service test program")
    println("type 'quit' to exit")

    let reader = getStdIn()
    while (true) {
        print("ip2region>> ")
        let lineOpt = reader.readln()
        if (lineOpt == None) {
            break
        }
        let line = lineOpt.getOrThrow()
        if (line == "quit" || line == "exit") {
            break
        }
        if (line.size == 0) {
            continue
        }

        let start = MonoTime.now()
        let region = searchIP(line, v4Searcher, v6Searcher)
        let elapsed = MonoTime.now() - start
        println("${line} -> ${region}  (took: ${elapsed.toMicroseconds()} us)")
    }

    v4Searcher.close()
    v6Searcher.close()
}

func runBench(args: Array<String>) {
    var dbFile = ""
    var srcFile = ""
    var cachePolicy = "content"

    var i = 1 // skip command name
    while (i < args.size) {
        let arg = args[i]
        if (arg == "--db" && i + 1 < args.size) {
            dbFile = args[i + 1]
            i = i + 2
        } else if (arg == "--src" && i + 1 < args.size) {
            srcFile = args[i + 1]
            i = i + 2
        } else if (arg == "--cache-policy" && i + 1 < args.size) {
            cachePolicy = args[i + 1]
            i = i + 2
        } else {
            i = i + 1
        }
    }

    if (dbFile == "" || srcFile == "") {
        println("ip2region bench [command options]")
        println("options:")
        println(" --db <path>              ip2region binary xdb file path")
        println(" --src <path>             source ip text file path")
        println(" --cache-policy <mode>    cache policy: file/vectorIndex/content")
        return
    }

    println("Loading xdb from: ${dbFile}")
    let content = File.readFrom(Path(dbFile))
    let header = newHeaderFromBytes(content)
    let version = versionFromHeader(header)
    let searcher = createSearcher(version, content, dbFile, cachePolicy)

    println("Loading source data from: ${srcFile}")
    let srcContent = File.readFrom(Path(srcFile))
    let srcStr = String.fromUtf8(srcContent)
    let lines = srcStr.split("\n")

    var count: Int64 = 0
    var totalCost: Int64 = 0
    let tStart = MonoTime.now()

    for (line in lines) {
        let trimmed = line.trimAsciiStart().trimAsciiEnd()
        if (trimmed.size == 0) {
            continue
        }

        let parts = trimmed.split("|")
        if (parts.size < 3) {
            continue
        }

        let ipBytes = parseIP(parts[0])
        let t0 = MonoTime.now()
        let _ = searcher.search(ipBytes)
        let t1 = MonoTime.now()
        totalCost = totalCost + (t1 - t0).toNanoseconds()
        count = count + 1

        if (count % 10000 == 0) {
            print(".")
        }
    }

    let tEnd = MonoTime.now()
    let totalTime = tEnd - tStart

    println("")
    println("Bench finished:")
    println("  cachePolicy: ${cachePolicy}")
    println("  total: ${count}")
    println("  took: ${totalTime.toMilliseconds()} ms")
    if (count > 0) {
        println("  avg: ${totalCost / count / 1000} us/op")
    }

    searcher.close()
}

func createSearcher(version: Version, content: Array<Byte>, dbPath: String, cachePolicy: String): Searcher {
    if (cachePolicy == "content") {
        println("Using content buffer mode")
        return Searcher(version, content, dbPath)
    } else if (cachePolicy == "vectorIndex") {
        println("Using vector index mode")
        let vIndex = loadVectorIndex(content)
        return Searcher(version, dbPath, vIndex)
    } else {
        println("Using file only mode")
        return Searcher(version, dbPath)
    }
}

func searchIP(ipStr: String, v4Searcher: Searcher, v6Searcher: Searcher): String {
    let ipBytes = parseIP(ipStr)
    if (ipBytes.size == 4) {
        return v4Searcher.search(ipBytes)
    } else if (ipBytes.size == 16) {
        return v6Searcher.search(ipBytes)
    }
    return "Invalid IP"
}

main(args: Array<String>): Int64 {
    if (args.size == 0) {
        printHelp()
        return 0
    }

    let cmd = args[0]
    if (cmd == "search") {
        runSearch(args)
    } else if (cmd == "bench") {
        runBench(args)
    } else {
        printHelp()
    }

    return 0
}
