package ip2region.tests.xdb

import std.fs.*
import ip2region.xdb.*

@Test
class SearcherV4Tests {
    var v4Content: Array<Byte> = Array<Byte>(0, repeat: 0)
    var initialized: Bool = false

    @BeforeAll
    func setup() {
        this.v4Content = File.readFrom(Path("../../data/ip2region_v4.xdb"))
        this.initialized = true
    }

    @TestCase
    func testSearchContentMode() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v4Content)
        let version = versionFromHeader(header)
        let searcher = Searcher(version, this.v4Content, "../../data/ip2region_v4.xdb")
        let ip = parseIP("220.181.108.183")
        let region = searcher.search(ip)
        if (region.size == 0) {
            throw Exception("should find region for known IP")
        }
        searcher.close()
    }

    @TestCase
    func testSearchVectorIndexMode() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v4Content)
        let version = versionFromHeader(header)
        let vIndex = loadVectorIndex(this.v4Content)
        let searcher = Searcher(version, "../../data/ip2region_v4.xdb", vIndex)
        let ip = parseIP("220.181.108.183")
        let region = searcher.search(ip)
        if (region.size == 0) {
            throw Exception("should find region for known IP")
        }
        searcher.close()
    }

    @TestCase
    func testSearchFileMode() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v4Content)
        let version = versionFromHeader(header)
        let searcher = Searcher(version, "../../data/ip2region_v4.xdb")
        let ip = parseIP("220.181.108.183")
        let region = searcher.search(ip)
        if (region.size == 0) {
            throw Exception("should find region for known IP")
        }
        searcher.close()
    }

    @TestCase
    func testAllModesSameResult() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v4Content)
        let version = versionFromHeader(header)
        let vIndex = loadVectorIndex(this.v4Content)
        let s1 = Searcher(version, this.v4Content, "../../data/ip2region_v4.xdb")
        let s2 = Searcher(version, "../../data/ip2region_v4.xdb", vIndex)
        let s3 = Searcher(version, "../../data/ip2region_v4.xdb")
        let ip = parseIP("1.2.3.4")
        let r1 = s1.search(ip)
        let r2 = s2.search(ip)
        let r3 = s3.search(ip)
        s1.close(); s2.close(); s3.close()
        if (r1 != r2 || r2 != r3) {
            throw Exception("all modes should return same result")
        }
    }

    @TestCase
    func testSearchByString() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v4Content)
        let version = versionFromHeader(header)
        let searcher = Searcher(version, this.v4Content, "../../data/ip2region_v4.xdb")
        let region = searcher.searchByString("220.181.108.183")
        if (region.size == 0) {
            throw Exception("searchByString should find region")
        }
        searcher.close()
    }
}

@Test
class SearcherV6Tests {
    var v6Content: Array<Byte> = Array<Byte>(0, repeat: 0)
    var initialized: Bool = false

    @BeforeAll
    func setup() {
        this.v6Content = File.readFrom(Path("../../data/ip2region_v6.xdb"))
        this.initialized = true
    }

    @TestCase
    func testSearchV6ContentMode() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v6Content)
        let version = versionFromHeader(header)
        let searcher = Searcher(version, this.v6Content, "../../data/ip2region_v6.xdb")
        let ip = parseIP("2408:8266:100:1000::")
        let region = searcher.search(ip)
        if (region.size == 0) {
            throw Exception("should find region for known IPv6")
        }
        searcher.close()
    }

    @TestCase
    func testSearchV6VectorIndexMode() {
        if (!this.initialized) { return }
        let header = newHeaderFromBytes(this.v6Content)
        let version = versionFromHeader(header)
        let vIndex = loadVectorIndex(this.v6Content)
        let searcher = Searcher(version, "../../data/ip2region_v6.xdb", vIndex)
        let ip = parseIP("2408:8266:100:1000::")
        let region = searcher.search(ip)
        if (region.size == 0) {
            throw Exception("should find region for known IPv6")
        }
        searcher.close()
    }
}
