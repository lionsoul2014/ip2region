package ip2region.tests.service

import ip2region.xdb.*
import ip2region.service.*

@Test
class ConfigV4Tests {
    @TestCase
    func testCreateFileOnly() {
        let cfg = Config(FileOnly, IPv4, "../../data/ip2region_v4.xdb", 5)
        if (cfg.cachePolicy != FileOnly) { throw Exception("wrong cache policy") }
        if (cfg.searchers != 5) { throw Exception("wrong searchers count") }
        if (cfg.ipVersion.id != IPv4VersionNo) { throw Exception("wrong version") }
    }

    @TestCase
    func testCreateVectorIndex() {
        let cfg = Config(VectorIndex, IPv4, "../../data/ip2region_v4.xdb", 10)
        if (cfg.vIndex.size == 0) { throw Exception("vIndex should be loaded") }
        let expectedSize = VectorIndexRows * VectorIndexCols * VectorIndexSize
        if (cfg.vIndex.size != expectedSize) {
            throw Exception("vIndex size mismatch: ${cfg.vIndex.size} vs ${expectedSize}")
        }
    }

    @TestCase
    func testCreateContentBuff() {
        let cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 3)
        if (cfg.cBuffer.size == 0) { throw Exception("cBuffer should be loaded") }
    }

    @TestCase
    func testFactoryV4() {
        let cfg = newV4Config(VectorIndex, "../../data/ip2region_v4.xdb", 8)
        if (cfg.ipVersion.id != IPv4VersionNo) { throw Exception("should be IPv4") }
        if (cfg.searchers != 8) { throw Exception("wrong searchers") }
    }

    @TestCase
    func testFactoryV6() {
        let cfg = newV6Config(VectorIndex, "../../data/ip2region_v6.xdb", 8)
        if (cfg.ipVersion.id != IPv6VersionNo) { throw Exception("should be IPv6") }
    }

    @TestCase
    func testFactoryDefaultSearchers() {
        let cfg = newV4Config(ContentBuff, "../../data/ip2region_v4.xdb")
        if (cfg.searchers != 20) { throw Exception("default searchers should be 20") }
    }
}
