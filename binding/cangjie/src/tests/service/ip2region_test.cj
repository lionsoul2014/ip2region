package ip2region.tests.service

import ip2region.xdb.*
import ip2region.service.*

@Test
class Ip2RegionBasicTests {
    @TestCase
    func testSearchWithContentBuff() {
        let v4Cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 1)
        let v6Cfg = Config(ContentBuff, IPv6, "../../data/ip2region_v6.xdb", 1)
        let region = Ip2Region(v4Cfg, v6Cfg)
        let r1 = region.search("220.181.108.183")
        if (r1.size == 0) {
            throw Exception("v4 search should return result")
        }
        let r2 = region.search("2408:8266:100:1000::")
        if (r2.size == 0) {
            throw Exception("v6 search should return result")
        }
        region.close()
    }

    @TestCase
    func testSearchWithVectorIndex() {
        let v4Cfg = Config(VectorIndex, IPv4, "../../data/ip2region_v4.xdb", 5)
        let v6Cfg = Config(VectorIndex, IPv6, "../../data/ip2region_v6.xdb", 5)
        let region = Ip2Region(v4Cfg, v6Cfg)
        let r = region.search("220.181.108.183")
        if (r.size == 0) {
            throw Exception("v4 search should return result")
        }
        region.close()
    }

    @TestCase
    func testInvalidIP() {
        let v4Cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 1)
        let v6Cfg = Config(ContentBuff, IPv6, "../../data/ip2region_v6.xdb", 1)
        let region = Ip2Region(v4Cfg, v6Cfg)
        let r = region.search("invalid")
        if (r != "Invalid IP") {
            throw Exception("expected 'Invalid IP', got '${r}'")
        }
        region.close()
    }

    @TestCase
    func testClose() {
        let v4Cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 1)
        let v6Cfg = Config(ContentBuff, IPv6, "../../data/ip2region_v6.xdb", 1)
        let region = Ip2Region(v4Cfg, v6Cfg)
        region.search("220.181.108.183")
        region.close()
    }

    @TestCase
    func testV4OnlyInit() {
        let cfg = Config(VectorIndex, IPv4, "../../data/ip2region_v4.xdb", 3)
        let region = Ip2Region(cfg, IPv4)
        let r = region.search("220.181.108.183")
        if (r.size == 0) {
            throw Exception("v4-only region should find v4 IP")
        }
        let r6 = region.search("2408:8266:100:1000::")
        if (r6.size != 0) {
            throw Exception("v4-only region should not find v6 IP")
        }
        region.close()
    }

    @TestCase
    func testV6OnlyInit() {
        let cfg = Config(VectorIndex, IPv6, "../../data/ip2region_v6.xdb", 3)
        let region = Ip2Region(cfg, IPv6)
        let r = region.search("2408:8266:100:1000::")
        if (r.size == 0) {
            throw Exception("v6-only region should find v6 IP")
        }
        let r4 = region.search("220.181.108.183")
        if (r4.size != 0) {
            throw Exception("v6-only region should not find v4 IP")
        }
        region.close()
    }

    @TestCase
    func testNewV4Factory() {
        let cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 2)
        let region = newIp2RegionV4(cfg)
        let r = region.search("220.181.108.183")
        if (r.size == 0) {
            throw Exception("newIp2RegionV4 should work")
        }
        region.close()
    }

    @TestCase
    func testNewV6Factory() {
        let cfg = Config(ContentBuff, IPv6, "../../data/ip2region_v6.xdb", 2)
        let region = newIp2RegionV6(cfg)
        let r = region.search("2408:8266:100:1000::")
        if (r.size == 0) {
            throw Exception("newIp2RegionV6 should work")
        }
        region.close()
    }
}
