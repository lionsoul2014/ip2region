package ip2region.tests.xdb

import ip2region.xdb.*

@Test
class UtilTests {
    @TestCase
    func testParseIPv4() {
        let ip = parseIP("1.2.3.4")
        if (ip.size != 4) {
            throw Exception("expected 4 bytes, got ${ip.size}")
        }
        if (Int64(ip[0]) != 1 || Int64(ip[1]) != 2 || Int64(ip[2]) != 3 || Int64(ip[3]) != 4) {
            throw Exception("IPv4 parse mismatch")
        }
    }

    @TestCase
    func testParseIPv6() {
        let ip = parseIP("::1")
        if (ip.size != 16) {
            throw Exception("expected 16 bytes for IPv6, got ${ip.size}")
        }
    }

    @TestCase
    func testIPCompareEqual() {
        let a = Array<Byte>(4, repeat: 0)
        var v: Byte = 1; a[0] = v; v = 2; a[1] = v; v = 3; a[2] = v; v = 4; a[3] = v
        let b = Array<Byte>(4, repeat: 0)
        v = 1; b[0] = v; v = 2; b[1] = v; v = 3; b[2] = v; v = 4; b[3] = v
        let r = ipCompare(a, b)
        if (r != 0) {
            throw Exception("expected 0, got ${r}")
        }
    }

    @TestCase
    func testIPCompareLess() {
        let a = Array<Byte>(4, repeat: 0)
        var v: Byte = 1; a[0] = v; v = 2; a[1] = v; v = 3; a[2] = v; v = 4; a[3] = v
        let b = Array<Byte>(4, repeat: 0)
        v = 1; b[0] = v; v = 2; b[1] = v; v = 3; b[2] = v; v = 5; b[3] = v
        let r = ipCompare(a, b)
        if (r != -1) {
            throw Exception("expected -1, got ${r}")
        }
    }

    @TestCase
    func testIPCompareGreater() {
        let a = Array<Byte>(4, repeat: 0)
        var v: Byte = 1; a[0] = v; v = 2; a[1] = v; v = 3; a[2] = v; v = 5; a[3] = v
        let b = Array<Byte>(4, repeat: 0)
        v = 1; b[0] = v; v = 2; b[1] = v; v = 3; b[2] = v; v = 4; b[3] = v
        let r = ipCompare(a, b)
        if (r != 1) {
            throw Exception("expected 1, got ${r}")
        }
    }

    @TestCase
    func testReadLEUint32() {
        let buf = Array<Byte>(4, repeat: 0)
        var v: Byte = 0x78; buf[0] = v; v = 0x56; buf[1] = v; v = 0x34; buf[2] = v; v = 0x12; buf[3] = v
        let val = readLEUint32(buf, 0)
        let expected: Int64 = 0x12345678
        if (val != expected) {
            throw Exception("expected 0x12345678, got ${val}")
        }
    }

    @TestCase
    func testReadLEUint16() {
        let buf = Array<Byte>(2, repeat: 0)
        var v: Byte = 0x34; buf[0] = v; v = 0x12; buf[1] = v
        let val = readLEUint16(buf, 0)
        if (val != 0x1234) {
            throw Exception("expected 0x1234, got ${val}")
        }
    }

    @TestCase
    func testIpToString() {
        let ip = Array<Byte>(4, repeat: 0)
        var v: Byte = 127; ip[0] = v; v = 0; ip[1] = v; v = 0; ip[2] = v; v = 1; ip[3] = v
        let s = ipToString(ip)
        if (s != "127.0.0.1") {
            throw Exception("expected 127.0.0.1, got ${s}")
        }
    }

    @TestCase
    func testCompareIPv4() {
        let ip = Array<Byte>(4, repeat: 0)
        var v: Byte = 1; ip[0] = v; v = 2; ip[1] = v; v = 3; ip[2] = v; v = 4; ip[3] = v
        let entry = Array<Byte>(8, repeat: 0)
        v = 4; entry[0] = v; v = 3; entry[1] = v; v = 2; entry[2] = v; v = 1; entry[3] = v
        v = 8; entry[4] = v; v = 7; entry[5] = v; v = 6; entry[6] = v; v = 5; entry[7] = v
        let r = compareIPv4(ip, entry, 0)
        if (r != 0) {
            throw Exception("expected equal, got ${r}")
        }
    }

    @TestCase
    func testCompareIPv6() {
        let ip = Array<Byte>(16, repeat: 0)
        var v: Byte = 0x20; ip[0] = v; v = 0x01; ip[1] = v; v = 0x0d; ip[2] = v; v = 0xb8; ip[3] = v
        let entry = Array<Byte>(16, repeat: 0)
        v = 0x20; entry[0] = v; v = 0x01; entry[1] = v; v = 0x0d; entry[2] = v; v = 0xb8; entry[3] = v
        let r = compareIPv6(ip, entry, 0)
        if (r != 0) {
            throw Exception("expected equal, got ${r}")
        }
    }

    @TestCase
    func testLoadVectorIndex() {
        let viSize = VectorIndexRows * VectorIndexCols * VectorIndexSize
        let cBuff = Array<Byte>(HeaderInfoLength + viSize, repeat: 0)
        var v: Byte = 0x42
        cBuff[HeaderInfoLength] = v
        let vi = loadVectorIndex(cBuff)
        if (vi.size != viSize) {
            throw Exception("expected size ${viSize}, got ${vi.size}")
        }
        let firstVal = Int64(vi[0])
        if (firstVal != 0x42) {
            throw Exception("expected first byte 0x42, got ${firstVal}")
        }
    }
}
