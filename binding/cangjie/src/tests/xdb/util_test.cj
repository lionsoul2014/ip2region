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
        let a: Array<Byte> = [1, 2, 3, 4]
        let b: Array<Byte> = [1, 2, 3, 4]
        let r = ipCompare(a, b)
        if (r != 0) {
            throw Exception("expected 0, got ${r}")
        }
    }

    @TestCase
    func testIPCompareLess() {
        let a: Array<Byte> = [1, 2, 3, 4]
        let b: Array<Byte> = [1, 2, 3, 5]
        let r = ipCompare(a, b)
        if (r != -1) {
            throw Exception("expected -1, got ${r}")
        }
    }

    @TestCase
    func testIPCompareGreater() {
        let a: Array<Byte> = [1, 2, 3, 5]
        let b: Array<Byte> = [1, 2, 3, 4]
        let r = ipCompare(a, b)
        if (r != 1) {
            throw Exception("expected 1, got ${r}")
        }
    }

    @TestCase
    func testReadLEUint32() {
        let buf: Array<Byte> = [0x78, 0x56, 0x34, 0x12]
        let val = readLEUint32(buf, 0)
        let expected: Int64 = 0x12345678
        if (val != expected) {
            throw Exception("expected 0x12345678, got ${val}")
        }
    }

    @TestCase
    func testReadLEUint16() {
        let buf: Array<Byte> = [0x34, 0x12]
        let val = readLEUint16(buf, 0)
        if (val != 0x1234) {
            throw Exception("expected 0x1234, got ${val}")
        }
    }

    @TestCase
    func testIpToString() {
        let ip: Array<Byte> = [127, 0, 0, 1]
        let s = ipToString(ip)
        if (s != "127.0.0.1") {
            throw Exception("expected 127.0.0.1, got ${s}")
        }
    }

    @TestCase
    func testCompareIPv4() {
        let ip: Array<Byte> = [1, 2, 3, 4]
        let entry: Array<Byte> = [4, 3, 2, 1, 8, 7, 6, 5]
        let r = compareIPv4(ip, entry, 0)
        if (r != 0) {
            throw Exception("expected equal, got ${r}")
        }
    }

    @TestCase
    func testCompareIPv6() {
        let ip: Array<Byte> = [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        let entry = ip.clone()
        let r = compareIPv6(ip, entry, 0)
        if (r != 0) {
            throw Exception("expected equal, got ${r}")
        }
    }

    @TestCase
    func testLoadVectorIndex() {
        let viSize = VectorIndexRows * VectorIndexCols * VectorIndexSize
        let cBuff = Array<Byte>(HeaderInfoLength + viSize, repeat: 0)
        cBuff[HeaderInfoLength] = 0x42
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
