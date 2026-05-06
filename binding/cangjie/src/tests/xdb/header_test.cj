package ip2region.tests.xdb

import ip2region.xdb.*

@Test
class HeaderTests {
    func makeHeaderBytes(): Array<Byte> {
        let buf = Array<Byte>(256, repeat: 0)
        var v: Byte
        v = 3; buf[0] = v; v = 0; buf[1] = v
        v = 1; buf[2] = v; v = 0; buf[3] = v
        v = 0xA0; buf[4] = v; v = 0x86; buf[5] = v; v = 0x01; buf[6] = v; v = 0x00; buf[7] = v
        v = 0xE8; buf[8] = v; v = 0x03; buf[9] = v; v = 0x00; buf[10] = v; v = 0x00; buf[11] = v
        v = 0xD0; buf[12] = v; v = 0x07; buf[13] = v; v = 0x00; buf[14] = v; v = 0x00; buf[15] = v
        v = 4; buf[16] = v; v = 0; buf[17] = v
        v = 0; buf[18] = v; v = 0; buf[19] = v
        return buf
    }

    @TestCase
    func testNewHeaderFromBytes() {
        let buf = makeHeaderBytes()
        let h = newHeaderFromBytes(buf)
        if (h.version != UInt16(3)) {
            throw Exception("expected version 3, got ${h.version}")
        }
        if (h.ipVersion != UInt16(4)) {
            throw Exception("expected ipVersion 4, got ${h.ipVersion}")
        }
    }

    @TestCase
    func testHeaderToString() {
        let buf = makeHeaderBytes()
        let h = newHeaderFromBytes(buf)
        let s = h.toString()
        if (s.size == 0) {
            throw Exception("toString should not be empty")
        }
    }

    @TestCase
    func testVersionFromHeaderV4() {
        let buf = makeHeaderBytes()
        let h = newHeaderFromBytes(buf)
        let v = versionFromHeader(h)
        if (v.id != IPv4VersionNo) {
            throw Exception("expected IPv4, got ${v.name}")
        }
    }

    @TestCase
    func testVersionFromHeaderV6() {
        let buf = makeHeaderBytes()
        var v: Byte = 6; buf[16] = v
        let h = newHeaderFromBytes(buf)
        let ver = versionFromHeader(h)
        if (ver.id != IPv6VersionNo) {
            throw Exception("expected IPv6, got ${ver.name}")
        }
    }

    @TestCase
    func testVersionFromHeader20() {
        let buf = makeHeaderBytes()
        var v: Byte = 2; buf[0] = v
        v = 0; buf[16] = v
        let h = newHeaderFromBytes(buf)
        let ver = versionFromHeader(h)
        if (ver.id != IPv4VersionNo) {
            throw Exception("expected IPv4 for v2.0")
        }
    }

    @TestCase
    func testHeaderConstants() {
        if (HeaderInfoLength != 256) { throw Exception("expected 256") }
        if (VectorIndexRows != 256) { throw Exception("expected 256") }
        if (VectorIndexSize != 8) { throw Exception("expected 8") }
        if (IPv4SegmentIndexSize != 14) { throw Exception("expected 14") }
        if (IPv6SegmentIndexSize != 38) { throw Exception("expected 38") }
    }
}
