package ip2region.tests.xdb

import ip2region.xdb.*

@Test
class HeaderTests {
    func makeHeaderBytes(): Array<Byte> {
        let buf = Array<Byte>(256, repeat: 0)
        // version 3, index policy 1, created at, start and end index ptr
        let fields: Array<Byte> = [3, 0, 1, 0, 0xA0, 0x86, 0x01, 0x00, 0xE8, 0x03, 0x00, 0x00, 0xD0, 0x07, 0x00, 0x00]
        fields.copyTo(buf, 0, 0, fields.size)
        buf[16] = 4 // IPv4
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
        buf[16] = 6
        let h = newHeaderFromBytes(buf)
        let ver = versionFromHeader(h)
        if (ver.id != IPv6VersionNo) {
            throw Exception("expected IPv6, got ${ver.name}")
        }
    }

    @TestCase
    func testVersionFromHeader20() {
        let buf = makeHeaderBytes()
        buf[0] = 2
        buf[16] = 0
        let h = newHeaderFromBytes(buf)
        let ver = versionFromHeader(h)
        if (ver.id != IPv4VersionNo) {
            throw Exception("expected IPv4 for v2.0")
        }
    }

    @TestCase
    func testHeaderConstants() {
        if (HeaderInfoLength != 256) {
            throw Exception("expected 256")
        }
        if (VectorIndexRows != 256) {
            throw Exception("expected 256")
        }
        if (VectorIndexSize != 8) {
            throw Exception("expected 8")
        }
        if (IPv4SegmentIndexSize != 14) {
            throw Exception("expected 14")
        }
        if (IPv6SegmentIndexSize != 38) {
            throw Exception("expected 38")
        }
    }
}
