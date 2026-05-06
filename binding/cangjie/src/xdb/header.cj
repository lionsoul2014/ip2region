package ip2region.xdb

// xdb file format constants
public let HeaderInfoLength: Int64 = 256
public let VectorIndexRows: Int64 = 256
public let VectorIndexCols: Int64 = 256
public let VectorIndexSize: Int64 = 8
public let IPv4SegmentIndexSize: Int64 = 14  // 4 + 4 + 2 + 4
public let IPv6SegmentIndexSize: Int64 = 38  // 16 + 16 + 2 + 4

public let Structure20: Int64 = 2
public let Structure30: Int64 = 3

// xdb header parsed from the first 256 bytes of the xdb file
public class Header {
    public let version: UInt16
    public let indexPolicy: UInt16
    public let createdAt: UInt32
    public let startIndexPtr: UInt32
    public let endIndexPtr: UInt32
    public let ipVersion: UInt16
    public let runtimePtrBytes: UInt16

    public init(
        ver: UInt16,
        idxPolicy: UInt16,
        createAt: UInt32,
        sPtr: UInt32,
        ePtr: UInt32,
        ipVer: UInt16,
        rtPtrBytes: UInt16
    ) {
        this.version = ver
        this.indexPolicy = idxPolicy
        this.createdAt = createAt
        this.startIndexPtr = sPtr
        this.endIndexPtr = ePtr
        this.ipVersion = ipVer
        this.runtimePtrBytes = rtPtrBytes
    }

    public func toString(): String {
        return "{version: ${this.version}, index_policy: ${this.indexPolicy}, " +
            "created_at: ${this.createdAt}, start_index_ptr: ${this.startIndexPtr}, " +
            "end_index_ptr: ${this.endIndexPtr}, ip_version: ${this.ipVersion}, " +
            "runtime_ptr_bytes: ${this.runtimePtrBytes}}"
    }
}

// Read a little-endian UInt16 from byte array at offset
func readHeaderUInt16(buf: Array<Byte>, offset: Int64): UInt16 {
    let b0 = UInt16(buf[offset])
    let b1 = UInt16(buf[offset + 1]) << 8
    return b0 | b1
}

// Read a little-endian UInt32 from byte array at offset
func readHeaderUInt32(buf: Array<Byte>, offset: Int64): UInt32 {
    let b0 = UInt32(buf[offset])
    let b1 = UInt32(buf[offset + 1]) << 8
    let b2 = UInt32(buf[offset + 2]) << 16
    let b3 = UInt32(buf[offset + 3]) << 24
    return b0 | b1 | b2 | b3
}

// Create a Header from raw byte buffer (first 256 bytes of xdb file)
public func newHeaderFromBytes(buf: Array<Byte>): Header {
    return Header(
        readHeaderUInt16(buf, 0),
        readHeaderUInt16(buf, 2),
        readHeaderUInt32(buf, 4),
        readHeaderUInt32(buf, 8),
        readHeaderUInt32(buf, 12),
        readHeaderUInt16(buf, 16),
        readHeaderUInt16(buf, 18)
    )
}
