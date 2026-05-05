package ip2region.xdb

// IP version definitions for ip2region xdb searcher
public class Version {
    public let id: Int64
    public let name: String
    public let bytes: Int64
    public let segmentIndexSize: Int64

    public init(id: Int64, name: String, bytes: Int64, segmentIndexSize: Int64) {
        this.id = id
        this.name = name
        this.bytes = bytes
        this.segmentIndexSize = segmentIndexSize
    }

    public func toString(): String {
        return "{id: ${this.id}, name: ${this.name}, bytes: ${this.bytes}, " +
            "segment_index_size: ${this.segmentIndexSize}}"
    }
}

public let IPv4VersionNo: Int64 = 4
public let IPv6VersionNo: Int64 = 6

// Pre-defined IP versions
public let IPv4 = Version(4, "IPv4", 4, 14)
public let IPv6 = Version(6, "IPv6", 16, 38)

// Determine IP version from header info
public func versionFromHeader(header: Header): Version {
    // Old structure (2.0) with IPv4 only
    if (Int64(header.version) == Structure20) {
        return IPv4
    }
    // Structure 3.0+
    if (Int64(header.ipVersion) == IPv4VersionNo) {
        return IPv4
    }
    return IPv6
}
