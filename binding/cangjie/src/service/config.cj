package ip2region.service

import std.fs.*
import ip2region.xdb.*

// Cache policy constants - match Searcher modes
public let FileOnly: Int64 = 0
public let VectorIndex: Int64 = 1
public let ContentBuff: Int64 = 2

// Config holds xdb file configuration and pre-loaded data for creating Searcher instances
public class Config {
    public let cachePolicy: Int64
    public let ipVersion: Version
    public let xdbPath: String
    public let header: Header
    public let vIndex: Array<Byte>
    public let cBuffer: Array<Byte>
    public let searchers: Int64

    public init(cachePolicy: Int64, ipVersion: Version, xdbPath: String, searchers: Int64) {
        if (searchers <= 0) {
            throw Exception("searchers must be > 0")
        }

        this.cachePolicy = cachePolicy
        this.ipVersion = ipVersion
        this.xdbPath = xdbPath
        this.searchers = searchers

        let content = File.readFrom(Path(xdbPath))
        this.header = newHeaderFromBytes(content)

        // Verify IP version matches
        let detectedVersion = versionFromHeader(this.header)
        if (detectedVersion.id != ipVersion.id) {
            throw Exception("xdb file IP version mismatch: expected ${ipVersion.name}, got ${detectedVersion.name}")
        }

        if (cachePolicy == VectorIndex) {
            this.vIndex = loadVectorIndex(content)
            this.cBuffer = Array<Byte>(0, repeat: 0)
        } else if (cachePolicy == ContentBuff) {
            this.vIndex = Array<Byte>(0, repeat: 0)
            this.cBuffer = content
        } else {
            this.vIndex = Array<Byte>(0, repeat: 0)
            this.cBuffer = Array<Byte>(0, repeat: 0)
        }
    }

    public init(cachePolicy: Int64, ipVersion: Version, xdbPath: String) {
        this(cachePolicy, ipVersion, xdbPath, 20)
    }
}

// Static factories for IPv4 and IPv6
public func newV4Config(cachePolicy: Int64, xdbPath: String, searchers: Int64): Config {
    return Config(cachePolicy, IPv4, xdbPath, searchers)
}

public func newV6Config(cachePolicy: Int64, xdbPath: String, searchers: Int64): Config {
    return Config(cachePolicy, IPv6, xdbPath, searchers)
}

public func newV4Config(cachePolicy: Int64, xdbPath: String): Config {
    return Config(cachePolicy, IPv4, xdbPath, 20)
}

public func newV6Config(cachePolicy: Int64, xdbPath: String): Config {
    return Config(cachePolicy, IPv6, xdbPath, 20)
}
