package ip2region.service

import ip2region.xdb.*

// Ip2Region is the high-level API for searching IP addresses.
// It supports both IPv4 and IPv6 with thread-safe access via SearcherPool.
public class Ip2Region {
    let v4Pool: SearcherPool
    let v6Pool: SearcherPool
    let v4InMemSearcher: Searcher
    let v6InMemSearcher: Searcher
    let hasV4: Bool
    let hasV6: Bool
    let v4IsInMem: Bool
    let v6IsInMem: Bool

    // Create Ip2Region with both v4 and v6 configurations.
    public init(v4Config: Config, v6Config: Config) {
        // v4 setup
        if (v4Config.cachePolicy == ContentBuff) {
            this.hasV4 = true
            this.v4IsInMem = true
            this.v4InMemSearcher = Searcher(v4Config.ipVersion, v4Config.cBuffer, v4Config.xdbPath)
            this.v4Pool = SearcherPool(v4Config)
        } else {
            this.hasV4 = true
            this.v4IsInMem = false
            this.v4InMemSearcher = Searcher(v4Config.ipVersion, Array<Byte>(0, repeat: 0), v4Config.xdbPath)
            this.v4Pool = SearcherPool(v4Config)
        }

        // v6 setup
        if (v6Config.cachePolicy == ContentBuff) {
            this.hasV6 = true
            this.v6IsInMem = true
            this.v6InMemSearcher = Searcher(v6Config.ipVersion, v6Config.cBuffer, v6Config.xdbPath)
            this.v6Pool = SearcherPool(v6Config)
        } else {
            this.hasV6 = true
            this.v6IsInMem = false
            this.v6InMemSearcher = Searcher(v6Config.ipVersion, Array<Byte>(0, repeat: 0), v6Config.xdbPath)
            this.v6Pool = SearcherPool(v6Config)
        }
    }

    // Single-version: provide Config and IP version.
    // Examples:
    //   Ip2Region(v4Config, IPv4)  — IPv4 only
    //   Ip2Region(v6Config, IPv6)  — IPv6 only
    public init(config: Config, ipVersion: Version) {
        if (ipVersion.id == IPv4VersionNo) {
            this.hasV4 = true
            this.v4IsInMem = (config.cachePolicy == ContentBuff)
            if (config.cachePolicy == ContentBuff) {
                this.v4InMemSearcher = Searcher(config.ipVersion, config.cBuffer, config.xdbPath)
            } else {
                this.v4InMemSearcher = Searcher(config.ipVersion, Array<Byte>(0, repeat: 0), config.xdbPath)
            }
            this.v4Pool = SearcherPool(config)
            // IPv6 disabled
            this.hasV6 = false
            this.v6IsInMem = false
            this.v6InMemSearcher = Searcher(IPv4, Array<Byte>(0, repeat: 0), config.xdbPath)
            this.v6Pool = SearcherPool(Config(ContentBuff, IPv4, config.xdbPath, 1))
        } else {
            this.hasV6 = true
            this.v6IsInMem = (config.cachePolicy == ContentBuff)
            if (config.cachePolicy == ContentBuff) {
                this.v6InMemSearcher = Searcher(config.ipVersion, config.cBuffer, config.xdbPath)
            } else {
                this.v6InMemSearcher = Searcher(config.ipVersion, Array<Byte>(0, repeat: 0), config.xdbPath)
            }
            this.v6Pool = SearcherPool(config)
            // IPv4 disabled
            this.hasV4 = false
            this.v4IsInMem = false
            this.v4InMemSearcher = Searcher(IPv6, Array<Byte>(0, repeat: 0), config.xdbPath)
            this.v4Pool = SearcherPool(Config(ContentBuff, IPv6, config.xdbPath, 1))
        }
    }

    // Search an IP string and return the region info.
    public func search(ipStr: String): String {
        try {
            let ipBytes = parseIP(ipStr)

            if (ipBytes.size == 4) {
                return this.searchV4(ipBytes)
            } else if (ipBytes.size == 16) {
                return this.searchV6(ipBytes)
            }
        } catch (_) {
            return "Invalid IP"
        }
        return "Invalid IP"
    }

    func searchV4(ip: Array<Byte>): String {
        if (!this.hasV4) {
            return ""
        }
        if (this.v4IsInMem) {
            return this.v4InMemSearcher.search(ip)
        }
        let searcher = this.v4Pool.borrow()
        let region = searcher.search(ip)
        this.v4Pool.return_(searcher)
        return region
    }

    func searchV6(ip: Array<Byte>): String {
        if (!this.hasV6) {
            return ""
        }
        if (this.v6IsInMem) {
            return this.v6InMemSearcher.search(ip)
        }
        let searcher = this.v6Pool.borrow()
        let region = searcher.search(ip)
        this.v6Pool.return_(searcher)
        return region
    }

    // close releases all resources.
    public func close() {
        if (this.hasV4) {
            this.v4Pool.close()
        }
        if (this.hasV6) {
            this.v6Pool.close()
        }
    }

    // closeTimeout releases all resources with the specified timeout.
    public func closeTimeout(timeout: Duration) {
        if (this.hasV4) {
            this.v4Pool.closeTimeout(timeout)
        }
        if (this.hasV6) {
            this.v6Pool.closeTimeout(timeout)
        }
    }
}

// Create an Ip2Region instance using xdb file paths.
// Uses VectorIndex cache policy with 20 searchers by default.
public func newIp2Region(v4XdbPath: String, v6XdbPath: String): Ip2Region {
    let v4Config = Config(VectorIndex, IPv4, v4XdbPath, 20)
    let v6Config = Config(VectorIndex, IPv6, v6XdbPath, 20)
    return Ip2Region(v4Config, v6Config)
}

// Create an IPv4-only Ip2Region service.
public func newIp2RegionV4(config: Config): Ip2Region {
    return Ip2Region(config, IPv4)
}

// Create an IPv6-only Ip2Region service.
public func newIp2RegionV6(config: Config): Ip2Region {
    return Ip2Region(config, IPv6)
}
