package ip2region.xdb

import std.fs.*
import std.io.SeekPosition

// Searcher for ip2region xdb database
public class Searcher {
    let version: Version
    let handle: File
    var ioCount: Int64
    let vectorIndex: Array<Byte>
    let contentBuff: Array<Byte>
    let mode: Int64

    public let FileOnlyMode: Int64 = 0
    public let VectorIndexMode: Int64 = 1
    public let ContentBuffMode: Int64 = 2

    public init(ver: Version, dbFile: String) {
        this.version = ver
        this.handle = File(dbFile, Read)
        this.vectorIndex = Array<Byte>(0, repeat: 0)
        this.contentBuff = Array<Byte>(0, repeat: 0)
        this.mode = FileOnlyMode
        this.ioCount = 0
    }

    public init(ver: Version, dbFile: String, vIndex: Array<Byte>) {
        this.version = ver
        this.handle = File(dbFile, Read)
        this.vectorIndex = vIndex
        this.contentBuff = Array<Byte>(0, repeat: 0)
        this.mode = VectorIndexMode
        this.ioCount = 0
    }

    public init(ver: Version, cBuff: Array<Byte>, dbPath: String) {
        this.version = ver
        this.handle = File(dbPath, Read)
        this.vectorIndex = Array<Byte>(0, repeat: 0)
        this.contentBuff = cBuff
        this.mode = ContentBuffMode
        this.ioCount = 0
    }

    public func close() {
        this.handle.close()
    }

    public func getIOCount(): Int64 {
        return this.ioCount
    }

    public func searchByString(ipStr: String): String {
        let ipBytes = parseIP(ipStr)
        return this.search(ipBytes)
    }

    public func search(ip: Array<Byte>): String {
        this.ioCount = 0

        let il0 = Int64(ip[0])
        let il1 = Int64(ip[1])
        let idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize

        var sPtr: Int64 = 0
        var ePtr: Int64 = 0

        if (this.mode == VectorIndexMode) {
            sPtr = readLEUint32(this.vectorIndex, idx)
            ePtr = readLEUint32(this.vectorIndex, idx + 4)
        } else if (this.mode == ContentBuffMode) {
            sPtr = readLEUint32(this.contentBuff, HeaderInfoLength + idx)
            ePtr = readLEUint32(this.contentBuff, HeaderInfoLength + idx + 4)
        } else {
            let buff = Array<Byte>(VectorIndexSize, repeat: 0)
            this.readFromFile(HeaderInfoLength + idx, buff)
            sPtr = readLEUint32(buff, 0)
            ePtr = readLEUint32(buff, 4)
        }

        if (sPtr == 0 || ePtr == 0) {
            return ""
        }

        let segIndexSize = this.version.segmentIndexSize
        let bytes = this.version.bytes
        let dBytes = bytes * 2
        var dataLen: Int64 = 0
        var dataPtr: Int64 = 0

        var l: Int64 = 0
        var h: Int64 = (ePtr - sPtr) / segIndexSize
        let buff = Array<Byte>(segIndexSize, repeat: 0)

        while (l <= h) {
            let m = (l + h) / 2
            let p = sPtr + m * segIndexSize

            this.read(p, buff)
            this.ioCount = this.ioCount + 1

            if (this.compareIP(ip, buff, 0) < 0) {
                h = m - 1
            } else if (this.compareIP(ip, buff, bytes) > 0) {
                l = m + 1
            } else {
                dataLen = readLEUint16(buff, dBytes)
                dataPtr = readLEUint32(buff, dBytes + 2)
                break
            }
        }

        if (dataLen == 0) {
            return ""
        }

        let regionBuff = Array<Byte>(dataLen, repeat: 0)
        this.read(dataPtr, regionBuff)
        return String.fromUtf8(regionBuff)
    }

    func read(offset: Int64, buff: Array<Byte>) {
        if (this.mode == ContentBuffMode) {
            for (i in 0..buff.size) {
                buff[i] = this.contentBuff[offset + i]
            }
        } else {
            this.readFromFile(offset, buff)
            ()
        }
    }

    func readFromFile(offset: Int64, buff: Array<Byte>) {
        this.handle.seek(SeekPosition.Begin(offset))
        this.handle.read(buff)
        ()
    }

    func compareIP(ip: Array<Byte>, entry: Array<Byte>, offset: Int64): Int64 {
        if (this.version.id == IPv4VersionNo) {
            return compareIPv4(ip, entry, offset)
        }
        return compareIPv6(ip, entry, offset)
    }
}
