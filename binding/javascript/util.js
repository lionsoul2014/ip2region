// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util functions
// @Author Lion <chenxin619315@gmail.com>

import fs from 'fs';

export const XdbStructure20 = 2;
export const XdbStructure30 = 3;
export const XdbIPv4Id = 4;
export const XdbIPv6Id = 6;

export const HeaderInfoLength = 256;
export const VectorIndexRows  = 256;
export const VectorIndexCols  = 256;
export const VectorIndexSize  = 8;

export class Header {
    constructor(buff) {
        this.version = buff.readUInt16LE(0);
        this.indexPolicy = buff.readUInt16LE(2);
        this.createdAt = buff.readUInt32LE(4);
        this.startIndexPtr = buff.readUInt32LE(8);
        this.endIndexPtr = buff.readUInt32LE(12);

        // since IPv6 supporting
        this.ipVersion = buff.readUInt16LE(16);
        this.runtimePtrBytes = buff.readUInt16LE(18);

        // keep the raw data
        this.buff = buff;
    }

    toString() {
        return `{
            "version":${this.version}, 
            "index_policy":${this.indexPolicy},
            "start_index_ptr": ${this.startIndexPtr},
            "end_index_ptr": ${this.endIndexPtr},
            "ipVersion": ${this.ipVersion},
            "runtime_ptr_bytes": ${this.runtimePtrBytes}
        }`;
    }
}

// ---

// parse ipv4 address
function _parse_ipv4_addr(v4String) {
    let ps = v4String.split('.', 4);
    if (ps.length != 4) {
        throw new Error('invalid ipv4 address');
    }

    var v;
    const ipBytes =  Buffer.alloc(4);
    for (var i = 0; i < ps.length; i++) {
        v = parseInt(ps[i], 10);
        if (isNaN(v)) {
            throw new Error(`invalid ipv4 part '${ps[i]}', a valid number expected`);
        }

        if (v < 0 || v > 255) {
            throw new Error(`invalid ipv4 part '${ps[i]}' should >= 0 and <= 255`);
        }

        ipBytes[i] = (v & 0xFF);
    }

    return ipBytes;
}

// parse ipv6 address
function _parse_ipv6_addr(v6String) {
    let ps = v6String.split(':', 8);
    if (ps.length < 3) {
        throw new Error('invalid ipv6 address');
    }

    let dc_num = 0, offset = 0;
    const ipBytes = Buffer.alloc(16);
    for (var i = 0; i < ps.length; i++) {
        let s = ps[i].trim();

        // Double colon check and auto padding
        if (s.length == 0) {
            // ONLY one double colon allow
            if (dc_num > 0) {
                throw new Error('invalid ipv6 address: multi double colon detected');
            }

            let start = i, mi = ps.length - 1;
            // clear all the consecutive spaces
            for (i++;;) {
                s = ps[i].trim();
                if (s.length > 0) {
                    i--;
                    break;
                }

                if (i >= mi) {
                    break;
                }

                i++;
            }

            dc_num = 1;
            // padding = 8 - start - left
            let padding = 8 - start - (mi - i);
            offset += 2 * padding;
            continue;
        }

        let v = parseInt(s, 16);
        if (isNaN(v)) {
            throw new Error(`invalid ipv6 part '${ps[i]}', a valid hex number expected`);
        }

        if (v < 0 || v > 0xFFFF) {
            throw new Error(`invalid ipv6 part '${ps[i]}' should >= 0 and <= 65534`);
        }

        ipBytes.writeUint16BE(v, offset);
        offset += 2;
    }

    return ipBytes;
}


// parse the specified string ip and return its bytes
// @param  ip string
// @return Buffer
export function parseIP(ipString) {
    let sDot = ipString.indexOf('.');
    let cDot = ipString.indexOf(':');
    if (sDot > -1 && cDot == -1) {
        return _parse_ipv4_addr(ipString);
    } else if (cDot > -1) {
        return _parse_ipv6_addr(ipString);
    } else {
        throw new Error(`invalid ip address '${ipString}'`);
    }
}

// ---

// ipv4 bytes to string
function _ipv4_to_string(v4Bytes) {
    return v4Bytes.join('.');
}

// ipv6 bytes to string
function _ipv6_to_string(v6Bytes, compress) {
    let ps = [], needCompress = false;
    let lastHex = -1, hex = 0;
    for (var i = 0; i < v6Bytes.length; i += 2) {
        hex = v6Bytes.readUint16BE(i).toString(16);
        ps.push(hex);
     
        // check the necessity for compress
        if (lastHex > -1 
            && hex == 0 && lastHex == 0) {
            needCompress = true;
        }

        // reset the last hex
        lastHex = hex;
    }

    if (needCompress == false || compress === false) {
        return ps.join(':');
    }

    // auto compression of consecutive zero
    let _ = [], mi = ps.length - 1;
    for (i = 0; i < ps.length; i++) {
        if (i >= mi) {
            _.push(ps[i]);
            continue;
        }

        if (ps[i] != '0' || ps[i+1] != '0') {
            _.push(ps[i]);
            continue;
        }

        // find the first two zero part
        // and keep find all the zero part
        for (i += 2; i < ps.length;) {
            if (ps[i] != '0') {
                i--;
                break;
            }
            i++;
        }

        // make sure there is an empty head.
        if (_.length == 0) {
            _.push('');
        }

        _.push(''); // empty for double colon

        // make sure there is an empty tail
        if (i == ps.length && _.length < ps.length) {
            _.push('');
        }
    }

    return _.join(':');
}

// bytes ip to humen-readable string ip
export function ipToString(ipBytes, compress) {
    if (!Buffer.isBuffer(ipBytes)) {
        throw new Error('invalid bytes ip, not a Buffer');
    }

    if (ipBytes.length == 4) {
        return _ipv4_to_string(ipBytes, compress);
    } else if (ipBytes.length == 16) {
        return _ipv6_to_string(ipBytes, compress);
    } else {
        throw new Error('invalid bytes ip with length not 4 or 16');
    }
}

export function ipBytesString(ipBytes) {
    if (!Buffer.isBuffer(ipBytes)) {
        throw new Error('invalid bytes ip, not a Buffer');
    }

    let ps = [];
    for (var i = 0; i < ipBytes.length; i++) {
        ps.push(ipBytes[i] & 0xFF);
    }

    return ps.join('.');
}

// compare two byte ips
// ip2 = buff[offset:ip1.length]
// returns: -1 if ip1 < ip2, 1 if ip1 > ip2 or 0
export function ipSubCompare(ip1, buff, offset) {
    return ip1.compare(buff, offset, offset + ip1.length);
}

export function ipCompare(ip1, ip2) {
    return ipSubCompare(ip1, ip2, 0);
}

// ---

export class Version {
    constructor(id, name, bytes, indexSize, ipCompareFunc) {
        this.id = id;
        this.name = name;
        this.bytes = bytes;
        this.indexSize = indexSize;
        this.ipCompareFunc = ipCompareFunc;
    }

    ipCompare(ip1, ip2) {
        return this.ipCompareFunc(ip1, ip2, 0);
    }

    ipSubCompare(ip1, ip2, offset) {
        return this.ipCompareFunc(ip1, ip2, offset);
    }

    toString() {
        return `{"id": ${this.id}, "name": "${this.name}", "bytes":${this.bytes}, "index_size": ${this.indexSize}}`;
    }
}

// 14 = 4 + 4 + 2 + 4
export const IPv4 = new Version(XdbIPv4Id, "IPv4", 4, 14, function(ip1, buff, offset){
    // ip1: Big endian byte order parsed from input
    // ip2: Little endian byte order read from xdb index.
    // @Note: to compatible with the old Litten endian index encode implementation.
    let i, j = offset + ip1.length - 1;
    for (i = 0; i < ip1.length; i++, j--) {
        const i1 = ip1[i] & 0xFF;
        const i2 = buff[j] & 0xFF;
        if (i1 < i2) {
            return -1;
        }

        if (i1 > i2) {
            return 1;
        }
    }

    return 0;
});

// 38 = 16 + 16 + 2 + 4
export const IPv6 = new Version(XdbIPv6Id, "IPv6", 16, 38, ipSubCompare);

export function versionFromName(name) {
    let n = name.toUpperCase();
    if (n == "V4" || n == "IPV4") {
        return IPv4;
    } else if (n == "V6" || n == "IPV6") {
        return IPv6;
    } else {
        return null;
    }
}

export function versionFromHeader(h) {
    // old structure with ONLY IPv4 supporting
    if (h.version == XdbStructure20) {
        return IPv4;
    }

    // structure 3.0 with IPv6 supporting
    if (h.version != XdbStructure30) {
        return null;
    }

    let ipVer = h.ipVersion;
    if (ipVer == XdbIPv4Id) {
        return IPv4;
    } else if (ipVer == XdbIPv6Id) {
        return IPv6;
    } else {
        return null;
    }
}

// ---

export function loadHeader(fd) {
    const buffer = Buffer.alloc(HeaderInfoLength);
    const rBytes = fs.readSync(fd, buffer, 0, HeaderInfoLength, 0);
    if (rBytes != HeaderInfoLength) {
        throw new Error(`incomplete read (${rBytes} read, ${header.HeaderInfoLength} expected)`);
    }
    return new Header(buffer);
}

export function loadHeaderFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const header = loadHeader(fd);
    fs.closeSync(fd);
    return header;
}

export function loadVectorIndex(fd) {
    const vBytes = VectorIndexCols * VectorIndexRows * VectorIndexSize;
    const buffer = Buffer.alloc(vBytes);
    const rBytes = fs.readSync(fd, buffer, 0, vBytes, HeaderInfoLength);
    if (rBytes != vBytes) {
        throw new Error(`incomplete read (${rBytes} read, ${vBytes} expected)`);
    }
    return buffer;
}

export function loadVectorIndexFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const vIndex = loadVectorIndex(fd);
    fs.closeSync(fd);
    return vIndex;
}

export function loadContent(fd) {
    const stats = fs.fstatSync(fd);
    const buffer = Buffer.alloc(stats.size);
    const rBytes = fs.readSync(fd, buffer, 0, buffer.length, 0);
    if (rBytes != stats.size) {
        throw new Error(`incomplete read (${rBytes} read, ${stats.size} expected)`);
    }
    return buffer;
}

export function loadContentFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const content = loadContent(fd);
    fs.closeSync(fd);
    return content;
}

// --- 

// Verify if the current Searcher could be used to search the specified xdb file.
// Why do we need this check ?
// The future features of the xdb impl may cause the current searcher not able to work properly.
//
// @Note: You Just need to check this ONCE when the service starts
// Or use another process (eg, A command) to check once Just to confirm the suitability.
export function verify(fd) {
    const header = loadHeader(fd);

    // get the runtime ptr bytes
    let runtimePtrBytes = 0;
    if (header.version == XdbStructure20) {
        runtimePtrBytes = 4;
    } else if (header.version == XdbStructure30) {
        runtimePtrBytes = header.runtimePtrBytes;
    } else {
        throw new Error(`invalid structure version ${header.version}`);
    }

    // 1, confirm the xdb file size
    // to ensure that the maximum file pointer does not overflow
    const maxFilePtr = (1n << BigInt(runtimePtrBytes * 8)) - 1n;
    const _fileBytes = BigInt(fs.fstatSync(fd).size);
    if (_fileBytes > maxFilePtr) {
        throw new Error(`xdb file exceeds the maximum supported bytes: ${maxFilePtr}`);
    }
}

export function verifyFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    verify(fd);
    fs.closeSync(fd);
}