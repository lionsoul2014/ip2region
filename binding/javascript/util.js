// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util functions
// @Author Lion <chenxin619315@gmail.com>

const { exec } = require('child_process');
const header = require('./header');
const fs = require('fs');

// --
// parse the specified string ip and return its bytes

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
            throw new Error(`invalid ipv4 part '${ps[i]}'`);
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

    var s, v, dc_num = 0;
    const ipBytes = Buffer.alloc(16);
    for (var i = 0; i < ps.length; i++) {
        s = ps[i].trim();
        if (s.length == 0) {    // Double colon
            dc_num++;
        }

        v = parseInt(s, 16);
        if (v < 0 || v > 0xFFFE) {
            throw new Error(`invalid ipv6 part '${ps[i]}' should >= 0 and <= 65534`);
        }

        // @TODO: keep coding
        ipBytes.writeUint16BE(v, i);
    }

    return ipBytes;
}


// @param  ip string
// @return Buffer
function parseIP(ipString) {
    let sDot = ipString.indexOf('.');
    let cDot = ipString.indexOf(':');
    if (sDot > -1 && cDot == -1) {
        return _parse_ipv4_addr(ipString);
    } else if (cDot > -1) {
        return _parse_ipv6_addr(ipString);
    } else {
        return null;
    }
}

// ---


// ---
// bytes ip to humen-readable string ip

// ipv4 bytes to string
// function _ipv4_to_string(v4Bytes) {
//     return v4Bytes.join('.');
// }

// ipv6 bytes to string
function _ipv6_to_string(v6Bytes) {
    return null;
}

function ipToString(ipBytes) {
    if (!Buffer.isBuffer(ipBytes)) {
        throw new Error('invalid bytes ip, not Buffer');
    }

    if (ipBytes.length == 4) {
        // return _ipv4_to_string(ipBytes);
        return ipBytes.join('.');
    } else if (ipBytes.length == 16) {
        return _ipv6_to_string(ipBytes);
    } else {
        throw new Error('invalid bytes ip with length not 4 or 16');
    }
}

// compare two byte ips
// returns: -1 if ip1 < ip2, 1 if ip1 > ip2 or 0
function ipCompare(ip1, ip2) {

}

// load header from xdb file
function loadHeader(fd) {
    const buffer = Buffer.alloc(header.HeaderInfoLength);
    const rBytes = fs.readSync(fd, buffer, 0, header.HeaderInfoLength, 0);
    if (rBytes != header.HeaderInfoLength) {
        throw new Error(`incomplete read (${rBytes} read, ${header.HeaderInfoLength} expected)`);
    }
    return new header.Header(buffer);
}

function loadHeaderFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const header = loadHeader(fd);
    fs.closeSync(fd);
    return header;
}

function loadVectorIndex(fd) {
    const vBytes = header.VectorIndexCols * header.VectorIndexRows * header.VectorIndexSize;
    const buffer = Buffer.alloc(vBytes);
    const rBytes = fs.readSync(fd, buffer, 0, vBytes, header.HeaderInfoLength);
    if (rBytes != vBytes) {
        throw new Error(`incomplete read (${rBytes} read, ${vBytes} expected)`);
    }
    return buffer;
}

function loadVectorIndexFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const vIndex = loadVectorIndex(fd);
    fs.closeSync(fd);
    return vIndex;
}

function loadContent(fd) {
    const stats = fs.fstatSync(fd);
    const buffer = Buffer.alloc(stats.size);
    const rBytes = fs.readSync(fd, buffer, 0, buffer.length, 0);
    if (rBytes != stats.size) {
        throw new Error(`incomplete read (${rBytes} read, ${stats.size} expected)`);
    }
    return buffer;
}

function loadContentFromFile(dbPath) {
    const fd = fs.openSync(dbPath, "r");
    const content = loadContent(fd);
    fs.close(fd, function(){});
    return content;
}

module.exports = {
    parseIP, ipToString, ipCompare,
    loadHeader, loadHeaderFromFile,
    loadVectorIndex, loadVectorIndexFromFile,
    loadContent, loadContentFromFile
}