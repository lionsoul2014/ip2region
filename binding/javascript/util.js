// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util functions
// @Author Lion <chenxin619315@gmail.com>

const header = require('./header');
const fs = require('fs');

// parse the specified string ip and return its bytes
// @param  ip string
// @return Buffer
function parseIP(ipString) {
}


// bytes ip to humen-readable string ip
function ipToString(ipBytes) {

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