// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// searcher implementation
// @Author Lion <chenxin619315@gmail.com>

import fs from 'fs';
import {
    parseIP, 
    HeaderInfoLength, VectorIndexCols, VectorIndexSize,
    ipToString
} from './util.js';

export class Searcher {
    constructor(version, dbPath, vectorIndex, cBuffer) {
        this.ioCount = 0;
        this.dbPath  = dbPath;
        this.version = version;
        if (cBuffer != null) {
            this.handle = null;
            this.vectorIndex = null;
            this.cBuffer = cBuffer;
        } else {
            this.handle = fs.openSync(dbPath, 'r');
            this.vectorIndex = vectorIndex;
            this.cBuffer = null;
        }
    }

    getIPVersion() {
        return this.version;
    }

    getIOCount() {
        return this.ioCount;
    }

    async search(ip) {
        // check and parse the string ip
        const ipBytes = Buffer.isBuffer(ip) ? ip : parseIP(ip);

        // ip version check
        if (ipBytes.length != this.version.bytes) {
            throw new Error(`invalid ip address '${ipToString(ipBytes)}' (${this.version.name} expected)`);
        }

        // reset the global counter
        this.ioCount = 0;

        // located the segment index block based on the vector index
        let sPtr = 0, ePtr = 0;
        let il0 = ipBytes[0], il1 = ipBytes[1];
        let idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;
        if (this.vectorIndex != null) {
            sPtr = this.vectorIndex.readUint32LE(idx);
            ePtr = this.vectorIndex.readUint32LE(idx + 4);
        } else if (this.cBuffer != null) {
            sPtr = this.cBuffer.readUint32LE(HeaderInfoLength + idx);
            ePtr = this.cBuffer.readUint32LE(HeaderInfoLength + idx + 4);
        } else {
            const buff = Buffer.alloc(VectorIndexSize);
            this.read(HeaderInfoLength + idx, buff);
            sPtr = buff.readUInt32LE(0);
            ePtr = buff.readUInt32LE(4);
        }

        // console.log(`sPtr: ${sPtr}, ePtr: ${ePtr}`);
        // binary search the segment index block to get the region info
        const bytes = ipBytes.length, dBytes = ipBytes.length << 1;
        const indexSize = this.version.indexSize;
        const buff = Buffer.alloc(indexSize);
        let dLen = 0, dPtr = 0, l = 0, h = (ePtr - sPtr) / indexSize;
        while (l <= h) {
            const m = (l + h) >> 1;
            const p = sPtr + m * indexSize;

            // read the segment index block
            this.read(p, buff);
            if (this.version.ipSubCompare(ipBytes, buff, 0) < 0) {
                h = m - 1;
            } else if (this.version.ipSubCompare(ipBytes, buff, bytes) > 0) {
                l = m + 1;
            } else {
                dLen = buff.readUint16LE(dBytes);
                dPtr = buff.readUint32LE(dBytes + 2);
                break;
            }
        }

        // empty match interception.
        // and this could be a case.
        if (dLen == 0) {
            return "";
        }

        // console.log(`dLen: ${dLen}, dPtr: ${dPtr}`);
        const region = Buffer.alloc(dLen);
        this.read(dPtr, region);
        return region.toString('utf-8');
    }

    read(offset, buff, stats) {
        // check the in-memory buffer first
        if (this.cBuffer != null) {
            this.cBuffer.copy(buff, 0, offset, offset + buff.length);
            return;
        }

        // increase the io counts
        this.ioCount++;
        
        // read the data
        let rBytes = fs.readSync(this.handle, buff, 0, buff.length, offset);
        if (rBytes != buff.length) {
            throw new Error(`incomplete read: read bytes should be ${buff.length}`);
        }
    }

    // close the searcher
    close() {
        if (this.handle != null) {
            fs.close(this.handle);
        }
    }

    toString() {
        const vn = this.version.name;
        const vi = this.vectorIndex == null ? 'null' : this.vectorIndex.length;
        const cf = this.cBuffer == null ? 'null' : this.cBuffer.length;
        return `{"version": ${vn}, "dbPath": ${this.dbPath}, "handle": ${this.handle}, "vectorIndex": ${vi} "cBuffer": ${cf}}`;
    }
}

export function newWithFileOnly(version, dbPath) {
    return new Searcher(version, dbPath, null, null);
}

export function newWithVectorIndex(version, dbPath, vectorIndex) {
    return new Searcher(version, dbPath, vectorIndex, null);
}

export function newWithBuffer(version, cBuffer) {
    return new Searcher(version, null, null, cBuffer);
}