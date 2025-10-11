// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// xdb header class
// @Author Lion <chenxin619315@gmail.com>

const XdbStructure20 = 2;
const XdbStructure30 = 3;
const xdbIPv4Id = 4;
const xdbIPv6Id = 6;

const HeaderInfoLength = 256;
const VectorIndexRows  = 256;
const VectorIndexCols  = 256;
const VectorIndexSize  = 8;

class Header {
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

    version() {
        return this.version;
    }

    indexPolicy() {
        return this.indexPolicy;
    }

    createdAt() {
        return this.createdAt;
    }

    startIndexPtr() {
        return this.startIndexPtr;
    }

    endIndexPtr() {
        return this.endIndexPtr;
    }

    ipVersion() {
        return this.ipVersion;
    }

    runtimePtrBytes() {
        return this.runtimePtrBytes;
    }

    buff() {
        return this.buff;
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

module.exports = {
    XdbStructure20, XdbStructure30, xdbIPv4Id, xdbIPv6Id,
    HeaderInfoLength, VectorIndexRows, VectorIndexCols, VectorIndexSize,
    Header
}