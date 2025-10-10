// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// version module
// @Author Lion <chenxin619315@gmail.com>

const header = require('./header');

const xdb_ipv4_id = 4;
const xdb_ipv6_id = 6;

class Version {
    constructor(id, name, bytes, indexSize, ip_compare_func) {
        this.id = id;
        this.name = name;
        this.bytes = bytes;
        this.indexSize = indexSize;
        this.ip_compare_func = ip_compare_func;
    }

    id() {
        return this.id;
    }

    name() {
        return this.name;
    }

    bytes() {
        return this.bytes;
    }

    indexSize() {
        return this.indexSize;
    }

    compareFunc() {
        return this.ip_compare_func;
    }

    ipCompare(ip1, ip2) {
        return this.ip_compare_func(ip1, ip2);
    }

    toString() {
        return `{"id": ${this.id}, "name": "${this.name}", "bytes":${this.bytes}, "index_size": ${this.indexSize}}`;
    }
}

// 14 = 4 + 4 + 2 + 4
const IPv4 = new Version(xdb_ipv4_id, "IPv4", 4, 14, function(ip1, ip2){
});

// 38 = 16 + 16 + 2 + 4
const IPv6 = new Version(xdb_ipv6_id, "IPv6", 6, 38, function(ip1, ip2){
});

function versionFromName(name) {
    let n = name.toUpperCase();
    if (n == "V4" || n == "IPV4") {
        return IPv4;
    } else if (n == "V6" || n == "IPV6") {
        return IPv6;
    } else {
        return null;
    }
}

function versionFromHeader(h) {
    let v = h.version();

    // old structure with ONLY IPv4 supporting
    if (v == header.xdb_structure_20) {
        return IPv4;
    }

    // structure 3.0 with IPv6 supporting
    if (v != header.xdb_structure_30) {
        return null;
    }

    let ipVer = h.ipVersion();
    if (ipVer == xdb_ipv4_id) {
        return IPv4;
    } else if (ipVer == xdb_ipv6_id) {
        return IPv6;
    } else {
        return null;
    }
}

module.exports = {
    xdb_ipv4_id, xdb_ipv6_id,
    Version, IPv4, IPv6,
    versionFromName, versionFromHeader
}