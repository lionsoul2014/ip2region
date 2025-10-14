// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util test script
// @Author Lion <chenxin619315@gmail.com>

import * as util from '../util.js';
import path from 'node:path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const dbPath = path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v4.xdb')

test('const print', () => {
    console.log("IPv4: ", util.IPv4.toString());
    console.log("IPv6: ", util.IPv6.toString());
});

test("test version from name", () => {
    let vs = ["v4", "ipv4", "v4x", "v6", "ipv6", "v6x"];
    vs.forEach(ele => {
        let v = util.versionFromName(ele);
        if (v == null) {
            console.log(`invalid version name ${ele}`);
            return;
        }

        console.log(`versionFrom(${ele}): ${v.toString()}, id=${v.id}, name=${v.name}`);
    });
});

test("test version ip compare", () => {
    let ip_list = [
        ["1.0.0.0", "0.0.1.2", 1],
        ["192.168.1.101", "192.168.1.90", 1],
        ["219.133.111.87", "114.114.114.114", 1],
        ["2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff", -1],
        ["2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff", -1],
        ["ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff", 1]
    ];

    ip_list.forEach(ips => {
        const ip1 = util.parseIP(ips[0]);
        const ip2 = util.parseIP(ips[1]);
        if (ip1.length != ip2.length) {
            fail(`ip1 and ip2 are not the same ip type`);
        }

        const version = ip1.length == 4 ? util.IPv4 : util.IPv6;
        const cmp = version.ipSubCompare(ip1, ip1.length == 4 ? ip2.reverse() : ip2, 0);
        expect(cmp).toBe(ips[2]);
        console.log(`compare(${ips[0]}, ${ips[1]}): ${cmp}`);
    });
});



test('parse ip address', () => {
    let ip_list = [
        "1.0.0.0", "58.251.30.115", "192.168.1.100", "126.255.32.255", "219.xx.xx.11", 
        "::", "::1", "fffe::", "2c0f:fff0::", "2c0f:fff0::1", "2a02:26f7:c409:4001::",
        "2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "240e:982:e617:ffff:ffff:ffff:ffff:ffff", "::xx:ffff"
    ];

    ip_list.forEach(ipString => {
        let ipBytes = null;
        try {
            ipBytes = util.parseIP(ipString);
        } catch (e) {
            console.log(`failed to parse ip '${ipString}': ${e.message}`);
            return;
        }

        let to_Str = util.ipToString(ipBytes, true);
        let toByte = util.ipBytesString(ipBytes);
        console.log(`parseIP(${ipString}): {Bytes: ${toByte}, String: ${to_Str}}`);
        expect(ipString).toBe(to_Str);
    });
});

test('ip compare', () => {
    let ip_list = [
        ["1.0.0.0", "1.0.0.1", -1],
        ["192.168.1.101", "192.168.1.90", 1],
        ["219.133.111.87", "114.114.114.114", 1],
        ["2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff", -1],
        ["2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff", -1],
        ["ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff", 1]
    ];

    ip_list.forEach(ips => {
        const ip1 = util.parseIP(ips[0]);
        const ip2 = util.parseIP(ips[1]);
        const cmp = util.ipCompare(ip1, ip2);
        expect(cmp).toBe(ips[2]);
        console.log(`compare(${ips[0]}, ${ips[1]}): ${cmp}`);
    });
});

test('test load header', () => {
    let header = util.loadHeaderFromFile(dbPath);
    console.log(`dbPath: ${dbPath}, header: ${header.toString()}}`);
});

test('test load vector index', () => {
    let vIndex = util.loadVectorIndexFromFile(dbPath);
    console.log(`dbPath: ${dbPath}, vIndex: ${vIndex.length}}`);
});

test('test load content', () => {
    let content = util.loadContentFromFile(dbPath);
    console.log(`dbPath: ${dbPath}, content: ${content.length}}`);
});