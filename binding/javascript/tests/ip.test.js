// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util test script
// @Author Lion <chenxin619315@gmail.com>

const util = require('../util');

test('parse ip address', () => {
    let ip_list = [
        "1.0.0.0", "58.251.30.115", "192.168.1.100", "126.255.32.255", "219.xx.xx.11", 
        "::", "::1", "fffe::", "2c0f:fff0::", "2c0f:fff0::1", 
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

        let to_Str = ipBytes == null ? '0' : util.ipToString(ipBytes, true);
        let toByte = ipBytes == null ? '0' : util.ipBytesString(ipBytes);
        console.log(`parseIP(${ipString}): {Bytes: ${toByte}, String: ${to_Str}}`);
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