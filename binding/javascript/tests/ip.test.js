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

        let toStr = ipBytes == null ? '0' : util.ipToString(ipBytes);
        console.log(`parseIP(${ipString}): {Bytes: ${ipBytes.length}, String: ${toStr}`);
    });
});