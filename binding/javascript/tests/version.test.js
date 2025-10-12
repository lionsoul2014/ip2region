// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// version test script
// @Author Lion <chenxin619315@gmail.com>

const util = require('../util');

test('const print', () => {
    console.log("IPv4: ", util.IPv4.toString());
    console.log("IPv6: ", util.IPv6.toString());
});

test("test version from name", () => {
    let vs = ["v4", "ipv4", "v4x", "v6", "ipv6", "v6x"];
    vs.forEach(ele => {
        let v = util.versionFromName(ele);
        console.log(`versionFrom(${ele})`, v == null ? null : v.toString());
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
