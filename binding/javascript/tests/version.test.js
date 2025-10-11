// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// version test script
// @Author Lion <chenxin619315@gmail.com>

const version = require('../version');

test('const print', () => {
    console.log("IPv4: ", version.IPv4.toString());
    console.log("IPv6: ", version.IPv6.toString());
});

test("test version from name", () => {
    let vs = ["v4", "ipv4", "v4x", "v6", "ipv6", "v6x"];
    vs.forEach(ele => {
        let v = version.versionFromName(ele);
        console.log(`versionFrom(${ele})`, v == null ? null : v.toString());
    });
});