// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// searcher new tester
// @Author Lion <chenxin619315@gmail.com>

const {IPv4, IPv6, loadVectorIndexFromFile, XdbIPv4Id, loadContentFromFile} = require('../util');
const {newWithFileOnly, newWithVectorIndex, newWithBuffer} = require('../searcher');
const path = require('path');

const dbPath = {
    v4: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v4.xdb'),
    v6: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v6.xdb')
}

function _get_creater_list(version) {
    let dbFile = version.id == XdbIPv4Id ? dbPath.v4 : dbPath.v6;
     return [function(){
        return ["newWithFileOnly", newWithFileOnly(version, dbFile)];
    }, function(){
        const vIndex = loadVectorIndexFromFile(dbFile);
        return ["newWithVectorIndex", newWithVectorIndex(version, dbFile, vIndex)];
    }, function(){
        const cBuffer = loadContentFromFile(dbFile);
        return ["newWithBuffer", newWithBuffer(version, cBuffer)];
    }];
}

test('ipv4 searcher test', () => {
    const ip_Str = '120.229.45.92';
    const c_list = _get_creater_list(IPv4);
    try {
        let bRegion = null;
        for (var i = 0; i < c_list.length; i++) {
            const meta = c_list[i]();
            const region = meta[1].search(ip_Str);
            if (bRegion != null) {
                expect(region).toBe(region);
            }
            bRegion = region;
            console.log(`${meta[0]}.search(${ip_Str}): ${region}`);
        }
    } catch (e) {
        console.error(`${e.message}`);
    }
});

test('ipv6 searcher test', async () => {
    const ip_Str = '240e:57f:32ff:ffff:ffff:ffff:ffff:ffff';
    const c_list = _get_creater_list(IPv6);
    try {
        let bRegion = null;
        for (var i = 0; i < c_list.length; i++) {
            const meta = c_list[i]();
            const region = meta[1].search(ip_Str);
            if (bRegion != null) {
                expect(region).toBe(region);
            }
            bRegion = region;
            console.log(`${meta[0]}.search(${ip_Str}): ${region}`);
        }
    } catch (e) {
        console.error(`${e.message}`);
    }
});