// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// searcher new tester
// @Author Lion <chenxin619315@gmail.com>

import path from 'path';
import { fileURLToPath } from 'url';
import {
    IPv4, IPv6, XdbIPv4Id, 
    parseIP, ipToString, verifyFromFile,
    loadVectorIndexFromFile, loadContentFromFile, 
    newWithFileOnly, newWithVectorIndex, newWithBuffer
} from '../index.js';
import { fail } from 'assert';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const dbPath = {
    v4: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v4.xdb'),
    v6: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v6.xdb')
}
test('xdb file verify', () => {
    // verify the xdb file
    // @Note: do NOT call it every time you create a searcher since this will slow
    // down the search response.
    // @see the verify function for details.
    for (k in dbPath) {
        if (!dbPath.hasOwnProperty(k)) {
            continue;
        }

        try {
            verifyFromFile(dbPath[k]);
            console.log(`xdb file '${dbPath[k]}' verified`);
        } catch (e) {
            throw new Error(`binding is not applicable for xdb file '${dbPath[k]}': ${e.message}`);
        }
    }
});

// ---
// search api testing

test('ipv4 search test', async () => {
    let searcher = newWithFileOnly(IPv4, dbPath.v4);
    let ip_list  = [
        '1.0.0.0',
        parseIP('113.118.112.93'),
        '240e:3b7::'
    ];

    for (var i = 0; i < ip_list.length; i++) {
        let ip = ip_list[i];
        searcher.search(ip).then((region)=>{
            let ipStr = Buffer.isBuffer(ip) ? ipToString(ip) : ip;
            console.log(`search(${ipStr}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
        }).catch((err) => {
            console.log(`${err.message}`);
        });
    }

    // close searcher
    searcher.close();
});

test('ipv6 search test', async () => {
    let searcher = newWithFileOnly(IPv6, dbPath.v6);
    let ip_list  = [
        '2a02:26f7:c409:4001::',
        parseIP('2a11:8080:200::a:a05c'),
        '240e:3b7::',
        '120.229.45.92'
    ];

    for (var i = 0; i < ip_list.length; i++) {
        let ip = ip_list[i];
        searcher.search(ip).then((region)=>{
            let ipStr = Buffer.isBuffer(ip) ? ipToString(ip) : ip;
            console.log(`search(${ipStr}): {region: ${region}, ioCount: ${searcher.getIOCount()}}`);
        }).catch((err) => {
            console.log(`${err.message}`);
        });
    }

    // close searcher
    searcher.close();
});


// ---
// searcher with different cache policy testing

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

test('ipv4 searcher test', async () => {
    const ip_Str = '120.229.45.92';
    const c_list = _get_creater_list(IPv4);
    try {
        let bRegion = null;
        for (var i = 0; i < c_list.length; i++) {
            const meta = c_list[i]();
            const region = await meta[1].search(ip_Str);
            if (bRegion != null) {
                expect(region).toBe(region);
            }
            bRegion = region;
            console.log(`${meta[0]}.search(${ip_Str}): ${region}`);

            // searcher close
            meta[1].close();
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
            const region = await meta[1].search(ip_Str);
            if (bRegion != null) {
                expect(region).toBe(region);
            }
            bRegion = region;
            console.log(`${meta[0]}.search(${ip_Str}): ${region}`);

            // searcher close
            meta[1].close();
        }
    } catch (e) {
        console.error(`${e.message}`);
    }
});