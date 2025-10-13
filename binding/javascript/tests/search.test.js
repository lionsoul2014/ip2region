// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// searcher search tester
// @Author Lion <chenxin619315@gmail.com>

import {IPv4, IPv6, parseIP, ipToString, newWithFileOnly} from '../index.js';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const dbPath = {
    v4: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v4.xdb'),
    v6: path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v6.xdb')
}

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