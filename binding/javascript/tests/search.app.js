// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// app to do the xdb search
// @Author Lion <chenxin619315@gmail.com>

import * as xdb from '../index.js';
import {ArgumentParser} from 'argparse';
import fs from 'fs';

const parser = new ArgumentParser({
    add_help: true,
    description: 'ip2region search script',
    prog: 'node tests/search.app.js',
    usage: 'Usage %(prog)s [command options]'
});

parser.add_argument('--db', {help: 'ip2region binary xdb file path'});
parser.add_argument('--cache-policy', {help: 'cache policy: file/vectorIndex/content, default: vectorIndex'});

const args = parser.parse_args();
const dbPath  = args.db || '';
const cachePolicy = args.cache_policy || 'vectorIndex';

// create the searcher
const createSearcher = () => {
    const handle = fs.openSync(dbPath, 'r');

    // verify the xdb file
    // @Note: do NOT call it every time you create a searcher since this will slow
    // down the search response.
    // @see the verify function for details.
    xdb.verify(handle);

    // get the ip version from the header
    const version = xdb.versionFromHeader(xdb.loadHeader(handle));

    let searcher = null;
    switch(cachePolicy) {
    case 'file':
        searcher = xdb.newWithFileOnly(version, dbPath);
        break;
    case 'vectorIndex':
        const vIndex = xdb.loadVectorIndexFromFile(dbPath);
        searcher = xdb.newWithVectorIndex(version, dbPath, vIndex);
        break;
    case 'content':
        const cBuffer = xdb.loadContentFromFile(dbPath);
        searcher = xdb.newWithBuffer(version, cBuffer);
        break;
    default:
        fs.closeSync(handle);
        throw new Error(`invalid cache-policy '${cachePolicy}'`);
    }

    fs.closeSync(handle);
    return searcher;
}

const readlineSync = () => {
    return new Promise((resolve, reject) => {
        process.stdin.resume();
        process.stdin.on('data', (buff) => {
            process.stdin.pause();
            resolve(buff.toString('utf-8'));
        });
    });
}

const main = async () => {
    if (dbPath.length < 1) {
        parser.print_help();
        return;
    }

    const searcher = createSearcher();
    console.log(`ip2region xdb searcher test program
source xdb: ${dbPath} (${searcher.getIPVersion().name}, ${cachePolicy})
type 'quit' to exit`);

    while (true) {
        process.stdout.write('ip2region>> ');

        // get the input ip
        const ipString = (await readlineSync()).trim();
        if (ipString.length == 0) {
            continue;
        }

        if (ipString == 'quit') {
            break;
        }

        // parse the ip address
        let ipBytes = null;
        try {
            ipBytes = xdb.parseIP(ipString);
        } catch (e) {
            console.log(`failed to parse ip: ${e.message}`);
            continue;
        }

        // do the search
        const sTime = process.hrtime();
        let region = null;
        try {
            region = await searcher.search(ipBytes);
        } catch (e) {
            console.log(`{err: ${e.message}, ioCount: ${searcher.getIOCount()}}`);
            continue;
        }

        const diff = process.hrtime(sTime);
        const took = diff[0] * 1_000_000 + diff[1] / 1e3;
        console.log(`{region: ${region}, ioCount: ${searcher.getIOCount()}, took: ${took} μs}`);
    }
}

main();