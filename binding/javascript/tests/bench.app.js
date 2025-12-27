// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// app to do the xdb bench
// @Author Lion <chenxin619315@gmail.com>

import * as xdb from '../index.js';
import {ArgumentParser} from 'argparse';
import LineByLine from 'n-readlines';
import fs from 'fs';


const parser = new ArgumentParser({
    add_help: true,
    description: 'ip2region bench script',
    prog: 'node tests/bench.app.js',
    usage: 'Usage %(prog)s [command options]'
});

parser.add_argument('--db', {help: 'ip2region binary xdb file path'});
parser.add_argument('--src', {help: 'source ip text file path'});
parser.add_argument('--cache-policy', {help: 'cache policy: file/vectorIndex/content, default: vectorIndex'});

const args = parser.parse_args();
const dbPath  = args.db || '';
const srcPath = args.src || '';
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

const _split = (line) => {
    const ps = [];
    const s1 = line.indexOf('|');
    if (s1 === -1) {
        ps.push(line);
        return ps;
    }

    ps.push(line.substring(0, s1));
    const s2 = line.indexOf('|', s1 + 1);
    if (s2 === -1) {
        ps.push(line.substring(s1+1));
        return ps;
    }

    ps.push(line.substring(s1 + 1, s2));
    ps.push(line.substring(s2 + 1));
    return ps;
}

const main = async () => {
    if (dbPath.length < 1 || srcPath.length < 1) {
        parser.print_help();
        return;
    }

    const searcher = createSearcher();
    console.log(`Searcher: ${searcher.toString()}`);

    // read the source line and do the search bench
    let totalMicroSecs = 0, count = 0, line = null;
    const rl = new LineByLine(srcPath);
    while (line = rl.next()) {
        const ps  = _split(line.toString('utf-8'));
        const sTime  = process.hrtime();
        const sip = xdb.parseIP(ps[0]);
        const eip = xdb.parseIP(ps[1]);
        if (xdb.ipCompare(sip, eip) > 0) {
            throw new Error(`start ip(${ps[0]}) should not be greater than end ip(${ps[1]})`);
        }

        const test_list = [sip, eip];
        for (let i = 0; i < test_list.length; i++) {
            const region = await searcher.search(test_list[i]);
            if (region != ps[2]) {
                throw new Error(`failed to search(${xdb.ipToString(test_list[i])}) with (${region} != ${ps[2]})`);
            }
            count++;
        }
        const diff = process.hrtime(sTime);
        const took = diff[0] * 1_000_000 + diff[1] / 1e3;
        totalMicroSecs += took;
    }

    const tookSec = totalMicroSecs / 1e6;
    const _eachUs = count == 0 ? 0 : totalMicroSecs / count;
    console.log(`Bench finished, {cachePolicy: ${cachePolicy}, total: ${count}, took: ${tookSec} s, cost: ${_eachUs} μs/op}`);
}

main();