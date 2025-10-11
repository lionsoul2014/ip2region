// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// util test script
// @Author Lion <chenxin619315@gmail.com>

const util = require('../util');
const path = require('path');

const dbPath = path.join(__dirname, '..', '..', '..', 'data', 'ip2region_v4.xdb')

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