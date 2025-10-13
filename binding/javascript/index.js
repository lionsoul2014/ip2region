// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ip2region JavaScript binding with IPv4 and IPv6 support.
// @Author Lion <chenxin619315@gmail.com>

export {
    Header,
    Version,
    IPv4,
    IPv6,
    XdbStructure20,
    XdbStructure30,
    XdbIPv4Id,
    XdbIPv6Id,
    HeaderInfoLength,
    VectorIndexRows,
    VectorIndexCols,
    VectorIndexSize,
    parseIP,
    ipToString,
    ipBytesString,
    ipSubCompare,
    ipCompare,
    versionFromName,
    versionFromHeader,
    loadHeader,
    loadHeaderFromFile,
    loadVectorIndex,
    loadVectorIndexFromFile,
    loadContent,
    loadContentFromFile,
    verify,
    verifyFromFile
} from './util.js';

export { 
    Searcher, 
    newWithFileOnly, 
    newWithVectorIndex, 
    newWithBuffer 
} from './searcher.js';