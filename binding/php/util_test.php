<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/22

require dirname(__FILE__) . '/XdbSearcher.class.php';

const XDB_PATH = '../../data/ip2region.xdb';

function testLoadHeader() {
    $header = XdbSearcher::loadHeaderFromFile(XDB_PATH);
    if ($header == null) {
        printf("failed to load header from file\n");
        return;
    }

    printf("header loaded: ");
    print_r($header);
}

function testLoadVectorIndex() {
    $vIndex = XdbSearcher::loadVectorIndexFromFile(XDB_PATH);
    if ($vIndex == null) {
        printf("failed to load vector index from file\n");
        return;
    }

    printf("vector index loaded, length=%d\n", strlen($vIndex));
}

function testLoadContent() {
    $cBuff = XdbSearcher::loadContentFromFile(XDB_PATH);
    if ($cBuff == null) {
        printf("failed to load content from file\n");
        return;
    }

    printf("content loaded, length=%d\n", strlen($cBuff));
}

printf("testing loadHeader ... \n");
$now = XdbSearcher::now();
testLoadHeader();
printf("done, cost: %0.5f ms\n\n", XdbSearcher::now() - $now);


printf("testing loadVectorIndex ... \n");
$now = XdbSearcher::now();
testLoadVectorIndex();
printf("done, cost: %0.5f ms\n\n", XdbSearcher::now() - $now);


printf("testing loadContent ... \n");
$now = XdbSearcher::now();
testLoadContent();
printf("done, cost: %0.5f ms\n\n", XdbSearcher::now() - $now);
