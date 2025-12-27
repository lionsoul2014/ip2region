<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/22

require 'Searcher.class.php';
use \ip2region\xdb\Util;
use \ip2region\xdb\Searcher;
use \ip2region\xdb\IPv4;
use \ip2region\xdb\IPv6;

// check and get the function to run
if($argc < 2) {
    printf("please specified the function name\n");
    return;
} else {
    $func_name = trim($argv[1]);
}

$basePath = dirname(dirname(dirname(dirname(__FILE__))));

function testLoadHeader() {
    global $basePath;
    $header = Util::loadHeaderFromFile("{$basePath}/data/ip2region_v4.xdb");
    if ($header == null) {
        printf("failed to load header from file\n");
        return;
    }

    printf("header loaded: ");
    print_r($header);
}

function testLoadVectorIndex() {
    global $basePath;
    $vIndex = Util::loadVectorIndexFromFile("{$basePath}/data/ip2region_v4.xdb");
    if ($vIndex == null) {
        printf("failed to load vector index from file\n");
        return;
    }

    printf("vector index loaded: length=%d\n", strlen($vIndex));
}

function testLoadContent() {
    global $basePath;
    $cBuff = Util::loadContentFromFile("{$basePath}/data/ip2region_v4.xdb");
    if ($cBuff == null) {
        printf("failed to load content from file\n");
        return;
    }

    printf("content loaded, length=%d\n", strlen($cBuff));
}

function testParseIP() {
    $ips = [
        // IPv4
        "1.0.0.1", 
        "192.168.1.100",
        "121.35.184.170",

        "xx.xx.1.100",

        // IPv6
        "3000::",
        "240e:87c:71a:639a:3dff:ffff:ffff:ffff",
        "240e:87c:71a:c877:900::"
    ];

    foreach ($ips as $ip) {
        $bytes = Util::parseIP($ip);
        if ($bytes == NULL) {
            printf("invalid ip address: `%s`\n", $ip);
            continue;
        }

        // for ($i = 0; $i < strlen($bytes); $i++) {
        //     printf("%d: []: %s, ord: %d\n", $i, $bytes[$i], ord($bytes[$i]));
        // }

        printf("bytes: %s (%s), address: %s\n", bin2hex($bytes), gettype($bytes), Util::ipToString($bytes));
    }
}

function testIPCompare() {
    $ipPairs = [
        ["1.0.0.0", "1.0.0.1"],
        ["192.168.1.101", "192.168.1.90"],
        ["219.133.111.87", "114.114.114.114"],
        ["1.0.4.0", "1.0.1.0"],
        ["1.0.4.0", "1.0.3.255"],
        ["2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff"],
        ["2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff"],
        ["ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff"]
    ];

    foreach ($ipPairs as $ips) {
        $ip1 = Util::parseIP($ips[0]);
        $ip2 = Util::parseIP($ips[1]);
        printf("ipCompare(%s, %s): %d\n", Util::ipToString($ip1), Util::ipToString($ip2), Util::ipSubCompare($ip1, $ip2, 0));
    }
}

function testAttributes() {
    printf("IPv4VersioNo: %d\n", \ip2region\xdb\IPv4VersionNo);
    printf("IPv6VersioNo: %d\n", \ip2region\xdb\IPv6VersionNo);
    printf("IPv4 Object: %s\n", IPv4::default());
    printf("IPv6 Object: %s\n", IPv6::default());
}


if (!function_exists($func_name)) {
    printf("function {$func_name} not found\n");
} else {
    printf("calling {$func_name} ... \n");
    $now = Util::now();
    $func_name();
    $cost = Util::now() - $now;
    printf("done, cost: %0.5f ms\n", $cost);
}