<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/21

require dirname(__FILE__) . '/XdbSearcher.class.php';

function printHelp($argv) {
    printf("php %s [command options]\n", $argv[0]);
    printf("options: \n");
    printf(" --db string             ip2region binary xdb file path\n");
    printf(" --cache-policy string   cache policy: file/vectorIndex/content\n");
}

if($argc < 2) {
    printHelp($argv);
    return;
}

$dbFile = "";
$cachePolicy = 'vectorIndex';
array_shift($argv);
foreach ($argv as $r) {
    if (strlen($r) < 5) {
        continue;
    }

    if (strpos($r, '--') != 0) {
        continue;
    }

    $sIdx = strpos($r, "=");
    if ($sIdx < 0) {
        printf("missing = for args pair %s\n", $r);
        return;
    }

    $key = substr($r, 2, $sIdx - 2);
    $val = substr($r, $sIdx + 1);
    if ($key == 'db') {
        $dbFile = $val;
    } else if ($key == 'cache-policy') {
        $cachePolicy = $val;
    } else {
        printf("undefined option `%s`\n", $r);
        return;
    }
}

if (strlen($dbFile) < 1) {
    printHelp($argv);
    return;
}

// printf("debug: dbFile: %s, cachePolicy: %s\n", $dbFile, $cachePolicy);
// create the xdb searcher by the cache-policy
switch ( $cachePolicy ) {
case 'file':
    try {
        $searcher = XdbSearcher::newWithFileOnly($dbFile);
    } catch (Exception $e) {
        printf("failed to create searcher with '%s': %s\n", $dbFile, $e);
        return;
    }
    break;
case 'vectorIndex':
    $vIndex = XdbSearcher::loadVectorIndexFromFile($dbFile);
    if ($vIndex == null) {
        printf("failed to load vector index from '%s'\n", $dbFile);
        return;
    }

    try {
        $searcher = XdbSearcher::newWithVectorIndex($dbFile, $vIndex);
    } catch (Exception $e) {
        printf("failed to create vector index cached searcher with '%s': %s\n", $dbFile, $e);
        return;
    }
    break;
case 'content':
    $cBuff = XdbSearcher::loadContentFromFile($dbFile);
    if ($cBuff == null) {
        printf("failed to load xdb content from '%s'\n", $dbFile);
        return;
    }

    try {
        $searcher = XdbSearcher::newWithBuffer($cBuff);
    } catch (Exception $e) {
        printf("failed to create content cached searcher: %s", $e);
        return;
    }
    break;
default:
    printf("undefined cache-policy `%s`\n", $cachePolicy);
    return;
}

printf("ip2region xdb searcher test program, cachePolicy: ${cachePolicy}\ntype 'quit' to exit\n");
while ( true ) {
    echo "ip2region>> ";
    $line = trim(fgets(STDIN));
    if (strlen($line) < 2) {
        continue;
    }

    if ($line == 'quit') {
        break;
    }

    if (XdbSearcher::ip2long($line) === null) {
        echo "Error: invalid ip address\n";
        continue;
    }

    $sTime = XdbSearcher::now();
    try {
        $region = $searcher->search($line);
    } catch (Exception $e) {
        printf("search call failed: %s\n", $e);
        continue;
    }

    printf("{region: %s, ioCount: %d, took: %.5f ms}\n",
        $region, $searcher->getIOCount(), XdbSearcher::now() - $sTime);
}

// close the searcher at last
$searcher->close();
printf("searcher test program exited, thanks for trying\n");