<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/21

require dirname(__FILE__) . '/xdb/Searcher.class.php';

use \ip2region\xdb\Util;
use \ip2region\xdb\{IPv4, IPv6};
use \ip2region\xdb\Searcher;

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
$handle = fopen($dbFile, 'r');
if ($handle === false) {
    printf("failed to open the xdb file `{$dbFile}`\n");
    return;
}

// verify the xdb file
// @Note: do NOT call it every time you create a searcher since this will slow
// down the search response.
// @see the Util.verify function for details.
$err = Util::verify($handle);
if ($err != null) {
    printf("failed to verify xdb file `%s`: %s\n", $dbFile, $err);
    return;
}

// load header
$header = Util::loadHeader($handle);
if ($header == null) {
    printf("failed to load the header\n");
    return;
}

// get the version number from the xdb header
try {
    $version = Util::versionFromHeader($header);
} catch (Exception $e) {
    printf("failed to detect version from header: {$e->getMessage()}\n");
    return;
}

// create the xdb searcher by the cache-policy
switch ( $cachePolicy ) {
case 'file':
    try {
        $searcher = Searcher::newWithFileOnly($version, $dbFile);
    } catch (Exception $e) {
        printf("failed to create searcher with '%s': %s\n", $dbFile, $e);
        return;
    }
    break;
case 'vectorIndex':
    $vIndex = Util::loadVectorIndex($handle);
    if ($vIndex == null) {
        printf("failed to load vector index from '%s'\n", $dbFile);
        return;
    }

    try {
        $searcher = Searcher::newWithVectorIndex($version, $dbFile, $vIndex);
    } catch (Exception $e) {
        printf("failed to create vector index cached searcher with '%s': %s\n", $dbFile, $e);
        return;
    }
    break;
case 'content':
    $cBuff = Util::loadContent($handle);
    if ($cBuff == null) {
        printf("failed to load xdb content from '%s'\n", $dbFile);
        return;
    }

    try {
        $searcher = Searcher::newWithBuffer($version, $cBuff);
    } catch (Exception $e) {
        printf("failed to create content cached searcher: %s", $e);
        return;
    }
    break;
default:
    printf("undefined cache-policy `%s`\n", $cachePolicy);
    return;
}

printf(<<<EOF
ip2region xdb searcher test program
source xdb file: {$dbFile} ({$version->name}, ${cachePolicy})
type 'quit' to exit\n
EOF);
while ( true ) {
    echo "ip2region>> ";
    $line = trim(fgets(STDIN));
    if (strlen($line) < 2) {
        continue;
    }

    if ($line == 'quit') {
        break;
    }

    $cost = -1;
    try {
        $sTime = Util::now();
        $region = $searcher->search($line);
        $cost = Util::now() - $sTime;
    } catch (Exception $e) {
        printf("search call failed: %s\n", $e->getMessage());
        continue;
    }

    printf(
        "{region: %s, ioCount: %d, took: %.5f ms}\n",
        $region, $searcher->getIOCount(), $cost
    );
}

// close the searcher at last
$searcher->close();
printf("searcher test program exited, thanks for trying\n");