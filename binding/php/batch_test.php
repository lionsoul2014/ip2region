<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/22

require dirname(__FILE__) . '/xdb/Searcher.class.php';

use \ip2region\xdb\Util;
use \ip2region\xdb\{IPv4, IPv6};
use \ip2region\xdb\Searcher;

function printHelp($argv) {
    printf("php %s [command options]\n", $argv[0]);
    printf("options: \n");
    printf(" --db string             ip2region binary xdb file path\n");
    printf(" --src string            source ip text file path\n");
    printf(" --cache-policy string   cache policy: file/vectorIndex/content\n");
}

if($argc < 2) {
    printHelp($argv);
    return;
}

$dbFile = "";
$srcFile = "";
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
    } else if ($key == 'src') {
        $srcFile = $val;
    } else if ($key == 'cache-policy') {
        $cachePolicy = $val;
    } else {
        printf("undefined option `%s`\n", $r);
        return;
    }
}

if (strlen($dbFile) < 1 || strlen($srcFile) < 1) {
    printHelp($argv);
    return;
}

// printf("debug: dbFile: %s, cachePolicy: %s\n", $dbFile, $cachePolicy);
$handle = fopen($dbFile, 'r');
if ($handle === false) {
    printf("failed to open the xdb file `{$dbFile}`\n");
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


// do the bench test
$handle = fopen($srcFile, "r");
if ($handle === false) {
    printf("failed to open source text file `%s`\n", $srcFile);
    return null;
}

$count = 0;
$qx_count = 0;
while (!feof($handle)) {
    $line = trim(fgets($handle, 1024));
    if (strlen($line) < 1) {
        continue;
    }

    try {
        $ipBytes = Util::parseIP($line);
    } catch (Exception $e) {
        printf("failed to parse ip `%s`: %s\n", $line, $e->getMessage());
        continue;
    }

    $count++;
    $region = $searcher->searchByBytes($ipBytes);
    $ss = explode('|', $region);
    echo $line, ",", str_replace('|', ',', $region), "\n";
}

fclose($handle);
echo "Done, with {$count} IPs\n";
