<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/22

require dirname(__FILE__) . '/XdbSearcher.class.php';

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


// do the bench test
$handle = fopen($srcFile, "r");
if ($handle === false) {
    printf("failed to open source text file `%s`\n", $srcFile);
    return null;
}

$count = 0;
$costs = 0;
$sTime = XdbSearcher::now();
while (!feof($handle)) {
    $line = trim(fgets($handle, 1024));
    if (strlen($line) < 1) {
        continue;
    }

    $ps = explode('|', $line, 3);
    if (count($ps) != 3) {
        printf("invalid ip segment line `${line}`\n");
        return;
    }

    $sip = XdbSearcher::ip2long($ps[0]);
    if ($sip === null) {
        printf("invalid start ip `%s`\n", $ps[0]);
        return;
    }

    $eip = XdbSearcher::ip2long($ps[1]);
    if ($eip === null) {
        printf("invalid end ip `%s`\n", $ps[1]);
        return;
    }

    if ($sip > $eip) {
        printf("start ip(%s) should not be greater than end ip(%s)\n", $ps[0], $ps[1]);
        return;
    }

    $mip = ($sip + $eip) >> 1;
    foreach ([$sip, ($sip + $mip) >> 1, $mip, ($mip + $eip) >> 1, $eip] as $ip) {
        try {
            $cTime = XdbSearcher::now();
            $region = $searcher->search($ip);
            $costs += XdbSearcher::now() - $cTime;
        } catch (Exception $e) {
            printf("failed to search ip `%s`\n", long2ip($ip));
            return;
        }

        if ($region == null) {
            printf("failed to search ip `%s`\n", long2ip($ip));
            return;
        }

        // check the region info
        if ($region != $ps[2]) {
            printf("failed search(%s) with (%s != %s)\n", long2ip($ip), $region, $ps[2]);
            return;
        }

        $count++;
    }
}

// close the searcher at last
fclose($handle);
$searcher->close();
printf("Bench finished, {cachePolicy: %s, total: %d, took: %ds, cost: %.3f ms/op}\n",
    $cachePolicy, $count, (XdbSearcher::now() - $sTime)/1000, $count == 0 ? 0 : $costs/$count);
