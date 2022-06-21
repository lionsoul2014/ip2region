<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/21

require dirname(__FILE__) . '/XdbSearcher.class.php';

if($argc < 2) {
    printf("php %s [command options]\n", $argv[0]);
    printf("options: \n");
    printf(" --db string             ip2region binary xdb file path\n");
    printf(" --cache-policy string   cache policy: file/vectorIndex/content\n");
    return;
}

$dbFile = "";
$cachePolicy = isset($argv[2]) ? $argv[2] : 'vectorIndex';
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

printf("debug: dbFile: %s, cachePolicy: %s\n", $dbFile, $cachePolicy);

// create the xdb searcher by the cache-policy
$searcher = null;
switch ( $cachePolicy ) {
case 'file':
    try {
        $searcher = new XdbSearcher($dbFile);
    } catch (Exception $e) {
        printf("failed to create searcher with '%s': %s\n", $dbFile, $e);
        return;
    }
    break;
case 'vectorIndex':
    break;
case 'content':
    break;
default:
    printf("undefined cache-policy `%s`\n", $cachePolicy);
    return;
}

while ( true ) {
    echo "ip2region>> ";
    $line = trim(fgets(STDIN));
    if (strlen($line) < 2) {
        continue;
    }

    if ($line == 'quit') {
        break;
    }

    if (XdbSearcher::ip2long($line) === false) {
        echo "Error: invalid ip address\n";
        continue;
    }

    $sTime = getTime();
    try {
        $region = $searcher->search($line);
    } catch (Exception $e) {
        printf("search call failed: %s\n", $e);
        continue;
    }

    printf("{region: %s, took: %.5f ms}\n", $region, getTime() - $sTime);
}

// close the searcher at last
$searcher->close();

function getTime()
{
    return (microtime(true) * 1000);
}
