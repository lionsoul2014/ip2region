<?php
/**
 * test shell for ip2region for php
 *
 * */
$br = (php_sapi_name() == "cli")? "":"<br>";

if(!extension_loaded('ip2region')) {
	dl('ip2region.' . PHP_SHLIB_SUFFIX);
}

function getTime(){
    return microtime(true) * 1000;
}

// test class
function classBtreeSearch( $ip)
{

    $start = getTime();
    //$data  = $ip2region->btreeSearch($ip);
    $data  = Ip2region::btreeSearchString($ip);
    $end   = getTime();
    var_dump($data);
    echo " btree search string - taken: ", ($end - $start), "\n";

    $start = getTime();
    $data  = Ip2region:: btreeSearch(ip2long($ip));
    $end   = getTime();
    var_dump($data);
    echo " btree search - taken: ", ($end - $start), "\n";
}

function classBinarySearch($ip)
{

    $start = getTime();
    $data  = Ip2region::binarySearchString($ip);
    $end   = getTime();
    var_dump($data);
    echo " binary search string -taken: ", ($end - $start), "\n";

    $start = getTime();
    $data  = Ip2region::binarySearch(ip2long($ip));
    $end   = getTime();
    var_dump($data);
    echo " binary search - taken: ", ($end - $start), "\n";
}


function test(){
    $v1 = rand(0,255);
    $v2 = rand(0,255);
    $v3 = rand(0,255);
    $v4 = rand(0,255);
    $ip = "{$v1}.{$v2}.{$v3}.{$v4}";
    classBtreeSearch( $ip );
    classBinarySearch( $ip );
}

while(true)
{
    test();
    break;
    usleep(5000);
}
?>
