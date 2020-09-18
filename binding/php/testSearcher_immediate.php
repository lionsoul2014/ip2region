<?php
/**
* 支持以下三种查询方式:
* echo  125.39.52.26 | php ./testSearcher_immediate.php
* php ./testSearcher_immediate.php 125.39.52.26 
* cat t.txt | awk '{print $5}' | xargs php ./testSearcher_immediate.php 
*/
require dirname(__FILE__) . '/Ip2Region.class.php';

class IpSearcher
{
    private static $dbFile = "../../data/ip2region.db";
    private static  $method     = 'btreeSearch';
    private static  $algorithm  = 'B-tree';
    private static  $ip2regionObj;

    private $argv;

    public function __construct($argv)
    {
        $this->argv = $argv;
        self::$ip2regionObj = new Ip2Region(self::$dbFile);
    }

    public function run()
    {
        if (!empty($this->argv[1])) // cat与直接参数
        {
            $len = count($this->argv);
            for ($i = 1; $i <= $len - 1; $i++ )
            {
                $this->search($this->argv[$i]);
            }
        }
        else    // echo 方式
        {
            $ip = trim(fgets(STDIN));
            if (!empty($ip))
            {
                $this->search($ip);    
            }
        }
    }

    private function getTime()
    {
        return (microtime(true) * 1000);
    }

    private function search($ip)
    {
        $s_time = $this->getTime();
        $data   = self::$ip2regionObj->{self::$method}($ip);
        $c_time = $this->getTime() - $s_time;
        printf("%s|%s in %.5f millseconds\n", $data['city_id'], $data['region'], $c_time); 
    }
}

$is = new IpSearcher($argv);
$is->run();

?>
