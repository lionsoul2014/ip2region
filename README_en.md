
# What is Ip2region?

Read this in other languages: [简体中文](README.md) | [English](README_en.md)

ip2region - offline IP address location library with 99.9% accuracy, 0.0x millisecond queries, ip2region.db database is only a few MB, provides query bindings for 
```
java|php|c|python|nodejs|golang|c#
```
and three query algorithms for 
```
Binary|B-tree|memory
```
# Ip2region features

### 99.9% accuracy rate

The data is aggregated from some well-known ip to place name lookup providers, these are their official accuracy rates, tested to be actually a bit more accurate than the classic pure IP location. 

The data of ip2region is aggregated from the open API or data of the following service providers (2 to 4 requests per second for the upgrade program): 


01. &gt;80% - [Taobao IP address database](http://ip.taobao.com/) <br />
02. ≈10% - [GeoIP](https://geoip.com/) <br />
03. ≈2% - [Pure IP Library](http://www.cz88.net/) <br />

<b>Remarks:</b> If none of the above open APIs or data give open data ip2region will stop the update service for the data.


### Standardized data format

Each ip data segment has a fixed format of.
```
_cityId|country|region|province|city|ISP_
```

Only the data for China is accurate to the city, some of the data for other countries can only be located to the country, and all the options before the latter are 0, which already contains all the big and small countries you can check (please ignore the city Id in front, personal project requirements).


### Small size

Contains all the IPs, the generated database file ip2region.db is only a few MB, the smallest version is only 1.5 MB, with the increase in the level of detail of the data the size of the database is slowly increasing, currently not more than 8 MB.


### Fast query speed

All of the query client single query are in the 0.x millisecond level, built-in three query algorithms

1. memory algorithm: the entire database is loaded into memory, single query are within 0.1x milliseconds, C language client single query in 0.00x milliseconds level.
2. binary algorithm: based on dichotomous lookup, based on ip2region.db file, no need to load memory, single query in 0.x milliseconds level.
3. b-tree algorithm: based on btree algorithm, based on ip2region.db file, no need to load memory, word query at 0.x milliseconds level, faster than binary algorithm.

Any client b-tree is faster than binary algorithm, of course memory algorithm is certainly the fastest!


### Multi-query client support

Clients already integrated are:
```
java, C#, php, c, python, nodejs, php extensions (php5 and php7), golang, rust, lua, lua_c, nginx.
```
binding | description | development status | binary query time consuming | b-tree query time consuming | memory query time consuming
:-: | :-: | :-: | :-: | :-: | :-:
[c](binding/c) | ANSC c binding | completed | 0.0x ms | 0.0x ms | 0.00x ms
[c#](binding/c#) | c# binding | completed | 0.x ms | 0.x ms | 0.1x ms
[golang](binding/golang) | golang binding | done | 0.x ms | 0.x ms | 0.1x ms
[java](binding/java) | java binding | completed | 0.x ms | 0.x ms | 0.1x ms
[lua](binding/lua) | lua implementation of binding | done | 0.x ms | 0.x ms | 0.x ms
[lua_c](binding/lua_c) | lua's c extension | done | 0.0x ms | 0.0x ms | 0.00x ms
[nginx](binding/nginx) | c extensions for nginx | done | 0.0x ms | 0.0x ms | 0.00x ms
[nodejs](binding/nodejs) | nodejs | completed | 0.x ms | 0.x ms | 0.1x ms
[php](binding/php) | php implementation of binding | done | 0.x ms | 0.1x ms | 0.1x ms
[php5_ext](binding/php5_ext) | c extensions for php5 | done | 0.0x ms | 0.0x ms | 0.00x ms
[php7_ext](binding/php7_ext) | c extensions for php7 | done | 0.0x ms | 0.0x ms | 0.00x ms
[python](binding/python) | python bindng | done | 0.x ms | 0.x ms | 0.x ms
[rust](binding/rust) | rust binding | done | 0.x ms | 0.x ms | 0.x ms




# ip2region quick test

Please refer to the README instructions under each binding to run the cli test program, for example the C demo runs as follows.
```shell
cd binding/c/
gcc -g -O2 testSearcher.c ip2region.c
. /a.out ... /... /data/ip2region.db
```

You will see the following cli interface.
```
initializing B-tree ...
+----------------------------------+
| ip2region test script |
| Author: chenxin619315@gmail.com |
| Type 'quit' to exit program |
+----------------------------------+
p2region>> 101.105.35.57
2163|China|South|Guangdong|Shenzhen|Dr. Peng in 0.02295 millseconds
```

Enter the IP address to start the test, the first time will be slightly slow, after the run command access binary,memory to try other algorithms, it is recommended to use the b-tree algorithm, speed and concurrency requirements can use the memory algorithm, please refer to the test source code under different binding for specific integration.




# ip2region installation

Refer to the README documentation and test demos under each binding for details, and here are some available shortcut installations.

### maven repository address
```xml
<dependency>
    <groupId>org.lionsoul</groupId>
    <artifactId>ip2region</artifactId>
    <version>1.7.2</version>
</dependency>
```

### nodejs
```
npm install node-ip2region --save
```

### nuget install
```shell
NuGet install IP2Region
```

### php composer
```shell
# Plugin from: https://github.com/zoujingli/ip2region
composer require zoujingli/ip2region
```



# ip2region Concurrent use

1. each search interface of all binding is <b>not</b> a thread-safe implementation, different threads can use it by creating different query objects. with high concurrency, the binary and b-tree algorithms may have too many open files error, please modify the kernel's maximum number of open files allowed (fs.file-max=a higher value), or use a persistent memory algorithm.

2. memorySearch interface, which performs a pre-query before publishing the object (essentially loading the ip2region.db file into memory), can be safely used in a multi-threaded environment.




# ip2region.db generation

Starting from version 1.8, ip2region has open sourced the java implementation of the ip2region.db generator, providing ant compilation support, which will result in the dbMaker-{version}.jar mentioned below, for those who need to study the generator or change the custom generation configuration please refer to ${ip2region_ root}/maker/java for the java source code.


Starting with ip2region version 1.2.2 there is an executable jar file dbMaker-{version}.jar committed inside, which is used to do this.

1. make sure you have the java environment installed (kids who don't play with Java will Google themselves to find a pull, temporary use, a matter of minutes)
2. cd to `${ip2region_root}/maker/java` and run the following command.
```shell
java -jar dbMaker-{version}.jar -src text data file -region geographic csv file [-dst directory of the generated ip2region.db file]

# text data file: the original text data file path of the db file, the self-contained ip2region.db file is generated from /data/ip.merge.txt, you can replace it with your own or change /data/ip.merge.txt to regenerate it
# geographic csv file: the purpose of this file is to facilitate the configuration of ip2region for data relationship storage, the data obtained contains a city_id, this can be directly used /data/origin/global_region.csv file
# ip2region.db file directory: is optional, if not specified, a copy will be generated in the current directory. /data/ip2region.db file
```

3. get the generated ip2region.db file to overwrite the original ip2region.db file
4. The default ip2region.db file generation command:

```shell
cd ${ip2region_root}/java/
java -jar dbMaker-1.2.2.jar -src . /data/ip.merge.txt -region . /data/global_region.csv

# You'll see a big chunk of output
```




# Related remarks

### Declaration
ip2region focuses on <b>
researching IP data storage design and query implementation in various languages</b>, there is no original IP data support, please refer to the description above for data sources, upgrading data requires a lot of IP support and will cause a certain amount of request pressure on the original platform, this project does not guarantee timely data updates, there is no and will not be a commercial version, you can use You can use the custom data import ip2region for custom query implementation.

### Technical Communication
1. the structure and principle of the database file, please read @冬芽's blog:

- [ip2region Database File Structure and Principle](https://github.com/dongyado/dongyado.github.io/blob/master/_posts/2016-08-18-structure-of-ip2region-database-file.md) 
- [ip2region Data Structure Design and Implementation Video share](https://www.bilibili.com/video/BV1wv4y1N7SD)

2. ip2region exchange and sharing

- WeChat: lionsoul2014 (please note ip2region)
- QQ: 1187582057 (little attention)

3. based on the detection algorithm of the data update way video sharing: 

- [Data Update to Achieve Video Sharing Part1](https://www.bilibili.com/video/BV1934y1E7Q5/)
- [Data Update to Achieve Video Sharing part2](https://www.bilibili.com/video/BV1pF411j7Aw/)
