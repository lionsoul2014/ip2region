# ip2region 的PHP拓展(php7版本)

### 安装步骤
* git clone https://github.com/lionsoul2016/ip2region.git
* cd ip2region
* cp binding/php_extension/php5/ip2region 到 php source code 的ext目录下
* cp binding/c/下面所有的文件到 php source code 的ext/ip2region/lib 目录下
* 在ext/ip2region下，运行 

        phpize
        ./configure
        make && sudo make install

* 配置ip2region.ini 指定db_file路径，(cli/fpm)
    
        extension=ip2region.so
        ip2region.db_file=/path/to/ip2region.db

ip2region.db 在项目根目录下的data文件夹下

* 测试 

在ext/ip2region/下运行
    
        php ip2region.php

### 使用

参考ip2region/ip2region.php
