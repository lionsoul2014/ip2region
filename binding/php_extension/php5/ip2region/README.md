# ip2region client - PHP扩展(php5版本) 

### 安装步骤
* git clone https://github.com/lionsoul2014/ip2region.git
* cd ip2region
* cp binding/php_extension/php5/ip2region 到 php source code 的 ext 目录下
* cp binding/c/ 里面所有的文件到 php source code 的 ext/ip2region/lib 目录下
* 在 ext/ip2region 下，运行 

        phpize
        ./configure
        make && sudo make install

* 配置 ip2region.ini 指定 db_file 路径，(cli/fpm)
    
        extension=ip2region.so
        ip2region.db_file=/path/to/ip2region.db

默认 ip2region.db 在项目根目录下的 data 文件夹下，如有改动，请修改 ip2region.ini 的 db_file

* 测试 

在 ext/ip2region/ 下运行
    
        php ip2region.php

### 使用

参考当前目录下的 ip2region.php
