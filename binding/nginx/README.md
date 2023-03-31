# nginx-ip2region

## build

```shell
$ mkdir -p workspace
$ cd workspace
$ wget https://nginx.org/download/nginx-1.23.4.tar.gz
$ tar -zxf nginx-1.23.4.tar.gz && rm -rf nginx-1.23.4.tar.gz
$ git clone https://github.com/lionsoul2014/ip2region.git
$ cd ip2region/binding/c
$ make xdb_searcher_lib
$ cd ../../../nginx-1.23.4
$ ./configure                                                            \
    --add-module=$(PWD)/../ip2region/binding/nginx                       \
    --with-cc-opt="-I $(PWD)/../ip2region/binding/c/build/include"       \
    --with-ld-opt="-L$(PWD)/../ip2region/binding/c/build/lib"
$ make
$ make install
```

## nginx conf

> Syntax:  `ip2region_db xdb_file_path [cache_policy Optional]`;
> Context: http

cache_policy: `file` or `vectorIndex` or `content`, default: `content`

Edit `nginx.conf` add `ip2region_db` directive

```nginx
...
http {

    log_format main escape=json '{'
                                '"remote_addr": "$remote_addr", '
                                '"region": "$ip2region", '
                                '"http_x_forwarded_for": "$http_x_forwarded_for"'
                                '}';

    access_log logs/access.log main;

    # set xdb file path
    ip2region_db ip2region.xdb;
    # ip2region_db ip2region.xdb vectorIndex;
    # ip2region_db ip2region.xdb file;
    # ip2region_db ip2region.xdb content;

    server {
        listen 80;
        server_name localhost;

        location / {
            root html;
            index index.html index.htm;
        }
    }
}

```

Copy `ip2region.xdb` to `nginx/config` folder, then restart nginx, the `region` data stored in `ip2region` variable

nginx access log sample

```log
{"remote_addr": "127.0.0.1", "region": "0|0|0|内网IP|内网IP", "http_x_forwarded_for": ""}
{"remote_addr": "127.0.0.1", "region": "0|0|0|内网IP|内网IP", "http_x_forwarded_for": ""}

```

Made with ♥ by Wu Jian Ping
