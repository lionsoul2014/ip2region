# nginx-ip2region

## 编译

```shell
$ mkdir ip2region
$ wget https://nginx.org/download/nginx-1.23.4.tar.gz
$ tar -zxf nginx-1.23.4.tar.gz && rm -rf nginx-1.23.4.tar.gz
$ git clone https://github.com/lionsoul2014/ip2region.git
$ cd nginx-1.23.4
$ ./configure \
    --add-module=$(PWD)/../ip2region/binding/nginx
$ make
$ make install
```

## Nginx 配置

> Syntax:  `ip2region_db xdb_file_path` [cache_policy 可选];
> Context: http

cache_policy: `file` or `vectorIndex` or `content`, default: `content`

```nginx
...
http {

    log_format main escape=json '{'
                                '"remote_addr": "$remote_addr", '
                                '"region": "$ip2region", '
                                '"http_x_forwarded_for": "$http_x_forwarded_for"'
                                '}';

    access_log logs/access.log main;

    # 设置xdb文件路径
    ip2region_db ip2region.xdb;

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

$ ./configure \
    --prefix=$(PWD)/../build \
    --add-module=$(PWD)/../ip2region/binding/nginx

Made with ♥ by Wu Jian Ping