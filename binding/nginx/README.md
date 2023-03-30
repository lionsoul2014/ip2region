# nginx-ip2region

## 编译

```shell
$ ./configure \
    --prefix=$(PWD)/../build \
    --add-module=$(PWD)/../ip2region/binding/nginx

$ make
$ make install
```

```nginx
...
http {

    log_format json_access_log escape=json '{'
                                           '"remote_addr": "$remote_addr", '
                                           '"region": "$ip2region", '
                                           '"http_x_forwarded_for": "$http_x_forwarded_for"'
                                           '}';

    access_log logs/access.log json_access_log;

    # 设置xdb文件路径
    ip2region ip2region.xdb;

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

Made with ♥ by Wu Jian Ping
