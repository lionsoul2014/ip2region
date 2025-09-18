ARG NGINX_VERSION=1.27.0
FROM nginx:${NGINX_VERSION} AS build

# 嫌更新慢,可以试试这个镜像站地址
# sed -i 's/deb.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list.d/debian.sources && \
RUN apt-get update && \
    apt-get install -y build-essential libpcre3-dev zlib1g-dev libssl-dev git

WORKDIR /usr/src
RUN curl -O http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-${NGINX_VERSION}.tar.gz && \
    git clone --depth=1 https://github.com/lionsoul2014/ip2region.git && \
    # 生成configure参数, 最好与nginx原始配置参数一致, 不然模块会出现兼容问题 \
    nginx -V 2>&1 | grep 'configure arguments' | sed 's/ --/ \\\n  --/g' | sed "s/pie'/pie' \\\/g" | grep -v 'configure arguments' >> /tmp/conf_arg && \
    echo \
    ' --add-dynamic-module=$(pwd)/../ip2region/binding/nginx \\\n' \
    ' --with-cc-opt="-I $(pwd)/../ip2region/binding/c/build/include" \\\n' \
    ' --with-ld-opt="-L $(pwd)/../ip2region/binding/c/build/lib"' >> /tmp/conf_arg

WORKDIR /usr/src/ip2region/binding/c
RUN make xdb_searcher_lib

WORKDIR /usr/src/nginx-${NGINX_VERSION}
RUN cat /tmp/conf_arg && \
    bash -c 'eval "./configure $(cat /tmp/conf_arg)"' && \
    make modules && \
    cp /usr/src/nginx-${NGINX_VERSION}/objs/ngx_http_ip2region_module.so /

# 用于builx导出
FROM scratch AS export_so
COPY --from=build /ngx_http_ip2region_module.so /

# 第三阶段,只把编译出来兼容版本的模块添加进自定义 nginx 镜像,节省空间
FROM nginx:${NGINX_VERSION}
COPY --from=build /usr/src/nginx-${NGINX_VERSION}/objs/ngx_http_ip2region_module.so /etc/nginx/modules/

CMD ["nginx", "-g", "daemon off;"]
