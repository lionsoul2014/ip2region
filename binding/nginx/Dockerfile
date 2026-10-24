ARG NGINX_VERSION=1.29.6
FROM nginx:${NGINX_VERSION} AS build
ARG NGINX_VERSION

# prepare the build environment
RUN apt-get update && \
    apt-get install -y build-essential libpcre2-dev zlib1g-dev libssl-dev git

WORKDIR /usr/src
RUN curl -LO https://github.com/nginx/nginx/releases/download/release-${NGINX_VERSION}/nginx-${NGINX_VERSION}.tar.gz && \
    tar -zxf nginx-$NGINX_VERSION.tar.gz

COPY . /usr/src/ip2region

WORKDIR /usr/src/ip2region/binding/c
RUN make xdb_searcher_lib

# parameters for building dynamic modules
WORKDIR /usr/src
RUN nginx -V 2>&1 | grep 'configure arguments' | sed 's/ --/ \\\n  --/g' | sed "s/pie'/pie' \\\/g" | grep -v 'configure arguments' >> /tmp/conf_arg

RUN echo \
    '  --add-dynamic-module=$(pwd)/../ip2region/binding/nginx \\\n' \
    ' --with-cc-opt="-I $(pwd)/../ip2region/binding/c/build/include" \\\n' \
    ' --with-ld-opt="-L $(pwd)/../ip2region/binding/c/build/lib"' >> /tmp/conf_arg
RUN cat  /tmp/conf_arg

WORKDIR /usr/src/nginx-$NGINX_VERSION
RUN eval "./configure $(cat /tmp/conf_arg)"

RUN make modules && \
    cp objs/ngx_http_ip2region_module.so /etc/nginx/modules

# static analysis of the module with its nginx compile command
FROM build AS lint
ARG NGINX_VERSION
RUN apt-get install -y bear clang-tidy
WORKDIR /usr/src/nginx-$NGINX_VERSION
RUN bear -- make -B modules && \
    clang-tidy -p . --warnings-as-errors='*' /usr/src/ip2region/binding/nginx/src/ngx_http_ip2region_module.c

# run the Test::Nginx suite against the module
FROM build AS test
RUN apt-get install -y cpanminus && \
    cpanm --notest Test::Nginx@0.32
WORKDIR /usr/src/ip2region/binding/nginx
CMD ["prove", "t"]

# for buildx export
FROM scratch AS export_so
ARG NGINX_VERSION
COPY --from=build /etc/nginx/modules/ngx_http_ip2region_module.so /ngx_http_ip2region_module.so
