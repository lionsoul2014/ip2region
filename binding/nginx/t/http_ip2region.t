use Test::Nginx::Socket 'no_plan';

my $data = '/usr/src/ip2region/data';

add_block_preprocessor(sub {
    my $block = shift;

    if (!defined $block->main_config) {
        $block->set_value('main_config',
            'load_module /etc/nginx/modules/ngx_http_ip2region_module.so;');
    }

    if (!defined $block->http_config) {
        $block->set_value('http_config', <<"_EOC_");
set_real_ip_from 127.0.0.1;
real_ip_header X-Real-IP;
ip2region_db $data/ip2region_v4.xdb;
ip2region_db6 $data/ip2region_v6.xdb;
_EOC_
    }

    if (!defined $block->config) {
        $block->set_value('config', <<'_EOC_');
location = /region {
    return 200 $ip2region;
}
_EOC_
    }

    if (!defined $block->request) {
        $block->set_value('request', 'GET /region');
    }
});

run_tests();

__DATA__

=== TEST 1: IPv4 address
--- more_headers
X-Real-IP: 1.0.1.1
--- response_body chomp
中国|福建省|福州市|中国电信|CN



=== TEST 2: IPv6 address
--- more_headers
X-Real-IP: 2001:200:101::1
--- response_body chomp
Japan|Tokyo|Tokyo|WIDE Project|JP



=== TEST 3: IPv4-mapped IPv6 address uses the IPv4 database
--- more_headers
X-Real-IP: ::ffff:1.0.0.1
--- response_body chomp
Australia|Queensland|0|0|AU



=== TEST 4: vectorIndex cache policy
--- http_config
set_real_ip_from 127.0.0.1;
real_ip_header X-Real-IP;
ip2region_db /usr/src/ip2region/data/ip2region_v4.xdb vectorIndex;
--- more_headers
X-Real-IP: 1.0.1.1
--- response_body chomp
中国|福建省|福州市|中国电信|CN



=== TEST 5: file cache policy
--- http_config
set_real_ip_from 127.0.0.1;
real_ip_header X-Real-IP;
ip2region_db6 /usr/src/ip2region/data/ip2region_v6.xdb file;
--- more_headers
X-Real-IP: 2001:200:101::1
--- response_body chomp
Japan|Tokyo|Tokyo|WIDE Project|JP



=== TEST 6: IPv6 address without an IPv6 database
--- http_config
set_real_ip_from 127.0.0.1;
real_ip_header X-Real-IP;
ip2region_db /usr/src/ip2region/data/ip2region_v4.xdb;
--- more_headers
X-Real-IP: 2001:200:101::1
--- response_body eval
""



=== TEST 7: ip2region_db rejects an IPv6 database
--- http_config
ip2region_db /usr/src/ip2region/data/ip2region_v6.xdb;
--- must_die
--- error_log
ip2region_db expects IPv4 xdb file, but got IPv6
