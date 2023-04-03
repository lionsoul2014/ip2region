use lib 'lib';
use Test::Nginx::Socket; # 'no_plan';

repeat_each(2);

plan tests => repeat_each() * 124;

no_long_string();
#no_diff;

run_tests();

__DATA__

=== TEST 1: set request header at client side
--- config
    location /foo {
        echo $http_x_foo;
    }
--- request
    GET /foo
--- more_headers
X-Foo: blah
--- response_headers
! X-Foo
--- response_body
blah
