.PHONY: all compile test clean bench-v4 bench-v6

all: compile

compile:
	rebar3 compile

test:
	rebar3 eunit

clean:
	rebar3 clean

bench-v4:
	sh benchmarks/xdb-benchmark.sh ipv4

bench-v6:
	sh benchmarks/xdb-benchmark.sh ipv6
