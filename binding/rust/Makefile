# ip2region rust binding makefile
all: build
.PHONY: all

build:
	cargo build -r
test:
	cargo test
bench:
	cargo bench
clean:
	find ./ -name xdb_searcher | xargs rm -f