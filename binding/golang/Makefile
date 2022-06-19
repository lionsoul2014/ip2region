# ip2region golang binding makefile
all: build
.PHONY: all

build:
	go build -o xdb_searcher
test:
	go test -v ./...
clean:
	find ./ -name xdb_searcher | xargs rm -f