
all: xdb_search xdb_bench

xdb_search: xdb_search.cc xdb_search_test.cc
	g++ -std=c++11 -O2 $^ -o $@

xdb_bench: xdb_search.cc xdb_bench.cc xdb_bench_test.cc
	g++ -std=c++11 -O2 $^ -o $@

clean:
	rm -f xdb_search xdb_bench
