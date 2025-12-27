
all: bin header search bench make edit

FILES=$(wildcard src/*.cc)

bin:
	mkdir -p bin

header: $(FILES) test/header.cc
	g++ -std=c++11 -O2 $^ -o bin/$@

search: $(FILES) test/search.cc
	g++ -std=c++11 -O2 $^ -o bin/$@

bench: $(FILES) test/bench.cc
	g++ -std=c++11 -O2 $^ -o bin/$@

make: $(FILES) test/make.cc
	g++ -std=c++11 -O2 $^ -o bin/$@

edit: $(FILES)
	g++ -std=c++11 -O2 $^ test/edit_v4.cc -o bin/edit_v4
	g++ -std=c++11 -O2 $^ test/edit_v6.cc -o bin/edit_v6

clean:
	rm -rf bin

