all: xdb_searcher util_test

xdb_searcher: xdb_searcher.h xdb_searcher.c main.c
	gcc -O2 -I./ xdb_searcher.c main.c -o xdb_searcher

util_test: xdb_searcher.h xdb_searcher.c util_test.c
	gcc -O2 -I./ xdb_searcher.c util_test.c -o util_test

xdb_searcher.o: xdb_searcher.c
	gcc -c -o xdb_searcher.o xdb_searcher.c

xdb_searcher_lib: xdb_searcher.o
	mkdir -p build/lib
	mkdir -p build/include
	ar -rc build/lib/libxdb_searcher.a `find . -name *.o`
	cp xdb_searcher.h build/include

clean:
	find ./ -name \*.o  | xargs rm -f
	find ./ -name util_test | xargs rm -f
	find ./ -name xdb_searcher | xargs rm -f
	rm -rf build

.PHONY: all clean xdb_searcher util_test
