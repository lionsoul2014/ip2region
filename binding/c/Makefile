all: xdb_searcher test_util

xdb_searcher: xdb_api.h xdb_util.c xdb_searcher.c main.c
	gcc -Wall -O2 -I./ xdb_util.c xdb_searcher.c main.c -o xdb_searcher

test_util: xdb_api.h xdb_util.c test_util.c
	gcc -Wall -O2 -I./ xdb_util.c test_util.c -o test_util

xdb_searcher.o: xdb_searcher.c
	gcc -Wall -c xdb_searcher.c

xdb_util.o: xdb_util.c
	gcc -Wall -c xdb_util.c

xdb_searcher_lib: xdb_util.o xdb_searcher.o
	mkdir -p build/lib
	mkdir -p build/include
	ar -rc build/lib/libxdb.a `find . -name *.o`
	cp xdb_api.h build/include

clean:
	find ./ -name \*.o  | xargs rm -f
	find ./ -name test_util | xargs rm -f
	find ./ -name xdb_searcher | xargs rm -f
	rm -rf build

.PHONY: all clean xdb_searcher test_util
