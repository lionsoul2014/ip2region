all: xdb_searcher util_test

xdb_searcher: xdb_searcher.h xdb_searcher.c main.c
	gcc -g -O2 -I./ xdb_searcher.c main.c -o xdb_searcher

util_test: xdb_searcher.h xdb_searcher.c util_test.c
	gcc -g -O2 -I./ xdb_searcher.c util_test.c -o util_test

clean:
	find ./ -name \*.o  | xargs rm -f
	find ./ -name util_test | xargs rm -f
	find ./ -name xdb_searcher | xargs rm -f

.PHONY: all clean xdb_searcher util_test