LuaVersion = 5.3
LIB_DIR = /usr/local/share/lua/$(LuaVersion)

all: ../c/xdb_searcher.h ../c/xdb_searcher.c xdb_searcher.c
	gcc -g -O2 -I../c/ -I/usr/include/lua$(LuaVersion) ../c/xdb_searcher.c xdb_searcher.c -fPIC -shared -o xdb_searcher.so

install:
	sudo mkdir -p $(LIB_DIR); \
	sudo cp xdb_searcher.so $(LIB_DIR);\
	echo "install xdb searcher to $(LIB_DIR) successfully.";\

clean:
	find . -name \*.so | xargs rm -f
	find . -name \*.o  | xargs rm -f

.PHONY: clean