LuaVersion ?= 5.4
LUA_INCDIR ?= /usr/include/lua$(LuaVersion)
CFLAGS ?= -std=c99 -Wall -O2
LIB_DIR = /usr/local/share/lua/$(LuaVersion)

ifeq ($(shell uname -s),Darwin)
	SHARED = -bundle -undefined dynamic_lookup
else
	SHARED = -shared
endif

all: ../c/xdb_api.h ../c/xdb_util.c ../c/xdb_searcher.c xdb_searcher.c
	gcc $(CFLAGS) -I../c/ -I$(LUA_INCDIR) ../c/xdb_util.c ../c/xdb_searcher.c xdb_searcher.c -fPIC $(SHARED) -o xdb_searcher.so

install:
	sudo mkdir -p $(LIB_DIR); \
	sudo cp xdb_searcher.so $(LIB_DIR);\
	echo "install xdb searcher to $(LIB_DIR) successfully.";\

clean:
	find . -name \*.so | xargs rm -f
	find . -name \*.o  | xargs rm -f

.PHONY: clean
