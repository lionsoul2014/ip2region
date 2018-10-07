# ip2region luc c module Make file
# @author   chenxin<chenxin619315@gmail.com>
# @date     2018/10/05

# @Note
# Please modify the LIBS and the LIB_DIR to fit you system
# 

VER = 5.2
CC = gcc
LIBS = -I ../c/ -I /usr/include/lua$(VER)/
FFLAGS = -O2 -Wall -fPIC
SO_FILE = Ip2region.so
LIB_DIR = /usr/local/share/lua/$(VER)

all: ../c/ip2region.c ../c/ip2region.h lua_ip2region.c
	$(CC) $(FFLAGS) $(LIBS) ../c/ip2region.c lua_ip2region.c -fPIC -shared -o $(SO_FILE)

install:
	sudo mkdir -p $(LIB_DIR); \
	sudo cp $(SO_FILE) $(LIB_DIR);\
	echo "install Ip2region to $(LIB_DIR) successfully.";\

clean:
	find . -name \*.so | xargs rm -f
	find . -name \*.o  | xargs rm -f

.PHONY: clean
