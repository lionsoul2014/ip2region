A = @
CFLAGS=	-g
CC=		gcc
LIBS= -I./
LDFLAGS=
RM = rm -f
MV = mv

testSearcher: ip2region.o
	$(A) $(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) -o testSearcher testSearcher.c ip2region.c
	$(A) $(RM) ip2region.o testSearcher.o

db:
	$(A) cd ../../;\
	export LANG=en_US.UTF-8;\
	java -jar dbMaker-1.2.2.jar -src ./data/ip.merge.txt -region ./data/global_region.csv -dst .;\
	$(MV) ./ip2region.db ./binding/c/;\
	cd ./binding/c

all: db testSearcher


clean:
	$(A) $(RM) testSearcher
	
.PHONY: all clean testSearcher db
