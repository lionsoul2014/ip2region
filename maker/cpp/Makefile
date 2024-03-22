
all: xdb_make xdb_edit

xdb_make: xdb_make.cc xdb_make_test.cc
	g++ -std=c++11 -O2 $^ -o $@

xdb_edit: xdb_edit.cc xdb_edit_test.cc
	g++ -std=c++11 -O2 $^ -o $@

clean:
	rm -f xdb_make xdb_edit
