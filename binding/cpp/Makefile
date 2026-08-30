
CMAKE ?= cmake
BUILD_DIR ?= build
BUILD_TYPE ?= Release

.PHONY: all configure header search bench make edit test clean

all: configure
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE) --parallel

configure:
	$(CMAKE) -S . -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)

header search bench make: configure
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE) --target xdb_$@ --parallel

edit: configure
	$(CMAKE) --build $(BUILD_DIR) --config $(BUILD_TYPE) --target edit_v4 edit_v6 --parallel

test: all
	ctest --test-dir $(BUILD_DIR) -C $(BUILD_TYPE) --output-on-failure

clean:
	$(CMAKE) -E remove_directory $(BUILD_DIR)
	$(CMAKE) -E remove_directory bin
