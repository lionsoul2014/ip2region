cmake_minimum_required(VERSION 3.16)

project(ip2region_c LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

if(MSVC)
    add_compile_options(/W4)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

add_library(xdb xdb_util.c xdb_searcher.c)
target_include_directories(xdb PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

if(WIN32)
    target_link_libraries(xdb PUBLIC ws2_32)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
foreach(config DEBUG RELEASE RELWITHDEBINFO MINSIZEREL)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config} "${CMAKE_BINARY_DIR}")
endforeach()

add_executable(xdb_searcher main.c)
target_link_libraries(xdb_searcher PRIVATE xdb)

add_executable(test_util test_util.c)
target_link_libraries(test_util PRIVATE xdb)

enable_testing()

foreach(test load_header load_vector_index load_content parse_ip ip_compare)
    add_test(NAME c.${test} COMMAND test_util test_${test})
    set_tests_properties(c.${test} PROPERTIES WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endforeach()
