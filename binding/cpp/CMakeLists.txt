cmake_minimum_required(VERSION 3.16)

project(ip2region_cpp LANGUAGES CXX)

option(IP2REGION_BUILD_TOOLS "Build the C++ command-line tools" ON)

add_library(
    ip2region_xdb
    src/base.cc
    src/bench.cc
    src/edit.cc
    src/header.cc
    src/ip.cc
    src/make.cc
    src/search.cc
)

target_compile_features(ip2region_xdb PUBLIC cxx_std_11)
target_include_directories(ip2region_xdb PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")

if(MSVC)
    target_compile_options(ip2region_xdb PRIVATE /W4 /permissive-)
else()
    target_compile_options(ip2region_xdb PRIVATE -Wall -Wextra -Wpedantic)
endif()

if(WIN32)
    target_compile_definitions(ip2region_xdb PRIVATE _WIN32_WINNT=0x0600)
    target_link_libraries(ip2region_xdb PUBLIC ws2_32)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")
foreach(config DEBUG RELEASE RELWITHDEBINFO MINSIZEREL)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config} "${CMAKE_CURRENT_SOURCE_DIR}/bin")
endforeach()

function(add_xdb_executable target source output_name)
    add_executable(${target} ${source})
    target_link_libraries(${target} PRIVATE ip2region_xdb)
    set_target_properties(${target} PROPERTIES OUTPUT_NAME ${output_name})
endfunction()

if(IP2REGION_BUILD_TOOLS)
    add_xdb_executable(xdb_header test/header.cc header)
    add_xdb_executable(xdb_search test/search.cc search)
    add_xdb_executable(xdb_bench test/bench.cc bench)
    add_xdb_executable(xdb_make test/make.cc make)
    add_xdb_executable(edit_v4 test/edit_v4.cc edit_v4)
    add_xdb_executable(edit_v6 test/edit_v6.cc edit_v6)
endif()

include(CTest)

if(BUILD_TESTING)
    add_xdb_executable(ip_test test/ip.cc ip_test)

    add_test(NAME cpp.ip COMMAND ip_test)
    set_tests_properties(cpp.ip PROPERTIES WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

    if(IP2REGION_BUILD_TOOLS)
        add_test(NAME cpp.header COMMAND xdb_header)
        add_test(NAME cpp.search COMMAND xdb_search)
        add_test(NAME cpp.make COMMAND xdb_make)

        set_tests_properties(
            cpp.header cpp.search cpp.make
            PROPERTIES WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        )
    endif()
endif()
