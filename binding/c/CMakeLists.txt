cmake_minimum_required(VERSION 3.22)
project(c C)

set(CMAKE_C_STANDARD 11)

add_executable(c
        xdb_searcher.c xdb_searcher.h)
