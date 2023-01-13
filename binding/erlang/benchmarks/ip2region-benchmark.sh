#!/bin/bash

awk -v FS='|' '{print $1}' ../../../data/ip.merge.txt > test_data.txt

cd ..

rebar3 shell --eval="ip2region_benchmark:main(\"./benchmarks/test_data.txt\"), init:stop()."
