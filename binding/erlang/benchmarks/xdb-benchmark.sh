#!/bin/bash

cd ..

rebar3 shell --eval="xdb_benchmark:main(\"../../data/ip.merge.txt\"), init:stop()."
