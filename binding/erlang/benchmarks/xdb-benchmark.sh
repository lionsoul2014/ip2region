#!/bin/bash
set -e

# Run from binding/erlang so paths and rebar3 artifacts are correct.
cd "$(dirname "$0")/.."

VERSION="${1:-ipv4}"
PROJECT_ROOT="$(cd ../.. && pwd)"

if [ "$VERSION" = "ipv6" ]; then
    DB_FILE="${PROJECT_ROOT}/data/ip2region_v6.xdb"
    DATA_FILE="${PROJECT_ROOT}/data/ipv6_source.txt"
elif [ "$VERSION" = "ipv4" ]; then
    DB_FILE="${PROJECT_ROOT}/data/ip2region_v4.xdb"
    DATA_FILE="${PROJECT_ROOT}/data/ipv4_source.txt"
else
    echo "Usage: $0 [ipv4|ipv6]"
    exit 1
fi

rebar3 compile

erl -pa _build/default/lib/poolboy/ebin \
    -pa _build/default/lib/ip2region/ebin \
    -kernel logger_level error \
    -noshell \
    -run xdb_benchmark main "${VERSION}" "${DB_FILE}" "${DATA_FILE}" \
    -s init stop
