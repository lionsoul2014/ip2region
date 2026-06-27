#!/bin/bash
set -e

# Run from binding/erlang so paths and rebar3 artifacts are correct.
cd "$(dirname "$0")/.."

VERSION="${1:-ipv4}"
PROJECT_ROOT="$(cd ../.. && pwd)"

if [ "$VERSION" = "ipv6" ]; then
    DB_FILE="${PROJECT_ROOT}/data/ip2region_v6.xdb"
    DATA_FILE="${PROJECT_ROOT}/data/ipv6_source.txt"
    DB_CONFIG="[{ipv4, \"ip2region.xdb\"}, {ipv6, \"${DB_FILE}\"}]"
elif [ "$VERSION" = "ipv4" ]; then
    DATA_FILE="${PROJECT_ROOT}/data/ipv4_source.txt"
    DB_CONFIG="[{ipv4, \"ip2region.xdb\"}]"
else
    echo "Usage: $0 [ipv4|ipv6]"
    exit 1
fi

rebar3 compile

# shellcheck disable=SC2089
EVAL="application:load(ip2region), \
application:set_env(ip2region, db, ${DB_CONFIG}), \
application:set_env(ip2region, poolargs, [{size,4},{max_overflow,8}]), \
{ok,_}=application:ensure_all_started(ip2region), \
xdb_benchmark:main(\"${DATA_FILE}\"), \
init:stop()."

# shellcheck disable=SC2090
erl -pa _build/default/lib/poolboy/ebin \
    -pa _build/default/lib/ip2region/ebin \
    -kernel logger_level error \
    -noshell \
    -eval "${EVAL}"
