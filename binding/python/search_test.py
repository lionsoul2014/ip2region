# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# xdb searcher test on 2025/10/30
# Author Leon<chenxin619315@gmail.com>

import io
import sys
import argparse
import time
import ip2region.util as util
import ip2region.searcher as xdb

def create_searcher(db_path, cache_policy):
    # open the source xdb file
    handle = io.open(db_path, "rb")

    # verify the xdb file
    # @Note: do NOT call it every time you create a searcher since this will slow
    # down the search response.
    # @see the verify function for details.
    util.verify(handle)

    # get the ip version from header
    header = util.load_header(handle)
    version = util.version_from_header(header)
    if version is None:
        handle.close()
        raise Exception("failed to get version from header")

    searcher = None
    if cache_policy == "file":
        searcher = xdb.new_with_file_only(version, db_path)
    elif cache_policy == "vectorIndex":
        v_index = util.load_vector_index(handle)
        searcher = xdb.new_with_vector_index(version, db_path, v_index)
    elif cache_policy == "content":
        c_buffer = util.load_content(handle)
        searcher = xdb.new_with_buffer(version, c_buffer)
    else:
        raise ValueError("invalid cache_policy `{}`".format(cache_policy))

    handle.close()
    return searcher


def run_search_test(db_path: str, cache_policy: str):
    # create the searcher
    searcher = None
    try:
        searcher = create_searcher(args.db, args.cache_policy)
    except Exception as e:
        print("failed to create searcher: {}".format(str(e)))
        return

    # print the searcher for debug
    # print("searcher -> ", searcher)
    print('''ip2region xdb searcher test program
source xdb: {} ({}, {})
type 'quit' to exit'''.format(db_path, searcher.get_ip_version().name, cache_policy))

    # get input ip address and do the search
    while True:
        ip_str = input("ip2region>> ").strip()

        if len(ip_str) < 2:
            continue
        if ip_str == "quit":
            break

        s_time = time.time()
        try:
            ip_bytes = util.parse_ip(ip_str)
        except Exception as e:
            print(f"invalid ip address `{ip_str}`")
            continue
        
        try:
            region = searcher.search(ip_bytes)
        except Exception as e:
            print("failed to search({}): {}".format(util.ip_to_string(ip_bytes), str(e)))
            continue

        took = (time.time() - s_time) * 1000
        print(f"{{region: {region}, ioCount: {searcher.get_io_count()}, took: {took:.3f} ms}}");

    # close the searcher
    searcher.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        add_help=True,
        prog="python search_test.py",
        description="ip2region search test script",
        usage="%(prog)s [command option]"
    )

    # check the args
    parser.add_argument('--db', help='ip2region binary xdb file path')
    parser.add_argument('--cache-policy', help='cache policy: file/vectorIndex/content, default: vectorIndex', default="vectorIndex")
    args = parser.parse_args()
    if args.db is None:
        parser.print_help()
        sys.exit()

    # run the search test
    run_search_test(args.db, args.cache_policy)