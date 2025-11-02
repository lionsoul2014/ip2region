# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# xdb searcher bench test on 2025/10/30
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


def run_bench_test(db_path: str, src_path: str, cache_policy: str):
    # create the searcher
    searcher = None
    try:
        searcher = create_searcher(args.db, args.cache_policy)
    except Exception as e:
        print("failed to create searcher: {}".format(str(e)))
        return

    print("searcher ->", searcher)
    # read the source lines and do the search test
    count, total_secs = 0, 0
    try:
        handle = io.open(src_path, "rb")
        while True:
            line = handle.readline().decode("utf-8").strip()
            # ignore empty  or comment line
            if len(line) < 1:
                break

            if line[0] == "#":
                continue

            # line splits
            ps = line.split("|", 2)
            if len(ps) != 3:
                raise Exception(f"invalid ip segment line `{line}`")

            # ip parse and compare
            start_time = time.time()
            sip_bytes = util.parse_ip(ps[0])
            eip_bytes = util.parse_ip(ps[1])
            if util.ip_compare(sip_bytes, eip_bytes) > 0:
                raise Exception(f"start ip({ps[0]}) should not be greater than end ip({ps[1]})")

            # do the search test
            for ip_bytes in [sip_bytes, eip_bytes]:
                count  = count + 1
                region = searcher.search(ip_bytes)
                if region != ps[2]:
                    raise Exception(f"failed to search({util.ip_to_string(ip_bytes)}) with {region} != {ps[2]}")
            
            total_secs = total_secs + (time.time() - start_time)

        # resource cleanup
        handle.close()
    except Exception as e:
        print(f"bench failed: {str(e)}")
        return

    # print the stats info
    each_us = total_secs * 1000_000 / count
    print(f"Bench finished, {{cachePolicy: {cache_policy}, total: {count}, took: {total_secs:.3f} s, cost: {each_us:.0f} μs/op}}");

    # resource cleanup
    searcher.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        add_help=True,
        prog="python bench_test.py",
        description="ip2region bench test script",
        usage="%(prog)s [command option]"
    )

    # check the args
    parser.add_argument('--db', help='ip2region binary xdb file path')
    parser.add_argument('--src', help='source ip text file path')
    parser.add_argument('--cache-policy', help='cache policy: file/vectorIndex/content, default: vectorIndex', default="vectorIndex")
    args = parser.parse_args()
    if (args.db is None) or (args.src is None):
        parser.print_help()
        sys.exit()

    # run the search test
    run_bench_test(args.db, args.src, args.cache_policy)