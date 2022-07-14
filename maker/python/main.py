# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
# Author: linyufeng <leolin49@foxmail.com>
# Date  : 2022/7/14 17:00
#
import logging
import sys
import time

import xdb.maker as mk
import xdb.index as idx

# Format log
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s-%(name)s-%(lineno)s-%(levelname)s - %(message)s",
)
log = logging.getLogger(__name__)


def print_help():
    print("ip2region xdb python maker")
    print("{} [command] [command options]".format(sys.argv[0]))
    print("Command: ")
    print("  gen      generate the binary db file")


def gen_db():
    src_file, dst_file = "", ""
    index_policy = idx.Vector_Index_Policy
    # Check input parameters
    for i in range(2, len(sys.argv)):
        r = sys.argv[i]
        if len(r) < 5:
            continue
        if not r.startswith("--"):
            continue
        s_idx = r.index("=")
        if s_idx < 0:
            print("missing = for args pair '{}'".format(r))
            return
        if r[2:s_idx] == "src":
            src_file = r[s_idx + 1:]
        elif r[2:s_idx] == "dst":
            dst_file = r[s_idx + 1:]
        elif r[2:s_idx] == "index":
            index_policy = idx.index_policy_from_string(r[s_idx + 1:])
        else:
            print("undefined option `{}`".format(r))
            return
    if src_file == "" or dst_file == "":
        print("{} gen [command options]".format(sys.argv[0]))
        print("options:")
        print(" --src string    source ip text file path")
        print(" --dst string    destination binary xdb file path")
        return

    start_time = time.time()
    # Make the binary file
    maker = mk.new_maker(index_policy, src_file, dst_file)
    maker.init()
    maker.start()
    maker.end()

    logging.info(
        "Done, elapsed: {:.0f}m{:.0f}s".format(
            (time.time() - start_time) / 60, (time.time() - start_time) % 60
        )
    )


def main():
    if len(sys.argv) < 2:
        print_help()
        return

    cmd = sys.argv[1].lower()
    if cmd == "gen":
        gen_db()
    else:
        print_help()


if __name__ == "__main__":
    main()
