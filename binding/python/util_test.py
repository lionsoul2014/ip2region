# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# util test script on 2025/10/29
# Author Leon<chenxin619315@gmail.com>

import os
import sys
import time
from xdb import util

script_dir = os.path.dirname(__file__)
data_dir = os.path.join(script_dir, '../../data/')
xdb_v4_path = os.path.join(data_dir, "ip2region_v4.xdb")
xdb_v6_path = os.path.join(data_dir, "ip2region_v6.xdb")

# print(script_dir, data_dir, xdb_v4_path, xdb_v6_path)
def test_version():
    print("1, version contants: ")
    print("IPv4 -> ", util.IPv4)
    print("IPv6 -> ", util.IPv6)

    # version from name
    print("2, version from name: ")
    for name in ["v4", "IPv4", "v4x", "v6", "IPv6", "v6x"]:
        print("version_from_name({}) -> ".format(name), util.version_from_name(name))

    # version from header
    print("3, version from header: ")
    v4_header = util.load_header_from_file(xdb_v4_path)
    v6_header = util.load_header_from_file(xdb_v6_path)
    print("version_from_header(v4_header) -> ", util.version_from_header(v4_header))
    print("version_from_header(v6_header) -> ", util.version_from_header(v6_header))

def test_load_header():
    v4_header = util.load_header_from_file(xdb_v4_path)
    v6_header = util.load_header_from_file(xdb_v6_path)
    print("v4_header -> ", v4_header)
    print("v6_header -> ", v6_header)

def test_load_vector_index():
    v4_v_index = util.load_vector_index_from_file(xdb_v4_path)
    v6_v_index = util.load_vector_index_from_file(xdb_v6_path)
    print("v4_v_index.length={}".format(len(v4_v_index)))
    print("v6_v_index.length={}".format(len(v6_v_index)))

def test_load_content():
    v4_content = util.load_content_from_file(xdb_v4_path)
    v6_content = util.load_content_from_file(xdb_v6_path)
    print("v4_content.length={}".format(len(v4_content)))
    print("v6_content.length={}".format(len(v6_content)))

if __name__ == "__main__":
    # check and call the specified function
    if len(sys.argv) < 2:
        sys.exit("please specified the function to test")
    
    func = sys.argv[1]
    all_ids = globals()
    if func in all_ids and callable(all_ids[func]):
        print("+---calling test function {} ...".format(func))
        s_time = time.time()
        all_ids[func]()
        c_time = time.time() - s_time
        print(f"|---Done, elapsed {c_time:.6f}s")
    else:
        sys.exit("unable to call function {}".format(func))