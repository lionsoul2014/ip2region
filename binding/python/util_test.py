# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# util test script on 2025/10/29
# Author Leon<chenxin619315@gmail.com>

import os
import sys
import time
import ip2region.util as util
import ip2region.searcher as xdb

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

def test_verify():
    # v4 xdb verify
    try:
        util.verify_from_file(xdb_v4_path)
    except Exception as e:
        print("failed to verify the xdb file `{}`: {}".format(xdb_v4_path, str(e)))
    else:
        print("xdb file `{}` verified".format(xdb_v4_path))

    # v6 xdb verify
    try:
        util.verify_from_file(xdb_v6_path)
    except Exception as e:
        print("failed to verify the xdb file `{}`: {}".format(xdb_v6_path, str(e)))
    else:
        print("xdb file `{}` verified".format(xdb_v6_path))

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

def test_parse_ip():
    ip_list = [
        "1.0.0.0", "58.251.30.115", "192.168.1.100", "126.255.32.255", "219.xx.xx.11", 
        "::", "::1", "fffe::", "2c0f:fff0::", "2c0f:fff0::1", "2a02:26f7:c409:4001::",
        "2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "240e:982:e617:ffff:ffff:ffff:ffff:ffff", "::xx:ffff"
    ]
    for ip in ip_list:
        try :
            ip_bytes  = util.parse_ip(ip)
            ip_string = util.ip_to_string(ip_bytes)
            print("parse_ip({}) -> {{addr:{}, equal:{}}}".format(ip, ip_string, ip_string == ip))
        except ValueError as e:
            print("failed to parse ip `{}`: {}".format(ip, e))

def test_ip_compare():
    ip_list = [
        ["1.0.0.0", "1.0.0.1", -1],
        ["192.168.1.101", "192.168.1.90", 1],
        ["219.133.111.87", "114.114.114.114", 1],
        ["2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff", -1],
        ["2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff", -1],
        ["ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff", 1]
    ]
    
    for ip_pair in ip_list:
        ip1 = util.parse_ip(ip_pair[0])
        ip2 = util.parse_ip(ip_pair[1])
        cmp = util.ip_compare(ip1, ip2)
        print("compare({}, {}) -> {} ? {}".format(util.ip_to_string(ip1), util.ip_to_string(ip2), cmp, cmp == ip_pair[2]))

def _get_searcher_list(version: util.Version):
    db_path  = xdb_v4_path if version.id == util.XdbIPv4Id else xdb_v6_path
    return [
        ["new_with_file_only", lambda: xdb.new_with_file_only(version, db_path)],
        ["new_with_vector_index", lambda: xdb.new_with_vector_index(version, db_path, util.load_vector_index_from_file(db_path))],
        ["new_with_buffer", lambda: xdb.new_with_buffer(version, util.load_content_from_file(db_path))]
    ]

def test_ip_search():
    # ipv4 search test
    print("---ipv4 search test:")
    ip_str = "120.229.45.92"
    s_list = _get_searcher_list(util.IPv4)
    try:
        b_region = None
        for meta in s_list:
            searcher = meta[1]()
            region = searcher.search(ip_str)
            if b_region != None:
                assert b_region == region, f"region and b_region is not the same"
            print(f"{meta[0]}.search({ip_str}): {region}")

            # searcher close
            searcher.close()
    except Exception as e:
        print(f"failed to search({ip_str}): {str(e)}")

    # ipv6 search test
    print("---ipv6 search test:")
    ip_str = "240e:3b7:3272:d8d0:db09:c067:8d59:539e"
    s_list = _get_searcher_list(util.IPv6)
    try:
        b_region = None
        for meta in s_list:
            searcher = meta[1]()
            region = searcher.search(ip_str)
            if b_region != None:
                assert b_region == region, f"region and b_region is not the same"
            print(f"{meta[0]}.search({ip_str}): {region}")

            # searcher close
            searcher.close()
    except Exception as e:
        print(f"failed to search({ip_str}): {str(e)}")

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
