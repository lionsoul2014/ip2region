# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# xdb utils on 2025/10/29
# Author Leon<chenxin619315@gmail.com>

import io
import struct

# global constants
XdbStructure20 = 2
XdbStructure30 = 3
XdbIPv4Id = 4
XdbIPv6Id = 6

HeaderInfoLength = 256
VectorIndexRows  = 256
VectorIndexCols  = 256
VectorIndexSize  = 8

class Header(object):
    '''
    header class
    '''
    def __init__(self, buff):
        self.version = struct.unpack_from("<H", buff, 0)[0]
        self.indexPolicy = struct.unpack_from("<H", buff, 2)[0]
        self.createdAt = struct.unpack_from("<I", buff, 4)[0]
        self.startIndexPtr = struct.unpack_from("<I", buff, 8)[0]
        self.endIndexPtr = struct.unpack_from("<I", buff, 12)[0]

        # since IPv6 supporting
        self.ipVersion = struct.unpack_from("<H", buff, 16)[0]
        self.runtimePtrBytes = struct.unpack_from("<H", buff, 18)[0]

        # keep the raw data
        self.buff = buff

    def __str__(self):
        return '''{{
    "version": {},
    "indexPolicy": {},
    "createdAt": {},
    "startIndexPtr": {},
    "endIndexPtr": {},
    "ipVersion": {},
    "runtimePtrBytes": {}
}}'''.format(self.version, self.indexPolicy, self.createdAt, self.startIndexPtr, self.endIndexPtr, self.ipVersion, self.runtimePtrBytes)


class Version(object):
    '''
    version class
    '''
    def __init__(self, id, name, byte_num, index_size, ip_compare_func):
        self.id = id
        self.name = name
        self.byte_num = byte_num
        self.index_size = index_size
        self.ip_compare_func = ip_compare_func

    def ip_compare(self, ip1, ip2):
        return self.ip_sub_compare(ip1, ip2, 0)

    def ip_sub_compare(self, ip1, ip2, offset):
        pass

    def __str__(self):
        return '{{"id": {}, "name": "{}", "bytes": {}, "index_size": {}}}'.format(self.id, self.name, self.byte_num, self.index_size)


# IPv4 and IPv6 version constants
IPv4 = Version(XdbIPv4Id, "IPv4", 4, 14, None)
IPv6 = Version(XdbIPv6Id, "IPv6", 16, 38, None)

def version_from_name(name):
    u_name = name.upper()
    if u_name == "IPV4" or u_name == "V4":
        return IPv4
    elif u_name == "IPV6" or u_name == "V6":
        return IPv6
    else:
        return None

def version_from_header(header):
    # old xdb 2.0 with IPv4 supports ONLY
    if header.version <= XdbStructure30:
        return IPv4

    # xdb 3.0 or later version
    ip_version = header.ipVersion
    if ip_version == XdbIPv4Id:
        return IPv4
    elif ip_version == XdbIPv6Id:
        return IPv6
    else:
        return None


# ---
# xdb buffer load functions

def load_header(handle):
    '''
    load xdb header from a specified file handle
    '''
    handle.seek(0)
    buff = handle.read(HeaderInfoLength)
    return Header(buff)

def load_header_from_file(db_file):
    handle = io.open(db_file, "rb")
    header = load_header(handle)
    handle.close()
    return header


if __name__ == "__main__":
    # header class test
    header = load_header_from_file("../../../data/ip2region_v4.xdb")
    print(header)

    # verison class test
    print("IPv4 ->", IPv4)
    print("IPv6 ->", IPv6)
    print("version_from_name(v4) ->", version_from_name("v4"))
    print("version_from_name(v6) ->", version_from_name("v4"))
    print("version_from_header() ->", version_from_header(header))
