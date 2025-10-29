# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# xdb utils on 2025/10/29
# Author Leon<chenxin619315@gmail.com>

import io

# global constants
XdbStructure20 = 2
XdbStructure30 = 3
XdbIPv4Id = 4
XdbIPv6Id = 6

HeaderInfoLength = 256
VectorIndexRows  = 256
VectorIndexCols  = 256
VectorIndexSize  = 8
# cache of VectorIndexCols × VectorIndexRows × VectorIndexSize
VectorIndexLength = 524288

class Header(object):
    '''
    header class
    '''
    def __init__(self, buff):
        self.version = le_get_uint16(buff, 0)
        self.indexPolicy = le_get_uint16(buff, 2)
        self.createdAt = le_get_uint32(buff, 4)
        self.startIndexPtr = le_get_uint32(buff, 8)
        self.endIndexPtr = le_get_uint32(buff, 12)

        # since IPv6 supporting
        self.ipVersion = le_get_uint16(buff, 16)
        self.runtimePtrBytes = le_get_uint16(buff, 18)


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
# ip parse and convert functions

def parse_ip(ip_string):
    pass

def ip_to_string(ip_bytes):
    pass

def ip_compare(ip1, ip2):
    pass

def ip_sub_compare(ip1, ip2, offset):
    pass


# ---
# buffer decode functions

def le_get_uint32(buff, offset):
    '''
    decode an unsinged 4-bytes int from a buffer started from offset
    with little byte endian
    '''
    return (
        ((buff[offset  ]) & 0x000000FF) |
        ((buff[offset+1] <<  8) & 0x0000FF00) | 
        ((buff[offset+2] << 16) & 0x00FF0000) |
        ((buff[offset+3] << 24) & 0xFF000000)
    )

def le_get_uint16(buff, offset):
    '''
    decode an unsinged 2-bytes short from a buffer started from offset
    with little byte endian
    '''
    return (
        ((buff[offset  ]) & 0x000000FF) |
        ((buff[offset+1] <<  8) & 0x0000FF00)
    )


# ---
# xdb buffer load functions

def load_header(handle):
    '''
    load xdb header from a specified file handle
    '''
    handle.seek(0)
    return Header(handle.read(HeaderInfoLength))

def load_header_from_file(db_file):
    handle = io.open(db_file, "rb")
    header = load_header(handle)
    handle.close()
    return header

def load_vector_index(handle):
    '''
    load xdb vector index from a specified file handle
    '''
    handle.seek(HeaderInfoLength)
    return handle.read(VectorIndexLength)

def load_vector_index_from_file(db_file):
    handle = io.open(db_file, "rb")
    v_index = load_vector_index(handle)
    handle.close()
    return v_index

def load_content(handle):
    '''
    load the whole xdb content from a specified file handle
    '''
    handle.seek(0)
    return handle.read()

def load_content_from_file(db_file):
    handle = io.open(db_file, "rb")
    c_buff = load_content(handle)
    handle.close()
    return c_buff