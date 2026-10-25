# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.

# xdb utils on 2025/10/29
# Author Leon<chenxin619315@gmail.com>

import io
import ipaddress
import os
from typing import BinaryIO, Callable, Optional

# global constants
XdbStructure20 = 2
XdbStructure30 = 3
XdbIPv4Id = 4
XdbIPv6Id = 6

HeaderInfoLength = 256
VectorIndexRows = 256
VectorIndexCols = 256
VectorIndexSize = 8
# cache of VectorIndexCols × VectorIndexRows × VectorIndexSize
VectorIndexLength = 524288


class Header(object):
    def __init__(self, buff: bytes) -> None:
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

    def __str__(self) -> str:
        return """{{
    "version": {},
    "indexPolicy": {},
    "createdAt": {},
    "startIndexPtr": {},
    "endIndexPtr": {},
    "ipVersion": {},
    "runtimePtrBytes": {}
}}""".format(
            self.version,
            self.indexPolicy,
            self.createdAt,
            self.startIndexPtr,
            self.endIndexPtr,
            self.ipVersion,
            self.runtimePtrBytes,
        )


# ---
# ip parse and convert functions


def parse_ip(ip_string: str) -> bytes:
    try:
        return ipaddress.ip_address(ip_string).packed
    except ValueError:
        raise ValueError("invalid ip address `{}`".format(ip_string))


def ip_to_string(ip_bytes: bytes) -> str:
    if isinstance(ip_bytes, bytes):
        return str(ipaddress.ip_address(ip_bytes))
    else:
        raise ValueError("invalid bytes ip `{}`".format(ip_bytes))


def ip_compare(ip1: bytes, ip2: bytes) -> int:
    if ip1 > ip2:
        return 1
    elif ip1 < ip2:
        return -1
    else:
        return 0


def ip_sub_compare(ip1: bytes, buff: bytes, offset: int) -> int:
    ip2 = buff[offset : offset + len(ip1)]
    if ip1 > ip2:
        return 1
    elif ip1 < ip2:
        return -1
    else:
        return 0


# ---
# ip version class and functions


class Version(object):
    def __init__(
        self,
        id: int,
        name: str,
        byte_num: int,
        index_size: int,
        ip_compare: Callable[[bytes, bytes, int], int],
    ) -> None:
        self.id = id
        self.name = name
        self.byte_num = byte_num
        self.index_size = index_size
        self._ip_compare = ip_compare

    def ip_compare(self, ip1: bytes, ip2: bytes) -> int:
        return self._ip_compare(ip1, ip2, 0)

    def ip_sub_compare(self, ip1: bytes, buff: bytes, offset: int) -> int:
        return self._ip_compare(ip1, buff, offset)

    def __str__(self) -> str:
        return '{{"id": {}, "name": "{}", "bytes": {}, "index_size": {}}}'.format(
            self.id, self.name, self.byte_num, self.index_size
        )


def _v4_sub_compare(ip1: bytes, buff: bytes, offset: int) -> int:
    # ip1: Big endian byte order parsed from input
    # ip2: Little endian byte order read from xdb index.
    # @Note: to compatible with the old Litten endian index encode implementation.
    j = offset + len(ip1) - 1
    for i in range(len(ip1)):
        i1 = ip1[i]
        i2 = buff[j]
        if i1 < i2:
            return -1

        if i1 > i2:
            return 1

        # increase the j
        j = j - 1

    return 0


# ---
# IPv4 and IPv6 version constants
# 14 = 4 + 4 + 2 + 4
IPv4 = Version(XdbIPv4Id, "IPv4", 4, 14, _v4_sub_compare)
# 38 = 16 + 16 + 2 + 4
IPv6 = Version(XdbIPv6Id, "IPv6", 16, 38, ip_sub_compare)


def version_from_name(name: str) -> Optional[Version]:
    u_name = name.upper()
    if u_name == "IPV4" or u_name == "V4":
        return IPv4
    elif u_name == "IPV6" or u_name == "V6":
        return IPv6
    else:
        return None


def version_from_header(header: Header) -> Optional[Version]:
    # old xdb 2.0 with IPv4 supports ONLY
    if header.version < XdbStructure30:
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
# buffer decode functions


def le_get_uint32(buff: bytes, offset: int) -> int:
    """
    decode an unsinged 4-bytes int from a buffer started from offset
    with little byte endian
    """
    return (
        ((buff[offset]) & 0x000000FF)
        | ((buff[offset + 1] << 8) & 0x0000FF00)
        | ((buff[offset + 2] << 16) & 0x00FF0000)
        | ((buff[offset + 3] << 24) & 0xFF000000)
    )


def le_get_uint16(buff: bytes, offset: int) -> int:
    """
    decode an unsinged 2-bytes short from a buffer started from offset
    with little byte endian
    """
    return ((buff[offset]) & 0x000000FF) | ((buff[offset + 1] << 8) & 0x0000FF00)


# ---
# xdb buffer load functions


def load_header(handle: BinaryIO) -> Header:
    """
    load xdb header from a specified file handle
    """
    handle.seek(0)
    return Header(handle.read(HeaderInfoLength))


def load_header_from_file(db_file: str) -> Header:
    handle = io.open(db_file, "rb")
    header = load_header(handle)
    handle.close()
    return header


def load_vector_index(handle: BinaryIO) -> bytes:
    """
    load xdb vector index from a specified file handle
    """
    handle.seek(HeaderInfoLength)
    return handle.read(VectorIndexLength)


def load_vector_index_from_file(db_file: str) -> bytes:
    handle = io.open(db_file, "rb")
    v_index = load_vector_index(handle)
    handle.close()
    return v_index


def load_content(handle: BinaryIO) -> bytes:
    """
    load the whole xdb content from a specified file handle
    """
    handle.seek(0)
    return handle.read()


def load_content_from_file(db_file: str) -> bytes:
    handle = io.open(db_file, "rb")
    c_buff = load_content(handle)
    handle.close()
    return c_buff


# ---
# Verify if the current Searcher could be used to search the specified xdb file.
# Why do we need this check ?
# The future features of the xdb impl may cause the current searcher not able to work properly.
#
# @Note: You Just need to check this ONCE when the service starts
# Or use another process (eg, A command) to check once Just to confirm the suitability.
def verify(handle: BinaryIO) -> None:
    header = load_header(handle)

    # get the runtime ptr bytes
    runtime_ptr_bytes = 0
    if header.version == XdbStructure20:
        runtime_ptr_bytes = 4
    elif header.version == XdbStructure30:
        runtime_ptr_bytes = header.runtimePtrBytes
    else:
        # Higher versions of the structure are usually incompatible.
        raise ValueError("invalid structure version {}".format(header.version))

    # 1, confirm the xdb file size
    # to ensure that the maximum file pointer does not overflow
    max_file_ptr = (1 << (runtime_ptr_bytes * 8)) - 1
    __file_bytes = os.stat(handle.fileno()).st_size
    # print("max_file_ptr: {}, file_bytes: {}".format(max_file_ptr, __file_bytes))
    if __file_bytes > max_file_ptr:
        raise Exception(
            "xdb file exceeds the maximum supported bytes: {}".format(max_file_ptr)
        )


def verify_from_file(db_file: str) -> None:
    handle = io.open(db_file, "rb")
    verify(handle)
    handle.close()
