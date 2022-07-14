# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
# Author: linyufeng <leolin49@foxmail.com>
# Date  : 2022/7/14 17:00
#
import struct

Vector_Index_Policy = 1
BTree_Index_Policy = 2


def index_policy_from_string(s: str) -> int:
    sl = s.lower()
    if sl == "vector":
        return Vector_Index_Policy
    elif sl == "btree":
        return BTree_Index_Policy
    else:
        print("invalid policy `{}`, used default vector index".format(s))
        return Vector_Index_Policy


class VectorIndexBlock:
    first_ptr = 0
    last_ptr = 0

    def __init__(self, fp=0, lp=0):
        self.first_ptr = fp
        self.last_ptr = lp

    def __str__(self):
        return "FirstPtr: {}, LastPrt: {}".format(self.first_ptr, self.last_ptr)

    def encode(self) -> bytes:
        return struct.pack("<II", self.first_ptr, self.last_ptr)


Segment_Index_Block_Size = 14


class SegmentIndexBlock:
    start_ip = 0
    end_ip = 0
    data_len = 0
    data_ptr = 0

    def __init__(self, sip, eip, dl, dp):
        self.start_ip = sip
        self.end_ip = eip
        self.data_len = dl
        self.data_ptr = dp

    def __str__(self):
        return "{sip: {}, eip: {}, len: {}, ptr: {}}".format(
            self.start_ip, self.end_ip, self.data_len, self.data_ptr
        )

    def encode(self) -> bytes:
        return struct.pack(
            "<IIHI", self.start_ip, self.end_ip, self.data_len, self.data_ptr
        )
