#  Created by leolin49 on 2022/7/7.
#  Copyright (C) 2022 leolin49. All rights reserved.
import struct

VectorIndexPolicy = 1
BTreeIndexPolicy = 2
SegmentIndexBlockSize = 14


def index_policy_from_string(s: str) -> int:
    sl = s.lower()
    if sl == "vector":
        return VectorIndexPolicy
    elif sl == "btree":
        return BTreeIndexPolicy
    else:
        print("invalid policy `{}`, used default vector index".format(s))
        return VectorIndexPolicy


class VectorIndexBlock:
    first_ptr = 0
    last_ptr = 0

    def __init__(self, fp=0, lp=0):
        self.first_ptr = fp
        self.last_ptr = lp

    def encode(self) -> bytes:
        return struct.pack("<II", self.first_ptr, self.last_ptr)

    def string(self) -> str:
        return "FirstPtr: {}, LastPrt: {}".format(self.first_ptr, self.last_ptr)


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

    def encode(self) -> bytes:
        return struct.pack("<IIHI", self.start_ip, self.end_ip, self.data_len, self.data_ptr)

    def string(self) -> str:
        return "{sip: {}, eip: {}, len: {}, ptr: {}}".format(self.start_ip, self.end_ip, self.data_len, self.data_ptr)
