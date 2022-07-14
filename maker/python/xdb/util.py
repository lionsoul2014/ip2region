# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
# Author: linyufeng <leolin49@foxmail.com>
# Date  : 2022/7/14 17:00
#
_SHIFT_INDEX = (24, 16, 8, 0)


def check_ip(ip: str) -> int:
    """
    Convert ip string to integer.
    Return -1 if ip is not the correct ipv4 address.
    """
    if not is_ipv4(ip):
        return -1
    ps = ip.split(".")
    val = 0
    for i in range(len(ps)):
        d = int(ps[i])
        val |= d << _SHIFT_INDEX[i]
    return val


def long2ip(num: int) -> str:
    """
    Convert integer to ip string.
    Return empty string if the num greater than UINT32_MAX or less than 0.
    """
    if num < 0 or num > 0xFFFFFFFF:
        return ""
    return "{}.{}.{}.{}".format(
        (num >> 24) & 0xFF, (num >> 16) & 0xFF, (num >> 8) & 0xFF, num & 0xFF
    )


def is_ipv4(ip: str) -> bool:
    """
    Determine whether it is an ipv4 address.
    """
    ps = ip.split(".")
    if len(ps) != 4:
        return False
    for p in ps:
        if not p.isdigit() or len(p) > 3 or (int(p) < 0 or int(p) > 255):
            return False
    return True
