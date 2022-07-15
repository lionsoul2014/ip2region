# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
# Author: linyufeng <leolin49@foxmail.com>
# Date  : 2022/7/14 17:00
#
import xdb.util as util


class Segment:
    start_ip = 0
    end_ip = 0
    region = ""

    def __init__(self, sip=0, eip=0, reg=""):
        self.start_ip, self.end_ip = sip, eip
        self.region = reg

    def __str__(self):
        return "{}|{}|{}".format(
            util.long2ip(self.start_ip), util.long2ip(self.end_ip), self.region
        )

    def split(self) -> list:
        """
        Split the segment based on the pre-two bytes.
        :return: the list of segment ofter split
        """
        # Example:
        # split the segment "116.31.76.0|117.21.79.49|region"
        #
        # Return the list with segments:
        # 116.31.76.0 | 116.31.255.255  | region
        # 116.32.0.0  | 116.32.255.255  | region
        # ...         | ...             | region
        # 116.255.0.0 | 116.255.255.255 | region
        # 117.0.0.0   | 117.0.255.255   | region
        # 117.1.0.0   | 117.1.255.255   | region
        # ...         | ...             | region
        # 117.21.0.0  | 117.21.79.49    | region

        # 1. Split the segment with the first byte
        t_list_1 = []
        s_byte_1, e_byte_1 = (self.start_ip >> 24) & 0xFF, (self.end_ip >> 24) & 0xFF
        n_sip = self.start_ip
        for i in range(s_byte_1, e_byte_1 + 1):
            sip = (i << 24) | (n_sip & 0xFFFFFF)
            eip = (i << 24) | 0xFFFFFF
            if eip < self.end_ip:
                n_sip = (i + 1) << 24
            else:
                eip = self.end_ip
            # Append the new segment (maybe)
            t_list_1.append(Segment(sip, eip))

        # 2. Split the segments with the second byte
        t_list_2 = []
        for s in t_list_1:
            base = s.start_ip & 0xFF000000
            n_sip = s.start_ip
            s_byte_2, e_byte_2 = (s.start_ip >> 16) & 0xFF, (s.end_ip >> 16) & 0xFF
            for i in range(s_byte_2, e_byte_2 + 1):
                sip = base | (i << 16) | (n_sip & 0xFFFF)
                eip = base | (i << 16) | 0xFFFF
                if eip < self.end_ip:
                    n_sip = 0
                else:
                    eip = self.end_ip
                t_list_2.append(Segment(sip, eip, self.region))
        return t_list_2
