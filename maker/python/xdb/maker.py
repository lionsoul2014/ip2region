# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
# Author: linyufeng <leolin49@foxmail.com>
# Date  : 2022/7/14 17:00
#
# ----
# ip2region database v2.0 structure
#
# +----------------+-------------------+---------------+--------------+
# | header space   | speed up index    |  data payload | block index  |
# +----------------+-------------------+---------------+--------------+
# | 256 bytes      | 512 KiB (fixed)   | dynamic size  | dynamic size |
# +----------------+-------------------+---------------+--------------+
#
# 1. padding space : for header info like block index ptr, version, release date eg ... or any other temporary needs.
# -- 2bytes: version number, different version means structure update, it fixed to 2 for now
# -- 2bytes: index algorithm code.
# -- 4bytes: generate unix timestamp (version)
# -- 4bytes: index block start ptr
# -- 4bytes: index block end ptr
#
#
# 2. data block : region or whatever data info.
# 3. segment index block : binary index block.
# 4. vector index block  : fixed index info for block index search speed up.
# space structure table:
# -- 0   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
# -- 1   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
# -- 2   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
# -- ...
# -- 255 -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
#
#
# super block structure:
# +-----------------------+----------------------+
# | first index block ptr | last index block ptr |
# +-----------------------+----------------------+
#
# data entry structure:
# +--------------------+-----------------------+
# | 2bytes (for desc)  |  dynamic length	   |
# +--------------------+-----------------------+
#  data length   whatever in bytes
#
# index entry structure
# +------------+-----------+---------------+------------+
# | 4bytes	   | 4bytes	   | 2bytes		   | 4 bytes    |
# +------------+-----------+---------------+------------+
#  start ip 	  end ip	  data length     data ptr
import logging
import struct
import time
import sys

import xdb.segment as seg
import xdb.index as idx
import xdb.util as util


Version_No = 2
Header_Info_Length = 256
Vector_Index_Rows = 256
Vector_Index_Cols = 256
Vector_Index_Size = 8
Vector_Index_Length = Vector_Index_Rows * Vector_Index_Cols * Vector_Index_Size


class Maker:
    src_handle = None
    dst_handle = None
    index_policy = idx.Vector_Index_Policy
    segments = None
    region_pool = None
    vector_index = None

    def __init__(self, sh, dh, ip, sg, rp, vi):
        self.src_handle = sh
        self.dst_handle = dh
        self.index_policy = ip
        self.segments = sg
        self.region_pool = rp
        self.vector_index = vi

    def init(self):
        """
        Init the `xdb` binary file.
        1. Init the file header
        2. Load all the segments
        """
        self.init_db_header()
        self.load_segments()

    def init_db_header(self):
        """
        Init and write the file header to the destination xdb file.
        """
        logging.info("try to init the db header ... ")
        self.src_handle.seek(0, 0)

        # Make and write the header space
        header = bytearray([0] * 256)
        # 1. Version number
        header[0:2] = Version_No.to_bytes(2, byteorder="little")
        # 2. Index policy code
        header[2:4] = int(self.index_policy).to_bytes(2, byteorder="little")
        # 3. Generate unix timestamp
        header[4:8] = int(time.time()).to_bytes(4, byteorder="little")
        # 4. Index block start ptr
        header[8:12] = int(0).to_bytes(4, byteorder="little")
        # 5. Index block end ptr
        header[12:16] = int(0).to_bytes(4, byteorder="little")
        # Write header buffer to file
        self.dst_handle.write(header)

    def load_segments(self):
        """
        Load the segments [start ip|end ip|region] from source ip text file.
        :return: the list of Segment
        """
        logging.info("try to load the segments ... ")
        last = None
        s_tm = time.time()

        lines = self.src_handle.read().splitlines()
        for line in lines:
            logging.info("load segment: `{}`".format(line))
            ps = line.split("|", maxsplit=2)
            if len(ps) != 3:
                raise Exception("invalid ip segment line `{}`".format(line))

            sip = util.check_ip(ps[0])
            if sip == -1:
                raise Exception(
                    "invalid ip address `{}` in line `{}`".format(ps[0], line)
                )
            eip = util.check_ip(ps[1])
            if eip == -1:
                raise Exception(
                    "invalid ip address `{}` in line `{}`".format(ps[1], line)
                )

            if sip > eip:
                raise Exception(
                    "start ip({}) should not be greater than end ip({})".format(
                        ps[0], ps[1]
                    )
                )
            if len(ps[2]) < 1:
                raise Exception("empty region info in segment line `{}`".format(line))

            segment = seg.Segment(sip=sip, eip=eip, reg=ps[2])
            # Check the continuity of data segment
            if last is not None:
                if last.end_ip + 1 != segment.start_ip:
                    raise Exception(
                        "discontinuous data segment: last.eip+1({})!=seg.sip({}, {})".format(
                            sip, eip, ps[0]
                        )
                    )
            self.segments.append(segment)
            last = segment
        logging.info(
            "all segments loaded, length: {}, elapsed: {}".format(
                len(self.segments), time.time() - s_tm
            )
        )

    def set_vector_index(self, ip, ptr):
        """
        Init and refresh the vector index based on the IP pre-two bytes.
        """
        row, col = (ip >> 24) & 0xFF, (ip >> 16) & 0xFF
        vi_block = self.vector_index[row][col]
        if vi_block.first_ptr == 0:
            vi_block.first_ptr = ptr
            vi_block.last_ptr = ptr + idx.Segment_Index_Block_Size
        else:
            vi_block.last_ptr = ptr + idx.Segment_Index_Block_Size
        self.vector_index[row][col] = vi_block

    def start(self):
        """
        Start to make the 'xdb' binary file.
        """
        if len(self.segments) < 1:
            raise Exception("empty segment list")

        # 1. Write all the region/data to the binary file
        self.dst_handle.seek(Header_Info_Length + Vector_Index_Length, 0)

        logging.info("try to write the data block ... ")
        for s in self.segments:
            logging.info("try to write region '{}'...".format(s.region))
            if s.region in self.region_pool:
                logging.info(
                    " --[Cached] with ptr={}".format(self.region_pool[s.region])
                )
                continue
            region = bytes(s.region, encoding="utf-8")
            if len(region) > 0xFFFF:
                raise Exception(
                    "too long region info `{}`: should be less than {} bytes".format(
                        s.region, 0xFFFF
                    )
                )
            # Get the first ptr of the next region
            pos = self.dst_handle.seek(0, 1)
            logging.info("{} {} {}".format(pos, region, s.region))
            self.dst_handle.write(region)
            self.region_pool[s.region] = pos
            logging.info(" --[Added] with ptr={}".format(pos))
        # 2. Write the index block and cache the super index block
        logging.info("try to write the segment index block ... ")
        counter, start_index_ptr, end_index_ptr = 0, -1, -1
        for sg in self.segments:
            if sg.region not in self.region_pool:
                raise Exception("missing ptr cache for region `{}`".format(sg.region))
            data_len = len(bytes(sg.region, encoding="utf-8"))
            if data_len < 1:
                raise Exception("empty region info for segment '{}'".format(sg.region))

            seg_list = sg.split()
            logging.info(
                "try to index segment({} split) {} ...".format(len(seg_list), sg)
            )
            for s in seg_list:
                pos = self.dst_handle.seek(0, 1)

                s_index = idx.SegmentIndexBlock(
                    sip=s.start_ip,
                    eip=s.end_ip,
                    dl=data_len,
                    dp=self.region_pool[sg.region],
                )
                self.dst_handle.write(s_index.encode())
                logging.info(
                    "|-segment index: {}, ptr: {}, segment: {}".format(counter, pos, s)
                )
                self.set_vector_index(s.start_ip, pos)
                counter += 1

                # Check and record the start index ptr
                if start_index_ptr == -1:
                    start_index_ptr = pos
                end_index_ptr = pos

        # 3. Synchronized the vector index block
        logging.info("try to write the vector index block ... ")
        self.dst_handle.seek(Header_Info_Length, 0)
        for i in range(0, len(self.vector_index)):
            for j in range(0, len(self.vector_index[i])):
                vi = self.vector_index[i][j]
                self.dst_handle.write(vi.encode())

        # 4. Synchronized the segment index info
        logging.info("try to write the segment index ptr ... ")
        buff = struct.pack("<II", start_index_ptr, end_index_ptr)
        self.dst_handle.seek(8, 0)
        self.dst_handle.write(buff)

        logging.info(
            "write done, dataBlocks: {}, indexBlocks: ({}, {}), indexPtr: ({}, {})".format(
                len(self.region_pool),
                len(self.segments),
                counter,
                start_index_ptr,
                end_index_ptr,
            )
        )

    def end(self):
        """
        End of make the 'xdb' binary file.
        """
        try:
            self.src_handle.close()
            self.dst_handle.close()
        except IOError as e:
            logging.error(e)
            sys.exit()


def new_maker(policy: int, srcfile: str, dstfile: str) -> Maker:
    """
    Create a xdb Maker to make the xdb binary file
    :param policy: index algorithm code 1:vector, 2:b-tree
    :param srcfile: source ip text file path
    :param dstfile: destination binary xdb file path
    :return: the 'xdb' Maker
    """
    try:
        sh = open(srcfile, mode="r", encoding="utf-8")
        dh = open(dstfile, mode="wb")
        return Maker(
            sh=sh,
            dh=dh,
            ip=policy,
            sg=[],
            rp={},
            vi=[
                [idx.VectorIndexBlock() for _ in range(Vector_Index_Rows)]
                for _ in range(Vector_Index_Cols)
            ],
        )
    except IOError as e:
        logging.error(e)
        sys.exit()
