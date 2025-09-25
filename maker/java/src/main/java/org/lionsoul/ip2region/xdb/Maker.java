// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/12

// --- Ip2Region v2.0 data structure
//
// +----------------+--------------------------+---------------+--------------+
// | header space   | vector speed up index    |  data payload | block index  |
// +----------------+--------------------------+---------------+--------------+
// | 256 bytes      | 512 KiB (fixed)          | dynamic size  | dynamic size |
// +----------------+--------------------------+---------------+--------------+
//
// 1. padding space : for header info like block index ptr, version, release date eg ... or any other temporary needs.
// -- 2bytes: version number, different version means structure update, it fixed to 2 for now
// -- 2bytes: index algorithm code.
// -- 4bytes: generate unix timestamp (version)
// -- 4bytes: index block start ptr
// -- 4bytes: index block end ptr
// -- 2bytes: ip version number (4/6 since IPv6 supporting)
// -- 2bytes: runtime ptr bytes
//
//
// 2. data block : region or whatever data info.
// 3. segment index block : binary index block.
// 4. vector index block  : fixed index info for block index search speed up.
// space structure table:
// -- 0   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 1   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 2   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- ...
// -- 255 -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
//
//
// super block structure:
// +-----------------------+----------------------+
// | first index block ptr | last index block ptr |
// +-----------------------+----------------------+
//
// data entry structure:
// +--------------------+-----------------------+
// | 2bytes (for desc)	| dynamic length        |
// +--------------------+-----------------------+
//  data length   whatever in bytes
//
// index entry structure
// +------------+-----------+---------------+------------+
// | 4bytes     | 4bytes    | 2bytes        | 4 bytes    |
// +------------+-----------+---------------+------------+
//  start ip 	  end ip	  data length     data ptr

package org.lionsoul.ip2region.xdb;

import java.io.*;
import java.nio.charset.Charset;
import java.util.*;

public class Maker {
    // constants define
    public static final int VersionNo         = 3;  // 2 for XDB 2.0, 3 for XDB 3.0
    public static final int HeaderInfoLength  = 256;
    public static final int VectorIndexRows   = 256;
    public static final int VectorIndexCols   = 256;
    public static final int VectorIndexSize   = 8;  // in bytes
    public static final int RuntimePtrSize    = 4;  // in bytes
    public static final long MaxFilePointer   = (1L << (RuntimePtrSize * 8)) - 1;
    public static final int VectorIndexLength = VectorIndexRows * VectorIndexCols * VectorIndexSize;

    public static final Log log = Log.getLogger(Maker.class);

    // IP version since xdb 3.0
    private final Version version;

    // source text file handle
    private final File srcFile;
    private final int[] fields;

    private final List<Segment> segments;
    private final Charset bytesCharset;

    // destination binary file handle
    private final RandomAccessFile dstHandle;

    // index policy
    private final int indexPolicy;

    // region pool
    private final Map<String, DataEntry> regionPool;

    // vector index raw bytes
    private final byte[] vectorIndex;

    public Maker(Version version, int policy, String srcPath, String dstPath, int[] fields) throws IOException {
        this.srcFile = new File(srcPath);
        if (!this.srcFile.exists()) {
            throw new FileNotFoundException("source text file `" + srcPath + "` not found");
        }

        this.version = version;
        this.fields  = fields;

        /// check and delete the target xdb file if it exists
        /// final File dstFile = new File(dstPath);
        /// if (dstFile.exists() && !dstFile.delete()) {
        ///     log.warnf("failed to delete the dest xdb file `%s`", dstPath);
        /// }

        this.bytesCharset = Charset.forName("utf-8");
        this.segments = new LinkedList<Segment>();
        this.dstHandle = new RandomAccessFile(dstPath, "rw");
        this.indexPolicy = policy;
        this.regionPool = new HashMap<String, DataEntry>();
        this.vectorIndex = new byte[VectorIndexLength]; // all filled with 0

        // truncate the original xdb file
        this.dstHandle.setLength(0);
    }

    // init the header of the target xdb binary file
    private void initHeader() throws IOException {
        log.infof("try to init the db header ... ");
        dstHandle.seek(0);

        // make and write the header space
        final byte[] header = new byte[HeaderInfoLength];

        // encode the data
        // 1, data version number
        LittleEndian.put(header, 0, VersionNo, 2);

        // 2, index policy code
        LittleEndian.put(header, 2, indexPolicy, 2);

        // 3, generate unix timestamp
        LittleEndian.put(header, 4, System.currentTimeMillis() / 1000, 4);

        // 4, index block start ptr
        LittleEndian.put(header, 8, 0, 4);    // start index ptr

        // 5, index block end ptr
        LittleEndian.put(header, 12, 0, 4);   // end index ptr

        // since xdb 3.0
        // 6, IP version
        LittleEndian.put(header, 16, version.id, 2);        // IP version

        // 7, runtime ptr bytes
        LittleEndian.put(header, 18, RuntimePtrSize, 2);    // runtime ptr bytes

        dstHandle.write(header);
    }

    // load all the segments.
    private void loadSegments() throws Exception {
        log.infof("try to load the segments ... ");
        final long tStart = System.currentTimeMillis();
        Segment.iterate(srcFile, new Segment.IterateAction() {
            private Segment last = null;

            @Override
            public void before(String line) {
                log.debugf("load segment: `%s`", line);
            }

            @Override
            public String filter(String region) {
                return Util.regionFiltering(region, fields);
            }

            @Override
            public void handle(Segment seg) throws Exception {
                // ip version check
                if (seg.startIP.length != version.bytes) {
                    throw new Exception("invalid ip segment("+version.name+" expected)");
                }

                if (last != null && !seg.after(last)) {
                    throw new Exception("discontinuous data segment: last.eip("
                        + Util.ipToString(last.endIP)+")+1 != seg.sip("+ Util.ipToString(seg.startIP) + ", "+ seg.region +")");
                }

                // allow empty region
                // if (region.length() < 1) {
                //     throw new Exception("empty region info for segment `"+seg+"`");
                // }

                segments.add(seg);
                last = seg;
            }
        });

        log.infof("all segments loaded, length: %d, elapsed: %d ms", segments.size(), System.currentTimeMillis() - tStart);
    }

    // init the maker
    public void init() throws Exception {
        // init the db header
        initHeader();

        // load all the segments
        loadSegments();
    }

    // set the vector index info of the specified ip
    private void setVectorIndex(final byte[] ip, long ptr) {
        final int il0 = (int) (ip[0] & 0xFF);
        final int il1 = (int) (ip[1] & 0xFF);
        final int idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;
        final long sPtr = LittleEndian.getUint32(vectorIndex, idx);
        if (sPtr == 0) {
            LittleEndian.put(vectorIndex, idx, ptr, 4);
            LittleEndian.put(vectorIndex, idx + 4, ptr + version.segmentIndexSize, 4);
        } else {
            LittleEndian.put(vectorIndex, idx + 4, ptr + version.segmentIndexSize, 4);
        }
    }

    // start to make the binary file
    public void start() throws Exception {
        if (segments.isEmpty()) {
            throw new Exception("empty segment list");
        }

        // 1, write all the region/data to the binary file
        dstHandle.seek(HeaderInfoLength + VectorIndexLength);

        log.infof("try to write the data block ... ");
        for (final Segment seg : segments) {
            log.debugf("try to write region `%s` ... ", seg.region);
            final DataEntry e = regionPool.get(seg.region);
            if (e != null) {
                log.debugf(" --[Cached] with ptr=%d", e.ptr);
                continue;
            }

            // get the utf-8 bytes of the region info
            final byte[] regionBuff = seg.region.getBytes(bytesCharset);
            if (regionBuff.length < 1) {
                // allow empty region info
                // throw new Exception("empty region info for segment `"+seg+"`");
            } else if (regionBuff.length > 0xFFFF) {
                throw new Exception("too long region info `"+seg.region+"`: should be less than 65535 bytes");
            }

            // record the current ptr
            final long pos = dstHandle.getFilePointer();
            dstHandle.write(regionBuff);

            // @TODO: remove this if the long ptr operation were supported
            if (pos >= MaxFilePointer) {
                throw new IOException("region ptr exceed the max length of '" + MaxFilePointer + "'");
            }

            // record the mapping
            regionPool.put(seg.region, new DataEntry(regionBuff.length, pos));
            log.debugf(" --[Added] with ptr=%d", pos);
        }

        // 2, write the index block cache the super index block
        log.infof("try to write the segment index block ... ");
        int counter = 0;
        long startIndexPtr = -1, endIndexPtr = -1;
        final byte[] indexBuff = new byte[version.segmentIndexSize];
        for (Segment seg : segments) {
            // we need the region ptr
            final DataEntry e = regionPool.get(seg.region);
            if (e == null) {
                throw new Exception("missing ptr cache for region `"+seg.region+"`");
            }

            int _offset = 0;
            List<Segment> segList = seg.split();
            log.debugf("try to index segment(%d splits) %s ... ", segList.size(), seg);
            for (final Segment s : segList) {
                long pos = dstHandle.getFilePointer();

                // @TODO: remove this if the long ptr operation were supported
                if (pos >= MaxFilePointer) {
                    throw new IOException("region ptr exceed the max length of '" + MaxFilePointer + "'");
                }

                // encode the segment index info.
                // @Note: in order to compatible with the old searcher implementation we choose to keep
                // encode the IPv4 bytes with little endian.
                // for IPv6 we choose the Network byte order (Big endian).
                version.putBytes(indexBuff, 0, s.startIP);
                version.putBytes(indexBuff, s.startIP.length, s.endIP);
                _offset = s.startIP.length + s.endIP.length;
                LittleEndian.put(indexBuff, _offset, e.length, 2);
                LittleEndian.put(indexBuff, _offset + 2, e.ptr, 4);
                dstHandle.write(indexBuff);

                log.debugf("|-segment index: %d, ptr: %d, segment: %s", counter, pos, s);
                setVectorIndex(s.startIP, pos);
                counter++;

                // check and record the start index ptr
                if (startIndexPtr == -1) {
                    startIndexPtr = pos;
                }

                endIndexPtr = pos;
            }
        }

        // 3, synchronize the vector index block
        log.infof("try to write the vector index block ... ");
        dstHandle.seek(HeaderInfoLength);
        dstHandle.write(vectorIndex);

        // 4, synchronize the segment index info
        log.infof("try to write the segment index ptr ... ");
        LittleEndian.put(indexBuff, 0, startIndexPtr, 4);
        LittleEndian.put(indexBuff, 4, endIndexPtr, 4);
        dstHandle.seek(8);
        dstHandle.write(indexBuff, 0, 8);

        log.infof("write done, dataBlocks: %d, indexBlocks: (%d, %d), indexPtr: (%d, %d)",
            regionPool.size(), segments.size(), counter, startIndexPtr, endIndexPtr);
    }

    // end the make, do the resource clean up
    public void end() throws IOException {
        this.dstHandle.close();
    }

    private static class DataEntry {
        final long ptr;
        final int length; // in bytes

        DataEntry(int length, long ptr) {
            this.length = length;
            this.ptr = ptr;
        }
    }

}
