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
// | 2bytes (for desc)	| dynamic length		|
// +--------------------+-----------------------+
//  data length   whatever in bytes
//
// index entry structure
// +------------+-----------+---------------+------------+
// | 4bytes		| 4bytes	| 2bytes		| 4 bytes    |
// +------------+-----------+---------------+------------+
//  start ip 	  end ip	  data length     data ptr

package org.lionsoul.ip2region.xdb;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashMap;
import java.util.Map;

public class Maker {
    // constants define
    public static final int VersionNo = 2;
    public static final int HeaderInfoLength = 256;
    public static final int VectorIndexRows = 256;
    public static final int VectorIndexCols = 256;
    public static final int VectorIndexSize = 8;
    public static final int SegmentIndexSize = 14;

    private static final Log log = Log.getLogger(Maker.class);

    // source text file handle
    private final File srcFile;

    // destination binary file handle
    private final RandomAccessFile dstHandle;

    // index policy
    private final int indexPolicy;

    // region pool
    private final Map<String, Long> regionPool;

    // vector index raw bytes
    private byte[] vectorIndex;

    public Maker(int policy, String srcFile, String dstFile) throws FileNotFoundException {
        this.srcFile = new File(srcFile);
        if (!this.srcFile.exists()) {
            throw new FileNotFoundException("source text file `" +srcFile+ "` not found");
        }

        this.dstHandle = new RandomAccessFile(dstFile, "r");
        this.indexPolicy = policy;
        this.regionPool = new HashMap<String, Long>();
    }

    // init the header of the target xdb binary file
    private void initHeader() throws IOException {
        log.infof("try to init the db header ... ");
        dstHandle.seek(0);

        // make and write the header space
        final byte[] header = new byte[HeaderInfoLength];

        // encode the data
        Util.write(header, 0, VersionNo, 2);
        Util.write(header, 2, indexPolicy, 2);
        Util.write(header, 4, System.currentTimeMillis() / 1000, 4);
        Util.write(header, 8, 0, 4);    // start index ptr
        Util.write(header, 12, 0, 4);   // end index ptr

        dstHandle.write(header);
    }

    // load all the segments
    private void loadSegments() {

    }

    // init the maker
    public void init() throws IOException {
        // init the db header
        initHeader();

        // load all the segments
        loadSegments();
    }

    // start to make the binary file
    public void make() {

    }

    // end the make, do the resource clean up
    public void end() throws IOException {
        this.dstHandle.close();
    }

}
