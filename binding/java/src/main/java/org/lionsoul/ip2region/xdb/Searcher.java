// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// xdb searcher (Not thread safe implementation)
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

import java.io.IOException;
import java.io.RandomAccessFile;

public class Searcher {
    // xdb structure version no
    public static final int STRUCTURE_20 = 2;
    public static final int STRUCTURE_30 = 3;

    // constant defined copied from the xdb maker
    public static final int HeaderInfoLength = 256;
    public static final int VectorIndexRows  = 256;
    public static final int VectorIndexCols  = 256;
    public static final int VectorIndexSize  = 8;

    // Linux max write / read bytes
    public static final int MAX_WRITE_BYTES = 0x7ffff000;

    // ip version
    private final Version version;

    // random access file handle for file-based search
    private final RandomAccessFile handle;

    private int ioCount = 0;

    // vector index.
    // use the byte[] instead of VectorIndex entry array to keep
    // the minimal memory allocation.
    private final byte[] vectorIndex;

    // xdb content buffer, used for in-memory search.
    // @Note: use the LongByteArray instead since 2025/08/22
    // private final byte[] contentBuff;
    private final LongByteArray contentBuff;

    // --- static method to create searchers

    public static Searcher newWithFileOnly(Version version, String dbPath) throws IOException {
        return new Searcher(version, dbPath, null, null);
    }

    public static Searcher newWithVectorIndex(Version version, String dbPath, byte[] vectorIndex) throws IOException {
        return new Searcher(version, dbPath, vectorIndex, null);
    }

    public static Searcher newWithBuffer(Version version, LongByteArray cBuff) throws IOException {
        return new Searcher(version, null, null, cBuff);
    }

    // --- End of creator

    public Searcher(Version version, String dbFile, byte[] vectorIndex, LongByteArray cBuff) throws IOException {
        this.version = version;
        if (cBuff != null) {
            this.handle = null;
            this.vectorIndex = null;
            this.contentBuff = cBuff;
        } else {
            this.handle = new RandomAccessFile(dbFile, "r");
            this.vectorIndex = vectorIndex;
            this.contentBuff = null;
        }
    }

    public void close() throws IOException {
        if (this.handle != null) {
            this.handle.close();
        }
    }

    public Version getIPVersion() {
        return version;
    }

    public int getIOCount() {
        return ioCount;
    }

    public String search(String ipStr) throws Exception {
        return search(Util.parseIP(ipStr));
    }

    public String search(byte[] ip) throws IOException, InetAddressException {
        // ip version check
        if (ip.length != version.bytes) {
            throw new InetAddressException("invalid ip address ("+version.name+" expected)");
        }

        // reset the global counter
        this.ioCount = 0;

        // locate the segment index block based on the vector index
        long sPtr = 0, ePtr = 0;
        int il0 = (int) (ip[0] & 0xFF);
        int il1 = (int) (ip[1] & 0xFF);
        int idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;
        // System.out.printf("il0: %d, il1: %d, idx: %d\n", il0, il1, idx);
        if (vectorIndex != null) {
            sPtr = LittleEndian.getUint32(vectorIndex, idx);
            ePtr = LittleEndian.getUint32(vectorIndex, idx + 4);
        } else if (contentBuff != null) {
            sPtr = contentBuff.getUint32(HeaderInfoLength + idx);
            ePtr = contentBuff.getUint32(HeaderInfoLength + idx + 4);
        } else {
            final byte[] buff = new byte[VectorIndexSize];
            read(HeaderInfoLength + idx, buff);
            sPtr = LittleEndian.getUint32(buff, 0);
            ePtr = LittleEndian.getUint32(buff, 4);
        }

        // System.out.printf("sPtr: %d, ePtr: %d\n", sPtr, ePtr);

        // binary search the segment index block to get the region info
        final int bytes = ip.length, dBytes = ip.length << 1;
        final int segIndexSize = version.segmentIndexSize;
        final byte[] buff = new byte[segIndexSize];
        int dataLen = 0;
        long dataPtr = 0, l = 0, h = (ePtr - sPtr) / segIndexSize;
        while (l <= h) {
            long m = (l + h) >> 1;
            long p = sPtr + m * segIndexSize;

            // read the segment index
            read(p, buff);
            if (version.ipSubCompare(ip, buff, 0) < 0) {
                h = m - 1;
            } else if (version.ipSubCompare(ip, buff, bytes) > 0) {
                l = m + 1;
            } else {
                dataLen = LittleEndian.getUint16(buff, dBytes);
                dataPtr = LittleEndian.getUint32(buff, dBytes + 2);
                break;
            }
        }

        // empty match interception
        // System.out.printf("dataLen: %d, dataPtr: %d\n", dataLen, dataPtr);
        if (dataLen == 0) {
            return "";
        }

        // load and return the region data
        final byte[] regionBuff = new byte[dataLen];
        read(dataPtr, regionBuff);
        return new String(regionBuff, "utf-8");
    }

    protected void read(long offset, byte[] buffer) throws IOException {
        // check the in-memory buffer first
        if (contentBuff != null) {
            contentBuff.copy(offset, buffer, 0, buffer.length);
            return;
        }

        // read from the file handle
        assert handle != null;
        handle.seek(offset);

        this.ioCount++;
        int rLen = handle.read(buffer);
        if (rLen != buffer.length) {
            throw new IOException("incomplete read: read bytes should be " + buffer.length);
        }
    }

    // --- static util function

    public static Header loadHeader(RandomAccessFile handle) throws IOException {
        handle.seek(0);
        final byte[] buff = new byte[HeaderInfoLength];
        handle.read(buff);
        return new Header(buff);
    }

    public static Header loadHeaderFromFile(String dbPath) throws IOException {
        final RandomAccessFile handle = new RandomAccessFile(dbPath, "r");
        final Header header = loadHeader(handle);
        handle.close();
        return header;
    }

    public static byte[] loadVectorIndex(RandomAccessFile handle) throws IOException {
        handle.seek(HeaderInfoLength);
        int len = VectorIndexRows * VectorIndexCols * VectorIndexSize;
        final byte[] buff = new byte[len];
        int rLen = handle.read(buff);
        if (rLen != len) {
            throw new IOException("incomplete read: read bytes should be " + len);
        }

        return buff;
    }

    public static byte[] loadVectorIndexFromFile(String dbPath) throws IOException {
        final RandomAccessFile handle = new RandomAccessFile(dbPath, "r");
        final byte[] vIndex = loadVectorIndex(handle);
        handle.close();
        return vIndex;
    }

    public static LongByteArray loadContent(RandomAccessFile handle) throws IOException {
        handle.seek(0);
        // check the length and do the buff load
        long toRead = handle.length();
        final LongByteArray byteArray = new LongByteArray();
        while (toRead > 0) {
            final byte[] buff = new byte[(int) Math.min(toRead, MAX_WRITE_BYTES)];
            final int rLen = handle.read(buff);
            if (rLen != buff.length) {
                throw new IOException("incomplete read: read bytes should be " + buff.length + ", got `" + rLen + "`");
            }

            byteArray.append(buff);
            toRead -= rLen;
        }

        return byteArray;
    }

    public static LongByteArray loadContentFromFile(String dbPath) throws IOException {
        final RandomAccessFile handle = new RandomAccessFile(dbPath, "r");
        final LongByteArray content = loadContent(handle);
        handle.close();
        return content;
    }

    // --- verify util function

    // Verify if the current Searcher could be used to search the specified xdb file.
    // Why do we need this check ?
    // The future features of the xdb impl may cause the current searcher not able to work properly.
    //
    // @Note: You Just need to check this ONCE when the service starts
    // Or use another process (eg, A command) to check once Just to confirm the suitability.
    public static void verify(RandomAccessFile handle) throws IOException, XdbException {
        final Header header = loadHeader(handle);

        // get the runtime ptr bytes
        int runtimePtrBytes = 0;
        if (header.version == STRUCTURE_20) {
            runtimePtrBytes = 4;
        } else if (header.version == STRUCTURE_30) {
            runtimePtrBytes = header.runtimePtrBytes;
        } else {
            throw new XdbException("invalid structure version `" + header.version + "`");
        }

        // 1, confirm the xdb file size
        // to ensure that the maximum file pointer does not overflow
        final long maxFilePtr = (1L << (runtimePtrBytes * 8)) - 1;
        if (handle.length() > maxFilePtr) {
            throw new XdbException("xdb file exceeds the maximum supported bytes: "+maxFilePtr+"");
        }
    }

    public static void verifyFromFile(String dbFile) throws IOException, XdbException {
        final RandomAccessFile handle = new RandomAccessFile(dbFile, "r");
        verify(handle);
        handle.close();
    }

}