package org.lionsoul.ip2region.xdb;

import java.nio.charset.StandardCharsets;

import static org.lionsoul.ip2region.xdb.InternalUtil.*;

/**
 * @author Li.Wei by 2022/8/6
 */
public abstract class AbstractSearcher implements Searcher {
    // constant defined copied from the xdb maker
    protected static final int HEADER_INFO_LENGTH = 256;
    protected static final int VECTOR_INDEX_ROWS = 256;
    protected static final int VECTOR_INDEX_COLS = 256;
    protected static final int VECTOR_INDEX_SIZE = 8;
    protected static final int SEGMENT_INDEX_SIZE = 14;

    @Deprecated
    public int getIOCount() {
        return 0;
    }

    public Region searchRegion(String ip) {
        return search0(ip2long(ip));
    }

    public String search(String ip) {
        return searchRegion(ip).getRegion();
    }

    private Region search0(long ip) {
        int ioCount = 0;

        // locate the segment index block based on the vector index
        int il0 = (int) ((ip >> 24) & 0xFF);
        int il1 = (int) ((ip >> 16) & 0xFF);
        int idx = il0 * VECTOR_INDEX_COLS * VECTOR_INDEX_SIZE + il1 * VECTOR_INDEX_SIZE;
        // System.out.printf("il0: %d, il1: %d, idx: %d\n", il0, il1, idx);
        PointIndex pointIndex = this.pointIndex(idx);
        int sPtr = pointIndex.sPtr, ePtr = pointIndex.ePtr;
        // System.out.printf("sPtr: %d, ePtr: %d\n", sPtr, ePtr);

        // binary search the segment index block to get the region info
        final byte[] buff = new byte[SEGMENT_INDEX_SIZE];
        int dataLen = -1, dataPtr = -1;
        int l = 0, h = (ePtr - sPtr) / SEGMENT_INDEX_SIZE;
        while (l <= h) {
            int m = (l + h) >> 1;
            int p = sPtr + m * SEGMENT_INDEX_SIZE;

            // read the segment index
            ioCount += read(p, buff);

            long sip = getIntLong(buff, 0);
            if (ip < sip) {
                h = m - 1;
            } else {
                long eip = getIntLong(buff, 4);
                if (ip > eip) {
                    l = m + 1;
                } else {
                    dataLen = getInt2(buff, 8);
                    dataPtr = getInt(buff, 10);
                    break;
                }
            }
        }

        // empty match interception
        // System.out.printf("dataLen: %d, dataPtr: %d\n", dataLen, dataPtr);
        if (dataPtr < 0) {
            return Region.builder().ioCount(ioCount).build();
        }

        // load and return the region data
        final byte[] regionBuff = new byte[dataLen];
        ioCount += read(dataPtr, regionBuff);
        return Region.builder().region(new String(regionBuff, StandardCharsets.UTF_8)).ioCount(ioCount).build();
    }

    /**
     * find index start,end point
     *
     * @param idx idx
     * @return PointIndex
     */
    protected abstract PointIndex pointIndex(int idx);

    /**
     * read 2 buffer
     *
     * @param offset offset
     * @param buffer buffer
     * @return io count
     */
    protected abstract int read(int offset, byte[] buffer);

    protected static class PointIndex {
        int sPtr, ePtr;

        public PointIndex(int sPtr, int ePtr) {
            this.sPtr = sPtr;
            this.ePtr = ePtr;
        }
    }
}
