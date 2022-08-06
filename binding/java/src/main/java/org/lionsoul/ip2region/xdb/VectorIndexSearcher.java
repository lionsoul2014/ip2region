package org.lionsoul.ip2region.xdb;

import java.io.IOException;
import java.io.RandomAccessFile;

import static org.lionsoul.ip2region.xdb.InternalUtil.getInt;

/**
 * vector index.
 * use the byte[] instead of VectorIndex entry array to keep the minimal memory allocation.
 */
public class VectorIndexSearcher extends FileSearcher {

    private final byte[] vectorIndex;

    public VectorIndexSearcher(String dbFile) {
        this(dbFile, null);
    }

    public VectorIndexSearcher(String dbFile, byte[] vectorIndex) {
        super(dbFile);
        try {
            this.vectorIndex = vectorIndex != null ? vectorIndex : loadVectorIndexFromFile(dbFile);
        } catch (SearcherException | IOException e) {
            throw new SearcherException("load dbFile error: " + e.getMessage() + ", file=" + dbFile, e);
        }
    }

    @Override
    protected PointIndex pointIndex(int idx) {
        return new PointIndex(
                getInt(vectorIndex, idx),
                getInt(vectorIndex, idx + 4)
        );
    }

    // --- static util function

    private static byte[] loadVectorIndexFromFile(String dbPath) throws IOException {
        final byte[] buff;
        try (RandomAccessFile handle = new RandomAccessFile(dbPath, "r")) {
            handle.seek(HEADER_INFO_LENGTH);
            int len = VECTOR_INDEX_ROWS * VECTOR_INDEX_COLS * SEGMENT_INDEX_SIZE;
            buff = new byte[len];
            int rLen = handle.read(buff);
            if (rLen != len) {
                throw new IOException("incomplete read: read bytes should be " + len);
            }
        }
        return buff;
    }
}
