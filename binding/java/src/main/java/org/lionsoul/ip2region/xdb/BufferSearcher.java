package org.lionsoul.ip2region.xdb;

import net.jcip.annotations.ThreadSafe;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Serializable;

import static org.lionsoul.ip2region.xdb.InternalUtil.getInt;

/**
 * xdb content buffer, used for in-memory search.
 */
@ThreadSafe
public class BufferSearcher extends AbstractSearcher implements Serializable {

    private final byte[] contentBuff;

    public BufferSearcher(byte[] contentBuff) {
        this.contentBuff = contentBuff;
    }

    public BufferSearcher(String dbFile) {
        try {
            this.contentBuff = loadFile(dbFile);
        } catch (IOException e) {
            throw new SearcherException("load dbFile error: " + e.getMessage() + ", file=" + dbFile, e);
        }
    }

    /**
     * default load local file. override load hdfs/s3/oss...
     *
     * @param dbFile dbFile
     * @return byte array
     * @throws IOException ex
     */
    protected byte[] loadFile(String dbFile) throws IOException {
        return loadLocalFile(dbFile);
    }

    @Override
    protected int read(int offset, byte[] buffer) {
        System.arraycopy(contentBuff, offset, buffer, 0, buffer.length);
        return 0;
    }

    @Override
    protected PointIndex pointIndex(int idx) {
        return new PointIndex(
                getInt(contentBuff, HEADER_INFO_LENGTH + idx),
                getInt(contentBuff, HEADER_INFO_LENGTH + idx + 4)
        );
    }

    @Override
    public void close() {
        // nothing
    }

    private static byte[] loadLocalFile(String dbPath) throws IOException {
        final byte[] buff;
        try (RandomAccessFile handle = new RandomAccessFile(dbPath, "r")) {
            handle.seek(0);
            buff = new byte[(int) handle.length()];
            int rLen = handle.read(buff);
            if (rLen != buff.length) {
                throw new IOException("incomplete read: read bytes should be " + buff.length);
            }
        }
        return buff;
    }
}
