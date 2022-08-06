package org.lionsoul.ip2region.xdb;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import static org.lionsoul.ip2region.xdb.InternalUtil.getInt;

/**
 * random access file handle for file based search.
 */
public class FileSearcher extends AbstractSearcher {
    private final RandomAccessFile handle;

    public FileSearcher(String dbFile) {
        try {
            this.handle = new RandomAccessFile(dbFile, "r");
        } catch (FileNotFoundException e) {
            throw new SearcherException("load dbFile error: " + e.getMessage() + ", file=" + dbFile, e);
        }
    }

    @Override
    protected int read(int offset, byte[] buffer) {
        int rLen;
        try {
            handle.seek(offset);
            rLen = handle.read(buffer);
        } catch (SearcherException | IOException e) {
            throw new SearcherException("read error", e);
        }
        if (rLen != buffer.length) {
            throw new SearcherException("incomplete read: read bytes should be " + buffer.length);
        }
        return 2;
    }

    @Override
    protected PointIndex pointIndex(int idx) {
        final byte[] buff = new byte[VECTOR_INDEX_SIZE];
        read(HEADER_INFO_LENGTH + idx, buff);
        return new PointIndex(
                getInt(buff, 0),
                getInt(buff, 4)
        );
    }

    @Override
    public void close() throws IOException {
        this.handle.close();
    }
}
