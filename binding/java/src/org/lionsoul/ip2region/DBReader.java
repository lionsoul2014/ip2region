package org.lionsoul.ip2region;

import java.io.IOException;

public interface DBReader {

    byte[] full() throws IOException;

    void seek(long pos) throws IOException;

    void readFully(byte[] buf, int offset, int length) throws IOException;

    void close() throws IOException;
}
