package org.lionsoul.ip2region.impl;

import java.io.IOException;

import org.lionsoul.ip2region.DBReader;

/**
 * 把一个byte[]当成ip2region的数据库文件
 * @author wendal(wendal1985@gmail.com)
 *
 */
public class ByteArrayDBReader implements DBReader {

    protected byte[] buf;

    protected long pos;

    public ByteArrayDBReader(byte[] buf) {
        this.buf = buf;
    }

    public byte[] full() throws IOException {
        return buf;
    }

    public void seek(long pos) throws IOException {
        this.pos = pos;
    }

    public void readFully(byte[] buf, int offset, int length) throws IOException {
        System.arraycopy(this.buf, (int) pos, buf, offset, length);
    }

    public void close() throws IOException {
        // nop
    }

}
