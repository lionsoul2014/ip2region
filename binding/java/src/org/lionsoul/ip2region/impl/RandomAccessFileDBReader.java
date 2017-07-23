package org.lionsoul.ip2region.impl;

import java.io.IOException;
import java.io.RandomAccessFile;

import org.lionsoul.ip2region.DBReader;

/**
 * 把一个RandomAccessFile当成ip2region的数据库文件,即兼容原本的行为
 * @author wendal(wendal1985@gmail.com)
 *
 */
public class RandomAccessFileDBReader implements DBReader {

    protected RandomAccessFile raf;

    public RandomAccessFileDBReader(RandomAccessFile raf) {
        this.raf = raf;
    }

    public byte[] full() throws IOException {
        byte[] buf = new byte[(int) raf.length()];
        raf.readFully(buf, 0, buf.length);
        return buf;
    }

    public void seek(long pos) throws IOException {
        raf.seek(pos);
    }

    public void readFully(byte[] buf, int offset, int length) throws IOException {
        raf.readFully(buf, offset, length);
    }

    public void close() throws IOException {
        raf.close();
    }
}
