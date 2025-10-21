// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// xdb byte buffer which used to instead of the byte array
// when the size of the xdb file is greater than 2^32 << 2;
// xdb file v4 is designed to be a maximum of 2^32 bytes in size.
// @Author Leon <chenxin619315@gmail.com>
// @Date 2025/08/22

import java.util.ArrayList;
import java.util.List;

public class LongByteArray {
    // byte buffer list
    private final List<byte[]> buffs = new ArrayList<byte[]>();
    private long length;

    public LongByteArray() {
        this.length = 0;
    }

    public LongByteArray(byte[] buff) {
        buffs.add(buff);
        length = buff.length;
    }

    // append new buffer
    public void append(final byte[] buffer) {
        buffs.add(buffer);
        length += buffer.length;
    }

    public long length() {
        return length;
    }

    // internal method to determine the position of the specified offset
    private Position determinate(final long offset) {
        int index = 0, position = 0, buffLen = buffs.size();
        long curIndex = 0;
        for (index = 0; index < buffLen; index++) {
            final byte[] buff = buffs.get(index);
            if (curIndex + buff.length < offset) {
                curIndex += buff.length;
                continue;
            }

            // matched and calc the position
            position = (int) (offset - curIndex);
            break;
        }

        return new Position(index, position);
    }

    // Copy from the current buffers to a specified buffer
    // from the specified offset with a specified length
    public byte[] copy(final long srcPos, final byte[] dest, final int destPos, final int length) {
        if (srcPos >= this.length) {
            throw new IndexOutOfBoundsException("srcPos exceed the maximum array length `" + this.length + "`");
        }

        if (destPos + length > dest.length) {
            throw new IndexOutOfBoundsException("destPost+length exceed the maximum dest buffer length `" + dest.length + "`");
        }

        final Position pos = determinate(srcPos);

        // copy from the current buffer
        final byte[] hBuff = buffs.get(pos.index++);
        final int copyLen = Math.min(hBuff.length - pos.offset, length);
        System.arraycopy(hBuff, pos.offset, dest, destPos, copyLen);

        // check and copy from the rest buffer?
        int sPos = destPos + copyLen;
        int left = length - copyLen;
        while (left > 0) {
            final byte[] tBuff = buffs.get(pos.index++);
            final int buffLen = tBuff.length;
            if (left <= buffLen) {
                System.arraycopy(tBuff, 0, dest, sPos, left);
                break;
            }

            System.arraycopy(tBuff, 0, dest, sPos, buffLen);
            sPos += buffLen;
            left -= buffLen;
        }

        return dest;
    }

    // get a byte-buffer from the specified index with a specified length.
    // this method will allocate a new byte buffer with length = $length.
    public byte[] slice(long offset, int length) {
        if (offset + length > this.length) {
            throw new IndexOutOfBoundsException("offset+length exceed the maximum array length `" + this.length + "`");
        }

        final byte[] buffer = new  byte[length];
        return copy(offset, buffer, 0, length);
    }

    // get a 4-bytes uint32 integer from the specified index
    public long getUint32(long offset) {
        final byte[] b = new byte[4];
        copy(offset, b, 0, 4);
        return LittleEndian.getUint32(b, 0);
    }

    public int getInt2(long offset) {
        final byte[] b = new byte[4];
        copy(offset, b, 0, 4);
        return LittleEndian.getUint16(b, 0);
    }

    // position entry class
    public static class Position {
        public int index;
        public int offset;
        public Position(int index, int offset) {
            this.index = index;
            this.offset = offset;
        }
    }

}
