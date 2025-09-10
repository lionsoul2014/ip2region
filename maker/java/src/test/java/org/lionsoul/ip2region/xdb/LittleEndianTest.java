package org.lionsoul.ip2region.xdb;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class LittleEndianTest {

    @Test
    public void testAll() {
        final byte[] buff = new byte[14];

        // encode
        // do the put
        LittleEndian.put(buff, 0, 1L, 4);
        LittleEndian.put(buff, 4, 2L, 4);

        // putUint32
        LittleEndian.putInt2(buff, 8, 24);
        LittleEndian.putUint32(buff, 10, 1024L);

        // decode
        assertEquals(LittleEndian.getUint32(buff, 0), 1);
        assertEquals(LittleEndian.getUint32(buff, 4), 2);
        assertEquals(LittleEndian.getInt2(buff, 8), 24);
        assertEquals(LittleEndian.getUint32(buff, 10), 1024);

        System.out.printf("uint32(buff, 0): %d\n", LittleEndian.getUint32(buff, 0));
        System.out.printf("uint32(buff, 4): %d\n", LittleEndian.getUint32(buff, 4));
        System.out.printf("int2(buff, 8): %d\n", LittleEndian.getInt2(buff, 8));
        System.out.printf("uint32(buff, 10): %d\n", LittleEndian.getUint32(buff, 10));
    }

}
