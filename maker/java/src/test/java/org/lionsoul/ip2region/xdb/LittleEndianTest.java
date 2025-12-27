package org.lionsoul.ip2region.xdb;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class LittleEndianTest {

    private static final Log log = Log.getLogger(LittleEndianTest.class).setLevel(Log.DEBUG);

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

        log.debugf("uint32(buff, 0): %d", LittleEndian.getUint32(buff, 0));
        log.debugf("uint32(buff, 4): %d", LittleEndian.getUint32(buff, 4));
        log.debugf("int2(buff, 8): %d", LittleEndian.getInt2(buff, 8));
        log.debugf("uint32(buff, 10): %d", LittleEndian.getUint32(buff, 10));
    }

}
