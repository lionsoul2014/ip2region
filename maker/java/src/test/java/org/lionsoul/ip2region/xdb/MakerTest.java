package org.lionsoul.ip2region.xdb;

import org.junit.Test;

public class MakerTest {

    private static final Log log = Log.getLogger(MakerTest.class);

    @Test
    public void testInit() {
        log.infof("MaxFilePointer(%d bytes): %d", Maker.RuntimePtrSize, Maker.MaxFilePointer);
        log.infof("MaxFilePoiner(5 bytes): %d", ((1L << (5 * 8)) - 1));
        log.infof("MaxFilePoiner(6 bytes): %d", ((1L << (6 * 8)) - 1));
    }
}
