package org.lionsoul.ip2region.xdb;

import java.io.FileInputStream;
import java.security.CodeSource;

import org.junit.Test;

public class BufferTest {

    private static final Log log = Log.getLogger(VersionTest.class).setLevel(Log.DEBUG);

    public static final String getDataPath(String xdbFile) {
        final CodeSource cs = BufferTest.class.getProtectionDomain().getCodeSource();
        if (cs != null) {
            // log.debugf("code path: %s", cs.getLocation().getPath().concat("../../../../data/"));
            return cs.getLocation().getPath().concat("../../../../data/").concat(xdbFile);
        } else {
            return "../../../../data/".concat(xdbFile);
        }
    }

    @Test
    public void testV4InputStreamBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromInputStream(
            new FileInputStream(getDataPath("ip2region_v4.xdb"))
        );
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
    }

    @Test
    public void testV6InputStreamBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromInputStream(
            new FileInputStream(getDataPath("ip2region_v6.xdb"))
        );
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
    }

}
