package org.lionsoul.ip2region.xdb;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
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

    // --- v4

    @Test
    public void testV4InputStreamBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromInputStream(
            new FileInputStream(getDataPath("ip2region_v4.xdb"))
        );
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
    }

    @Test
    public void testV4FixedBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromFile(
            new File(getDataPath("ip2region_v4.xdb")), 2 * 1024 * 1024
        );
        final Header header = Searcher.loadHeaderFromBuffer(cBuffer);
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
        log.debugf("Header->%s", header);
    }

    @Test
    public void testV4BufferAssert() throws Exception {
        final LongByteArray m2Bufer = Searcher.loadContentFromFile(
            new File(getDataPath("ip2region_v4.xdb")), 2 * 1024 * 1024
        );
        final LongByteArray m5Bufer = Searcher.loadContentFromFile(
            new File(getDataPath("ip2region_v4.xdb")), 5 * 1024 * 1024
        );

        final int[] offsets = new int[]{0, 10, 512, 1024, 39672, 1024 * 1024 * 2};
        for (int idx : offsets) {
            final long m2Val = m2Bufer.getUint32(idx);
            final long m5Val = m5Bufer.getUint32(idx);
            log.debugf("m2Buffer[%8d:4]: %10d, m5Buffer[%8d:4]: %10d, equals ? %s", idx, m2Val, idx, m5Val, m2Val == m5Val ? "true" : "false");
        }
    }

    @Test
    public void testV4BufferEOF() throws IOException {
        final LongByteArray buffer = Searcher.loadContentFromFile(
            new File(getDataPath("ip2region_v4.xdb")), 2 * 1024 * 1024
        );

        try {
            buffer.append(new byte[1024]);
        } catch (IOException e) {
            log.debugf("failed to append: %s", e.getMessage());
        }
    }

    // --- v6

    @Test
    public void testV6InputStreamBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromInputStream(
            new FileInputStream(getDataPath("ip2region_v6.xdb"))
        );
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
    }

    @Test
    public void testV6FixedBuffer() throws Exception {
        final LongByteArray cBuffer = Searcher.loadContentFromFile(
            new File(getDataPath("ip2region_v6.xdb")), 5 * 1024 * 1024
        );
        final Header header = Searcher.loadHeaderFromBuffer(cBuffer);
        log.debugf("cBuffer->{length:%d, size:%d}", cBuffer.length(), cBuffer.size());
        log.debugf("Header->%s", header);
    }

}
