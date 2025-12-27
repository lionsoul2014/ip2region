package org.lionsoul.ip2region.service;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.CodeSource;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.XdbException;

public class ConfigTest {

    private static final Log log = Log.getLogger(ConfigTest.class).setLevel(Log.DEBUG);

    public static final String getDataPath(String xdbFile) {
        final CodeSource cs = ConfigTest.class.getProtectionDomain().getCodeSource();
        if (cs != null) {
            // log.debugf("code path: %s", cs.getLocation().getPath().concat("../../../../data/"));
            return cs.getLocation().getPath().concat("../../../../data/").concat(xdbFile);
        } else {
            return "../../../../data/".concat(xdbFile);
        }
    }

    @Test
    public void testBuildV4Config() throws IOException, XdbException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setXdbPath(getDataPath("ip2region_v4.xdb"))
            .setSearchers(20)
            .asV4();
        log.debugf("builded config: %s", v4Config);
    }

    @Test
    public void testBuildV4ConfigFromFile() throws IOException, XdbException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbFile(new File(getDataPath("ip2region_v4.xdb")))
            .setSearchers(20)
            .asV4();
        log.debugf("builded config: %s", v4Config);
    }

    @Test
    public void testBuildV4ConfigFromInputStream() throws IOException, XdbException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbInputStream(new FileInputStream(getDataPath("ip2region_v4.xdb")))
            .setSearchers(20)
            .asV4();
        log.debugf("builded config: buffs.size=%d, %s", v4Config.cBuffer.size(), v4Config);
    }

    @Test
    public void testBuildV4SliceBytes() throws IOException, XdbException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.FullCache)
            .setCacheSliceBytes(1024 * 1024)  // 1MiB
            .setXdbPath(getDataPath("ip2region_v4.xdb"))
            .setSearchers(20)
            .asV4();
        log.debugf("builded config: buffs.size=%d, %s", v4Config.cBuffer.size(), v4Config);
    }

    // --- IPv6

    @Test
    public void testBuildV6Config() throws IOException, XdbException, InvalidConfigException {
        final Config v6Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setXdbPath(getDataPath("ip2region_v6.xdb"))
            .setSearchers(20)
            .asV6();
        log.debugf("builded config: %s", v6Config);
    }

    @Test
    public void testBuildV6ConfigFromFile() throws IOException, XdbException, InvalidConfigException {
        final Config v6Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbFile(new File(getDataPath("ip2region_v6.xdb")))
            .setSearchers(20)
            .asV6();
        log.debugf("builded config: %s", v6Config);
    }

    @Test
    public void testBuildV6ConfigFromInputStream() throws IOException, XdbException, InvalidConfigException {
        final Config v6Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbInputStream(new FileInputStream(getDataPath("ip2region_v6.xdb")))
            .setSearchers(20)
            .asV6();
        log.debugf("builded config: buffs.size=%d, %s", v6Config.cBuffer.size(), v6Config);
    }

    @Test
    public void testBuildV6SliceBytes() throws IOException, XdbException, InvalidConfigException {
        final Config v6Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setCacheSliceBytes(1024 * 1024 * 4)    // 4 MiB
            .setXdbInputStream(new FileInputStream(getDataPath("ip2region_v6.xdb")))
            .setSearchers(20)
            .asV6();
        log.debugf("builded config: buffs.size=%d, %s", v6Config.cBuffer.size(), v6Config);
    }

}
