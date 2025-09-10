package org.lionsoul.ip2region.xdb;

import org.junit.Test;

public class IndexPolicyTest {

    private static final Log log = Log.getLogger(IndexPolicyTest.class).setLevel(Log.DEBUG);

    @Test
    public void testParse() {
        final String[] inputs = {"vector", "btree", "VecTor", "BTree", "abc"};
        for (String str : inputs) {
            try {
                int policy = IndexPolicy.parse(str);
                log.infof("parse(%s)=%d", str, policy);
            } catch (Exception e) {
                log.errorf("parse index policy `%s`: %s", str, e.getMessage());
            }
        }
    }
}
