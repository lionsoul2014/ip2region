package ip2region.tests.service

import ip2region.xdb.*
import ip2region.service.*

@Test
class SearcherPoolTests {
    @TestCase
    func testBorrowAndReturn() {
        let cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 3)
        let pool = SearcherPool(cfg)
        let s = pool.borrow()
        if (pool.getLoanCount() != 1) {
            throw Exception("loan count should be 1, got ${pool.getLoanCount()}")
        }
        pool.return_(s)
        if (pool.getLoanCount() != 0) {
            throw Exception("loan count should be 0 after return")
        }
        pool.close()
    }

    @TestCase
    func testBorrowAll() {
        let cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 5)
        let pool = SearcherPool(cfg)
        let s1 = pool.borrow()
        let s2 = pool.borrow()
        let s3 = pool.borrow()
        let s4 = pool.borrow()
        let s5 = pool.borrow()
        if (pool.getLoanCount() != 5) {
            throw Exception("should have 5 loans, got ${pool.getLoanCount()}")
        }
        pool.return_(s5)
        pool.return_(s4)
        pool.return_(s3)
        pool.return_(s2)
        pool.return_(s1)
        if (pool.getLoanCount() != 0) {
            throw Exception("all should be returned, got ${pool.getLoanCount()}")
        }
        pool.close()
    }

    @TestCase
    func testClosePool() {
        let cfg = Config(ContentBuff, IPv4, "../../data/ip2region_v4.xdb", 2)
        let pool = SearcherPool(cfg)
        let s = pool.borrow()
        pool.return_(s)
        pool.close()
        pool.close()
    }
}
